(ns smtbot.task.process_result
  #_{:clj-kondo/ignore [:refer-all]}
  (:require
   [cheshire.core :as json]
   [clojure.string :as s]
   [smtbot.enum.process_result :refer :all]
   [smtbot.enum.sm :as sm]
   [smtbot.http_errors :as http-errors]
   [smtbot.task.writers :as tw]
   [smtbot.utils.macro :refer [_case]]
   [smtbot.utils.sm_resp_decode :refer [get-RC get-jbody]]
   [taoensso.timbre.appenders.core :as appenders]
   [smtbot.bot.users.validation-hook :as vh]
   [smtbot.cache.result-processor :as rp]
   [smtbot.hook.hook :as hook]
   [taoensso.timbre :as timbre
    :refer [debug warn fatal]]))

(defmacro ^:private tread-details
  ([opts] `(format "Thread:%s,Mode:%s,User:%s,RecID:%s,Url:%s"
                   (:thread ~opts) (:mode ~opts)
                   (:user-name ~opts) (:rec-id ~opts) (:url ~opts)))

  ([prev opts  post]
   `(str (if (nil? ~prev) "" ~prev)
         (tread-details ~opts)
         (if (nil? ~post) "" ~post))))

(def super-hook-enabled  true)


(defn  get-super-hook [tag]
  (if super-hook-enabled
    (case (subs tag 0 7)
      "TBOT:CB" (fn [resp]
                  (rp/resp-processor-cacheable resp  (-> resp :opts :tag (subs 7))))
      "TBOT:NB" (fn [resp]
                  (rp/resp-processor-not-cacheable resp (-> resp :opts :tag (subs 7))))
      "TBOT:US" vh/validation-hook
      nil)
    nil))


(defmacro writer-error [err message opts body]
  `(timbre/error (str "Exeption " ~err " in " (tread-details ~message ~opts (str "Response body " ~body)))))

#_(defn- finalize-action-OK [body headers status opts]
    (if-let [supper-hook  (get-super-hook (opts :tag))]
      (supper-hook opts status body  headers)
      (when (string? (@tw/result-writer (opts :rec-id) body status))
        (timbre/error
         (tread-details  "Write error in status http-errors/OK"
                         opts
                         (str "Responce body " body)))))
  ;(hook/post-and-copy opts status body  headers)
  ;;TODO simple attachment copy required
    )


#_(defn- finalize-action-ERROR [body headers status opts]
    (when (string? (@tw/result-writer (opts :rec-id) body status))
      (timbre/error
       (tread-details  "Write error in status http-errors/OK"
                       opts
                       (str "Responce body " body))))
    (hook/post-message opts status body (:content-type headers)))


#_(defn reschedule-action [body headers status opts]
    (when (string? (@tw/action-rescheduler (opts :rec-id) body status))
      (timbre/error
       (tread-details  "Write error in status http-errors/OK"
                       opts
                       (str "Responce body " body))))
    (when (= (:attempt opts) 1) ;; it was last attempt, inform about fail
      (hook/post-message opts status body (:content-type headers))))


(defn- finalize-action-OK [{:keys [body headers status opts] :as resp}]
  (if-let [supper-hook  (-> opts :tag get-super-hook)]
    (supper-hook resp)
    (do
      (try (@tw/result-writer (opts :rec-id) body status)
           (catch Exception e (writer-error (ex-message e) "status NOT http-errors/OK " opts body)))
      (hook/post-and-copy opts status body (:content-type headers))))
  ;;TODO simple attachment copy required
  )

(defn- finalize-action-ERROR [{:keys [body headers status opts] :as resp}]
  (if-let [supper-hook  (get-super-hook (opts :tag))]
    (supper-hook resp)
    (do
      (try (@tw/result-writer (opts :rec-id) body status)
           (catch Exception e (writer-error (ex-message e) "status NOT http-errors/OK " opts body)))
      (hook/post-message opts status body (:content-type headers)))))


(defn reschedule-action [{:keys [body headers status opts] :as resp}]
  (try (@tw/action-rescheduler (opts :rec-id) body status)
       (catch Exception e (writer-error (ex-message e) " reschedule " opts body)))
  ;; it was last attempt, inform about fail
  (when (= (:attempt opts) 1)
    (if-let [supper-hook  (get-super-hook (opts :tag))]
      (supper-hook resp)
      (hook/post-message opts status body (:content-type headers)))))

(defn- process-http-ok [{:keys [body headers] :as resp}]
  (if (= (get-RC body headers) sm/RC_SUCCESS)
    (finalize-action-OK resp)
    (reschedule-action resp))
  OK)


(defn- process-http-unathorized [{:keys [body headers opts] :as resp}]
  (let [jbody (get-jbody body headers)]
    (if  (= (get jbody "ReturnCode") sm/RC_WRONG_CREDENTIALS)
      (if (and (some? (jbody "Messages"))
               (s/includes? ((jbody "Messages") 0) "Not Authorized"))
        (do
          (timbre/error "Not Authorized. Write the answer. Body:"  body)
          (finalize-action-ERROR resp)
          NOT-ATHORIZED)

        (do
          (warn "Two many threads." (tread-details   opts))
          TOO-MANY-THREADS))
      (do
        (timbre/error "Unkown athorization error:"  body)
        (finalize-action-ERROR resp)
        OK))))

(defn- process-http-not-found [{:keys [body headers opts] :as resp}]
  (_case  (get-RC body headers)
          sm/RC_NO_MORE (do ; sm responce - has not requested item 
                          (timbre/error "Attempt to use incorrect service name:" body)
                          (finalize-action-ERROR resp)
                          OK)

          nil (do ; ReturnCode is nil, most probably no SM
                (timbre/error (tread-details "SM not available - " opts "."))
                SERVER-NOT-AVAILABLE)

          (do (warn (tread-details
                     (format "Suspections combination of status 404 and ReturnCode %s"
                             (get (json/parse-string body) "ReturnCode"))
                     opts "."))
                          ;cheat - this code reused to retry action 
              TOO-MANY-THREADS)))


(defn- process-http-internal-error [{:keys [body headers] :as resp}]
  (if (= (get-RC body headers) sm/RC_WRONG_CREDENTIALS)
    (reschedule-action resp)
    (finalize-action-ERROR resp))
  OK)

(defn process
  "Process responce from SM. 
   Return code must by processed by push managers
   Responce matrix 
   !status         !ReturnCode !    -- Message --    ! Action ! Return code            
   !OK 200         ! SUCCESS 0 !      ANY            !   W    ! OK             
   !OK 200         ! not 0     !      ANY            !   R    ! OK             
   !Unathorized 401! WRONG_C -4! 'Not Authorized'    !   W    ! NOT-ATHORIZED  
   !Unathorized 401! WRONG_C -4! not 'Not Authorized'!   -    ! TOO-MANY-THREADS
   !Unathorized 401! not -4    !      ANY            !   W    ! ОК
   !Not-Found   404! NO_MORE  9!      ANY            !   W    ! ОК             
   !Not-Found   404! not      9!      ANY            !   -    ! TOO-MANY-THREADS 
   !Not-Found   404! nil       !      ANY            !   -    ! SERVER-NOT-AVAILABLE 
   !Bad-Request 400! ANY       !      ANY            !   W    ! ОК             
   !Not-Found   500! WRONG_C -4!      ANY            !   R    ! OK 
   !Not-Found   500!  not -4   !      ANY            !   W    ! OK 
   !ANY OTHER      !   ANY     !      ANY            !   W    ! OK
   "
  ^long [{:keys [status opts body headers error] :as resp}]
  (timbre/with-merged-config
    {:appenders {:println {:enabled? false}
                 :spit (appenders/spit-appender {:fname "log/process_result.log"})}}
    (debug (:thread opts) ":" (:rec-id opts) "=> S:" status " RC:" (get-RC body headers))
    (if (or error (nil? status) (nil? opts))
      (do
        (fatal "Failed, exception: error " error " status " status)
        (fatal (tread-details "!!!!" opts " - exited!!!"))
        ERROR)
      (_case (long status)
             http-errors/OK    (process-http-ok resp)

             http-errors/Unathorized  (process-http-unathorized resp)


             http-errors/Not-Found (process-http-not-found resp)

             http-errors/Bad-Request  (do (finalize-action-ERROR resp) OK)

             http-errors/Internal-Server-Error (process-http-internal-error resp)

             (do
               (timbre/error (tread-details (format "UnSuccess. Status %s. Server error in " status) opts "."))
               (finalize-action-ERROR resp)
               OK)))))