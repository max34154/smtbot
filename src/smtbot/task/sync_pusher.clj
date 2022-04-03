(ns smtbot.task.sync_pusher
  {:clj-kondo/config  '{:linters {:unused-referred-var
                                  {:exclude {taoensso.timbre [log  trace  debug  info  warn  error  fatal  report
                                                              logf tracef debugf infof warnf errorf fatalf reportf
                                                              spy get-env]}}}}}
  (:require  [cheshire.core :as json]
             [org.httpkit.client :as http]
            ; [smtbot.utils.base64 :as b64]
             [taoensso.timbre :as timbre
              :refer [log  trace  debug  info  warn  error  fatal  report
                      logf tracef debugf infof warnf errorf fatalf reportf
                      spy get-env]]
             [taoensso.timbre.appenders.core :as appenders]
             ;[smtbot.config :as config]
             [smtbot.session :as session]
             [smtbot.dal.globals :refer [attachment-action]]
             [smtbot.utils.macro :refer [_case]]
             [smtbot.enum.task_result :as tr]
             [smtbot.enum.process_result :as pr]
             [smtbot.task.process_result :as result])
  (:import [java.net URLEncoder]))


(defn- fill-async-request [{:keys [action parameters schedule_name execution_retries retry_interval req_id]}]
  (json/generate-string     {:action action
                             :parameters parameters
                             :execution
                             {:mode "scheduleOnly"
                              :sheduler schedule_name
                              :retries execution_retries
                              :attachmentsCount ((@attachment-action :get-attachments-count) req_id)
                              :retiryInterval  retry_interval}}))


(defn url-builder-factory [mode get-allowed {:keys [base-url async-action-url]}]
  (if (= mode :async-mode)
    (fn [_] async-action-url)
    (if get-allowed
      (fn [action] #_(if (= (action :action) "get")
                       (str base-url  (action :service) "/" (action :subject))
                       (if (empty? (action :subject))
                         (str base-url  (action :service))
                         (str base-url  (action :service) "/" (action :subject) "/" (action :action))))
        (case (action :action)
          "get" (str base-url  (action :service) "/" (action :subject))
          "get-query" (str base-url  (action :service)
                           "?query=" (URLEncoder/encode ^String (action :parameters) "UTF-8")  "&view=expand")
          (if (empty? (action :subject))
            (str base-url  (action :service))
            (str base-url  (action :service) "/" (action :subject) "/" (action :action)))))

      (fn [action] (if (empty? (action :subject))
                     (str base-url  (action :service))
                     (if (empty? (action :action))
                       (str base-url  (action :service) "/" (action :subject))
                       ;(str base-url  (action :service) "/" (action :subject) "/action/" (action :action))
                       (str base-url  (action :service) "/" (action :subject) "/" (action :action))))))))

(defn method-builder-factory [mode get-allowed]
  (if (= mode :async-mode)
    (fn [_] :post)
    (if get-allowed
      (fn [action] (case (action :action)
                     ("get" "get-query") :get
                     :post))
      (fn [_] :post))))


(defn authorization-builder-factory [mode {:keys [async-credentials global-credentials global-mode-user]}]
  (case mode
    :user-mode  (fn [action] (session/get-credentials (action :user_name)))
    :async-mode (fn [_] async-credentials)
    :global-mode (if (= global-mode-user "masked")
                   (fn [_] global-credentials)
                   (fn [action] (session/get-credentials (action :user_name))))))

(defn body-builder-factory [mode]
  (if (= mode :async-mode)
    (fn [action] (json/generate-string {:UserName (action :user_name)
                                        :TicketID (action :subject)
                                        :Request  (fill-async-request
                                                   action)}))
    (fn [action]
      (when  (not= (action :action) "get-query") ;; parameters is used to store query 
        (action :parameters)))))

(defn build-opts-factory [mode get-allowed config workers]
  (let [url-builder (url-builder-factory mode get-allowed config)
        method-builder (method-builder-factory mode get-allowed)
        authorization-builder (authorization-builder-factory mode workers)
        body-builder (body-builder-factory mode)]
    (fn [action thread]
      {:url (url-builder action)
       :method (method-builder action)
       :thread thread
       :user-name (action :user_name)
       :rec-id (action :req_id)
       :subject (action :subject)
       :service (action :service)
       :attempt (action :attempt)
       :tag (action :tag)
       :mode mode
       :headers
       {"Content-Type"  "application/json"
        "Connection" "keep-alive"
        "Authorization"  (#(if (nil? %) (error "Auth is nil for action " action) %)
                          (authorization-builder action))}
       :body (body-builder action)})))


(defn get-pusher-factory
  "Returns pusher factory. 
   Depends on  async-pusher-enabled created factory generates asyn or sync pusher factory.
   Pusher generated by async factory uses async http request pushing task to sm.
   Pusher factory requers the following parameters to create pusher:
    mode - one of :user-mode, :global-mode or async-mode 
    get-allowed - true or false, make sence for :user-mode only, allow user place get request 
    config -  global configuration, in most cases thes best choise is (config/get-config)
    workers -  workers configuration, in most cases thes best choise is (config/get-workers) 
   "
  [async-pusher-enabled]
  (timbre/with-merged-config
    {:appenders {:spit (appenders/spit-appender {:fname "log/pusher_conf.log"})}}
    (if (true? async-pusher-enabled)
      (fn  [mode get-allowed config workers]
        (let [option-builder (build-opts-factory mode get-allowed config workers)]
          (fn [action thread write-channel-callback]
            (http/request (option-builder action thread) write-channel-callback))))

      (fn  [mode get-allowed config workers]
        (let [option-builder (build-opts-factory mode get-allowed config workers)]
          (fn [action thread]
            @(http/request (option-builder action thread))))))))


(defn processor ^long [resp ^String thread]
  (timbre/with-merged-config
    {:appenders {:spit (appenders/spit-appender {:fname "log/pusher.log"})}}
    (if (nil? resp)
      (do (warnf "Thread %s got empty responce. Time out? " thread)   tr/RETRY-ACTION)
      (let [opts (:opts resp)
            {:keys [user-name  credentials mode rec-id]}  opts
            result-code (result/process resp)]
        (debug thread ":Task:" rec-id "(" (:status resp) (:body resp) ") result code " result-code)
        (_case  result-code
                pr/OK  tr/NEXT-ACTION

                pr/NOT-ATHORIZED
                (if (= mode :user-mode)
                  (do
                    (session/remove-credentials user-name credentials)
                    tr/NEXT-ACTION)
                  (do
                    (fatal (format "Mode %s user not athorized. Thread %s exited." mode thread))
                    tr/EXIT-THREAD))

                pr/TOO-MANY-THREADS tr/RETRY-ACTION

                pr/SERVER-NOT-AVAILABLE tr/SERVER-NOT-AVAILABLE

                (do  (fatal "Can't parse result: " resp "\n")
                     tr/EXIT-THREAD))))))



