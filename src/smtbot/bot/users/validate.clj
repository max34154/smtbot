(ns smtbot.bot.users.validate
  (:require   [smtbot.task.sync_pusher :as pusher-manager]
              ;[smtbot.utils.sm_resp_decode :refer [get-jbody]]
              [taoensso.timbre :as timbre]
              [smtbot.http_errors :as http-errors]
              ;[smtbot.dal.globals :refer [botuser-action request-action ]]
              [smtbot.dal.globals :refer [request-action]]
              ;[smtbot.enum.sm :as sm]
              [smtbot.bot.users.validation-hook :refer [validation-hook]]
              [smtbot.config :as config :refer  [get-uid]]))


(def ^:private request-template {;:req_id rec_id
                                 :user_name "TBOT"
                                 :status "N"
                       ;:schedule_name schedule_name - not valied
                                 :execution_mode  "I"
                                 :attempt 10
                                 :execution_retries 10
                                 :retry_interval 50 ; just place holder
                       ;:action (route-params :action_id)
                       ;:parameters (if (string? parameters) parameters (json/generate-string parameters))
                                 :tag "TBOT:US"
                       ;:expire_at expire_at ;(if (nil? expire_at) "NULL" expire_at)
                                 :service "tbotuser"
                       ;:subject (route-params :name)
                                 })


(def ^:private fast-getter
  " get function to reqiest target system 
  "
  (delay (;; Оnly async-pusher supported.
          ;; (get-config  :async-pusher-enabled) value ignored 
          (pusher-manager/get-pusher-factory false)
          :global-mode
          true
          (@config/config :config)
          (@config/config :workers))))

(defn- registration-request [^String user-key ]
  (assoc request-template :req_id  (get-uid) :parameters
         (format "{\"user\":{\"key\":\"%s\",\"status\":true}}" user-key)))

(defn- validation-request [user-key]
  (assoc request-template :req_id  (get-uid) :parameters
         (str "{\"user\":{\"key\":\"" user-key "\"}}")))

#_(defn validation-hook [opts _ body headers]
  (let [jbody (get-jbody body headers)]
    (if (= (get jbody "ReturnCode")  sm/RC_SUCCESS)
      (do
        (timbre/debug "User status recived " jbody)
        (-> jbody
            (get  "user")
            (#((botuser-action :update-by-key) (% "key") (% "status") (% "role")))))
      (timbre/error "Request failed, responce "  body "request options " opts))))


(defn validation-response-parser
  [{:keys [status opts body headers error]}]
  (if (or (not= http-errors/OK status) (nil? opts) (nil? body))
    (timbre/error "Unable to get update from the target system, exception: error " error
                  " status " status "opts" opts "body" body)
    (validation-hook opts status body headers)))

(defn validate-user
  "Validate user syncronusly"
  [smkey]
  (validation-response-parser
   (@fast-getter (validation-request smkey) "unknow")))

(defn request-validation
  "Place request for async user validation"
  [smkey]
  ((@request-action :direct-insert)  (validation-request smkey)))


(defn request-reqistration   [^String smkey]
  ((@request-action :direct-insert)  (registration-request smkey)))

(defn registrate-user  [^String smkey]
   (validation-response-parser (@fast-getter (registration-request smkey) "unknow")))