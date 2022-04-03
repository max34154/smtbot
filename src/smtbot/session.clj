(ns smtbot.session
  {:clj-kondo/config  '{:linters {:unused-referred-var
                                  {:exclude {taoensso.timbre [log  trace  debug  info  warn  error  fatal  report
                                                              logf tracef debugf infof warnf errorf fatalf reportf
                                                              spy get-env]}}}}}
  (:require  [cheshire.core :as json]
             ;[clojure.java.io :as io]
             [clojure.string :as str]
             ;[clj-http.client :as http]
             [org.httpkit.client :as http]
             [smtbot.utils.base64 :refer [b64->string]]
             [smtbot.http_errors :as http-errors]
             [smtbot.utils.macro :refer [tod-seconds]]
             [taoensso.timbre :as timbre
              :refer [log  trace  debug  info  warn  error  fatal  report
                      logf tracef debugf infof warnf errorf fatalf reportf
                      spy get-env]]
             [taoensso.timbre.appenders.core :as appenders]
             [smtbot.config :as config]
             [smtbot.dal.user :as dal-u]))



#_(def module_ip  (get cconfig/config :module_ip))

(def max_session_length (* 3600 12))

(defonce sessions (atom {}))

(defonce user-cache (atom {}))

(defn acl-to-keys [acl]
  (for [l (str/split acl #",")] (keyword l)))

(defn new-session [authorization name access-list]
  ;(debugf "Session created for %s (%s) with acl %s"  name authorization access-list)
  (let [valid (+ (tod-seconds) max_session_length)
        row {:name name
             :password authorization
             :expire_at  valid}]
    (swap! sessions assoc authorization {:name name
                                         :access-list (when access-list (set (acl-to-keys access-list)))
                                         :valid-till valid})
    (swap! user-cache assoc name row)
    (@dal-u/update-user  row)))

(defn get-credentials [name]
  (or  (->> @user-cache
            (#(% name))
            :password)
       (#(when %                     ;when-not (nil? %)
           (swap! user-cache assoc name %)
           (:password %))
        (:val (@dal-u/get-user name)))))

(defn remove-credentials [name password]
  (when (= (->> @user-cache
                (#(% name))
                :password)
           password)
    (swap! user-cache dissoc name))
  (when (= (-> (@dal-u/get-user name) :val :password) password)
    (@dal-u/delete-user name)))

(defn check-acl [request]
  (let [authorization ((:headers request) "authorization")
        service (:service request)
        session (get @sessions authorization)
        access-list (session :access-list)]
    (debug (format "Access list checked - access %S" (if (and  access-list  (access-list service)) "Granted" "Denied")))
    (and  access-list  (access-list service))))

#_(def auth-url (format "http://%s/SM/9/rest/asyncuser" module_ip))

(defn auth-token-info
  [token-info-response]
  (let [{:keys [status body]} token-info-response]
    (if (= status 200)
      (json/parse-string body keyword)
      {:ReturnCode status})))

(defn sm-login [user-name user-password auth]
  (let [token-info-response
        @(http/post (config/get-config :auth-url)
                    {:basic-auth   [user-name user-password]
                     :content-type :json
                     :body (format "{user:{name:\"%s\"}}" user-name)
                    ;:throw-exceptions false
                     })
        {:keys [ReturnCode user]} (auth-token-info token-info-response)
        {:keys [acl]} user]
    (when (= ReturnCode 0)
      (new-session auth user-name acl))))

(defn basic-authentication-request
  [request auth-fn handler]
  (let [auth ((:headers request) "authorization")
        cred (and auth (b64->string (last (re-find #"^Basic (.*)$" auth))))
        [user pass] (and cred (str/split (str cred) #":" 2))
        user (str user)]
    (if (auth-fn user (str pass) auth)
      (do (debug (format  "External athorization succeded for %s" user))
          (handler (assoc request :user_name user)))
      (do ;{:status 401 :body  "{\"Messages\":[\"Unathorized\"], \"ReturnCode\":28}"}
        (debug (format  "External athorization declined for %s" user))
        http-errors/unathorized-401))))


(defn local-auth [request handler]
  (timbre/with-merged-config
    {:appenders {:println {:enabled? false}
                 :spit (appenders/spit-appender {:fname "log/session.log"})}}
    (let [authorization (last (re-find #"^Basic (.*)$" ((:headers request) "authorization")))
          session (get @sessions authorization)]
    ;(debug "Authorization " authorization)
    ;  (debug  (format "Session %s for user %" (if session "exists"  "doesn't exits")))
      (if (and session (> (session :valid-till) (tod-seconds)))
        (do (debug (format  "User %s has valid session" (str (session :name))))
            (handler (assoc request :user_name (session :name))))
        (basic-authentication-request request sm-login handler)))))


(defn warp-auth [handler]
  (fn [request]  (local-auth request handler)))

(defn warp-check-acl [handler]
  (fn [request]
    (if (check-acl request)
      (handler request)
      http-errors/forbiden-403)))
