(ns smtbot.core
  (:require
   [taoensso.timbre :as timbre :refer [info  fatal]]
   [smtbot.config :as cfg :refer [configure]]
   [smtbot.dal.configure :refer [configure-database stop-database]]
   ;[smtbot.request.request :refer [start-AccessHTTPServer stop-AccesHTTPServer]]
   [smtbot.task.sync_dispatcher :refer [start-pushers stop-pushers]]
   [smtbot.utils.options :refer [read-options]]
   ;[smtbot.hook.dispatcher :refer [start-messengers stop-messengers]]
   [smtbot.bot.config :as bot]
   [clojure.stacktrace :refer [print-stack-trace]])
  (:gen-class))



(defn shutdown []
  ;(stop-AccesHTTPServer)
  (stop-pushers)
  ;(stop-messengers)
  (stop-database))




(defn startup
  ([config] {:pre [(some? config)]}
            (try
              (let [port (-> config :-port
                             (#(if (vector? %) (% 0) %))
                             (#(if (integer? %) % (Integer/parseInt % 10))))]
                (when-not (integer? port)
                  (throw (AssertionError. (str "Incorrect port number: "  (config :-port)))))
                (info "Read configuration from  " (config :-path))
                (when (nil? (configure (config :-path))) (throw (AssertionError. (str "Incorrect or missing configuration in  "  (config :-path)))))
                (info "SM Base URL:" (cfg/get-config :base-url))
                (info "Authorization URL:" (cfg/get-config :auth-url))
                (info "SM Async Actions URL:" (cfg/get-config :async-action-url))
                (info "Start initialization.")
                (when (= (configure-database config) -1) (throw (AssertionError. "Database configuration error.")))
                (when (some? (start-pushers)) (throw (AssertionError.  "Pushers configuration error.")))
                ;(when (nil? (cache/configure (config :-path))) (throw (AssertionError.  "Cache configuration error.")))
                ;(when (some? (start-messengers)) (throw (AssertionError. "Messengers configuration error")))
                ;(when (nil? (start-AccessHTTPServer port)) (throw (AssertionError. "Gatekeeper configuration error")))
                (bot/configure (config :-path))
                (info "Initialization sucessfully completed."))
              (catch Exception e
                (fatal (ex-message e) "\nInitialization failed.")
                (print-stack-trace e)
                (shutdown)))))





(defn -main [& args]
  ;(let [startup-config {:-port "8080" :-path "./"}])
  (startup (read-options args {:-port "8080" :-path "./"})))
  ;(startup (Integer/parseInt (or port "8080") 10) path))

(comment (-main "3000"))
