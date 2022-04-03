(ns smtbot.dal.hook
  (:require [clojure.java.jdbc :as jdbc]
            [smtbot.validate]
            [clojure.string :as str]
            [smtbot.dal.globals :as g :refer [db]]
            [taoensso.timbre :as timbre]
            [clojure.stacktrace :as stacktrace]
            [smtbot.utils.macro :refer [unixtime->timestamp tod]]))



(def ^:private hook-global-user-name "0")

(defn user-lock-conditions
  "All the messages created for particular user"
  [^String user-name]
  (str "user_name='" user-name "' "))

(defn user-list-conditions
  "All the messages create for listed users "
  [^String included-names]
  (str " user_name  IN ('" included-names "') "))

(defn global-lock-condition
  "All tasks from all users except excluded"
  [^String excluded-names]
  (str " user_name NOT IN ('" excluded-names "') "))

(def global-lock-condition-global-only
  " TRUE ")

;; 
;; Database type dependend. Please add methods for each supported db 
;; 
(defmulti ^:private lock-msg-on-time-sql (fn [db-config] (:db-type  db-config)))

(defmethod lock-msg-on-time-sql :default [db-config]
  (throw (IllegalArgumentException.
          (str "Unsupported database type " (:db-type  db-config) "."))))

(defmulti ^:private  get-msg-on-time-sql (fn [db-config] (:db-type  db-config)))

(defmethod get-msg-on-time-sql :default [db-config]
  (throw (IllegalArgumentException.
          (str "Unsupported database type " (:db-type  db-config) "."))))


(defmulti ^:private  get-template-sql (fn [db-config] (:db-type  db-config)))

(defmethod get-template-sql :default [db-config]
  (throw (IllegalArgumentException.
          (str "Unsupported database type " (:db-type  db-config) "."))))

(defmulti ^:private  get-all-templates-sql (fn [db-config] (:db-type  db-config)))

(defmethod get-all-templates-sql :default [db-config]
  (throw (IllegalArgumentException.
          (str "Unsupported database type " (:db-type  db-config) "."))))

(defmulti ^:private  get-all-available-templates-sql (fn [db-config] (:db-type  db-config)))

(defmethod get-all-available-templates-sql :default [db-config]
  (throw (IllegalArgumentException.
          (str "Unsupported database type " (:db-type  db-config) "."))))

(defmulti ^:private  get-message-by-id-sql (fn [db-config] (:db-type  db-config)))

(defmethod get-message-by-id-sql :default [db-config]
  (throw (IllegalArgumentException.
          (str "Unsupported database type " (:db-type  db-config) "."))))

(defmulti ^:private  post-message-sql (fn [db-config] (:db-type  db-config)))

(defmethod post-message-sql :default [db-config]
  (throw (IllegalArgumentException.
          (str "Unsupported database type " (:db-type  db-config) "."))))

(defmulti ^:private  reschedule-msg-sql (fn [db-config] (:db-type  db-config)))

(defmethod reschedule-msg-sql :default [db-config]
  (throw (IllegalArgumentException.
          (str "Unsupported database type " (:db-type  db-config) "."))))



(defmulti ^:private  dump-message-queue-sql (fn [db-config] (:db-type  db-config)))

(defmethod dump-message-queue-sql :default [db-config]
  (throw (IllegalArgumentException.
          (str "Unsupported database type " (:db-type  db-config) "."))))

(defmulti ^:private  log-message-delivery-sql (fn [db-config] (:db-type  db-config)))

(defmethod log-message-delivery-sql :default [db-config]
  (throw (IllegalArgumentException.
          (str "Unsupported database type " (:db-type  db-config) "."))))



;; H2 methods 

(defmethod lock-msg-on-time-sql "h2" [{:keys [^String db-schema]}]
  (str "UPDATE " db-schema ".MESSAGE"
       " SET lock_time= ?,locked_by=?"
       " WHERE locked_by is NULL and next_run < SYSDATE and %s limit ?"))


(defmethod get-msg-on-time-sql "h2" [{:keys [^String db-schema]}]
  (str "SELECT ID, URL, METHOD, STRINGDECODE(HEADERS) AS HEADERS,"
       "STRINGDECODE(BODY) AS BODY,"
       "ATTEMPT, "
       "RETRY_INTERVAL,"
       "NEXT_RUN,"
       "LOCKED_BY,"
       "USER_NAME,"
       "LOCK_TIME "
       " FROM " db-schema ".MESSAGE "
       " WHERE  lock_time=? and locked_by=?"))

(defmethod get-template-sql "h2" [{:keys [^String db-schema]}]
  (str "SELECT  "
       "NAME,"
       "USER_NAME,"
       "STRINGDECODE(HEADERS) AS HEADERS,"
       "STRINGDECODE(BODY) AS BODY,"
       "URL,"
       "MAX_RETRIES,"
       "RETRY_INTERVAL, "
       "METHOD "
       " FROM " db-schema ".HOOK WHERE NAME=? and "
       "(USER_NAME='" hook-global-user-name "' or USER_NAME=?) "
       " ORDER BY USER_NAME DESC"))


(defmethod get-all-available-templates-sql "h2" [{:keys [^String db-schema]}]
  (str "SELECT  "
       "NAME,"
       "USER_NAME,"
       "STRINGDECODE(HEADERS) AS HEADERS,"
       "STRINGDECODE(BODY) AS BODY,"
       "URL,"
       "MAX_RETRIES,"
       "RETRY_INTERVAL, "
       "METHOD "
       " FROM " db-schema ".HOOK WHERE  "
       " USER_NAME='" hook-global-user-name "' or USER_NAME=? "
       " ORDER BY USER_NAME DESC"))


(defmethod get-all-templates-sql "h2" [{:keys [^String db-schema]}]
  (str "SELECT  "
       "NAME,"
       "USER_NAME,"
       "STRINGDECODE(HEADERS) AS HEADERS,"
       "STRINGDECODE(BODY) AS BODY,"
       "URL,"
       "MAX_RETRIES,"
       "RETRY_INTERVAL, "
       "METHOD "
       " FROM " db-schema ".HOOK "))

(defmethod get-message-by-id-sql "h2" [{:keys [^String db-schema]}]
  (str "SELECT ID, URL, METHOD, STRINGDECODE(HEADERS) AS HEADERS,"
       "STRINGDECODE(BODY) AS BODY,"
       "ATTEMPT, "
       "RETRY_INTERVAL,"
       "NEXT_RUN,"
       "LOCKED_BY,"
       "LOCK_TIME FROM " db-schema ".MESSAGE WHERE ID=?"))

(defmethod post-message-sql "h2" [{:keys [^String db-schema]}]
  (str "INSERT INTO " db-schema ".MESSAGE"
       "(URL, HEADERS, BODY, METHOD, ATTEMPT, USER_NAME, RETRY_INTERVAL, ID, NEXT_RUN) "
       " VALUES(?, ?, ?, ?, ?, ?, ?, ?, SYSDATE)"))

(defmethod reschedule-msg-sql "h2" [{:keys [^String db-schema]}]
  (str "UPDATE " db-schema ".MESSAGE "
       "SET  locked_by=NULL,"
       "next_run = DATEADD(SECOND,retry_interval,next_run), "
       "attempt=attempt-1"
       "WHERE  id=?"))


(defmethod dump-message-queue-sql "h2" [{:keys [^String db-schema]}]
  (str "SELECT ID, URL, METHOD, STRINGDECODE(HEADERS) AS HEADERS,"
       "STRINGDECODE(BODY) AS BODY,"
       "ATTEMPT, "
       "RETRY_INTERVAL,"
       "NEXT_RUN,"
       "LOCKED_BY,"
       "LOCK_TIME FROM " db-schema ".MESSAGE"))

(defmethod log-message-delivery-sql "h2" [{:keys [^String db-schema]}]
  (str "INSERT INTO " db-schema ".MESSAGE_LOG(MES_ID, ATTEMPT,STATUS,URL, BODY, DELIVERY_TIME) "
       " VALUES( ?,?,?,?,?,?)"))

;; H2 methods END 

;; Postgesql methods
(defmethod lock-msg-on-time-sql "postgres" [{:keys [^String db-schema]}]
  (str "UPDATE " db-schema ".MESSAGE"
       " SET lock_time= ?::TIMESTAMP ,locked_by=?"
       " WHERE id IN "
       " (SELECT id FROM "  db-schema ".MESSAGE"
       " WHERE locked_by is NULL and next_run < CURRENT_TIMESTAMP and %s LIMIT ?)"))


(defmethod get-msg-on-time-sql "postgres" [{:keys [^String db-schema]}]
  (str "SELECT ID, URL, METHOD,  HEADERS,"
       "BODY,"
       "ATTEMPT, "
       "RETRY_INTERVAL,"
       "NEXT_RUN,"
       "LOCKED_BY,"
       "USER_NAME,"
       "LOCK_TIME "
       " FROM " db-schema ".MESSAGE "
       " WHERE  lock_time=  ?::TIMESTAMP and locked_by=?"))

(defmethod get-template-sql "postgres" [{:keys [^String db-schema]}]
  (str "SELECT  "
       "NAME,"
       "USER_NAME,"
       "HEADERS,"
       "BODY,"
       "URL,"
       "MAX_RETRIES,"
       "RETRY_INTERVAL, "
       "METHOD "
       " FROM " db-schema ".HOOK WHERE NAME=? and "
       "(USER_NAME='" hook-global-user-name "' or USER_NAME=?) "
       " ORDER BY USER_NAME DESC"))


(defmethod get-all-available-templates-sql "postgres" [{:keys [^String db-schema]}]
  (str "SELECT  "
       "NAME,"
       "USER_NAME,"
       "HEADERS,"
       "BODY,"
       "URL,"
       "MAX_RETRIES,"
       "RETRY_INTERVAL, "
       "METHOD "
       " FROM " db-schema ".HOOK WHERE  "
       " USER_NAME='" hook-global-user-name "' or USER_NAME=? "
       " ORDER BY USER_NAME DESC"))


(defmethod get-all-templates-sql "postgres" [{:keys [^String db-schema]}]
  (str "SELECT  "
       "NAME,"
       "USER_NAME,"
       "HEADERS,"
       "BODY,"
       "URL,"
       "MAX_RETRIES,"
       "RETRY_INTERVAL, "
       "METHOD "
       " FROM " db-schema ".HOOK "))

(defmethod get-message-by-id-sql "postgres" [{:keys [^String db-schema]}]
  (str "SELECT ID, URL, METHOD, HEADERS,"
       "BODY,"
       "ATTEMPT, "
       "RETRY_INTERVAL,"
       "NEXT_RUN,"
       "LOCKED_BY,"
       "LOCK_TIME FROM " db-schema ".MESSAGE WHERE ID=?"))

(defmethod post-message-sql "postgres" [{:keys [^String db-schema]}]
  (str "INSERT INTO " db-schema ".MESSAGE"
       "(URL, HEADERS, BODY, METHOD, ATTEMPT, USER_NAME, RETRY_INTERVAL, ID, NEXT_RUN) "
       " VALUES(?, ?, ?, ?, ?, ?, ?, ?, CURRENT_TIMESTAMP)"))

(defmethod reschedule-msg-sql "postgres" [{:keys [^String db-schema]}]
  (str "UPDATE " db-schema ".MESSAGE "
       "SET  locked_by=NULL,"
       "next_run = next_run + retry_interval * interval '1 second', "
       "attempt=attempt-1"
       "WHERE  id=?"))

(defmethod dump-message-queue-sql "postgres" [{:keys [^String db-schema]}]
  (str "SELECT ID, URL, METHOD, HEADERS,"
       "BODY,"
       "ATTEMPT, "
       "RETRY_INTERVAL,"
       "NEXT_RUN,"
       "LOCKED_BY,"
       "LOCK_TIME FROM " db-schema ".MESSAGE"))

(defmethod log-message-delivery-sql "postgres" [{:keys [^String db-schema]}]
  (str "INSERT INTO " db-schema ".MESSAGE_LOG(MES_ID, ATTEMPT,STATUS,URL, BODY, DELIVERY_TIME) "
       " VALUES( ?,?,?,?,?,?::TIMESTAMP)"))

;; Postgesql methods END

;; Oracle methods 


;; Oracle methods END 
;;
;; Database typed independed request. At least I think so. 
;;
;;


#_(defn message-reader-factory
    "Lock task for pusher  according to specified condition.
   According to prefetch strategy returns only just-locked tasks.
   Use get_task if you need all tasks locked by locker."
    [db-config]
    (let [lock-sql  (lock-msg-on-time-sql db-config)
          get-sql (get-msg-on-time-sql db-config)]
      (fn [^String messenger-id ^Integer chunk_size ^String condition]
        (let [lock-time (unixtime->timestamp (tod))]
          (jdbc/with-db-transaction [t-con @db]
            (when (< 0  (first  (jdbc/execute! t-con [(format lock-sql condition)
                                                      lock-time
                                                      messenger-id
                                                      chunk_size])))
              (jdbc/query   t-con [get-sql  lock-time messenger-id])))))))

(defn message-reader-factory
  "Lock task for pusher  according to specified condition.
   According to prefetch strategy returns only just-locked tasks.
   Use get_task if you need all tasks locked by locker."
  [db-config]
  (let [lock-sql  (lock-msg-on-time-sql db-config)
        get-sql (get-msg-on-time-sql db-config)]
    (fn [^String messenger-id ^Integer chunk_size ^String condition]
      (let [lock-time (unixtime->timestamp (tod))]
        (jdbc/with-db-transaction [t-con @db]
          (when (< 0  (first  (jdbc/execute! t-con
                                             [(format lock-sql condition)
                                              lock-time
                                              messenger-id
                                              chunk_size])))
            (jdbc/query   t-con [get-sql  lock-time messenger-id])))))))


(defn reschedule-msg-factory [dbconfig]
  (let [sql (reschedule-msg-sql dbconfig)]
    (fn [^String id]
      (jdbc/execute! @db  [sql id]))))


(defn delete-msg-factory [{:keys [^String db-schema]}]
  (let [message (keyword (str db-schema ".MESSAGE"))]
    (fn [^String id]
      (jdbc/delete! @db message ["id=?" id]))))


(defn delete-expired-msg-factory [{:keys [^String db-schema ^String db-type]}]
  (let [message (keyword (str db-schema ".MESSAGE"))]
    (case db-type
      "postgres"
      (fn [^Integer expire]
        (jdbc/delete! @db message ["locked_by is NULL and next_run<?::TIMESTAMP" (unixtime->timestamp (- (tod) (* expire 1000)))]))
      (fn [^Integer expire]
        (jdbc/delete! @db message ["locked_by is NULL and next_run<?" (unixtime->timestamp (- (tod) (* expire 1000)))])))))


(defn unlock-msg-factory
  "Just remove message lock"
  [{:keys [^String db-schema]}]
  (let [message (keyword (str db-schema ".MESSAGE"))]
    (fn [^String id]
      (jdbc/update!  @db message {:locked_by nil} [" id=?" id]))))

(defn clear-locks-factory
  "Remove lock mark from all unfinished message lock"
  [{:keys [^String db-schema]}]
  (let [message (keyword (str db-schema ".MESSAGE"))]
    (fn []
      (jdbc/update!  @db message {:locked_by nil} [" locked_by is not NULL"]))))


(defn cleanup-exited-worker-factory
  "Unlock all no delivered messages for worker.
   Return number of unlocked tasks"
  [{:keys [^String db-schema]}]
  (let [message (keyword (str db-schema ".MESSAGE"))]
    (fn [^String worker]
      (jdbc/update!  @db message {:locked_by nil}  ["locked_by=?" worker]))))



#_(defn post-message-factory
    "Add message to send queue"
    [{:keys [^String db-schema]}]
    (let [message (keyword (str db-schema ".MESSAGE"))]
      (fn [msg]
        (jdbc/insert!  @db message msg))))

#_(defn- clob->string [params index]
    (if (string? (params index))
      params
      (assoc params index (m/clob->string (params index)))))

(defn post-message-factory
  "Add message to send queue
   params is [url, headers, body, method, max_retries, user-name, retry_interval, id ]"
  [db-config]
  (case (:db-type db-config)
    "postgres"
    (let [sql  [(post-message-sql db-config)]]
      (fn [msg]
        (jdbc/execute!  @db (into sql (map val msg)))))
    (let [message (keyword (str (:db-schema db-config) ".MESSAGE"))]
      #_(fn [msg]
          #_(jdbc/insert!  @db message {:url (msg 0)
                                        :headers (msg 1)
                                        :body (msg 2)
                                        :method (msg 3)
                                        :max_retries (msg 4)
                                        :user_name (msg 5)
                                        :id (msg 6)
                                        :next_run (unixtime->timestamp (tod))}))
      (fn [msg]
        (jdbc/insert!  @db
                       message
                       (assoc msg :next_run (unixtime->timestamp (tod))))))))


(defn get-message-queue-length-factory
  "Get number of messages are waiting to send"
  [{:keys [^String db-schema]}]
  (fn []
    ((first (jdbc/query @db (format "SELECT COUNT(*) as QTY FROM %s.MESSAGE" db-schema))) :qty)))

(defn get-message-by-id-factory
  "Get particular message by id"
  [db-config]
  (let [sql (get-message-by-id-sql db-config)]
    (fn [id]
      (first (jdbc/query @db [sql  id])))))

(defn dump-message-queue-factory
  "Dump all the messages are waiting to send"
  [db-config]
  (let [sql (dump-message-queue-sql db-config)]
    (fn []
      (jdbc/query @db sql))))


(defn get-template-factory
  "Get hook template
   tag and user-name are fields of http-client/request options structure"
  [db-config]
  (let [sql (get-template-sql db-config)]
    (fn [{:keys [^String tag ^String user-name]}]
      ;(timbre/debug "Get template params " tag " " user-name)
      ; (timbre/debug "Get template query " sql)
      (when-not (nil? tag)
        (jdbc/query @db  [sql tag user-name])))))

#_(defn- get-all-templates-sql [{:keys [^String db-schema]}]
    (str "SELECT * FROM " db-schema ".HOOK"))

(defn get-all-templates-factory
  "Get all  hook templates"
  [db-config]
  (let [sql  (get-all-templates-sql db-config)]
    (fn []  (jdbc/query @db sql))))

(defn get-all-available-templates-factory
  "Get list of all templates available for user"
  [db-config]
  (let [sql (get-all-available-templates-sql db-config)]
    (fn [user-name]  (jdbc/query @db [sql user-name]))))


;;
;;  HOOKS DEFAULTS AND LIMITS
;;
;;
;;
(def ^:private max-max-retries  200)

(def ^:private min-retry-interval  10)

(defn- check-retries [hook]
  (let [max_retries (:max_retries hook)]
    (cond
      (nil? max_retries) (assoc hook :max_retries 1)
      (< max_retries 1)  (assoc hook :max_retries 1)
      (< max-max-retries max_retries)  (assoc hook :max_retries max-max-retries)
      :else hook)))

(defn- check-retry-interval [hook]
  (if (or (nil? (:retry_interval hook))
          (< (:retry_interval hook) min-retry-interval))
    (assoc hook :retry_interval min-retry-interval)
    hook))

(defn- check-method [hook]
  (case (str/lower-case (:method hook))
    ("put" "post" "get") hook
    nil (assoc hook :method "post")
    ((throw (AssertionError. (str "Incorrect or unsuppoted method in template" hook))))))

(defmacro ^:private validate [hook]
  `(-> ~hook
       check-retries
       check-method
       check-retry-interval))

(defn- insert-template-sql [{:keys [^String db-schema]}]
  (str "INSERT INTO " db-schema ".HOOK"
       " (NAME, USER_NAME, HEADERS, METHOD,BODY, MAX_RETRIES,RETRY_INTERVAL,URL)"
       " VALUES(?,?,?,?,?,?,?,? )"))

(defn add-hook-factory
  "Add hook template "
  [db-config]
  (let [sql (insert-template-sql db-config)]
    (fn [hook]
      (let [hook (validate hook)]
        ;; jdbc/insert tries to use all fields of passed structure -> sql error due to extra fields 
        (jdbc/execute!  @db   [sql (:name hook)
                               (:user_name hook)
                               (:headers hook)
                               (:method hook)
                               (:body hook)
                               (:max_retries hook)
                               (:retry_interval hook)
                               (:url hook)])))))


(defn update-hook-factory
  "Update hook template "
  [{:keys [^String db-schema]}]
  (let [table (keyword (str db-schema ".MESSAGE"))]
    (fn [hook]
      (jdbc/update!  @db table
                     (validate hook)
                     ["NAME=? AND USER_NAME=?"
                      (:name hook)
                      (:user_name hook)]))))


(defn update-or-insert-hook-factory [{:keys [^String db-schema]}]
  (let [table (keyword (str db-schema ".HOOK"))]
    (fn  [hook]
      (let [hook  (validate hook)]
        (jdbc/with-db-transaction [t-con @db]
          (when  (zero? (first (jdbc/update! t-con table hook ["NAME=? AND USER_NAME=?"
                                                               (:name hook)
                                                               (:user_name hook)])))
            (jdbc/insert! t-con table hook)))))))


(defn delete-hook-factory [{:keys [^String db-schema]}]
  (let [table (keyword (str db-schema ".HOOK"))]
    (fn [name user_name]
      (jdbc/delete! @db table ["NAME=? AND USER_NAME=?"
                               name user_name]))))


#_(defn log-message-delivery-factory [{:keys [^String db-schema ^String db-message-log]}]
    (when (= db-message-log "enable")
      (let [table (keyword (str db-schema ".MESSAGE_LOG"))]
        (fn [{:keys [id attempt url]} status body]
          (jdbc/insert! @db table {:mes_id id
                                   :attempt attempt
                                   :status status
                                   :url url
                                   :body body
                                   :delivery_time (unixtime->timestamp (tod))})))))

(defn log-message-delivery-factory [db-config]
  (when (= (:db-message-log db-config) "enable")
    (let [sql (log-message-delivery-sql db-config)]
      (fn [{:keys [id attempt url]} status body]
        (jdbc/execute! @db  [sql id attempt status url body (unixtime->timestamp (tod))])))))

(defn check-delivery-log-factory [{:keys [^String db-schema ^String db-message-log]}]
  (when (= db-message-log "enable")
    (let [sql (str "SELECT * FROM " db-schema ".MESSAGE_LOG WHERE MES_ID=?")]
      (fn [message-id]
        (first (jdbc/query @db [sql message-id]))))))


(defn configure [db-config]
  (try
    {:post (post-message-factory db-config)
     :get-template  (get-template-factory db-config)
     :get-all-templates (get-all-templates-factory db-config)
     :get-all-available-templates (get-all-available-templates-factory db-config)
     :add-template   (add-hook-factory db-config)
     :update-or-insert-template (update-or-insert-hook-factory db-config)
     :delete-template (delete-hook-factory db-config)
     :message-reader  (message-reader-factory db-config)
     :reschedule (reschedule-msg-factory db-config)
     :unlock (unlock-msg-factory db-config)
     :clear-locks (clear-locks-factory db-config)
     :delete  (delete-msg-factory db-config)
     :log-message-delivery  (log-message-delivery-factory db-config)
     :check-message-delivery  (check-delivery-log-factory db-config)
     :delete-expired (delete-expired-msg-factory db-config)
     :get-message-queue-length (get-message-queue-length-factory db-config)
     :cleanup (cleanup-exited-worker-factory db-config)}
    (catch  Exception e  (timbre/fatal "Database configuration error: hook configuration error - " (ex-message e))
            (stacktrace/print-stack-trace e))))
