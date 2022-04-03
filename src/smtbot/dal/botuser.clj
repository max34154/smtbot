(ns smtbot.dal.botuser
  (:require [clojure.java.jdbc :as jdbc]
            [smtbot.bot.users.globals :as ug]
            [smtbot.dal.globals :as g :refer [db]]
            [clojure.string :as str]
            [taoensso.timbre :as timbre]
            [smtbot.utils.macro :refer [unixtime->timestamp
                                        tod
                                        expire-at-timestamp-seconds]]))

(def default-session-length ug/user-lifetime)
;; 
;; Database type dependend. Please add methods for each supported db 
;; 
(defmulti ^:private update-botuser-by-key-sql (fn [db-config] (:db-type  db-config)))

(defmethod update-botuser-by-key-sql :default [db-config]
  (throw (IllegalArgumentException.
          (str "Unsupported database type " (:db-type  db-config) "."))))


(defmulti ^:private setup-botuser-update-time-sql (fn [db-config] (:db-type  db-config)))

(defmethod setup-botuser-update-time-sql :default [db-config]
  (throw (IllegalArgumentException.
          (str "Unsupported database type " (:db-type  db-config) "."))))

(defmulti ^:private get-require-update-sql (fn [db-config] (:db-type  db-config)))

(defmethod get-require-update-sql :default [db-config]
  (throw (IllegalArgumentException.
          (str "Unsupported database type " (:db-type  db-config) "."))))

;; H2 methods 


(defmethod update-botuser-by-key-sql "h2" [db-config]
  (str "UPDATE " (:db-schema db-config) ".BOTUSER "
       " SET status=?, expire_at=?, role=?,  update_at=NULL"
       " WHERE smkey=?"))

(defmethod setup-botuser-update-time-sql "h2" [db-config]
  (str "UPDATE " (:db-schema db-config) ".BOTUSER "
       " SET  update_at=?"
       " WHERE id=?"
       " WHERE smkey IN (SELECT * FROM TABLE(v VARCHAR =ARRAY['%s']))"))

(defmethod get-require-update-sql "h2" [db-config]
  (str "SELECT smkey FROM " (:db-schema db-config) ".BOTUSER "
       " WHERE update_at IS NULL AND  (expire_at IS NULL OR expire_at < ? )"
       " ORDER BY expire_at ASC "
       " LIMIT ?"))

;; H2 methods END 

;; Postgesql methods
(defmethod update-botuser-by-key-sql "postgres" [db-config]
  (str "UPDATE " (:db-schema db-config) ".BOTUSER "
       " SET status=?,  expire_at=?::TIMESTAMP, role=?, update_at=NULL"
       " WHERE smkey=?"))


(defmethod setup-botuser-update-time-sql "postgres" [db-config]
  (str "UPDATE " (:db-schema db-config) ".BOTUSER "
       " SET update_at=?::TIMESTAMP"
       " WHERE smkey IN ('%s')"))

(defmethod get-require-update-sql "postgres" [db-config]
  (str "SELECT smkey FROM " (:db-schema db-config) ".BOTUSER "
       " WHERE update_at IS NULL AND  (expire_at IS NULL OR expire_at < ?::TIMESTAMP)"
       " ORDER BY expire_at ASC "
       " LIMIT ?"))

;; Postgesql methods END


(defn insert-or-update-botuser-factory
  [^String db-schema]
  (let [insert-sql (str "INSERT INTO " db-schema ".BOTUSER(id, smkey, status ) VALUES(?,?, 'W')")
        update-sql (str "UPDATE " db-schema ".BOTUSER SET status = 'W', expire_at = NULL WHERE  id=? AND smkey=?")]
    (fn [id smkey]
      (let [smkey (str/lower-case smkey)]
      (jdbc/with-db-transaction [t-con @db]
        (when  (zero? (first (jdbc/execute! t-con
                                            [update-sql id smkey])))
          (jdbc/execute! t-con [insert-sql
                                 id
                                 smkey])))))))

(defn update-botuser-by-key-factory [db-config]
  (let [update-sql (update-botuser-by-key-sql db-config)]
    (fn [smkey status role]
      (jdbc/execute! @db [update-sql
                          status
                          (expire-at-timestamp-seconds default-session-length)
                          role
                          (str/lower-case smkey)]))))




(defn setup-botuser-update-time-factory [db-config]
  (let [update-sql (setup-botuser-update-time-sql db-config)]
    (fn [key-list]
      (timbre/debug "update-sql->" (format update-sql
                       (str/join "','" key-list)))
      (jdbc/execute! @db [(format update-sql (str/join "','" key-list))  
                          (unixtime->timestamp (tod))]))))


(defn get-require-update [db-config]
  (let [sql (get-require-update-sql db-config)]
    (fn  [expire_at limit]
      (timbre/debug "get-require-update-sql->" sql)
      (jdbc/query @db  [sql expire_at limit]))))

(defn get-all-botuser-factory [^String db-schema]
  (let [sql (str  "SELECT * FROM " db-schema ".BOTUSER")]
    (fn  []
      (jdbc/query @db  sql))))

(defn get-botuser-by-id-factory [^String db-schema]
  (let [sql (str  "SELECT * FROM " db-schema ".BOTUSER WHERE id=?")]
    (fn [^String  id]
      (first (jdbc/query @db [sql id])))))

(defn get-botuser-by-key-factory [^String db-schema]
  (let [sql (str  "SELECT * FROM " db-schema ".BOTUSER WHERE smkey=?")]
    (fn [^String smkey]
      (first (jdbc/query @db [sql (str/lower-case smkey)])))))

(defn delete-botuser-by-id-factory [^String db-schema]
  (let [user-cache  (keyword (str db-schema ".botuser"))]
    (fn [^String id]
      (jdbc/delete! @db  user-cache ["id=?" id]))))

(defn delete-botuser-by-key-factory [^String db-schema]
  (let [user-cache  (keyword (str db-schema ".botuser"))]
    (fn [^String smkey]
      (jdbc/delete! @db  user-cache ["smkey=?" (str/lower-case smkey)]))))

(defn delete-botuser-cache-factory [^String db-schema]
  (let [user-cache  (keyword (str db-schema ".botuser"))]
    (fn []
      (jdbc/delete! @db  user-cache ["true"]))))

(defn configure [{:keys [^String db-schema] :as db-config}]
  {:insert-or-update (insert-or-update-botuser-factory db-schema) ;;test +
   :update-by-key (update-botuser-by-key-factory db-config) ;;test +
   :set-update-time (setup-botuser-update-time-factory db-config) ;;test +
   :get-require-update (get-require-update db-config)
   :get-all (get-all-botuser-factory db-schema) ;;test +
   :get-by-id (get-botuser-by-id-factory db-schema) ;;test +
   :get-by-key (get-botuser-by-key-factory db-schema) ;;test +
   :delete-by-id (delete-botuser-by-id-factory db-schema) ;;test +
   :delete-by-key (delete-botuser-by-key-factory db-schema) ;;test +
   :delete-botuser-cache (delete-botuser-cache-factory db-schema)})

