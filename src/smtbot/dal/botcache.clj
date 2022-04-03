(ns smtbot.dal.botcache
  (:require [clojure.java.jdbc :as jdbc]
            [smtbot.validate]
            [smtbot.dal.globals :as g :refer [db]]
            [taoensso.timbre :as timbre]
            [clojure.stacktrace :refer [print-stack-trace]]
            [smtbot.utils.macro :refer [unixtime->timestamp tod]]))

;; 
;; Database type dependend. Please add methods for each supported db 
;; 
(defmulti ^:private get-item-sql (fn [db-config] (:db-type  db-config)))

(defmethod get-item-sql :default [db-config]
  (throw (IllegalArgumentException.
          (str "Unsupported database type " (:db-type  db-config) "."))))

(defmulti ^:private insert-item-sql (fn [db-config] (:db-type  db-config)))

(defmethod insert-item-sql :default [db-config]
  (throw (IllegalArgumentException.
          (str "Unsupported database type " (:db-type  db-config) "."))))

(defmulti ^:private update-item-sql (fn [db-config] (:db-type  db-config)))

(defmethod update-item-sql :default [db-config]
  (throw (IllegalArgumentException.
          (str "Unsupported database type " (:db-type  db-config) "."))))


;; H2 methods 
(defmethod get-item-sql "h2" [db-config]
  (str "SELECT STRINGDECODE(ITEM_JSON) AS ITEM_JSON FROM " (:db-schema db-config) ".BOTCACHE "
       "WHERE CALLBACK=? AND ITEM_KEY=? AND EXPIRE_AT > SYSDATE"))

(defmethod insert-item-sql "h2" [db-config]
  (str "INSERT INTO " (:db-schema db-config) ".BOTCACHE(ITEM_JSON,ITEM_KEY, CALLBACK, EXPIRE_AT)"
       " VALUES(?,?,?,?)"))

(defmethod update-item-sql "h2" [db-config]
  (str "UPDATE " (:db-schema db-config) ".BOTCACHE "
       " SET ITEM_JSON=?, EXPIRE_AT=?"
       " WHERE CALLBACK=? AND ITEM_KEY=?"))


;; H2 methods END 
;; Postgesql methods

(defmethod get-item-sql "postgres" [db-config]
  (str "SELECT ITEM_JSON  FROM " (:db-schema db-config) ".BOTCACHE "
       "WHERE CALLBACK=? AND ITEM_KEY=? AND EXPIRE_AT > CURRENT_TIMESTAMP"))

(defmethod insert-item-sql "postgres" [db-config]
  (str "INSERT INTO " (:db-schema db-config) ".BOTCACHE(ITEM_JSON,ITEM_KEY, CALLBACK, EXPIRE_AT)"
       " VALUES(?,?,?,?::TIMESTAMP)"))

(defmethod update-item-sql "postgres" [db-config]
  (str "UPDATE " (:db-schema db-config) ".BOTCACHE "
       " SET ITEM_JSON=?, EXPIRE_AT=?::TIMESTAMP"
       " WHERE CALLBACK=? AND ITEM_KEY=?"))


;; Postgesql methods END

(defn- get-item-factory [db-config]
  (let [sql  (get-item-sql db-config)]
    (fn [callback item-key]
      (:item_json (first (jdbc/query @db [sql (name callback) (name item-key)]))))))


(defn- update-or-insert-item-factory [db-config]
  (let [insert-sql (insert-item-sql db-config)
        update-sql (update-item-sql db-config)]
    (fn  [callback item-key item-json expire-at]
      (jdbc/with-db-transaction [t-con @db]
        (when  (zero? (first (jdbc/execute! t-con
                                            [update-sql  item-json expire-at (name callback)  (name item-key)])))
          (jdbc/execute! t-con [insert-sql item-json  (name item-key) (name callback) expire-at]))))))

(defn- delete-expired-item-factory [{:keys [^String db-schema ^String db-type]}]
  (let [table (keyword (str db-schema ".BOTCACHE"))]
    (case db-type
      "postgres" (fn [offset] (jdbc/delete! @db table ["EXPIRE_AT < ?::TIMESTAMP"
                                                       (unixtime->timestamp (- (tod) (* 1000 offset)))]))
      "h2" (fn [offset] (jdbc/delete! @db table ["EXPIRE_AT < ?"
                                                 (unixtime->timestamp (- (tod) (* 1000 offset)))]))
      (throw (IllegalArgumentException.
              (str "Unsupported database type " db-type "."))))))


(defn configure [db-config]
  (try
    {:get-item (get-item-factory db-config)
     :update-item (update-or-insert-item-factory db-config)
     :delete-expired (delete-expired-item-factory db-config)}
    (catch  Exception e  (timbre/fatal "Database configuration error: hook configuration error - " (ex-message e))
            (print-stack-trace e))))