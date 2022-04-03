(ns smtbot.dal.user
  (:require [clojure.java.jdbc :as jdbc]
            [smtbot.validate]
            [smtbot.utils.crypto :as crypto :refer [encrypt decrypt]]
            [smtbot.dal.globals :as g :refer [db user-action]]
            [smtbot.utils.macro :refer [unixtime->timestamp tod-seconds]]))

(def default-session-length (* 60 2))
;; 
;; Database type dependend. Please add methods for each supported db 
;; 
(defmulti ^:private update-user-sql (fn [db-config] (:db-type  db-config)))

(defmethod update-user-sql :default [db-config]
  (throw (IllegalArgumentException.
          (str "Unsupported database type " (:db-type  db-config) "."))))

(defmulti ^:private insert-user-sql (fn [db-config] (:db-type  db-config)))

(defmethod insert-user-sql :default [db-config]
  (throw (IllegalArgumentException.
          (str "Unsupported database type " (:db-type  db-config) "."))))

(defmulti ^:private setup-user-update-time-sql (fn [db-config] (:db-type  db-config)))

(defmethod setup-user-update-time-sql :default [db-config]
  (throw (IllegalArgumentException.
          (str "Unsupported database type " (:db-type  db-config) "."))))

;; H2 methods 
(defmethod update-user-sql "h2" [db-config]
  (str "UPDATE " (:db-schema db-config) ".USER "
       " SET password=?, toc=?, expire_at=?"
       " WHERE key in SELECT * FROM TABLE(v VARCHAR =ARRAY[%s])"))

(defmethod insert-user-sql "h2" [db-config]
  (str "INSERT " (:db-schema db-config) ".USER(name, password, toc, expire_at) "
       " VALUES(?,?,?, ?)"))

(defmethod update-user-sql "h2" [db-config]
  (str "UPDATE " (:db-schema db-config) ".USER "
       " SET  update_at=?"
       " WHERE name=?"))

;; H2 methods END 

;; Postgesql methods
(defmethod update-user-sql "postgres" [db-config]
  (str "Update " (:db-schema db-config) ".USER "
       " SET password=?, toc=?, expire_at=?::TIMESTAMP"
       " WHERE name=?"))

(defmethod insert-user-sql "postgres" [db-config]
  (str "INSERT INTO " (:db-schema db-config) ".USER(name, password, toc, expire_at) "
       " VALUES(?,?,?, ?::TIMESTAMP )"))

;; Postgesql methods END


(defn- encrypt-row [{:keys [password name expire_at]}]
  (let [{:keys [data iv]} (encrypt password name)]
    {:name name :password data :toc iv :expire_at expire_at}))

(defn- decrypt-row [{:keys [name password toc expire_at]}]
  {:name name :val {:name name
                    :password (decrypt {:data password :iv toc} name)
                    :expire_at expire_at}})

(defn update-user-factory [db-config]
  (let [update-sql (update-user-sql db-config)
        insert-sql (insert-user-sql db-config)]
    (fn [row]
      (jdbc/with-db-transaction [t-con @db]
        (let [{:keys [name password toc expire_at]} (encrypt-row row)
              expire_at (unixtime->timestamp (* 1000
                                                (or
                                                 expire_at
                                                 (+ (tod-seconds)
                                                    default-session-length))))
              result (jdbc/execute! t-con [update-sql password toc expire_at name])]
          (if (zero? (first result))
            (jdbc/execute! t-con [insert-sql name password toc expire_at])
            result))))))

(defn get-all-user-factory [^String db-schema]
  (let [user-cache  (keyword (str db-schema ".user"))]
    (fn  []
      (reduce #(let [{:keys [name val]}  %2] (assoc %1 name val)) {}
              (jdbc/query @db  user-cache ;[(str  "SELECT * FROM " user-cache)]
                          {:row-fn decrypt-row})))))

(defn get-user-factory [^String db-schema]
  (let [sql (str  "SELECT * FROM " db-schema ".USER WHERE name=?")]
    (fn [name]
      (first (jdbc/query @db [sql name]
                         {:row-fn decrypt-row})))))

(defn delete-user-factory [^String db-schema]
  (let [user-cache  (keyword (str db-schema ".user"))]
    (fn [name]
      (jdbc/delete! @db  user-cache ["name=?" name]))))

(defn delete-user-cache-factory [^String db-schema]
  (let [user-cache  (keyword (str db-schema ".user"))]
    (fn []
      (jdbc/delete! @db  user-cache ["true"]))))

(defn configure [{:keys [^String db-schema] :as db-config}]
  {:update (update-user-factory db-config)
   :get-all (get-all-user-factory db-schema)
   :get-by-name (get-user-factory db-schema)
   :delete-by-name (delete-user-factory db-schema)
   :delete-user-cache (delete-user-cache-factory db-schema)})

(def get-user (delay (@user-action :get-by-name)))

(def update-user (delay (@user-action :update)))

(def delete-user (delay (@user-action :delete-by-name)))