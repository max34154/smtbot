(ns smtbot.bot.users.user-cache
  (:require [smtbot.bot.users.globals :refer [user-lifetime
                                              user-cache
                                              default-cache-clean-period
                                              user-validation-chank
                                              user-validation-margin
                                              user-validation-mode]]
            [taoensso.timbre :as timbre]
            [smtbot.dal.globals :refer [botuser-action]]
            [smtbot.bot.users.validate :refer [validate-user request-validation]]
            [clojure.core.async :as a]
            [smtbot.utils.macro :refer [tod expire-at-timestamp-seconds]]))


;; User cache structure
;;     user-id  - keyword(user id)
;;               {:id  - user id
;;               :smkey - user unique key in sm
;;               :role - user role 
;;               :status - user status 
;;               :expire_at - expiration time in ticks 
;;      :key-id-map { smkey  id} - smkey and id, both keyword 
;;                  


(defn- user-cache-remove [user-cache user-id]
  (let [user-id (keyword user-id)
        user-key (-> user-cache user-id :smkey keyword)]
    (-> user-cache
        (dissoc user-id)
        (update :key-id-map dissoc user-key))))

(defn- user-cache-create [user-cache user]
  (let [id (keyword (user :id))]
    (-> user-cache
        (assoc  id (update user :role keyword))
        (update :key-id-map assoc (keyword (user :smkey)) id))))

(defn user-cache-update-by-id [user-cache {:keys [id status]}]
  (update user-cache  (keyword id)
          #((assoc % :status status  :expire_at (+ (tod) (* 1000 user-lifetime))))))


(defn- user-cache-update-by-key [user-cache, {:keys [smkey status role]}]
  (if-let [id (-> user-cache :key-id-map (get (keyword smkey)))]
    (update user-cache  id
            #((assoc % :status status
                     :role  (keyword role)
                     :expire_at (+ (tod) (* 1000 user-lifetime)))))
    (do
      (timbre/errorf "Unable to update cache with %s : smkey not found ")
      user-cache)))

#_{:clj-kondo/ignore [:redefined-var]}
(defn user-cache-reset [] 
   (reset! user-cache {:key-id-map {}}))

(defn add
  "
   Add new user to cache by create user or by reading from database.
   Use only after check if the user exits in database. 
  "
  [user] {:pre [(some? (:id user)) (some? (:smkey user))]}
  (swap! user-cache user-cache-create user)
  user)

(defn remove-by-id
  "
    Remove user from cache and db. 
    Use if you need delete user to free smkey linked to id 
  "
  [user-id]
  (swap! user-cache user-cache-remove  user-id))

(defn remove-by-smkey
  "
   Nice to have :) Have no idea about usage pattern
  "
  [user-smkey]
  (when-let [id (-> user-cache :key-id-map (get (keyword user-smkey)))]
    (swap! user-cache user-cache-remove  id)))

(defn update-by-id
  "
   Update user status by known id. 
   Attention:the function  update status and expire_at only.
   To restore cache from db use function add 
  "
  [user] {:pre [(some? (:id user))]}
  (swap! user-cache user-cache-update-by-id user))

(defn update-by-smkey
  "
   Update user status/role/expe by known smkey in cache and user db 
   Attention:the function  update status, role and expire_at only.
  "
  [{:keys [smkey status role] :as user}] {:pre [(some? (:smkey user))]}
  (swap! user-cache user-cache-update-by-key user)
  ((@botuser-action :update-by-key)  smkey status role))



(defn squeze
  "
   Remove expired items from cache 
  "
  [user-cache]
  (-> (into {}  (filter
                 #(and (some? (:expire_at (% 1))) (> (:expire_at (% 1)) (tod)))
                 user-cache))
      (#(assoc % :key-id-map
               (reduce (fn [a [k v]] (assoc a (-> v :smkey keyword)  k))
                       {} %)))))


(defn get-by-id [id] (->> id keyword (get @user-cache)))

(defonce cache-clean-period (atom default-cache-clean-period))


(defn set-cache-clean-period
  "
    Setup cache clean period. It could not be more than  default-cache-clean-period.
  "
  [period]
  (let [period (* 1000 period)]
    (when (< period default-cache-clean-period)
      (reset! cache-clean-period period))))

(defn stop-cleaner []
  (reset! cache-clean-period 0))

;; !! If validation fail - botuser record freezed in update in progress 
;; !! It's not a part of normal operation but worth late considiration. 
(defn botuser-cache-manager []
  (timbre/reportf "Botuser care process started. Activation period is %s "
                  @cache-clean-period)
  (a/thread
    (loop [x @cache-clean-period]
      (when (pos-int? x)
        (swap! user-cache squeze)
        (if (= user-validation-mode "sync")
          (doseq [smkey ((@botuser-action :get-require-update)
                         (expire-at-timestamp-seconds (- user-validation-margin))
                         user-validation-chank)] 
             (validate-user smkey))
          
          ((@botuser-action :set-update-time)
           (reduce #((request-validation %2)
                     (into %1 %2))
                   []
                   ((@botuser-action :get-require-update)
                    (expire-at-timestamp-seconds (- user-validation-margin))
                    user-validation-chank))))
        (Thread/sleep x)
        (recur @cache-clean-period)))
    (timbre/report "Cleaner exited.")))


