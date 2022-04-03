(ns smtbot.bot.users.operations
  (:require [smtbot.utils.macro :refer [tod]]
            [smtbot.dal.globals :refer [botuser-action]]
            [taoensso.timbre :as timbre]
            [smtbot.bot.users.globals :refer [user-validation-mode]]
            [smtbot.bot.users.user-cache :as user-cache]
            [smtbot.bot.users.validate :as v]))


(defn set-role [obj {:keys [role smkey]}]
  (assoc obj :user-smkey smkey :user-role role))



;;  User record structure
;;  status - current user status 
;;            A - active, confirmed 
;;            B - blocked, 
;;            N - not-found 
;;            M - non-unique record 
;;            C<ConfirmationCode> - active, confirmation required
;;  role  - only for active confirmed 
;;         user  - listed in sm contacts 
;;         operator - has sm operator record 
;; expire_at - user record expiration date  
;;

(defn get-user-status
  "
   Analize current status of user based on found user record.
   Return:
   :user-not-found - user does not exits 
   :user-invalid - user record expired 
   :user-ok - user active 
   :user-blocked - user blocked 
   :reg-in-progress - user registration process not finished yet. 
  "
  [user]
  (cond (nil? user) :user-not-found
        (and (some? (user :expire_at))
             (< (.getTime (user :expire_at)) (tod))) :user-invalid
        :else (case (user :status)
                ("A" nil) :user-ok
                "B" :user-blocked
                ("N" "M") :user-not-found
                :reg-in-progress)))



(defn authorize
  "
   Role base athorization. 
   Paramaters: required role, keyword or set  
                user record.
   Return true if user comply required role. 
   Special consideration:   
   - if required role is nill or :any authorization always granted (true)
   -XXXdel if user's role does not comply requirments directly, 
      role equive map is applyed to the role to get mapped role 
      then attempt authorize  with the mapped role  -XXX 
   
  "
  [required-role {role :role}]
  (cond
    (nil? required-role) true
    (= required-role :any) true
    (nil? role) false
    (keyword? required-role)  (= required-role role) #_(or (= required-role role)
                                                           (some? (get (role-equive role) required-role)))
    :else (some? (get required-role role))
    #_(or (some? (required-role role))
          (if (some-> (role-equive role)
                      (clojure.set/intersection (hash-set :a :b))
                      seq) true false))))



(defn get-user
  "
   Get user by id 
   If in cache and not expired - from cache 
   Otherwise from db and update cache for user with stable status (A and B)
  "
  [user-id]
  (let [user-id (str user-id)
        user (user-cache/get-by-id user-id)]
    (if (and (some? user) (> (.getTime (user :expire_at)) (tod)))
      user
      (when-let [user (some-> ((@botuser-action :get-by-id) user-id)
                              (update :role keyword))]
        (timbre/debug "Prepare user to update cache:" user)
        (case (:status user)
          (nil "A" "B") (user-cache/add user)
          user)))))

(defn init-registration
  "
   Check current user status and if possible start registration procedure
   by sending request to SM 
   Return 
    :continue-registration - request has been sand  or scheduled to send if user-validation-mode is async 
    :already-registred - user with specified smkey already exists 
    :check-progress - request scheduled but not proccessed yet, async mode only
  "
  [user-id smkey]
  (case  (-> user-id get-user get-user-status)
    (:user-ok  :user-invalid :user-blocked) :already-registred
    :reg-in-progress :check-progress
    :user-not-found (do
                      (@(delay (@botuser-action :insert-or-update)) (str user-id) smkey)
                      (if (= user-validation-mode "sync")
                        (v/registrate-user smkey)
                        (v/request-reqistration smkey))
                      :continue-registration)))

(defn confirm-registration
  "
   Check supplied confirmation code. 
   If OK run user validation and return true 
   Otherwise return nil 
  "
  [user-id reg-code]
  (let [user   ((@botuser-action :get-by-id) (str user-id))
        status (user :status)]
    (when (and (not-empty status)
               (= (subs status 1) reg-code))
      (if (= user-validation-mode "sync")
        (v/validate-user (user :smkey))
        (v/request-validation (user :smkey)))
      true)))


(defn unreg
  "Delete user from user cache and database"
  [user-id]
  (let [user-id (str user-id)]
    (user-cache/remove-by-id user-id)
    ((@botuser-action :delete-by-id) user-id)))




