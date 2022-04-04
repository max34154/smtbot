(ns smtbot.cache.acl-checker
  (:require
   [taoensso.timbre :as timbre]
   [clojure.string :as str]
   [smtbot.bot.messages.tmessage :as tmessage]))

(defn check-access-fabric
  " 
   Access list checkers fabric. 
   Check if user data in request confirm access list requirments
   Paramters:
    - role - role 
    - access-list - callback access list 
    - item - requested data
   Return:
    fn[ request] - return true/false 
  "
  [role  access-list  item]
  (if-let [[path transform] (get access-list role)]
    (do
      (timbre/debug "ACL parameters path " path " transform " transform)
      (case transform
        :user-mail (fn [request]
                     (timbre/debug "Check: user mail  " (tmessage/get-user-mail request)
                                   "item mail " (str/lower-case (get-in item path)))
                     (= (str/lower-case (tmessage/get-user-mail request)) (str/lower-case (get-in item path))))
        :operator-id  (fn [request]
                        (timbre/debug "Check: user operator id  " (str/lower-case (tmessage/get-operator-id request))
                                      "item operator id " (str/lower-case (get-in item path)))
                        (= (str/lower-case (tmessage/get-operator-id request)) (str/lower-case (get-in item path))))
        :always (fn [_] true)
        (do
          (timbre/error "Unknown transform function:" transform)
          (fn [_] false))))
    (do
      (timbre/warnf "Access list %s does not entry for role %s. Access denied." access-list role)
      (fn [_] false))))