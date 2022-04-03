(ns smtbot.bot.users.globals)

(def user-lifetime (* 3600 24))

(defonce user-cache (atom {:key-id-map {}}))

(def user-validation-margin 
  "
   User eligable for update during this period before expire time. 
   In seconds
  "
  3600)

(def user-validation-chank  100)

(def user-validation-mode "sync")

(def default-cache-clean-period   (- user-lifetime user-validation-margin))

#_(def role-filter 
  "
   Roles with data-depended authorization 
  " 
  (hash-set [:assignee :contact]))

#_(def role-equive
  "
   Mapping role eqivialence map 
    
  "
  {:user (hash-set :contact)
   :operator (hash-set :assignee)})


