(ns smtbot.bot.users.validation-hook
  (:require
   [smtbot.utils.sm_resp_decode :refer [get-jbody]]
   [taoensso.timbre :as timbre]
   [smtbot.dal.globals :refer [botuser-action]]
   [smtbot.enum.sm :as sm]))




(defn validation-hook
  ([{:keys [opts body headers]}]
   (validation-hook opts nil body headers))
  ([opts _ body headers]
   (let [jbody (get-jbody body headers)]
     (if (= (get jbody "ReturnCode")  sm/RC_SUCCESS)
       (do
         (timbre/debug "User status recived " jbody)
         (-> jbody
             (get  "user")
             (#(@(delay (@botuser-action :update-by-key)) (% "key") (% "status") (% "role")))))
       (timbre/error "Request failed, responce "  body "request options " opts)))))