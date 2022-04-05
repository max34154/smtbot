(ns smtbot.bot.messages.synonim
  (:require
   [smtbot.bot.globals :refer [bot]]
   [smtbot.bot.users.operations :as user-ops]
   [clojure.string :as str]))


(defn check-synonim-list
  "
   Check if command in synonim list 
  "
  [command]
  ;(when-let [command (first (str/split text #"\s+"))]
    (->> command
         (re-find #"^(\D+).*")
         rest
         first
         keyword
         ((@bot :synonim))));)

(defn select-synonim-by-role 
  "
    Select synonim extention by role.
    If not found return default value
  "
  [synonim {user-id :id}]
  (if-let [role-synonim  (synonim :role-base)]
    (some-> user-id
            user-ops/get-user
            :role
            role-synonim
            (#(if (some? %) % synonim)))
    synonim))

(defn transform-command-text [synonim text]
  (let [[command options] (str/split text #"\s+")]
    (if (some? (synonim  :item-reg))
      (str (synonim :command) " " options " " (first (rest (re-find (synonim  :item-reg) command))))
      (str (synonim :command) " " options))))

