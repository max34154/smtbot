(ns smtbot.bot.query-processor
  (:require [clojure.string :as str]
            [smtbot.utils.macro :refer [tod]]
            [smtbot.utils.formater :as ftr]))

(def ticks-in-minute (* 60 1000))

(def ticks-in-day (* 60000 60 24))



(defmacro tod-trim [ticks]
  `(* (quot (tod) ~ticks) ~ticks))

;; Query macros list 
;; $user - user sm key.
;; $ctime - current time
;; $tod - current date
;; $regN - N-th item in vals vector, staring from 1

(def regPatterns (reduce (fn [ar i] (conj ar (re-pattern (str "\\$reg" i "\\$"))))   [] (range 20)))

(defn query-processor
  "
   Create query  using query template and values vector by macro subsitution.
   Use AFTER athorizaion if athorizaion required. 
   All $regN macros having not value in vector replaced by empty string
   Parameters: 
    query - query template (callback :query)
    item  - inline obj | command obj | menu-callback obj
    vals  - string vector, contains values to replace $regN$, 
            ATTENTION: only values in range [1 19] are used by query-processor     
    
  "
  ([query item]
   (-> query
       (str/replace #"\$user\$" (str "\"" (item :user-smkey) "\""))
       (str/replace #"\$ctime\$" (str "'" (ftr/time->sm-short-time  (tod-trim ticks-in-minute)) "'"))
       (str/replace #"\$tod\$" (str "'" (ftr/time->sm-short-time  (tod-trim ticks-in-day)) "'"))))

  ([query item  vals]
   (str/replace
    ((reduce (fn [{:keys [query pos]} val]
              {:query (str/replace query (regPatterns pos) val)
               :pos (inc pos)})
            {:query (query-processor query item) :pos 1}
            vals) :query)
    #"\$reg\d+\$" "")))
