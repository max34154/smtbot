(ns smtbot.utils.formater
  (:require
   [clojure.instant :as instant]))


(defmacro  time-formater 
  "
   Convert date in java.util.Date format into string  according to format string 
  "
  [^String time-val ^String format-string]
  `(some->> ~time-val
            instant/read-instant-date
            (format ~format-string)))

(defmacro time->sm-short-time 
   "
   Convert date in unixtime  format into SM date string dd.mm.yy HH:MI:SS 
  "
  [^String time-val]
  `(some->> ~time-val
    (new java.sql.Timestamp)
    (format  "%1$td.%1$tm.%1$ty %1$tH:%1$tM:%1$tS")))

#_(defn time->sm-short-time [time-val]
    (some->> time-val
             instant/read-instant-date
             (format "%1$td.%1$tm.%1$ty %1$tH:%1$tM:%1$tS")))