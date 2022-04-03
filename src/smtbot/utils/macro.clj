(ns smtbot.utils.macro
  {:clj-kondo/config  '{:linters {:unused-public-var {:level :off}}
                        :clojure-lsp/unused-public-var {:level :off}}})


;{:exclude {taoensso.timbre [log  trace  debug  info  warn  error  fatal  report
;                            logf tracef debugf infof warnf errorf fatalf reportf
;                            spy get-env]}}
;
#_{:clj-kondo/ignore [:unused-public-var]}
(defmacro _qoute [_str] `(str "'" ~_str "'"))



(defn- case_list  [cases]
  (let [lng (count cases)]
    (map (fn [a b]
           (if (even? b)  a
               (if (= b lng) a (eval a))))
         cases
         (iterate inc 1))))

(defmacro _case [v & cases]
  (conj (case_list   cases) v 'case))

(defmacro tod []
  `(System/currentTimeMillis))

(defmacro tod-seconds []
  `(quot (System/currentTimeMillis) 1000))

(defmacro unixtime->timestamp [t]
  `(.toString (new java.sql.Timestamp ~t)))

(defmacro get-channel-id [ch]
  `((str/split (pr-str ~ch) #" ") 1))

(defmacro expire-at-timestamp-seconds
  "
   Calc expire times stamp like exp-time, or current time + exp-offset, if exp-time nil or ommited 
   both parameters should be in seconds  
  "
  ([exp-offset]
   `(smtbot.utils.macro/unixtime->timestamp (+ (System/currentTimeMillis) (* 1000 ~exp-offset))))
  ([exp-time, exp-offset]
   `(smtbot.utils.macro/unixtime->timestamp (* 1000 (or   ~exp-time
                                                          (+ (tod-seconds) ~exp-offset))))))

(defmacro expire-at-timestamp
  "
   Calc expire times stamp like exp-time, or current time + exp-offset, if exp-time nil or ommited 
   both parameters should be in ms   
  "
  ([exp-offset]
   `(smtbot.utils.macro/unixtime->timestamp (+ (System/currentTimeMillis) ~exp-offset)))
  ([exp-time, exp-offset]
   `(smtbot.utils.macro/unixtime->timestamp  (or   ~exp-time (+ (tod) ~exp-offset)))))


(defmacro resp-data [resp]
  `(str  (#(str  (:thread %1) ": rec-id " (:rec-id %1))
          (:opts ~resp))
         " status " (:status ~resp)))

(defmacro thread-group [thread]
  `((str/split ~thread #"/" 2) 0))


(defmacro remove-athorization [req]
  `(update-in ~req [:headers "authorization"] (constantly "XXXXXXXXXX")))

(defmacro if-do [value condition func]
  `(if (-> ~value  ~condition) (-> ~value  ~func) ~value))
