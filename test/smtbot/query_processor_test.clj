(ns smtbot.query-processor-test
  #_{:clj-kondo/ignore [:refer-all]}
  (:require [clojure.test :refer :all]
            [smtbot.bot.query-processor :as qp]))


(def test-item {:user-key "test-user"})

(def user-query "user=$user$")

(def current-time-query "time < $ctime$")

(def tod-query "time > $tod$")

(def complex-query "user=$user$ and var=\"$reg0$\"  and var=\"$reg1$\" and var2=\"$reg10$\"")

(def test-vals ["val1"  "val2" "val3" "val4" "val5" "val6" "val7" "val8" "val9" "val10" "val11" ])


(deftest converters-test  
   )

(deftest test-query-processor
  (testing "Simple queries"
    (is (= "user=\"test-user\"" (qp/query-processor user-query test-item)))
    (is (some? (re-find #"time < '\d+.\d+.\d+ \d+:\d+:\d+'" (qp/query-processor current-time-query test-item)))
    (is (some? (re-find #"time > '\d+.\d+.\d+ \d+:\d+:\d+'" (qp/query-processor tod-query test-item))))))
  (testing "Complex query"
    (is (= "user=\"test-user\" and var=\"\"  and var=\"val1\" and var2=\"val10\""
         (qp/query-processor complex-query test-item test-vals)))))


