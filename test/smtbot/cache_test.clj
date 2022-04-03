(ns smtbot.cache-test
  (:require [clojure.test :refer [testing  deftest  is use-fixtures]]
            #_[smtbot.utils.reflector :refer [reflector-start
                                              relector-set-responce
                                              reflector-stop]]
            [smtbot.dal.configure :refer [configure-database execute-script]]
            [smtbot.bot.config :as bot]
            [smtbot.cache.cache :refer [get-item test-item-L1 test-item-L2]]
            [smtbot.config :as config]
            [smtbot.bot.messages.tmessage  :as tmessage :refer [TMessage]]
            [clojure.stacktrace :as clojure.stacktrace]))


(defonce TestResult (atom {}))


(def test-response     {:status 200
                        :headers {"content-type" "application/json; charset=UTF-8"}
                        :body "{ \"Interaction\": { \"CallbackContact\": \"Абашкина Мария Александровна\", \"Category\": \"консультация\", \"CompanyGRBS\": \"Префектура ЮВАО\", \"CompanyINN\": \"7721573836\", \"CompanyKPP\": \"772101001\", \"CompanyName\": \"ГБУ ЦКДС ИСТОКИ\", \"ContactEmail\": \"AbashkinaMA@puvao.mos.ru\", \"ContactName\": \"Абашкина Мария Александровна\", \"ContactNameUserID\": \"591365\", \"Description\": [ \"test description\", \"line2\", \"error ЕИС-500\" ], \"Direction\": \"ЕАИСТ\", \"Group\": \"ОМ ЕАИСТ\", \"InteractionID\": \"SD1698508\", \"OpenTime\": \"2022-01-07T13:25:08+03:00\", \"Source\": \"5\", \"StatusForUser\": \"open\", \"Title\": \"test title\" }, \"Messages\": [], \"ReturnCode\": 0 }"})

(defn fix-read-config [t]
  (try
    (config/configure "test/config/run/")
    (configure-database)
    (execute-script "TRUNCATE TABLE TBOT.BOTCACHE;")
    (bot/configure "test/config/run/")
    ;;(reset! TestResult {})
    ;;(reflector-start)
    (t)
    (catch Exception e (println (ex-message e))
           (clojure.stacktrace/print-stack-trace e))
    ;;(finally (reflector-stop))
    ))

(defn fix-test-result-cleanup [t]
  (reset! TestResult {})
  (t))

(use-fixtures :once fix-read-config)

(use-fixtures :each fix-read-config)

(def test-item-key "SD1848894")

(def test-wrong-item
  {:item-key "XSD1698508"
   :callback :getSD
   :id "test-inline-id"})

(def test-inline-item
  {:item-key test-item-key
   :callback :getSD
   :user-role :operator
   :user-smkey "AbashkinaMA@puvao.mos.ru"
   :id "test-inline-id"})

(def test-inline-query
  {:sm-query "ContactEmail= \"AbashkinaMA@puvao.mos.ru\""
   :item-key (str (hash "ContactEmail= \"AbashkinaMA@puvao.mos.ru\""))
   :callback :getMySD
   :id "test-inline-id"})

(defn one-item-id [item]
  (-> item (get :Interaction) (get :InteractionID)))

(defn many-item-ReturnCode [item]
  (-> item (get :ReturnCode)))

(defrecord TestMessage  [inline-object] TMessage
           (get-item-key [_] (:item-key inline-object))
           (get-query [_]  (:sm-query inline-object))
           (get-callback [_]  (:callback inline-object))
           (get-role [_] (:user-role inline-object))
           (get-user-mail [_] ^String  (:user-smkey inline-object))
           (get-operator-id [_] ^String (:user-smkey inline-object))
           (response [_ sm-responce] (swap! TestResult assoc  (:item-key inline-object) sm-responce))
           (error-response [_ _] (swap! TestResult assoc  :error-message "Wrong responce"))
           (wait-a-minute [_] (swap! TestResult assoc  :wait-a-minute "Wait a minute")
             (TestMessage. (assoc inline-object :message-id "something"))))


(deftest test-get-item-error
  (try (get-item (TestMessage. test-wrong-item))
       (catch Exception e (println (ex-message e))
              (clojure.stacktrace/print-stack-trace e)))
  (Thread/sleep (* 1000 2))
  (is (some? (@TestResult :wait-a-minute)))
  (is (some? (@TestResult :error-message))))

(deftest test-get-item
  (testing "one-item"
    (let [item (TestMessage. test-inline-item)]
      (testing "request not cached item"
        (try (get-item  item)
             (catch Exception e (println (ex-message e))
                    (clojure.stacktrace/print-stack-trace e)))

        (Thread/sleep (* 1000 5))
        (is (= test-item-key (-> TestResult deref (get test-item-key) one-item-id))))
      (testing "L1 cache hit"
        (is (= test-item-key (one-item-id (test-item-L1 item)))))
      (testing "L2 cache hit"
        (is (= test-item-key (one-item-id (test-item-L2 item)))))
      #_(testing "L2 cache miss (expired)"
          (Thread/sleep (* 1000 60))
          (is (nil? (one-item-id (test-item-L2 item))))))))



(deftest test-get-query
  (testing "query"
    (let [item (TestMessage. test-inline-query)
          test-item-key (test-inline-query :item-key)]
      (testing "request not cached item"
        (try (get-item item)
             (catch Exception e (println (ex-message e))
                    (clojure.stacktrace/print-stack-trace e)))
        (Thread/sleep (* 1000 2))
        (is (= 0 (-> TestResult deref (get test-item-key) many-item-ReturnCode))))
      (testing "L1 cache hit"
        (is (= 0 (many-item-ReturnCode (test-item-L1 item)))))
      (testing "L2 cache hit"
        (is (= 0 (many-item-ReturnCode (test-item-L2 item)))))
      #_(testing "L2 cache miss (expired)"
          (Thread/sleep (* 1000 60))
          (is (nil? (one-item-id (test-item-L2 item))))))))