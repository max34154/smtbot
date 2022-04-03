(ns smtbot.botuser-test
  (:require
   [clojure.test :refer [testing  deftest  is use-fixtures]]
   [smtbot.dal.configure :refer [configure-database execute-script stop-database]]
   [taoensso.timbre :as timbre :refer [info fatal]]
   [smtbot.bot.config :as bot]
   [smtbot.config :as config]
   [smtbot.bot.users.globals :as g]
   [clojure.string :as str]
   [smtbot.dal.globals :refer [botuser-action]]
   [smtbot.bot.users.operations :as user-ops]
   [smtbot.bot.users.user-cache :as user-cache]
   [smtbot.utils.message-log :as m-log]
   [clojure.stacktrace :refer [print-stack-trace]]
   [smtbot.task.sync_dispatcher :refer [start-pushers stop-pushers]]
   [smtbot.hook.dispatcher :refer [start-messengers
                                   stop-messengers]]))


(defn shutdown []
  (stop-pushers)
  (stop-messengers)
  (stop-database))



(defn fix-run-infrastructure [t]
  (timbre/with-merged-config
    {:min-level  [[#{"smtbot.task.*" "smtbot.hook.*" "smtbot.utils.*"} :error]  [#{"*"} :debug]]}
    (try
      (info "Start initialization.")
      (config/configure "test/config/run/")
      (configure-database)
      (bot/configure "test/config/run/")
      (when (some? (start-pushers)) (throw (AssertionError.  "Pushers configuration error.")))
      (when (some? (start-messengers)) (throw (AssertionError. "Messengers configuration error")))
      (info "Initialization sucessfully completed.")
      (t)
      (info "Change validation mode cooldown.")
      (Thread/sleep (* 2 1000))
      (with-redefs [g/user-validation-mode 'async']
        (t))
      (catch Exception e
        (fatal (ex-message e) "\nInitialization failed.")
        (print-stack-trace e))
      (finally (shutdown)))))


(defn fix-test-result-cleanup [t]
  (m-log/start-test)
  (execute-script "TRUNCATE TABLE TBOT.ATTACHMENT; TRUNCATE TABLE TBOT.MESSAGE; TRUNCATE TABLE TBOT.HOOK; TRUNCATE TABLE TBOT.RESPONCE;DELETE FROM TBOT.REQUEST;")
  (execute-script "TRUNCATE TABLE TBOT.BOTUSER;")
  (t)
  (m-log/end-test))

(use-fixtures :once fix-run-infrastructure)

(use-fixtures :each fix-test-result-cleanup)

;; Attention: blocked operator is treated as user  
;; Possible worth changing 

(deftest user-registration-test
  (testing
   (str "Init registration.  User validation mode: " g/user-validation-mode)

    (testing "Valid user"
      (user-cache/remove-by-id "35271137")
      (user-ops/init-registration  "35271137"  "MasyaginaNV1@dfks.mos.ru")
      (Thread/sleep (* 5 1000))
      (let [user ((@botuser-action :get-by-key) "MasyaginaNV1@dfks.mos.ru")]
        (is (= (str/lower-case "MasyaginaNV1@dfks.mos.ru")
               (:smkey user)))
        (is (some? (re-find #"C\d+" (:status user))))
        (is (= "user" (:role user)))))

    (testing "Unknown user"
      (user-cache/remove-by-id "35271147")
      (user-ops/init-registration  "35271147"  "unknown.mos.ru")
      (Thread/sleep (* 5 1000))
      (println "Unknown user " ((@botuser-action :get-by-key) "unknown.mos.ru"))
      (is (= "N" (:status ((@botuser-action :get-by-key) "unknown.mos.ru"))))
      (is (nil? (user-cache/get-by-id "35271147"))))

    (testing "Not active contact"
      (user-cache/remove-by-id "35271157")
      (user-ops/init-registration  "35271157"  "Anton.Sharin@rtall.ru")
      (Thread/sleep (* 5 1000))
      (is (= "N" (:status ((@botuser-action :get-by-key) "anton.sharin@rtall.ru"))))
      (is (nil? (user-cache/get-by-id "35271157"))))

    (testing "User unregistration test"
      (user-ops/unreg 35271137)
      (is (nil? (user-cache/get-by-id "35271137")))
      (is (nil? ((@botuser-action :get-by-id) "35271137"))))

    (testing "Valid operator"
      (user-cache/remove-by-id "35271137")
      (user-ops/init-registration  "35271137"  "max@rtall.ru")
      (Thread/sleep (* 5 1000))
      (let [user ((@botuser-action :get-by-key) "max@rtall.ru")]
        (is (= (str/lower-case "max@rtall.ru")
               (:smkey user)))
        (is (some? (re-find #"C\d+" (:status user))))
        (is (= "operator" (:role user))))))

   (testing (str "User validation test. User validation mode:" g/user-validation-mode)

    (testing "Invalid code"
      (is (nil? (user-ops/confirm-registration "35271137" "1111"))))

    (testing "Correct code"
      (let [code (subs
                  (re-find #"C\d+"
                           (:status ((@botuser-action :get-by-id) "35271137")))
                  1)]
        (user-ops/confirm-registration "35271137" code))
      (Thread/sleep (* 2 1000))
      (let [user ((@botuser-action :get-by-id) "35271137")]
        (is (= "A" (:status user)))
        (is (= "operator" (:role user)))))))