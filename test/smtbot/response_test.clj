
(ns smtbot.response-test
  (:require [clojure.test :refer [testing  deftest  is use-fixtures]]
            #_[smtbot.utils.reflector :refer [reflector-start
                                              relector-set-responce
                                              reflector-stop]]
            [smtbot.dal.configure :refer [configure-database execute-script]]
            [smtbot.bot.config :as bot]
            [smtbot.bot.messages.inline :as inline]
            [smtbot.bot.messages.command :as command]
            [smtbot.bot.messages.menu :as menu]
            [smtbot.config :as config]
            [smtbot.message-format-checker :as checker]
            [smtbot.utils.message-log :as m-log]
            [smtbot.dal.globals :as g :refer [botuser-action]]
            [clojure.stacktrace :as clojure.stacktrace]))

(defn set-up-user [id email]
  ((@botuser-action :insert-or-update) id email))

(defn confirm-user-as-user [email]
  ((@botuser-action :update-by-key) email "A" "user"))


(defn confirm-user-as-operator [email]
  ((@botuser-action :update-by-key) email "A" "operator"))

(def test-item-key "SD1848894")

(def test-inline-request  {:id 151488382774279532,
                           :from {:id 35271137,
                                  :is_bot false,
                                  :first_name "Max",
                                  :last_name "Ahmed",
                                  :language_code "ru"},
                           :chat_type "private",
                           :query test-item-key,
                           :offset 0})


(defn fix-read-config [t]
  (try
    (config/configure "test/config/run/")
    (configure-database)
    (bot/configure "test/config/run/")
    (t)
    (catch Exception e (println (ex-message e))
           (clojure.stacktrace/print-stack-trace e))))

(defn fix-test-result-cleanup [t]
  (m-log/start-test)
  (execute-script "TRUNCATE TABLE TBOT.BOTUSER;")
  (t)
  (m-log/end-test))

(use-fixtures :once fix-read-config)

(use-fixtures :each fix-test-result-cleanup)

(deftest test-inline
  (inline/processor test-inline-request)
  (Thread/sleep (* 1000 2))
  (println "Messages:" @m-log/MessageLog)
  (is (empty? (checker/check-message-vec checker/inline-answer @m-log/MessageLog))))





(def test-command-status

  {:message_id 67,
   :from {:id 35271137, :is_bot false, :first_name "Max", :last_name "Ahmed", :language_code "ru"},
   :chat {:id 35271137, :first_name "Max", :last_name "Ahmed", :type "private"},
   :date 1644313111,
   :text (str "/status " test-item-key), :entities [{:offset 0, :length 5, :type "bot_command"}]})


(def test-command-mySD

  {:message_id 67,
   :from {:id 35271137, :is_bot false, :first_name "Max", :last_name "Ahmed", :language_code "ru"},
   :chat {:id 35271137, :first_name "Max", :last_name "Ahmed", :type "private"},
   :date 1644313111,
   :text (str "/mySD " test-item-key), :entities [{:offset 0, :length 5, :type "bot_command"}]})

(def test-command-mySD-open

  {:message_id 67,
   :from {:id 35271137, :is_bot false, :first_name "Max", :last_name "Ahmed", :language_code "ru"},
   :chat {:id 35271137, :first_name "Max", :last_name "Ahmed", :type "private"},
   :date 1644313111,
   :text (str "/mySD open"), :entities [{:offset 0, :length 5, :type "bot_command"}]})

(deftest test-simple-command
  (testing "Simple Command"
    (command/processor test-command-status)
    (Thread/sleep (* 1000 2))
    (println "Messages:" @m-log/MessageLog)
    (is (empty? (checker/check-message-vec checker/command-answer @m-log/MessageLog)))))

(deftest test-list-command
  (testing "List command Not authorized"
    (command/processor test-command-mySD)
    (Thread/sleep (* 1000 2))
    (println "Messages:" @m-log/MessageLog)
    (is (empty? (checker/check-message-vec checker/command-answer @m-log/MessageLog))))
  (testing "List command  authorized"
    (set-up-user "35271137"  "MasyaginaNV1@dfks.mos.ru")
    (confirm-user-as-user "MasyaginaNV1@dfks.mos.ru")
    (command/processor test-command-mySD)
    (testing "List command option authorized "
      (command/processor test-command-mySD-open))
    (Thread/sleep (* 1000 2))
    (println "Messages:" @m-log/MessageLog)
    (is (empty? (checker/check-message-vec checker/command-answer @m-log/MessageLog)))))

(def test-one-item-callback
  {:id 151488381075741984,
   :from {:id 35271137,
          :is_bot false,
          :first_name "Max",
          :last_name "Ahmed", :language_code :ru},
   :message {:message_id 167,
             :from {:id 5142232953,
                    :is_bot true,
                    :first_name "smtestmaxbot", :username "SMTestMaxBot"},
             :chat {:id 35271137,
                    :first_name "Max",
                    :last_name "Ahmed", :type "private"},
             :date 1647750100, :edit_date 1647750100,
             :text "SD1878232:Подготовка к публикации (223-ФЗ) Open: 09:40 02-02-22 Status: Closed Category: Consultation",
             :reply_markup {:inline_keyboard [[{:text "Brief", :callback_data "breifSD$1$SD1878232"}]
                                              [{:text "Description", :callback_data "questionSD$1$SD1878232"}]
                                              [{:text "Solution", :callback_data "answerSD$1$SD1878232"}]]}},
   :chat_instance -2790797245060395431, :data "briefSD$1$SD1878232"})

(deftest test-callback
  (testing "One item callback request"
    (set-up-user "35271137"  "MasyaginaNV1@dfks.mos.ru")
    (confirm-user-as-user "MasyaginaNV1@dfks.mos.ru")
    (menu/processor test-one-item-callback)
    (Thread/sleep (* 1000 2))
    (println "Messages:" @m-log/MessageLog)
    (is (empty? (checker/check-message-vec checker/command-answer @m-log/MessageLog)))))