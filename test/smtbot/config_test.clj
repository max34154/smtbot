(ns smtbot.config-test
  (:require [clojure.test :refer [testing  deftest  is]]
            [smtbot.bot.config :refer [simulate-configure]]))

(def path  "test/config/run/")

(deftest bot-config-loading-test
  (let [config (simulate-configure path)]
    (testing "Bot configuration "
      (is (some? (config :bot))))
    (testing "Mapper configuration "
      (is (some? (config :mapper))))
    (testing "Messages configuration "
      (is (some? (config :messages))))
    (testing "Callback map  configuration "
      (is (some? (config :callback-map))))))
