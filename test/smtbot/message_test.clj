(ns smtbot.message-test
  #_{:clj-kondo/ignore [:refer-all]}
  (:require [clojure.test :refer :all]
            [cheshire.core :as json]
            [smtbot.bot.mappers :as m]
            [smtbot.bot.message :refer [render-message render-answer parse-options]]
            [smtbot.bot.config :as bot-config]))

(def test-object
  (json/parse-string " { \"Interaction\": { \"CallbackContact\": \"Абашкина Мария Александровна\", \"Category\": \"консультация\", \"CompanyGRBS\": \"Префектура ЮВАО\", \"CompanyINN\": \"7721573836\", \"CompanyKPP\": \"772101001\", \"CompanyName\": \"ГБУ ЦКДС ИСТОКИ\", \"ContactEmail\": \"AbashkinaMA@puvao.mos.ru\", \"ContactName\": \"Абашкина Мария Александровна\", \"ContactNameUserID\": \"591365\", \"Description\": [ \"test description\", \"line2\", \"error ЕИС-500\" ], \"Direction\": \"ЕАИСТ\", \"Group\": \"ОМ ЕАИСТ\", \"InteractionID\": \"SD1698508\", \"OpenTime\": \"2022-01-07T13:25:08+03:00\", \"Source\": \"5\", \"StatusForUser\": \"open\", \"Title\": \"test title\" }, \"Messages\": [], \"ReturnCode\": 0 }" true))

(def test-object-list
  (json/parse-string (slurp "test/test_data/sm_list_resp.json") true))

(def test-converter-list [{:func :standart-short-time
                           :field-name :OpenTime}])

(deftest test-converters
  (is (m/apply-converter-list {:converter-list test-converter-list} (test-object :Interaction))))

(deftest test-message-creation
  (is (= "SD1698508:test title - In progress "
         (render-message :SDstatus :en test-object)))
  (is (= "SD1698508:test title\n Время создания: 13:25 07-01-22\n Cтатус: В Работе\n Категория: Консультация"
         (render-message :SDbrief :ru test-object)))
  (is (= " SD1698508:test title -> В Работе\n SD1698509:test title -> В Работе\n"
         (render-message :SDMyList :ru test-object-list))))

(deftest test-parse-options
  (is (= "MarkdownV2" (parse-options :SDanswer))))

(def menu-json
  "[[{\"text\":\"Краткая информация\",\"callback_data\":\"breifSD$1$SD1698508\"}],[{\"text\":\"Описание\",\"callback_data\":\"questionSD$1$SD1698508\"}],[{\"text\":\"Решение\",\"callback_data\":\"answerSD$1$SD1698508\"}]]")

(deftest test-render-answer
  (let [{:keys [text parse_mode reply_markup]}  (render-answer "SD1698508" :ru :SDprint test-object :SDanswer)]
    (is (some? text))
    (is (= "MarkdownV2" parse_mode))
    (is (= menu-json (json/generate-string (reply_markup :inline_keyboard))))))


(comment
  (render-message :SDMyList :ru test-object-list)
  (test-object-list "content"))


(defn fix-test-bot [t]
  (bot-config/configure "test/config/run/")
  (t))

(use-fixtures  :once fix-test-bot)