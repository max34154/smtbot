
(ns smtbot.bot.bot
  (:require [morse.handlers :as h]
            [morse.api :as t]
            [smtbot.morse.polling :as p]
            [smtbot.bot.messages.inline :as inline]
            [smtbot.bot.messages.command :as command]
            [smtbot.bot.messages.menu :as menu]
            [smtbot.bot.messages.user-managment :as um]
            ;[org.httpkit.server :as httpkit]
            [taoensso.timbre :as timbre]
            [smtbot.dal.configure :refer [configure-database]]
            [smtbot.bot.config :as bot]
            [smtbot.config :as config]
            [taoensso.timbre.appenders.core :as appenders])
  (:gen-class))


(def token "5142232953:AAHKj2XXwB-J9MKfs4Xtn7GKjYHpsTa1aZw")

;;Max Ahmed, [20 Jan 2022, 09:32:00]:
;;smtestmaxbot

;;BotFather, [20 Jan 2022, 09:32:01]:
;;Good. Now let's choose a username for your bot. It must end in `bot`. Like this, for example: TetrisBot or tetris_bot.

;;Max Ahmed, [20 Jan 2022, 09:32:31]:
;;SMTestMaxBot

(defonce ^:private TheBot (atom nil))

(def briefSD
  {:text "Сводка"
   :callback_data "brief"})

(def answerSD
  {:text "Решение"
   :callback_data "answer"})

(def questionSD
  {:text "Описание"
   :callback_data "question"})

(defn- callbackSD [{:keys [data]}]
  (case data
    "brief" "Печатем сводку"
    "answer" "Печатем решение"
    "question" "Печатем описание"
    "Чего-то не то пришло"))





(defn- status [request]
  (let [SD (re-find #"SD\d+" request)]
    (if (some? SD)
      (str "Сейчас попробуем узнать статус по обращению "  SD " ....")
      (str "Мне кажется не верно указали номер обращения. Номер начинается с букв SD, затем идет несколько цифр"))))

(defn- articleSD [request]
  (let [SD (re-find #"SD\d+" request)]
    {:type "article"
     :id (if (some? SD) SD "SD000000")
     :title (if (some? SD) SD "Не указан код")


     :input_message_content {:message_text
                             (if (some? SD)
                               (str "Сейчас попробуем узнать статус по обращению "  SD " ....")
                               (str "Мне кажется не верно указали номер обращения. Номер начинается с букв SD, затем идет несколько цифр"))}
     :reply_markup {:inline_keyboard [[briefSD] [answerSD questionSD]]}}))


(defn- menuSD [request]
  (if-let [SD (re-find #"SD\d+" request)]
    {:text (str "Сейчас попробуем узнать статус по обращению "  SD " ....")
     :reply_markup {:inline_keyboard [[briefSD] [answerSD questionSD]]}}
    {:text  (str "Мне кажется не верно указали номер обращения. Номер начинается с букв SD, затем идет несколько цифр")}))





; This will define bot-api function, which later could be
; used to start your bot
#_{:clj-kondo/ignore [:unresolved-symbol]}
(h/defhandler bot-api
  ; Each bot has to handle /start and /help commands.
  ; This could be done in form of a function:
  (h/command-fn "start" (fn [message]
                          (println "Bot joined new chat: " (message :chat))
                          (um/welcome  message)))

  ; You can use short syntax for same purposes
  ; Destructuring works same way as in function above
  (h/command-fn "help" (fn [message]
                         (println "Help was requested in " (message :chat))
                         (um/help message)))

  (h/command-fn "reg" (fn [message]
                        (println "Registration was requested in " (message :chat))
                        (println "Message " message)
                        (um/registration message)))

  #_(h/command-fn "menu" (fn [message]
                           (let [{{id :id :as chat} :chat} message]
                             (println "Help was requested in " chat)
                             (println "Message " message)
                             (t/send-text token id (-> message :text menuSD) nil))))

  (h/inline-fn (fn [inline] (println "Received inline: " inline)
                 (timbre/info  "Received inline: " (inline :id) "=> " (-> inline :query articleSD))
                 (inline/processor inline)
                 #_(println " Response:" (t/answer-inline token (inline :id)
                                                          [(-> inline :query articleSD)]))))
  (h/callback-fn (fn [data]
                   (timbre/info "Received callback: " data)
                   (menu/processor data)
                   #_(if (:inline_message_id data)
                       (do
                         (t/answer-callback token (data :id) (callbackSD data))
                         (println "Del response:" (t/delete-text token (-> data :chat_instance)  (-> data :inline_message_id))))
                       (do (println "Put into " (-> data :chat) "mess: " (callbackSD data))
                           (println " Response:" (t/send-text token (-> data :message :chat :id) (callbackSD data)))
                           (t/delete-text token (-> data :message :chat :id)  (-> data :message :message_id))))))

  (h/message-fn (fn [message]  (println "Intercepted message:" message)
                  (command/processor message))))


;;(def channel (p/start token handler))

(defn open-bot-channel  [bot-token]
  (with-redefs [token bot-token]
    (if (nil? @TheBot)
      (do
        (config/configure "test/config/run/")
        (configure-database)
        (bot/configure "test/config/run/")
        (reset! TheBot #_{:clj-kondo/ignore [:unresolved-symbol]}
                (p/start token bot-api))
        (println "Hello, World!"))
      (print "Already started!"))))

(defn close-bot-channel []
  (if (some? @TheBot)
    (do (p/stop @TheBot)
        (reset! TheBot nil))
    (print "Already stopped!")))

(comment
  (open-bot-channel "5142232953:AAHKj2XXwB-J9MKfs4Xtn7GKjYHpsTa1aZw")
  (close-bot-channel))