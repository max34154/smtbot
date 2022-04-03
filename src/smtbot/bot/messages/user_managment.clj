(ns smtbot.bot.messages.user-managment
  (:require
   [morse.api :as t]
   [smtbot.bot.globals :refer [bot-token default-lang]]
   [clojure.string :as str]
   [smtbot.bot.users.operations :as user-ops]
   [smtbot.bot.message :as ms]))

(defn- simple-message [chat-id lang msg-template-id]
   (t/send-text @bot-token chat-id
                (ms/render-message msg-template-id
                                   (keyword (or lang default-lang))
                                   {})))

(defn- wait-a-minute [chat-id lang]
   (:message_id (simple-message chat-id lang :CommandWaiting)))


(defn welcome [{:keys [chat from]}]
  (simple-message (:id chat) (:lang from) :welcome))

(defn help [{:keys [chat from]}]
  (simple-message (:id chat) (:lang from) :help))


(defn- response-message [message-id chat-id lang msg-template-id]
  (if (nil? message-id)
    (t/send-text @bot-token chat-id
                 (ms/render-message msg-template-id
                                    (keyword (or lang default-lang))
                                    {}))
    (t/edit-text @bot-token chat-id message-id
                 (ms/render-message msg-template-id
                                    (keyword (or lang default-lang))
                                    {}))))

(defn- init-registration  [mail {chat-id :id}, {lang :language_code}]
  (let [msg-id (wait-a-minute chat-id lang)]
    (response-message msg-id chat-id lang
                      (case (user-ops/init-registration  chat-id  mail)
                        :already-registred  :TBOTRegAlreadyRegistred
                        :check-progress :TBOTRegInProgress
                        :continue-registration :TBOTRegComplite
                        :ТВОТDefaulCommandErorr))))


(defn- confirm-registation [code {chat-id :id}, {lang :language_code}]
  (let [msg-id (wait-a-minute chat-id lang)]
    (response-message msg-id chat-id lang
                      (if (user-ops/confirm-registration chat-id code)
                        :TBOTRegSuccess
                        :TBITWrongCode))))

(defn registration  [{:keys [text chat from]}]
  (let [[_ mail code] (str/split text #"\s+")]
    (if (str/includes? mail "@")
      (if (nil? code)
        (init-registration mail chat from)
        (confirm-registation  code chat from))
      (simple-message  chat from :TBOTRegIncorrectMail))))