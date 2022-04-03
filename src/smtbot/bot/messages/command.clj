(ns smtbot.bot.messages.command
  (:require
   [smtbot.bot.globals :refer [bot bot-token default-lang
                               ;callback-map
                               ]]
   ;[smtbot.bot.query-processor :refer [query-processor]]
   ;[smtbot.bot.users.operations :as user-ops]
   [smtbot.enum.sm :as sm]
   [smtbot.cache.cache :as cache]
   [smtbot.bot.messages.utils :as mu]
   [morse.api :as t]
   [clojure.string :as str]
   [taoensso.timbre :as timbre]
   [smtbot.bot.messages.tmessage :refer [TMessage]]
   [smtbot.utils.message-log :as m-log]
   [taoensso.timbre.appenders.core :as appenders]
   [smtbot.bot.message :as ms]))

;;  Command message format 
;; /help
;; {:message_id 67, 
;;  :from {:id 35271137, :is_bot false, :first_name Max, :last_name Ahmed, :language_code en},
;;  :chat {:id 35271137, :first_name Max, :last_name Ahmed, :type private}, 
;;  :date 1644313111, 
;;  :text /help, :entities [{:offset 0, :length 5, :type bot_command}]}
;; generic message 
;; {:message_id 73, 
;;  :from {:id 35271137, :is_bot false, :first_name Max, :last_name Ahmed, :language_code en},
;;  :chat {:id 35271137, :first_name Max, :last_name Ahmed, :type private}, 
;;  :date 1644313474, 
;;  :text generick text somthing}

(def default-command-error :ТВОТDefaulCommandErorr)

(defn- send-answer
  "
    Send message to selected by chat-id chat 
    Answer structure: 
     :text - answer text. !! Overrides message-text in supplyed 
     :parse_mode -  MarkdownV2|HTML
     :reply_markup - menu, use  ms/build-inline-keyboard to create menu 
    
    Details and the other answer options:
     https://core.telegram.org/bots/api#sendmessage
     
  "
  ([obj answer]
   (send-answer obj nil answer))

  ([obj message-text answer]
   (when (or (some? message-text)
             (some? answer))
     (if (m-log/test-run?)
       (:message_id (if (some? (obj :messag-id))
                      (m-log/add-edit-text @bot-token (:chat-id obj) (:messag-id obj)  answer message-text)
                      (m-log/add-send-text @bot-token (:chat-id obj)  answer message-text)))
       (try
         (if (timbre/may-log? :debug)
           (timbre/with-merged-config
             {:appenders {:println {:enabled? false}
                          :min-level :debug
                          :spit (appenders/spit-appender {:fname "log/command.log"})}}
             (timbre/debugf "->Telegram answer: %s message-text: %s message-id: %s " answer message-text (obj :message-id))
             (let [tresp (if (some? (obj :message-id))
                           (t/edit-text @bot-token (:chat-id obj) (:message-id obj)  answer message-text)
                           (t/send-text @bot-token (:chat-id obj)  answer message-text))]
               (timbre/debug "<-Telegram " tresp)
               (-> tresp :result :message_id)))
           (:message_id (if (some? (obj :message-id))
                          (t/edit-text @bot-token (:chat-id obj) (:message-id obj)  answer message-text)
                          (t/send-text @bot-token (:chat-id obj)  answer message-text))))
         (catch Exception e
           (timbre/with-merged-config
             {:appenders {:println {:enabled? false}
                          :spit (appenders/spit-appender {:fname "log/command.log"})}}
             (timbre/error  "Message delivery error " e))))))))

(defn simple-message [obj message]
  (send-answer obj (ms/render-message message (:lang obj) {}) {}))


(defn select-command
  "
   Analyze message text. If starts with known command creates command object.
   Otherwise send error message. 
   Returns command obj or nil
  "
  [{:keys [text chat from]}]
  (let [[command option] (str/split text #"\s+")]
    (if-let [command ((@bot :command)
                      (-> command
                          (subs 1) ;; remove leading /
                          keyword))]
      (do
        (timbre/debugf "Command %s selected. Option text: %s" command option)
        {:command-text  text
         :option-text option
         :chat-id (chat :id)
         :user-id (:id from)
         :lang (keyword (or (:language_code from) default-lang))
         :command command})
      (do
        (timbre/debug "Command not found. Source:" text)
        (t/send-text @bot-token (chat :id)
                     (ms/render-message :TBOTUnknowCommand
                                        (keyword (or (:language_code from) default-lang))
                                        {}))
        nil))))


(defn authorize-command
  "
   Check if user, who suppled the command athorized to run this command.
   If yes, return valid command object.
   Otherwise send error message. 
  "
  [obj]
  (when (some? (:command obj))
    (mu/authorize obj (-> obj :command :role) simple-message)
    #_(let [required-role (-> obj :command :role)]
        (if (or (nil? required-role) (= required-role :any))
          obj
          (-> obj
              :user-id
              user-ops/get-user
              (#(case (user-ops/get-user-status %)
                  :user-ok (if (user-ops/authorize required-role %)
                             obj
                             (simple-message obj :TBOTNotPremited))
                  :user-blocked (simple-message obj :TBOTUserBlocked)
                  :user-not-found (simple-message obj :TBOTRegReq)
                  :reg-in-progress (simple-message obj :TBOTRegInProgress)
                  :user-invalid (simple-message obj :TBOTUserCheck))))))))

(defn select-option [{:keys [command option-text] :as obj}]
  (if-let [option  (some-> command :options (get (keyword option-text)))]
    (assoc obj :option option) obj))


(defn setup-callback [{:keys [command option] :as obj}]
  (timbre/spy :debug
              (assoc obj :callback-name (or (:callback option) (:callback command))
                     :callback-reg (or (:callback-reg option) (:callback-reg command))
                     :menu (when-not (= (:menu option) :NoMenu)
                             (or (:menu option) (:menu command)))
                     :errmessage  (or (:errmessage option) (:errmessage command) default-command-error))))

(defn- setup-item-key [{:keys [command-text callback-reg callback-name] :as obj}]
  (timbre/debugf "Setup item key using reg %s and source <<%s>>" callback-reg command-text)
  (mu/setup-item-key callback-name obj callback-reg command-text))


(defn create-answer-object
  ([command-object message-template]
   (create-answer-object command-object {} message-template))

  ([{:keys [item-key lang command option]} item message-template]
   (ms/render-answer item-key lang (or (:menu option) (:menu command)) item message-template)
   #_(-> {:text (ms/render-message message-template lang item)}
         (#(if-let [parse-mode (ms/parse-options message-template)] (assoc % :parse_mode parse-mode) %))
         (#(if-let [menu (or (:menu option) (:menu command))]
             (assoc % :reply_markup (ms/build-inline-keyboard item item-key lang menu)) %)))))

(defn make-answer

  ([{:keys [command option] :as command-object} item]
   (make-answer command-object item (or (:message option) (:message command))))

  ([command-object item message-template]
   (when (and (some? message-template) (some?  item))
     (create-answer-object command-object item message-template))))

(defn make-error-answer [{:keys [item-key lang] :as item} error]
  (ms/render-answer item-key lang nil item error))



(defrecord CommandQuery  [command-object] TMessage
           (get-item-key [_] (:item-key command-object))
           (get-query [_]  (:sm-query command-object))
           (get-callback [_] (-> command-object  :callback-name))
           (get-user-mail [_] ^String  (:user-smkey command-object))
           (get-operator-id [_] ^String (:user-smkey command-object))
           (get-role [_]  (:user-role command-object))
           (error-response [_ error-code] (if (= error-code sm/RC_NO_MORE)
                                            (send-answer command-object  
                                                         (make-error-answer command-object :TBOTNotFound)
                                                         #_(make-answer command-object
                                                                                      command-object
                                                                                      :TBOTNotFound))
                                            (send-answer command-object 
                                                        (make-error-answer command-object  (command-object :errmessage)) 
                                                         #_(make-answer command-object
                                                                                      command-object
                                                                                      (command-object :errmessage)))))
           (response [_ sm-responce]
             (if (some? sm-responce)
               (send-answer command-object (make-answer command-object
                                                        sm-responce))
               (send-answer command-object 
                            (make-error-answer command-object  (command-object :errmessage))
                            #_(make-answer command-object
                                                        command-object
                                                        (command-object :errmessage)))))
           (wait-a-minute [_] (CommandQuery. (assoc command-object :message-id
                                                    (simple-message command-object :CommandWaiting)))))



(defn process-command
  "
   Check if command found. 
   If command has configured callback - use it to request data from sm.  
   If item available from cache - create response and send to telegram, otherwise - send wait-a-minute message.
   If command does not have callback just generate response message. Internal command data available to use in message template. 
  "
  [{:keys [command callback-name item-key] :as obj}]
  (when (some? command)
    (if  (some? callback-name)
      (if (some? item-key)
        (send-answer obj (make-answer obj (cache/get-item (CommandQuery. obj))))
        (send-answer obj (make-answer obj obj (obj :errmessage))))
      #_(-> obj
            (#(if (some? (:item-key %))
                (send-answer
                 %
                 (make-answer
                  %
                  (cache/get-item (CommandQuery. %))))
                (send-answer % (make-answer % % (% :errmessage))))))
      (make-answer obj obj))))


(defn processor [command]
  (timbre/with-merged-config
    {:appenders {:min-level :debug
                 :println {:enabled? false}
                 :spit (appenders/spit-appender {:fname "log/command.log"})}}
    (some-> command
            select-command
            authorize-command
            select-option
            setup-callback
            setup-item-key
            process-command)))