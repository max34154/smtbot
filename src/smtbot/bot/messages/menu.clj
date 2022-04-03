(ns smtbot.bot.messages.menu
  (:require
   [smtbot.bot.globals :refer [bot bot-token default-lang]]
   [smtbot.cache.cache :as cache]
   [smtbot.bot.messages.utils :as mu]
   [morse.api :as t]
   [clojure.string :as str]
   [smtbot.enum.sm :as sm]
   [taoensso.timbre :as timbre]
   [smtbot.bot.messages.tmessage :refer [TMessage]]
   [smtbot.utils.message-log :as m-log]
   [taoensso.timbre.appenders.core :as appenders]
   [smtbot.bot.message :as ms]))
;;
;;  menu item push. 
;;  1.Menu placed via inline:  
;; {:id 151488382939201831, 
;; :from {:id 35271137, :is_bot false, :first_name Max, :last_name Ahmed, :language_code en}, 
;; :inline_message_id AgAAABnzAADhMRoCCaxUjF77V-Q, :chat_instance 197849829461861701, 
;; :data brief}

;;
;;  2.Menu placed in chat with bot 
;; {:id 151488382963454690, 
;;  :from {:id 35271137, :is_bot false, :first_name Max, :last_name Ahmed, :language_code en},
;;  :message {
;;  :message_id 91, 
;;  :from {:id 5142232953, :is_bot true, :first_name smtestmaxbot, :username SMTestMaxBot}, 
;;  :chat {:id 35271137, :first_name Max, :last_name Ahmed, :type private}, 
;;  :date 1644484214, :text Сейчас попробуем узнать статус по обращению SD12334 ...., 
;;  :reply_markup {:inline_keyboard [[{:text Сводка, :callback_data brief}] [{:text Решение, :callback_data answer} {:text Описание, :callback_data question}]]}}, 
;;  :chat_instance -2790797245060395431, 
;;  :data answer}

;; to remove menu after usage - 
;; (t/delete-text token (-> data :message :chat :id)  (-> data :message :message_id))

(def border #"\$1\$")


(defn send-inline-response [deliver-to message-text]
;; TODO: 1.check for formating for inline message 2.find oud how to delete inline-keyboard
  (if (m-log/test-run?)
    (m-log/add-answer-callback @bot-token deliver-to message-text)
    (try
      (if (timbre/may-log? :debug)
        (do
          (timbre/debugf "->Telegram deliver-to: %s message-text: %s" deliver-to message-text)
          (let [tresp (t/answer-callback @bot-token deliver-to
                                         (if (< 196 (count message-text))
                                           (str (subs message-text 0 196) "...")
                                           message-text))]
            (timbre/debug "<-Telegram " tresp)
            (-> tresp :result :message_id)))
        (t/answer-callback @bot-token deliver-to
                           (if (< 196 (count message-text))
                             (str (subs message-text 0 196) "...")
                             message-text)))
      (catch Exception e
        (timbre/with-merged-config
          {:appenders {:println {:enabled? false}
                       :spit (appenders/spit-appender {:fname "log/menu_callback.log"})}}
          (timbre/error  "Message delivery error " e))))))

(defn send-response
  "
    Send response on online-keyboard push selected. 
    Depends on mode the response placed in chat(private user-bot chat) or as inline message.
    For chat mode autodelete option (remove menu after pushing button) available only in chat mode. 
    Answer structure: 
     :text - answer text. !! Overrides message-text if supplyed 
     :parse_mode -  MarkdownV2|HTML
     :reply_markup - menu, use  mu/build-inline-keyboard to create menu 
    
    Details and the other answer options:
     https://core.telegram.org/bots/api#sendmessage
     
  "
  ([obj answer] (send-response obj (:text answer) answer))

  ([{:keys [mode deliver-to menu-message-id menu-callback] :as obj} message-text answer]
   (timbre/with-merged-config
     {:appenders {:println {:enabled? false}
                  :min-level :debug
                  :spit (appenders/spit-appender {:fname "log/menu_callback.log"})}}
     (if (= mode :inline)
       (send-inline-response  deliver-to message-text)
     ;(t/answer-callback @bot-token deliver-to message-text)
       (try
         (if (m-log/test-run?)
           (when (some? answer)
             (:message_id (if (some? (obj :messag-id))
                            (m-log/add-edit-text @bot-token deliver-to (:messag-id obj)  answer message-text)
                            (m-log/add-send-text @bot-token deliver-to  answer message-text))))
           (if (timbre/may-log? :debug)
             (do
               (when (and (some? menu-message-id) (menu-callback :autodelete))
                 (timbre/debugf "Аttempt to delete menu %s <-Telegram %s" menu-message-id (t/delete-text @bot-token deliver-to  menu-message-id)))
               (timbre/debugf "->Telegram deliver-to: %s message-id: %s answer: %s message-text: %s \nmessage text length: %s" deliver-to (:messag-id obj) answer message-text (count message-text))
               (when (some? answer)
                 (let [tresp
                       (if (some? (obj :messag-id))
                         (t/edit-text @bot-token deliver-to (:messag-id obj)  answer message-text)
                         (t/send-text @bot-token deliver-to  answer message-text))]
                   (timbre/debug "<-Telegram " tresp)
                   (:message_id tresp))))

             (do
               (when (and (some? menu-message-id) (menu-callback :autodelete))
                 (t/delete-text @bot-token deliver-to  menu-message-id))
               (when (some? answer)
                 (:message_id
                  (if (some? (obj :messag-id))
                    (t/edit-text @bot-token deliver-to (:messag-id obj)  answer message-text)
                    (t/send-text @bot-token deliver-to  answer message-text)))))))
         (catch Exception e
           (timbre/with-merged-config
             {:appenders {:println {:enabled? false}
                          :spit (appenders/spit-appender {:fname "log/menu_callback.log"})}}
             (timbre/error  "Message delivery error " e))))))))

(defn simple-message [obj message]
  (send-response obj (ms/render-message message (:lang obj) {}) {}))

(defn select-menu-callback
  "
    Slpit callback data for menu-callback name and item-key parts
    Select menu-callback by name. 
    Returns menu-callback obj or nil 
  "
  [{:keys [message data inline_message_id from id] :as request}]
  (let [data (or data (:data message))
        [menu-callback-name item-key] (str/split data border)]
    (timbre/debug "Request " request)
    (if (some? menu-callback-name)
      (if-let [menu-callback  ((keyword menu-callback-name) (@bot :menu-callback))]
        {:callback-name menu-callback-name ;; get-data callback
         :menu-callback menu-callback ;; menu-callback object
         :callback-reg (menu-callback :callback-reg)
         :item-key-source item-key ;; row key-part of callback data
         :mode  (if (nil? inline_message_id) :chat :inline)
         :deliver-to (or   (-> message :chat :id) id)
         :menu-message-id (-> message :message_id)
         :lang (keyword (or (:language_code from) default-lang))
         :user-id (:id from)
         :request request}
        (timbre/error "Invalid menu callback not found for " request))
      (timbre/error "Invalid menu data in " request))))


(defn- setup-item-key [{:keys [item-key-source callback-reg menu-callback] :as obj}]
  (mu/setup-item-key (:callback menu-callback) obj callback-reg item-key-source))

(defn authorize-menu-callback
  "
   Check if user, who supplied the menu-callback athorized to run this menu-callback
   If yes, return valid menu-callback object.
   Otherwise send error message. 
  "
  [{:keys [menu-callback] :as obj}]
  (when (some? menu-callback)
    (mu/authorize obj (:role menu-callback) simple-message)))

(defn make-answer
  ([mcallback-object item] (make-answer mcallback-object item  (-> mcallback-object :menu-callback :message)))

  ([{:keys [item-key lang menu-callback]} item message-template]
   (timbre/debugf "Attempt to build answer: message-template: %s item: %s item-key: %s )" message-template item item-key)
   (when  (and (some? message-template) (some?  item))
     (ms/render-answer item-key lang (:menu menu-callback) item message-template))))


(defrecord MCallackQuery  [mcallback-object] TMessage
           (get-item-key [_] (:item-key mcallback-object))
           (get-query [_]  (:sm-query mcallback-object))
           (get-callback [_] (-> mcallback-object  :menu-callback :callback))
           (get-user-mail [_] ^String  (:user-smkey mcallback-object))
           (get-operator-id [_] ^String (:user-smkey mcallback-object))
           (get-role [_]  (:user-role mcallback-object))
           (error-response [_ error-code]
             (timbre/debug "Error response input " error-code " for " mcallback-object)
             (if (= error-code sm/RC_NO_MORE)
               (send-response mcallback-object  (make-answer mcallback-object
                                                             mcallback-object
                                                             :TBOTNotFound))
               (send-response mcallback-object  (make-answer mcallback-object
                                                             mcallback-object
                                                             (-> mcallback-object :menu-callback :errmessage)))))
           (response [_ sm-responce]
             (timbre/debug "Response input" sm-responce " for " mcallback-object)
             (if (some? sm-responce)
               (send-response mcallback-object (make-answer mcallback-object
                                                            sm-responce))
               (send-response mcallback-object (make-answer mcallback-object
                                                            mcallback-object
                                                            (-> mcallback-object :menu-callback :errmessage)))))
           (wait-a-minute [_] (MCallackQuery. (assoc mcallback-object :message-id
                                                     (:message_id (simple-message mcallback-object :CommandWaiting))))))


(defn process-menu-callback
  "
   Check if command found. 
   If command has configured callback - use it to request data from sm.  
   If item available from cache - create response and send to telegram, otherwise - send wait-a-minute message.
   If command does not have callback just generate response message. Internal command data available to use in message template. 
  "
  [{:keys [menu-callback callback-name item-key] :as obj}]
  (when (some? menu-callback)
    (if (some? callback-name)
      (if (some? item-key)
        (send-response obj (make-answer obj (cache/get-item (MCallackQuery. obj))))
        (send-response obj (make-answer obj obj (obj :errmessage))))
      (make-answer obj obj))))


(defn processor [menu-callback]
  (timbre/with-merged-config
    {:appenders {:min-level :debug
                 :println {:enabled? false}
                 :spit (appenders/spit-appender {:fname "log/menu_callback.log"})}}
    (some-> menu-callback
            select-menu-callback
            authorize-menu-callback
            setup-item-key
            process-menu-callback)))