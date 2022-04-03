(ns smtbot.bot.messages.utils
  (:require
   [smtbot.bot.message :refer [render-message]]
   [taoensso.timbre :as timbre]
   [smtbot.bot.query-processor :refer [query-processor]]
   [smtbot.bot.users.operations :as user-ops]
   [smtbot.bot.globals :refer [callback-map]]))

(defmacro get-item-key [regexp source-string]
  `(clojure.string/join "/" (rest (re-find ~regexp ~source-string))))


(defn menu-row-fabric [item item-key lang]
  (fn [menu-row]
    (map (fn [menu-item]
           (-> menu-item
               (update :callback_data str item-key)
               (update :text render-message lang item)))  menu-row)))

(def constantly-nil  (constantly nil))

#_(defmacro build-inline-keyboard
    "
   Create inline keyboard.
   Parameters:  
    item -  value returned by sm  
    item-key - item uqiue key 
    lang - menu language 
    keyboard-name - unique menu name 
   Return:
    inline keyboard object.
  "
    [item item-key lang keyboard-name]
    `{:inline_keyboard (map  (smtbot.bot.messages.utils/menu-row-fabric ~item ~item-key ~lang) (-> @smtbot.bot.globals/bot :menu ~keyboard-name))})


#_(defn simple-message
    "
   Send text-only message to the chat. 
   Paramters 
    - obj -  object. {:chat-id <unique-chat-id>  :lang <lang-code> }
    - message - message template id 
  "
    [obj message log debug]
    (if (m-log/test-run?)
      (m-log/add-send-text @smtbot.bot.globals/bot-token (:chat-id obj)
                           (render-message message (:lang obj) {}))

      (constantly-nil
       (try
         (if debug
           (timbre/with-merged-config
             {:appenders {:println {:enabled? false}
                          :spit (appenders/spit-appender {:fname log})}}
             (timbre/debugf "->Telegram "
                            (into {:chat_id (:chat-id obj)
                                   :text (render-message message (:lang obj))}))
             (let [tresp (t/send-text @smtbot.bot.globals/bot-token
                                      (:chat-id obj)
                                      (render-message message (:lang obj) {}))]
               (timbre/debug "<-Telegram " tresp)
               (:message_id tresp)))
           (t/send-text @smtbot.bot.globals/bot-token (:chat-id obj)
                        (render-message message (:lang obj) {})))

         (catch Exception e
           (timbre/with-merged-config
             {:appenders {:println {:enabled? false}
                          :spit (appenders/spit-appender {:fname log})}}
             (timbre/error  "Message delivery error " e)))))))

(defn setup-item-key
  "
   Set up 
    - item-key for post, put and get request;
    - sm-query and item-key for get-query request
   
   Parameters:
   - callback-id - callback key in callback-map, keyword 
   - obj -  action object (inline, command or menu-callback) 
   - item-key-re - reg-exp to select item-key or query parameters from request-text 
   - request-text - user typed text from telegram

  "
  [callback-id obj item-key-re request-text]
  (if-let [callback  (@callback-map  callback-id)]
    (do
      (timbre/debug "Callback found:" callback)
      (case (callback :action)
        #_(:post :put :get)
        ("post" "put" "get") (if (some? item-key-re)
                               (assoc obj :item-key (get-item-key item-key-re request-text))
                               obj)
        "get-query" (if (some? (callback :query))
                      (#(assoc obj :sm-query % :item-key (str (hash %)))
                       (query-processor (callback :query) obj
                                        (when (some? item-key-re)
                                          (rest (re-find item-key-re request-text)))))
                      (timbre/errorf "Query messing in get-query callback %s" callback))
        (do
          (timbre/errorf "Undefined callback action <<%s>> in callback %s" (get callback :action) callback)
          obj)))
    (do
      (timbre/debug "Callback  not found using callback id " callback-id)
      obj)))






(def default-message-map
  {:user-ok :TBOTNotPremited
   :user-blocked  :TBOTUserBlocked
   :user-not-found :TBOTRegReq
   :reg-in-progress :TBOTRegInProgress
   :user-invalid :TBOTUserCheck})



;;TODO - user data filter. Use requestor field to filter out from sm responce data 
;;TODO   should not be visible for particular user
;;TODO - role list 
(defn authorize
  "
   Authorize user.
   If authorized, return supplied object.
   Otherwise send error message and return nil. 
   Parameters:
    obj - inline, command or menu-callaback object 
    required-role -  minimal required user level [:any :user :operator]
    message-sender - function with two pramaters sends message to telegram
                     parameters:
                     first - obj (first parameter of authorize function)
                     second - message template id 
    message-map - error message map, contains message template ids. 
                  Options, if not specified default-message-map is used
                  It should have messages for the following keys: 
                  :user-ok  - not enough priveleges 
                  :user-blocked - user disable 
                  :user-not-found - user not found 
                  :reg-in-progress - user have to complete registation before proceed 
                  :user-invalid - user expired, in many ways its technical error means that user update procedure fault
  "
  ([obj required-role  message-sender] (authorize obj required-role  message-sender default-message-map))

  ([obj required-role  message-sender message-map]
   (if (or (nil? required-role) (= required-role :any))
     obj
     (-> obj
         :user-id
         user-ops/get-user
         (#(assoc % :user-status  (user-ops/get-user-status %)))
         (#(if (and (= (:user-status %) :user-ok)
                    (user-ops/authorize required-role %))
             #_(assoc obj :user-key (:smkey %) :user-role (:role ))
             (user-ops/set-role obj %)
             (do
               (timbre/debugf "Not authorized: obj %s \n       user: %s \n         message-id:%s"
                              obj % (message-map (:user-status %)))
               (message-sender obj (message-map (:user-status %)))
               nil)))))))

