(ns smtbot.bot.messages.inline
  (:require
   [smtbot.bot.globals :refer [bot bot-token
                               default-lang 
                              ; callback-map
                               bild-id]]
   ;[smtbot.bot.query-processor :refer [query-processor]]
   [smtbot.bot.users.operations :as user-ops]
   [smtbot.bot.messages.utils :as mu]
   [taoensso.timbre :as timbre]
   [smtbot.cache.cache :as cache]
   [morse.api :as t]
   [smtbot.utils.message-log :as m-log]
   [smtbot.bot.messages.tmessage :refer [TMessage]]
   [taoensso.timbre.appenders.core :as appenders]
   [smtbot.bot.message :as ms]))



;; Inline object 
;;    inline ->  inline command 
;;                callback 

;; Inline object structure
;; query - inline query from telegram message 
;; id - inlined message id from telegram message 
;; lan - selected message lang
;; inline - inline command, in found or nil 
;; callback - call back unique id 
;; sm-query - if callback has query - calculated callback query (all macros replaced by real values)
;; item-key - if sm-query not nil hash from sm-query otherwise substring from query selected using (inline :callback-reg) regexp 
;; item - result of callback from L1/L2 cache or :waiting if missed 

;; Inline message format 
;; Received inline:  {:id 151488382774279532, 
;;                    :from {:id 35271137, 
;;                           :is_bot false, 
;;                           :first_name Max, 
;;                           :last_name Ahmed, 
;;                           :language_code en}, 
;;                    :chat_type private, 
;;                    :query SD2342122, 
;;                    :offset }

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn- select-inline [{:keys [query id from]}]
  {:query  query
   :id id
   :user-id (:id from)
   :lang (keyword (or (:language_code from) default-lang))
  ; :request-type :inline
   :inline (reduce  #(if (re-find (:reg %2) query)  (reduced %2) nil) nil  (@bot :inline))})

#_(defn- setup-item-key [{:keys [query inline] :as obj}]
    (if-let [callback  (@callback-map (inline :callback))]
      (do
        (timbre/debug "Callback found:" callback)
        (case (callback :action)
          #_(:post :put :get)
          ("post" "put" "get") (if (some? (:callback-reg inline))
                                 (assoc obj :item-key (mu/get-item-key (:callback-reg inline) query))
                                 obj)
          "get-query" (if (some? (callback :query))
                        (#(assoc obj :sm-query % :item-key (str (hash %)))
                         (query-processor (callback :query) obj (rest (re-find (:callback-reg inline) query))))
                        (timbre/errorf "Query messing in get-query callback %s" (callback :callback)))
          (timbre/errorf "Undefined callback action <<%s>> in callback %s" (get callback :action) callback)))
      (do
        (timbre/debug "Callback  not found using callback id " (obj :callback))
        obj)))

(defn- setup-item-key [{:keys [query inline] :as obj}]
  (mu/setup-item-key  (inline :callback) obj (:callback-reg inline) query))




#_{:clj-kondo/ignore [:unused-private-var]}
(defn- waiting [{:keys [lang item-key] :as obj}]
  (let  [mesg-text (ms/render-message :InlineWaiting lang obj)]
    {:type "article"
     :id  (bild-id item-key)
     :title mesg-text
     :input_message_content
     {:message_text mesg-text}}))

(defn- not-found [{:keys [lang item-key] :as obj}]
  (let  [mesg-text (ms/render-message :InlineNotFound lang obj)]
    {:type "article"
     :id  (bild-id item-key)
     :title mesg-text
     :input_message_content
     {:message_text mesg-text}}))


(defn- make-answer
  ([{:keys [item-key inline lang]} item]
   (timbre/debug "make response: inline %s  lang %s"  inline lang)
   (timbre/debug "make response: item "   item)
   (when (some? item)
     (let [resp
           {:type "article"
            :id   (bild-id item-key)
            :title (ms/render-message (inline :title) lang item)
            :input_message_content (if-let [parse_mode (-> @smtbot.bot.globals/messages (get (inline :message))  :format)]
                                     {:message_text (ms/render-message (inline :message) lang item)
                                      :parse_mode parse_mode}
                                     {:message_text (ms/render-message (inline :message) lang item)})}]
       (if (some? (inline :menu))
         (ms/build-inline-keyboard resp item item-key lang (inline :menu))
         resp)))))

(defn- send-answer [obj answer]
  (when (some? answer)
    (if (m-log/test-run?)
      (m-log/add-answer-inline  @bot-token (:id obj) [answer])
      (try
        (if (timbre/may-log? :debug)
          (timbre/with-merged-config
            {:appenders {:println {:enabled? false}
                         :spit (appenders/spit-appender {:fname "log/inline.log"})}}
            (timbre/debug "->Telegram " answer)
            (let [tresp (t/answer-inline @bot-token (:id obj) [answer])]
              (timbre/debug "<-Telegram " tresp)
              (:message_id tresp)))
          (:message_id (t/answer-inline @bot-token (:id obj) [answer])))
        (catch Exception e
          (timbre/with-merged-config
            {:appenders {:println {:enabled? false}
                         :spit (appenders/spit-appender {:fname "log/inline.log"})}}
            (timbre/error  "Message delivery error " e)))))))

(defrecord InlineQuery  [inline-object] TMessage
           (get-item-key [_] (:item-key inline-object))
           (get-query [_]  (:sm-query inline-object))
           (get-callback [_] (-> inline-object :inline :callback))
           (get-user-mail [_] ^String  (:user-smkey inline-object))
           (get-operator-id [_] ^String (:user-smkey inline-object))
           (get-role [_]  (:user-role inline-object))
           (response [_ sm-responce] (send-answer inline-object (make-answer inline-object sm-responce)))
           (error-response [_ _] (send-answer inline-object (not-found inline-object)))
           (wait-a-minute [_] (InlineQuery. inline-object)
             #_(InlineQuery.
                (assoc inline-object :message-id
                       (send-answer inline-object (waiting inline-object))))))

(defn- process-inline [obj]
  (if (some? (:inline obj))
    (-> obj
        setup-item-key
        (#(if (some? (:item-key %))
            (send-answer
             %
             (make-answer
              %
              (cache/get-item (InlineQuery. %))))
            (timbre/debug "Missing :item-key in" %))))
    (timbre/debug "Missing :inline in " obj)))

(defn- user-blocked [{:keys [lang]}]
  {:type "article"
   :id  (bild-id "blocked")
   :title (ms/render-message :TBOTUserBlocked lang {})})

(defn- registration-required [{:keys [lang]}]
  {:type "article"
   :id  (bild-id "regreq")
   :title (ms/render-message :TBOTRegReq lang {})})

(defn- registration-in-progress [{:keys [lang]}]
  {:type "article"
   :id  (bild-id "reginprg")
   :title (ms/render-message :TBOTRegInProgress lang {})})

(defn- check-in-progress [{:keys [lang]}]
  {:type "article"
   :id  (bild-id "chinprg")
   :title (ms/render-message :TBOTUserCheck lang {})})

(defn- not-permited [{:keys [lang]}]
  {:type "article"
   :id  (bild-id "notpermited")
   :title (ms/render-message :TBOTNotPremited lang {})})


(defn- authorize-inline [obj]
  (when (some? (:inline obj))
    (let [required-role (-> obj :inline :role)]
      (timbre/debugf "Required role is <<%s>>" required-role)
      (if (or (nil? required-role) (= required-role :any))
        obj
        (-> obj
            :user-id
            user-ops/get-user
            (#(case (user-ops/get-user-status %)
                :user-ok (if (user-ops/authorize required-role %)
                           obj
                           (send-answer obj (not-permited obj)))
                :user-blocked (send-answer obj (user-blocked obj))
                :user-not-found (send-answer obj (registration-required obj))
                :reg-in-progress (send-answer obj (registration-in-progress obj))
                :user-invalid (send-answer obj (check-in-progress obj)))))))))

(defn- debug [item message] (timbre/debug message item) item)

(defn processor [inline]
  (some-> inline
          select-inline
      ;(debug "afer select-inline ")
          authorize-inline
      ;(debug "afer authorize-inline ")
          process-inline))

