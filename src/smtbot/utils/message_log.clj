(ns smtbot.utils.message-log)

(def testable true)

(defonce MessageLog (atom []))

(defonce test-run (atom false))

(defn start-test []
  (reset! test-run  true)
  (reset! MessageLog []))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn end-test []
  (reset! test-run  false))

(defn add-answer-inline
  ([token inline-query-id results] (add-answer-inline token inline-query-id {} results))
  ([token inline-query-id options results]
   (let [message {:token token
                  :body (into {:inline_query_id inline-query-id :results results} options)}]
     (swap! MessageLog conj message))))

(defn add-edit-text
  ([token chat-id message-id text] (add-edit-text token chat-id message-id {} text))
  ([_ chat-id message-id options text]
   (swap! MessageLog conj
          (into {:chat_id chat-id :text text :message_id message-id} options))
   {:message_id  481}))

(defn add-send-text
  ([token chat-id text] (add-send-text token chat-id {} text))
  ([_ chat-id  options text]
   (swap! MessageLog conj
          (into {:chat_id chat-id :text text} options))
   {:message_id  481}))

(defn add-answer-callback
  ([_ deliver-to message-text] (add-answer-callback _ deliver-to message-text false))
  ([_ deliver-to message-text show-alert]
   (swap! MessageLog conj {:callback_query_id deliver-to :text message-text :show_alert show-alert})))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn get-last-messages []
  (let [Messages @MessageLog]
    (start-test)
    Messages))

(defmacro test-run? []
  (if smtbot.utils.message-log/testable `@smtbot.utils.message-log/test-run false))

