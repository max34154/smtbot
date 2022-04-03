(ns smtbot.message-format-checker)


#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(def inline-answer-exmaple [{:token "5142232953:AAHKj2XXwB-J9MKfs4Xtn7GKjYHpsTa1aZw",
                             :body
                             {:inline_query_id 151488382774279532,
                              :results [{:type "article", :id "SD1848894:1647410748573", :title "Ищем SD1848894..."}]}}
                            {:token "5142232953:AAHKj2XXwB-J9MKfs4Xtn7GKjYHpsTa1aZw",
                             :body
                             {:inline_query_id 151488382774279532,
                              :results
                              [{:type "article",
                                :id "SD1848894:1647410748780",
                                :title ": -  ",
                                :input_message_content {:message_text ": -  "},
                                :reply_markup
                                {:inline_keyboard
                                 [[{:text "Краткая информация", :callback_data "breifSD$1$SD1848894"}]
                                  [{:text "Описание", :callback_data "questionSD$1$SD1848894"}]
                                  [{:text "Решение", :callback_data "answerSD$1$SD1848894"}]]}}]}}])


(defn parse-mode-checker [errors parse_mode]
  (if-not (or (nil? parse_mode)
              (= parse_mode "HTML")
              (= parse_mode "MarkdownV2")) (conj errors (str "Invalid parse_mode: " parse_mode)) errors))

(defn menu-row-checker [errors menu-row]
  (reduce (fn [err i]  (if (or (empty? (i :text))  (empty? (i :callback_data)))
                         (conj err  (str "invalid result menu item " i)) err))   errors menu-row))

(defn reply-markup-checker [errors reply_markup]
  (if (some? reply_markup)
    (if (nil? (reply_markup :inline_keyboard))
      (conj errors  "invalid result reply_markup")
      (reduce menu-row-checker  errors (reply_markup :inline_keyboard))
      #_(doseq [m1 (reply_markup :inline_keyboard)]
          (doseq [i m1]
            (when (or (empty? (i :text))  (empty? (i :callback_data)))
              (conj errors  (str "invalid result menu item " i))))))
    errors))

(defn inline-answer-body-result  [{:keys [type id title input_message_content reply_markup]}]
  (-> []
      (#(if-not (= type "article") (conj % (format "invalid result type <<%s>>, must by <<article>>" type)) %))
      (#(if (empty? id) (conj %  "empty result id") %))
      (#(if (empty? title) (conj %  "empty result title") %))
      (#(if (and (some? input_message_content) (empty? (input_message_content :message_text)))
          (conj %  "invalid result input_message_content") %))
      (parse-mode-checker (:parse_mode input_message_content))
      (reply-markup-checker reply_markup)
      #_(when (some? reply_markup)
          (if (nil? (reply_markup :inline_keyboard))  (conj errors  "invalid result reply_markup")
              (doseq [m1 (reply_markup :inline_keyboard)]
                (doseq [i m1]
                  (when (or (empty? (i :text))  (empty? (i :callback_data)))
                    (conj errors  (str "invalid result menu item " i)))))))))

(defn inline-answer-body [{:keys [inline_query_id results]}]
  (-> []
      (#(if (or (nil? inline_query_id)
                (and (string? inline_query_id)
                     (empty? inline_query_id)))  (conj % "inline_query_id is empty") %))
      (#(if-not (vector? results)
          (conj % "invalid results format - must be vector")
          (reduce  (fn [errors result] (into errors  (inline-answer-body-result result)))  % results)))))

(defn inline-answer [{:keys [token body]}]
  (-> []
      (#(if-not (string? token) (conj % (str "Invalid token " token)) %))
      (#(if (empty? body)
          (conj % "Body is empty")
          (into % (inline-answer-body body))))))


(defn command-answer [{:keys [chat_id text parse_mode reply_markup]}]
  (-> [] 
      (#(if (nil? chat_id) (conj % "chat_id is empty") %))
      (#(if (nil? text) (conj % "text is empty") %))
      (parse-mode-checker parse_mode)
      (reply-markup-checker reply_markup)))

(defn check-message-vec [check-func message-vector]
  (reduce #(into %1 (check-func %2)) [] message-vector))