(ns smtbot.bot.message
  (:require [smtbot.bot.globals :refer [default-lang messages]]
            [clostache.parser :as parser]
            [smtbot.bot.mappers :refer [apply-mapper-list apply-converter-list]]
            [taoensso.timbre :as timbre :refer [error spy]]))


;; TODO: Formating options support.
;; TODO: Special symbols must be escaped accordign to format specification before  
;; TODO: render
;; https://core.telegram.org/bots/api#formatting-options
;;
(defmacro parse-options [msg-id] `(-> @smtbot.bot.globals/messages (get ~msg-id) :format))

(defn render-message
  "
   Render timeplate with provided data
   Parameters:
   msg-id - template id 
   lang - lang code, if ommited default lang applyed 
   data - data object to fill in template.
   Template may have data conversion and mapping function. 
   Exectution order is 
      1.Converters - change data formats  
      2.Mapper - map inernal dictionary value into human readable form in supplyed lang
      3.Render mustache template 
   Return: String 
  "

  ([msg-id data]
   (render-message msg-id default-lang data))

  ([msg-id lang data]
   (if-let [msg-template  (@messages msg-id)]
     (let [lang (if (some? (msg-template lang)) lang default-lang)
           item (msg-template :item)
           transform  (if (some? item)
                        (fn [data]  (->> (get data item)
                                         (apply-converter-list msg-template)
                                         (apply-mapper-list msg-template lang)))
                        (fn [data]
                          (->> data
                               (apply-converter-list msg-template)
                               (apply-mapper-list   msg-template lang))))]
       (if-not (empty? data)
         (parser/render (msg-template lang)
                        (if (some? (data :content))
                          {:content  (doall (map  transform (data :content)))}
                          (spy :debug (transform data))))
          (parser/render (msg-template lang))))
     (error "Message template not found for msg-id=" msg-id))))


(defn menu-row-fabric [item item-key lang]
  (fn [menu-row]
    (map (fn [menu-item]
           (-> menu-item
               (update :callback_data str item-key)
               (update :text render-message lang item)))  menu-row)))


(defn build-inline-keyboard
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
  [resp item item-key lang keyboard-name ]
  (if-let [menu-template (-> @smtbot.bot.globals/bot :menu keyboard-name)]
    (let [menu (map  (menu-row-fabric item item-key lang) menu-template)]
      (if-not (empty? menu)
       (assoc resp :reply_markup {:inline_keyboard menu})
        resp))
    resp))



(defn render-answer
  "
   Create answer object with t/send_text supported options:
     :text - rendr
     :parse_mode -  MarkdownV2|HTML
     :reply_markup - menu, use  mu/build-inline-keyboard to create menu
   
    Parameters:  
    item-key - item uqiue key 
    lang - menu language 
    menu - unique menu name
    item -  value returned by sm 
    message-template - message template id
  "


  ([item-key lang menu item message-template]
   (-> {:text (render-message message-template lang item)}
       (#(if-let [parse-mode  (parse-options  message-template)]
           (assoc % :parse_mode parse-mode)
           %))
       (#(case menu ;(or (nil? menu) (false? menu) ())
           (nil false :NoMenu) %
           (build-inline-keyboard % item item-key lang menu)
           #_(assoc % :reply_markup (build-inline-keyboard item item-key lang menu)))))))








