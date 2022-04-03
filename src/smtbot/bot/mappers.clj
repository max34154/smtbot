(ns smtbot.bot.mappers
  (:require [cheshire.core :as json]
            ;[clojure.instant :as instant]
            [clojure.string :as str]
            [smtbot.utils.formater :as ftr]
            [taoensso.timbre :as timbre]
            [smtbot.bot.globals :refer [mapper default-lang]]))

(def max-block-length 1996)


(defn text-block [max-legnth]
  (fn [str-vec]
    (-> (reduce #(if-not (str/blank? %2)
                   (str %1 (str/trim %2) "\n")
                   (if (str/blank? %1) %1 (str %1 "\n"))) "" str-vec)
        (#(if (< max-legnth (count %)) (str (subs % 0  max-legnth) "...") %))
        (str/trim-newline))))

(def converters
  {:standart-short-time (fn [time-val]
                          (ftr/time-formater time-val "%1$tH:%1$tM %1$td-%1$tm-%1$ty"))
   :text-block (text-block max-block-length) 
   :text-block-500 (text-block 500)})

(defn- get-map-value
  ([value mp  lang]
   (when (some? value)
     (if (:indexed  mp)
       (#(or (lang %) (default-lang %) value)  (mp (keyword value)))
       (#(or (-> mp lang (get %)) (-> mp default-lang (get %)) value)
        (reduce (fn [i v] (if (= value v) (reduced  i) (inc i)))   0 (:keys mp)))))))



(defn apply-converter-list [message-template  obj]
  (timbre/debug "Template:" message-template " for obj:" obj)
  (reduce (fn [o {:keys [func field-name]}]
            (update o field-name (fn [val] (or
                                            (some-> converters (get func) (#(% val)))
                                            val))))  obj   (:converter-list message-template)))


(defn apply-mapper-list [{:keys [mapper-list]} lang obj]
  (reduce (fn [o {:keys [map-name field-name]}]
            (update o field-name get-map-value (@mapper map-name) lang))  obj   mapper-list))


(comment

  (def OpenTime "2022-01-07T13:29:57+03:00")

  (def  testSD
    {:LinkToSystem ["https://eaist.mos.ru/index.html#/definition-supplier/preparation-publication/13914838"],
     :ContactNameUserID "505776",
     :Group "ОМ ЕАИСТ 223",
     :Direction "ЕАИСТ",
     :CloseTime "2022-02-08T09:40:13+03:00",
     :CompanyINN "7712037162",
     :StatusForUser "closed",
     :Title "Подготовка к публикации (223-ФЗ)",
     :CompanyKPP "772601001",
     :ContactName "Масягина Наталья Васильевна",
     :OpenTime "2022-02-02T09:40:51+03:00",
     :ContactEmail "MasyaginaNV1@dfks.mos.ru",
     :Category "консультация",
     :CompanyName "ГАОУ ВО МГУСиТ",
     :InteractionID "SD1878232",
     :Description ["Прошу изменить статус закупки на опубликована 32211080901"],
     :Resolution [nil "08.02.22 09:40:13: Предоставлено решение" "Процедура - 32211080901 - Извещение опубликовано"],
     :ResolutionCode "Проведена консультация",
     :CompanyGRBS "Москомспорт",
     :Source "5",
     :CallbackContact "Масягина Наталья Васильевна"})

  ((converters :standart-short-time) OpenTime)
  #_{:clj-kondo/ignore [:unused-private-var]}
  (defn- get-map-obj
    ([obj [mp index] lang] (get-map-obj obj index mp lang))
    ([obj index mp lang]
     (get-map-value (get obj index) mp lang)))

  #_{:clj-kondo/ignore [:unused-private-var]}
  (defn- get-map-obj-json [obj-json index mp lang]
    (get-map-value (get (json/parse-string obj-json) index) mp lang))

  (get-map-value :open (:SDStatusForUser @mapper)  :en)
  (get-map-value "консультация" (:SDCategory @mapper)  :en)
  (apply-mapper-list {:I {"StatusForUser" "open" "Category" "консультация"}}
                     {:item :I :mapper-list [{:map-name :SDStatusForUser :field-name "StatusForUser"} {:map-name :SDCategory :field-name "Category"}]}
                     :en))