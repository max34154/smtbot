(ns smtbot.bot.config
  {:clj-kondo/config  '{:linters {:unused-referred-var
                                  {:exclude {taoensso.timbre [log  trace  debug  info  warn  error  fatal  report
                                                              logf tracef debugf infof warnf errorf fatalf reportf
                                                              spy get-env]}}}}}
  (:refer-clojure :exclude [load])
  (:require [yaml.core :as yaml]
            [smtbot.utils.keywordizer :as k]
            [smtbot.bot.globals :refer [bot mapper messages callback-map]]
            [smtbot.cache.cache :as cache :refer [read-callbacks]]
            [taoensso.timbre :as timbre
             :refer [log  trace  debug  info  warn  error  fatal  report
                     logf tracef debugf infof warnf errorf fatalf reportf
                     spy get-env]]))

(def default-conf-path "./config/")

(defn options-list-remap [options]
  (reduce  #(assoc %1 (%2 :option) (dissoc %2 :option))   {} options))

(defn command-options-remap [command]
  (reduce  (fn [command command-key]
             (if (some? (-> command command-key :options))
               (update-in command [command-key :options] options-list-remap)
               command))     command (keys command)))


(defn role-process [command]
  (reduce  (fn [command command-key]
             (if-let [role  (-> command command-key :role)]
               (update-in command [command-key :role]
                          (if (string? role)
                            keyword
                            #(apply hash-set (map keyword %))))
               command))     command (keys command)))

(defn synonim-role-process [synonim]
  (reduce  (fn [synonim synonim-key]
             (if (some? (-> synonim synonim-key :role-base))
               (update-in synonim [synonim-key :role-base]
                          (fn [v]
                            (into {}
                                  (map #(assoc {} (key %) (update  (val %) :item-reg re-pattern))
                                       v))))
               synonim))
           synonim
           (keys synonim)))

(defn- read-config [path]
  (let [path (or path default-conf-path)]
    (conj (-> (yaml/from-file (str path "commands.yml"))
              (update :command k/keywordize-map [:message :errmessage :callback :menu])
              (update :command k/mk-pattern-map [:callback-reg])
              (update :command k/process-internal-vec [:options] [:option :message :errmessage :callback :menu] [:callback-reg])
              (update :command command-options-remap)
              (update :command role-process)
              (update :inline k/keywordize-vec [:title :menu :callback  :message])
              (update :inline k/mk-pattern-vec [:reg :callback-reg])
              (update :inline (fn [v] (map
                                       (fn [vv] (update vv :role
                                                        #(if  (vector? %)
                                                           (apply hash-set (map keyword %))
                                                           (keyword %))))
                                       v)))
             ; (update :callback k/keywordize-map [:role :text :callback])
             ; (update :callback k/mk-pattern-map [:callback-reg])
              (update :menu-callback k/keywordize-map [:text :callback :message :errmessage])
              (update :menu-callback k/mk-pattern-map [:callback-reg])
              (update :menu-callback role-process)
             ; (update :menu-callbac role-process)
              (update :menu k/keywordize-map-vec2 [:text :callback-data])
              (update :synonim k/mk-pattern-map [:item-reg])
              (update :synonim synonim-role-process)))))

(defn- read-mappers [_ path]
  (let [path (or path default-conf-path)]
    (yaml/from-file (str path "mappers.yml"))))

(defn- read-messages [_ path]
  (let [path (or path default-conf-path)]
    (->>
     (-> (yaml/from-file (str path "messages.yml"))
         (k/keywordize-map [:item]))

     (map (fn [[k v]] {k (if (some? (v :mapper-list))
                           (update v :mapper-list k/keywordize-vec [:map-name :field-name])
                           v)}))
     (reduce conj {})
     (map (fn [[k v]] {k (if (some? (v :converter-list))
                           (update v :converter-list k/keywordize-vec [:func :field-name])
                           v)}))
     (reduce conj {}))))


(defn- check-config [conf]
  (reduce (fn [conf [k v]] (if (nil? v)
                             (do
                               (fatalf " Incorrect config - section <<%s>> not found" (name k))
                               (reduced nil))
                             conf)) conf conf))

(defn configure
  ([] (configure default-conf-path))
  ([path] (send bot (fn [_ p] (check-config (read-config p))) path)
          (send mapper read-mappers path)
          (send messages read-messages path)
          (send callback-map read-callbacks path)
          (when-not (await-for 10000 bot mapper messages callback-map)
            (throw (AssertionError.  "Bot configuration error! ")))))

(defn simulate-configure
  ([] (simulate-configure default-conf-path))
  ([path]
   (assoc {}
          :bot (check-config (read-config  path))
          :mapper (read-mappers nil path)
          :messages (read-messages nil path)
          :callback-map (read-callbacks nil path))
   #_(configure "test/config/run/")))

(comment (simulate-configure "test/config/run/")
         (-> (simulate-configure "test/config/run/") :bot :synonim))