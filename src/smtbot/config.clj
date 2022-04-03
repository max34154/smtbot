(ns smtbot.config
  {:clj-kondo/config  '{:linters {:unused-referred-var
                                  {:exclude {taoensso.timbre [log  trace  debug  info  warn  error  fatal  report
                                                              logf tracef debugf infof warnf errorf fatalf reportf
                                                              spy get-env]}}}}}
  (:refer-clojure :exclude [load])
  (:require [yaml.core :as yaml]
            [smtbot.utils.crypto :refer [encrypt decrypt]]
            [smtbot.utils.base64 :refer [string->b64]]
            [smtbot.utils.misc :refer [write-yml-file]]
            [taoensso.timbre :as timbre
             :refer [log  trace  debug  info  warn  error  fatal  report
                     logf tracef debugf infof warnf errorf fatalf reportf
                     spy get-env]]
            ;[clojure.java.io :as io]
            ))

;; TODO - base-path and async-service must be configurable 
;; Parse a YAML file

(def ^:const main-config-file "smtbot.yml")

(def ^:const default-values
  '([:to-many-threads-sleep 100]
    [:server-not-availalbe-sleep 3000]
    [:auth-service "asyncuser"]
    [:async-service "async"]
    [:base-path "/SM/9/rest/"]))

(def default-lock-chunk-size 10)

(def default-threads-per-manager 2)

(def keystore (atom {}))

(def config (atom {}))

(def default-conf-path "./config/")

(defn- get-key [key]
  (@keystore key))

(defn- decode-password
  ([raw login password key]
   (let [key (get-key key)]
     (if (nil? key)  raw
         (assoc raw password (decrypt key (raw login)))))))


(defn- encode-password
  ([raw login password key]
   (swap! keystore assoc  key (encrypt (password raw) (login raw)))
   (dissoc raw password)))


(defn- build-mime-types [raw]
  (assoc raw :mime-types (reduce #(assoc %1 (keyword %2) true)
                                 {}
                                 (raw :mime-types))))

(def ^:private ^:const keylist [[:global-login :global-password  :g]
                                [:async-login :async-password :a]
                                [:login :password :c]
                                [:db-login :db-password :db]
                                [:admin :admin-password :d]])

(defn- decoder [raw]
  (reduce   (fn  [raw  creads]
              (let [login (creads 0)
                    password (creads 1)
                    key (creads 2)]
                (if  (nil? (login raw))
                  raw
                  (if (nil? (password raw))
                    (decode-password raw login password key)
                    (do
                      (encode-password raw login password key)
                      raw)))))
            raw keylist))

(defn- password-remover [raw]
  (reduce #(dissoc %1 (%2 1)) raw keylist))


(defn set-default [config default]
  (if (nil? (default 0))
    (do (error "Incorrect default " default ". Skiped.")
        config)
    (if (nil? (config (default 0)))
      (assoc config  (default 0) (default 1))
      config)))

(defn fill-config-defaults [config]
  (assoc config :config
         (reduce set-default
                 (config :config)
                 default-values)))

(defn calc-config-defaults [config]
  (assoc config :config (->> (config :config)
                             (#(assoc % :base-url
                                      (str "http://" (%  :module_ip) (% :base-path))))
                             (#(assoc % :auth-url
                                      (str  (% :base-url) (% :auth-service))))
                             (#(assoc % :async-action-url
                                      (str  (% :base-url) (% :async-service))))
                             build-mime-types)))


(def file-not-found-msg "Mandatory configuration file %s is corrupted or not found in %s")

(defn- config-file-not-found [path base workers db]
  (when-not (some? base) (fatalf file-not-found-msg main-config-file path))
  (when-not (some? db) (fatalf file-not-found-msg "db.yml" path))
  (when-not (some? workers) (fatalf file-not-found-msg "workers.yml" path)))


(defn read-config [path]
  (let [base-file (str path main-config-file)
        workers-file (str path "workers.yml")
        db-file (str path "db.yml")
        keystore-file (str path "keystore.yml")
        globals-file (str path "globals.yml")
        messengers-file (str path "messengers.yml")]
    (with-local-vars [local-config* {}]
      (let [workers  (yaml/from-file workers-file)
            db (yaml/from-file db-file)
            base (yaml/from-file base-file)
            globals (yaml/from-file globals-file)
            messengers (yaml/from-file messengers-file)]
        (if (and (some? workers) (some? base) (some? db))
          (do
            (reset! keystore (yaml/from-file keystore-file))
            (var-set local-config* (-> {}
                                       (assoc :workers (decoder workers))
                                       (assoc :messengers  (if (some? messengers) messengers {}))
                                       (assoc :config (decoder base))
                                       (assoc :database (decoder db))
                                       (assoc :executors-globals
                                              (if (some? globals) globals {}))))

            (write-yml-file keystore-file @keystore)
            (write-yml-file workers-file (password-remover workers) {:flow-style :block})
            (write-yml-file base-file (password-remover base) {:flow-style :block})
            (write-yml-file db-file (password-remover db) {:flow-style :block})
            (var-get local-config*))
          (config-file-not-found path base workers db))))))

(defn- fill-workers [workers]
  (assoc workers :dedicated
         (for [w (workers :dedicated)]
           (->> w
                (#(if (nil? (:get-allowed %))
                    (assoc % :get-allowed (workers :dedicated-get-allowed)) %))
                (#(if (nil? (:threads %))
                    (assoc % :threads (workers :dedicated-threads)) %))
                (#(if (nil? (:chank-size %))
                    (assoc % :chank-size (workers :dedicated-chank-size)) %))))))

(defn- configure-workers [global-configuration]
  (assoc global-configuration :workers
         (->> global-configuration
              :workers
              (#(if (= (% :dedicated-get-allowed) true) %
                    (assoc % :dedicated-get-allowed false)))
              (#(if (nil? (% :dedicated-threads))
                  (assoc % :dedicated-threads default-threads-per-manager)
                  %))
              (#(if (nil? (% :dedicated-chank-size))
                  (assoc % :dedicated-chank-size default-lock-chunk-size)
                  %))
              (#(assoc % :global-credentials
                      (str "Basic " (string->b64 (str (% :global-login) ":" (% :global-password))))))
              (#(assoc % :async-credentials
                       (str "Basic " (string->b64 (str  (% :async-login) ":" (% :async-password))))))
              fill-workers)))

(defn- fill-messengers [messengers]
  (assoc messengers :dedicated
         (for [w (messengers :dedicated)]
           (->> w
                (#(if (nil? (:threads %))
                    (assoc % :threads (messengers :dedicated-threads)) %))
                (#(if (nil? (:chank-size %))
                    (assoc % :chank-size (messengers :dedicated-chank-size)) %))))))

(defn- configure-messengers [global-configuration]
  (assoc global-configuration :messengers
         (->> global-configuration
              :messengers
              (#(if (nil? (% :dedicated-threads))
                  (assoc % :dedicated-threads default-threads-per-manager)
                  %))
              (#(if (nil? (% :dedicated-chank-size))
                  (assoc % :dedicated-chank-size default-lock-chunk-size)
                  %))
              fill-messengers)))


(defn create-uid-generator [_config]
  (assoc  _config  :get-unique-id
          (let [i (atom 0)
                module-name (-> _config  :config :async-gateway-id)]
            (fn  []
              (format "%X-%X-%s"
                      (quot (System/currentTimeMillis) 1000)
                      (swap! i inc)
                      module-name)))))

(defn collection-keylist-builder [config]
  (assoc config :collections-keylist
         (reduce #(if (and (some? (:name %2)) (some? (:keylist %2)))
                    (conj %1 {(keyword (:name %2))  (:keylist %2)})
                    %1) {} (-> config :config :collections))))

(defn configure [path]
  (reset! config
          (when-let [_config (read-config
                              (if (empty? path) default-conf-path path))]
            (-> _config
                (assoc :path path)
                fill-config-defaults
                calc-config-defaults
                configure-workers
                collection-keylist-builder
                configure-messengers
                create-uid-generator))))



(defmacro get-config
  ([]  `(@smtbot.config/config :config))
  ([key] `(-> @smtbot.config/config :config ~key)))

(defmacro get-workers
  ([]  `(@smtbot.config/config :workers))
  ([key] `(-> @smtbot.config/config :workers ~key)))


(defmacro get-messengers
  ([]  `(@smtbot.config/config :messengers))
  ([key] `(-> @smtbot.config/config :messengers ~key)))


(defmacro get-executors-globals
  ([]  `(@smtbot.config/config :executors-globals))
  ([key] `(-> @smtbot.config/config :executors-globals ~key)))

(defmacro get-module-name [] `(-> @smtbot.config/config :config :async-gateway-id))

(defmacro get-uid []  `((@smtbot.config/config  :get-unique-id)))


(defmacro get-keylist [collection]
  `(-> @smtbot.config/config :collections-keylist ~collection))



;(defn set-module-name [name] (reset! module_name name))



