(ns smtbot.cache.cache
  (:require
   [yaml.core :as yaml]
   [cheshire.core :as json]
   [taoensso.timbre :as timbre]
   [clojure.core.async :as a :refer [>!! chan  dropping-buffer close!]]
   [smtbot.config :as config :refer  [default-conf-path get-uid]]
   [smtbot.dal.globals :refer [botcache-action request-action]]
   [smtbot.task.sync_pusher :as pusher-manager]
   [smtbot.enum.sm :as sm]
   ;[smtbot.utils.sm_resp_decode :refer [get-jbody]]
   ;[smtbot.enum.sm :as sm]
   [smtbot.cache.acl-checker :refer [check-access-fabric]]
   [smtbot.bot.messages.tmessage :as tmessage]
   ;[taoensso.timbre.appenders.core :as appenders]
   [smtbot.bot.globals :refer [callback-map no-cache-buffer]]
   [smtbot.cache.result-processor :as rp]
   #_[smtbot.utils.macro :refer [expire-at-timestamp]]))

(def ^:private get-L2-item  (delay (@botcache-action :get-item)))

#_(def ^:private update-L2-item  (delay (@botcache-action :update-item)))

(def ^:private default-expire-time (* 1000 600))

(def ^:private max-channel-size 100)

(def ^:private ttl-numerator 10)

;(defonce callback-map (agent {}))

;; Callback structure 
;;  - callback - callback id (keyword)
;;  - expire - time to live result in cache, seconds 
;;  - service - service (collection name) to request source for item 
;;  - action  - name of the action, defined in SM for specified servic or special value get
;;  - collection-item-key - collection item unique key name
;;  - query  -  only for get action, valid sm-api query, may contains following special macros
;;              - $user$ - user id in SM, for registred user only, 
;;              - $ctime$ - current time adjusted to minutes 
;;              - $tod$ - start of current date 
;;

;; callback-map structure 
;; Hash map:
;;   key - callback id (keyword)
;;   val - all fileds of callback structure 
;;       - ttl - cache buffer life time (1/10 of default-expire-time)
;;       - buffer  - hash map of cache buffers for this callback 

;; callback-map buffer structure
;; Hash map:
;;  key - requested item unique id. 
;;        Depends of callback type: 
;;            -- callback returns one item (nil? callback :query) - SM collection item key (only one key supported so far )
;;            -- callback returns array of items (some? callback :query) - hash value of calculated query string. 
;;               Calculated means all macros replaced for real values 
;;  val  - item-val - requested value 
;;       - requests-chan - channel for placing requests for this particular item 
;;                         Request object placed in channel should have type field to select correct requests proccessor 
;;

;;no-cache-buffer sturucture 
;; Used to keep link between message object and request, supplied to workers 
;; Hash map: 
;;  key - unique request id 
;;  val - TMessage object 




(def ^:private request-template {;:req_id rec_id
                                 :user_name "TBOT"
                                 :status "N"
                                 :tag "TBOT:CB"
                                 :execution_mode  "I"
                                 :attempt 1
                                 :execution_retries 1
                                 :retry_interval 100000 ; just place holder
                                 ;:req_id rec_id
                                 ;:schedule_name schedule_name - not valied
                                 ;:action (route-params :action_id)
                                 ;:parameters (if (string? parameters) parameters (json/generate-string parameters))
                                 ;:expire_at expire_at ;(if (nil? expire_at) "NULL" expire_at)
                                 ;:service (name service)
                                 ;:subject (route-params :name)
                                 })

(def cachable-request-tag  "TBOT:CB")

(def not-cachable-request-tag  "TBOT:NB")


(def ^:private fast-getter
  " get function to reqiest target system 
  "
  (delay (;; Оnly async-pusher supported.
          ;; (get-config  :async-pusher-enabled) value ignored 
          (pusher-manager/get-pusher-factory true)
          :global-mode
          true
          (@config/config :config)
          (@config/config :workers))))


#_{:clj-kondo/ignore [:unused-private-var]}
(defn- clear-cache-item [callback item-key]
  (some-> @(callback :buffer)
          item-key
          :requests-chan
          close!)
  (swap! (callback :buffer) dissoc item-key))


(def  inqueue-request (delay (@request-action :direct-insert)))

(defn- callback-fabric [callback]
  (assoc callback :schedule  (if (true? (callback :no-cache))
                               (fn [_ item]
                                 (let [rec_id (get-uid)]
                                   (@inqueue-request   (assoc request-template
                                                              :rec_id   rec_id
                                                              :service  (callback :service)
                                                              :action   (callback :action)
                                                              :tag      (str not-cachable-request-tag (callback :callback))
                                                              :parameters (tmessage/get-query item)
                                                              :subject  (tmessage/get-item-key item)))
                                   (swap! no-cache-buffer
                                          assoc rec_id  (tmessage/wait-a-minute item))))

                               (fn [item-key item]
                                 (timbre/debugf "Cache missed for item %s " item-key)
                                 (let [buffer-item (@(callback :buffer) item-key)]
                                   (if (nil? buffer-item)
                                     (let [local-chan (chan (dropping-buffer max-channel-size))]
                                       (timbre/debugf "Item  %s  not found in the buffer. Try register new target system request." item-key)
                                       (swap! (callback :buffer)
                                              assoc  item-key
                                              {:requests-chan local-chan
                                               :item-val (atom nil)})
                                       (>!! local-chan (tmessage/wait-a-minute item))
                                       (timbre/debug "Get request " (assoc request-template
                                                                           :rec_id   (get-uid)
                                                                           :service  (callback :service)
                                                                           :action   (callback :action)
                                                                           :tag     (str cachable-request-tag (callback :callback))
                                                                           :parameters (tmessage/get-query item)
                                                                           :subject  (tmessage/get-item-key item)))
                                       (@fast-getter  (assoc request-template
                                                             :rec_id   (get-uid)
                                                             :service  (callback :service)
                                                             :action   (callback :action)
                                                             :tag     (str cachable-request-tag (callback :callback))
                                                             :parameters (tmessage/get-query item)
                                                             :subject  (tmessage/get-item-key item))  "undefined"
                                                      (fn [resp] (rp/fast-update-resp-processor resp callback)))
                                       (timbre/debug "Target system requested for " item-key))
                                     (or (some-> buffer-item :item-val deref)
                                         (when-not (>!! (buffer-item :requests-chan) (tmessage/wait-a-minute item))
                                           (timbre/error "Can't add request " item "to channel in " buffer-item)))))))))


(defn check-access-list [item callback-object obj]
  (when (some? item)
    (->  callback-object
         :access-list
         (#(if (nil? %)
             true
             ((check-access-fabric (tmessage/get-role obj) % item) obj)))
         (if item (tmessage/error-response obj sm/RC_NO_MORE)))))


(defn get-item [obj]
  (if-let [callback-id (tmessage/get-callback obj)]
    (let [callback-object (@callback-map callback-id)]
      (timbre/debugf "Get item %s with callback %s" (tmessage/get-item-key obj) callback-id)
      (if (true? (callback-object :no-cache))
        ((:schedule callback-object) obj (keyword (tmessage/get-item-key obj)))
        (-> callback-object
            (#(or (some-> %1
                          :buffer
                          deref
                          %2
                          :item-val
                          deref)
                  (json/parse-string  (@get-L2-item callback-id %2) true)
                  ((:schedule %1) %2 obj))
             (keyword (tmessage/get-item-key obj)))
            (check-access-list callback-object obj))))
    (timbre/errorf "Callback not found for  object: " obj)))

(defn test-item-L1
  " 
   Get item from L1 cache. 
  "
  [obj]
  (some-> obj tmessage/get-callback  (#(@callback-map %)) :buffer deref (get (keyword (tmessage/get-item-key obj))) :item-val deref))

(defn test-item-L2
  " 
   Get item from L2 cache. 
  "
  [obj]
  (json/parse-string  (@get-L2-item (name (tmessage/get-callback obj)) (tmessage/get-item-key obj)) true))


#_(defn- register-callback [callback]
    (-> callback
        (#(assoc % :expire (or (% :expire) default-expire-time)))
        (assoc :buffer (atom (hash-map)))
        (assoc :ttl (/ default-expire-time ttl-numerator))
        callback-fabric))



(defn- remap-role-access-list [access-list]
  (println "Access-list " access-list)
  (println "Access-list map?" (map? access-list))
  (println "Access-list map?" (vector? access-list))
  (if (vector? access-list)
    (into []
          (map (fn [item]
                 (println "item -> " item)
                 (if (string? item)
                   (keyword item)
                   (into [] (map keyword item)))) access-list))
    [nil (keyword access-list)]))

(defn- remap-access-list [access-list]
  (reduce #(assoc %1 (key %2) (remap-role-access-list (val %2)))
          {}  access-list))

(defn- config-access-list [{acl :access-list :as callback}]
  (if (seq acl)
    (update callback :access-list remap-access-list)
    callback))

(defn- register-callback [callback]
  (-> callback
      (assoc
       :expire (or (callback :expire) default-expire-time)
       :buffer (atom (hash-map))
       :ttl (/ default-expire-time ttl-numerator))
      config-access-list
      callback-fabric))



(defn read-callbacks [_ path]
  (when-let [callback-list (yaml/from-file (str (or path default-conf-path) "callbacks.yml"))]
    (reduce #(assoc %1
                    (keyword (%2 :callback))
                    (register-callback %2))
            {} callback-list)))


