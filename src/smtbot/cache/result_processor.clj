(ns smtbot.cache.result-processor
  (:require
   [taoensso.timbre :as timbre]
   [clojure.core.async :as a :refer [<!! close!]]
   [smtbot.bot.globals :refer [no-cache-buffer callback-map]]
   [smtbot.utils.sm_resp_decode :refer [get-jbody]]
   [smtbot.dal.globals :refer [botcache-action]]
   [smtbot.cache.acl-checker :refer [check-access-fabric]]
   [smtbot.enum.sm :as sm]
   [clojure.walk :as w]
   [smtbot.http_errors :as http-errors]
   [taoensso.timbre.appenders.core :as appenders]
   [smtbot.bot.messages.tmessage :as tmessage]
   [smtbot.utils.macro :refer [expire-at-timestamp]]))




(def ^:private update-L2-item  (delay (@botcache-action :update-item)))


#_(defn check-access-fabric
  " 
   Access list checkers fabric. 
   Check if user data in request confirm access list requirments
   Paramters:
    - role - role 
    - access-list - callback access list 
    - item - requested data
   Return:
    fn[ request] - return true/false 
  "
  [role  access-list  item]
  (if-let [[path transform] (get access-list role)]
    (case transform
      :user-mail (fn [request] (= (tmessage/get-user-mail request) (str/lower-case (get-in item path))))
      :operator-id  (fn [request] (= (str/lower-case (tmessage/get-operator-id request)) (str/lower-case (get-in item path))))
      :always (fn [_] true)
      (do
        (timbre/error "Unknown transform function:" transform)
        (fn [_] false)))
     (timbre/warnf "Access list %s does not entry for role %s. Access denied." access-list role )))



(defn- message-delivery
  "
  Run response delivery procedure for all TMessage in channel.
  "
  [callback item-key jbody]
  (when-let [requests (-> callback :buffer deref item-key :requests-chan)]
    (close! requests)
    (let [jbody-k (w/keywordize-keys jbody)
          access-list (callback :access-list)]
      (timbre/debug "Access-list " access-list)
      (if (nil? access-list)
        (loop [request  (<!! requests)]
          (when (some? request)
            (tmessage/response request jbody-k)
            (recur (<!! requests))))
        (let [get-access-checker (memoize check-access-fabric)]
          (loop [request  (<!! requests)]
            (when (some? request)
              (if-let [role (tmessage/get-role request)]
                (if ((check-access-fabric role access-list jbody-k) request)
                  (tmessage/response request jbody-k)
                  (tmessage/error-response request sm/RC_NO_MORE))
                (timbre/error "User role undefined in request:" request))
              (recur (<!! requests)))))))))


(defn- process-error-cacheable [_ callback item-key log-message error-type]
  (timbre/error log-message)
  (when-let [requests (-> callback :buffer deref item-key :requests-chan)]
    (close! requests)
    (loop [request  (<!! requests)]
      (when (some? request)
        (tmessage/error-response request error-type)
        (recur (<!! requests))))))


(defn- process-ok-cacheable [_ item-key jbody body rec-count callback]
  (timbre/debug "Callback buffer" (callback :buffer) " item-key " item-key)
  (timbre/debug "Response body: " body "items count " rec-count)
  (-> callback :buffer deref item-key :item-val (reset! (w/keywordize-keys jbody)))
  (@update-L2-item (callback :callback) item-key body  (expire-at-timestamp (:expire callback)))
  (message-delivery callback item-key jbody)
  (Thread/sleep (:ttl callback)))


(defn- process-error-not-cacheable [{rec-id :rec-id} _ _ log-message error-type]
  (timbre/error log-message)
  (when (some? rec-id)
    (some-> rec-id ((deref no-cache-buffer)) (tmessage/error-response error-type))
    (swap! no-cache-buffer dissoc rec-id)))


(defn- process-ok-not-cacheable [{rec-id :rec-id} _ jbody body rec-count _]
  (timbre/debug "Response body: " body "items count " rec-count)
  (when-let  [request (get no-cache-buffer rec-id)]
    (let [jbody-k (w/keywordize-keys jbody)]
      (if-let [access-list (-> callback-map  (get (tmessage/get-callback request)) :access-list)]
        (if ((check-access-fabric (tmessage/get-role request) access-list jbody-k) request)
          (tmessage/response request (w/keywordize-keys jbody))
          (tmessage/error-response request sm/RC_NO_MORE))
        (tmessage/response request (w/keywordize-keys jbody)))))
  (swap! no-cache-buffer dissoc rec-id))


(defn- resp-processor-fabric [process-ok process-error]
  (fn [{:keys [status opts body headers error]} callback]
    (timbre/with-merged-config
      {:appenders {:min-level :debug
                   :println {:enabled? false}
                   :spit (appenders/spit-appender {:fname "log/cache.log"})}}
      (let [item-key (keyword (:subject opts))]
        (if (or (not= http-errors/OK status) (nil? opts) (nil? body))
          (process-error  opts callback item-key
                          (str "Unable to get update from the target system, exception: error " error " status " status "opts" opts "body" body) sm/RC_ERROR)
          (let [jbody (get-jbody body headers)
                rc (get jbody "ReturnCode")
                rec-count (get jbody "@count")]
            (if (= rc sm/RC_SUCCESS)
              (if (or (nil? rec-count) (> rec-count 0))
                (process-ok opts item-key jbody body rec-count callback)
                (process-error opts callback item-key (str "Not found, response is " body) sm/RC_NO_MORE))
              (process-error opts callback item-key (str "Request failed, response " body "request options ") rc))))
        #_(when (some? item-key) (process-error opts callback item-key))))))

(def resp-processor-cacheable
  "
   Callback, used to process cacheable response in async mode 
  "
  (let [resp-processor (resp-processor-fabric process-ok-cacheable process-error-cacheable)]
    (fn [resp callback-id]
      (resp-processor resp (@callback-map (keyword callback-id))))))

(def resp-processor-not-cacheable
  "
   Callback, used to process not-cacheable response in async mode 
  "
  (let [resp-processor (resp-processor-fabric process-ok-not-cacheable process-error-not-cacheable)]
    (fn [resp callback-id]
      (resp-processor resp (@callback-map (keyword callback-id))))))

(defn fast-update-resp-processor
  "
   Process sync mode response processor for cacheable requests.
  "
  ([resp callback]
   (let [resp-processor (resp-processor-fabric process-ok-cacheable process-error-cacheable)]
     (resp-processor resp callback))))