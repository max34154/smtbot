(ns smtbot.utils.sm_resp_decode
  (:require
   [cheshire.core :as json]
   [clojure.string :as s]
   [taoensso.timbre :as timbre
    :refer [errorf]]))

(defn get-jbody 
  ([body headers ] (get-jbody body headers false))
  ([body headers kwd]
   (when-let [content-type (:content-type headers)]
     (when (s/includes? content-type "application/json")
       (try (json/parse-string body kwd)
            (catch Exception e
              (errorf "Error %s on parsing json %s "  (ex-message e) body)))))))

(defmacro get-RC [body headers]
  `(get (get-jbody ~body ~headers) "ReturnCode"))

