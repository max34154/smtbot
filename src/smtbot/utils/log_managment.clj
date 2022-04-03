(ns smtbot.utils.log_managment
  {:clj-kondo/config  '{:linters {:unused-public-var {:level :off}}
                        :clojure-lsp/unused-public-var {:level :off}}}
  (:require [smtbot.utils.macro :refer [tod unixtime->timestamp]]
             [clojure.java.io :as io]))

(defn clear-log [file-name]
(with-open [w (io/writer  (str "log/" file-name))]
  (.write w (str "Log restarted at " (unixtime->timestamp (tod))))))