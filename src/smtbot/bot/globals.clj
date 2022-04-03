(ns smtbot.bot.globals
  (:require [smtbot.config :refer [get-config]]))

(defonce bot (agent {}))
(defonce mapper (agent {}))
(defonce messages (agent {}))
(defonce callback-map (agent {}))
(defonce no-cache-buffer (atom {}))

(def default-lang :ru)

(def bot-token (delay (get-config :smtbot-token)))

(defmacro bild-id [item-key]
  `(str  ~item-key ":" (System/currentTimeMillis)))

