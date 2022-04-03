(ns smtbot.utils.misc
    (:require [yaml.core :as yaml]
            [clojure.java.io :as io]))

(defn arity
  "Returns the maximum arity of:
    - anonymous functions like `#()` and `(fn [])`.
    - defined functions like `map` or `+`.
    - macros, by passing a var like `#'->`.

  Returns -1 if the function/macro is variadic."
  ^long [f]
  (let [func (if (var? f) @f f)
        methods (->> func class .getDeclaredMethods
                     (map #(vector (.getName %)
                                   (count (.getParameterTypes %)))))
        var-args? (some #(-> % first #{"getRequiredArity"})
                        methods)]
    (if var-args?
      -1
      (let [max-arity (->> methods
                           (filter (comp #{"invoke"} first))
                           (sort-by second)
                           last
                           second)]
        (if (and (var? f) (-> f meta :macro))
          (- max-arity 2) ;; substract implicit &form and &env arguments
          max-arity)))))

(defn clob->string [clob]
   (with-open [rdr (java.io.BufferedReader. (.getCharacterStream clob))]
                     (apply str (line-seq rdr))))

(defn write-yml-file
  ([^String file-name to-write]
   (with-open [w (io/writer  file-name)]
     (.write w  ^String (yaml/generate-string  to-write))))
  ([^String file-name to-write dumper-options]
   (with-open [w (io/writer  file-name)]
     (.write w  ^String (yaml/generate-string  to-write :dumper-options dumper-options)))))



