(ns smtbot.utils.keywordizer)
;;
;; 
(defn mk-pattern
  [val keys-list]
  (reduce #(if (some? (%1 %2)) (update %1 %2 re-pattern) %1) val keys-list))

(defn reduce-mk-pattern-fabric [keys-list]
  (fn [obj item]
    (assoc obj (item 0) (mk-pattern (item 1) keys-list))))

(defn mk-pattern-fabric [keys-list]
  (fn [obj] (mk-pattern obj keys-list)))

(defn mk-pattern-vec [obj keys-list]
  (map (mk-pattern-fabric keys-list)  obj))

#_{:clj-kondo/ignore [:unused-private-var]}
(defn mk-pattern-map  [obj keys-list]
  (reduce (reduce-mk-pattern-fabric keys-list) {} obj))


;;
;; Keywordize values of keys
;;
;;

(defn keywordize
  " Convert simple map values to keywords. 
    Values must be string, integer or keyword.
    val - map 
    keys-list - vector of keys
   "
  [val keys-list]
  (reduce #(if (some? (%1 %2)) (update %1 %2 keyword) %1) val keys-list))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn keywordize-in
  " Convert structured map values to keywords.
    It's expected that map objects has a same structure 
    Values must be string, integer or keyword.
    val - map 
    keys-list - vector of vectors of keys
   " [val keys-list]
  (reduce #(if (some? (get-in %1 %2)) (update-in %1 %2 keyword) %1) val keys-list))


(defn- reduce-keywordizer-fabric [keys-list]
  (fn [obj item]
    (assoc obj (item 0) (keywordize (item 1) keys-list))))

(defn- map-keywordizer-fabric [keys-list]
  (fn [obj] (keywordize obj keys-list)))

(defn keywordize-map
  " Keywordize structures stored in map 
    obj - map of structured values 
    key-list - vector of structure keys
  "
  [obj keys-list]
  (reduce (reduce-keywordizer-fabric keys-list) {} obj))

(defn keywordize-vec
  " Keywordize structures stored in vector 
    obj - vector of structured values 
    key-list - vector of structure keys
  "
  [obj keys-list]
  (map (map-keywordizer-fabric keys-list)  obj))



(defn process-internal-vec
  "
   Keywordize and re-parrtern structures stored inside map-object in  vector-type attribute 
    obj - map-object   
    path - path to the attribute inside object, vector   
    key-list - vector of attributes to keywordize
    pattern-list - vector of attributes to re-partern
  "
  ([obj path keys-list]
   (process-internal-vec obj path keys-list nil))

  ([obj path keys-list pattern-list]
   (let [f (fn [item path]
             (if (nil? pattern-list)
               (update-in item path keywordize-vec  keys-list)
               (-> item
                   (update-in  path keywordize-vec  keys-list)
                   (update-in  path mk-pattern-vec  pattern-list))))]
     (reduce #(let [fpath (into (conj [] %2) path)]
                (if (vector? (get-in %1 fpath)) (f %1 fpath) %1))
             obj (keys obj)))))



(defn keywordize-vec2
  " 
    Keywordize structures stored in vector of vectors  
    obj - vector of vectors of structured values 
    key-list - vector of structure keys
  "
  [obj keys-list]
  (let [kv (map-keywordizer-fabric keys-list)]
    (map #(map kv %) obj)))


(defn keywordize-map-vec2
  " Keywordize structures stored in map of vector of vectors  
    obj -  map of vector of vectors of structured values 
    key-list - vector of structure keys
  "
  [obj keys-list]
  (let [kv (map-keywordizer-fabric keys-list)
        mp (fn [obj] (map #(map kv %) obj))]
    (reduce #(assoc %1 (%2 0) (mp (%2 1)))  {} obj)))