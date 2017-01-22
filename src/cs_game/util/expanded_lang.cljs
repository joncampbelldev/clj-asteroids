(ns cs-game.util.expanded-lang)

(defn group-by-transform [key-fn transform-fn initial-collection coll]
  (persistent! (reduce
                 (fn [m e]
                   (let [k (key-fn e)
                         v (transform-fn e)]
                     (assoc! m k (conj (get m k initial-collection) v))))
                 (transient {})
                 coll)))

(defn strict-empty? [v]
  (= 0 (count v)))

(defn concatv
  ([v1 v2]
   (into v1 v2))
  ([v1 v2 & vs]
   (into v1 (apply concat v2 vs))))

(defn index-by [key-fn coll]
  (persistent! (reduce
                 (fn [m e] (assoc! m (key-fn e) e))
                 (transient {})
                 coll)))

(defn map-values
  [f m]
  (reduce-kv #(assoc %1 %2 (f %3)) {} m))

(defn trace [v]
  (println v)
  v)

(declare .-innerWidth .-innerHeight)
(defn get-window-dimensions []
  [(.-innerWidth js/window) (.-innerHeight js/window)])

(defn get-time [] (.getTime (new js/Date)))