(ns cs-game.util.expanded-lang)

(defn group-by-transform [key-fn transform-fn initial-collection coll]
  (reduce
    (fn [m e]
      (let [k (key-fn e)
            v (transform-fn e)]
        (assoc m k (conj (get m k initial-collection) v))))
    {}
    coll))

(defn strict-empty? [v]
  (= 0 (count v)))

(defn concatv [& vs]
  (into [] (apply concat vs)))

(defn index-by [key-fn coll]
  (reduce
    (fn [m e] (assoc m (key-fn e) e))
    {}
    coll))

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