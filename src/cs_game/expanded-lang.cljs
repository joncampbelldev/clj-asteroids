(ns cs-game.expanded-lang)

(defn group-by-transform [key-fn transform-fn coll]
  (reduce
    (fn [m e]
      (let [k (key-fn e)
            v (transform-fn e)]
        (assoc m k (conj (get m k []) v))))
    {}
    coll))

(defn concatv [& vs]
  (into [] (apply concat vs)))

(defn index-by [key-fn coll]
    (reduce
      (fn [m e] (assoc m (key-fn e) e))
      {}
      coll))

(defn println-value [v]
    (println v)
    v)

(declare .-innerWidth .-innerHeight)
(defn get-window-dimensions []
  [(.-innerWidth js/window) (.-innerHeight js/window)])

(defn get-time [] (.getTime (new js/Date)))