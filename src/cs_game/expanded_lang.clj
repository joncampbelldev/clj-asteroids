(ns cs-game.expanded-lang)

(defmacro defn-memo [name & body]
  `(def ~name (memoize (fn ~body))))
