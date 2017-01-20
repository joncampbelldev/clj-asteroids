(ns cs-game.util.expanded-lang)

(defmacro defn-memo [name & body]
  `(def ~name (memoize (fn ~body))))
