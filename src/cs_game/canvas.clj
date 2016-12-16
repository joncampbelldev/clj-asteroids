(ns cs-game.canvas)

(defmacro state [[context-name context] & body]
  `(let [~context-name ~context]
     (.save ~context-name)
     ~@body
     (.restore ~context-name)))