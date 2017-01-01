(ns cs-game.canvas)

(defmacro fast-state [{:keys [context translate rotation]
                       :or {rotation 0 translate [0 0]}}
                      & body]
  `(let [[x# y#] ~translate]
     (if ~translate
       (.translate ~context x# y#))
     (if ~rotation
       (.rotate ~context ~rotation))
     ~@body
     (if ~rotation
       (.rotate ~context (- ~rotation)))
     (if ~translate
       (.translate ~context (- x#) (- y#)))))