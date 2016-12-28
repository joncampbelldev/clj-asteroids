(ns cs-game.canvas)

(defmacro fast-state [{:keys [context translate rotation]} & body]
  `(let [[x# y#] ~translate]
     (.translate ~context x# y#)
     (if ~rotation
        (.rotate ~context ~rotation))
     ~@body
     (if ~rotation
        (.rotate ~context (- ~rotation)))
     (.translate ~context (- x#) (- y#))))