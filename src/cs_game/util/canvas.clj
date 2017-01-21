(ns cs-game.util.canvas)

(defmacro fast-state [{:keys [ctx translate rotation alpha]
                       :or {translate [0 0] rotation 0 alpha -1}}
                      & body]
  `(let [[x# y#] ~translate
         do-alpha# (not= -1 ~alpha)
         do-rotate# (not= 0 ~rotation)
         do-translate# (or (not= 0 x#) (not= 0 y#))
         original-alpha# (if do-alpha# (.-globalAlpha ~ctx))]
     (if do-alpha#
       (set! (.-globalAlpha ~ctx) (* original-alpha# ~alpha)))
     (if do-translate#
       (.translate ~ctx x# y#))
     (if do-rotate#
       (.rotate ~ctx ~rotation))
     ~@body
     (if do-rotate#
       (.rotate ~ctx (- ~rotation)))
     (if do-translate#
       (.translate ~ctx (- x#) (- y#)))
     (if do-alpha#
       (set! (.-globalAlpha ~ctx) original-alpha#))))
