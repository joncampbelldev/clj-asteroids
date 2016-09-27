(ns cs-game.canvas)

(defmacro state [ctx & body]
  (list 'do
        (list '.save ctx)
        (cons 'do body)
        (list '.restore ctx)))
