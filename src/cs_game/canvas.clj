(ns cs-game.canvas)

(defmacro state [ctx & body]
  (list 'do
        (list '.save ctx)
        (cons 'do body)
        (list '.restore ctx)))

(def path-types {:fill '.fill :stroke '.stroke})

(defmacro path [ctx type & body]
  (list 'do
        (list '.beginPath ctx)
        (cons 'do body)
        (list (path-types type) ctx)))