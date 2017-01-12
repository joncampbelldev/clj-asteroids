(ns cs-game.util.easing)

(defn out-expo [time duration start target]
  (if (>= time duration)
    (+ start target)
    (+ start
       (* target
          (+ 1
             (- (Math/pow 2 (* -10 (/ time duration)))))))))

(defn out-quad [time duration start target]
  (let [tdivd (/ time duration)]
    (+ start
       (* (- target)
          tdivd
          (- tdivd 2)))))