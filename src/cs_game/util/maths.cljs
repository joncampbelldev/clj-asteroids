(ns cs-game.util.maths
  (:require-macros [cs-game.util.maths :refer [def-vector-op]]))

(def pi 3.14159265359)
(def radians-per-degree (/ pi 180))
(def degrees-per-radian (/ 180 pi))

(defn degrees-to-radians [degrees]
  (* degrees radians-per-degree))

(defn radians-to-degrees [radians]
  (* radians degrees-per-radian))

(defn cos [x] (.cos js/Math x))
(defn sin [x] (.sin js/Math x))
(defn tan [x] (.tan js/Math x))
(defn atan2 [y x] (.atan2 js/Math y x))

(defn safe+ [& xs] (apply + (mapv #(if % % 0) xs)))

(def-vector-op v+ +)
(def-vector-op v- -)
(def-vector-op vdiv /)
(def-vector-op v* *)

(defn vneg [& vs]
  (apply v* [-1 -1] vs))

(defn vmag [[x y]]
  (.sqrt js/Math (+ (* x x) (* y y))))

(defn clamp [min max x]
  (cond
    (< x min) min
    (> x max) max
    :else x))

(defn abs [x] (.abs js/Math x))

(defn rand-between-positives [low high]
  (+ (rand (- high low)) low))

(defn rand-between [low high]
  (cond
    (and (neg? low)
         (neg? high)) (- (rand-between-positives (abs high) (abs low)))
    (and (pos? low)
         (pos? high)) (rand-between-positives low high)
    :else (+ (rand (+ (abs low)
                      (abs high)))
             low)))

(defn move-towards-asymptote [start target scale]
  (let [diff (- target start)]
    (+ start (* diff scale))))

(defn move-towards-linear [start target amount]
  (let [op (if (< start target) + -)
        comp (if (< start target) > <)
        new (op start amount)]
    (if (comp new target)
      target
      new)))
