(ns cs-game.maths)

(def pi 3.14159265359)
(def radians-per-degree (/ pi 180))
(def degrees-per-radian (/ pi 180))

(defn degrees-to-radians [degrees]
  (* degrees radians-per-degree))

(defn radians-to-degrees [radians]
  (* radians degrees-per-radian))

(defn vec+
  ([[v1x v1y] [v2x v2y]]
   [(+ v1x v2x) (+ v1y v2y)])
  ([[v1x v1y] [v2x v2y] [v3x v3y]]
   [(+ v1x v2x v3x) (+ v1y v2y v3y)]))

(defn vec- [[v1x v1y] [v2x v2y]]
  [(- v1x v2x) (- v1y v2y)])

(defn vec-div [[v1x v1y] [v2x v2y]]
  [(/ v1x v2x) (/ v1y v2y)])

(defn vec-negate [[vx vy]]
  [(- vx) (- vy)])

(defn vec* [[v1x v1y] [v2x v2y]]
  [(* v1x v2x) (* v1y v2y)])

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

(defn move-vec-towards-asymptote [[sx sy] [tx ty] scale]
  [(move-towards-asymptote sx tx scale)
   (move-towards-asymptote sy ty scale)])

(defn move-towards-linear [start target amount]
  (let [op (if (< start target) + -)
        comp (if (< start target) > <)
        new (op start amount)]
    (if (comp new target)
      target
      new)))