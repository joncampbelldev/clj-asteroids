(ns cs-game.maths)

(def pi 3.14159265359)
(def radians-per-degree (/ pi 180))
(def degrees-per-radian (/ pi 180))

(defn degrees-to-radians [degrees]
  (* degrees radians-per-degree))

(defn radians-to-degrees [radians]
  (* radians degrees-per-radian))

(defn vec+ [& vs]
  (apply mapv + vs))

(defn vector-divide [& vs]
  (apply mapv / vs))

(defn vec- [& vs]
  (apply mapv - vs))

(defn vec* [& vs]
  (apply mapv * vs))

(defn clamp [min max x]
  (cond
    (< x min) min
    (> x max) max
    :else x))