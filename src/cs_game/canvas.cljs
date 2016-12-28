(ns cs-game.canvas)

(defn context [dom]
  (.getContext dom "2d"))

(defn fill-style [ctx color]
  (set! (.-fillStyle ctx) color))

(defn stroke-style [ctx color]
  (set! (.-strokeStyle ctx) color))

(defn line-width [ctx width]
  (set! (.-lineWidth ctx) width))

(defn set-size [el width height]
  (set! (.-width el) width)
  (set! (.-height el) height))

(defn save [ctx] (.save ctx))
(defn restore [ctx] (.restore ctx))
(defn translate [ctx x y] (.translate ctx x y))
(defn rotate [ctx radians] (.rotate ctx radians))

(defn begin-path [ctx] (.beginPath ctx))

(defn move-to [ctx x y] (.moveTo ctx x y))
(defn line-to [ctx x y] (.lineTo ctx x y))

(defn arc [ctx [x y] radius start-angle end-angle counter-clockwise?]
  (.arc ctx x y radius start-angle end-angle counter-clockwise?))

(def pi 3.14159265359)
(defn centered-circle [ctx [x y] radius]
  (arc ctx [x y] radius 0 (* 2 pi) false))

(defn fill [ctx] (.fill ctx))
(defn stroke [ctx] (.stroke ctx))

(defn fill-rect [ctx x y width height]
  (.fillRect ctx x y width height))

(defn fill-centered-rect [ctx x y width height]
  (fill-rect ctx (- x (/ width 2)) (- y (/ height 2)) width height))

(defn stroke-rect [ctx x y width height]
  (.strokeRect ctx x y width height))

(defn stroke-centered-rect [ctx x y width height]
  (stroke-rect ctx (- x (/ width 2)) (- y (/ height 2)) width height))

(declare .fillText)
(defn fill-text [ctx text [x y]]
  (.fillText ctx text x y))
