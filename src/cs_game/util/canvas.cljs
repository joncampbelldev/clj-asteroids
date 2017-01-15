(ns cs-game.util.canvas)

(defn context [dom]
  (.getContext dom "2d"))

;TODO macros for getter/setter of canvas props
(defn global-alpha
  ([ctx]
    (.-globalAlpha ctx))
  ([ctx alpha]
   (set! (.-globalAlpha ctx) alpha)))

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
(defn close-path [ctx] (.closePath ctx))

(defn move-to [ctx x y] (.moveTo ctx x y))
(defn line-to [ctx x y] (.lineTo ctx x y))
(defn arc-to [ctx sx sy ex ey radius] (.arcTo ctx sx sy ex ey radius))

(defn draw-points [ctx points]
  (let [[[fx fy] rest-of-points] [(first points) (rest points)]]
    (begin-path ctx)
    (move-to ctx fx fy)
    (doseq [[x y] rest-of-points]
      (line-to ctx x y))
    (close-path ctx)))

(defn arc [ctx x y radius start-angle end-angle counter-clockwise?]
  (.arc ctx x y radius start-angle end-angle counter-clockwise?))

(def pi 3.14159265359)
(defn centered-circle [ctx x y radius]
  (begin-path ctx)
  (arc ctx x y radius 0 (* 2 pi) false))

(defn fill [ctx] (.fill ctx))
(defn stroke [ctx] (.stroke ctx))

(defn fill-rect [ctx x y width height]
  (.fillRect ctx x y width height))

(defn fill-centered-rect [ctx x y width height]
  (fill-rect ctx (- x (/ width 2)) (- y (/ height 2)) width height))

(defn draw-rounded-rect [ctx x y width height radius]
  (begin-path ctx)
  (move-to ctx x (+ y radius))
  (line-to ctx x (- (+ y height) radius))
  (arc-to ctx x (+ y height) (+ x radius) (+ y height) radius)
  (line-to ctx (- (+ x width) radius) (+ y height))
  (arc-to ctx (+ x width) (+ y height) (+ x width) (- (+ y height) radius) radius)
  (line-to ctx (+ x width) (+ y radius))
  (arc-to ctx (+ x width) y (- (+ x width) radius) y radius)
  (line-to ctx (+ x radius) y)
  (arc-to ctx x y x (+ y radius) radius))

(defn stroke-rect [ctx x y width height]
  (.strokeRect ctx x y width height))

(defn stroke-centered-rect [ctx x y width height]
  (stroke-rect ctx (- x (/ width 2)) (- y (/ height 2)) width height))

(defn draw-image [ctx el sx sy swidth sheight dx dy dwidth dheight]
  (.drawImage ctx el sx sy swidth sheight dx dy dwidth dheight))


(defn text-align [ctx text-align]
  (set! (.-textAlign ctx) text-align))

(defn text-baseline [ctx text-baseline]
  (set! (.-textBaseline ctx) text-baseline))

(defn font [ctx font]
  (set! (.-font ctx) font))

(declare .fillText)
(defn fill-text [ctx text x y]
  (.fillText ctx text x y))
