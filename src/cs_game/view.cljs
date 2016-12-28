(ns cs-game.view
  (:require [cs-game.canvas :as canvas]
            [cs-game.maths :as maths])
  (:require-macros [cs-game.canvas :as canvas]))

(defmulti draw (fn [_ entity _] (:view entity)))

(defmethod draw :laser [ctx laser _]
  (canvas/fast-state {:context ctx
                      :translate (:position laser)
                      :rotation (maths/degrees-to-radians (:rotation laser))}
    (canvas/fill-style ctx "red")
    (let [size (:size laser)]
      (canvas/fill-centered-rect ctx 0 0 size (/ size 4)))))

(defmethod draw :player [ctx player _]
  (canvas/fast-state {:context ctx
                      :translate (:position player)
                      :rotation (maths/degrees-to-radians (:rotation player))}
    (canvas/fill-style ctx "grey")
    (let [size (:size player)]
      (canvas/begin-path ctx)
      (canvas/move-to ctx (* size 0.7) 0)
      (canvas/line-to ctx (- (* size 0.3)) (* size 0.4))
      (canvas/line-to ctx (- (* size 0.3)) (- (* size 0.4)))
      (canvas/fill ctx))))

(defmethod draw :asteroid [ctx asteroid _]
  (canvas/fast-state {:context ctx
                      :translate (:position asteroid)
                      :rotation (maths/degrees-to-radians (:rotation asteroid))}
    (canvas/fill-style ctx "saddlebrown")
    (let [size (:size asteroid)]
      (canvas/fill-centered-rect ctx 0 0 size size))))

(defn get-entities-seen-by-camera [entities camera]
  (let [[cx cy] (:position camera)
        [cw ch] (:dimensions camera)
        c-half-width (/ cw 2)
        c-half-height (/ ch 2)
        c-left (- cx c-half-width)
        c-right (+ cx c-half-width)
        c-top (- cy c-half-height)
        c-bottom (+ cy c-half-height)]
    (filter
      (fn [entity]
        (let [[ex ey] (:position entity)
              e-half-size (/ (:size entity) 2)
              e-left (- ex e-half-size)
              e-right (+ ex e-half-size)
              e-top (- ey e-half-size)
              e-bottom (+ ey e-half-size)]
          (and (< e-left c-right)
               (> e-right c-left)
               (< e-top c-bottom)
               (> e-bottom c-top))))
      entities)))

(defn render [ctx world]
  (let [drawable-entities (filter :view (:entities world))
        [world-width world-height] (:dimensions world)]
    (doseq [camera (:cameras world)]
      (canvas/fast-state {:context ctx
                          :translate (maths/vec+
                                       (maths/vec-negate (:position camera))
                                       (maths/vec-div (:dimensions camera) [2 2])
                                       (:screen-position camera))
                          :rotation 0}
        ;TODO tidy up render of world borders, only render when necessary
        (canvas/fill-style ctx "black")
        (canvas/fill-rect ctx 0 0 world-width world-height)

        ;TODO only render necessary stars
        (canvas/fill-style ctx "white")
        (doseq [star-position (:star-positions world)]
          (canvas/begin-path ctx)
          (canvas/centered-circle ctx star-position 2)
          (canvas/fill ctx))

        (doseq [e (get-entities-seen-by-camera drawable-entities camera)]
          (draw ctx e world))))
    (canvas/fill-style ctx "white")
    (canvas/fill-text ctx (str (.round js/Math (:fps world)) " fps") [20 20])))
