(ns cs-game.view
  (:require [cs-game.canvas :as canvas]
            [cs-game.maths :as maths]
            [cs-game.spatial-hashing :as spatial-hashing]
            [cs-game.expanded-lang :refer [concatv]])
  (:require-macros [cs-game.canvas :as canvas]))

(defmulti draw (fn [_ entity _] (:view entity)))

(defmethod draw :laser [ctx laser _]
  (canvas/fast-state {:context ctx
                      :translate (:position laser)
                      :rotation (maths/degrees-to-radians (:rotation laser))}
    (canvas/fill-style ctx (:color laser))
    (canvas/draw-points ctx (:points laser))
    (canvas/fill ctx)))

(defmethod draw :player [ctx player _]
  (canvas/fast-state {:context ctx
                      :translate (:position player)
                      :rotation (maths/degrees-to-radians (:rotation player))}
    (canvas/fill-style ctx (:color player))
    (canvas/draw-points ctx (:points player))
    (canvas/fill ctx)))

(defmethod draw :asteroid [ctx asteroid _]
  (canvas/fast-state {:context ctx
                      :translate (:position asteroid)
                      :rotation (maths/degrees-to-radians (:rotation asteroid))}
    (canvas/fill-style ctx (:color asteroid))
    (canvas/draw-points ctx (:points asteroid))
    (canvas/fill ctx)))

(def view->minimap-size {:player 5
                         :laser 2
                         :asteroid 3})
(def minimap-size 250)
(defn draw-mini-map [ctx drawable-entities world]
  (let [[world-width world-height] (:dimensions world)
        xscale (/ minimap-size world-width)
        yscale (/ minimap-size world-height)]
    (canvas/fill-style ctx "#222")
    (canvas/draw-rounded-rect ctx 0 0 minimap-size minimap-size 20)
    (canvas/fill ctx)
    #_(doseq [e drawable-entities]
        (let [[x y] (:position e)
              scaled-x (* x xscale)
              scaled-y (* y yscale)
              size (:size e)
              scaled-size (.ceil js/Math (* size (/ (+ xscale yscale) 2)))]
          (canvas/fast-state {:context ctx
                              :rotation (maths/degrees-to-radians (:rotation e))
                              :translate [scaled-x scaled-y]}
            (canvas/fill-style ctx (:minimap-color e))
            (canvas/fill-centered-rect ctx 0 0 scaled-size scaled-size))))
    (doseq [e drawable-entities]
      (let [[x y] (:position e)
            scaled-x (* x xscale)
            scaled-y (* y yscale)
            size (view->minimap-size (:view e))]
        (canvas/fill-style ctx (:color e))
        (canvas/fill-centered-rect ctx scaled-x scaled-y size size)))
    (canvas/stroke-style ctx "white")
    (canvas/line-width ctx 4)
    (canvas/draw-rounded-rect ctx 0 0 minimap-size minimap-size 20)
    (canvas/stroke ctx)))

(defn render [off-screen-el off-screen-ctx on-screen-ctx world]
  (let [spatial-hash-config (:spatial-hash-config world)

        entities (:entities world)

        ; TODO find an easy way to index entities by properties not involved in a system to get all drawable entities
        drawable-entities (filterv :view entities)
        entity-spatial-hash (spatial-hashing/build drawable-entities spatial-hash-config)

        stars (:stars world)
        star-spatial-hash (:star-spatial-hash world)

        [world-width world-height] (:dimensions world)
        [screen-width] (:screen-dimensions world)
        ctx off-screen-ctx]
    (doseq [camera (:cameras world)]
      (canvas/fill-style ctx "black")
      (canvas/fill-rect ctx 0 0 world-width world-height)

      (canvas/fast-state {:context ctx
                          :translate (maths/v+
                                       (maths/vneg (:position camera))
                                       (maths/vdiv (:dimensions camera) [2 2]))}

        ;TODO tidy up render of world borders, only render when necessary
        (canvas/stroke-style ctx "red")
        (canvas/line-width ctx 4)
        (canvas/stroke-rect ctx 0 0 world-width world-height)

        (let [stars-for-camera (->> (spatial-hashing/nearby-entity-indexes star-spatial-hash camera)
                                    (mapv #(nth stars %)))]
          (canvas/fill-style ctx "white")
          (doseq [{[x y] :position size :size} stars-for-camera]
            (canvas/centered-circle ctx x y size)
            (canvas/fill ctx)))

        (let [entities-for-camera (->> (spatial-hashing/nearby-entity-indexes entity-spatial-hash camera)
                                       (mapv #(nth entities %)))]
          (doseq [e entities-for-camera]
            (draw ctx e world))))

      (let [[c-width c-height] (:dimensions camera)]
        (canvas/fast-state {:context on-screen-ctx
                            :translate (:screen-position camera)}
          (.drawImage on-screen-ctx off-screen-el
                      0 0 c-width c-height
                      0 0 c-width c-height)
          (canvas/stroke-style on-screen-ctx "white")
          (canvas/line-width on-screen-ctx 4)
          (canvas/stroke-rect on-screen-ctx 0 0 c-width c-height))))
    (canvas/fast-state {:context on-screen-ctx
                        :translate [(- (/ screen-width 2) (/ minimap-size 2)) 0]}
      (draw-mini-map on-screen-ctx drawable-entities world))
    (canvas/fill-style on-screen-ctx "white")
    (canvas/fill-text on-screen-ctx (str (.round js/Math (:fps world)) " fps") [20 20])))
