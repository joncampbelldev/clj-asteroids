(ns cs-game.view
  (:require [cs-game.canvas :as canvas]
            [cs-game.maths :as maths]
            [cs-game.spatial-hashing :as spatial-hashing]
            [cs-game.expanded-lang :refer [concatv]])
  (:require-macros [cs-game.canvas :as canvas]
                   [cs-game.expanded-lang :refer [doseq-indexed]]))

(defmulti draw (fn [_ entity _] (:type entity)))

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
(def minimap-size 200)
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
            size (view->minimap-size (:type e))]
        (canvas/fill-style ctx (:color e))
        (canvas/fill-centered-rect ctx scaled-x scaled-y size size)))
    (canvas/stroke-style ctx "white")
    (canvas/line-width ctx 4)
    (canvas/draw-rounded-rect ctx 0 0 minimap-size minimap-size 20)
    (canvas/stroke ctx)))

(def camera-border-width 2)

(def weapon-type->name
  {:single-laser "1L"
   :shotgun-laser "SGL"})

(defn render-world [off-screen-el off-screen-ctx on-screen-ctx world]
  (let [spatial-hash-config (:spatial-hash-config world)

        entities (:entities world)

        ; TODO find an easy way to index entities by properties not involved in a system to get all drawable entities
        drawable-entities (filterv #(not (nil? %)) entities)
        entity-spatial-hash (spatial-hashing/build drawable-entities spatial-hash-config)

        stars (:stars world)
        star-spatial-hash (:star-spatial-hash world)

        [world-width world-height] (:dimensions world)
        [screen-width] (:screen-dimensions world)
        ctx off-screen-ctx]
    (doseq [camera (:cameras world)]
      ; TODO only clear the section of canvas required for this camera render
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

      (let [{[camera-width camera-height] :dimensions
             screen-position :screen-position} camera]
        (canvas/fast-state {:context on-screen-ctx
                            :translate screen-position}
          (canvas/draw-image on-screen-ctx off-screen-el
                             0 0 camera-width camera-height
                             0 0 camera-width camera-height)
          (canvas/stroke-style on-screen-ctx "white")
          (canvas/line-width on-screen-ctx camera-border-width)
          (canvas/stroke-rect on-screen-ctx 0 0 camera-width camera-height))))

    (canvas/fast-state {:context on-screen-ctx
                        :translate [(- (/ screen-width 2) (/ minimap-size 2)) 0]}
      (draw-mini-map on-screen-ctx drawable-entities world))

    (doseq [entity (->> world
                        :entity-indexes-by-system
                        :tracked-by-camera-index
                        (map #(nth entities %)))]
      (let [camera (-> world
                       :cameras
                       (nth (:tracked-by-camera-index entity)))
            camera-top-left (:screen-position camera)
            camera-height (-> camera :dimensions (nth 1))
            weapon-box-size 50
            current-weapon-index (:current-weapon-index entity)]
        (doseq [[index weapon] (map-indexed vector (:weapons entity))]
          (canvas/fast-state {:context on-screen-ctx
                              :translate (maths/v+ camera-top-left
                                                   [(+ camera-border-width (* index (+ 1 weapon-box-size)))
                                                    (- camera-height (+ weapon-box-size camera-border-width))])}
            (canvas/fill-style on-screen-ctx (if (= current-weapon-index index) "red" "dimgrey"))
            (canvas/fill-rect on-screen-ctx 0 0 weapon-box-size weapon-box-size)

            (canvas/fill-style on-screen-ctx "white")
            (canvas/set-font on-screen-ctx "18px Arial")
            (canvas/set-text-align on-screen-ctx "center")
            (canvas/fill-text on-screen-ctx (weapon-type->name (:type weapon)) (/ weapon-box-size 2) (/ weapon-box-size 2))))))))

(defn render-menu [on-screen-ctx screen-width screen-height]
  (canvas/fill-style on-screen-ctx "black")
  (canvas/fill-rect on-screen-ctx 0 0 screen-width screen-height)

  (canvas/fill-style on-screen-ctx "white")
  (canvas/set-font on-screen-ctx "30px Arial")
  (canvas/set-text-align on-screen-ctx "center")
  (canvas/fill-text on-screen-ctx "Press Enter to Start" (/ screen-width 2) (/ screen-height 2)))

(defn render-pause-overlay [on-screen-ctx screen-width screen-height]
  (canvas/fill-style on-screen-ctx "rgba(0, 0, 0, 0.6)")
  (canvas/fill-rect on-screen-ctx 0 0 screen-width screen-height)

  (canvas/fill-style on-screen-ctx "white")
  (canvas/set-font on-screen-ctx "30px Arial")
  (canvas/set-text-align on-screen-ctx "center")
  (canvas/fill-text on-screen-ctx "Paused" (/ screen-width 2) (/ screen-height 2)))

(defn render-fps-overlay [on-screen-ctx fps]
  (canvas/fill-style on-screen-ctx "white")
  (canvas/set-font on-screen-ctx "12px Arial")
  (canvas/set-text-align on-screen-ctx "start")
  (canvas/fill-text on-screen-ctx (str (.round js/Math fps) " fps") 20 20))