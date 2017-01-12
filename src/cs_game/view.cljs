(ns cs-game.view
  (:require [cs-game.util.canvas :as canvas]
            [cs-game.util.maths :as maths]
            [cs-game.spatial-hashing :as spatial-hashing]
            [cs-game.expanded-lang :refer [concatv]])
  (:require-macros [cs-game.util.canvas :as canvas]))


(def view->minimap-size {:player 5
                         :laser 2
                         :asteroid 3})
(def minimap-size 200)

(def camera-border-width 2)

(def weapon-type->name
  {:single-laser "1"
   :shotgun-laser "SG"})

(def types-in-z-order [:laser :player :asteroid :explosion])

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

(defmethod draw :explosion [ctx explosion _]
  (canvas/fast-state {:context ctx
                      :translate (:position explosion)
                      :alpha (:alpha explosion)}
    (canvas/fill-style ctx (:color explosion))
    (canvas/centered-circle ctx 0 0 (/ (:size explosion) 2))
    (canvas/fill ctx)))

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

(defn render-player-hud [ctx player camera]
  (let [camera-top-left (:screen-position camera)
        camera-height (-> camera :dimensions (nth 1))
        weapon-box-size 50

        health-bar-padding 1
        health-bar-border 1
        health-bar-height 16
        health-bar-width 80
        current-weapon-index (:current-weapon-index player)]
    (canvas/fast-state {:context ctx
                        :translate camera-top-left}
      (canvas/fill-style ctx "green")
      (canvas/fill-rect ctx health-bar-padding health-bar-padding (* (/ health-bar-width 100) (:health player)) health-bar-height)
      (canvas/stroke-style ctx "green")
      (canvas/line-width ctx health-bar-border)
      (canvas/stroke-rect ctx 0 0 (+ (* 2 health-bar-padding) health-bar-width) (+ (* 2 health-bar-padding) health-bar-height))

      (doseq [[index weapon] (map-indexed vector (:weapons player))]
        (canvas/fast-state {:context ctx
                            :translate (maths/v+ [(* index (+ 1 weapon-box-size))
                                                  (- camera-height weapon-box-size)])}
          (canvas/fill-style ctx (if (= current-weapon-index index) "red" "dimgrey"))
          (canvas/fill-rect ctx 0 0 weapon-box-size weapon-box-size)

          (canvas/fill-style ctx "white")
          (canvas/font ctx "18px Arial")
          (canvas/text-align ctx "center")
          (canvas/text-baseline ctx "middle")
          (canvas/fill-text ctx (weapon-type->name (:type weapon)) (/ weapon-box-size 2) (/ weapon-box-size 2)))))))

(defn render-world [off-screen-el off-screen-ctx on-screen-ctx world]
  (let [spatial-hash-config (:spatial-hash-config world)

        entities (:entities world)

        ; TODO find an easy way to index entities by properties not involved in a system to get all drawable entities
        ; TODO currently assumed that all entities are drawable
        drawable-entities (filterv #(not (nil? %)) entities)
        entity-spatial-hash (spatial-hashing/build drawable-entities spatial-hash-config)

        stars (:stars world)
        star-spatial-hash (:star-spatial-hash world)

        [world-width world-height] (:dimensions world)
        [screen-width screen-height] (:screen-dimensions world)
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
                                       (mapv #(nth entities %)))
              type->entities (group-by :type entities-for-camera)
              entities-in-z-order (mapcat type->entities types-in-z-order)]
          (doseq [e entities-in-z-order]
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
                        :translate (maths/v- [(/ screen-width 2) (/ screen-height 2)]
                                             [(/ minimap-size 2) (/ minimap-size 2)])}
      (draw-mini-map on-screen-ctx drawable-entities world))

    (doseq [player (->> world
                        :entity-indexes-by-system
                        :tracked-by-camera-index
                        (map #(nth entities %)))]
      (let [camera (-> world
                       :cameras
                       (nth (:tracked-by-camera-index player)))]
        (render-player-hud on-screen-ctx player camera)))))

(defn render-menu [ctx screen-width screen-height]
  (canvas/fill-style ctx "black")
  (canvas/fill-rect ctx 0 0 screen-width screen-height)

  (canvas/fill-style ctx "white")
  (canvas/font ctx "30px Arial")
  (canvas/text-align ctx "center")
  (canvas/fill-text ctx "Press 2, 3 or 4 to start a game for that many players" (/ screen-width 2) (/ screen-height 2)))

(defn render-pause-overlay [ctx screen-width screen-height]
  (canvas/fill-style ctx "black")
  (canvas/fast-state {:context ctx
                      :alpha 0.7}
    (canvas/fill-rect ctx 0 0 screen-width screen-height))

  (canvas/fill-style ctx "white")
  (canvas/font ctx "30px Arial")
  (canvas/text-align ctx "center")
  (canvas/fill-text ctx "Paused" (/ screen-width 2) (/ screen-height 2)))

(defn render-fps-overlay [ctx {:keys [fps-history max-fps-history]}]
  (let [graph-width 250
        step-width (/ graph-width max-fps-history)
        offset-for-index (- max-fps-history (count fps-history))

        graph-height 50

        graph-top 20
        graph-bottom (+ graph-top graph-height)
        graph-left 20
        graph-right (+ graph-left graph-width)

        max-fps 60
        fps->y (fn [fps] (- graph-bottom (* fps (/ graph-height max-fps))))
        index->x (fn [index] (+ graph-left (* index step-width)))]
    (canvas/fill-style ctx "white")
    (canvas/font ctx "12px Arial")
    (canvas/text-align ctx "start")
    (canvas/text-baseline ctx "middle")
    (canvas/fill-text ctx (str (.round js/Math (last fps-history)) " fps") (+ 5 graph-right) (+ graph-top (/ graph-height 2)))

    (canvas/stroke-style ctx "white")
    (canvas/line-width ctx 1)
    (canvas/begin-path ctx)
    (canvas/move-to ctx graph-left (fps->y (first fps-history)))
    (doseq [[index fps] (map-indexed vector fps-history)]
      (canvas/line-to ctx (index->x (+ offset-for-index index)) (fps->y fps)))
    (canvas/stroke ctx)))