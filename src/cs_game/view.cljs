(ns cs-game.view
  (:require [cs-game.util.canvas :as canvas]
            [cs-game.util.maths :as maths]
            [cs-game.spatial-hashing :as spatial-hashing]
            [cs-game.expanded-lang :refer [concatv]])
  (:require-macros [cs-game.util.canvas :as canvas]))

(def minimap-size 200)

(def types-in-z-order [:laser :player :asteroid :explosion])

(defmulti render-entity (fn [_ entity _] (:view entity)))

(defn outline [ctx]
  (canvas/line-width ctx 2.5)
  (canvas/stroke-style ctx "black")
  (canvas/stroke ctx))

(defmethod render-entity :laser [ctx laser _]
  (canvas/fast-state {:context ctx
                      :translate (:position laser)
                      :rotation (maths/degrees-to-radians (:rotation laser))}
    (canvas/draw-points ctx (:points laser))
    (canvas/fill-style ctx (:color laser))
    (canvas/fill ctx)
    (outline ctx)))

(defmethod render-entity :player [ctx player _]
  (canvas/fast-state {:context ctx
                      :translate (:position player)
                      :rotation (maths/degrees-to-radians (:rotation player))}
    (canvas/draw-points ctx (:points player))
    (canvas/fill-style ctx (:color player))
    (canvas/fill ctx)
    (outline ctx)))

(defmethod render-entity :asteroid [ctx asteroid _]
  (canvas/fast-state {:context ctx
                      :translate (:position asteroid)
                      :rotation (maths/degrees-to-radians (:rotation asteroid))}
    (canvas/draw-points ctx (:points asteroid))
    (canvas/fill-style ctx (:color asteroid))
    (canvas/fill ctx)
    (outline ctx)))

(defmethod render-entity :explosion [ctx explosion _]
  (canvas/fast-state {:context ctx
                      :translate (:position explosion)
                      :alpha (:alpha explosion)}
    (canvas/centered-circle ctx 0 0 (/ (:size explosion) 2))
    (canvas/fill-style ctx (:color explosion))
    (canvas/fill ctx)
    (when-not (:no-outline explosion)
      (outline ctx))))

(defn visible-in-map? [world-width world-height entity]
  (let [[x y] (:position entity)]
    (and (< x world-width)
         (> x 0)
         (> y 0)
         (< y world-height))))

(defn render-minimap [ctx drawable-entities world]
  (let [[world-width world-height] (:dimensions world)
        xscale (/ minimap-size world-width)
        yscale (/ minimap-size world-height)]
    (canvas/fill-style ctx "black")
    (canvas/fill-rect ctx 0 0 minimap-size minimap-size)
    (doseq [e (filter #(and (not= :explosion (:view %))
                            (visible-in-map? world-width world-height %))
                      drawable-entities)]
      (let [[x y] (:position e)
            scaled-x (* x xscale)
            scaled-y (* y yscale)
            size-scale (/ (+ xscale yscale) 2)
            size (case (:view e)
                   :laser 2
                   :player 6
                   (max 2 (/ (* size-scale (:size e)) 2)))]
        (canvas/fill-style ctx (:color e))
        (canvas/fill-centered-rect ctx scaled-x scaled-y size size)))

    (canvas/stroke-style ctx "#111")
    (canvas/line-width ctx 3)
    (canvas/stroke-rect ctx 0 0 minimap-size minimap-size)))

(def health-bar-height 16)
(def health-bar-width 80)
(defn render-health-bar [ctx player]
  (canvas/fill-style ctx "green")
  (canvas/fill-rect ctx 0 0 (* (/ health-bar-width 100) (:health player)) health-bar-height))

(def weapon-box-size 30)
(defn render-weapons [ctx player]
  (let [current-weapon-index (:current-weapon-index player)]
    (doseq [[index weapon] (map-indexed vector (:weapons player))]
      (canvas/fast-state {:context ctx :translate [(* index (+ 2 weapon-box-size)) 0]}
        (canvas/fill-style ctx (if (= current-weapon-index index) "red" "dimgrey"))
        (canvas/fill-rect ctx 0 0 weapon-box-size weapon-box-size)

        (canvas/fill-style ctx "white")
        (canvas/font ctx "14px Arial")
        (canvas/text-align ctx "center")
        (canvas/text-baseline ctx "middle")
        (canvas/fill-text ctx (:name weapon) (/ weapon-box-size 2) (/ weapon-box-size 2))))))

(def padding 5)
(def vertical-spacer [0 padding])
(def horizontal-spacer [padding 0])
(def hud-height (+ padding health-bar-height padding weapon-box-size padding))
(defn render-player-hud [ctx player camera]
  (let [camera-top-left (:screen-position camera)
        [_ camera-height] (:dimensions camera)
        starting-y (case (:hud-position camera)
                     :top 0
                     :bottom (- camera-height hud-height))]
    (canvas/fast-state {:context ctx
                        :translate (maths/v+ [0 starting-y]
                                             horizontal-spacer
                                             vertical-spacer
                                             camera-top-left)}
      (render-health-bar ctx player)
      (canvas/fast-state {:context ctx
                          :translate (maths/v+ vertical-spacer
                                               [0 health-bar-height])}
        (render-weapons ctx player)))))

(defn render-world [off-screen-el off-screen-ctx on-screen-ctx world bg-pattern]
  (let [entities (:entities world)

        drawable-entities (filterv :view entities)
        entity-spatial-hash (spatial-hashing/build drawable-entities (:spatial-hash-config world))

        [world-width world-height] (:dimensions world)
        [screen-width screen-height] (:screen-dimensions world)
        ctx off-screen-ctx]
    (doseq [camera (:cameras world)]

      (canvas/fast-state {:context ctx
                          :translate (maths/v+
                                       (maths/vneg (:position camera))
                                       (maths/vdiv (:dimensions camera) [2 2]))}

        (let [[x y] (maths/v- (:position camera) (maths/vdiv (:dimensions camera) [2 2]))
              [width height] (:dimensions camera)]
          (canvas/fill-style ctx bg-pattern)
          (canvas/fill-rect ctx x y width height))

        ;TODO tidy up render of world borders, only render when necessary
        (canvas/stroke-style ctx "black")
        (canvas/line-width ctx 30)
        (canvas/stroke-rect ctx 0 0 world-width world-height)

        (let [entities-for-camera (->> (spatial-hashing/nearby-entity-indexes entity-spatial-hash camera)
                                       (mapv #(nth entities %))
                                       (sort-by #(or (:z-index %) (:id %))))
              view->entities (group-by :view entities-for-camera)
              views-in-z-order (mapcat view->entities types-in-z-order)]
          (doseq [e views-in-z-order]
            (render-entity ctx e world))))

      (let [{[camera-width camera-height] :dimensions
             screen-position :screen-position} camera]
        (canvas/fast-state {:context on-screen-ctx
                            :translate screen-position}
          (canvas/draw-image on-screen-ctx off-screen-el
                             0 0 camera-width camera-height
                             0 0 camera-width camera-height)

          (canvas/stroke-style on-screen-ctx "#111")
          (canvas/line-width on-screen-ctx 4)
          (canvas/stroke-rect on-screen-ctx 0 0 camera-width camera-height))))

    (doseq [player (->> world
                        :entity-indexes-by-system
                        :tracked-by-camera-index
                        (map #(nth entities %)))]
      (let [camera (-> world
                       :cameras
                       (nth (:tracked-by-camera-index player)))]
        (render-player-hud on-screen-ctx player camera)))

    (canvas/fast-state {:context on-screen-ctx
                        :translate (maths/v- [(/ screen-width 2) (/ screen-height 2)]
                                             [(/ minimap-size 2) (/ minimap-size 2)])}
      (render-minimap on-screen-ctx drawable-entities world))))

(defn render-menu [ctx screen-width screen-height]
  (canvas/fill-style ctx "black")
  (canvas/fill-rect ctx 0 0 screen-width screen-height)

  (canvas/fill-style ctx "white")
  (canvas/font ctx "30px Arial")
  (canvas/text-align ctx "center")
  (canvas/fill-text ctx "Press 2, 3 or 4 to start a game for that many players" (/ screen-width 2) (/ screen-height 2)))

(defn render-pause-overlay [ctx screen-width screen-height]
  (canvas/fast-state {:context ctx
                      :alpha 0.7}
    (canvas/fill-style ctx "black")
    (canvas/fill-rect ctx 0 0 screen-width screen-height))

  (canvas/fill-style ctx "white")
  (canvas/font ctx "30px Arial")
  (canvas/text-align ctx "center")
  (canvas/fill-text ctx "Paused (q to return to menu)" (/ screen-width 2) (/ screen-height 2)))

(defn render-fps-overlay [ctx {:keys [fps-history max-fps-history]}]
  (let [graph-width 250
        step-width (/ graph-width max-fps-history)
        offset-for-index (- max-fps-history (count fps-history))

        graph-height 50

        graph-top 0
        graph-bottom (+ graph-top graph-height)
        graph-left 100
        graph-right (+ graph-left graph-width)

        max-fps 60
        fps->y (fn [fps] (- graph-bottom (* fps (/ graph-height max-fps))))
        index->x (fn [index] (+ graph-left (* index step-width)))]
    (canvas/fill-style ctx "white")
    (canvas/font ctx "12px Arial")
    (canvas/text-align ctx "start")
    (canvas/text-baseline ctx "middle")
    (canvas/fill-text ctx (str (.round js/Math (last fps-history)) " fps") (+ 5 graph-right) (+ graph-top (/ graph-height 2)))

    (canvas/line-width ctx 1)
    (canvas/begin-path ctx)
    (canvas/move-to ctx graph-left (fps->y (first fps-history)))
    (doseq [[index fps] (map-indexed vector fps-history)]
      (canvas/line-to ctx (index->x (+ offset-for-index index)) (fps->y fps)))
    (canvas/stroke-style ctx "white")
    (canvas/stroke ctx)))