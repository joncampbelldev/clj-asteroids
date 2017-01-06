(ns cs-game.core
  (:require [cs-game.ces :as ces]
            [cs-game.view :as view]
            [cs-game.keyboard :as keyboard]
            [cs-game.canvas :as canvas]
            [cs-game.maths :as maths]
            [cs-game.collisions :as collisions]
            [cs-game.expanded-lang :refer [get-window-dimensions get-time strict-empty? concatv]]
            [cs-game.spatial-hashing :as spatial-hashing]
            [cs-game.sat :as sat])
  (:require-macros [cs-game.expanded-lang :refer [defn-memo]]))

(enable-console-print!)

(defn delta-vector [world v]
  (let [delta (:delta world)]
    (maths/v* [delta delta] v)))

(defn accelerate-forwards [entity acceleration-magnitude]
  (let [rotation (maths/degrees-to-radians (:rotation entity))
        acceleration [(* acceleration-magnitude (maths/cos rotation))
                      (* acceleration-magnitude (maths/sin rotation))]
        new-velocity (maths/v+ (:velocity entity) acceleration)
        new-velocity-magnitude (maths/vmag new-velocity)
        max-velocity-magnitude 7
        scaled-velocity (if (> new-velocity-magnitude max-velocity-magnitude)
                          (let [scale (/ max-velocity-magnitude new-velocity-magnitude)]
                            (maths/v* new-velocity [scale scale]))
                          new-velocity)]
    (assoc entity :velocity scaled-velocity)))

(defn keyboard-move [entity world]
  (let [rotate-speed 4
        accel 0.4
        key-bindings (:key-bindings entity)]
    (as-> entity e
          (if (keyboard/held? (:up key-bindings)) (accelerate-forwards entity (* (:delta world) accel)) e)
          (if (keyboard/held? (:left key-bindings)) (update e :rotation - (* (:delta world) rotate-speed)) e)
          (if (keyboard/held? (:right key-bindings)) (update e :rotation + (* (:delta world) rotate-speed)) e))))

(defn-memo create-points-for-rectangle [x y width height]
  (let [half-width (/ width 2)
        half-height (/ height 2)]
    [[(- x half-width) (- y half-height)]
     [(+ x half-width) (- y half-height)]
     [(+ x half-width) (+ y half-height)]
     [(- x half-width) (+ y half-height)]]))

(defn create-laser-at-entity [entity offset-angle-in-degrees]
  (let [rotation (maths/degrees-to-radians (+ offset-angle-in-degrees (:rotation entity)))
        laser-speed 25
        [evx evy] (:velocity entity)
        velocity [(+ (* laser-speed (maths/cos rotation)) evx) (+ (* laser-speed (maths/sin rotation)) evy)]
        laser-size 15]
    {:position (:position entity)
     :velocity velocity
     :rotation (:rotation entity)
     :size laser-size
     :points (create-points-for-rectangle 0 0 laser-size 4)
     :color "red"
     :type :laser
     :remove-off-screen true
     :collision true}))

(defmulti fire-weapon-from-entity (fn [weapon _ _] (:type weapon)))

(defmethod fire-weapon-from-entity :single-laser [_ entity world]
  (let [laser (create-laser-at-entity entity 0)]
    [entity (ces/add-entity-before-render laser world)]))

(defmethod fire-weapon-from-entity :shotgun-laser [_ entity world]
  (let [lasers [(create-laser-at-entity entity 0)
                (create-laser-at-entity entity -2)
                (create-laser-at-entity entity 2)
                (create-laser-at-entity entity -4)
                (create-laser-at-entity entity 4)]]
    [entity (ces/add-entities-before-render lasers world)]))

(defn shoot [entity world]
  (let [weapons (:weapons entity)
        current-weapon (nth weapons (:current-weapon-index entity))]
    (fire-weapon-from-entity current-weapon entity world)))

(defn change-weapon [entity _]
  (update entity :current-weapon-index #(mod (inc %) (count (:weapons entity)))))

(defn keyboard-weapon-control [entity world]
  (let [key-bindings (:key-bindings entity)]
    (cond
      (keyboard/just-down? (:shoot key-bindings)) (shoot entity world)
      (keyboard/just-down? (:change-weapon key-bindings)) (change-weapon entity world)
      :else entity)))

(defn wrap [entity world]
  (let [[x y] (:position entity)
        half-size (/ (:size entity) 2)
        [world-width world-height] (:dimensions world)
        left (- 0 half-size)
        right (+ world-width half-size)
        top (- 0 half-size)
        bottom (+ world-height half-size)]
    (cond
      (< x left) (assoc entity :position [right y])
      (> x right) (assoc entity :position [left y])
      (< y top) (assoc entity :position [x bottom])
      (> y bottom) (assoc entity :position [x top])
      :else entity)))

(defn moving [entity world]
  (update entity :position maths/v+ (delta-vector world (:velocity entity))))

(defn rotating [entity world]
  (update entity :rotation + (* (:delta world) (:rotate-speed entity))))

(defn remove-off-screen [entity world]
  (let [[x y] (:position entity)
        size (:size entity)
        [world-width world-height] (:dimensions world)
        left (- 0 size)
        right (+ world-width size)
        top (- 0 size)
        bottom (+ world-height size)
        off-screen? (or (< x left) (> x right) (< y top) (> y bottom))]
    [entity (if off-screen? (ces/remove-entities-after-render [(:id entity)] world) world)]))

(defn bounce [n] (* -0.5 n))

(defn bounce-off-edge [entity world]
  (let [[x y] (:position entity)
        size (:size entity)
        half-size (/ size 2)
        [world-width world-height] (:dimensions world)
        left (+ 0 half-size)
        right (- world-width half-size)
        top (+ 0 half-size)
        bottom (- world-height half-size)]
    (as-> entity e
          (if (< x left) (-> e
                             (update-in [:velocity 0] bounce)
                             (assoc-in [:position 0] left)) e)
          (if (> x right) (-> e
                              (update-in [:velocity 0] bounce)
                              (assoc-in [:position 0] right)) e)
          (if (< y top) (-> e
                            (update-in [:velocity 1] bounce)
                            (assoc-in [:position 1] top)) e)
          (if (> y bottom) (-> e
                               (update-in [:velocity 1] bounce)
                               (assoc-in [:position 1] bottom)) e))))

(defn narrow-phase-detect [entity1 entity2]
  (let [polygon1 (sat/to-polygon (:position entity1)
                                 (:points entity1)
                                 (maths/degrees-to-radians (:rotation entity1)))
        polygon2 (sat/to-polygon (:position entity2)
                                 (:points entity2)
                                 (maths/degrees-to-radians (:rotation entity2)))]
    (sat/test-polygon-polygon polygon1 polygon2)))

(defmethod collisions/detect-between [:player :asteroid] [player asteroid world]
  (let [[dx dy] (maths/v- (:position player) (:position asteroid))
        dist-sq (+ (* dx dx) (* dy dy))
        min-dist (+ (/ (:size player) 2) (/ (:size asteroid) 2))
        min-dist-sq (* min-dist min-dist)
        colliding? (and (< dist-sq min-dist-sq) (narrow-phase-detect player asteroid))
        updated-player (if colliding?
                         (update player :health - 20)
                         player)]
    [updated-player asteroid (if colliding? (ces/remove-entity-after-render (:id asteroid) world) world)]))

(defmethod collisions/detect-between [:laser :asteroid] [laser asteroid world]
  (let [[dx dy] (maths/v- (:position laser) (:position asteroid))
        dist-sq (+ (* dx dx) (* dy dy))
        min-dist (+ (/ (:size laser) 2) (/ (:size asteroid) 2))
        min-dist-sq (* min-dist min-dist)
        colliding? (and (< dist-sq min-dist-sq) (narrow-phase-detect laser asteroid))]
    [laser asteroid (if colliding? (ces/remove-entities-after-render [(:id asteroid) (:id laser)] world) world)]))

(defn track-with-camera [entity world]
  (let [camera-index (:tracked-by-camera-index entity)
        updated-world (assoc-in world [:cameras camera-index :position] (:position entity))]
    [entity updated-world]))

(defmulti on-death (fn [entity _] (:type entity)))

(defmethod on-death :player [player world]
  [player (ces/remove-entity-before-render (:id player) world)])

(defn check-if-dead [entity world]
  (if (< (:health entity) 0)
    (on-death entity world)
    entity))

; NOTE systems that add entities based on positions of existing entities should be placed
; after moving / rotating to ensure correct positions
(def systems
  [{:filter-fn :key-bindings
    :system-fn keyboard-move}

   {:filter-fn :velocity
    :system-fn moving}

   {:filter-fn :rotate-speed
    :system-fn rotating}

   {:filter-fn :key-bindings
    :system-fn keyboard-weapon-control}

   {:filter-fn :health
    :system-fn check-if-dead}

   {:filter-fn :bounce-off-edge
    :system-fn bounce-off-edge}

   {:filter-fn :remove-off-screen
    :system-fn remove-off-screen}

   {:filter-fn :wrap
    :system-fn wrap}

   {:filter-fn :collision
    :multiple-entity-system? true
    :system-fn collisions/system}

   {:filter-fn :tracked-by-camera-index
    :system-fn track-with-camera}])

(def initial-screen-dimensions (get-window-dimensions))
(def initial-screen-width (nth initial-screen-dimensions 0))
(def initial-screen-height (nth initial-screen-dimensions 1))
(def initial-world-width 3000)
(def initial-world-height 3000)
(def initial-world-dimensions [initial-world-width initial-world-height])

(def stars (mapv (fn [index] {:id index
                              :position [(rand initial-world-width) (rand initial-world-height)]
                              :size (maths/rand-between 1 3)})
                 (range 400)))

(defn create-isoceles-triangle [circle-size]
  [[(* circle-size 0.4) 0]
   [(- (* circle-size 0.3)) (* circle-size 0.2)]
   [(- (* circle-size 0.3)) (- (* circle-size 0.2))]])

(defn create-player [{:keys [size health color position velocity rotation]
                      :or {size 35
                           health 100
                           color "grey"
                           position [0 0]
                           velocity [0 0]
                           rotation 0}
                      :as overrides}]
  (merge {:position position
          :velocity velocity
          :rotation rotation
          :size size
          :points (create-isoceles-triangle size)
          :weapons [{:type :single-laser} {:type :shotgun-laser}]
          :current-weapon-index 0
          :health health
          :color color
          :bounce-off-edge true
          :collision true
          :type :player}
         overrides))

(defn create-player1 []
  (create-player {:color "white"
                  :position [(* initial-world-width 0.34) (* initial-world-height 0.5)]
                  :tracked-by-camera-index 0
                  :key-bindings {:left 65
                                 :right 68
                                 :up 87
                                 :down 83
                                 :shoot 70
                                 :change-weapon 71}}))


(defn create-player2 []
  (create-player {:color "green"
                  :rotation 180
                  :position [(* initial-world-width 0.67) (* initial-world-height 0.5)]
                  :tracked-by-camera-index 1
                  :key-bindings {:left 37
                                 :right 39
                                 :up 38
                                 :down 40
                                 :shoot 75
                                 :change-weapon 76}}))

(defn create-convex-polygon [radius]
  (let [full-circle (* maths/pi 2)]
    (loop [path []
           angle 0]
      (let [angle (+ angle (maths/rand-between 0.05 1.5))
            position [(* radius (maths/cos angle))
                      (* radius (maths/sin angle))]]
        (if (< angle full-circle)
          (recur (conj path position) angle)
          path)))))

(defn create-random-asteroid []
  (let [size (maths/rand-between 80 600)
        points (create-convex-polygon (/ size 2))
        position [(rand initial-world-width) (rand initial-world-height)]]
    {:position position
     :velocity [(maths/rand-between -1 1) (maths/rand-between -1 1)]
     :size size
     :rotation 0
     :rotate-speed (maths/rand-between -1 1)
     :points points
     :color "saddlebrown"
     :type :asteroid
     :wrap true
     :collision true}))

(defn create-world []
  (let [player1 (create-player1)
        player2 (create-player2)
        entities (-> []
                     (conj player1 player2)
                     (concatv (mapv create-random-asteroid (range 50))))
        spatial-hash-config (spatial-hashing/generate-config initial-world-width initial-world-height 5 200)
        player1-camera {:position (:position player1)
                        :screen-position [0 0]
                        :dimensions [(/ initial-screen-width 2) initial-screen-height]}
        player2-camera {:position (:position player2)
                        :screen-position [(/ initial-screen-width 2) 0]
                        :dimensions [(/ initial-screen-width 2) initial-screen-height]}]
    (as-> ces/blank-world _
          (merge _ {:collision-groups #{[:player :asteroid] [:laser :asteroid]}
                    :spatial-hash-config spatial-hash-config
                    :dimensions initial-world-dimensions
                    :screen-dimensions initial-screen-dimensions
                    :cameras [player1-camera player2-camera]
                    :stars stars
                    :star-spatial-hash (spatial-hashing/build stars spatial-hash-config)
                    :delta 1
                    :delta-scale 1})
          ; TODO this add after render is only to avoid blank frame, perhaps add a function for initial entity population?
          (ces/add-entities-after-render entities _))))

(def off-screen-canvas-el (.getElementById js/document "off-screen-canvas"))
(canvas/set-size off-screen-canvas-el initial-screen-width initial-screen-height)
(def off-screen-ctx (canvas/context off-screen-canvas-el))

(def on-screen-canvas-el (.getElementById js/document "on-screen-canvas"))
(canvas/set-size on-screen-canvas-el initial-screen-width initial-screen-height)
(def on-screen-ctx (canvas/context on-screen-canvas-el))

(def bindings {:bullet-time 0
               :reverse-time 0
               :pause 80
               :back-to-menu 27
               :start-game 13})

; TODO drop frames if delta is really small
(defn playback-past [game]
  (let [past (:past game)
        ideal-frame-time (:ideal-frame-time game)
        [updated-world updated-past] (if (not (strict-empty? past))
                                       [(peek past) (pop past)]
                                       [(:world game) past])]
    (assoc game :world updated-world
                :past updated-past
                :suggested-wait-time (* (:delta updated-world) ideal-frame-time))))

(defn snapshot-world-to-past [game]
  (let [leeway 100
        max-frames-of-past (+ leeway (:max-frames-of-past game))
        snapshot-of-world (:world game)
        current-frames-of-past (count (:past game))]
    (as-> game g
          (update g :past conj snapshot-of-world)
          (if (> current-frames-of-past max-frames-of-past)
            (update g :past #(into [] (drop leeway %)))
            g))))

(defn update-world [game]
  (let [target-delta-scale (if (keyboard/held? (:bullet-time bindings)) 0.1 1)
        delta-scale (maths/move-towards-linear (-> game :world :delta-scale) target-delta-scale 0.05)]
    (as-> game g
          (assoc-in g [:world :delta] (* (-> game :world :delta) delta-scale))
          (assoc-in g [:world :delta-scale] delta-scale)
          (snapshot-world-to-past g)
          (assoc g :world (ces/run-systems (:world g) systems)))))

(defn start-game [game]
  (assoc game :world (create-world)
              :last-frame-start-time (get-time)
              :past []
              :state :in-game))

(defn update-loop [game]
  (let [current-frame-start-time (get-time)
        game (assoc game :current-frame-start-time current-frame-start-time)

        time-since-last-frame (- current-frame-start-time (:last-frame-start-time game))
        delta (/ time-since-last-frame (:ideal-frame-time game))
        game (assoc-in game [:world :delta] delta)

        game (case (:state game)
               :menu (let [game (cond
                                  (keyboard/just-down? (:start-game bindings)) (start-game game)
                                  :else game)]
                       (view/render-menu on-screen-ctx initial-screen-width initial-screen-height)
                       game)
               :in-game (let [game (cond
                                     (keyboard/held? (:reverse-time bindings)) (playback-past game)
                                     (keyboard/just-down? (:pause bindings)) (assoc game :state :pause)
                                     :else (update-world game))
                              world (:world game)]
                          (view/render-world off-screen-canvas-el off-screen-ctx on-screen-ctx world)
                          game)
               :pause (let [game (cond
                                   (keyboard/just-down? (:pause bindings)) (assoc game :state :in-game)
                                   (keyboard/just-down? (:back-to-menu bindings)) (assoc game :state :menu)
                                   :else game)]
                        (view/render-world off-screen-canvas-el off-screen-ctx on-screen-ctx (:world game))
                        (view/render-pause-overlay on-screen-ctx initial-screen-width initial-screen-height)
                        game))

        ideal-frame-time (:ideal-frame-time game)
        current-frame-duration (- (get-time) current-frame-start-time)

        true-wait-time (max 0 (- ideal-frame-time current-frame-duration))
        suggested-wait-time (:suggested-wait-time game)
        game (dissoc game :suggested-wait-time)

        game (assoc game :last-frame-start-time current-frame-start-time)]
    (keyboard/tick)
    (view/render-fps-overlay on-screen-ctx (/ 1000 time-since-last-frame))
    (js/setTimeout #(update-loop game) (or suggested-wait-time true-wait-time))))

#_(defn on-resize []
    (let [window-dimensions (get-window-dimensions)
          [window-width window-height] window-dimensions]
      (canvas/set-size off-screen-canvas-el window-dimensions)))

(defn start []
  ;(.addEventListener js/window "resize" on-resize)
  (keyboard/add-listeners)
  (let [ideal-fps 25
        game {:ideal-fps ideal-fps
              :ideal-frame-time (/ 1000 ideal-fps)
              :max-frames-of-past (* 60 ideal-fps)
              :state :menu}]
    (update-loop (start-game game))))

(defonce _ (start))
(comment _ on-js-reload)