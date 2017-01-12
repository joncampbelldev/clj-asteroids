(ns cs-game.core
  (:require [cs-game.ces :as ces]
            [cs-game.view :as view]
            [cs-game.util.keyboard :as keyboard]
            [cs-game.util.shapes :as shapes]
            [cs-game.util.canvas :as canvas]
            [cs-game.util.maths :as maths]
            [cs-game.util.easing :as easing]
            [cs-game.collisions :as collisions]
            [cs-game.expanded-lang :refer [get-window-dimensions get-time strict-empty? concatv]]
            [cs-game.spatial-hashing :as spatial-hashing]
            [cs-game.util.sat :as sat])
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
        max-velocity-magnitude 8
        scaled-velocity (if (> new-velocity-magnitude max-velocity-magnitude)
                          (let [scale (/ max-velocity-magnitude new-velocity-magnitude)]
                            (maths/v* new-velocity [scale scale]))
                          new-velocity)]
    (assoc entity :velocity scaled-velocity)))

(defn keyboard-move [entity world]
  (let [rotate-speed 8
        accel 0.5
        key-bindings (:key-bindings entity)]
    (as-> entity e
          (if (keyboard/held? (:up key-bindings)) (accelerate-forwards entity (* (:delta world) accel)) e)
          (if (keyboard/held? (:left key-bindings)) (update e :rotation - (* (:delta world) rotate-speed)) e)
          (if (keyboard/held? (:right key-bindings)) (update e :rotation + (* (:delta world) rotate-speed)) e))))

(defn create-laser-at-entity [entity offset-angle-in-degrees]
  (let [rotation (maths/degrees-to-radians (+ offset-angle-in-degrees (:rotation entity)))
        laser-speed 750
        standard-laser-velocity [(* laser-speed (maths/cos rotation)) (* laser-speed (maths/sin rotation))]
        velocity (maths/v+ standard-laser-velocity
                           (:velocity entity))
        laser-size 30]
    {:position (maths/v+ (:position entity) standard-laser-velocity)
     :velocity velocity
     :rotation (:rotation entity)
     :size laser-size
     :points (shapes/rectangle 0 0 laser-size 6)
     :color "red"
     :type :laser
     :remove-off-screen true
     :fired-by (:id entity)
     :collision true}))

(defmulti fire-weapon-from-entity (fn [weapon _ _] (:type weapon)))

(defmethod fire-weapon-from-entity :single-laser [_ entity world]
  (let [laser (create-laser-at-entity entity 0)]
    [entity (ces/add-entity-before-render laser world)]))

(defmethod fire-weapon-from-entity :shotgun-laser [_ entity world]
  (let [lasers [(create-laser-at-entity entity 0)
                (create-laser-at-entity entity -3)
                (create-laser-at-entity entity 3)
                (create-laser-at-entity entity -6)
                (create-laser-at-entity entity 6)]]
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

(defn moving [entity world]
  (update entity :position maths/v+ (delta-vector world (:velocity entity))))

(defn rotating [entity world]
  (update entity :rotation + (* (:delta world) (:rotate-speed entity))))

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

(defn mid-phase-colliding? [entity1 entity2]
  (let [[dx dy] (maths/v- (:position entity1) (:position entity2))
        dist-sq (+ (* dx dx) (* dy dy))
        min-dist (+ (/ (:size entity1) 2) (/ (:size entity2) 2))
        min-dist-sq (* min-dist min-dist)]
    (< dist-sq min-dist-sq)))

(defmethod collisions/detect-between [:player :asteroid] [player asteroid world]
  (let [narrow-phase-response (narrow-phase-detect player asteroid)
        colliding? (and (mid-phase-colliding? player asteroid) narrow-phase-response)
        updated-player (if colliding?
                         (-> player
                             (update :position maths/v- (:overlap narrow-phase-response))
                             (update :velocity maths/vneg)
                             (update :health - 10))
                         player)]
    [updated-player asteroid world]))

(def base-explosion
  {:type :explosion
   :size 1
   :lifetime 0
   :alpha 1})

(defn player-explosions [position]
  [(merge base-explosion {:max-size 200 :position position :color "yellow" :duration 80})
   (merge base-explosion {:max-size 100 :position position :color "orange" :duration 60})
   (merge base-explosion {:max-size 60 :position position :color "red" :duration 40})])

(defn laser-explosions [position]
  [(merge base-explosion {:position position :max-size 100 :duration 30 :color "red"})
   (merge base-explosion {:position position :max-size 50 :duration 15 :color "pink"})])

(defmethod collisions/detect-between [:laser :asteroid] [laser asteroid world]
  (let [colliding? (and (mid-phase-colliding? laser asteroid) (narrow-phase-detect laser asteroid))]
    [laser asteroid (if colliding?
                      (->> world
                           (ces/remove-entity-after-render (:id laser))
                           (ces/add-entities-before-render (laser-explosions (:position laser))))
                      world)]))

(defmethod collisions/detect-between [:laser :player] [laser player world]
  (let [colliding? (and (not= (:fired-by laser) (:id player))
                        (mid-phase-colliding? laser player)
                        (narrow-phase-detect laser player))
        updated-world (if colliding?
                        (->> world
                             (ces/remove-entity-after-render (:id laser))
                             (ces/add-entities-before-render (laser-explosions (:position laser))))
                        world)
        updated-player (if colliding?
                         (update player :health - 20)
                         player)]
    [laser updated-player updated-world]))

(defn track-with-camera [entity world]
  (let [camera-index (:tracked-by-camera-index entity)
        updated-world (assoc-in world [:cameras camera-index :position] (:position entity))]
    [entity updated-world]))

(defmulti on-death (fn [entity _] (:type entity)))

(defmethod on-death :player [player world]
  (let [updated-world (->> world
                           (ces/remove-entity-before-render (:id player))
                           (ces/add-entities-before-render (player-explosions (:position player))))]
    [player updated-world]))

(defn check-if-dead [entity world]
  (if (< (:health entity) 0)
    (on-death entity world)
    entity))

(defmulti lifetime (fn [entity _] (:type entity)))

(defmethod lifetime :explosion [explosion world]
  (let [{:keys [lifetime duration]} explosion
        ease-value (easing/out-expo lifetime duration 0 1)]
    (if (<= lifetime duration)
      (assoc explosion :size (* (:max-size explosion) ease-value)
                       :alpha (- 1 ease-value))
      [explosion (ces/remove-entity-after-render (:id explosion) world)])))

(defn do-lifetime [entity world]
  (let [result (lifetime entity world)
        updated-entity (if (vector? result) (nth result 0) result)
        updated-world (if (vector? result) (nth result 1) world)]
    [(update updated-entity :lifetime + (:delta world)) updated-world]))

; NOTE systems that add entities based on positions of existing entities should be placed
; after moving / rotating to ensure correct positions
(def systems
  (mapv #(assoc % :key (ces/key-for-system %))
        [{:filter-fn :key-bindings
          :system-fn keyboard-move}

         {:filter-fn :velocity
          :system-fn moving}

         {:filter-fn :rotate-speed
          :system-fn rotating}

         {:filter-fn :key-bindings
          :system-fn keyboard-weapon-control}

         {:filter-fn :lifetime
          :system-fn do-lifetime}

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
          :system-fn track-with-camera}]))

(def initial-screen-dimensions (get-window-dimensions))
(def initial-screen-width (nth initial-screen-dimensions 0))
(def initial-screen-height (nth initial-screen-dimensions 1))
(def initial-world-width 4000)
(def initial-world-height 4000)
(def initial-world-dimensions [initial-world-width initial-world-height])

(def stars (mapv (fn [index] {:id index
                              :position [(rand initial-world-width) (rand initial-world-height)]
                              :size (maths/rand-between 1 3)})
                 (range 400)))

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
          :points (shapes/isoceles-triangle size)
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
                  :position [(* initial-world-width 0.34) (* initial-world-height 0.34)]
                  :rotation 45
                  :tracked-by-camera-index 0
                  :key-bindings {:left 65
                                 :right 68
                                 :up 87
                                 :down 83
                                 :shoot 70
                                 :change-weapon 71}}))


(defn create-player2 []
  (create-player {:color "green"
                  :rotation 135
                  :position [(* initial-world-width 0.67) (* initial-world-height 0.34)]
                  :tracked-by-camera-index 1
                  :key-bindings {:left 37
                                 :right 39
                                 :up 38
                                 :down 40
                                 :shoot 75
                                 :change-weapon 76}}))

(defn create-player3 []
  (create-player {:color "blue"
                  :rotation 315
                  :position [(* initial-world-width 0.34) (* initial-world-height 0.67)]
                  :tracked-by-camera-index 2
                  :key-bindings {:left 37
                                 :right 39
                                 :up 38
                                 :down 40
                                 :shoot 75
                                 :change-weapon 76}}))

(defn create-player4 []
  (create-player {:color "purple"
                  :rotation 225
                  :position [(* initial-world-width 0.67) (* initial-world-height 0.67)]
                  :tracked-by-camera-index 3
                  :key-bindings {:left 37
                                 :right 39
                                 :up 38
                                 :down 40
                                 :shoot 75
                                 :change-weapon 76}}))

(defn create-random-asteroid []
  (let [size (maths/rand-between 80 400)
        points (shapes/random-sided-convex-polygon (/ size 2))
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

(defn cameras-for-2players [player1 player2]
  (let [half-screen-width (/ initial-screen-width 2)]
    [{:position (:position player1)
      :screen-position [0 0]
      :dimensions [half-screen-width initial-screen-height]}
     {:position (:position player2)
      :screen-position [half-screen-width 0]
      :dimensions [half-screen-width initial-screen-height]}]))

(defn cameras-for-3players [player1 player2 player3]
  (let [half-screen-width (/ initial-screen-width 2)
        half-screen-height (/ initial-screen-height 2)]
    [{:position (:position player1)
      :screen-position [0 0]
      :dimensions [half-screen-width half-screen-height]}
     {:position (:position player2)
      :screen-position [half-screen-width 0]
      :dimensions [half-screen-width half-screen-height]}
     {:position (:position player3)
      :screen-position [0 half-screen-height]
      :dimensions [half-screen-width half-screen-height]}]))

(defn cameras-for-4players [player1 player2 player3 player4]
  (let [half-screen-width (/ initial-screen-width 2)
        half-screen-height (/ initial-screen-height 2)]
    [{:position (:position player1)
      :screen-position [0 0]
      :dimensions [half-screen-width half-screen-height]}
     {:position (:position player2)
      :screen-position [half-screen-width 0]
      :dimensions [half-screen-width half-screen-height]}
     {:position (:position player3)
      :screen-position [0 half-screen-height]
      :dimensions [half-screen-width half-screen-height]}
     {:position (:position player4)
      :screen-position [half-screen-width half-screen-height]
      :dimensions [half-screen-width half-screen-height]}]))

(defn create-world [number-of-players]
  (let [players (case number-of-players
                  2 [(create-player1) (create-player2)]
                  3 [(create-player1) (create-player2) (create-player3)]
                  4 [(create-player1) (create-player2) (create-player3) (create-player4)])
        cameras (case number-of-players
                  2 (apply cameras-for-2players players)
                  3 (apply cameras-for-3players players)
                  4 (apply cameras-for-4players players))
        entities (-> []
                     (concatv players)
                     (concatv (mapv create-random-asteroid (range 30))))
        spatial-hash-config (spatial-hashing/generate-config initial-world-width initial-world-height 5 200)]
    (as-> ces/blank-world _
          (merge _ {:collision-groups #{[:player :asteroid] [:laser :asteroid] [:laser :player]}
                    :spatial-hash-config spatial-hash-config
                    :dimensions initial-world-dimensions
                    :screen-dimensions initial-screen-dimensions
                    :cameras cameras
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

(def bindings {:bullet-time 84
               :reverse-time 82
               :pause 80
               :back-to-menu 27
               :start-game-2player 50
               :start-game-3player 51
               :start-game-4player 52})

; TODO drop frames if delta is really small
(defn playback-past [game]
  (let [past (:past game)
        [updated-world updated-past] (if (not (strict-empty? past))
                                       [(peek past) (pop past)]
                                       [(:world game) past])]
    (assoc game :world updated-world
                :past updated-past)))

(defn snapshot-world-to-past [game]
  (let [leeway 100
        max-frames-of-past (+ leeway (:max-frames-of-past game))
        snapshot-of-world (:world game)
        current-frames-of-past (count (:past game))]
    (as-> game g
          (update g :past conj snapshot-of-world)
          (if (> current-frames-of-past max-frames-of-past)
            (update g :past #(subvec % (- (count %) leeway)))
            g))))

(defn update-world [game]
  (let [target-delta-scale (if (keyboard/held? (:bullet-time bindings)) 0.1 1)
        delta-scale (maths/move-towards-linear (-> game :world :delta-scale) target-delta-scale 0.05)]
    (as-> game g
          (assoc-in g [:world :delta] (* (-> game :world :delta) delta-scale))
          (assoc-in g [:world :delta-scale] delta-scale)
          (snapshot-world-to-past g)
          (assoc g :world (ces/run-systems (:world g) systems)))))

(defn start-game [game number-of-players]
  (assoc game :world (create-world number-of-players)
              :last-frame-start-time (get-time)
              :past []
              :state :in-game))

(defn update-loop [game]
  (let [current-frame-start-time (get-time)
        game (assoc game :current-frame-start-time current-frame-start-time)

        time-since-last-frame (- current-frame-start-time (:last-frame-start-time game))
        ideal-frame-time (:ideal-frame-time game)
        game (assoc-in game [:world :delta] (/ time-since-last-frame ideal-frame-time))

        game (case (:state game)
               :menu (let [game (cond
                                  (keyboard/just-down? (:start-game-2player bindings)) (start-game game 2)
                                  (keyboard/just-down? (:start-game-3player bindings)) (start-game game 3)
                                  (keyboard/just-down? (:start-game-4player bindings)) (start-game game 4)
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

        _ (view/render-fps-overlay on-screen-ctx game)

        current-frame-duration (- (get-time) current-frame-start-time)
        time-to-next-frame (max 0 (- ideal-frame-time current-frame-duration))

        game (assoc game :last-frame-start-time current-frame-start-time)

        max-fps-history (:max-fps-history game)
        game (update game :fps-history #(as-> % fps-history
                                              (if (> (count fps-history) max-fps-history)
                                                (subvec fps-history (- (count fps-history) max-fps-history))
                                                fps-history)
                                              (conj fps-history (/ 1000 time-since-last-frame))))]
    (keyboard/tick)
    (js/setTimeout #(update-loop game) time-to-next-frame)))

#_(defn on-resize []
    (let [window-dimensions (get-window-dimensions)
          [window-width window-height] window-dimensions]
      (canvas/set-size off-screen-canvas-el window-dimensions)))

(defn start []
  ;(.addEventListener js/window "resize" on-resize)
  (keyboard/add-listeners)
  (let [ideal-fps 60
        game {:max-fps-history 200
              :fps-history []
              :ideal-fps ideal-fps
              :ideal-frame-time (/ 1000 ideal-fps)
              :max-frames-of-past (* 60 ideal-fps)
              :state :menu}]
    (update-loop game)))

(defonce _ (start))
(comment _ on-js-reload)