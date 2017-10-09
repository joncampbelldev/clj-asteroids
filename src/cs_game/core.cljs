(ns cs-game.core
  (:require [cs-game.ces :as ces]
            [cs-game.view :as view]
            [cs-game.util.keyboard :as keyboard]
            [cs-game.util.shapes :as shapes]
            [cs-game.util.canvas :as canvas]
            [cs-game.util.maths :as maths]
            [cs-game.util.easing :as easing]
            [cs-game.collisions :as collisions]
            [cs-game.util.expanded-lang :refer [get-window-dimensions get-time strict-empty? concatv]]
            [cs-game.spatial-hashing :as spatial-hashing]

            [cljs.pprint :refer [pprint]])
  (:require-macros [cs-game.util.expanded-lang :refer [defn-memo]]))

(enable-console-print!)

; TODO give everything (lasers/missiles) a health, collisions cause damage, death causes explosion
; TODO use main entity spatial hash for collisions

(def entity-type->z-index
  {:weapon 100
   :spaceship 200
   :asteroid 300
   :explosion 400})

(defn accelerate-forwards [rotation-in-degrees velocity max-speed acceleration-magnitude]
  (let [rotation (maths/degrees-to-radians rotation-in-degrees)
        acceleration [(* acceleration-magnitude (maths/cos rotation))
                      (* acceleration-magnitude (maths/sin rotation))]
        new-velocity (maths/v+ velocity acceleration)
        new-velocity-magnitude (maths/vmag new-velocity)
        max-velocity-magnitude max-speed
        scaled-velocity (if (> new-velocity-magnitude max-velocity-magnitude)
                          (let [scale (/ max-velocity-magnitude new-velocity-magnitude)]
                            (maths/v* new-velocity [scale scale]))
                          new-velocity)]
    scaled-velocity))

(defn keyboard-move [entity world]
  (let [key-bindings (:entity/control-bindings entity)]
    (as-> entity e
          (if (keyboard/held? (:up key-bindings))
            (assoc e :velocity (accelerate-forwards (:rotation e)
                                                    (:velocity e)
                                                    (:max-speed e)
                                                    (* (:delta world) (:max-accel entity))))
            e)
          (if (keyboard/held? (:left key-bindings))
            (update e :rotation - (* (:delta world) (:max-rotate-speed entity)))
            e)
          (if (keyboard/held? (:right key-bindings))
            (update e :rotation + (* (:delta world) (:max-rotate-speed entity)))
            e))))

(defn create-laser-at-entity [entity offset-angle-in-degrees color]
  (let [rotation (maths/degrees-to-radians (+ offset-angle-in-degrees (:rotation entity)))
        speed 750
        cos-rotation (maths/cos rotation)
        sin-rotation (maths/sin rotation)
        velocity (maths/v+ [(* speed cos-rotation) (* speed sin-rotation)]
                           (:velocity entity))
        size 30
        polygon-points (shapes/rectangle size 6)]
    {:position (maths/v+ (:position entity)
                         [(* (:size entity) cos-rotation) (* (:size entity) sin-rotation)])
     :velocity velocity
     :rotation (+ offset-angle-in-degrees (:rotation entity))
     :size size
     :entity/collision :entity-collision/projectile
     :entity/collision-polygon-points polygon-points
     :entity/remove-off-screen true

     :view/polygon-points polygon-points
     :view/type :view-type/polygon
     :view/color color
     :view/z-index (:weapon entity-type->z-index)
     :view/show-in-minimap true}))

(defn create-missile-at-entity [entity]
  (let [rotation (maths/degrees-to-radians (:rotation entity))
        initial-speed 200
        cos-rotation (maths/cos rotation)
        sin-rotation (maths/sin rotation)
        velocity (maths/v+ [(* initial-speed cos-rotation) (* initial-speed sin-rotation)]
                           (:velocity entity))
        size 30
        polygon-points (shapes/rectangle size 10)]
    {:position (maths/v+ (:position entity)
                         [(* (:size entity) cos-rotation) (* (:size entity) sin-rotation)])
     :velocity velocity
     :rotation (:rotation entity)
     :size size

     :fired-by-index (:entity/id entity)
     :range 600
     :max-accel 900
     :max-speed 300
     :max-rotate-speed 270
     :fuel-time (maths/rand-between 4 7)
     :entity/state-type :entity-state-type/missile

     :entity/collision-polygon-points polygon-points
     :entity/collision :entity-collision/projectile
     :entity/remove-off-screen true

     :view/type :view-type/polygon
     :view/polygon-points polygon-points
     :view/color "grey"
     :view/z-index (:weapon entity-type->z-index)
     :view/show-in-minimap true}))

(defmulti fire-weapon-from-entity (fn [weapon _ _] (:weapon/type weapon)))

(defmethod fire-weapon-from-entity :weapon-type/missile [weapon entity world]
  (let [missile (create-missile-at-entity entity)]
    [weapon entity (ces/add-entity missile world)]))

(defmethod fire-weapon-from-entity :weapon-type/single-laser [weapon entity world]
  (let [laser (create-laser-at-entity entity 0 "lightgreen")]
    [weapon entity (ces/add-entity laser world)]))

(defmethod fire-weapon-from-entity :weapon-type/shotgun-laser [weapon entity world]
  (let [lasers [(create-laser-at-entity entity 0 "red")
                (create-laser-at-entity entity -4 "red")
                (create-laser-at-entity entity 4 "red")
                (create-laser-at-entity entity -8 "red")
                (create-laser-at-entity entity 8 "red")]]
    [weapon entity (ces/add-entities lasers world)]))

(defmethod fire-weapon-from-entity :weapon-type/scatter-shot [weapon entity world]
  (let [colours ["red" "pink" "blue" "lightblue" "green" "white" "black"]
        shots (repeatedly (maths/rand-between 8 16)
                          #(create-laser-at-entity entity (maths/rand-between 0 360) (rand-nth colours)))]
    [weapon entity (ces/add-entities shots world)]))

(defn shoot [entity world]
  (let [weapons (:weapons entity)
        weapon-index (:current-weapon-index entity)
        weapon (nth weapons weapon-index)
        [updated-weapon updated-entity updated-world] (fire-weapon-from-entity weapon entity world)]
    [(assoc-in updated-entity [:weapons weapon-index] updated-weapon) updated-world]))

(defn change-weapon [entity _]
  (update entity :current-weapon-index #(mod (inc %) (count (:weapons entity)))))

(defn keyboard-weapon-control [entity world]
  (let [key-bindings (:entity/control-bindings entity)]
    (cond
      (keyboard/just-down? (:shoot key-bindings)) (shoot entity world)
      (keyboard/just-down? (:change-weapon key-bindings)) (change-weapon entity world)
      :else entity)))

(defn moving [entity world]
  (let [delta (:delta world)]
    (update entity :position maths/v+ (maths/v* [delta delta] (:velocity entity)))))

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
    [entity (if off-screen? (ces/remove-entity (:entity/id entity) world) world)]))

(defn bounce-off-edge [entity world]
  (let [[x y] (:position entity)
        size (:size entity)
        half-size (/ size 2)
        [world-width world-height] (:dimensions world)
        left (+ 0 half-size)
        right (- world-width half-size)
        top (+ 0 half-size)
        bottom (- world-height half-size)
        coef-restitution (- (:entity/bounce-off-edge entity))]
    (as-> entity e
          (if (< x left) (-> e
                             (update-in [:velocity 0] * coef-restitution)
                             (assoc-in [:position 0] left)) e)
          (if (> x right) (-> e
                              (update-in [:velocity 0] * coef-restitution)
                              (assoc-in [:position 0] right)) e)
          (if (< y top) (-> e
                            (update-in [:velocity 1] * coef-restitution)
                            (assoc-in [:position 1] top)) e)
          (if (> y bottom) (-> e
                               (update-in [:velocity 1] * coef-restitution)
                               (assoc-in [:position 1] bottom)) e))))

(defn damage-for-speed-diff [velocity1 velocity2]
  (let [diff (-> (maths/v- velocity1 velocity2)
                 (maths/vmag)
                 (maths/abs))]
    (if (> diff 80)
      (/ diff 5)
      0)))

(defmethod collisions/collision-between [:entity-collision/spaceship :entity-collision/asteroid] [spaceship asteroid response world]
  (let [updated-spaceship (-> spaceship
                              (update :position maths/v- (:overlap response))
                              (update :velocity maths/v* [-0.5 -0.5])
                              (update :entity/health - (damage-for-speed-diff (:velocity spaceship) (:velocity asteroid))))]
    [updated-spaceship asteroid world]))

(def base-explosion
  {:entity/state-type :entity-state-type/explosion
   :size 1
   :view/alpha 1
   :view/type :view-type/explosion
   :view/z-index (:explosion entity-type->z-index)})

(defn spaceship-explosions [position]
  [(merge base-explosion {:max-size 100 :position position :view/color "red" :duration 2.5 :view/no-outline true})
   (merge base-explosion {:max-size 150 :position position :view/color "orange" :duration 1.5 :view/no-outline true})
   (merge base-explosion {:max-size 250 :position position :view/color "yellow" :duration 1})])

(defn laser-explosions [{:keys [position view/color]}]
  [(merge base-explosion {:position position :max-size 50 :duration 1 :view/color "white" :view/no-outline true})
   (merge base-explosion {:position position :max-size 100 :duration 0.75 :view/color color})])

(defmethod collisions/collision-between [:entity-collision/spaceship :entity-collision/spaceship] [spaceship1 spaceship2 response world]
  (let [damage (damage-for-speed-diff (:velocity spaceship1) (:velocity spaceship2))
        updated-spaceship1 (-> spaceship1
                               (update :position maths/v- (:overlap response))
                               (update :velocity maths/v* [-0.5 -0.5])
                               (update :entity/health - damage))
        updated-spaceship2 (-> spaceship2
                               (update :velocity maths/v* [-0.5 -0.5])
                               (update :entity/health - damage))]
    [updated-spaceship1 updated-spaceship2 world]))

(defn explode-laser [laser world]
  (->> world
       (ces/remove-entity (:entity/id laser))
       (ces/add-entities (laser-explosions laser))))

(defmethod collisions/collision-between [:entity-collision/projectile :entity-collision/asteroid] [laser asteroid _ world]
  [laser asteroid (explode-laser laser world)])

(defmethod collisions/collision-between [:entity-collision/projectile :entity-collision/spaceship] [laser spaceship _ world]
  [laser (update spaceship :entity/health - 20) (explode-laser laser world)])

(defn track-with-camera [entity world]
  (let [camera-index (:entity/tracked-by-camera-index entity)
        updated-world (assoc-in world [:cameras camera-index :position] (:position entity))]
    [entity updated-world]))

(defmulti on-entity-death (fn [entity _] (:death-type entity)))

(defmethod on-entity-death :entity-collision/spaceship [spaceship world]
  [spaceship (->> world
                  (ces/remove-entity (:entity/id spaceship))
                  (ces/add-entities (spaceship-explosions (:position spaceship))))])

(defn check-if-dead [entity world]
  (if (<= (:entity/health entity) 0)
    (on-entity-death entity world)
    entity))

(defmulti state (fn [entity _] (:entity/state-type entity)))

(defn do-state-update [entity world]
  (let [result (state entity world)
        [updated-entity updated-world] (cond
                                         (vector? result) result
                                         (map? result) [result world])]
    [(update updated-entity :entity/time maths/safe+ (:delta world)) updated-world]))

(defmethod state :entity-state-type/explosion [explosion world]
  (let [{:keys [entity/time duration]} explosion
        ease-value (easing/out-expo time duration 0 1)]
    (if (<= time duration)
      (assoc explosion :size (* (:max-size explosion) ease-value)
                       :view/alpha (- 1 ease-value))
      [explosion (ces/remove-entity (:entity/id explosion) world)])))

(defn find-nearest-spaceship [missile world]
  (let [range (:range missile)

        entity-spatial-hash (:entity-spatial-hash world)
        entities (:ces/entities world)
        nearby-entity-indexes (spatial-hashing/nearby-entity-indexes entity-spatial-hash {:position (:position missile)
                                                                                          :size (* range 2)})
        nearby-entities (mapv #(nth entities %) nearby-entity-indexes)
        nearby-spaceships (filterv #(and (= (:entity/collision %) :entity-collision/spaceship)
                                         (not= (:entity/id %) (:fired-by-index missile))
                                         (< (maths/vmag (maths/v- (:position %) (:position missile))) range))
                                   nearby-entities)]
    ; TODO find closest within radius
    (first nearby-spaceships)))

(defn aim-missile-towards-target [missile target delta]
  (update missile
          :rotation
          (fn [current-angle]
            (let [current-angle (mod current-angle 360)

                  predicted-position (maths/v+ (:position target) (maths/v* [2 2] [delta delta] (:velocity target)))
                  [dx dy] (maths/v- predicted-position (:position missile))
                  angle-to-target (maths/radians-to-degrees (maths/atan2 dy dx))

                  delta-angle (maths/abs (- current-angle angle-to-target))
                  go-other-way? (> delta-angle 180)

                  [current-angle angle-to-target] (cond
                                                    (and go-other-way? (> angle-to-target current-angle)) [current-angle (- angle-to-target 360)]
                                                    (and go-other-way? (> current-angle angle-to-target)) [(- current-angle 360) angle-to-target]
                                                    :else [current-angle angle-to-target])]
              (maths/move-towards-linear current-angle angle-to-target (* delta (:max-rotate-speed missile)))))))

(defmethod state :entity-state-type/missile [missile world]
  (case (:missile-state missile)
    :searching
    (if-let [nearest-spaceship (find-nearest-spaceship missile world)]
      (assoc missile :missile-state :locked
                     :missile-target-index (:entity/id nearest-spaceship))
      missile)

    :locked
    (cond
      (nil? (nth (:ces/entities world) (:missile-target-index missile)))
      (-> missile
          (assoc :missile-state :searching)
          (dissoc :missile-target-index))

      (pos? (:fuel-time missile))
      (-> missile
          (aim-missile-towards-target (nth (:ces/entities world) (:missile-target-index missile)) (:delta world))
          (assoc :velocity (accelerate-forwards (:rotation missile)
                                                (:velocity missile)
                                                (:max-speed missile)
                                                (* (:delta world) (:max-accel missile))))
          (update :fuel-time - (:delta world)))

      :else missile)

    (assoc missile :missile-state :searching)))

; NOTE systems that add entities based on positions of existing entities should be placed
; after moving / rotating to ensure correct positions
(def systems
  (mapv #(assoc % :key (ces/key-for-system %))
        [{:ces/filter-fn :entity/control-bindings
          :ces/system-fn keyboard-move}

         {:ces/filter-fn :velocity
          :ces/system-fn moving}

         {:ces/filter-fn :rotate-speed
          :ces/system-fn rotating}

         {:ces/filter-fn :entity/control-bindings
          :ces/system-fn keyboard-weapon-control}

         {:ces/filter-fn :entity/health
          :ces/system-fn check-if-dead}

         {:ces/filter-fn :entity/state-type
          :ces/system-fn do-state-update}

         {:ces/filter-fn :entity/bounce-off-edge
          :ces/system-fn bounce-off-edge}

         {:ces/filter-fn :entity/remove-off-screen
          :ces/system-fn remove-off-screen}

         {:ces/filter-fn :entity/collision
          :ces/multiple-entity-system? true
          :ces/system-fn collisions/system}

         {:ces/filter-fn :entity/tracked-by-camera-index
          :ces/system-fn track-with-camera}]))

(def initial-screen-dimensions (get-window-dimensions))
(def initial-screen-width (nth initial-screen-dimensions 0))
(def initial-screen-height (nth initial-screen-dimensions 1))
(def initial-world-width 3000)
(def initial-world-height 3000)
(def initial-world-dimensions [initial-world-width initial-world-height])

(def single-laser {:weapon/type :weapon-type/single-laser :weapon/short-label "1"})
(def shotgun-laser {:weapon/type :weapon-type/shotgun-laser :weapon/short-label "SG"})
(def scatter-shot {:weapon/type :weapon-type/scatter-shot :weapon/short-label "X"})
(def missile {:weapon/type :weapon-type/missile :weapon/short-label "M"})
(def starter-weapons [single-laser shotgun-laser scatter-shot missile])

(def default-player-size 35)
(def player-polygon-points (shapes/isoceles-triangle default-player-size))
(def base-player
  {:velocity [0 0]
   :size default-player-size

   :max-speed 150
   :max-rotate-speed 150
   :max-accel 300

   :entity/collision :entity-collision/spaceship
   :entity/collision-polygon-points player-polygon-points
   :entity/bounce-off-edge 0.5

   :weapons starter-weapons
   :current-weapon-index 0

   :entity/health 100
   :death-type :entity-collision/spaceship

   :view/polygon-points player-polygon-points
   :view/type :view-type/polygon
   :view/z-index (:spaceship entity-type->z-index)
   :view/show-in-minimap true})

(defn create-player1 []
  (merge base-player
         {:view/color "white"
          :entity/team 1
          :position [(* initial-world-width 0.34) (* initial-world-height 0.34)]
          :rotation 45
          :entity/tracked-by-camera-index 0
          :entity/control-bindings {:left 65
                                    :right 68
                                    :up 87
                                    :down 83
                                    :shoot 70
                                    :change-weapon 71}}))


(defn create-player2 []
  (merge base-player
         {:view/color "green"
          :rotation 135
          :entity/team 2
          :position [(* initial-world-width 0.67) (* initial-world-height 0.34)]
          :entity/tracked-by-camera-index 1
          :entity/control-bindings {:left 37
                                    :right 39
                                    :up 38
                                    :down 40
                                    :shoot 79
                                    :change-weapon 80}}))

(defn create-player3 []
  (merge base-player
         {:view/color "blue"
          :rotation 315
          :entity/team 3
          :position [(* initial-world-width 0.34) (* initial-world-height 0.67)]
          :entity/tracked-by-camera-index 2
          :entity/control-bindings {:left 37
                                    :right 39
                                    :up 38
                                    :down 40
                                    :shoot 75
                                    :change-weapon 76}}))

(defn create-player4 []
  (merge base-player
         {:view/color "purple"
          :rotation 225
          :entity/team 4
          :position [(* initial-world-width 0.67) (* initial-world-height 0.67)]
          :entity/tracked-by-camera-index 3
          :entity/control-bindings {:left 37
                                    :right 39
                                    :up 38
                                    :down 40
                                    :shoot 75
                                    :change-weapon 76}}))

(defn create-random-asteroid []
  (let [size (maths/rand-between 80 500)
        points (shapes/random-sided-convex-polygon (/ size 2))
        position [(rand initial-world-width) (rand initial-world-height)]]
    {:position position
     :velocity [(maths/rand-between -30 30) (maths/rand-between -30 30)]
     :size size
     :rotation 0
     :rotate-speed (maths/rand-between -30 30)

     :entity/collision :entity-collision/asteroid
     :entity/collision-polygon-points points
     :entity/bounce-off-edge 1

     :view/polygon-points points
     :view/color "saddlebrown"
     :view/type :view-type/polygon
     :view/z-index (:asteroid entity-type->z-index)
     :view/show-in-minimap true}))

(defn cameras-for-2players [player1 player2]
  (let [half-screen-width (/ initial-screen-width 2)]
    [{:position (:position player1)
      :screen-position [0 0]
      :hud-position :top
      :dimensions [half-screen-width initial-screen-height]}
     {:position (:position player2)
      :screen-position [half-screen-width 0]
      :hud-position :top
      :dimensions [half-screen-width initial-screen-height]}]))

(defn cameras-for-3players [player1 player2 player3]
  (let [half-screen-width (/ initial-screen-width 2)
        half-screen-height (/ initial-screen-height 2)]
    [{:position (:position player1)
      :screen-position [0 0]
      :hud-position :top
      :dimensions [half-screen-width half-screen-height]}
     {:position (:position player2)
      :screen-position [half-screen-width 0]
      :hud-position :top
      :dimensions [half-screen-width half-screen-height]}
     {:position (:position player3)
      :screen-position [0 half-screen-height]
      :hud-position :bottom
      :dimensions [half-screen-width half-screen-height]}]))

(defn cameras-for-4players [player1 player2 player3 player4]
  (let [half-screen-width (/ initial-screen-width 2)
        half-screen-height (/ initial-screen-height 2)]
    [{:position (:position player1)
      :screen-position [0 0]
      :hud-position :top
      :dimensions [half-screen-width half-screen-height]}
     {:position (:position player2)
      :screen-position [half-screen-width 0]
      :hud-position :top
      :dimensions [half-screen-width half-screen-height]}
     {:position (:position player3)
      :screen-position [0 half-screen-height]
      :hud-position :bottom
      :dimensions [half-screen-width half-screen-height]}
     {:position (:position player4)
      :hud-position :bottom
      :screen-position [half-screen-width half-screen-height]
      :dimensions [half-screen-width half-screen-height]}]))

(def collision-pairs
  #{[:entity-collision/spaceship :entity-collision/asteroid]
    [:entity-collision/spaceship :entity-collision/spaceship]
    [:entity-collision/projectile :entity-collision/asteroid]
    [:entity-collision/projectile :entity-collision/spaceship]})

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
                     (concatv (mapv create-random-asteroid (range 35))))]
    (as-> ces/blank-world w
          (merge w {:collision-pairs collision-pairs
                    :spatial-hash-config (spatial-hashing/generate-config initial-world-width
                                                                          initial-world-height
                                                                          5
                                                                          200)
                    :dimensions initial-world-dimensions
                    :screen-dimensions initial-screen-dimensions
                    :cameras cameras
                    :delta 1
                    :delta-scale 1})
          (ces/add-entities entities w)
          (ces/run-systems w systems))))

(def off-screen-canvas-el (.getElementById js/document "off-screen-canvas"))
(canvas/set-size off-screen-canvas-el initial-screen-width initial-screen-height)
(def off-screen-ctx (canvas/context off-screen-canvas-el))
(def bg-el (.getElementById js/document "bg"))
(def bg-pattern (.createPattern off-screen-ctx bg-el "repeat"))

(def on-screen-canvas-el (.getElementById js/document "on-screen-canvas"))
(canvas/set-size on-screen-canvas-el initial-screen-width initial-screen-height)
(def on-screen-ctx (canvas/context on-screen-canvas-el))

(def bindings {:toggle-freeze-time 69
               :step-forwards 190
               :step-backwards 188

               :toggle-debug 81
               :bullet-time 84
               :reverse-time 82
               :pause 27
               :back-to-menu 81
               :start-game-2player 50
               :start-game-3player 51
               :start-game-4player 52})

; TODO drop frames if delta is really small
(defn playback-past [game]
  (let [past (:game/past game)
        [updated-world updated-past] (if (not (strict-empty? past))
                                       [(peek past) (pop past)]
                                       [(:game/world game) past])]
    (assoc game :game/world updated-world
                :game/past updated-past)))

(defn snapshot-world-to-past [game]
  (let [leeway 100
        max-frames-of-past (+ leeway (:game/max-frames-of-past game))
        snapshot-of-world (:game/world game)
        current-frames-of-past (count (:game/past game))]
    (as-> game g
          (update g :game/past conj snapshot-of-world)
          (if (> current-frames-of-past max-frames-of-past)
            (update g :game/past #(subvec % (- (count %) leeway)))
            g))))

(defn update-world [game]
  (let [target-delta-scale (if (keyboard/held? (:bullet-time bindings)) 0.1 1)
        current-delta-scale (-> game :game/world :delta-scale)
        new-delta-scale (maths/move-towards-linear current-delta-scale target-delta-scale 0.05)]
    (as-> game g
          (assoc-in g [:game/world :entity-spatial-hash] (spatial-hashing/build
                                                           (filterv (complement nil?) (-> g :game/world :ces/entities))
                                                           (-> g :game/world :spatial-hash-config)))
          (assoc-in g [:game/world :delta] (* (-> game :game/world :delta) new-delta-scale))
          (assoc-in g [:game/world :delta-scale] new-delta-scale)
          (snapshot-world-to-past g)
          (assoc g :game/world (ces/run-systems (:game/world g) systems)))))

(defn start-game [game number-of-players]
  (assoc game :game/world (create-world number-of-players)
              :game/last-frame-start-time (get-time)
              :game/past []
              :game/state :game-state/in-game))

(defn render [game]
  (case (:game/state game)
    :game-state/menu (view/render-menu on-screen-ctx
                                       initial-screen-width
                                       initial-screen-height)
    (:game-state/in-game
      :game-state/frozen) (view/render-world off-screen-canvas-el
                                             off-screen-ctx on-screen-ctx
                                             (:game/world game)
                                             bg-pattern)
    :game-state/pause (do (view/render-world off-screen-canvas-el
                                             off-screen-ctx on-screen-ctx
                                             (:game/world game)
                                             bg-pattern)
                          (view/render-pause-overlay on-screen-ctx
                                                     initial-screen-width
                                                     initial-screen-height)))
  (view/render-fps-overlay on-screen-ctx game))

(defn handle-global-input [game]
  (as-> game g
        (case (:game/state g)
          :game-state/menu (cond
                             (keyboard/just-down? (:start-game-2player bindings)) (start-game g 2)
                             (keyboard/just-down? (:start-game-3player bindings)) (start-game g 3)
                             (keyboard/just-down? (:start-game-4player bindings)) (start-game g 4)
                             :else g)
          :game-state/in-game (cond
                                (keyboard/held? (:reverse-time bindings)) (playback-past g)
                                (keyboard/just-down? (:toggle-freeze-time bindings)) (assoc g :game/state :game-state/frozen)
                                (keyboard/just-down? (:pause bindings)) (assoc g :game/state :game-state/pause)
                                :else (update-world g))
          :game-state/pause (cond
                              (keyboard/just-down? (:pause bindings)) (assoc g :game/state :game-state/in-game)
                              (keyboard/just-down? (:back-to-menu bindings)) (assoc g :game/state :game-state/menu)
                              :else g)
          :game-state/frozen (cond
                               (keyboard/just-down? (:step-backwards bindings)) (playback-past g)
                               (keyboard/just-down? (:step-forwards bindings)) (update-world g)
                               (keyboard/just-down? (:toggle-freeze-time bindings)) (assoc g :game/state :game-state/in-game)
                               :else g)
          g)
        (if (keyboard/just-down? (:toggle-debug bindings))
          (update-in g [:game/world :debug?] not)
          g)))

(defn update-fps-history [fps-history max-frames-to-keep time-since-last-frame]
  (as-> fps-history fps-history
        (if (> (count fps-history) max-frames-to-keep)
          (subvec fps-history (- (count fps-history) max-frames-to-keep))
          fps-history)
        (conj fps-history (/ 1000 time-since-last-frame))))

(defonce repl-game-snapshot (atom nil))
(defn update-loop []
  (let [game @repl-game-snapshot
        current-frame-start-time (get-time)
        last-frame-start-time (:game/last-frame-start-time game)
        time-since-last-frame (- current-frame-start-time last-frame-start-time)
        game (as-> game g
                   (assoc-in g [:game/world :delta] (/ time-since-last-frame 1000))
                   (assoc g :game/last-frame-start-time current-frame-start-time)
                   (handle-global-input g)
                   (update g :game/fps-history update-fps-history (:game/max-fps-history g) time-since-last-frame))]
    (render game)
    (keyboard/tick)
    (let [current-frame-duration (- (get-time) current-frame-start-time)
          time-to-next-frame (max 0 (- (:game/ideal-frame-time game) current-frame-duration))]
      (js/setTimeout update-loop time-to-next-frame)
      (reset! repl-game-snapshot game))))

#_(defn on-resize []
    (let [window-dimensions (get-window-dimensions)
          [window-width window-height] window-dimensions]
      ; TODO resize all cameras
      (canvas/set-size off-screen-canvas-el window-dimensions)))

(defn start []
  ;(.addEventListener js/window "resize" on-resize)
  (keyboard/add-listeners)
  (let [ideal-fps 30
        game {:game/max-fps-history (* ideal-fps 5)
              :game/fps-history []
              :game/ideal-fps ideal-fps
              :game/ideal-frame-time (/ 1000 ideal-fps)
              :game/max-frames-of-past (* 60 ideal-fps)
              :game/state :game-state/menu}]
    (reset! repl-game-snapshot game)
    (update-loop)))

; for repl stuff
(defn repl-game [] @repl-game-snapshot)
(defn repl-world [] (:game/world (repl-game)))
(defn repl-entity
  ([index] (nth (:ces/entities (repl-world)) index))
  ([index f & args]
   (let [entity (repl-entity index)
         updated-entity (apply f entity args)]
     (swap! repl-game-snapshot assoc-in [:game/world :ces/entities index] updated-entity)
     updated-entity)))
(defn repl-add-entities [& entities]
  (swap! repl-game-snapshot update #(ces/add-entities-to-world entities % systems)))

(defonce _ (start))
(comment _ on-js-reload repl-add-entities)
