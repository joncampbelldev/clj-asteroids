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
            [cs-game.util.sat :as sat])
  (:require-macros [cs-game.util.expanded-lang :refer [defn-memo]]))

(enable-console-print!)

(defn accelerate-forwards [entity acceleration-magnitude]
  (let [rotation (maths/degrees-to-radians (:rotation entity))
        acceleration [(* acceleration-magnitude (maths/cos rotation))
                      (* acceleration-magnitude (maths/sin rotation))]
        new-velocity (maths/v+ (:velocity entity) acceleration)
        new-velocity-magnitude (maths/vmag new-velocity)
        max-velocity-magnitude (:max-speed entity)
        scaled-velocity (if (> new-velocity-magnitude max-velocity-magnitude)
                          (let [scale (/ max-velocity-magnitude new-velocity-magnitude)]
                            (maths/v* new-velocity [scale scale]))
                          new-velocity)]
    (assoc entity :velocity scaled-velocity)))

(defn keyboard-move [entity world]
  (let [rotate-speed (:max-rotate-speed entity)
        accel (:max-accel entity)
        key-bindings (:key-bindings entity)]
    (as-> entity e
          (if (keyboard/held? (:up key-bindings))
            (accelerate-forwards entity (* (:delta world) accel))
            e)
          (if (keyboard/held? (:left key-bindings))
            (update e :rotation - (* (:delta world) rotate-speed))
            e)
          (if (keyboard/held? (:right key-bindings))
            (update e :rotation + (* (:delta world) rotate-speed))
            e))))

(defn create-laser-at-entity [entity offset-angle-in-degrees]
  (let [rotation (maths/degrees-to-radians (+ offset-angle-in-degrees (:rotation entity)))
        laser-speed 750
        cos-rotation (maths/cos rotation)
        sin-rotation (maths/sin rotation)
        velocity (maths/v+ [(* laser-speed cos-rotation) (* laser-speed sin-rotation)]
                           (:velocity entity))
        laser-size 30]
    {:position (maths/v+ (:position entity)
                         [(* (:size entity) cos-rotation) (* (:size entity) sin-rotation)])
     :velocity velocity
     :rotation (:rotation entity)
     :size laser-size
     :points (shapes/rectangle laser-size 6)
     :color "red"
     :collision :laser
     :view :laser
     :remove-off-screen true}))

(defmulti fire-weapon-from-entity (fn [weapon _ _] (:weapon/type weapon)))

(defmethod fire-weapon-from-entity :single-laser [weapon entity world]
  (let [laser (create-laser-at-entity entity 0)]
    [weapon entity (ces/add-entity-before-render laser world)]))

(defmethod fire-weapon-from-entity :shotgun-laser [weapon entity world]
  (let [lasers [(create-laser-at-entity entity 0)
                (create-laser-at-entity entity -4)
                (create-laser-at-entity entity 4)
                (create-laser-at-entity entity -8)
                (create-laser-at-entity entity 8)]]
    [weapon entity (ces/add-entities-before-render lasers world)]))

(defn shoot [entity world]
  (let [weapons (:weapons entity)
        weapon-index (:current-weapon-index entity)
        weapon (nth weapons weapon-index)
        [updated-weapon updated-entity updated-world] (fire-weapon-from-entity weapon entity world)]
    [(assoc-in updated-entity [:weapons weapon-index] updated-weapon) updated-world]))

(defn change-weapon [entity _]
  (update entity :current-weapon-index #(mod (inc %) (count (:weapons entity)))))

(defn keyboard-weapon-control [entity world]
  (let [key-bindings (:key-bindings entity)]
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
    [entity (if off-screen? (ces/remove-entity-after-render (:id entity) world) world)]))

(defn bounce-off-edge [entity world]
  (let [[x y] (:position entity)
        size (:size entity)
        half-size (/ size 2)
        [world-width world-height] (:dimensions world)
        left (+ 0 half-size)
        right (- world-width half-size)
        top (+ 0 half-size)
        bottom (- world-height half-size)
        coef-restitution (- (:bounce-off-edge entity))]
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

(defmethod collisions/collision-between [:player :asteroid] [player asteroid response world]
  (let [updated-player (-> player
                           (update :position maths/v- (:overlap response))
                           (update :velocity maths/v* [-0.5 -0.5])
                           (update :health - (damage-for-speed-diff (:velocity player) (:velocity asteroid))))]
    [updated-player asteroid world]))

(def base-explosion
  {:view :explosion
   :state-type :explosion
   :size 1
   :time 0
   :alpha 1})

(defn player-explosions [position]
  [(merge base-explosion {:max-size 100 :position position :color "red" :duration 2.5 :no-outline true})
   (merge base-explosion {:max-size 150 :position position :color "orange" :duration 1.5 :no-outline true})
   (merge base-explosion {:max-size 250 :position position :color "yellow" :duration 1})])

(defn laser-explosions [position]
  [(merge base-explosion {:position position :max-size 50 :duration 1 :color "pink" :no-outline true})
   (merge base-explosion {:position position :max-size 100 :duration 0.75 :color "red"})])

(defmethod collisions/collision-between [:player :player] [player1 player2 response world]
  (let [damage (damage-for-speed-diff (:velocity player1) (:velocity player2))
        updated-player1 (-> player1
                            (update :position maths/v- (:overlap response))
                            (update :velocity maths/v* [-0.5 -0.5])
                            (update :health - damage))
        updated-player2 (-> player2
                            (update :velocity maths/v* [-0.5 -0.5])
                            (update :health - damage))]
    [updated-player1 updated-player2 world]))

(defn explode-laser [laser world]
  (->> world
       (ces/remove-entity-after-render (:id laser))
       (ces/add-entities-before-render (laser-explosions (:position laser)))))

(defmethod collisions/collision-between [:laser :asteroid] [laser asteroid _ world]
  [laser asteroid (explode-laser laser world)])

(defmethod collisions/collision-between [:laser :player] [laser player _ world]
  [laser (update player :health - 20) (explode-laser laser world)])

(defn track-with-camera [entity world]
  (let [camera-index (:tracked-by-camera-index entity)
        updated-world (assoc-in world [:cameras camera-index :position] (:position entity))]
    [entity updated-world]))

(defmulti on-entity-death (fn [entity _] (:death-type entity)))

(defmethod on-entity-death :player [player world]
  [player (->> world
               (ces/remove-entity-after-render (:id player))
               (ces/add-entities-before-render (player-explosions (:position player))))])

(defn check-if-dead [entity world]
  (if (<= (:health entity) 0)
    (on-entity-death entity world)
    entity))

(defmulti state (fn [entity _] (:state-type entity)))

(defn do-state-update [entity world]
  (let [result (state entity world)
        [updated-entity updated-world] (cond
                                         (vector? result) result
                                         (map? result) [result world])]
    [(update updated-entity :time + (:delta world)) updated-world]))

(defmethod state :explosion [explosion world]
  (let [{:keys [time duration]} explosion
        ease-value (easing/out-expo time duration 0 1)]
    (if (<= time duration)
      (assoc explosion :size (* (:max-size explosion) ease-value)
                       :alpha (- 1 ease-value))
      [explosion (ces/remove-entity-after-render (:id explosion) world)])))

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

         {:filter-fn :health
          :system-fn check-if-dead}

         {:filter-fn :time
          :system-fn do-state-update}

         {:filter-fn :bounce-off-edge
          :system-fn bounce-off-edge}

         {:filter-fn :remove-off-screen
          :system-fn remove-off-screen}

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

(def single-laser {:weapon/type :single-laser :name "1"})
(def shotgun-laser {:weapon/type :shotgun-laser :name "SG"})
(def starter-weapons [single-laser shotgun-laser])

(def default-player-size 35)
(def base-player
  {:velocity [0 0]

   :max-speed 150
   :max-rotate-speed 150
   :max-accel 300

   :size default-player-size
   :points (shapes/isoceles-triangle default-player-size)

   :weapons starter-weapons
   :current-weapon-index 0

   :health 100
   :bounce-off-edge 0.5

   :collision :player
   :view :player
   :death-type :player})

(defn create-player1 []
  (merge base-player
         {:color "white"
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
  (merge base-player
         {:color "green"
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
  (merge base-player
         {:color "blue"
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
  (merge base-player
         {:color "purple"
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
  (let [size (maths/rand-between 80 500)
        points (shapes/random-sided-convex-polygon (/ size 2))
        position [(rand initial-world-width) (rand initial-world-height)]]
    {:position position
     :velocity [(maths/rand-between -30 30) (maths/rand-between -30 30)]
     :size size
     :rotation 0
     :rotate-speed (maths/rand-between -30 30)
     :points points
     :color "saddlebrown"
     :view :asteroid
     :bounce-off-edge 1
     :collision :asteroid}))

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
  #{[:player :asteroid]
    [:player :player]
    [:laser :asteroid]
    [:laser :player]})

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
                     (concatv (mapv create-random-asteroid (range 50))))]
    (as-> ces/blank-world _
          (merge _ {:collision-pairs collision-pairs
                    :spatial-hash-config (spatial-hashing/generate-config initial-world-width initial-world-height 5 200)
                    :dimensions initial-world-dimensions
                    :screen-dimensions initial-screen-dimensions
                    :cameras cameras
                    :delta 1
                    :delta-scale 1})
          (ces/add-entities-after-render entities _))))

(def off-screen-canvas-el (.getElementById js/document "off-screen-canvas"))
(canvas/set-size off-screen-canvas-el initial-screen-width initial-screen-height)
(def off-screen-ctx (canvas/context off-screen-canvas-el))
(def bg-el (.getElementById js/document "bg"))
(def bg-pattern (.createPattern off-screen-ctx bg-el "repeat"))

(def on-screen-canvas-el (.getElementById js/document "on-screen-canvas"))
(canvas/set-size on-screen-canvas-el initial-screen-width initial-screen-height)
(def on-screen-ctx (canvas/context on-screen-canvas-el))

(def bindings {:bullet-time 84
               :reverse-time 82
               :pause 27
               :back-to-menu 81
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
        current-delta-scale (-> game :world :delta-scale)
        new-delta-scale (maths/move-towards-linear current-delta-scale target-delta-scale 0.05)]
    (as-> game g
          (assoc-in g [:world :delta] (* (-> game :world :delta) new-delta-scale))
          (assoc-in g [:world :delta-scale] new-delta-scale)
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
        game (assoc-in game [:world :delta] (/ time-since-last-frame 1000))

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
                                     :else (update-world game))]
                          (view/render-world off-screen-canvas-el off-screen-ctx on-screen-ctx (:world game) bg-pattern)
                          game)
               :pause (let [game (cond
                                   (keyboard/just-down? (:pause bindings)) (assoc game :state :in-game)
                                   (keyboard/just-down? (:back-to-menu bindings)) (assoc game :state :menu)
                                   :else game)]
                        (view/render-world off-screen-canvas-el off-screen-ctx on-screen-ctx (:world game) bg-pattern)
                        (view/render-pause-overlay on-screen-ctx initial-screen-width initial-screen-height)
                        game))

        current-frame-duration (- (get-time) current-frame-start-time)
        time-to-next-frame (max 0 (- ideal-frame-time current-frame-duration))
        time-of-decision (get-time)

        game (assoc game :last-frame-start-time current-frame-start-time)

        max-fps-history (:max-fps-history game)
        game (update game :fps-history #(as-> % fps-history
                                              (if (> (count fps-history) max-fps-history)
                                                (subvec fps-history (- (count fps-history) max-fps-history))
                                                fps-history)
                                              (conj fps-history (/ 1000 time-since-last-frame))))]
    (view/render-fps-overlay on-screen-ctx game)
    (keyboard/tick)
    (js/setTimeout #(update-loop game) (- time-to-next-frame (- (get-time) time-of-decision)))))

#_(defn on-resize []
    (let [window-dimensions (get-window-dimensions)
          [window-width window-height] window-dimensions]
      ; TODO resize all cameras
      (canvas/set-size off-screen-canvas-el window-dimensions)))

(defn start []
  ;(.addEventListener js/window "resize" on-resize)
  (keyboard/add-listeners)
  (let [ideal-fps 30
        game {:max-fps-history (* ideal-fps 5)
              :fps-history []
              :ideal-fps ideal-fps
              :ideal-frame-time (/ 1000 ideal-fps)
              :max-frames-of-past (* 60 ideal-fps)
              :state :menu}]
    (update-loop game)))

(defonce _ (start))
(comment _ on-js-reload)
