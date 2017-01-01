(ns cs-game.core
  (:require [cs-game.ces :as ces]
            [cs-game.view :as view]
            [cs-game.keyboard :as keyboard]
            [cs-game.canvas :as canvas]
            [cs-game.maths :as maths]
            [cs-game.collisions :as collisions]
            [cs-game.expanded-lang :refer [get-window-dimensions get-time strict-empty? concatv]]
            [cs-game.spatial-hashing :as spatial-hashing]))

(enable-console-print!)

(declare systems)

(defn delta-vector [world v]
  (let [delta (:delta world)]
    (maths/v* [delta delta] v)))

(defn accelerate-forwards [entity acceleration-magnitude]
  (let [rotation (maths/degrees-to-radians (:rotation entity))
        acceleration [(* acceleration-magnitude (Math/cos rotation))
                      (* acceleration-magnitude (Math/sin rotation))]
        new-velocity (maths/v+ (:velocity entity) acceleration)
        new-velocity-magnitude (maths/vmag new-velocity)
        max-velocity-magnitude 12
        scaled-velocity (if (> new-velocity-magnitude max-velocity-magnitude)
                          (let [scale (/ max-velocity-magnitude new-velocity-magnitude)]
                            (maths/v* new-velocity [scale scale]))
                          new-velocity)]
    (assoc entity :velocity scaled-velocity)))

(defn keyboard-move [entity world]
  (let [rotate-speed 5
        accel 0.5
        key-bindings (:key-bindings entity)]
    (as-> entity e
          (if (keyboard/held? (:up key-bindings)) (accelerate-forwards entity (* (:delta world) accel)) e)
          (if (keyboard/held? (:left key-bindings)) (update e :rotation - (* (:delta world) rotate-speed)) e)
          (if (keyboard/held? (:right key-bindings)) (update e :rotation + (* (:delta world) rotate-speed)) e))))

(defn create-laser-at-entity [entity]
  (let [rotation (maths/degrees-to-radians (:rotation entity))
        laser-speed 30
        [evx evy] (:velocity entity)
        velocity [(+ (* laser-speed (Math/cos rotation)) evx) (+ (* laser-speed (Math/sin rotation)) evy)]]
    {:position (:position entity)
     :velocity velocity
     :rotation (:rotation entity)
     :size 20
     :color "red"
     :view :laser
     :collision :laser
     :remove-off-screen true}))

(defn keyboard-shoot [entity world]
  (if (keyboard/just-down? (-> entity :key-bindings :shoot))
    [entity (ces/add-entity-to-world (create-laser-at-entity entity) world systems)]
    [entity world]))

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
        bottom (+ world-height size)]
    (cond
      (< x left) (assoc entity :remove true)
      (> x right) (assoc entity :remove true)
      (< y top) (assoc entity :remove true)
      (> y bottom) (assoc entity :remove true)
      :else entity)))

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

(defmethod collisions/detect-between [:player :asteroid] [player asteroid world]
  (let [[dx dy] (maths/v- (:position player) (:position asteroid))
        dist-sq (+ (* dx dx) (* dy dy))
        min-dist (+ (/ (:size player) 2) (/ (:size asteroid) 2))
        min-dist-sq (* min-dist min-dist)
        colliding? (< dist-sq min-dist-sq)
        updated-asteroid (if colliding?
                           (assoc asteroid :remove true)
                           asteroid)]
    [player updated-asteroid world]))

(defmethod collisions/detect-between [:laser :asteroid] [laser asteroid world]
  (let [[dx dy] (maths/v- (:position laser) (:position asteroid))
        dist-sq (+ (* dx dx) (* dy dy))
        min-dist (+ (/ (:size laser) 2) (/ (:size asteroid) 2))
        min-dist-sq (* min-dist min-dist)
        colliding? (< dist-sq min-dist-sq)
        [updated-laser updated-asteroid] (if colliding?
                                           [(assoc laser :remove true) (assoc asteroid :remove true)]
                                           [laser asteroid])]
    [updated-laser updated-asteroid world]))

(defn track-with-camera [entity world]
  (let [camera-index (:tracked-by-camera-index entity)
        updated-world (assoc-in world [:cameras camera-index :position] (:position entity))]
    [entity updated-world]))

(def systems
  [{:filter-fn :key-bindings
    :system-fn keyboard-move}

   {:filter-fn :key-bindings
    :system-fn keyboard-shoot}

   {:filter-fn :wrap
    :system-fn wrap}

   {:filter-fn :velocity
    :system-fn moving}

   {:filter-fn :rotate-speed
    :system-fn rotating}

   {:filter-fn :remove-off-screen
    :system-fn remove-off-screen}

   {:filter-fn :bounce-off-edge
    :system-fn bounce-off-edge}

   {:filter-fn :collision
    :multiple-entity-system? true
    :system-fn collisions/system}

   {:filter-fn :tracked-by-camera-index
    :system-fn track-with-camera}])

(def initial-screen-dimensions (get-window-dimensions))
(def initial-screen-width (nth initial-screen-dimensions 0))
(def initial-screen-height (nth initial-screen-dimensions 1))
(def initial-world-width 5000)
(def initial-world-height 5000)
(def initial-world-dimensions [initial-world-width initial-world-height])

(def stars (mapv (fn [index] {:id index
                              :position [(rand initial-world-width) (rand initial-world-height)]
                              :size (maths/rand-between 1 3)})
                 (range 300)))

(def initial-player1-camera
  {:position [(* initial-world-width 0.34) (* initial-world-height 0.5)]
   :screen-position [0 0]
   :dimensions [(/ initial-screen-width 2) initial-screen-height]})

(def initial-player1
  {:position [(* initial-world-width 0.34) (* initial-world-height 0.5)]
   :velocity [0 0]
   :rotation 0
   :size 50
   :health 100
   :view :player
   :color "blue"
   :bounce-off-edge true
   :tracked-by-camera-index 0
   :collision :player
   :key-bindings {:left 65
                  :right 68
                  :up 87
                  :down 83
                  :shoot 32}})

(def initial-player2-camera
  {:position [(* initial-world-width 0.67) (* initial-world-height 0.5)]
   :screen-position [(/ initial-screen-width 2) 0]
   :dimensions [(/ initial-screen-width 2) initial-screen-height]})

(def initial-player2
  {:position [(* initial-world-width 0.67) (* initial-world-height 0.5)]
   :velocity [0 0]
   :rotation 180
   :size 50
   :health 100
   :view :player
   :color "green"
   :tracked-by-camera-index 1
   :bounce-off-edge true
   :collision :player
   :key-bindings {:left 37
                  :right 39
                  :up 38
                  :down 40
                  :shoot 13}})

(defn create-random-asteroid []
  {:position [(rand initial-world-width) (rand initial-world-height)]
   :velocity [(maths/rand-between -1.5 1.5) (maths/rand-between -1.5 1.5)]
   :size (maths/rand-between 50 250)
   :rotation (rand-int 360)
   :rotate-speed (maths/rand-between -1.5 1.5)
   :health 100
   :color "saddlebrown"
   :view :asteroid
   :collision :asteroid
   :wrap true})

(defn create-world []
  (let [asteroids (mapv create-random-asteroid (range 80))
        entities (-> []
                     (conj initial-player1 initial-player2)
                     (concatv asteroids))]
    (as-> ces/blank-world _
          (merge _ {:collision-groups #{[:player :asteroid] [:laser :asteroid]}
                    :spatial-hash-config (spatial-hashing/generate-config initial-world-width initial-world-height 3 200)
                    :dimensions initial-world-dimensions
                    :screen-dimensions initial-screen-dimensions
                    :cameras [initial-player1-camera initial-player2-camera]
                    :stars stars})
          (ces/add-entities-to-world entities _ systems))))

(defonce g-world-atom (atom (create-world)))

(def off-screen-canvas-el (.getElementById js/document "off-screen-canvas"))
(canvas/set-size off-screen-canvas-el initial-screen-width initial-screen-height)
(def off-screen-ctx (canvas/context off-screen-canvas-el))

(def on-screen-canvas-el (.getElementById js/document "on-screen-canvas"))
(canvas/set-size on-screen-canvas-el initial-screen-width initial-screen-height)
(def on-screen-ctx (canvas/context on-screen-canvas-el))

(def bindings {:bullet-time 82
               :reverse-time 71})

(defn playback-past [game]
  (let [past (:past game)
        ideal-frame-time (:ideal-frame-time game)
        [updated-world updated-past] (if (not (strict-empty? past))
                                       [(peek past) (pop past)]
                                       [(:world game) past])
        updated-world (assoc updated-world :last-frame-start-time (get-time)
                                           :suggested-wait-time (* (:delta updated-world) ideal-frame-time))]
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
            (update g :past #(into [] (drop leeway %)))
            g))))

(defn do-normal-game-stuff [game]
  (let [updated-game (snapshot-world-to-past game)

        ideal-frame-time (:ideal-frame-time game)
        world (:world updated-game)
        last-frame-start-time (:last-frame-start-time world)
        current-frame-start-time (get-time)
        time-since-last-frame (- current-frame-start-time last-frame-start-time)
        delta (/ time-since-last-frame ideal-frame-time)

        target-delta-scale (if (keyboard/held? (:bullet-time bindings)) 0.1 1)
        delta-scale (maths/move-towards-linear (:delta-scale world) target-delta-scale 0.05)

        updated-world (-> world
                          (assoc :delta (* delta delta-scale) :delta-scale delta-scale)
                          (ces/run-systems systems)
                          (assoc :last-frame-start-time current-frame-start-time
                                 :fps (/ 1000 time-since-last-frame)))]
    (keyboard/tick)
    (assoc updated-game :world updated-world)))

(defn update-loop [game]
  (let [updated-game (if (keyboard/held? (:reverse-time bindings))
                       (playback-past game)
                       (do-normal-game-stuff game))
        world (:world updated-game)]
    (view/render off-screen-canvas-el off-screen-ctx on-screen-ctx world)

    (let [ideal-frame-time (:ideal-frame-time game)
          frame-duration (- (get-time) (:last-frame-start-time world))
          time-to-wait (max 0 (- ideal-frame-time frame-duration))
          suggested-wait-time (:suggested-wait-time world)
          updated-game (assoc-in updated-game [:world :suggested-wait-time] nil)]
      (js/setTimeout #(update-loop updated-game) (or suggested-wait-time time-to-wait)))))

#_(defn on-resize []
    (let [window-dimensions (get-window-dimensions)
          [window-width window-height] window-dimensions]
      (swap! g-world-atom
             update
             :cameras
             (fn [cameras]
               ))
      (canvas/set-size off-screen-canvas-el window-dimensions)))

(defn start []
  ;(.addEventListener js/window "resize" on-resize)
  (keyboard/add-listeners)
  (let [world (-> (create-world)
                  (assoc :last-frame-start-time (get-time)
                         :delta-scale 1))
        ideal-fps 30
        game {:ideal-fps ideal-fps
              :ideal-frame-time (/ 1000 ideal-fps)
              :max-frames-of-past (* 60 ideal-fps)
              :past []
              :world world
              :state :in-game}]
    (update-loop game)))

(defonce _ (start))
(comment _ on-js-reload)