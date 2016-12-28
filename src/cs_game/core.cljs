(ns cs-game.core
  (:require [cs-game.ces :as ces]
            [cs-game.view :as view]
            [cs-game.keyboard :as keyboard]
            [cs-game.canvas :as canvas]
            [cs-game.maths :as maths]
            [cs-game.collisions :as collisions]
            [cs-game.expanded-lang :refer [get-window-dimensions get-time not-empty?]]))

(enable-console-print!)

(declare .requestAnimationFrame systems)

(defn delta-vector [world v]
  (let [delta (:delta world)]
    (maths/vec* [delta delta] v)))

(defn accelerate-forwards [entity acceleration-magnitude]
  (let [rotation (maths/degrees-to-radians (:rotation entity))
        acceleration [(* acceleration-magnitude (Math/cos rotation))
                      (* acceleration-magnitude (Math/sin rotation))]]
    (update entity :velocity maths/vec+ acceleration)))

(defn keyboard-move [entity world]
  (let [rotate-speed 10
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
  (update entity :position maths/vec+ (delta-vector world (:velocity entity))))

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
  (let [[dx dy] (maths/vec- (:position player) (:position asteroid))
        dist-sq (+ (* dx dx) (* dy dy))
        min-dist (+ (/ (:size player) 2) (/ (:size asteroid) 2))
        min-dist-sq (* min-dist min-dist)
        colliding? (< dist-sq min-dist-sq)
        updated-asteroid (if colliding?
                           (assoc asteroid :remove true)
                           asteroid)]
    [player updated-asteroid world]))

(defmethod collisions/detect-between [:laser :asteroid] [laser asteroid world]
  (let [[dx dy] (maths/vec- (:position laser) (:position asteroid))
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

(def initial-canvas-dimensions (get-window-dimensions))
(def initial-canvas-width (nth initial-canvas-dimensions 0))
(def initial-canvas-height (nth initial-canvas-dimensions 1))
(def initial-world-size 4000)
(def initial-world-dimensions [initial-world-size initial-world-size])

(def star-positions (mapv (fn [_] (vector (rand initial-world-size) (rand initial-world-size)))
                          (range 100)))

(def initial-player1-camera
  {:position [(* initial-world-size 0.34) (* initial-world-size 0.5)]
   :screen-position [0 0]
   :dimensions [(/ initial-canvas-width 2) initial-canvas-height]})

(def initial-player1
  {:position [(* initial-world-size 0.34) (* initial-world-size 0.5)]
   :velocity [0 0]
   :rotation 0
   :size 30
   :health 100
   :view :player
   :bounce-off-edge true
   :tracked-by-camera-index 0
   :collision :player
   :key-bindings {:left 65
                  :right 68
                  :up 87
                  :down 83
                  :shoot 32}})

(def initial-player2-camera
  {:position [(* initial-world-size 0.67) (* initial-world-size 0.5)]
   :screen-position [(/ initial-canvas-width 2) 0]
   :dimensions [(/ initial-canvas-width 2) initial-canvas-height]})

(def initial-player2
  {:position [(* initial-world-size 0.67) (* initial-world-size 0.5)]
   :velocity [0 0]
   :rotation 180
   :size 30
   :health 100
   :view :player
   :tracked-by-camera-index 1
   :bounce-off-edge true
   :collision :player
   :key-bindings {:left 37
                  :right 39
                  :up 38
                  :down 40
                  :shoot 13}})

(defn create-random-asteroid []
  {:position [(rand initial-world-size) (rand initial-world-size)]
   :velocity [(maths/rand-between -2 2) (maths/rand-between -2 2)]
   :size (maths/rand-between 50 200)
   :rotation (rand-int 360)
   :rotate-speed (maths/rand-between -5 5)
   :health 100
   :view :asteroid
   :collision :asteroid
   :wrap true})

(defn create-world []
  (let [asteroids (mapv create-random-asteroid (range 100))
        entities (conj asteroids initial-player1 initial-player2)]
    (as-> ces/blank-world _
          (merge _ {:collision-groups #{[:player :asteroid] [:laser :asteroid]}
                    :dimensions initial-world-dimensions
                    :cameras [initial-player1-camera initial-player2-camera]
                    :star-positions star-positions})
          (ces/add-entities-to-world entities _ systems))))

(defonce g-world-atom (atom (create-world)))

(def off-screen-canvas-el (.getElementById js/document "off-screen-canvas"))
(canvas/set-size off-screen-canvas-el initial-canvas-width initial-canvas-height)
(def off-screen-ctx (canvas/context off-screen-canvas-el))

(def on-screen-canvas-el (.getElementById js/document "on-screen-canvas"))
(canvas/set-size on-screen-canvas-el initial-canvas-width initial-canvas-height)
(def on-screen-ctx (canvas/context on-screen-canvas-el))

(def ideal-update-fps 30)
(def ideal-update-frame-time (/ 1000 ideal-update-fps))

(def save-point-atom (atom nil))

(def bindings {:save 84
               :restore 89
               :bullet-time 82
               :reverse-time 71})

(def past-atom (atom []))

(defn update-loop [world-atom]
  (if (keyboard/held? (:reverse-time bindings))
    (do
      (let [past @past-atom]
        (when (not-empty? past)
          (reset! world-atom (peek past))
          (swap! past-atom pop)))
      (swap! world-atom assoc :last-frame-start-time (get-time))
      (view/render off-screen-canvas-el off-screen-ctx on-screen-ctx @world-atom)
      (js/setTimeout
        #(update-loop world-atom)
        (* ideal-update-frame-time (:delta @world-atom))))

    (do
      (swap! past-atom conj @world-atom)

      (if (keyboard/just-down? (:save bindings))
        (reset! save-point-atom @world-atom))

      (if-let [save-point @save-point-atom]
        (if (keyboard/just-down? (:restore bindings))
          (let [adjusted-save-point (assoc save-point :last-frame-start-time (- (get-time) ideal-update-frame-time))]
            (reset! world-atom adjusted-save-point))))

      (let [world @world-atom
            last-frame-start-time (:last-frame-start-time world)
            current-frame-start-time (get-time)
            time-since-last-frame (- current-frame-start-time last-frame-start-time)
            delta (/ time-since-last-frame ideal-update-frame-time)

            target-delta-scale (if (keyboard/held? (:bullet-time bindings)) 0.1 1)
            delta-scale (maths/move-towards-linear (:delta-scale world) target-delta-scale 0.05)]
        (swap! world-atom
               assoc
               :delta (* delta delta-scale)
               :delta-scale delta-scale)
        (swap! world-atom ces/run-systems systems)
        (keyboard/tick)
        (swap! world-atom
               assoc
               :last-frame-start-time current-frame-start-time
               :fps (/ 1000 time-since-last-frame))
        (view/render off-screen-canvas-el off-screen-ctx on-screen-ctx world)
        (let [frame-duration (- (get-time) current-frame-start-time)
              time-to-wait (max 0 (- ideal-update-frame-time frame-duration))]
          (js/setTimeout #(update-loop world-atom) time-to-wait))))))

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
  (swap! g-world-atom
         assoc
         :last-frame-start-time (get-time)
         :delta-scale 1)
  (update-loop g-world-atom))

(defn on-js-reload []
  (swap! g-world-atom update-in [:__figwheel_counter] inc))

(defonce _ (start))
(comment _ on-js-reload)