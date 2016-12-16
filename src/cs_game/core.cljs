(ns cs-game.core
  (:require [cs-game.ces :as ces]
            [cs-game.keyboard :as keyboard]
            [cs-game.canvas :as canvas]
            [cs-game.maths :as maths]
            [cs-game.collisions :as collisions])
  (:use [cs-game.expanded-lang :only [get-window-dimensions
                                      get-time]])
  (:require-macros [cs-game.canvas :as canvas]))

(enable-console-print!)

(def player1-bindings {:left 65
                       :right 68
                       :up 87
                       :down 83
                       :shoot 32})

(def player2-bindings {:left 37
                       :right 39
                       :up 38
                       :down 40
                       :shoot 13})

(declare .requestAnimationFrame)

(def initial-world-dimensions (get-window-dimensions))

(let [[initial-world-width initial-world-height] initial-world-dimensions]
  (def initial-player1
    {:position [(* initial-world-width 0.34) (* initial-world-height 0.5)]
     :velocity [0 0]
     :rotation 0
     :size 30
     :health 100
     :view :player
     :key-bindings player1-bindings
     :collision :player
     :wrap true})
  (def initial-player2
    {:position [(* initial-world-width 0.67) (* initial-world-height 0.5)]
     :velocity [0 0]
     :rotation 0
     :size 30
     :health 100
     :view :player
     :key-bindings player2-bindings
     :collision :player
     :wrap true})
  (def initial-asteroids
    (mapv
      (fn [_]
        {:position [(rand initial-world-width) (rand initial-world-height)]
         :velocity [(- (rand 2) 1) (- (rand 2) 1)]
         :size (+ 10 (rand 80))
         :health 100
         :view :asteroid
         :collision :asteroid
         :wrap true})
      (range 200))))

(def initial-collision-groups #{[:player :player] [:player :asteroid] [:asteroid :laser]})

(def initial-world
  (reduce
    (fn [world entity] (ces/add-entity-to-world entity world))
    (merge
      ces/blank-world
      {:collision-groups initial-collision-groups
       :dimensions initial-world-dimensions})
    (conj initial-asteroids initial-player1 initial-player2)))

(defonce g-world-atom (atom initial-world))

(def canvas-el (.getElementById js/document "app-canvas"))
(canvas/set-size canvas-el initial-world-dimensions)
(def g-ctx (canvas/context canvas-el))

(defn delta-vector [world v]
  (let [delta (:delta world)]
    (maths/vec* [delta delta] v)))

(defn accelerate-forwards [entity acceleration-magnitude]
  (let [rotation (maths/degrees-to-radians (:rotation entity))
        acceleration [(* acceleration-magnitude (Math/cos rotation))
                      (* acceleration-magnitude (Math/sin rotation))]]
    (update entity :velocity maths/vec+ acceleration)))

(defn keyboard-move [entity world]
  (let [rotate-magnitude 10
        accel-magnitude 0.2
        key-bindings (:key-bindings entity)]
    (as-> entity e
          (if (keyboard/held? (:up key-bindings)) (accelerate-forwards entity (* (:delta world) accel-magnitude)) e)
          (if (keyboard/held? (:left key-bindings)) (update e :rotation - (* (:delta world) rotate-magnitude)) e)
          (if (keyboard/held? (:right key-bindings)) (update e :rotation + (* (:delta world) rotate-magnitude)) e))))

(defn create-laser-at-entity [entity]
  (let [rotation (maths/degrees-to-radians (:rotation entity))
        laser-speed 20
        velocity [(* laser-speed (Math/cos rotation)) (* laser-speed (Math/sin rotation))]]
    {:position (:position entity)
     :velocity velocity
     :rotation (:rotation entity)
     :size 20
     :view :laser
     :remove-off-screen true}))

(defn keyboard-shoot [entity world]
  (if (keyboard/just-down? (-> entity :key-bindings :shoot))
    [entity (ces/add-entity-to-world (create-laser-at-entity entity) world)]
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

(defmethod collisions/detect-between [:player :player] [left-player right-player world]
  [left-player right-player world])

(defmethod collisions/detect-between [:asteroid :laser] [asteroid laser world]
  [asteroid laser world])

(defmethod collisions/detect-between [:player :asteroid] [player asteroid world]
  (let [[dx dy] (maths/vec- (:position player) (:position asteroid))
        dist-sq (+ (* dx dx) (* dy dy))
        min-dist (+ (/ (:size player) 2) (/ (:size asteroid) 2))
        min-dist-sq (* min-dist min-dist)
        colliding? (< dist-sq min-dist-sq)
        updated-asteroid (if colliding? (assoc asteroid :remove true) asteroid)]
    [player updated-asteroid world]))

(def update-systems
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

   {:filter-fn :collision
    :multiple-entity-system? true
    :system-fn collisions/system}

   {:filter-fn :remove
    :multiple-entity-system? true
    :system-fn ces/remove-entities}])

(def fps 30)
(def ideal-frame-time (/ 1000 fps))

(defn update-loop [world-atom]
  (let [world @world-atom
        last-tick-time (:last-update-tick world)
        current-time (get-time)
        frame-time (- current-time last-tick-time)
        delta (/ frame-time ideal-frame-time)]
    (swap! world-atom assoc :delta delta)
    (keyboard/tick)
    (swap! world-atom ces/run-systems update-systems)
    (swap! world-atom assoc :last-update-tick current-time))
  (js/setTimeout #(update-loop world-atom) ideal-frame-time))

(defmulti draw (fn [_ entity _] (:view entity)))

(defmethod draw :laser [ctx laser _]
  (canvas/state [c ctx]
    (canvas/translate c (:position laser))
    (canvas/rotate c (maths/degrees-to-radians (:rotation laser)))
    (canvas/fill-style c "red")
    (let [size (:size laser)]
      (canvas/fill-centered-rect c [0 0] [size (/ size 4)]))))

(defmethod draw :player [ctx player _]
  (canvas/state [c ctx]
    (canvas/translate c (:position player))
    (canvas/rotate c (maths/degrees-to-radians (:rotation player)))
    (canvas/fill-style c "grey")
    (let [size (:size player)]
      (canvas/begin-path c)
      (canvas/move-to c [(* size 0.7) 0])
      (canvas/line-to c [(- (* size 0.3)) (* size 0.4)])
      (canvas/line-to c [(- (* size 0.3)) (- (* size 0.4))])
      (canvas/fill c))))

(defmethod draw :asteroid [ctx asteroid _]
  (canvas/state [c ctx]
    (canvas/translate c (:position asteroid))
    (canvas/rotate c (maths/degrees-to-radians (:rotation asteroid)))
    (canvas/fill-style c "saddlebrown")
    (canvas/begin-path c)
    (let [radius (/ (:size asteroid) 2)]
      (canvas/centered-circle c [0 0] radius))
    (canvas/fill c)))

(defn clear-screen [ctx {:keys [dimensions]}]
  (canvas/fill-style ctx "black")
  (canvas/fill-rect ctx [0 0] dimensions))

(defn render-loop [ctx world-atom]
  (let [world @world-atom]
    (clear-screen ctx world)
    (doseq [entity (filter :view (:entities world))]
      (draw ctx entity world))
    (let [current-time (get-time)
          last-time (:last-render-tick world)
          frame-duration (- current-time last-time)
          true-fps (/ 1000 frame-duration)]))
  (swap! world-atom assoc :last-render-tick (get-time))
  (.requestAnimationFrame js/window #(render-loop ctx world-atom)))

(defn on-resize []
  (let [world-dimensions (get-window-dimensions)]
    (swap! g-world-atom assoc :dimensions world-dimensions)
    (canvas/set-size canvas-el world-dimensions)))

(defn start []
  (.addEventListener js/window "resize" on-resize)
  (keyboard/add-listeners)
  (swap! g-world-atom assoc :last-update-tick (get-time))
  (swap! g-world-atom assoc :last-render-tick (get-time))
  (render-loop g-ctx g-world-atom)
  (update-loop g-world-atom))

(defn on-js-reload []
    (swap! g-world-atom update-in [:__figwheel_counter] inc))

(defonce _ (start))