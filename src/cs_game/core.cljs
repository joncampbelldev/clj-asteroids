(ns cs-game.core
  (:require [cs-game.ces :as ces]
            [cs-game.keyboard :as keyboard]
            [cs-game.canvas :as canvas]
            [cs-game.maths :as maths])
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

;declare some js globals because cursive doesn't know about them
(declare .-innerWidth .-innerHeight .requestAnimationFrame)

(defn get-window-dimensions []
  [(.-innerWidth js/window) (.-innerHeight js/window)])

(def initial-world-dimensions (get-window-dimensions))

(let [id (atom 0)]
  (defn create-entity [component-map]
    (swap! id inc)
    (assoc component-map :id @id)))

(defn add-entity-to-world [entity world]
  (update world :entity-by-id assoc (:id entity) entity))

(let [[initial-world-width initial-world-height] initial-world-dimensions]
  (def initial-player1
    (create-entity
      {:position [(* initial-world-width 0.34) (* initial-world-height 0.5)]
       :velocity [0 0]
       :rotation 0
       :size 30
       :view :player
       :key-bindings player1-bindings
       :collision {:label :player}
       :wrap true}))
  (def initial-player2
    (create-entity
      {:position [(* initial-world-width 0.67) (* initial-world-height 0.5)]
       :velocity [0 0]
       :rotation 0
       :size 30
       :view :player
       :key-bindings player2-bindings
       :collision {:label :player}
       :wrap true}))

  (def initial-asteroids
    (mapv
      (fn [_]
        (create-entity
          {:position [(rand initial-world-width) (rand initial-world-height)]
           :velocity [(- (rand 2) 1) (- (rand 2) 1)]
           :size (+ 10 (rand 80))
           :view :asteroid
           :collision {:label :asteroid}
           :wrap true}))
      (range 20))))

(def collision-groups #{[:player :player] [:player :asteroid] [:asteroid :laser]})

(defn group-by-transform [key-fn transform-fn coll]
  (reduce
    (fn [m e]
      (let [k (key-fn e)
            v (transform-fn e)]
        (assoc m k (conj (get m k []) v))))
    {}
    coll))

(defn index-by [key-fn coll]
  (reduce
    (fn [m e] (assoc m (key-fn e) e))
    {}
    coll))

(def initial-entities (apply conj initial-asteroids [initial-player1 initial-player2]))
(def initial-entity-by-id (index-by :id initial-entities))

(def initial-world
  {:entity-by-id initial-entity-by-id
   :dimensions initial-world-dimensions})

(defonce g-world-atom (atom initial-world))

(def canvas-el (.getElementById js/document "app-canvas"))
(canvas/set-size canvas-el initial-world-dimensions)
(def g-ctx (canvas/context canvas-el))

(defn delta-vector [world v]
  (let [delta (:delta world)]
    (maths/vec* [delta delta] v)))

(defn delta-scalar [world s]
  (* (:delta world) s))

(defn keyboard-move [entity world]
  (let [rotation (maths/degrees-to-radians (mod (:rotation entity) 360))
        accel-x (delta-scalar world (* 0.1 (Math/cos rotation)))
        accel-y (delta-scalar world (* 0.1 (Math/sin rotation)))
        rotate-speed (delta-scalar world 5)
        key-bindings (:key-bindings entity)]
    (as-> entity e
          (if (keyboard/held? (:up key-bindings)) (update e :velocity maths/vec+ [accel-x accel-y]) e)
          (if (keyboard/held? (:left key-bindings)) (update e :rotation - rotate-speed) e)
          (if (keyboard/held? (:right key-bindings)) (update e :rotation + rotate-speed) e))))

(defn create-laser-at-entity [entity]
  (let [rotation (maths/degrees-to-radians (mod (:rotation entity) 360))
        velocity [(* 6 (Math/cos rotation)) (* 6 (Math/sin rotation))]]
    (create-entity {:position (:position entity)
                    :velocity velocity
                    :rotation (:rotation entity)
                    :size 20
                    :view :laser
                    :remove-off-screen true})))

(defn keyboard-shoot [entity world]
  (let [key-bindings (:key-bindings entity)]
    (if (keyboard/held? (:shoot key-bindings))
      [entity (add-entity-to-world (create-laser-at-entity entity) world)])))

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
  (update entity :rotation + (delta-scalar world (:rotate-speed entity))))

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

(defn remove-entities [entities world]
  [[]
   (reduce
     (fn [world entity] (update world :entity-by-id dissoc (:id entity)))
     world
     entities)])

(defn min-distance [left-entity right-entity]
  (+ (/ (:size left-entity) 2) (/ (:size right-entity) 2)))

(defn hypotenuse [a b] (Math/sqrt (+ (* a a) (* b b))))

(defn circle-collision-response [left-entity right-entity]
  (let [left-position (:position left-entity)
        right-position (:position right-entity)
        [mid-x mid-y] (maths/vector-divide (maths/vec+ left-position right-position) [2 2])
        [x-distance y-distance] (maths/vec- left-position right-position)
        distance (hypotenuse x-distance y-distance)
        overlap (- distance (min-distance left-entity right-entity))
        colliding? (pos? overlap)]
    (if colliding?
      [left-entity right-entity]
      [left-entity right-entity])))

(defmulti detect-collision-between
          (fn [left-entity right-entity world]
            [(get-in left-entity [:collision :label])
             (get-in right-entity [:collision :label])]))

(defmethod detect-collision-between [:player :player] [left-player right-player world]
  [left-player right-player world])

(defmethod detect-collision-between [:asteroid :laser] [asteroid laser world]
  [asteroid laser world])

(defmethod detect-collision-between [:player :asteroid] [player asteroid world]
  [player asteroid world])

(defn detect-collision-if-necessary [[entity-by-id world collision-id-pairs] left-entity-id right-entity-id]
  (let [left-entity (entity-by-id left-entity-id)
        right-entity (entity-by-id right-entity-id)
        id-pair #{left-entity-id right-entity-id}
        should-collide? (not (or
                               (= left-entity-id right-entity-id)
                               (contains? collision-id-pairs id-pair)))
        [updated-left-entity
         updated-right-entity
         updated-world] (if should-collide?
                          (detect-collision-between left-entity right-entity world)
                          [left-entity right-entity world])
        updated-entity-by-id (as-> entity-by-id _
                                   (assoc _ left-entity-id updated-left-entity)
                                   (assoc _ right-entity-id updated-right-entity))
        updated-collision-id-pairs (conj collision-id-pairs id-pair)]
    [updated-entity-by-id updated-world updated-collision-id-pairs]))

(defn detect-collisions-in-group [[entity-by-id world] [left-label right-label] entity-ids-by-label]
  (let [left-entity-ids (entity-ids-by-label left-label)
        right-entity-ids (entity-ids-by-label right-label)
        collision-id-pairs #{}]
    (reduce
      (fn [[entity-by-id world collision-id-pairs] left-entity-id]
        (reduce
          #(detect-collision-if-necessary %1 left-entity-id %2)
          [entity-by-id world collision-id-pairs]
          right-entity-ids))
      [entity-by-id world collision-id-pairs]
      left-entity-ids)))

(defn collision-system [entities world]
  (let [initial-entity-by-id (index-by :id entities)
        entity-ids-by-label (group-by-transform #(get-in % [:collision :label]) :id entities)
        [updated-entity-by-id
         updated-world] (reduce
                          #(detect-collisions-in-group %1 %2 entity-ids-by-label)
                          [initial-entity-by-id world]
                          collision-groups)]
    [(vals updated-entity-by-id) updated-world]))

(defmulti draw (fn [ctx entity world] (:view entity)))

(defmethod draw :laser [ctx laser _]
  (canvas/state ctx
                (canvas/translate ctx (:position laser))
                (canvas/rotate ctx (maths/degrees-to-radians (:rotation laser)))
                (canvas/fill-style ctx "red")
                (let [size (:size laser)]
                  (canvas/fill-centered-rect ctx [0 0] [size (/ size 4)]))))

(defmethod draw :player [ctx player _]
  (canvas/state ctx
                (canvas/translate ctx (:position player))
                (canvas/rotate ctx (maths/degrees-to-radians (:rotation player)))
                (canvas/fill-style ctx "grey")
                (let [size (:size player)]
                  (canvas/path :fill
                               (canvas/move-to ctx [(* size 0.7) 0])
                               (canvas/line-to ctx [(- (* size 0.3)) (* size 0.25)])
                               (canvas/line-to ctx [(- (* size 0.3)) (- (* size 0.25))])))))

(defmethod draw :asteroid [ctx asteroid _]
  (canvas/state ctx
                (canvas/translate ctx (:position asteroid))
                (canvas/rotate ctx (maths/degrees-to-radians (:rotation asteroid)))
                (canvas/fill-style ctx "saddlebrown")
                (canvas/begin-path ctx)
                (let [radius (/ (:size asteroid) 2)]
                  (canvas/centered-circle ctx [0 0] radius))
                (canvas/fill ctx)))

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
    :system-fn collision-system}

   {:filter-fn :remove
    :multiple-entity-system? true
    :system-fn remove-entities}])

(def update-fps 60)
(def ideal-frame-time (/ 1000 update-fps))
(defn get-time [] (.getTime (new js/Date)))

(defn update-loop [world-atom]
  (let [world @world-atom
        last-tick-time (:last-tick-time world)
        current-time (get-time)
        frame-time (- current-time last-tick-time)
        delta (/ frame-time ideal-frame-time)]
    (swap! world-atom assoc :delta delta)
    (swap! world-atom ces/run-systems update-systems)
    (keyboard/tick)
    (swap! world-atom assoc :last-tick-time current-time))
  (js/setTimeout #(update-loop world-atom) ideal-frame-time))

(defn clear-screen [ctx {:keys [dimensions]}]
  (canvas/fill-style ctx "black")
  (canvas/fill-rect ctx [0 0] dimensions))

(defn render-loop [ctx world-atom]
  (let [world @world-atom]
    (clear-screen ctx world)
    (doseq [entity (filter :view (vals (:entity-by-id world)))]
      (draw ctx entity world))
    (.requestAnimationFrame js/window #(render-loop ctx world-atom))))

(defn on-resize []
  (let [world-dimensions (get-window-dimensions)]
    (swap! g-world-atom assoc :dimensions world-dimensions)
    (canvas/set-size canvas-el world-dimensions)))

(defn start []
  (.addEventListener js/window "resize" on-resize)
  (keyboard/add-listeners)
  (swap! g-world-atom assoc :last-tick-time (get-time))
  (render-loop g-ctx g-world-atom)
  (update-loop g-world-atom))

(defn on-js-reload []
  (swap! g-world-atom update-in [:__figwheel_counter] inc))

(defonce _ (start))