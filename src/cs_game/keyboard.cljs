(ns cs-game.keyboard)

(def held (atom #{}))
(def prev-held (atom @held))

(defn held? [code]
  (contains? @held code))

(defn prev-held? [code]
  (contains? @prev-held code))

(defn just-up? [code]
  (and
    (not (contains? @held code))
    (contains? @prev-held code)))

(defn just-down? [code]
  (and
    (contains? @held code)
    (not (contains? @prev-held code))))

(defn- handle-keydown [e]
  (swap! held conj (.-keyCode e)))

(defn- handle-keyup [e]
  (swap! held disj (.-keyCode e)))

(defn add-listeners []
  (.addEventListener js/window "keydown" handle-keydown)
  (.addEventListener js/window "keyup" handle-keyup))

(defn remove-listeners []
  (.removeEventListener js/window "keydown" handle-keydown)
  (.removeEventListener js/window "keyup" handle-keyup))

(defn tick []
  (reset! prev-held @held))