(ns cs-game.collisions)

(defmulti detect-between
          (fn [left-entity right-entity _]
            [(:collision left-entity)
             (:collision right-entity)]))

(defn detect-between-indexes [world left-entity-index right-entity-index]
  (let [entities (:entities world)
        left-entity (nth entities left-entity-index)
        right-entity (nth entities right-entity-index)
        [updated-left-entity
         updated-right-entity
         updated-world] (detect-between left-entity right-entity world)]
    (as-> updated-world w
          (update w :entities assoc left-entity-index updated-left-entity)
          (update w :entities assoc right-entity-index updated-right-entity))))

(defn collision-check-necessary? [left-entity-index right-entity-index collision-index-pairs]
  (not
    (or
      (= left-entity-index right-entity-index)
      (contains? collision-index-pairs #{left-entity-index right-entity-index}))))

(defn detect-for-group [initial-world [left-label right-label] entity-indexes-by-label]
  (let [left-entity-indexes (get entity-indexes-by-label left-label)
        right-entity-indexes (get entity-indexes-by-label right-label)]
    (if (or (empty? left-entity-indexes) (empty? right-entity-indexes))
      initial-world
      (let [initial-collision-index-pairs #{}
            [updated-world] (reduce
                              (fn [[world collision-index-pairs] left-entity-index]
                                (reduce
                                  (fn [[world collision-index-pairs] right-entity-index]
                                    (let [check-collision? (collision-check-necessary? left-entity-index
                                                                                       right-entity-index
                                                                                       collision-index-pairs)
                                          updated-world (if check-collision?
                                                          (detect-between-indexes world
                                                                                  left-entity-index
                                                                                  right-entity-index)
                                                          world)
                                          updated-collision-index-pairs (conj collision-index-pairs #{left-entity-index right-entity-index})]
                                      [updated-world updated-collision-index-pairs]))
                                  [world collision-index-pairs]
                                  right-entity-indexes))
                              [initial-world initial-collision-index-pairs]
                              left-entity-indexes)]
        updated-world))))

; <10% optimisation available: cache entity-index-by-label on add/remove by wrapping add/remove ces functions
(defn system [entity-indexes world]
  (let [entities (:entities world)
        entity-indexes-by-label (group-by #(:collision (nth entities %)) entity-indexes)]
    (reduce
      #(detect-for-group %1 %2 entity-indexes-by-label)
      world
      (:collision-groups world))))