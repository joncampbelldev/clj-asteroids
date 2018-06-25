(ns cs-game.collisions
  (:require [cs-game.util.expanded-lang :refer [group-by-transform strict-empty?]]
            [cs-game.spatial-hashing :as spatial-hashing]
            [cs-game.util.sat :as sat]
            [cs-game.util.maths :as maths]))

(defmulti collision-between
          (fn [left-entity right-entity _ _]
            [(:entity/collision left-entity)
             (:entity/collision right-entity)]))

(defn narrow-phase-detect [entity1 entity2]
  (let [polygon1 (sat/to-polygon (:position entity1)
                                 (:entity/collision-polygon-points entity1)
                                 (maths/degrees-to-radians (:rotation entity1)))
        polygon2 (sat/to-polygon (:position entity2)
                                 (:entity/collision-polygon-points entity2)
                                 (maths/degrees-to-radians (:rotation entity2)))]
    (sat/test-polygon-polygon polygon1 polygon2)))

(defn mid-phase-colliding? [entity1 entity2]
  (let [[dx dy] (maths/v- (:position entity1) (:position entity2))
        dist-sq (+ (* dx dx) (* dy dy))
        min-dist (+ (/ (:size entity1) 2) (/ (:size entity2) 2))
        min-dist-sq (* min-dist min-dist)]
    (< dist-sq min-dist-sq)))

(defn detect-between-indexes [world left-entity right-entity]
  (let [collision-response (and (mid-phase-colliding? left-entity right-entity)
                                (narrow-phase-detect left-entity right-entity))
        [updated-left-entity
         updated-right-entity
         updated-world] (if collision-response
                          (collision-between left-entity right-entity collision-response world)
                          [left-entity right-entity world])]
    (update updated-world
            :ces/entities
            assoc
            (:entity/id left-entity) updated-left-entity
            (:entity/id right-entity) updated-right-entity)))

(defn check-for-entity [entity-index initial-world initial-collided-index-pairs]
  (let [entities (:ces/entities initial-world)
        entity (nth entities entity-index)
        nearby-entity-indexes (spatial-hashing/nearby-entity-indexes (:entity-spatial-hash initial-world) entity)]
    (reduce
      (fn [[world collided-index-pairs] other-entity-index]
        (let [left-entity (nth entities entity-index)
              right-entity (nth entities other-entity-index)

              ; TODO dealing with legacy, rethink collision system entirely
              collision-pairs (:collision-pairs world)
              type-pair1 [(:entity/collision left-entity) (:entity/collision right-entity)]
              type-pair2 [(:entity/collision right-entity) (:entity/collision left-entity)]
              is-pair1? (contains? collision-pairs type-pair1)
              is-pair2? (contains? collision-pairs type-pair2)
              correct-types? (or is-pair1? is-pair2?)
              [left-entity right-entity] (if is-pair2? [right-entity left-entity] [left-entity right-entity])

              same-entity? (= entity-index other-entity-index)
              already-collided? (contains? collided-index-pairs #{entity-index other-entity-index})

              updated-world (if (and (not same-entity?)
                                     (not already-collided?)
                                     correct-types?)
                              (detect-between-indexes world
                                                      left-entity
                                                      right-entity)
                              world)
              updated-collided-index-pairs (conj collided-index-pairs [entity-index other-entity-index])]
          [updated-world updated-collided-index-pairs]))
      [initial-world initial-collided-index-pairs]
      nearby-entity-indexes)))

(defn system [collidable-entity-indexes world]
  (let [[updated-world _] (reduce
                            (fn [[world collided-index-pairs] entity-index]
                              (check-for-entity entity-index world collided-index-pairs))
                            [world #{}]
                            collidable-entity-indexes)]
    updated-world))