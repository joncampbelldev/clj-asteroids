(ns cs-game.collisions
  (:require [cs-game.expanded-lang :refer [group-by-transform strict-empty?]]
            [cs-game.spatial-hashing :as spatial-hashing]
            [cs-game.util.sat :as sat]
            [cs-game.util.maths :as maths]))

(def reusable-response (new sat/Response))

(defmulti collision-between
  (fn [left-entity right-entity _ _]
    [(:type left-entity)
     (:type right-entity)]))

(defn narrow-phase-detect [entity1 entity2 response]
  (let [polygon1 (sat/to-polygon (:position entity1)
                                 (:points entity1)
                                 (maths/degrees-to-radians (:rotation entity1)))
        polygon2 (sat/to-polygon (:position entity2)
                                 (:points entity2)
                                 (maths/degrees-to-radians (:rotation entity2)))]
    (sat/test-polygon-polygon polygon1 polygon2 response)))

(defn mid-phase-colliding? [entity1 entity2]
  (let [[dx dy] (maths/v- (:position entity1) (:position entity2))
        dist-sq (+ (* dx dx) (* dy dy))
        min-dist (+ (/ (:size entity1) 2) (/ (:size entity2) 2))
        min-dist-sq (* min-dist min-dist)]
    (< dist-sq min-dist-sq)))

(defn detect-between-indexes [world left-entity-index right-entity-index]
  (let [entities (:entities world)
        left-entity (nth entities left-entity-index)
        right-entity (nth entities right-entity-index)
        collision-response (and (mid-phase-colliding? left-entity right-entity)
                                (narrow-phase-detect left-entity right-entity reusable-response))
        [updated-left-entity
         updated-right-entity
         updated-world] (if collision-response
                          (collision-between left-entity right-entity collision-response world)
                          [left-entity right-entity world])]
    (.clear reusable-response)
    (as-> updated-world w
          (update w :entities assoc left-entity-index updated-left-entity)
          (update w :entities assoc right-entity-index updated-right-entity))))

(defn collision-check-necessary? [left-entity-index right-entity-index collision-index-pairs]
  (not
    (or
      (= left-entity-index right-entity-index)
      (contains? collision-index-pairs #{left-entity-index right-entity-index}))))

(defn detect-for-group [initial-world [left-label right-label] label->entity-indexes label->spatial-hash]
  (let [left-entity-indexes (get label->entity-indexes left-label)
        right-entity-indexes (get label->entity-indexes left-label)
        right-spatial-hash (get label->spatial-hash right-label)

        swap? (> (count left-entity-indexes) (count right-entity-indexes))
        [left-entity-indexes right-entity-indexes] (if swap?
                                                     [right-entity-indexes left-entity-indexes]
                                                     [left-entity-indexes right-entity-indexes])
        initial-entities (:entities initial-world)]
    (if (or (strict-empty? left-entity-indexes) (strict-empty? right-entity-indexes))
      initial-world
      (let [initial-collision-index-pairs #{}
            [updated-world]
            (reduce
              (fn [[world collision-index-pairs] left-entity-index]
                (let [initial-left-entity (nth initial-entities left-entity-index)
                      nearby-right-entity-indexes (spatial-hashing/nearby-entity-indexes right-spatial-hash initial-left-entity)]
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
                    nearby-right-entity-indexes)))
              [initial-world initial-collision-index-pairs]
              left-entity-indexes)]
        updated-world))))

(def group-ids-by-type (partial group-by-transform :type :id []))

; small optimisation available: cache entity-index-by-label on add/remove by wrapping add/remove ces functions
(defn system [collidable-entity-indexes world]
  (let [all-entities (:entities world)
        collidable-entities (mapv #(nth all-entities %) collidable-entity-indexes)
        label->entity-indexes (group-ids-by-type collidable-entities)
        label->spatial-hash (reduce-kv
                              (fn [label->sh label entity-indexes-for-label]
                                (let [entities-with-label (mapv #(nth all-entities %) entity-indexes-for-label)
                                      spatial-hash (spatial-hashing/build entities-with-label (:spatial-hash-config world))]
                                  (assoc label->sh label spatial-hash)))
                              {}
                              label->entity-indexes)]
    (reduce
      #(detect-for-group %1 %2 label->entity-indexes label->spatial-hash)
      world
      (:collision-groups world))))