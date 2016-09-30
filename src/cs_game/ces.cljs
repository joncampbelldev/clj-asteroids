(ns cs-game.ces
  (:use [cs-game.expanded-lang :only [concatv]]))

(defn normalise-system-fn-call [system-fn entity world]
  (let [result (system-fn entity world)]
    (cond
      (vector? result) result
      (map? result) [result world])))

(defn run-single-entity-system [system-fn entity-indexes initial-world]
  (reduce
    (fn [world entity-index]
      (let [entities (:entities world)
            entity (nth entities entity-index)
            [updated-entity updated-world] (normalise-system-fn-call system-fn entity world)]
        (update updated-world :entities assoc (:id updated-entity) updated-entity)))
    initial-world
    entity-indexes))

;optimisation available: cache entity-indexes-for-system on add/remove
(defn run-system [{:keys [filter-fn system-fn multiple-entity-system?]} world]
  (let [entity-indexes-for-system (as-> (:entities world) _
                                        (filter filter-fn _)
                                        (map :id _)
                                        (into #{} _))]
    (if multiple-entity-system?
      (system-fn entity-indexes-for-system world)
      (run-single-entity-system system-fn entity-indexes-for-system world))))

(defn run-systems [world systems]
  (reduce
    (fn [w s] (run-system s w))
    world
    systems))

(defn add-entity-to-world [entity world]
  (let [reusable-ids (:reusable-ids world)
        [id leftover-reusable-ids] (if (empty? reusable-ids)
                                     [(count (:entities world)) reusable-ids]
                                     [(peek reusable-ids) (pop reusable-ids)])
        entity-with-id (assoc entity :id id)]
    (as-> world w
          (assoc w :reusable-ids leftover-reusable-ids)
          (update w :entities assoc id entity-with-id))))

(defn remove-entities [entity-indexes initial-world]
  (as-> initial-world w
        (reduce
          (fn [world entity-index] (update world :entities assoc entity-index nil))
          w
          entity-indexes)
        (update w :reusable-ids concatv entity-indexes)))

(def blank-world
  {:entities []
   :reusable-ids []})