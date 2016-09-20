(ns cs-game.ces)

(defn normalise-system-fn-call [system-fn entity world]
  (let [result (system-fn entity world)]
    (cond
      (vector? result) result
      (map? result) [result world]
      (nil? result) [entity world])))

(defn run-single-entity-system [system-fn entities world]
  (reduce
    (fn [[entities world] entity]
      (let [[updated-entity updated-world] (normalise-system-fn-call system-fn entity world)]
        [(conj entities updated-entity) updated-world]))
    [[] world]
    entities))

(defn run-system [{:keys [filter-fn system-fn multiple-entity-system?]} world]
  (let [entities-by-id (:entity-by-id world)
        entities-for-system (filterv filter-fn (vals entities-by-id))
        [updated-entities updated-world] (if multiple-entity-system?
                                           (system-fn entities-for-system world)
                                           (run-single-entity-system system-fn entities-for-system world))]
    (update updated-world :entity-by-id
            (fn [updated-entities-by-id]
              (reduce
                #(assoc %1 (:id %2) %2)
                updated-entities-by-id
                updated-entities)))))

(defn run-systems [world systems]
  (reduce
    (fn [w s] (run-system s w))
    world
    systems))