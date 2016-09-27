(ns cs-game.ces)

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

(defn run-system [{:keys [filter-fn system-fn multiple-entity-system?]} world]
  (let [entity-indexes-for-system (as-> (:entities world) es
                                        (filterv filter-fn es)
                                        (mapv :id es))]
    (if multiple-entity-system?
      (system-fn entity-indexes-for-system world)
      (run-single-entity-system system-fn entity-indexes-for-system world))))

(defn run-systems [world systems]
  (reduce
    (fn [w s] (run-system s w))
    world
    systems))