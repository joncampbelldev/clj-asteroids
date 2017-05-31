(ns cs-game.ces
  (:require [cs-game.util.expanded-lang :refer [map-values strict-empty?]]))

(defn- safe-conj-to-set [set value]
  (if (nil? set)
    #{value}
    (conj set value)))

(defn- system-keys-for-entity [entity systems]
  (reduce
    (fn [system-keys {:keys [ces/filter-fn key]}]
      (if (filter-fn entity)
        (conj system-keys key)
        system-keys))
    #{}
    systems))

(defn- register-entity [entity world systems]
  (reduce
    (fn [world system-key]
      (update-in world [:ces/system->entity-indexes system-key] #(safe-conj-to-set %1 (:entity/id entity))))
    world
    (system-keys-for-entity entity systems)))

(defn- unregister-entity-index [entity-index world]
  (update world
          :ces/system->entity-indexes
          (fn [system->entity-indexes]
            (map-values #(disj % entity-index) system->entity-indexes))))

(defn- add-entity-to-world [entity initial-world systems]
  (let [reusuable-indexes (:ces/reusable-indexes initial-world)
        [entity-index leftover-reusable-indexes] (if (strict-empty? reusuable-indexes)
                                                   [(count (:ces/entities initial-world)) reusuable-indexes]
                                                   [(peek reusuable-indexes) (pop reusuable-indexes)])
        entity (assoc entity :entity/id entity-index)]
    (as-> initial-world w
          (assoc w :ces/reusable-indexes leftover-reusable-indexes)
          (update w :ces/entities assoc entity-index entity)
          (register-entity entity w systems))))

(defn- add-entities-to-world [entities world systems]
  (reduce
    (fn [world entity] (add-entity-to-world entity world systems))
    world
    entities))

(defn- remove-entity-from-world [entity-index world]
  (as-> world w
        (update w :ces/entities assoc entity-index nil)
        (unregister-entity-index entity-index w)
        (update
          w
          :ces/system->entity-indexes
          (fn [system->entity-indexes] (map-values #(disj % entity-index) system->entity-indexes)))
        (update w :ces/reusable-indexes conj entity-index)))

(defn- remove-entities-from-world [entity-indexes initial-world]
  (reduce
    (fn [world entity-index]
      (remove-entity-from-world entity-index world))
    initial-world
    entity-indexes))

(defn- normalise-system-fn-call [system-fn entity world]
  (let [result (system-fn entity world)]
    (cond
      (vector? result) result
      (map? result) [result world])))

(defn- run-single-entity-system [system-fn entity-indexes initial-world]
  (reduce
    (fn [world entity-index]
      (let [entities (:ces/entities world)
            entity (nth entities entity-index)
            [updated-entity updated-world] (normalise-system-fn-call system-fn entity world)]
        (update updated-world :ces/entities assoc (:entity/id updated-entity) updated-entity)))
    initial-world
    entity-indexes))

(defn- run-system [world {:keys [key ces/system-fn ces/multiple-entity-system?]}]
  (let [entity-indexes-for-system (-> world :ces/system->entity-indexes key)]
    (if (strict-empty? entity-indexes-for-system)
      world
      (if multiple-entity-system?
        (system-fn entity-indexes-for-system world)
        (run-single-entity-system system-fn entity-indexes-for-system world)))))

(defn run-systems [world systems]
  (as-> world w
        (remove-entities-from-world (:ces/entity-indexes-to-remove w) w)
        (assoc w :ces/entity-indexes-to-remove #{})
        (reduce run-system w systems)
        (add-entities-to-world (:ces/entity-indexes-to-add w) w systems)
        (assoc w :ces/entity-indexes-to-add [])))

(defn add-entity [entity world]
  (update world :ces/entity-indexes-to-add conj entity))

(defn add-entities [entities world]
  (update world :ces/entity-indexes-to-add #(apply conj % entities)))

(defn remove-entity [entity-index world]
  (update world :ces/entity-indexes-to-remove conj entity-index))

(defn remove-entities [entity-indexes world]
  (update world :ces/entity-indexes-to-remove conj (apply conj % entity-indexes)))

(defn re-register-entity [entity world systems]
  (as-> world w
        (unregister-entity-index (:entity/id entity) w)
        (register-entity entity w systems)))

(defn re-register-entity-index [index world systems]
  (let [entity (-> world :ces/entities (nth index))]
    (re-register-entity entity world systems)))

(defn key-for-system [{:keys [ces/filter-fn key]}]
  (if (keyword? filter-fn)
    (or key filter-fn)
    (or key (throw (js/Error "system must have a keyword as filter-fn or define a custom label")))))

(def blank-world
  {:ces/entities []
   :ces/reusable-indexes []
   :ces/system->entity-indexes {}
   :ces/entity-indexes-to-remove #{}
   :ces/entity-indexes-to-add []})