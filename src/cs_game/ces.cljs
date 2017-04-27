(ns cs-game.ces
  (:require [cs-game.util.expanded-lang :refer [map-values strict-empty?]]))

(defn key-for-system [{:keys [ces/filter-fn key]}]
  (if (keyword? filter-fn)
    (or key filter-fn)
    (or key (throw (js/Error "system must have a keyword as filter-fn or define a custom label")))))

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

(defn- add-entity-to-world [entity initial-world systems]
  (let [reusuable-indexes (:ces/reusable-indexes initial-world)
        [entity-index leftover-reusable-indexes] (if (strict-empty? reusuable-indexes)
                                                   [(count (:ces/entities initial-world)) reusuable-indexes]
                                                   [(peek reusuable-indexes) (pop reusuable-indexes)])
        indexed-entity (assoc entity :entity/id entity-index)
        system-keys (system-keys-for-entity entity systems)]
    (as-> initial-world w
          (assoc w :ces/reusable-indexes leftover-reusable-indexes)
          (update w :ces/entities assoc entity-index indexed-entity)
          (reduce
            (fn [world system-key]
              (update-in world [:ces/system->entity-indexes system-key] #(safe-conj-to-set %1 entity-index)))
            w
            system-keys))))

(defn- add-entities-to-world [entities world systems]
  (reduce
    (fn [world entity] (add-entity-to-world entity world systems))
    world
    entities))

(defn add-entity-before-render [entity world]
  (update world :ces/add-before-render conj entity))

(defn add-entities-before-render [entities world]
  (update world :ces/add-before-render #(apply conj % entities)))

(defn add-entity-after-render [entity world]
  (update world :ces/add-after-render conj entity))

(defn add-entities-after-render [entities world]
  (update world :ces/add-after-render #(apply conj % entities)))

(defn remove-entity-before-render [entity-index world]
  (update world :ces/remove-before-render conj entity-index))

(defn remove-entities-before-render [entity-indexes world]
  (update world :ces/remove-before-render #(apply conj % entity-indexes)))

(defn remove-entity-after-render [entity-index world]
  (update world :ces/remove-after-render conj entity-index))

(defn remove-entities-after-render [entity-indexes world]
  (update world :ces/remove-after-render #(apply conj % entity-indexes)))

(defn- remove-entity [entity-index world]
  (as-> world w
        (update w :ces/entities assoc entity-index nil)
        (update
          w
          :ces/system->entity-indexes
          (fn [system->entity-indexes] (map-values #(disj % entity-index) system->entity-indexes)))
        (update w :ces/reusable-indexes conj entity-index)))

(defn- remove-entities [entity-indexes initial-world]
  (reduce
    (fn [world entity-index]
      (remove-entity entity-index world))
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
        (remove-entities (:ces/remove-after-render w) w)
        (assoc w :ces/remove-after-render #{})
        (add-entities-to-world (:ces/add-after-render w) w systems)
        (assoc w :ces/add-after-render [])
        (reduce run-system w systems)
        (remove-entities (:ces/remove-before-render w) w)
        (assoc w :ces/remove-before-render #{})
        (add-entities-to-world (:ces/add-before-render w) w systems)
        (assoc w :ces/add-before-render [])))

(def blank-world
  {:ces/entities []
   :ces/reusable-indexes []
   :ces/system->entity-indexes {}
   :ces/remove-before-render #{}
   :ces/remove-after-render #{}
   :ces/add-before-render []
   :ces/add-after-render []})