(ns cs-game.spatial-hashing
  (:require [clojure.set :refer [union]]))

; TODO calculate optimal cell size based on world size and average entity size
(defn generate-config [world-width world-height leeway-outside-world cell-size]
  (let [columns (+ (* leeway-outside-world 2) (int (/ world-width cell-size)))
        rows (+ (* leeway-outside-world 2) (int (/ world-height cell-size)))]
    {:empty (vec (repeat (* columns rows) #{}))
     :cell-size cell-size
     :columns columns
     :rows rows
     :offset leeway-outside-world}))

(defn index-for-point [x y cells-per-row offset]
  (let [offset-x (+ offset x)
        offset-y (+ offset y)]
    (+ offset-x (* offset-y cells-per-row))))

(defn get-points-for-entity [entity cell-size]
  (let [[x y] (:position entity)
        [width height] (or (:dimensions entity) [(:size entity) (:size entity)])
        half-width (/ width 2)
        half-height (/ height 2)
        min-x (int (/ (- x half-width) cell-size))
        max-x (int (/ (+ x half-width) cell-size))
        min-y (int (/ (- y half-height) cell-size))
        max-y (int (/ (+ y half-height) cell-size))]
    (for [px (range min-x (inc max-x))
          py (range min-y (inc max-y))]
      [px py])))

(defn- add-entity-to-spatial-hash [initial-transient-spatial-hash entity cell-size columns offset]
  (let [id (:id entity)
        points (get-points-for-entity entity cell-size)]
    (reduce
      (fn [transient-spatial-hash [x y]]
        (let [index (index-for-point x y columns offset)]
          (assoc! transient-spatial-hash index (conj (get transient-spatial-hash index) id))))
      initial-transient-spatial-hash
      points)))

(defn build [entities {:keys [offset cell-size columns rows empty]}]
  (let [transient-spatial-hash (reduce
                                 #(add-entity-to-spatial-hash %1 %2 cell-size columns offset)
                                 (transient empty)
                                 entities)]
    {:data (persistent! transient-spatial-hash)
     :offset offset
     :cell-size cell-size
     :columns columns
     :rows rows}))

(defn nearby-entity-indexes [spatial-hash entity]
  (let [{:keys [data cell-size columns offset]} spatial-hash]
    (->> (get-points-for-entity entity cell-size)
         (mapv (fn [[x y]]
                 (get data (index-for-point x y columns offset))))
         (apply union))))