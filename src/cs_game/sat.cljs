(ns cs-game.sat)

(declare .-Vector .-Polygon .-Circle .testPolygonPolygon .testPolygonCircle)

(def Vector (.-Vector js/SAT))
(def Polygon (.-Polygon js/SAT))
(def Circle (.-Circle js/SAT))

(defn to-vector [[x y]]
  (Vector. x y))

(defn to-polygon [position points rotation]
  (let [polygon (Polygon. (to-vector position) (into-array (map to-vector points)))]
    (.setAngle polygon rotation)
    polygon))

(defn to-circle [position radius]
  (Circle. (to-vector position) radius))

(defn test-polygon-polygon [polygon1 polygon2]
  (.testPolygonPolygon js/SAT polygon1 polygon2))

(defn test-polygon-circle [polygon circle]
  (.testPolygonCircle js/SAT polygon circle))