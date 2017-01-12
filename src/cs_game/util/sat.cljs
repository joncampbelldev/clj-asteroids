(ns cs-game.util.sat)

(declare .-Vector .-Polygon .-Response .-x .-y .-overlapV .testPolygonPolygon .setAngle)

(def Vector (.-Vector js/SAT))
(def Polygon (.-Polygon js/SAT))
(def Response (.-Response js/SAT))

(defn- to-vector [[x y]]
  (Vector. x y))

(defn- from-vector [v]
  [(.-x v) (.-y v)])

(defn to-polygon [position points rotation]
  (let [polygon (Polygon. (to-vector position) (into-array (map to-vector points)))]
    (.setAngle polygon rotation)
    polygon))

(defn test-polygon-polygon [polygon1 polygon2]
  (let [response (new Response)
        colliding? (.testPolygonPolygon js/SAT polygon1 polygon2 response)]
    (if colliding?
      {:overlap (from-vector (.-overlapV response))}
      nil)))