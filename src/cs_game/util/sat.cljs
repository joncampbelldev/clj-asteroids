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
  (let [polygon (Polygon. (to-vector position) (into-array (mapv to-vector points)))]
    (.setAngle polygon rotation)
    polygon))

(def reusable-response (new Response))

(defn test-polygon-polygon [polygon1 polygon2]
  (let [colliding? (.testPolygonPolygon js/SAT polygon1 polygon2 reusable-response)]
    (if colliding?
      (let [response {:overlap (from-vector (.-overlapV reusable-response))}]
        (.clear reusable-response)
        response)
      nil)))