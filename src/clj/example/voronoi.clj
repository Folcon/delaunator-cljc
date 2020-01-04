(ns example.voronoi
  (:require [quil.core :as q]
            [quil.middleware :refer [fun-mode]]
            [delaunator-cljc.macros :refer [inline-resource]]
            [delaunator-cljc.delaunay :refer [delaunator]]))



(defn circumcenter [[ax ay] [bx by] [cx cy]]
  (let [ad (+ (* ax ax) (* ay ay))
        bd (+ (* bx bx) (* by by))
        cd (+ (* cx cx) (* cy cy))
        d  (* 2 (+ (* ax (- by cy)) (* bx (- cy ay)) (* cx (- ay by))))]
    [(* (/ 1 d) (+ (* ad (- by cy)) (* bd (- cy ay)) (* cd (- ay by))))
     (* (/ 1 d) (+ (* ad (- cx bx)) (* bd (- ax cx)) (* cd (- bx ax))))]))

(defn next-half-edge [edge-idx]
  (if (= (mod edge-idx 3) 2) (- edge-idx 2) (inc edge-idx)))

(defn unique-triangle-edges [points triangles half-edges]
  (remove nil?
    (map
      (comp (fn [[edge-idx tri he]]
              (when (> edge-idx he) ;; half edges are paired, so only draw one of each
                (let [next-idx (next-half-edge edge-idx)
                      opposite-idx (nth triangles next-idx)]
                  [(nth points tri) (nth points opposite-idx)])))
            vector)
      (range)
      triangles
      half-edges)))

(defn triangle-of-edge [edge-idx]
  (int (/ edge-idx 3)))

(defn triangle-xf [points]
  (comp (partition-all 3)
        (map (fn [[p1 p2 p3]] [(nth points p1) (nth points p2) (nth points p3)]))))

(def circumcenter-xf (map (comp (fn [[x y]] [(double x) (double y)]) #(apply circumcenter %))))

(defn unique-voronoi-edges [points triangles half-edges]
  (let [triangle-circumcenters (into [] (comp (triangle-xf points) circumcenter-xf) triangles)]
    (remove nil?
      (map
        (comp (fn [[edge-idx he]]
                (when (< edge-idx he) ;; half edges are paired, so only draw one of each
                  [(nth triangle-circumcenters (triangle-of-edge edge-idx))
                   (nth triangle-circumcenters (triangle-of-edge he))]))
              vector)
        (range)
        half-edges))))

(defn edges-around-point [half-edges start]
  (loop [result []
         incoming start
         prior-e nil]
    (if (or (= incoming -1)
            ;; stop if (= incoming start) and first round
            (and (= incoming start)
                 (not (nil? prior-e))))
      result
      (recur (conj result incoming)
             (let [outgoing (next-half-edge incoming)]
               (nth half-edges outgoing))
             incoming))))

(defn voronoi-cells [points {:keys [triangles half-edges] :as _delaunator-state}]
  (let [triangle-circumcenters (into [] (comp (triangle-xf points) circumcenter-xf) triangles)
        incoming-edges (reduce (fn [m [edge-idx he]]
                                 (let [next-idx (next-half-edge edge-idx)
                                       opposite-edge-idx (nth triangles next-idx)]
                                   (if (or (not (contains? m opposite-edge-idx))
                                           (= he -1))
                                     (assoc m opposite-edge-idx edge-idx)
                                     m)))
                               {} (map vector (range) half-edges))]
    (into []
      (comp (map-indexed
              (fn [idx point]
                (when-let [incoming (get incoming-edges idx)]
                 (let [edges (edges-around-point half-edges incoming)
                       vertices (mapv (comp #(nth triangle-circumcenters %) triangle-of-edge) edges)]
                   {:point point :idx idx :vertices vertices}))))
            ;; we can get nils if the point gets excluded in the
            ;;   delaunay triangulation because the previous point was too close
            (remove nil?))
      points)))



;;; Rendering
(defn draw-line
  ([x y]
   (q/line x y)))


(defn setup-q []
  {:points
   (inline-resource "voronoi.edn")})

(def prior (atom nil))


(defn draw-q [{:keys [x y points] :as state}]
  (when-not (= @prior state)
    (println :dirty? (= @prior state) (dissoc @prior :points) (dissoc state :points))
    (let [delaunator-state (time (delaunator points))
          {:keys [triangles half-edges hull]} delaunator-state]
      (q/background 240)

      ;; triangle edges
      ;#_
      (doseq [edge (unique-triangle-edges points triangles half-edges)
              :let [[start stop] edge]]
        (draw-line start stop))

      (q/stroke 255 0 0 50)

      ;; voronoi edges
      ;#_
      (doseq [edge (unique-voronoi-edges points triangles half-edges)
              :let [[start stop] edge]]
        (draw-line start stop))
      (q/stroke 0 0 0 255)

      ;; voronoi cells
      ;#_
      (doseq [cell (time (voronoi-cells points delaunator-state))
              :let [hue     (mod (* 2 (:idx cell)) 360)
                    [cx cy] (:point cell)]]
        (q/color-mode :hsb 360 1 1)
        (q/fill hue 0.2 0.8)
        (q/begin-shape)
        (doseq [[x y] (:vertices cell)]
          (q/vertex x y))
        (q/end-shape :close)
        (q/color-mode :rgb)
        (q/stroke 255 0 0)
        (q/ellipse cx cy 5 5)
        (q/stroke 0 0 0))

      (q/fill 150 150 150 50)
      ;; hull
      (q/begin-shape)
      (doseq [[x y] (map #(nth points %) hull)]
        (q/vertex x y))
      (q/end-shape :close))

    (q/stroke 0 0 0)
    (q/fill 255 255 255)


    (when-not (or (nil? x) (nil? y))
      (q/ellipse x y 3 3))
    (reset! prior state)))

(defn q-mouse-moved [{:keys [_x _y] :as state} event]
  (let [mouse-x (:x event) mouse-y (:y event)]
    (if (and (not (nil? mouse-x)) (not (nil? mouse-y)))
      (-> state
          ; set circle position to mouse position
          (assoc :x mouse-x :y mouse-y))
      state)))


(q/defsketch
  app
  :size [1200 600]
  :setup setup-q
  :draw draw-q
  :mouse-moved q-mouse-moved
  :middleware [fun-mode])
