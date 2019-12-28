(ns delaunator-clj.core-test
    (:require [cljs.test :refer-macros [deftest is testing]]
              [clojure.test.check.clojure-test :refer [defspec]]
              [clojure.test.check.properties :as prop]
              [clojure.test.check.generators :as gen]
              [clojure.test.check :as tc]
              [delaunator-clj.delaunay :refer [delaunator]])
    (:require-macros [delaunator-clj.macros :refer [inline-resource]]))

(let [points [[0 0] [0 1] [0 2] [1 2] [2 2] [2 1] [2 0] [1 0]]]
  (println :js)
  (simple-benchmark [js-points (clj->js points)] (.from js/Delaunator js-points) 3)
  (println :cljs)
  (simple-benchmark [points points] (delaunator points) 3))

(defn array->vector [a]
  (into [] (array-seq a)))

(deftest check-small-map
 (let [points [[0 0] [0 1] [0 2] [1 2] [2 2] [2 1] [2 0] [1 0]]
       js-points (clj->js points)
       js-result (.from js/Delaunator js-points)
       js-triangles (.-triangles js-result)
       js-halfedges (.-halfedges js-result)
       js-hullnext (.-_hullNext js-result)
       js-hullstart (.-_hullStart js-result)
       js-hull (.-hull js-result)
       {:keys [triangles half-edges hull-next hull-start hull] :as clj-result} (delaunator points)]
   (println (js-keys js-result) (keys clj-result))
   (println)
   (println :js-triangles js-triangles)
   (println :triangles triangles)
   (is (= triangles (array->vector js-triangles)))

   (println)
   (println :js-halfedges js-halfedges)
   (println :half-edges half-edges)
   (is (= half-edges (array->vector js-halfedges)))

   (println)
   (println :js-hullnext js-hullnext)
   (println :js-hullstart js-hullstart)
   (println :js-hull js-hull)
   (println :hull-next hull-next)
   (println :hull-start hull-start)
   (println :hull hull)
   (is (= hull (array->vector js-hull)))))

#_#_#_#_#_#_#_
;; Generative Testing
(def gen-point (gen/tuple gen/int gen/int))
(def gen-points (gen/vector-distinct gen-point {:min-elements 3}))

(comment
  (gen/sample gen-points))

(defn check-sort
  "doublecheck the issue isn't purely based around the ordering of points"
  [points]
  (let [js-points (clj->js points)
        sorted-js (.quicksort js/Delaunator)]))


(defspec test-triangles-js-equal-to-cljc 100
  (prop/for-all [points gen-points]
    (let [_ (println points)
          js-points (clj->js points)
          js-result (.from js/Delaunator js-points)
          js-triangles (.-triangles js-result)
          js-halfedges (.-halfedges js-result)
          js-hull (.-hull js-result)
          {:keys [triangles half-edges hull] :as clj-result} (delaunator points)]
      (= triangles (array->vector js-triangles)))))


(defspec test-half-edges-js-equal-to-cljc 100
  (prop/for-all [points gen-points]
    (let [_ (println points)
          js-points (clj->js points)
          js-result (.from js/Delaunator js-points)
          js-triangles (.-triangles js-result)
          js-halfedges (.-halfedges js-result)
          js-hull (.-hull js-result)
          {:keys [triangles half-edges hull] :as clj-result} (delaunator points)]
      (= half-edges (array->vector js-halfedges)))))

(defspec test-hull-js-equal-to-cljc 100
  (prop/for-all [points gen-points]
    (let [_ (println points)
          js-points (clj->js points)
          js-result (.from js/Delaunator js-points)
          js-triangles (.-triangles js-result)
          js-halfedges (.-halfedges js-result)
          js-hull (.-hull js-result)
          {:keys [triangles half-edges hull] :as clj-result} (delaunator points)]
      (= hull (array->vector js-hull)))))


;#_#_#_#_
(def random-size 10)

(def random-points
  (into [] (for [_n (range random-size)
                 :let [x (rand-int 255) y (rand-int 255)]] [x y])))

(let [points random-points]
  (println :js)
  (simple-benchmark [js-points (clj->js points)] (.from js/Delaunator js-points) 3)
  (println :cljs)
  (simple-benchmark [points points] (delaunator points) 3))


(deftest check-random-map
  (let [points random-points
        js-points (clj->js points)
        js-result (.from js/Delaunator js-points)
        js-triangles (.-triangles js-result)
        js-halfedges (.-halfedges js-result)
        js-hullnext (.-_hullNext js-result)
        js-hullstart (.-_hullStart js-result)
        js-hull (.-hull js-result)
        {:keys [triangles half-edges hull-next hull-start hull] :as clj-result} (delaunator points)]
    (println (js-keys js-result) (keys clj-result))
    (println)
    (println :points points)
    (println)
    (println :js-triangles js-triangles)
    (println :triangles triangles)
    (is (= triangles (array->vector js-triangles)))

    (println)
    (println :js-halfedges js-halfedges)
    (println :half-edges half-edges)
    (is (= half-edges (array->vector js-halfedges)))

    (println)
    (println :js-hullnext js-hullnext)
    (println :js-hullstart js-hullstart)
    (println :js-hull js-hull)
    (println :hull-next hull-next)
    (println :hull-start hull-start)
    (println :hull hull)
    (is (= hull (array->vector js-hull)))))

;#_#_#_
(def big-map-points
  (inline-resource "voronoi.edn"))


(let [points big-map-points]
  (println :js)
  (simple-benchmark [js-points (clj->js big-map-points)] (.from js/Delaunator js-points) 1)
  (println :cljs)
  (simple-benchmark [points big-map-points] (delaunator points) 1))

(deftest check-larger-map
 (let [points big-map-points
       js-points (clj->js points)
       js-result (.from js/Delaunator js-points)
       js-triangles (.-triangles js-result)
       js-halfedges (.-halfedges js-result)
       js-hull (.-hull js-result)
       {:keys [triangles half-edges hull] :as clj-result} (delaunator points)]
   (is (= triangles (array->vector js-triangles)))

   (is (= half-edges (array->vector js-halfedges)))

   (is (= hull (array->vector js-hull)))))

(comment
  (require '[delaunator-clj.core-test :as t])
  (t/test-triangles-js-equal-to-cljc)
  (t/test-half-edges-js-equal-to-cljc)
  (t/test-hull-js-equal-to-cljc))
