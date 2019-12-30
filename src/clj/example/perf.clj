(ns example.perf
  (:require [clj-async-profiler.core :as prof]
            [delaunator-cljc.macros :refer [inline-resource]]
            [delaunator-cljc.delaunay :refer [delaunator]]))


(comment
  (require '[delaunator-cljc.delaunay :refer [delaunator]] :reload-all)
  (def big-map-points
    (doall (inline-resource "voronoi.edn")))

  ;; Profile
  (prof/profile (dotimes [_i 1000] (delaunator big-map-points)))

  ;; Serve results
  (prof/serve-files 8080)

  ;; Simple benchmarking
  (time (dotimes [_i 100] (delaunator big-map-points))))
