;; This test runner is intended to be run from the command line
(ns delaunator-cljc.test-runner
  ;; require all the namespaces that you want to test
  (:require [delaunator-cljc.core-test]
            [figwheel.main.testing :refer [run-tests-async]]))

(defn -main [& args]
  (run-tests-async 5000))
