(ns delaunator-clj.macros
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]))

(defmacro inline-resource [resource-path]
  (clojure.edn/read-string (slurp (clojure.java.io/resource resource-path))))
