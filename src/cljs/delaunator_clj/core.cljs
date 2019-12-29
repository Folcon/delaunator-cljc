(ns ^:figwheel-hooks delaunator-cljc.core
  (:require [goog.dom :as gdom]
            [delaunator-cljc.delaunay]))


;; define your app data so that it doesn't get over-written on reload
(defonce app-state (atom {}))

(defn get-app-element []
  (gdom/getElement "app"))



;; specify reload hook with ^;after-load metadata
(defn ^:after-load on-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  (swap! app-state update-in [:__figwheel_counter] inc))
