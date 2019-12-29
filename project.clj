(defproject delaunator-cljc "0.1.0-SNAPSHOT"
  :description "Fast 2D Delaunay triangulation in Clojure. A port of Delaunator."
  :url "https://github.com/Folcon/delaunator-cljc"
  :license {:name "MIT"
            :url "https://github.com/Folcon/delaunator-cljc/blob/master/LICENSE"}

  :min-lein-version "2.7.1"

  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/clojurescript "1.10.520"]
                 [org.clojure/test.check "0.10.0"]]

  :source-paths ["src/clj" "src/cljc" "src/cljs"]

  :clean-targets ^{:protect false} ["resources/public/js/compiled" "target"]

  :aliases {"fig"       ["trampoline" "run" "-m" "figwheel.main"]
            "fig:dev"   ["trampoline" "run" "-m" "figwheel.main" "-b" "dev" "-r"]
            "fig:min"   ["run" "-m" "figwheel.main" "-O" "advanced" "-bo" "dev"]
            "fig:test"  ["run" "-m" "figwheel.main" "-co" "test.cljs.edn" "-m" "delaunator-cljc.test-runner"]}

  :profiles {:dev {:dependencies [[com.bhauman/figwheel-main "0.2.3"]
                                  [com.bhauman/rebel-readline-cljs "0.1.4"]]}})


