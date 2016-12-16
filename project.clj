(defproject cs-game "1.0.0"
  :min-lein-version "2.6.1"
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/clojurescript "1.9.225"]
                 [org.clojure/core.async "0.2.385"
                  :exclusions [org.clojure/tools.reader]]
                 [figwheel-sidecar "0.5.0"]
                 [reagent "0.6.0-rc"]]
  :plugins [[lein-cljsbuild "1.1.3" :exclusions [[org.clojure/clojure]]]]
  :source-paths ["src", "script"]
  :clean-targets ^{:protect false} ["resources/public/js/compiled" "target"]
  :cljsbuild {:builds
              [{:id           "dev"
                :source-paths ["src"]
                :figwheel     {:on-jsload "cs-game.core/on-js-reload"
                               :open-urls ["http://localhost:3449/index.html"]}

                :compiler     {:main                 cs-game.core
                               :asset-path           "js/compiled/out"
                               :output-to            "resources/public/js/compiled/cs_game.js"
                               :output-dir           "resources/public/js/compiled/out"
                               :source-map-timestamp true
                               :preloads             [devtools.preload]}}
               {:id           "min"
                :source-paths ["src"]
                :compiler     {:output-to     "resources/public/js/compiled/cs_game.js"
                               :main          cs-game.core
                               :optimizations :advanced
                               :pretty-print  false}}]}
  :figwheel {:css-dirs ["resources/public/css"]}
  :profiles {:dev {:dependencies [[binaryage/devtools "0.7.2"]
                                  [figwheel-sidecar "0.5.4-7"]
                                  [com.cemerick/piggieback "0.2.1"]]
                   :source-paths ["src" "dev"]
                   :repl-options {:init             (set! *print-length* 50)
                                  :nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}}})
