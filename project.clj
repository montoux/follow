(defproject montoux/follow "1.0.0"
  :description "FRP-inspired state propagation library for ClojureScript."
  :url "http://github.com/montoux/follow"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.9.0" :scope "provided"]
                 [org.clojure/clojurescript "1.9.946" :scope "provided"]]

  :plugins [[lein-cljsbuild "1.1.7"]]

  :cljsbuild {:builds {}}

  :hooks [leiningen.cljsbuild]

  :profiles
  {:dev {:dependencies
         [[montoux/test "1.0.0"]]
         :cljsbuild
         {:test-commands
          {"node" ["node" "target/test/js/test.js"]}
          :builds
          {:test {:source-paths ["src" "test"]
                  :compiler     {:output-to     "target/test/js/test.js"
                                 :output-dir    "target/test/js"
                                 :optimizations :none
                                 :main          "test.node"
                                 :target        :nodejs}}}}}}
  )
