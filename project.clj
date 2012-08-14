(defproject cljs-bench "1.0.0-SNAPSHOT"
  :description "benchmark clojurescript from git"
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [enlive "1.0.1"]]

  :plugins [[lein-swank "1.4.4"]]

  :main cljs-bench.main)
