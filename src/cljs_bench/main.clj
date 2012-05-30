(ns cljs-bench.main
  (:gen-class)
  (:require [cljs-bench.core :as core]))

(defn -main [& args]
  (when (not= 2 (count args))
    (println "usage: program clojurescript report-dir")
    (System/exit 1))
  
  (core/update-benchmarks (first args) (second args))
  (System/exit 0))