(ns cljs.benchmark-runner
  (:refer-clojure :exclude [println])
  (:use-macros [cljs.bench-macros :only [simple-benchmark-data]])
  (:require [cljs.reader :as reader]))

(def println print)

(set! *print-fn* js/print)
(simple-benchmark-data [x 1] (identity x) 1000000)

(def arr (let [arr (array)]
           (dotimes [i 1000000]
             (.push arr i))
           arr))
(defn sum [a b] (+ a b))
(simple-benchmark-data [coll (seq arr)] (ci-reduce coll + 0) 1)
(simple-benchmark-data [coll (seq arr)] (ci-reduce coll sum 0) 1)
(simple-benchmark-data [coll arr] (array-reduce coll + 0) 1)
(simple-benchmark-data [coll arr] (array-reduce coll sum 0) 1)

;; WARNING: will get compiled away under advanced
(simple-benchmark-data [coll []] (instance? PersistentVector coll) 1000000)

(simple-benchmark-data [coll (list 1 2 3)] (satisfies? ISeq coll) 1000000)
(simple-benchmark-data [coll [1 2 3]] (satisfies? ISeq coll) 1000000)
(println)

(simple-benchmark-data [coll (list 1 2 3)] (first coll) 1000000)
(simple-benchmark-data [coll (list 1 2 3)] (-first coll) 1000000)
(simple-benchmark-data [coll (list 1 2 3)] (rest coll) 1000000)
(simple-benchmark-data [coll (list 1 2 3)] (-rest coll) 1000000)
(simple-benchmark-data [] (list) 1000000)
(simple-benchmark-data [] (list 1 2 3) 1000000)
(println)

(simple-benchmark-data [] [] 1000000)
(simple-benchmark-data [] [1 2 3] 1000000)
(simple-benchmark-data [coll [1 2 3]] (transient coll) 100000)
(simple-benchmark-data [coll [1 2 3]] (conj coll 4) 1000000)
(simple-benchmark-data [coll [1 2 3]] (-conj coll 4) 1000000)
(simple-benchmark-data [coll [1 2 3]] (seq coll) 1000000)
(simple-benchmark-data [coll (seq [1 2 3])] (first coll) 1000000)
(simple-benchmark-data [coll (seq [1 2 3])] (-first coll) 1000000)
(simple-benchmark-data [coll (seq [1 2 3])] (rest coll) 1000000)
(simple-benchmark-data [coll (seq [1 2 3])] (-rest coll) 1000000)
(simple-benchmark-data [coll (seq [1 2 3])] (next coll) 1000000)
(println)

(simple-benchmark-data [coll (take 100000 (iterate inc 0))] (reduce + 0 coll) 1)
(simple-benchmark-data [coll (range 1000000)] (reduce + 0 coll) 1)
(simple-benchmark-data [coll (into [] (range 1000000))] (reduce + 0 coll) 1)
(println)

(simple-benchmark-data [coll {:foo 1 :bar 2}] (get coll :foo) 1000000)
(simple-benchmark-data [coll {:foo 1 :bar 2}] (-lookup coll :foo nil) 1000000)
(simple-benchmark-data [coll {:foo 1 :bar 2}] (:foo coll) 1000000)
(defrecord Foo [bar baz])
(simple-benchmark-data [coll (Foo. 1 2)] (:bar coll) 1000000)
(simple-benchmark-data [coll {:foo 1 :bar 2}] (assoc coll :baz 3) 100000)
(simple-benchmark-data [coll {:foo 1 :bar 2}] (assoc coll :foo 2) 100000)
(println)

(simple-benchmark-data [coll (range 500000)] (reduce + coll) 1)
(println)

(simple-benchmark-data [s "{:foo [1 2 3]}"] (reader/read-string s) 1000)
(println)

(simple-benchmark-data [r (range 1000000)] (last r) 1)
(println)

(defn ints-seq
  ([n] (ints-seq 0 n))
  ([i n]
     (when (< i n)
       (lazy-seq
        (cons i (ints-seq (inc i) n))))))
(def r (ints-seq 1000000))
(simple-benchmark-data [r r] (last r) 1)
(simple-benchmark-data [r r] (last r) 1)
(println)

(println "\n")
