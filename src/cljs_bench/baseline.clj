(ns cljs-bench.baseline)

(def ^:dynamic *bench-results* (atom []))

(defmacro simple-benchmark-data [bindings expr iterations]
  `(let ~bindings
     (let [start# (System/currentTimeMillis)
           ret# (dotimes [_# ~iterations] ~expr)
           end# (System/currentTimeMillis)
           elapsed# (- end# start#)]
       (swap! cljs-bench.baseline/*bench-results* conj {:bindings '~bindings :expr '~expr
                                                        :elapsed-msecs elapsed#
                                                        :iterations ~iterations}))))


(defrecord Foo [bar baz])

(defn ints-seq
  ([n] (ints-seq 0 n))
  ([i n]
     (when (< i n)
       (lazy-seq
        (cons i (ints-seq (inc i) n))))))

(def r (ints-seq 1000000))

(defn baseline-benchmark []
  (binding [*bench-results* (atom [])]
    (simple-benchmark-data [coll (list 1 2 3)] (first coll) 1000000)
    (simple-benchmark-data [coll (list 1 2 3)] (rest coll) 1000000)
    (simple-benchmark-data [] (list) 1000000)
    (simple-benchmark-data [] (list 1 2 3) 1000000)
    
    (simple-benchmark-data [] [] 1000000)
    (simple-benchmark-data [] [1 2 3] 1000000)
    (simple-benchmark-data [coll [1 2 3]] (transient coll) 100000)
    (simple-benchmark-data [coll [1 2 3]] (conj coll 4) 1000000)
    (simple-benchmark-data [coll [1 2 3]] (seq coll) 1000000)
    (simple-benchmark-data [coll (seq [1 2 3])] (first coll) 1000000)
    (simple-benchmark-data [coll (seq [1 2 3])] (rest coll) 1000000)
    (simple-benchmark-data [coll (seq [1 2 3])] (next coll) 1000000)
    
    (simple-benchmark-data [coll (take 100000 (iterate inc 0))] (reduce + 0 coll) 1)
    (simple-benchmark-data [coll (range 1000000)] (reduce + 0 coll) 1)
    (simple-benchmark-data [coll (into [] (range 1000000))] (reduce + 0 coll) 1)
    
    (simple-benchmark-data [coll {:foo 1 :bar 2}] (get coll :foo) 1000000)
    (simple-benchmark-data [coll {:foo 1 :bar 2}] (:foo coll) 1000000)
    
    (simple-benchmark-data [coll (Foo. 1 2)] (:bar coll) 1000000)
    (simple-benchmark-data [coll {:foo 1 :bar 2}] (assoc coll :baz 3) 100000)
    (simple-benchmark-data [coll {:foo 1 :bar 2}] (assoc coll :foo 2) 100000)
    
    (simple-benchmark-data [coll (range 500000)] (reduce + coll) 1)
    
    (simple-benchmark-data [s "{:foo [1 2 3]}"] (read-string s) 1000)
    
    (simple-benchmark-data [r (range 1000000)] (last r) 1)
    
    (simple-benchmark-data [r r] (last r) 1)
    (simple-benchmark-data [r r] (last r) 1)
    
    @*bench-results*
    
    ))

