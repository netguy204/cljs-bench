(ns cljs-bench.core
  (:use [hiccup.core :only [html]])
  (:require [clojure.java.shell :as sh]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [cljs-bench.baseline :as baseline]))

(def ^:dynamic *delete-tempdir* true)

(def ^:dynamic *runtimes* [:v8 :spidermonkey :javascriptcore])

;;; stolen from clojure-script-one's git bootstrap code

(defn- exec
  "Run a command, throwing an exception if it fails, returning the
  result as with clojure.java.shell/sh."
  [& args]
  (let [{:keys [exit out err] :as result} (apply sh/sh args)]
    (if (zero? exit)
      result
      (throw
       (Exception.
        (format "Command %s failed with exit code %s\n%s\n%s"
                (apply str (interpose " " args))
                exit
                out
                err))))))

(defn- git-clone
  "Clone the git repository at url into dir-name while working in
  directory working-dir."
  [url dir-name working-dir]
  (apply exec (remove nil? ["git" "clone" url (str dir-name) :dir working-dir])))

(defn- git-checkout
  "Check out the specified commit in dir."
  [commit dir]
  (println "Running git checkout " commit " in " (str dir))
  (exec "git" "checkout" commit :dir dir))

;;; end stolen code

(defn- git-revisions-between [dir start end]
  (string/split-lines (:out (exec "git" "rev-list" start (str "^" end) :dir dir))))

(defn- current-environment []
  (into {} (System/getenv)))

(defn- temporary-file-name []
  (let [temp (java.io.File/createTempFile "cljs-bench" (str (System/nanoTime)))]
    (assert (.delete temp))
    temp))

(defn- copy-tree
  [src dest]
  (if (.isDirectory src)
    (doseq [file (.list src)]
      (copy-tree (io/file src file)
                 (io/file dest file)))
    (do
      (io/make-parents dest)
      (io/copy src dest))))

(defn- prepare-working-directory [src treeish]
  (let [temp-name (temporary-file-name)]
    (git-clone src temp-name ".")
    (git-checkout treeish temp-name)
    (copy-tree (io/file "test-scaffold") temp-name)
    (copy-tree (io/file src "lib") (io/file temp-name "lib"))
    temp-name))

(defn- recursive-delete [dir]
  (when (.isDirectory dir)
    (doseq [file (.list dir)]
      (recursive-delete (io/file dir file))))
  (io/delete-file dir))

(defn- string-reader [str]
  (java.io.PushbackReader. (java.io.StringReader. str)))

(defn benchmark-revision [src treeish]
  (let [dir (prepare-working-directory src treeish)
        result (exec "bash" "script/benchmark_data"
                     :dir dir
                     :env (assoc (current-environment)
                            "CLOJURESCRIPT_HOME" ""))
        err (string/trim-newline (:err result))
        reader (string-reader (:out result))]

    (if *delete-tempdir*
      (recursive-delete dir)
      (println "temporary directory " dir " was not deleted"))

    (when (not (empty? err))
      (println "while running " treeish "\n" err))
    
    (read reader)))

(defn- safe-benchmark-revision [src treeish]
  (try
    (benchmark-revision src treeish)
    (catch Exception ex
      nil)))

(defn- write-benchmark-revisions [src outstream start end]
  (doseq [rev (git-revisions-between src start end)]
    (.write
     outstream
     (pr-str {:revision rev :result (safe-benchmark-revision src rev)}))
    (.flush outstream)))

(defn benchmark-revisions [src outfile start end]
  (let [stream (io/writer outfile)]
    (.write stream "[")
    (write-benchmark-revisions src stream start end)
    (.write stream "]")
    (.close stream)))

(defn- read-data [file]
  (read (java.io.PushbackReader. (io/reader file))))

(defn- find-test [runtime-result target-measurement]
  (let [extractor (juxt :bindings :expr)
        target (extractor target-measurement)
        matcher #(= target (extractor %))]
   (loop [result runtime-result]
     (if-let [item (first result)]
       (if (matcher item)
         item
         (recur (rest result)))))))

(defn- merge-runtime-results [news olds]
  (for [new news]
    (if-let [old (find-test olds new)]
      ;; we can merge
      (let [old-msecs (:elapsed-msecs old)
            new-msecs (:elapsed-msecs new)]
        (if (or (nil? old-msecs) (nil? new-msecs))
          new
          (conj new {:elapsed-msecs (min old-msecs new-msecs)})))

      ;; nothing to merge with
      new)))

(defn- merge-data [new-data old-data]
  (let [new-revisions-set (into #{} (map :revision new-data))
        old-data-map (into {} (map #(vector (:revision %) (:result %)) old-data))]

    (concat
     (for [new-result new-data]
       {:revision (:revision new-result)
        
        :result
        (if-let [old-result (old-data-map (:revision new-result))]
          ;; try to merge all that we can
           
          {;; we're always compiling the same thing so we can do a min
           ;; here
           :compile-time-msecs
           (min (get-in new-result [:result :compile-time-msecs])
                (:compile-time-msecs old-result))
           
           ;; file size should always be constant given a revision so
           ;; we take the new side
           :gzipped-size-bytes
           (get-in new-result [:result :gzipped-size-bytes])
           
           :v8 (merge-runtime-results (get-in new-result [:result :v8]) (:v8 old-result))
           :spidermonkey (merge-runtime-results (get-in new-result [:result :spidermonkey])
                                                (:spidermonkey old-result))
           :javascriptcore (merge-runtime-results (get-in new-result [:result :javascriptcore])
                                                  (:javascriptcore old-result))
           }
          ;; nothing to merge
          (:result new-result))
        })
     
     ;; pull in anything from the old results that we don't have
     ;; in the new at all
     (drop-while #(new-revisions-set (:revision %)) old-data)
     
     )))

(defn- runtime-benchmark-name [runtime-datum]
  (str (pr-str (:bindings runtime-datum)) " "
       (pr-str (:expr runtime-datum)) " "
       (:iterations runtime-datum) " iters"))

(defn- runtime-benchmark-names [runtime-data]
  (map runtime-benchmark-name runtime-data))

(defn- benchmark-names [data runtime]
  (let [revision (first data)
        tests (get-in revision [:result runtime])]
    (cons "revision" (runtime-benchmark-names tests))))

(defn- tabulate-data [data]
  (for [revision data]
    (cons (:revision revision)
          (map (fn [v8 sp jsc]
                 {:v8 (:elapsed-msecs v8)
                  :spidermonkey (:elapsed-msecs sp)
                  :javascriptcore (:elapsed-msecs jsc)})
               
               (get-in revision [:result :v8])
               (get-in revision [:result :spidermonkey])
               (get-in revision [:result :javascriptcore])))))

(defn- transpose [data]
  (apply map vector data))

(defn- interpose-str [item coll]
  (apply str (interpose item coll)))

(defn results->csv [data runtime]
  (let [table (cons (benchmark-names data runtime)
                    (tabulate-data data))]
    (interpose-str "\n"
      (map #(interpose-str "," (map runtime %)) table))))

(def ^:dynamic *plot-number* nil)

(defn plot-column [labeled-column column-keys revisions outdir
                   & {:keys [ylabel plot-min plot-min-max]
                      :or {ylabel "msecs", plot-min 0, plot-min-max 300}}]
  
  (let [[title column] labeled-column
        plot-number @*plot-number*
        filtered-column (filter identity (mapcat vals column))
        min-column-time (reduce min plot-min filtered-column)
        max-column-time (reduce max plot-min-max filtered-column)
        column-range (- max-column-time min-column-time)
        plot-max (max plot-min-max max-column-time)
        tempfiles
        (for [runtime column-keys]
          (let [rows (map vector
                          (reverse (range (count column)))
                          revisions
                          (map runtime column))
                rows (map #(interpose-str " " %) rows)
                column (interpose-str "\n" rows)
                tempfile (temporary-file-name)]
            (spit tempfile column)
            [runtime tempfile]))
        
        outfile (io/file outdir (str plot-number ".png"))
        plotrange (str "[][" plot-min ":" plot-max "]")
        
        plotlines
        (for [[runtime tempfile] tempfiles]
          (str "\"" tempfile
               "\" using 1:3:xtic(2) title '" (name runtime) "'"
               " with linespoints"))
        
        plotline (str "plot " plotrange (interpose-str ", " plotlines) "\n")
        command (str
                 "set title '" title "'\n"
                 "set terminal png size 800,600\n"
                 "set grid\n"
                 "set xtics border in rotate by -90 offset character 0, -2.1, 0\n"
                 "set ylabel \"" ylabel "\"\n"
                 "set output \"" outfile "\"\n"
                 plotline)
        
        notes (sh/sh "gnuplot" :in command)
        err (string/trim-newline (:err notes))]

    ;; report any plot errors
    (when (not (empty? err))
      (println "while generating " plot-number "\n"
               err)
      (println (map vector revisions column)))
    

    ;; increment the plot number
    (swap! *plot-number* inc)
    
    (doseq [[_ tempfile] tempfiles]
      (io/delete-file tempfile))
    
    {:file outfile
     :title title
     :number (dec plot-number)}))


(defn plot-data [data outdir]
  (binding [*plot-number* (atom 0)]
    (let [results (map :result data)
          ctime (map (fn [result] {:compile-time-msecs (:compile-time-msecs result)}) results)        
          gzipped (map (fn [result] {:gzipped-size-kbytes (if-let [bytes (:gzipped-size-bytes result)]
                                                            (/ (float bytes)
                                                               1024))}) results)
          labels (rest (benchmark-names data :v8))
          data (transpose (tabulate-data data))
          revisions (map #(apply str (take 5 %)) (first data))
          columns (rest data)
          labeled-columns (map vector labels columns)
          
          baseline-data (baseline/baseline-benchmark)]
      
      ;; set up for output
      (io/make-parents (io/file outdir))
      (.mkdir (io/file outdir))
      
      (concat
       [ ;; the compile time results
        (plot-column ["TwitterBuzz Compile Time" ctime] [:compile-time-msecs] revisions outdir
                     :ylabel "msecs" :plot-min 3000 :plot-min-max 8000)
        (plot-column ["TwitterBuzz Gzipped Size" gzipped] [:gzipped-size-kbytes] revisions outdir
                     :ylabel "kilobytes" :plot-min 40 :plot-min-max 60)
        ]
       
       ;; plot the benchmark results
       (for [labeled-column labeled-columns]
         (let [label (first labeled-column)
               column (second labeled-column)
               baseline-match (first (filter #(= (runtime-benchmark-name %) label) baseline-data))]
           (if baseline-match
             (let [new-column (map #(conj % {:jvm-clojure14-msecs (:elapsed-msecs baseline-match)})
                                   column)]
               (plot-column [label new-column] (conj *runtimes* :jvm-clojure14-msecs) revisions outdir))
             
             (plot-column labeled-column *runtimes* revisions outdir))))))))

(defn plot-gallery [data outdir]
  (let [results (plot-data data outdir)
        imgs (map #(list [:a {:name (str "plot" (:number %))}]
                         [:img {:src (.getName (:file %))}])
                  results)
        links (map #(vector
                     :ul
                     [:li
                      [:a {:href (str "#plot" (:number %))}
                       (str (:title %))]])
                   results)
        results-csvs (for [runtime *runtimes*]
                       {:runtime runtime
                        :file (io/file outdir (str (name runtime)
                                                   ".csv"))}) 
        body [:html
              [:body
               [:h1 "Clojurescript Benchmark Times"]
               [:a {:href "https://github.com/netguy204/cljs-bench/blob/master/test-scaffold/cljs-bench/cljs/benchmark_runner.cljs"}
                "[benchmark source]"]
               [:br]
               [:a {:href "http://github.com/netguy204/cljs-bench"}
                "[cljs-bench source]"]
               [:br]
               (for [results-csv results-csvs]
                 (list
                  [:a {:href (.getName (:file results-csv))}
                   "[" (name (:runtime results-csv)) " as CSV]"]
                  [:br]))
               links
               imgs]]]
    
    (doseq [results-csv results-csvs]
      (spit (:file results-csv) (results->csv data (:runtime results-csv))))
    
    (spit (io/file outdir "index.html") (html body))))

(defn update-benchmarks [dir output-dir]
  (let [last-head (io/file "last-head")
        last-sha1 (string/trim-newline
                   (slurp last-head))
        current-sha1 (string/trim-newline
                      (:out (exec "git" "rev-parse" "origin/master" :dir dir)))
        results-next (io/file "results-next.clj")
        results (io/file "results.clj")]

    (if (= last-sha1 current-sha1)
      (println "no changes since " last-sha1)
      
      (do
        (println "benchmarking between " last-sha1 " and " current-sha1)
        (benchmark-revisions dir results-next current-sha1 last-sha1)
        
        (if (.exists (io/file results))
          (spit results (pr-str (merge-data (read-data results-next) (read-data results))))
          (copy-tree results-next results))
        
        (plot-gallery (read-data results) output-dir)
        (spit last-head current-sha1)))))

