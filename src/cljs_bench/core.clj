(ns cljs-bench.core
  (:use [hiccup.core :only [html]])
  (:require [clojure.java.shell :as sh]
            [clojure.java.io :as io]
            [clojure.string :as string]))

(def ^:dynamic *v8-home* "/usr/local/Cellar/v8/3.9.24/bin/")
(def ^:dynamic *spidermonkey-home* "/Users/btaylor/local")
(def ^:dynamic *jsc-home* "/Applications/WebKit.app/Contents/Frameworks/10.7/JavaScriptCore.framework/Resources/")
(def ^:dynamic *jsc-libs* "/Applications/WebKit.app/Contents/Frameworks/10.7/")

(def ^:dynamic *gnuplot-path*
  "/Applications/Gnuplot.app/Contents/Resources/bin/gnuplot")
(def ^:dynamic *delete-tempdir* true)

;;; from clojure-script-one

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

;;; non stolen stuff :-p

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

(defn- clojurescript-home []
  ((current-environment) "CLOJURESCRIPT_HOME"))

(defn- prepare-working-directory [src treeish]
  (let [temp-name (temporary-file-name)]
    (git-clone src temp-name ".")
    (git-checkout treeish temp-name)
    (copy-tree (io/file "test-scaffold") temp-name)
    (copy-tree (io/file (clojurescript-home) "lib") (io/file temp-name "lib"))
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
                            "V8_HOME" *v8-home*
                            "SPIDERMONKEY_HOME" *spidermonkey-home*
                            "JSC_HOME" *jsc-home*
                            "DYLD_FRAMEWORK_PATH" *jsc-libs*
                            "CLOJURESCRIPT_HOME" ""))
        reader (string-reader (:out result))]

    (if *delete-tempdir*
      (recursive-delete dir)
      (println "temporary directory " dir " was not deleted"))
    
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

(defn- merge-data [output in1 in2]
  (let [data (concat (read-data in1) (read-data in2))]
    (spit output (pr-str data))))

(defn- benchmark-names [data runtime]
  (let [revision (first data)
        tests (get-in revision [:result runtime])]
    (cons "revision" (map #(str (pr-str (:bindings %)) " "
                                (pr-str (:expr %))) tests))))

(defn- tabulate-data [data runtime]
  (for [revision data]
    (cons (:revision revision)
          (map #(:elapsed-msecs %) (get-in revision [:result runtime])))))

;; [[1,2],[3,4,5]] => [[1,3],[2,4],[5]]

(defn- transpose [data]
  (apply map vector data))

(defn- interpose-str [item coll]
  (apply str (interpose item coll)))

(defn results->csv [data runtime]
  (let [table (cons (benchmark-names data runtime)
                    (tabulate-data data runtime))]
    (interpose-str "\n"
      (map #(interpose-str "," %) table))))

(defn- standard-deviation [values]
  (let [sum (reduce + values)
        n (count values)
        mean (/ sum n)
        sumsqr (reduce (fn [result val]
                         (+ result (* val val)))
                       0 values)]
    (Math/sqrt (- (/ sumsqr n)
                  (* mean mean)))))

(defn- ->coll [val]
  (if (coll? val)
    val
    [val]))

(defn plot-data [data runtime outdir]
  (let [labels (rest (benchmark-names data runtime))
        data (transpose (tabulate-data data runtime))
        revisions (map #(apply str (take 5 %)) (first data))
        columns (rest data)
        labeled-columns (map vector
                             (range (count labels))
                             labels columns)]

    ;; set up for output
    (io/make-parents (io/file outdir))
    (.mkdir (io/file outdir))
    
    (for [[plot-number title column] labeled-columns]
      (let [filtered-column (filter identity column)
            min-column-time (reduce min filtered-column)
            max-column-time (reduce max filtered-column)
            column-range (- max-column-time min-column-time)
            plot-min 0
            plot-max (max 300
                          (min max-column-time
                               (+ min-column-time
                                  (* 3 (standard-deviation filtered-column)))))
            rows (map vector (reverse (range (count column))) revisions column)
            rows (map #(interpose-str " " %) rows)
            column (interpose-str "\n" rows)
            tempfile (temporary-file-name)
            outfile (io/file outdir (str plot-number ".png"))
            command (str
                     "set terminal png\n"
                     "set grid\n"
                     "set xtics border in rotate by -90 offset character 0, -2.1, 0\n"
                     "set ylabel \"msecs\"\n"
                     "set output \"" outfile "\"\n"
                     "plot [][" plot-min ":" plot-max "]\"" tempfile
                     "\" using 1:3:xtic(2)"
                     " title '" title "'"
                     " with linespoints\n")]
        (spit tempfile column)
        (let [notes (sh/sh *gnuplot-path* :in command)
              err (string/trim-newline (:err notes))]
          (when (not (empty? err))
            (println err)))
        
        (io/delete-file tempfile)
        {:file outfile
         :title title
         :number plot-number}))))

(defn plot-gallery [data runtime outdir]
  (let [results (plot-data data runtime outdir)
        imgs (map #(list [:a {:name (str "plot" (:number %))}]
                         [:img {:src (.getName (:file %))}])
                  results)
        links (map #(vector
                     :ul
                     [:li
                      [:a {:href (str "#plot" (:number %))}
                       (str (:title %))]])
                   results)
        results-csv (io/file outdir "results.csv")
        body [:html
              [:body
               [:h1 "Clojurescript Benchmark Times"]
               [:a {:href "https://github.com/netguy204/cljs-bench/blob/master/test-scaffold/cljs-bench/cljs/benchmark_runner.cljs"}
                "[benchmark source]"]
               [:br]
               [:a {:href "http://github.com/netguy204/cljs-bench"}
                "[cljs-bench source]"]
               [:br]
               [:a {:href (.getName results-csv)}
                "[results as CSV]"]
               links
               imgs]]]
    (spit (io/file outdir "index.html") (html body))
    (spit results-csv (results->csv data runtime))))

(defn update-benchmarks [dir output-dir]
  (let [last-head (io/file "last-head")
        last-sha1 (string/trim-newline
                   (slurp last-head))
        current-sha1 (string/trim-newline
                      (:out (exec "git" "rev-parse" "origin/master" :dir dir)))
        results-next "results-next.clj"
        results "results.clj"]

    (if (= last-sha1 current-sha1)
      (println "no changes since " last-sha1)
      
      (do
        (println "benchmarking between " last-sha1 " and " current-sha1)
        (benchmark-revisions dir results-next current-sha1 last-sha1)
        
        (if (.exists (io/file results))
          (merge-data results results-next results)
          (copy-tree results-next results))
        
        (plot-gallery (read-data results) :v8 output-dir)
        (spit last-head current-sha1)))))

