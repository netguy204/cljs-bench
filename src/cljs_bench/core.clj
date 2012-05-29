(ns cljs-bench.core
  (:require [clojure.java.shell :as sh]
            [clojure.java.io :as io]
            [clojure.string :as string]))

(def ^:dynamic *v8-home* "/usr/local/Cellar/v8/3.9.24/bin/")

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
                            "CLOJURESCRIPT_HOME" ""))
        reader (string-reader (:out result))]

    ;;(recursive-delete dir)
    (read reader)))

(defn safe-benchmark-revision [src treeish]
  (try
    (benchmark-revision src treeish)
    (catch Exception ex
      nil)))

(defn write-benchmark-revisions [src outstream start end]
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

(defn read-data [file]
  (read (java.io.PushbackReader. (io/reader file))))

(defn extract-column [data runtime expr]
  (for [rev data]
    (let [measurements (get-in rev [:result runtime])]
      (assoc (first (filter #(= expr (:expr %)) measurements))
        :revision (:revision rev)))))