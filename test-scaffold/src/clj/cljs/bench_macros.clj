(ns cljs.bench-macros)

(defmacro simple-benchmark-data
  "Runs expr iterations times in the context of a let expression with
  the given bindings, then prints out the bindings and the expr
  followed by number of iterations and total time. The optional
  argument print-fn, defaulting to println, sets function used to
  print the result. expr's string representation will be produced
  using pr-str in any case."
  [bindings expr iterations & {:keys [print-fn] :or {print-fn 'println}}]
  (let [bs-str   (pr-str bindings)
        expr-str (pr-str expr)]
    `(let ~bindings
       (~'js* "try {")
       (let [start#   (.getTime (js/Date.))
             ret#     (dotimes [_# ~iterations] ~expr)
             end#     (.getTime (js/Date.))
             elapsed# (- end# start#)]
         (~print-fn
          (pr-str
           {:bindings '~bindings
            :expr '~expr
            :iterations '~iterations
            :elapsed-msecs elapsed#})))

       (~'js* "} catch (blank) {")
       (~print-fn
        (pr-str
         {:bindings '~bindings
          :expr '~expr
          :iterations 'exception
          :elapsed-msecs elapsed#}))
       (~'js* "}"))))
