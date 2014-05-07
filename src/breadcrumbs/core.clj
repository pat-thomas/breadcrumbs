(ns breadcrumbs.core)

(comment
  "Utilities for tracing function calls"
  )

(def debug-atom (atom {}))

(defn lookup [])

(defmacro trace-fn
  [body]
  (let [[_ fn-name fn-args fn-body] body
        debug-key [(keyword (str *ns*)) (keyword fn-name)]]
    (swap! debug-atom assoc-in debug-key {})
    `(defn ~fn-name ~fn-args
       (doseq [[arg-sym# arg-val#] (partition 2 (interleave '~fn-args ~fn-args))]
         (let [arg-key# (conj ~debug-key (keyword (name arg-sym#)))]
           (swap! debug-atom assoc-in arg-key# arg-val#)))
       ~fn-body)))

(defmacro untrace-fn
  "Given a function name, allows to be called with args captured by trace-fn"
  [fn-name & args])

(comment

  (trace-fn
   (defn foo [a b c]
     :bar))

  (foo 1 2 3)
  
  )
