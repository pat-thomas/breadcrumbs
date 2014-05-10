(ns ^{:doc "Utilities for tracing function calls"}
  breadcrumbs.core)

(def debug-atom (atom {}))

(defmacro lookup
  ([fn-name]
     (let [namespace (keyword (str *ns*))]
       (-> debug-atom
           deref
           (get-in [namespace (keyword (name fn-name))])
           last)))
  ([fn-name arg-name]
     (let [namespace (keyword (str *ns*))]
       (-> debug-atom
           deref
           (get-in [namespace (keyword (name fn-name))])
           last
           (get (keyword (name arg-name)))))))

(defmacro trace-fn
  "When wrapped around a defn, redefines function to trace its calls"
  [body]
  (let [[_ fn-name fn-args fn-body] body
        debug-key [(keyword (str *ns*)) (keyword fn-name)]]
    (when (empty? (get-in @debug-atom debug-key))
      (swap! debug-atom assoc-in debug-key []))
    `(defn ~fn-name ~fn-args
       (let [capture-map# (reduce (fn [acc# [arg-sym# arg-val#]]
                                    (let [arg-key# (keyword (name arg-sym#))]
                                      (assoc acc# arg-key# arg-val#)))
                                  {}
                                  (partition 2 (interleave '~fn-args ~fn-args)))]
         (swap! debug-atom update-in ~debug-key #(conj % capture-map#)))
       ~fn-body)))

(defmacro untrace-fn
  "Given a function name, allows to be called with args captured by trace-fn"
  [fn-name & args])

(comment
  
  )
