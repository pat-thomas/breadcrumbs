(ns ^{:doc "Utilities for tracing function calls"}
  breadcrumbs.core)

(def debug-atom (atom {}))

(defmacro lookup
  ([fn-name]
     (let [namespace (keyword (str *ns*))]
       (-> debug-atom
           deref
           (get-in [namespace (keyword (name fn-name)) :calls])
           last)))
  ([fn-name arg-name]
     (let [namespace (keyword (str *ns*))]
       (-> debug-atom
           deref
           (get-in [namespace (keyword (name fn-name)) :calls])
           last
           (get (keyword (name arg-name)))))))

(defmacro lookup-exceptions
  ([fn-name]
     (let [namespace (keyword (str *ns*))]
       (-> debug-atom
           deref
           (get-in [namespace (keyword (name fn-name)) :exceptions])))))

(defmacro trace-fn
  "When wrapped around a defn, redefines function to trace its calls"
  [body]
  (let [[_ fn-name fn-args fn-body] body
        debug-key [(keyword (str *ns*)) (keyword fn-name)]]
    (when (empty? (get-in @debug-atom debug-key))
      (let [nascent-debug-map {:calls      []
                               :exceptions []}]
       (swap! debug-atom assoc-in debug-key nascent-debug-map)))
    `(defn ~fn-name ~fn-args
       (let [capture-map# (reduce (fn [acc# [arg-sym# arg-val#]]
                                    (let [arg-key# (keyword (name arg-sym#))]
                                      (assoc acc# arg-key# arg-val#)))
                                  {}
                                  (partition 2 (interleave '~fn-args ~fn-args)))]
         (swap! debug-atom update-in ~(conj debug-key :calls) #(conj % capture-map#))
         (try
           ~fn-body
           (catch Exception e#
             (let [exception-rec# {:message (.getMessage e#)
                                   :type    (.getClass e#)
                                   :args    capture-map#}]
               (swap! debug-atom update-in ~(conj debug-key :exceptions) #(conj % exception-rec#))
               (throw e#))))))))

(defmacro untrace-fn
  "Given a function name, allows to be called with args captured by trace-fn. By
   default, uses the values of the arguments most recently passed to the function."
  [fn-name & args]
  :implement-me)


(comment
  
  )
