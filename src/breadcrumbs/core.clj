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
  [fn-name]
  (let [namespace (keyword (str *ns*))]
    (-> debug-atom
        deref
        (get-in [namespace (keyword (name fn-name)) :exceptions]))))

(defn- normalize-destructuring-args
  [args]
  (let [looks-like-map-destructuring? #(and (map? (first %))
                                            (or (= (keys (first %)) [:keys])
                                                (= (keys (first %)) [:keys :as])))
        flatten-args                  (fn [form]
                                        (filter #(not= '& %)
                                                (flatten form)))]
    (if (looks-like-map-destructuring? args)
      (flatten-args (conj [(vals (first args))]
                          (last args)))
      (flatten-args args))))

(defmacro trace-fn
  "When wrapped around a defn, redefines function to trace its calls"
  [body]
  (let [[_ fn-name fn-args fn-body] body
        normalized-fn-args          (normalize-destructuring-args fn-args)
        debug-key                   [(keyword (str *ns*)) (keyword fn-name)]]
    (when (empty? (get-in @debug-atom debug-key))
      (swap! debug-atom assoc-in debug-key {:calls      []
                                            :exceptions []}))
    `(defn ~fn-name ~fn-args
       (let [normalized-arg-values# (list ~@normalized-fn-args)
             capture-map#           {:values        (reduce (fn [acc# [arg-sym# arg-val#]]
                                                              (assoc acc# (keyword (name arg-sym#)) arg-val#))
                                                            {}
                                                            (partition 2 (interleave '~normalized-fn-args normalized-arg-values#)))
                                     :argument-form '~fn-args}]
         (swap! debug-atom
                update-in
                ~(conj debug-key :calls)
                #(conj % capture-map#))
         (try ~fn-body
              (catch Exception e#
                (swap! debug-atom update-in ~(conj debug-key :exceptions) #(conj % {:message (.getMessage e#)
                                                                                    :type    (.getClass e#)
                                                                                    :args    capture-map#}))
                (throw e#)))))))

(defmacro untrace-fn
  "Given a function name, allows to be called with args captured by trace-fn. By
   default, uses the values of the arguments most recently passed to the function."
  [fn-name & args]
  :implement-me)
