(ns breadcrumbs.scratch
  (:require [breadcrumbs.core :refer :all]))

(trace-fn
 (defn cheese
   [a b]
   (mapcat reverse [a b])))

(comment
  (cheese [:a :b :c] [:four :five :six])
  )

