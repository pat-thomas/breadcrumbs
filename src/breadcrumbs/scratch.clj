(ns breadcrumbs.scratch
  (:require [breadcrumbs.core :refer :all]))

(comment

  (do (trace-fn
       (defn cheese
         [a b]
         (mapcat reverse [a b])))

      (cheese [:a :b :c] [:four :five :six])
      
      (map :values (get-in @debug-atom [:breadcrumbs.scratch :cheese :calls]))
      )
)
