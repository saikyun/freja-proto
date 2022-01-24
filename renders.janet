(use freja/flow)






(defn draw-cat :cat
  [self]
  (draw-circle (get self :x 100) 100 50
               #:blue
               :red
))