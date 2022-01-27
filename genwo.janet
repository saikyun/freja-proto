(import ./graph :as g)
(import ./state :as s)
(import ./renders :as r :fresh true)
(import ./wrap)

(defn genwo
  []
  (let [size 64
        offset 2
        w 10
        h 10
        start [size (* 1.15 size)]
        world (or (first (g/find-named "World" s/gos))
                  (g/new "World"))]
    (array/clear (world :children))

    (put world :size size)
    (put world :grid-size [w h])
    (put world :x (start 0))
    (put world :y (start 1))
    (put world :on-event (wrap/funf 'r/hover-hexa))

    (loop [yi :range [0 h]
           xi :range [0 w]]
      (-> (g/new "Tile" :parent world)
          (merge-into {:pos @[(+ (start 0)
                                 (* 0.8660254 size (mod yi 2))
                                 (* xi (* 0.8660254 2 size)))
                              (+ (start 1)
                                 (* yi 1.5 size))]
                       :radius size
                       :color [0.4 0.2 0.4]
                       :hover-color 0xB4BCD8ff
                       :render (wrap/fun r/draw-hexa)})))

    # rows, then cols
    (comment loop [yi :range [0 h]
                   xi :range [0 w]
                   :let [x (+ (start 0) (* xi size))
                         y (+ (start 1) (* yi size))]]
             (-> (g/new (string "Tile " xi "/" yi)
                        :parent world)
                 (merge-into {:x x
                              :y y
                              :w (- size offset)
                              :h (- size offset)
                              :color 0xA3ABC7ff
                              :hover-color 0xB4BCD8ff
                              :on-event #(wrap/funf 'r/drag)
                              (wrap/funf 'r/hover)
                              :render (wrap/funf 'r/draw-rec)})))
    (pp world)
    world))

(when (dyn :freja/loading-file)
  (genwo)
  (s/force-refresh!))

(comment
  (map g/delete (g/find-nodes |(string/has-prefix? "Tile" ($ :name)) s/gos))
  (map g/delete (g/find-named "cat" s/gos))
  (array/clear (get-in (g/find-named "Trash" s/gos) [0 :children]))
  #
)
