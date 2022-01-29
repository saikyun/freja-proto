(import ./graph :as g)
(import ./state :as s)
(import ./renders :as r :fresh true)
(import ./wrap)
(use freja/flow)

(def size 24)
(def start [size (* 1.15 size)])

(defn new-tile
  [xi yi zi parent]
  (def color (if (= zi 3)
               [0.4 0.2 0.4 0.2]
               (array/push
                 (map |(+ (* zi 0.1) $) [0.4 0.2 0.4])
                 0.8)))
  (-> (g/new "Tile" :parent parent)
      (merge-into {:pos @[(+ (start 0)
                             (* 0.8660254 size (mod yi 2))
                             (* xi (* 0.8660254 2 size)))
                          (+ (start 1)
                             (* yi 1.5 size))]
                   :height (* size 0.8)
                   :tile-pos [xi yi]
                   :z zi
                   :radius size
                   :color color
                   :hover-color |(map |(min 1 (+ 0.1 $)) ($ :color))
                   :render (wrap/fun r/draw-hexa)})))

(defn genwo
  []
  (let [w 10
        h 10
        world (or (first (g/find-named "World" s/gos))
                  (g/new "World"))]
    (array/clear (world :children))

    (put world :size size)
    (put world :grid-size [w h])
    (put world :x (start 0))
    (put world :y (start 1))
    (put world :on-event (wrap/funf 'r/hover-hexa))

    (comment
      (loop [yi :range [0 h]
             xi :range [0 w]]
        (new-tile xi yi 0 world))

      (loop [yi :range [1 3]
             xi :range [1 4]]
        (new-tile xi yi 1 world))

      (loop [yi :range [1 4]
             xi :range [6 10]]
        (new-tile xi yi 1 world))

      (new-tile 4 5 1 world))
    (def . 0)
    (def r 6)
    (def tiles [[r r r r r r r r r r r r r r r r r r r r r r r]
                '[r . . 4 . . . 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 3]
                [r . . . 4 . . 1 1 . 1 0 0 0 0 0 0 0 0 0 0 0 3]
                '[r . . 4 4 4 . 1 1 . 1 1 0 0 0 0 0 0 0 0 0 0 3]
                [r . . . . . . . . . . 0 0 0 0 0 0 0 0 0 0 0 3]
                '[r . . . . . . . . . . 1 0 0 0 0 0 0 0 0 0 0 3]
                [r 4 4 . . . . . 2 . . 1 1 0 0 0 0 0 0 0 0 0 3]
                '[3 4 4 4 2 2 2 4 2 . . 0 1 0 0 0 0 0 0 0 0 0 3]
                [3 0 0 0 0 0 0 2 4 2 0 0 1 0 0 0 0 0 0 0 0 0 3]
                '[3 0 0 0 0 0 0 2 2 2 0 0 1 0 0 0 0 0 0 0 0 0 3]
                [3 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 3]
                '[3 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 3]
                [3 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 3]
                '[3 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 3]
                [3 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 3]])

    (loop [y :range [0 (length tiles)]
           :let [row (in tiles y)]
           x :range [0 (length row)]
           :let [tile (in row x)
                 tile (case tile
                        '. 0
                        'r 9
                        tile)]]
      (new-tile x y tile world))

    (def lights [{:pos [10 4 2]
                  :range 5
                  :strength 0.8}
                 {:pos [5 10 2]
                  :range 5
                  :strength 0.8}])

    (defn light-amount
      [x y z]
      (var amount 0.1)
      (loop [{:strength s
              :pos lp
              :range r} :in lights
             :let [dist (v/dist [x y z] lp)]]
        (if (> z (lp 2))
          0
          (+= amount (min 1 (max 0 (* s (- r dist)))))))
      (min 1
           amount))

    (loop [t :in (world :children)
           :when (not= (t :z) 'r)]
      (update t :color |[;(take 3 $) (light-amount ;(t :tile-pos) (t :z))]))

    (pp (get-in world [:tiles 1]))

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
