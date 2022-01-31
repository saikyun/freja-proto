(use freja/flow)
(import freja/events :as e)
(import ./state :as s)
(import ./graph :as g)
(use ./assets)

(defn draw-cat
  [self]
  (def {:x x
        :y y
        :radius radius} self)
  (draw-circle x y radius (if (= self (s/state :dragged-go))
                            :red
                            :blue)))

(defn draw-rec
  [self]
  (def {:x x :y y :w w :h h
        :color color
        :hover-color hover-color
        :hover hover} self)

  (default color :blue)
  (default hover-color :red)

  (draw-rectangle
    (math/floor (- x (* w 0.5)))
    (math/floor (- y (* h 0.5)))
    w
    h
    (if hover
      hover-color
      color)))

(defn draw-hexa
  [self]
  (def {:pos p
        :z z
        :height height
        :radius radius
        :tile-pos tile-pos
        :hover hover
        :hover-color hover-color
        :color color} self)

  (default z 0)

  (def p (v/v+ p [0 (* z -1 height)]))

  (defer (rl-pop-matrix)
    (rl-push-matrix)

    (rl-scalef 1 s/height-scale 1)

    (loop [i :range [0 (inc z)]]
      (draw-poly (v/v+ p [0 (* height i)]) 6 radius 0 [0.1 0.2 0.2]))
    (draw-poly p 6 (+ -1 radius) 0 (if hover
                                     (hover-color self)
                                     (if (= 3 (length color))
                                       [;color 0.7]
                                       color)))
    #  (draw-poly p 6 (- radius 1) 0 (if hover
    #                                  hover-color
    #                                  color))
)
  (def n 0.8660254)

  (def radius (* radius 0.95))

  # draw triangles
  (comment defer (rl-pop-matrix)
           (rl-push-matrix)

           (rl-translatef ;p 0)

           (var last-p [0 radius])

           (loop [p :in [[(* n radius) (* 0.5 radius)]
                         [(* n radius) (* -0.5 radius)]
                         [0 (* -1 radius)]
                         [(* -1 n radius) (* -0.5 radius)]
                         [(* -1 n radius) (* 0.5 radius)]
                         [0 radius]]]
             (def dir (-> (v/v+ last-p p)
                          (v/v* 0.5)))
             (def norma (map math/abs dir))
             (def dir (map (fn [v1 v2] (if (zero? v2) 0 (/ v1 v2))) dir norma))
             (draw-triangle-fan
               [[0 0]
                last-p
                p]
               [(mod (* 0.5 (inc (dir 0))) 2)
                0
                0
                (mod (* 0.5 (inc (dir 1))) 2)])
             (set last-p p))

           (comment
             (draw-triangle-fan
               [[0 0]
                [(* n radius) (* 0.5 radius)]
                [(* n radius) (* -0.5 radius)]]
               :red)

             (draw-triangle-fan
               [[0 0]
                [(* n radius) (* -0.5 radius)]
                [0 (* -1 radius)]]
               [0.5 0.5 0.5]))))

(defn texture
  [self]
  (def {:x x
        :y y
        :offset offset
        :w w :h h
        :scale scale
        :texture texture} self)

  (default offset [0 0])

  (update self :render-x |(+ (or $ 0) (* 0.2 (- x (or $ 0)))))
  (update self :render-y |(+ (or $ 0) (* 0.2 (- y (or $ 0)))))

  (def b (max 0.3 (get self :brightness 1)))

  (defer (rl-pop-matrix)
    (rl-push-matrix)
    (rl-translatef
      (+ (offset 0) (- (self :render-x) (* w 0.5)))
      (+ (offset 1) (- (self :render-y) (* h 0.5)))
      0)
    (when scale
      (rl-scalef (scale 0) (scale 1) 0))
    (draw-texture-ex
      (in assets texture)
      [0
       0]
      0
      1
      [b
       b
       b
       (+ 0.8 (* 0.2 b))])))

(defn in-rec?
  [rec [px py]]
  (def {:x x :y y :h h :w w :radius radius} rec)
  (default w (* 2 radius))
  (default h (* 2 radius))

  (def w0.5 (* w 0.5))
  (def h0.5 (* h 0.5))

  (and
    (>= px (- x w0.5))
    (<= px (+ x w0.5))
    (>= py (- y h0.5))
    (<= py (+ y h0.5))))

(defn drag
  [self ev]
  # (pp ev)
  (def pos (if (= :scroll (first ev))
             (ev 2)
             (ev 1)))

  (match ev
    [:press _]
    (when (in-rec? self pos)
      (put s/state :selected self)
      (put s/state :dragged-go self)
      (put s/state :dragged-go-offset (v/v- [(self :x)
                                             (self :y)]
                                            pos)))

    [:drag [x y]]
    (when (= (s/state :dragged-go) self)
      (let [[ox oy] (s/state :dragged-go-offset)]
        (-> self
            (put :x (+ x ox))
            (put :y (+ y oy)))))

    [:release _]
    (when (= (s/state :dragged-go) self)
      (-> s/state
          (put :dragged-go nil)
          (put :dragged-go-offset nil))))

  # take code from other drag thing
)

(defn hexa-hit
  [hexas p]
  (var d nil)
  (var n nil)

  (def p @[;p])
  (update p 1 * (/ 1 s/height-scale))

  (loop [c :in hexas
         :let [dist (min (v/dist (c :pos) p)
                         (v/dist (v/v+ (c :pos)
                                       [0 (* -1
                                             (c :height)
                                             (get c :z 0))])
                                 p))]
         :when (<= dist (c :radius))]
    (cond (nil? d)
      (do (set d dist)
        (set n c))

      (or
        (< (get n :z 0) (get c :z 0))
        (< dist d))
      (do
        (set d dist)
        (set n c))))
  n)

(defn move-to-tile
  [self tile]
  (def {:pos p :color c} tile)
  (g/set-parent self tile)
  (put self :brightness (c 3))
  (def [nx ny] p)
  (def ny (+ (* s/height-scale ny) (* (get tile :z 0) s/height-scale (tile :height) -1)))
  (-> self
      (put :x nx)
      (put :y ny)))

(defn snap-drag
  [self ev]
  (unless (self :locked)
    # (pp ev)
    (def pos (if (= :scroll (first ev))
               (ev 2)
               (ev 1)))

    (match ev
      [:press _]
      (when (in-rec? self pos)
        (put s/state :selected self)
        (put s/state :dragged-go self)
        (put s/state :dragged-go-offset (v/v- [(self :x)
                                               (self :y)]
                                              pos)))

      [:drag [x y]]
      (when (= (s/state :dragged-go) self)
        (let [world (first (g/find-named "World" s/gos))
              tile (hexa-hit (world :children) pos)]
          (when tile
            (move-to-tile self tile))))

      [:release _]
      (when (= (s/state :dragged-go) self)
        (-> s/state
            (put :dragged-go nil)
            (put :dragged-go-offset nil))))

    # take code from other drag thing
))

(defn ->tile
  [world [x y]]
  (get (world :children) (+ (* y (world :world-width))
                            (/ (- x (mod y 2)) 2))))

(defn move
  [self delta]
  (let [world (first (g/find-named "World"))
        grid (world :children)
        pos (get-in self [:parent :tile-pos])
        new-pos (v/v+ pos delta)
        new-tile (->tile world new-pos)]
    (if-not new-tile
      (printf "No tile at: %p" new-pos)
      (do
        (printf "Moved to: %p = %p" new-pos (new-tile :tile-pos))
        (move-to-tile self new-tile)))))

(defn qp
  [& args]
  (array/push s/queue args))

(defn walk-with-keys
  [self ev]
  (print "walk with keys")
  (match ev
    [:key-down :e]
    (qp move self [1 -1])

    [:key-down :q]
    (qp move self [-1 -1])

    [:key-down :w123]
    (qp move self [-1 -1])

    [:key-down :s123]
    (qp move self [-2 0])

    [:key-down :a]
    (qp move self [-2 0])

    [:key-down :d]
    (qp move self [2 0])

    [:key-down :z]
    (qp move self [-1 1])

    [:key-down :x123]
    (qp move self [-1 1])

    [:key-down :c]
    (qp move self [1 1])))

(defn player
  [self ev]
  (snap-drag self ev)
  (walk-with-keys self ev))

(defn hover
  [self ev]
  # (pp ev)
  (def pos (if (= :scroll (first ev))
             (ev 2)
             (ev 1)))

  (match ev
    [:press _]
    (when (in-rec? self pos)
      (put s/state :selected self))

    [:mouse-move [x y]]
    (put self :hover (in-rec? self pos))

    [:drag [x y]]
    (when (self :hover)
      (put self :hover false))

    [:release [x y]]
    (put self :hover (in-rec? self pos)))

  # take code from other drag thing
)

(defn hover-hexa
  [self ev]
  # (pp ev)
  (def pos (if (= :scroll (first ev))
             (ev 2)
             (ev 1)))

  (match ev
    [:press _]
    (do (def hexa (hexa-hit (self :children) pos))
      (e/put! s/state :selected hexa))

    [:mouse-move [x y]]
    (do (when-let [s (self :hover)]
          (put s :hover nil)
          (put self :hover nil))
      (def hexa (hexa-hit (self :children) pos))
      (when hexa
        (put hexa :hover true)
        (put self :hover hexa)))

    [:drag [x y]]
    (do (when-let [s (self :hover)]
          (put s :hover nil)
          (put self :hover nil))
      (def hexa (hexa-hit (self :children) pos))
      (when hexa
        (put hexa :hover true)
        (put self :hover hexa))))

  # take code from other drag thing
)

(defn draw-rope
  [self]
  #(update self :to |(if (string? $) (first (g/find-named $)) $))

  (def {:from from
        :to to} self)

  (def from (first (g/find-named from)))
  (def to (first (g/find-named to)))

  (def {:offset o1} from)
  (def {:offset o2} to)

  (draw-line-ex [(math/floor (+ (from :x) (o1 0)))
                 (math/floor (+ (from :y) (o1 1)))]
                [(math/floor (+ (to :x) (o2 0)))
                 (math/floor (+ (to :y) (o2 1)))]
                7
                0xF9B4F2ff)

  (draw-line-ex [(math/floor (+ (from :x)))
                 (math/floor (+ (from :y)))]
                (v/v+
                  [(math/floor (+ (from :x)))
                   (math/floor (+ (from :y)))]
                  (map math/floor
                       (v/v*
                         (v/v-
                           [(math/floor (+ (to :x)))
                            (math/floor (+ (to :y)))]
                           [(math/floor (+ (from :x)))
                            (math/floor (+ (from :y)))])
                         0.2)))
                10
                0x000000aa)

  (draw-line-ex (v/v+
                  [(math/floor (+ (from :x)))
                   (math/floor (+ (from :y)))]
                  (map math/floor
                       (v/v*
                         (v/v-
                           [(math/floor (+ (to :x)))
                            (math/floor (+ (to :y)))]
                           [(math/floor (+ (from :x)))
                            (math/floor (+ (from :y)))])
                         0.8)))
                (v/v+
                  [(math/floor (+ (from :x)))
                   (math/floor (+ (from :y)))]
                  (map math/floor
                       (v/v*
                         (v/v-
                           [(math/floor (+ (to :x)))
                            (math/floor (+ (to :y)))]
                           [(math/floor (+ (from :x)))
                            (math/floor (+ (from :y)))])
                         1)))
                10
                0x000000aa)

  (draw-line-ex (v/v+
                  [(math/floor (+ (from :x)))
                   (math/floor (+ (* 32 0.75) (from :y)))]
                  (map math/floor
                       (v/v*
                         (v/v-
                           [(math/floor (+ (to :x)))
                            (math/floor (+ (to :y)))]
                           [(math/floor (+ (from :x)))
                            (math/floor (+ (from :y)))])
                         0.2)))
                (v/v+
                  [(math/floor (+ (from :x)))
                   (math/floor (+ (* 32 0.75) (from :y)))]
                  (map math/floor
                       (v/v*
                         (v/v-
                           [(math/floor (+ (to :x)))
                            (math/floor (+ (to :y)))]
                           [(math/floor (+ (from :x)))
                            (math/floor (+ (from :y)))])
                         0.8)))
                10
                0x000000aa))
