(use freja/flow)
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
        :radius radius
        :hover hover
        :hover-color hover-color
        :color color} self)

  (draw-poly p 6 radius 0 [0.1 0.2 0.2])
  (draw-poly p 6 (- radius 4) 0 (if hover
                                  hover-color
                                  color)))

(defn texture
  [self]
  (def {:x x
        :y y
        :w w :h h} self)

  (update self :render-x |(+ (or $ 0) (* 0.2 (- x (or $ 0)))))
  (update self :render-y |(+ (or $ 0) (* 0.2 (- y (or $ 0)))))

  (draw-texture
    (in assets :gunpriest)
    (math/floor (- (self :render-x) (* w 0.5)))
    (math/floor (- (self :render-y) (* h 0.5)))
    :white))

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
  (loop [c :in hexas
         :let [dist (v/dist (c :pos) p)]
         :when (<= dist (c :radius))]
    (cond (nil? d)
      (do (set d dist)
        (set n c))

      (< dist d)
      (do
        (set d dist)
        (set n c))))
  n)

(defn snap-drag
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
      (let [world (first (g/find-named "World" s/gos))
            tile (hexa-hit (world :children) pos)]
        (when-let [{:pos p} tile]
          (def [nx ny] p)
          (-> self
              (put :x nx)
              (put :y ny)))))

    [:release _]
    (when (= (s/state :dragged-go) self)
      (-> s/state
          (put :dragged-go nil)
          (put :dragged-go-offset nil))))

  # take code from other drag thing
)

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
    [:press-LULE _]
    (when (in-rec? self pos)
      (put s/state :selected self))

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
