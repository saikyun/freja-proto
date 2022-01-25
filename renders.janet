(use freja/flow)
(import ./state :as s)

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
  (def {:x x
        :y y
        :radius radius} self)
  (draw-rectangle x y radius radius :blue))

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
  (pp ev)
  (def pos (if (= :scroll (first ev))
             (ev 2)
             (ev 1)))
  (def hit (in-rec? self pos))

  (match ev
    [:press _]
    (when hit
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
