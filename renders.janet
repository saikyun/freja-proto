(use freja/flow)

(defn draw-cat
  [self]
  (def {:x x
        :y y
        :radius radius} self)
  (draw-circle x y radius :blue))

(defn draw-rec
  [self]
  (def {:x x
        :y y
        :radius radius} self)
  (draw-rectangle x y radius radius :blue))

(defn drag
  [self ev]
  (pp ev)
  # take code from other drag thing
)
