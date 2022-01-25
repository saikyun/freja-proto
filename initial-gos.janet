(import ./state :as s)
(import ./graph :as g)
(import ./initer)
(import ./wrap)
(import ./renders)

(comment
  (put s/gos :children @[])

  (def renders
    @{:name "render"
      :children @[]})

  (g/add-child
    renders
    @{:name "cat"
      :x 200
      :y 200
      :radius 50
      :children @[]
      :render (wrap/funf 'renders/draw-cat)})

  (comment g/add-child
           renders
           @{:name "dog"
             :x 50
             :children @[]
             :render (wrap/fun renders/draw-cat)})

  (g/add-child s/gos renders)

  (initer/force-refresh!)

  (comment
    (state :selected)
    (put (state :selected)
         :tick
         (fn [self]
           (loop [c :in (self :children)
                  :when (c :render)]
             (:render c))))

    (put (state
           (put s/gos :children @[])
           :render
           (fn [self] (draw-cat self)))
         #
)))
