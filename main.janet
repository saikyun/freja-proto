(import ./graph :as g)
(use freja/flow)
(import ./initer)
(import freja/open-file)
(import ./state :as s)

(comment
  (func->file-line-col func->file-line-col)
  #
)

(defn draw-cat :cat
  [self]
  (draw-circle (get self :x 100) 100 50
               :blue
               #:red
))

(put s/gos :children @[])

(def renders
  @{:name "render"
    :tick (fn [self] (loop [c :in (self :children)
                            :when (c :render)]
                       (:render c)))
    :children @[]})

(g/add-child
  renders
  @{:name "cat"
    :x 200
    :children @[]
    :render |(draw-cat $)})

(g/add-child
  renders
  @{:name "dog"
    :x 50
    :children @[]
    :render |(draw-cat $)})

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

  (put (state :selected)
       :render
       (fn [self] (draw-cat self)))
  #
)
