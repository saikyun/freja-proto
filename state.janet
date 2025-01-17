(import freja/state)
(import freja/events :as e)

(defn force-refresh!
  []
  (e/put! state/editor-state :force-refresh true))

(def gos
  @{:name "root"
    :children @[]})

(def state
  @{:freja/label "Editor"
    :root gos
    :expanded @{}})

(def anims @[])

(defmacro anim
  [& body]
  ~(array/push ',anims
               (fiber/new (fn [] ,;body))))

(comment
  # (macex '(anim 123))
  #
)

(def number-of-turns 3)

(def height-scale 0.6)

(def queue @[])
