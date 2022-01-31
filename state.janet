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

(def height-scale 0.75)

(def queue @[])