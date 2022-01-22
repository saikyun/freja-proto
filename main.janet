(import freja/textarea :as t)
(import freja/new_gap_buffer :as gb)
(import freja/state)
(import freja/events :as e)
(import ./graph :as g)
(use freja/flow)

(var dragged nil)
(var target nil)

(defn force-refresh!
  []
  (e/put! state/editor-state :force-refresh true))

(defn render-all
  [el]

  (def mp
    (v/v-
      (get-mouse-position)
      [(el :render-x) (el :render-y)]))

  (when dragged
    (def [w h] (measure-text (dragged :name) :font :sans-serif))

    (def p (update mp 1 - h))
    (draw-rectangle ;p (+ w 4) (+ h 4) [0.1 0.1 0.1 0.8])
    (draw-text
      (dragged :name)
      (v/v+ p [2 2])
      :font :sans-serif
      :color [0.9 0.9 0.9]))

  (loop [go :in (g/gos :children)
         :when (go :render)]
    (update go :x
            |(if (bytes? $)
               (scan-number $)
               $))
    (update go :y
            |(if (bytes? $)
               (scan-number $)
               $))
    (update go :w
            |(if (bytes? $)
               (scan-number $)
               $))
    (update go :h
            |(if (bytes? $)
               (scan-number $)
               $))
    (:render go)))

(def state
  @{:freja/label "Editor"
    :root g/gos
    :expanded @{}})

(def text-size 24)
(def text-color [0.1 0.1 0.1])

(defn node-item [_ _]
  "hej")

(comment
  (grand-parent? dragged)
  #
)

(defn grand-parent?
  [parent child]
  (cond (= parent (child :parent))
    true

    (child :parent)
    (grand-parent? parent (child :parent))

    false))

(defn remove-child
  [parent child]
  (update parent :children |(filter |(not= $ child) $)))

(defn add-child
  [parent child]
  (update parent :children array/push child)
  (put child :parent parent))

(defn set-parent
  [node new-parent]
  (if (grand-parent? node new-parent)
    (print "Can't make grandchild into parent.")
    (do
      (remove-child (node :parent) node)
      (add-child new-parent node))))


(defn field
  [{:on-enter on-enter
    :label label}]
  (def size (math/floor (* 0.8 text-size)))
  (def props @{:text/color :white
               :text/size (+ 4 size)
               :height (+ 4 size)
               :background [0.1 0.1 0.1 0.4]
               :extra-binds @{:enter (fn [props] (on-enter (gb/content props)))}})

  [:row {}
   [:block {:weight 1}
    [t/textarea props]]
   (when label
     [:clickable {:on-click
                  (fn [_]
                    (on-enter
                      (gb/content
                        (props :internal-gb))))}
      [:background {:color [0.8 0.8 0.8]}
       [:padding {:all 2}
        [:text {:size size
                :color text-color
                :text label}]]]])])

(defn expandable-nodes
  [{:parent parent
    :expanded expanded
    :selected selected}]
  [:block {}
   ;(seq [n :in (parent :children)
          :let [expanded? (in expanded n)]]
      [node-item {:node n
                  :expanded expanded
                  :selected selected}])

   [:block {:width 300}
    [:padding {:bottom 16}
     [field {:label "Add"
             :on-enter (fn [label]
                         (add-child parent
                                    @{:name label
                                      :children @[]})
                         (force-refresh!))}]]]])

(defn in-el?
  [el [px py]]
  (def x (dyn :offset-x))
  (def y (dyn :offset-y))
  (def w (el :width))
  (def h (el :height))

  (and
    (>= px x)
    (<= px (+ x w))
    (>= py y)
    (<= py (+ y h))))

(defn node-item
  [{:node node
    :expanded expanded
    :selected selected}]
  (def {:name name
        :children children} node)
  (default name (string node))
  [:row {}
   [:block {:width 24}
    (when children
      [:clickable
       {:on-click (fn [_]
                    (update-in state [:expanded node] not)
                    (e/put! state/editor-state :force-refresh true))}
       [:text {:size text-size
               :text (if (expanded node) "- " "+ ")}]])]

   [:block {:weight 1}
    [:block
     # :clickable
     {:on-click (fn [_]
                  (put state :selected node)
                  (e/put! state/focus :focus node)
                  (e/put! state/editor-state :force-refresh true))}
     [:event-handler {:on-event
                      (fn [el ev]
                        #(pp ev)
                        (match ev
                          [:press pos]
                          (when (in-el? el pos)
                            (set dragged node)
                            (force-refresh!)
                            false)

                          [:drag pos]
                          (when (and dragged
                                     (not= dragged node))
                            (cond
                              (and (not= target node)
                                   (in-el? el pos)
                                   (not (grand-parent? dragged node)))
                              (do
                                (set target node)
                                (force-refresh!)
                                true)

                              (and (= target node)
                                   (not (in-el? el pos)))
                              (do
                                (set target nil)
                                (force-refresh!)
                                true)))

                          [:release pos]
                          (when (and dragged
                                     (in-el? el pos)
                                     (not= dragged node))
                            (set-parent dragged node)
                            (set dragged nil)
                            (set target nil)
                            (force-refresh!)
                            true)))}
      [:block {}
       [:background {:color (cond (= selected node) [0.1 0.1 0.1]

                              (= dragged node)
                              [0.8 0.8 0.8]

                              (= target node)
                              [0.6 0.8 0.6]

                              [0 0 0 0])}
        [:text {:size text-size
                :color (cond (= selected node)
                         [0.9 0.9 0.9]

                         [0.1 0.1 0.1])
                :text name}]]]]]

    [:block {}
     (when (and (expanded node) children)
       [expandable-nodes {:parent node
                          :expanded expanded
                          :selected selected}])]]])

(defn second
  [vs]
  (in vs 1))

(def ignored {:children 1
              :editor-state 1
              :render 1})


(defn editor-window
  [node]
  (let [elems (seq [[k v] :pairs node
                    :when (not (ignored k))]
                [[:block {:height (+ 2 text-size)}
                  [:text {:size text-size
                          :text (string k ": ")}]]
                 [:block {:width 200
                          :height (+ 2 text-size)}
                  [t/textarea
                   @{#:state (node :editor-state)
                     :text/color :white
                     :text/size text-size
                     :text (string v)
                     :height text-size
                     :extra-binds
                     @{:enter (fn [props]
                                (print "hit enter")
                                (put node k (gb/content props))
                                (e/put! state/editor-state :force-refresh true))}}]
                  #[:text {:size text-size
                  #        :text (string v)}]
]])]
    [:row {:id (math/random)}
     [:block {:weight 1}
      ;(map first elems)
      [field {:label "Add prop:"
              :on-enter (fn [content]
                          (put node (keyword content) @"")
                          (force-refresh!))}]]
     [:block {:weight 1}
      ;(map second elems)]]))

(defn component
  [props]
  (def {:root root
        :expanded expanded
        :selected selected} props)
  [:block {}
   [:event-handler {:on-event (fn [_ ev]
                                (match ev
                                  [:release _]
                                  (when dragged
                                    (print "top")
                                    (set dragged nil)
                                    (set target nil)
                                    false)))}
    [:background {:color [0.9 0.9 0.9]}
     [:padding {:all 6}
      [:block {}
       [:row {}
        [:block {}
         [node-item {:node root :expanded expanded :selected selected}]]
        (when selected
          (unless (selected :editor-state)
            (put selected
                 :editor-state
                 @{}))
          [:block {}
           [:padding {:left 12}
            [editor-window selected]]])]]]]]

   [:block {}
    [custom {:render render-all}]]])

(e/put! state/editor-state :other [component state])
