(import freja/textarea :as t)
(import freja/new_gap_buffer :as gb)
(import freja/state)
(import freja/events :as e)
(import ./graph :as g)
(use freja/flow)
(import ./navigation :as nav)
(import ./state :as s)
(import ./wrap)

(defn force-refresh!
  []
  (e/put! state/editor-state :force-refresh true))

(defn render-self
  [self]
  (when (self :render)
    (:render self)))

(defn on-event-self
  [self ev]
  (when (self :on-event)
    (:on-event self ev)))

(defn tick-all
  [el]
  (def mp
    (v/v-
      (get-mouse-position)
      [(el :render-x) (el :render-y)]))

  (g/map-tree render-self s/gos)

  (when (s/state :dragged)
    (def [w h] (measure-text ((s/state :dragged) :name) :font :sans-serif))

    (def p (update mp 1 - h))
    (draw-rectangle ;p (+ w 4) (+ h 4) [0.1 0.1 0.1 0.8])
    (draw-text
      ((s/state :dragged) :name)
      (v/v+ p [2 2])
      :font :sans-serif
      :color [0.9 0.9 0.9])))

(defn on-event
  [ev]
  (def kind (first ev))
  (def new-ev
    (cond
      (or (= kind :press)
          (= kind :mouse-move)
          (= kind :drag)
          (= kind :release)
          (= kind :double-click)
          (= kind :triple-click))
      [kind (v/v- (ev 1) [(dyn :offset-x)
                          (dyn :offset-y)])
       ;(drop 2 ev)]

      (or (= kind :scroll))
      [kind (ev 1) (v/v- (ev 2) [(dyn :offset-x)
                                 (dyn :offset-y)])
       ;(drop 3 ev)]

      (printf "unrecog ev: %m" ev)))

  (g/map-tree |(on-event-self $ new-ev) s/gos))

(def text-size 24)
(def text-color [0.9 0.9 0.9])
(def bg [0.2 0.2 0.2])
(def scene-bg [0.3 0.4 0.3])
(def border [0.3 0.3 0.3])

(defn node-item [_ _]
  "hej")

(comment
  #
)

(defn field
  [{:on-enter on-enter
    :label label
    :value value}]
  (def size (math/floor (* 0.8 text-size)))
  (def props @{:text/color :white
               :text/size (+ 4 size)
               :height (+ 4 size)
               :text value
               :background [0.1 0.1 0.1 0.4]
               :extra-binds @{:enter (fn [props] (on-enter (gb/content props)))}})

  (if-not label
    [t/textarea props]
    [:row {}
     [:block {:weight 1}
      [t/textarea props]]
     [:clickable {:on-click
                  (fn [_] (on-enter (gb/content (props :internal-gb))))}
      [:background {:color bg}
       [:padding {:all 2}
        [:text {:size size
                :color text-color
                :text label}]]]]]))

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
                         (g/add-child parent
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
                    (update-in s/state [:expanded node] not)
                    (e/put! state/editor-state :force-refresh true))}
       [:text {:size text-size
               :color text-color
               :text (if (expanded node) "- " "+ ")}]])]

   [:block {:weight 1}
    [:block {}
     [:event-handler {:on-event
                      (fn [el ev]
                        #(pp ev)
                        (match ev
                          [:press pos]
                          (when (in-el? el pos)
                            (put s/state :dragged node)
                            #(force-refresh!)
                            false)

                          [:drag pos]
                          (when (and (s/state :dragged)
                                     (not= (s/state :dragged) node))
                            (cond
                              (and (not= (s/state :target) node)
                                   (in-el? el pos)
                                   (not (g/grand-parent? (s/state :dragged) node)))
                              (do
                                (put s/state :target node)
                                (force-refresh!)
                                true)

                              (and (= (s/state :target) node)
                                   (not (in-el? el pos)))
                              (do
                                (put s/state :target nil)
                                (force-refresh!)
                                true)))

                          [:release pos]
                          (when (and (s/state :dragged)
                                     (in-el? el pos))
                            (if (= (s/state :dragged) node)
                              (do
                                (put s/state :selected node)
                                (e/put! state/focus :focus node))
                              (g/set-parent (s/state :dragged)
                                            node))

                            (put s/state :dragged nil)
                            (put s/state :target nil)

                            (force-refresh!)

                            true)))}
      [:block {}
       [:background {:color (cond
                              (= (s/state :dragged) node)
                              [0.3 0.3 0.3]

                              (= (s/state :target) node)
                              [0.3 0.5 0.3]

                              (= selected node)
                              [0.1 0.1 0.1]

                              [0 0 0 0])}
        [:text {:size text-size
                :color (cond (= selected node)
                         [0.9 0.9 0.9]

                         text-color)
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
              :parent 1})

(defn on-enter
  [node k value]
  (print "hit enter")
  (def new-v (parse value))
  (def new-v (if (symbol? new-v)
               (let [parts (string/split "/" new-v)
                     path (-> (take (dec (length parts)) parts)
                              (string/join "/"))
                     sym (symbol (last parts))]
                 (wrap/funf sym :env (require (string path)))
                 #
)
               new-v))
  (put node k new-v)
  (e/put! state/editor-state :force-refresh true))


(defn edit-prop
  [node k v]
  (cond
    (function? v)
    [:row {}
     [:block {:weight 1}
      [:clickable {:on-click (fn [_]
                               (nav/jump-to-function v))}
       [:text {:size text-size
               :color text-color
               :text "jump"}]]]
     [:block {:width 300}
      [field {:on-enter (partial on-enter node k)
              :value (get (disasm v) :name "<anonymous function>")}]]]

    :else
    [field {:on-enter (partial on-enter node k)
            :value (string/format "%p" v)}]))

(defn editor-window
  [node]
  (let [elems (seq [[k v] :pairs node
                    :when (not (ignored k))]
                [[:block {:height (+ 2 text-size)}
                  [:text {:size text-size
                          :color text-color
                          :text (string k ": ")}]]
                 [:block {:width 400
                          :height (+ 2 text-size)}
                  (edit-prop node k v)]])]
    [:block {}
     [:row {:id (math/random)}
      [:block {:weight 1}
       ;(map first elems)]
      [:block {:weight 1}
       ;(map second elems)]]
     [field {:label "+ Prop"
             :on-enter (fn [content]
                         (put node (keyword content) @"")
                         #(put node :render (fn [self] (g/draw-cat self)))
                         (force-refresh!))}]]))

(defn component
  [props]
  (def {:root root
        :expanded expanded
        :selected selected} props)
  [:background {:color border}
   [:padding {:left 2}
    [:event-handler {:on-event (fn [_ ev]
                                 (match ev
                                   [:release _]
                                   (when (s/state :dragged)
                                     (print "top")
                                     (put s/state :dragged nil)
                                     (put s/state :target nil)
                                     false)))}
     [:background {:color bg}
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

    [:background {:color scene-bg}
     [:block {}
      [custom {:render tick-all
               :on-event on-event}]]]]])

(e/put! state/editor-state :other [component s/state])
