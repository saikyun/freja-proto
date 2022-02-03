(import ./graph :as g)
(use freja/flow)
(import ./initer)
(import freja/open-file)
(import ./state :as s)
(import ./renders)
(import ./genwo)
(import ./wrap)
(import ./navigation :as nav)
(import ./initial-gos)

(comment
  (func->file-line-col func->file-line-col)

  (-> (seq [[k v] :pairs form
            :let [v (if (function? v)
                      "hej"
                      v)]]
        [k v])
      from-pairs)

  (g/add-child s/gos @{:children @[]
                       :editor-state @{}
                       :h 50
                       :name "Gunpriest"
                       :on-event @{:freja/func true :name "snap-drag" :source "renders.janet"}
                       :render @{:freja/func true :name "texture" :source "renders.janet"}
                       :render-x 174.851
                       :render-y 265.6
                       :w 50
                       :x 174.851
                       :y 265.6})

  (pp s/gos)
  #
)

(defn traverse
  [node]
  (-> (seq [[k v] :pairs node
            :when (not= k :parent)
            # TODO: fix this
            :when (not= k :hover)
            :let [v (cond
                      (= k :children)
                      (map traverse v)

                      (function? v)
                      (-> (nav/get-source v)
                          (put :sourcemap nil)
                          (put :freja/func true))

                      v)]]
        [k v])
      from-pairs))

(defn persist
  []
  (def data @"")
  (xprintf data "%m" (traverse s/gos))
  #(pp data)
  (spit "saved.jdn" data))

(defn load-traverse
  [node]
  (def new-node
    (-> (seq [[k v] :pairs node
              :let [v (cond
                        (= k :children)
                        (map load-traverse v)

                        (and (dictionary? v)
                             (v :freja/func))
                        (let [{:source source
                               :name name} v
                              env (require source)]
                          (try
                            (do
                              (assert (get env (symbol name))
                                      (string/format
                                        ``could not find function: %m
in env with keys: %m`` v env))
                              (wrap/funf (symbol name) :env env))
                            ([err fib]
                              (debug/stacktrace fib err)
                              nil)))

                        v)]]
          [k v])
        from-pairs))

  (update new-node :children |(when $ (map |(put $ :parent new-node) $)))

  new-node)

(defn load
  []
  (->> (slurp "saved.jdn")
       parse
       load-traverse))

(put s/state :freja/quit (fn [data] (persist)))

(defn bigload
  []
  (def res (load))
  #(pp res)
  (table/clear s/gos)
  (merge-into s/gos res)

  (map |(put $ :parent s/gos) (s/gos :children))

  (s/force-refresh!))

(bigload)

(comment
  (persist)

  (put s/gos :dog true)
  #
)

(comment
  #
)
