(use freja/flow)

(import ./state :as s)

(defn map-tree
  [f t]
  (f t)
  (when-let [cs (in t :children)]
    (loop [c :in cs]
      (map-tree f c))))

(defn find-nodes*
  [f t res]
  (when (f t)
    (array/push res t))

  (when-let [cs (in t :children)]
    (loop [c :in cs]
      (find-nodes* f c res)))

  res)

(defn find-nodes
  [f t]
  (find-nodes* f t @[]))

(defn find-named
  [name &opt t]
  (default t s/gos)
  (find-nodes |(= name (in $ :name)) t))

(comment
  (def tree {:name "a"
             :children [{:name "b"}]})
  (find-nodes |(= ($ :name) "b") tree)
  (find-named "b" tree)
  #
)

(defn grand-parent?
  [parent child]
  (cond (= parent (child :parent))
    true

    (child :parent)
    (grand-parent? parent (child :parent))

    false))

(defn add-child
  [parent child]
  (update parent :children array/push child)
  (put child :parent parent))

(varfn set-parent [node new-parent])

(defn remove-child
  [parent child]
  (map (partial add-child s/gos) (child :children))
  (update parent :children |(filter |(not= $ child) $)))

(defn delete
  [child]
  (if-not (child :parent)
    (printf "Child %m has no parent." child)
    (remove-child (child :parent) child)))

(defn delete-children
  [parent]
  (map delete (array ;(parent :children))))

(defn set-parent
  [node new-parent]
  (if (grand-parent? node new-parent)
    (print "Can't make grandchild into parent.")
    (do
      (when (node :parent)
        (remove-child (node :parent) node))
      (add-child new-parent node)))
  node)

(defn new
  [name &keys {:parent parent}]
  (default parent s/gos)

  (-> @{:name name
        :children @[]}
      (set-parent parent)))

(defn clone
  [to-clone &keys {:parent parent}]
  (default parent (to-clone :parent))

  (-> (seq [[k v] :pairs to-clone]
        [k (case (type v)
             :table (table/clone v)
             :array (array ;v)
             :buffer (buffer ;v)
             v)])
      from-pairs
      (set-parent parent)))
