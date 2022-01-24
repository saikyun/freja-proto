(use freja/flow)

(defn map-tree
  [f t]
  (when-let [cs (in t :children)]
    (loop [c :in cs]
      (map-tree f c)))
  (f t))

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
