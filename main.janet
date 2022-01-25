(import ./graph :as g)
(use freja/flow)
(import ./initer)
(import freja/open-file)
(import ./state :as s)
(import ./renders)
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

  (pp s/gos)
  #
)

(defn traverse
  [node]
  (-> (seq [[k v] :pairs node
            :when (not= k :parent)
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
  (pp data)
  (spit "saved.jdn" data))

(defn load-traverse
  [node]
  (-> (seq [[k v] :pairs node
            :let [v (cond
                      (= k :children)
                      (map load-traverse v)

                      (and (dictionary? v)
                           (v :freja/func))
                      (let [{:source source
                             :name name} v
                            env (require source)]
                        (assert (get env (symbol name))
                                (string/format
                                  ``could not find function: %m
in env with keys: %m`` v env))
                        (wrap/funf (symbol name) :env env))

                      v)]]
        [k v])
      from-pairs))

(defn load
  []
  (->> (slurp "saved.jdn")
       parse
       load-traverse))

(put s/state :freja/quit (fn [data]
                           (persist)
                           (pp data)
                           (print "lul")))

(defn bigload
  []
  (def res (load))
  (pp res)
  (table/clear s/gos)
  (merge-into s/gos res)
  (initer/force-refresh!))

(bigload)

(comment
  (persist)
  #
)

(comment
  #
)
