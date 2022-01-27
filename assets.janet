(use freja/flow)

(defonce assets
  (-> (seq [f :in (os/dir "assets")]
        (print "asset: " f)
        [(keyword (first (string/split "." f)))
         (load-texture (string "assets/" f))])
      from-pairs))
