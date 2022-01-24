(import freja/open-file)

(defn jump-to-function
  [f]
  (let [{:name name
         :source source
         :sourcemap sourcemap} (disasm f)
        #mod (require source)
        #{:source-map source-map} (mod (symbol name))
        ]
    (open-file/open-file
      source
      (dec (get-in sourcemap [0 0]))
      )))

(comment
  (func->file-line-col func->file-line-col)
  #
  )