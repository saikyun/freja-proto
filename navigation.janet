(import freja/open-file)

(defn get-source
  [f]
  (let [data (disasm f)
        data (if (= (data :source) "const-1")
               (disasm (get-in data [:constants 0 0]))
               data)]
    (-> (filter |({:name 1 :source 1 :sourcemap 1} (first $)) (pairs data))
        from-pairs)))

(defn jump-to-function
  [f]
  (let [{:name name
         :source source
         :sourcemap sourcemap} (get-source f)
        #mod (require source)
        #{:source-map source-map} (mod (symbol name))
]
    #(tracev source)
    #(tracev sourcemap)
    (open-file/open-file
      source
      (dec (get-in sourcemap [0 0])))))

(comment
  (func->file-line-col func->file-line-col)
  #
)
