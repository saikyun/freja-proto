(defn placeholder [v] nil)
(def wrapping-func
  (fn [self]
    (placeholder self)))

(defmacro fun
  ``
  Wraps symbol containing function `f` with a function.
  That function will get the name, sourcemap and source of `f`.
  The purpose is to allow tables to have the wrapped function
  as a property, but still work with `varfn` and `:redef true`.
  ``
  [f]
  (assert (symbol? f) (string "f must be a symbol, was: " f))
  (let [f ((dyn f) :ref)
        old-data (disasm (f 0))
        data (struct/to-table (disasm wrapping-func))]
    (tracev old-data)
    (tracev data)
    (put-in data [:constants 0] f)
    (put data :name (old-data :name))
    (put-in data [:sourcemap 0] (get-in old-data [:sourcemap 0]))
    (put data :source (old-data :source))
    (asm data)))

#(comment
  (varfn a2 [self] (+ (self :n) 10))

  (def o {:age (fun a2)
          :n 10})

  (pp (:age o))

  (varfn a2 [self] (+ (self :n) 1337))

  (pp (:age o))
  #
  #)