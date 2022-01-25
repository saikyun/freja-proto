(defn placeholder [v] nil)

(def wrapping-func
  (fn [self]
    (placeholder self)))

(def wrapping-func2
  (fn [self o]
    (placeholder self o)))

(defn funf
  ``
  Wraps symbol containing function `f` with a function.
  That function will get the name, sourcemap and source of `f`.
  The purpose is to allow tables to have the wrapped function
  as a property, but still work with `varfn` and `:redef true`.
  ``
  [f &keys {:env env}]
  (assert (or (symbol? f)
              (and (dictionary? f)
                   (get f :ref)))
          (string "f must be a symbol or have :ref key, was: " f))

  (default env (curenv))

  (let [f (or (and (dictionary? f)
                   (get f :ref))
              ((get env f) :ref))
        old-data (disasm (f 0))
        arity (old-data :max-arity)
        data (struct/to-table (disasm (case arity
                                        1 wrapping-func
                                        2 wrapping-func2)))]
    #(tracev old-data)
    #(tracev data)
    (put-in data [:constants 0] f)
    #(put-in data [:constants 1] f)
    (put data :name (string "<wrapped> " (old-data :name)))
    #(put-in data [:sourcemap 0] (get-in old-data [:sourcemap 0]))
    (put data :source "const-1")
    #(tracev data)
    (asm data)))

(comment defmacro fun
         ``
  Wraps symbol containing function `f` with a function.
  That function will get the name, sourcemap and source of `f`.
  The purpose is to allow tables to have the wrapped function
  as a property, but still work with `varfn` and `:redef true`.
  ``
         [f &keys {:env env}]
         (assert (or (symbol? f)
                     (and (dictionary? f)
                          (get f :ref)))
                 (string "f must be a symbol or have :ref key, was: " f))

         (default env (curenv))

         (let [f (or (and (dictionary? f)
                          (get f :ref))
                     ((get env f) :ref))
               old-data (disasm (f 0))
               arity (old-data :max-arity)
               data (struct/to-table (disasm (case (tracev arity)
                                               1 wrapping-func
                                               2 wrapping-func2)))]
           #(tracev old-data)
           #(tracev data)
           (put-in data [:constants 0] f)
           #(put-in data [:constants 1] f)
           (put data :name (string "<wrapped> " (old-data :name)))
           #(put-in data [:sourcemap 0] (get-in old-data [:sourcemap 0]))
           (put data :source "const-1")
           #(tracev data)
           (asm data)))

#(comment
(varfn a [self] (+ (self :n) 10))

(def o {:age (funf 'a)
        :n 10})

(pp (:age o))

(varfn a2 [self] (+ (self :n) 1337))

(pp (:age o))


(varfn a2 [self o] (+ (self :n) 10 o))

(def o {:age (funf 'a2)
        :n 10})

(pp (:age o 100))

(varfn a2 [self o] (+ (self :n) 1337 o))

(pp (:age o 100))
#
#)

