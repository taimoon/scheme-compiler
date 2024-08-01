(load "convert-labels.scm")
(pretty-print
  (let loop ([obj '()] [t 'something] [i 0])
    (if (> i 10000)
        obj
        (let ([x (vector 1 2 3 4)]
              [y (list 1 2 3 4 5 6 7 8 9 10)]
              [z "something"])
        (if (and (eq? (string->symbol (symbol->string t)) (string->symbol (symbol->string t)))
                 (eq? (string->symbol (symbol->string t)) t)
                 (eq? t 'something))
            (loop (list x y z) t (add1 i))
            (list t 'something
                  (symbol->string t) (symbol->string 'something)
                  (eq? (string->symbol (symbol->string t)) (string->symbol (symbol->string t)))
                  (eq? (string->symbol (symbol->string t)) t)
                  (eq? t 'something)
                  ))))))