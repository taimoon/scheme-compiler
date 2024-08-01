(load "convert-labels.scm")
(pretty-print
  (let loop ([obj '()] [i 0])
    (if (>= i 3)
        obj
        (loop (cons i obj) (add1 i)))))