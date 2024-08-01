(load "convert-labels.scm")
(pretty-print
  (let recur ([i 0])
    (if (>= i 3)
        '()
        (cons i (recur (add1 i))))))