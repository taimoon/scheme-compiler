(pretty-print (apply (lambda (x y) (+ x y)) '(2 3)))
(pretty-print (apply - '(7 2)))
(pretty-print (apply * ((lambda () (list 2 3)))))
(pretty-print (apply apply (list + (list 2 3))))