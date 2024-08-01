(pretty-print 
  (pair? '(() ())))
(pretty-print
  (pair? '()))
(pretty-print
  '(1 . 2))
(pretty-print
  '(1 2))
(pretty-print
  '(1 (1) #f . #\t))
(pretty-print
  '(1 2 3 4 5))
(pretty-print
  (car '(1 2)))
(pretty-print
  (cdr '(1 2)))
(let ([xs '((1 . 2) 3)])
  (pretty-print (eq? xs xs)))
(let ([xs '((1 . 2) 3)])
  (pretty-print (cons (car (car xs)) (cdr xs))))
(let ([xs '((1 . 2) 3)])
  (set-car! xs (car (car xs)))
  (set-cdr! (cdr xs) '())
  (pretty-print xs))