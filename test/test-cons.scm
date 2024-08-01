(pretty-print 
  (pair? (cons '() '())))
(pretty-print
  (pair? '()))
(pretty-print
  (cons 1 2))
(pretty-print
  (cons 1 (cons 2 '())))
(pretty-print
  (cons 1 (cons (cons 1 '()) (cons #f #\t))))
(pretty-print
  (list 1 2 3 4 5))
(pretty-print
  (car (cons 1 2)))
(pretty-print
  (cdr (cons 1 2)))
(let ([xs (cons (cons 1 2) (cons 3 '()))])
  (pretty-print (eq? xs xs)))
(let ([xs (cons (cons 1 2) (cons 3 '()))])
  (pretty-print (cons (car (car xs)) (cdr xs))))
(let ([xs (cons (cons 1 2) (cons 3 4))])
  (set-car! xs (car (car xs)))
  (set-cdr! (cdr xs) '())
  (pretty-print xs))