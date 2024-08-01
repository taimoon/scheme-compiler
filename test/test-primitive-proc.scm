(define (map f xs)
  (if (pair? xs)
      (cons (f (car xs)) (map f (cdr xs)))
      '()))

(pretty-print
  (map add1 (list 1 2 3)))

(pretty-print
  (map integer? (list 2 3 5 (list 1 2 3) (vector) '() #t "something")))