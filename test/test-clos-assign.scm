(let* ([add! (lambda (x) (lambda () (set! x (add1 x)) x))]
       [f (add! 0)])
  (pretty-print (f))
  (pretty-print (f))
  (pretty-print (f)))

(let* ([x 0]
         [f (lambda () (set! x (add1 x)) x)])
    (pretty-print (f))
    (pretty-print (f))
    (pretty-print (f)))

(let* ([add! (lambda (x) (lambda (y) (set! x (+ y x)) x))]
       [f (add! 0)])
  (pretty-print (f 2))
  (pretty-print (f 3))
  (pretty-print (f 5)))