(pretty-print
  (let ([const (lambda (x) (lambda () x))])
    ((const 0))
  )
)

(pretty-print
  (let ([add (lambda (x) (lambda (y) (+ x y)))])
    ((add 2) 3))
)

(pretty-print
  (let ([pair (lambda (x) (lambda (y) (list x y)))])
    ((pair 2) 3))
)

(pretty-print
  (let* ([pair (lambda (x) (lambda (y) (list x y)))]
         [p (cons "indefinite extend" "no")]
         [res ((pair p) (list 0 1 3))])
    (set-cdr! p "yes")
    res)
)
