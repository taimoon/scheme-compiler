(pretty-print
  (let ([sqr (lambda (x) (* x x))])
    (sqr 2)
  )
)

(pretty-print
  (let ([abs (lambda (x) (if (< x 0) (- 0 x) x))])
    (abs -17)
  )
)

(pretty-print
  (let ([max (lambda (x y) (if (< x y) y x))])
    (max -10 3))
)

(pretty-print
  (let ([is_ordered (lambda (a b c d) (and (< a b) (< b c) (< c d)))])
    (is_ordered 2 3 5 7))
)

(pretty-print
  (let (
      [neg (lambda (x) (- 0 x))]
      [identity (lambda (x) x)]
      [x -17]
    )
    ((if (< x 0) neg identity) x)
  )
)

(pretty-print
  (let ([const (lambda () (lambda () 0))])
    (and (procedure? const) (procedure? (const)))))

(pretty-print
  (let ([const (lambda () (lambda () 0))])
    ((const))))

(pretty-print
  (let ([const (lambda (x) (lambda () "weird"))])
    ((const "something"))))

(pretty-print
  (let ([const (lambda (x) (lambda (y) "ah?"))])
    ((const (cons '() '())) (cons '() '()))))