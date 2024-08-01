(load "convert-labels.scm")
(pretty-print
  (labels ([sqr (code (x) (* x x))])
    (sqr 2)
  )
)

(pretty-print
  (labels ([abs (code (x) (if (< x 0) (- 0 x) x))])
    (abs -17)
  )
)

(pretty-print
  (labels ([max (code (x y) (if (< x y) y x))])
    (max -10 3))
)

(pretty-print
  (labels ([fib (code (n)
                  (if (< n 2) 
                      1 
                      (+ (fib (- n 1)) (fib (- n 2)))))])
    (fib 10)
  )
)

(pretty-print
  (labels (
    [even (code (n) (if (eq? n 0) #t (odd (sub1 n))))]
    [odd (code (n) (if (eq? n 0) #f (even (sub1 n))))]
  )
    (and (even 2) (odd 3) (odd 5))
  )
)

(pretty-print
  (labels ([is_ordered (code (a b c d) (and (< a b) (< b c) (< c d)))])
    (is_ordered 2 3 5 7))
)

(pretty-print
  (labels (
      [neg (code (x) (- 0 x))]
      [identity (code (x) x)]
    )
  (let ([x -17])
    (labelcall (if (< x 0) neg identity) x))
  )
)
