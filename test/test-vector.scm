(load "compiler.scm")

(pretty-print
  (vector? (make-vector 0))
)

(pretty-print
  (let ([xs (make-vector 3)])
    (vector-set! xs 0 17)
    (vector-set! xs 1 23)
    (vector-set! xs 2 29)
    xs)
)

(pretty-print
  (let ([xs (make-vector 3)])
    (vector-set! xs 0 #t)
    (vector-set! xs 1 #\c)
    (vector-set! xs 2 "vector")
    xs)
)

(pretty-print
  (vector-length (make-vector 3))
)

(pretty-print
  (let ([xs (make-vector 3)])
    (vector-set! xs 2 29)
    (vector-ref xs 2))
)

(pretty-print
  (let ([xs (make-vector 3)])
    (vector-set! xs 0 2)
    (vector-set! xs 1 (vector-length xs))
    (vector-set! xs 2 (+ (vector-ref xs 0) (vector-ref xs 1)))
    xs
    )
)

(pretty-print
  (vector-length (vector 1 2 3))
)

(pretty-print
  (vector 1 2 3)
)

(pretty-print
  (vector (vector (vector #t) #\t) (vector (list 1 2 3) "compiler"))
)

(pretty-print
  (let ([xs (vector 0 0 0)])
    (vector-set! xs 0 2)
    (vector-set! xs 1 (vector-length xs))
    (vector-set! xs 2 (+ (vector-ref xs 0) (vector-ref xs 1)))
    xs)
)

(pretty-print
  (let ([xs (make-vector (if #t 3 4))])
    (vector-set! xs 0 2)
    (vector-set! xs 1 (vector-length xs))
    (vector-set! xs 2 (+ (vector-ref xs 0) (vector-ref xs 1)))
    xs)
)
