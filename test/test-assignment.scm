(pretty-print
  (let ([x 2])
    (set! x 3)
    x)
)

(pretty-print
  (let ([x 2])
    (set! x (+ x 3))
    x)
)

(pretty-print
  (let ([x (let ([x 2]) (set! x (+ x 3)) x)])
    (set! x (+ x 2))
    x)
)