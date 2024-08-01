(pretty-print
  (let ((x 3) (y 4) (z (* 5 5)))
    (- z (+ (* x x) (* y y)))))

(pretty-print
  (let* (
    [x 3]
    [x (* x x)]
    [y 4]
    [y (* y y)]
    [z (let ((x 3) (y 4) (z (* 5 5))) (+ (* x x) (* y y)))]
  )
    (- z (+ x y)))
)