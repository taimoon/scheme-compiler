(load "match.scm")

(define (calc x)
  (match x
    [,() (guard (integer? x)) x]
    [(+ ,x ,y) (+ (calc x) (calc y))]
    [(* ,x ,y) (* (calc x) (calc y))]
    [(- ,x ,y) (- (calc x) (calc y))]))

(pretty-print
  (calc '(+ (* 3 3) (* 5 5))))

(pretty-print
  (calc '(+ (* (- 4 1) 3) (* 5 (+ 3 2)))))