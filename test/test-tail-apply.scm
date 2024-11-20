(define (write-line x)
  (write x)
  (newline))

(define (fact n)
  (if (<= n 0)
      1
      (* n (fact (- n 1)))))

(let loop ((x 0) (y (fact 10)))
  (if (<= y 0)
      (apply write-line (list x))
      (apply loop (list (+ x 1) (- y 1)))))
