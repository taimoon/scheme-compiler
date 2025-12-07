(let ()
(define (fact n)
  (if (<= n 1)
      (begin (collect) 1)
      (* n (fact (- n 1)))))

(write (fact 10)) (newline)
)