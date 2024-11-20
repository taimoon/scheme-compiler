#|
How to run:
scheme --script make-compiler.scm
scheme --script compiler.so -o /tmp/test-simple-gc-0.out test/test-simple-gc-0.scm
/tmp/test-simple-gc-0.out
|#
(let iter ((n 10) (acm 1))
  (cond
    ((<= n 1)
     (%walk-stack)
     (write acm) (newline)
     acm)
    (else
     (iter (- n 1) (* acm n)))))

(define (fact n)
  (cond
    ((<= n 1)
     (%walk-stack)
     n)
    (else
     (* n (fact (- n 1))))))

(write (fact 10)) (newline)
