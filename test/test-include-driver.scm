(include "test/test-include-engine.scm")

(define (odd? n)
  (if (eq? n 0)
      #f
      (even? (- n 1))))

(write (and (even? 2) (odd? 3) (odd? 5)))
(newline)
