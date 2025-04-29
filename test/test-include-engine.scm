(define (even? n)
  (if (eq? n 0)
      #t
      (odd? (- n 1))))
