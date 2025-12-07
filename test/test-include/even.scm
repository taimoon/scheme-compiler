(define (even? n)
  (if (<= n 0)
      #t
      (odd? (sub1 n))))