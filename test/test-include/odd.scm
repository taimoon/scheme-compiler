(define (odd? n)
  (if (<= n 0) #f (even? (sub1 n))))