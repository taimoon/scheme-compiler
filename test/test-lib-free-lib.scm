(define (write-line x)
  (write x)
  (newline))

(define (odd? n)
    (if (= n 0)
        #f
        (even? (- n 1))))

(define IMM-SHIFT 8)
