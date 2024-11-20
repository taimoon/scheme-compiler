(define (write-line x)
  (write x)
  (newline))

(define (odd? n)
    (if (= n 0)
        #f
        (even? (- n 1))))

(define (even? n)
  (if (= n 0)
      #t
      (odd? (- n 1))))

(define IMM-SHIFT 8)

(write-line IMM-SHIFT)
(set! IMM-SHIFT 10)
(write-line IMM-SHIFT)