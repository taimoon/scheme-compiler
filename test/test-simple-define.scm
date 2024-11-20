(define (write-line x)
  (write x)
  (newline))

(let ()
  (define (odd? n)
    (if (= n 0)
        #f
        (even? (- n 1))))
  (define (even? n)
    (if (= n 0)
        #t
        (odd? (- n 1))))
  (write-line
    (and (even? 2) (odd? 3) (odd? 5))))

(let ()
  (define (fact n)
    (if (<= n 0)
        1
        (* n (fact (- n 1)))))
  (define (add-iter n m)
    (if (<= m 0)
        n
        (add-iter (+ n 1) (- m 1))))
  (write-line (fact 10))
  (write-line (add-iter 0 (fact 10))))

(let ()
  (define (make-counter x)
    (lambda (i) (set! x (+ x i)) x))
  (define counter (make-counter 0))
  (write-line (counter 2))
  (write-line (counter 3))
  (write-line (counter 5))
  (write-line (counter 7))
)