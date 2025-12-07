(let ()
(define Z
  (lambda (f)
    ((lambda (x) (f (lambda (v) ((x x) v))))
     (lambda (x) (f (lambda (v) ((x x) v)))))))

(define Z*
  (lambda (f)
    ((lambda (x) (f (lambda v (apply (x x) v))))
     (lambda (x) (f (lambda v (apply (x x) v)))))))

(define mul
  (let ((f
         (Z* (lambda (f)
               (lambda (m n s)
                 (if (<= n 0) s (f m (- n 1) (+ s m))))))))
    (lambda (m n) (f m n 0))))

(define fact
  (Z (lambda (f)
      (lambda (n)
        (if (<= n 1)
            1
            (* n (f (- n 1))))))))

(define (fact* n)
  (if (<= n 1) 1 (* n (fact* (- n 1)))))

(define fib
  (Z
    (lambda (f)
      (lambda (n)
        (if (<= n 1) n (+ (f (- n 1)) (f (- n 2))))))))

(define (writeln n) (write n) (newline))

(writeln (= (fact 10) (fact* 10)))
(writeln (eq? (mul 11 (fact 10)) (fact* 11)))
(writeln (fib 10))
)