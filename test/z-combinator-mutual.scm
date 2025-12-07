(let ()
(define Z
  (lambda (f)
    ((lambda (x) (f (lambda (v) ((x x) v))))
     (lambda (x) (f (lambda (v) ((x x) v)))))))
(define pare
  (lambda (x y)
    (lambda (s) (s x y))))
(define kar (lambda (p) (p (lambda (x y) x))))
(define kdr (lambda (p) (p (lambda (x y) y))))
(define even::odd
  (Z
    (lambda (even::odd)
      (pare
        (lambda (x)
          (if (<= x 0) #t ((kdr even::odd) (- x 1))))
        (lambda (x)
          (if (<= x 0) #f ((kar even::odd) (- x 1))))))))
(define even? (kar even::odd))
(define odd? (kdr even::odd))

(define (writeln x) (write x) (newline))
(writeln (not (even? 12345)))
(writeln (not (odd? 123456)))
)