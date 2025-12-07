(let ()
(define (write-line x)
  (write x)
  (newline))

(define (list . x) x)

(apply write-line (list (apply (lambda (x y) (+ x y)) (list 2 3))))

(apply write-line (list (apply list (list 1 2 3 4))))

(let loop ((n 10) (acm 0))
  (if (<= n 0)
      (apply write-line (list acm))
      (apply loop (let ((n (- n 1)) (acm (+ acm n))) (list n acm)))))

(let ((f (lambda (x y . z)
          (if (< x y)
              z
              (cons x (cons y z))))))
  (define (%apply f xs) (apply f xs))
  (write-line (%apply f (list 1 2)))
  (write-line (%apply f (list -7 7 "reimu" "yes")))
  (write-line (%apply f (list 2 1 #t))))

(let ()
  (define (%apply f xs) (apply f xs))
  (define (plus x y) (+ x y))
  (write-line (%apply (lambda (x y) (+ x y)) (list 2 3)))
  (write-line (%apply plus (list 2 3)))
  (write-line (%apply (lambda (f vs) (%apply f vs)) (list plus (list 2 3))))
  (write-line (%apply %apply (list %apply (list plus (list 2 3)))))
)
)