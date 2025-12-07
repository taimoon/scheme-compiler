(letrec ((even?
          (lambda (n)
            (if (eq? n 0)
                #t
                (odd? (- n 1)))))
         (odd?
          (lambda (n)
            (if (eq? n 0)
                #f
                (even? (- n 1))))))
(write (and (even? 2) (not (odd? 2))
            (not (even? 3)) (odd? 3)
            (even? 4) (not (odd? 4))
            (not (even? 5)) (odd? 5)))
(newline)
)