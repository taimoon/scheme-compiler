(letrec ((mul
          (lambda (n m)
            (let loop ((n n) (acm 0))
              (if (<= n 0)
                  acm
                  (loop (- n 1) (+ acm m)))))))
  ;;; TODO: buggy if cross-compile from i686 to x86-64
  ; (write (= (mul 123456789 2) (* 123456789 2)))
  (write (= (mul 12345678 2) (* 12345678 2)))
  (newline))