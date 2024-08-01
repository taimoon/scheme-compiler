(load "utils.scm")
(define (count-cons e)
  (if (pair? e)
      (add1 (+ (count-cons (car e)) (count-cons (cdr e))))
      0))
(pretty-print
  (count-cons
    (read-sexps-from-path "./test/test-reader.scm")))