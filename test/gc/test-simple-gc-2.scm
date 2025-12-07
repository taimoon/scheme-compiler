(let ((LEN 5))
(define (writeln x)
  (write x)
  (newline))

(define (length es)
  (if (pair? es)
      (+ 1 (length (cdr es)))
      0))

(define (f)
  (let loop ((obj '()) (i 0))
    (if (>= i LEN)
        obj
        (loop (cons i obj) (add1 i)))))

(let loop ((i 0) (len (length (f))))
  (if (< i 5000)
      (if (= len LEN)
          (loop (add1 i) (length (f)))
          (writeln len))
      (writeln (f))))
)