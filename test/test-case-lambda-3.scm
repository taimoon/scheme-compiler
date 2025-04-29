(let ()
(define (map* f xs k)
  (let recur ((xs xs) (k k))
    (if (not (pair? xs))
        (f k)
        (f (car xs)
           (lambda ys
            (recur (cdr xs)
              (lambda yss
                (apply k (map cons ys yss)))))))))

(define (for-each f xs)
  (if (not (pair? xs))
      '()
      (begin
        (f (car xs))
        (for-each f (cdr xs)))))

(define (writeln e) (write e) (newline))

(for-each writeln
(map*
  (case-lambda
    ((k) (k '() '() '() '()))
    ((x k)
     (k (* x x) (+ x x) `(+ ,x ,x) `(* ,x ,x))))
  (iota 5)
  (lambda xss xss))
)
)