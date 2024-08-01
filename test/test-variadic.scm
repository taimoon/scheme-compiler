(pretty-print
  (let ([lst (lambda x x)])
    (lst (lst 1 2 3) (lst #\t #t) "something"))
)

(pretty-print
  (let ([f (lambda (f . xs) xs)])
    (f f 3 5 7))
)

(pretty-print
  (let ([f (lambda (f . xs) xs)])
    (f f))
)

(pretty-print
  (let ([sum (lambda x
    (let loop ([x x] [acm 0])
      (if (pair? x)
          (loop (cdr x) (+ (car x) acm))
          acm)))])
  (sum 1 2 3 4 5 6 7 8 9)
  ))

(let ()
  (define (member x xs)
    (if (pair? xs)
        (if (eq? x (car xs))
            xs
            (member x (cdr xs)))
        #f))

  (define (set-add-elem x xs)
    (if (member x xs)
        xs
        (cons x xs)))
    
  (define (list->set xs)
    (if (null? xs)
        '()
        (set-add-elem (car xs) (list->set (cdr xs)))))

  (define (make-set . xs)
    (list->set xs))

  (pretty-print (make-set 3 1 4 1 5 9 2 6 5 3 5 9))
)

(let ()
  (define (append xs ys)
    (if (pair? xs)
        (cons (car xs) (append (cdr xs) ys))
        ys))
  (define (string->list s)
    (let recur ([i 0])
      (if (eq? i (string-length s))
          '()
          (cons (string-ref s i) (recur (add1 i))))))
  (pretty-print
    (append (string->list "abcdefg") (list #t #f))))