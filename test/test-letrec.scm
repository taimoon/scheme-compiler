(load "compiler.scm")

(pretty-print
  (letrec (
    [odd (lambda (n) (if (eq? n 0) #f (even (sub1 n))))]
    [even (lambda (n) (if (eq? n 0) #t (odd (sub1 n))))]
     )
    (and (even 2) (odd 3) (odd 5)))
)


(pretty-print
  (letrec ([sum (lambda (n) (if (eq? n 0) 0 (+ n (sum (sub1 n)))))])
    (sum 10))
)

(pretty-print
  (let ([sum (lambda (n) (let loop ([i 0] [res 0]) (if (eq? n i) (+ i res) (loop (add1 i) (+ i res)))))])
    (sum 10))
)

(pretty-print
  (letrec (
    [append (lambda (xs ys) (if (null? xs) ys (cons (car xs) (append (cdr xs) ys))))]
  )
  (append (list 1 2 3) (list "list" "ouh"))))

(pretty-print
  (letrec (
    [append (lambda (xs ys) (if (null? xs) ys (cons (car xs) (append (cdr xs) ys))))]
    [reverse
      (lambda (xs)
        (let loop ([xs xs] [res '()])
          (if (null? xs)
              res
              (loop (cdr xs) (append (list (car xs)) res)))))])
  (reverse (append (list 1 2 3) (list "list" "ouh")))))


(pretty-print
  (letrec (
    [append (lambda (xs ys) (if (null? xs) ys (cons (car xs) (append (cdr xs) ys))))]
    [reverse
      (lambda (xs)
        (let loop ([xs xs] [res '()])
          (if (null? xs)
              res
              (loop (cdr xs) (append (list (car xs)) res)))))]
    [iota (lambda (n) (let loop ([i 0] [res '()])
            (if (eq? i n)
                (reverse res) 
                (loop (add1 i) (cons i res)))))]
          
    )
  (iota 10))
)


(pretty-print
  (letrec (
    [append (lambda (xs ys) (if (null? xs) ys (cons (car xs) (append (cdr xs) ys))))]
    [reverse
      (lambda (xs)
        (let loop ([xs xs] [res '()])
          (if (null? xs)
              res
              (loop (cdr xs) (append (list (car xs)) res)))))]
    [iota (lambda (n) (let loop ([i 0] [res '()])
            (if (eq? i n)
                (reverse res) 
                (loop (add1 i) (cons i res)))))]
    [fold-left (lambda (f init xs) (if (null? xs) init (fold-left f (f init (car xs)) (cdr xs))))]
    [add (lambda (a b) (+ a b))]
    [sum (lambda (n) (fold-left add n (iota n)))]
    )
  (sum 10))
)


(pretty-print
  (letrec (
    [append (lambda (xs ys) (if (null? xs) ys (cons (car xs) (append (cdr xs) ys))))]
    [reverse
      (lambda (xs)
        (let loop ([xs xs] [res '()])
          (if (null? xs)
              res
              (loop (cdr xs) (append (list (car xs)) res)))))]
    [iota (lambda (n) (let loop ([i 0] [res '()])
            (if (eq? i n)
                (reverse res) 
                (loop (add1 i) (cons i res)))))]
    [odd (lambda (n) (if (eq? n 0) #f (even (sub1 n))))]
    [even (lambda (n) (if (eq? n 0) #t (odd (sub1 n))))]
    [filter (lambda (f xs) (if (pair? xs) (if (f (car xs)) (cons (car xs) (filter f (cdr xs))) (filter f (cdr xs))) '()))]
    [fold-left (lambda (f init xs) (if (null? xs) init (fold-left f (f init (car xs)) (cdr xs))))]
    [add (lambda (a b) (+ a b))]
    )
  (fold-left add 0 (filter odd (iota 10)))
  )
)


