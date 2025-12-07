(let ()

(define (list . x) x)

(define (list* x . xs)
  (let recur ((x x) (xs xs))
    (if (not (pair? xs))
        x
        (cons x (recur (car xs) (cdr xs))))))

(define (append xs ys)
  (if (pair? xs)
      (cons (car xs) (append (cdr xs) ys))
      ys))

(define (write-line x)
  (write x)
  (newline))

(write-line
  (let ((fn 'cons*) (params 'xs))
    (list (append (list fn) params))))

(write-line
  (let ((fn 'cons*) (params 'xs))
    (append
      (list 'define)
      (append (list (append (list fn) params)) '()))))

(write-line
  (let ((fn 'cons*)
        (params 'xs))
    `(define (,fn ,params))))

(write-line
  (let ((fn 'cons*)
        (params 'xs))
    `(define (,fn ,params))
    `(define (,fn . ,params))))
)