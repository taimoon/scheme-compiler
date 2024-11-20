(let ()
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