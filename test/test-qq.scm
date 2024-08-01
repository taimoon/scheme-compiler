(pretty-print
  (let ([fn 'cons*] [params 'xs])
    (list (append (list fn) params))))

(pretty-print
  (let ([fn 'cons*] [params 'xs])
    (append
      (list 'define)
      (append (list (append (list fn) params)) '()))))

(pretty-print
  (let ([fn 'cons*]
        [params 'xs])
    `(define (,fn ,params))))

(pretty-print
  (let ([fn 'cons*]
        [params 'xs])
    `(define (,fn ,params))
    `(define (,fn . ,params))))