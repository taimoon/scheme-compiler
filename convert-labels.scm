(define-syntax code
  (syntax-rules ()
    [(code params . body)
     (lambda params . body)]))

(define-syntax labels
  (syntax-rules ()
    [(labels . bindings)
     (letrec . bindings)]))

(define-syntax labelcall
  (syntax-rules ()
    [(labelcall f . es)
     (f . es)]))

(define-syntax gc-flip
  (syntax-rules ()
    [(gc-flip . _) 0]))

(define (write-string s p)
  (put-string p s))

(define (vector-map! f vs)
  (let loop ([i 0])
    (if (eq? i (vector-length vs))
        0
        (begin
          (vector-set! vs i (f (vector-ref vs i)))
          (loop (add1 i))))))

(define (integer->string x)
  (number->string x))