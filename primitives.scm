(define (add1 n) (prim-call add1 n))
(define (sub1 n) (prim-call sub1 n))
(define (+ n m) (prim-call + n m))
(define (- n m) (prim-call - n m))
(define (* n m) (prim-call * n m))
(define (div n m) (prim-call div n m))
(define (mod n m) (prim-call mod n m))
(define (< n m) (prim-call < n m))
(define (<= n m) (prim-call <= n m))
(define (= n m) (prim-call = n m))
(define (>= n m) (prim-call >= n m))
(define (> n m) (prim-call > n m))
(define (ashl n m) (prim-call ashl n m))
(define (ashr n m) (prim-call ashr n m))
(define (bitwise-and n m) (prim-call bitwise-and n m))
(define (bitwise-ior n m) (prim-call bitwise-ior n m))
(define (eq? x y) (prim-call eq? x y))
(define (integer? x) (prim-call integer? x))
(define (char? x) (prim-call char? x))
(define (char->integer c) (prim-call char->integer c))
(define (integer->char i) (prim-call integer->char i))
(define (procedure? x) (prim-call procedure? x))
(define (boolean? x) (prim-call boolean? x))
(define (null? x) (prim-call null? x))
(define (pair? x) (prim-call pair? x))
(define (cons x y) (prim-call cons x y))
(define (car p) (prim-call car p))
(define (cdr p) (prim-call cdr p))
(define (string? x) (prim-call string? x))
(define (make-string n) (prim-call make-string n))
(define (string-length s) (prim-call string-length s))
(define (string-ref s i) (prim-call string-ref s i))
(define (string-set! s i c) (prim-call string-set! s i c))
(define (vector? x) (prim-call vector? x))
(define (make-vector v) (prim-call make-vector v))
(define (vector-length v) (prim-call vector-length v))
(define (vector-ref v i) (prim-call vector-ref v i))
(define (vector-set! v i x) (prim-call vector-set! v i x))
(define (symbol? x) (prim-call symbol? x))
