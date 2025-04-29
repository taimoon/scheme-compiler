; https://srfi.schemers.org/srfi-158/
; APL
(define (generator-next g) (g))

#;(define (make-coroutine-generator proc)
  ; proc is a procedure that takes one argument, yield.
  (define resume #f)
  (define return #f)
  (define reply (if #f #f))
  (define yield
    (lambda (v)
      (call/cc (lambda (r) (set! resume r) (return v)))))
  (define (coro)
    (call/cc (lambda (cc)
      (set! return cc)
      (if (not resume)
          (begin
            (proc yield)
            (set! coro eof-object)
            (return (eof-object)))
          (resume reply)))))
  (case-lambda
    (() (coro))
    ((r) (set! reply r) (coro))))

;;; constructor
(define (make-unfold-generator pred mapper next seed tail-gen)
  (lambda ()
    (if (pred seed)
        (tail-gen seed)
        (let ((res (mapper seed)))
          (set! seed (next seed))
          res))))

(define make-range-generator
  (case-lambda
    ((end)
     (make-range-generator 0 end 1))
    ((start end)
     (make-range-generator start end 1))
    ((start end step)
     (make-unfold-generator
        (lambda (x) (>= x end))
        (lambda (x) x)
        (lambda (x) (+ x step))
        start
        (lambda _ (eof-object))))))

(define make-counter
  (case-lambda
    (() (make-counter 0 1))
    ((start)
     (make-counter start 1))
    ((start step)
     (make-unfold-generator
      (lambda _ #f)
      (lambda (x) x)
      (lambda (x) (+ x step))
      start
      (lambda _ (eof-object))))))

(define (make-iota n) (make-range-generator 0 n 1))

(define (list->generator xs)
  (lambda ()
    (if (not (pair? xs))
        (eof-object)
        (let ((v (car xs)))
          (set! xs (cdr xs))
          v))))

(define repeat
  (case-lambda
    ((v)
     (lambda () v))
    ((v n)
     (lambda ()
      (if (<= n 0)
          (eof-object)
          (begin
            (set! n (sub1 n))
            v))))))

;;; Transfomer
(define (gzip . gs)
  (define (exists-generators pred gs fail-cont cont)
    (let recur ((gs gs) (k cont))
        (if (not (pair? gs))
            (k '())
            (let ((v ((car gs))))
              (if (pred v)
                  (fail-cont v)
                  (recur (cdr gs) (lambda (acm) (k (cons v acm)))))))))
  (lambda ()
    (exists-generators
      eof-object?
      gs
      (lambda _ (eof-object))
      (lambda (x) x))))

(define (unfold-generator pred mapper tail-gen . gs)
  (define gen (apply gzip gs))
  (define seeds (gen))
  (lambda ()
    (if (eof-object? seeds)
        seeds
        (if (apply pred seeds)
            (let ((v (apply tail-gen seeds)))
              (set! seeds (list (eof-object)))
              v)
            (let ((v (apply mapper seeds)))
              (set! seeds (map generator-next gs))
              v)))))

(define (gmap proc . gs)
  (let ((gen (apply gzip gs)))
    (lambda ()
      (let ((vs (gen)))
        (if (eof-object? vs)
            vs
            (apply proc vs))))))

(define (gchain gen-gen)
  (define (f gen)
    (lambda ()
      (if (eof-object? gen)
          gen
          (let ((v (gen)))
            (if (eof-object? v)
                (begin
                  (set! gen (gen-gen))
                  ((f gen)))
                v)))))
  (define mut-f
    (vector
      (lambda ()
        (vector-set! mut-f 0 (f (gen-gen)))
        (call-f))))
  (define (call-f) ((vector-ref mut-f 0)))
  call-f)

(define (greplicate value-gen mask-gen)
  (gchain (gmap repeat value-gen mask-gen)))

(define (gselect value-gen truth-gen)
  (greplicate value-gen (gmap (lambda (t) (if (eq? t #f) 0 1)) truth-gen)))

(define (gappend . gs)
  (gchain (list->generator gs)))

(define genum
  (case-lambda
    ((gen)
     (genum gen 0))
    ((gen start)
     (gzip (make-counter start) gen))))

(define gtake
  (case-lambda
    ((gen n) (gtake gen n (eof-object)))
    ((gen n padding)
     (lambda ()
      (if (<= n 0)
          (eof-object)
          (begin
            (set! n (sub1 n))
            (let ((v (gen)))
              (if (eof-object? v)
                  padding
                  v))))))))

(define (gdrop gen k)
  (lambda ()
    (let loop ()
      (if (not (<= k 0))
          (begin
            (set! k (- k 1))
            (gen)
            (loop))))
    (gen)))

(define (gtake-while pred gen)
  (unfold-generator
      (lambda (x) (not (pred x)))
      (lambda (x) x)
      (lambda (_) (eof-object))
      gen))

(define (gdrop-while pred gen)
  (define g
    (lambda ()
      (let loop ((v (gen)))
        (if (pred v)
            (loop (gen))
            (begin
              (set! g gen)
              v)))))
  (lambda () (g)))

(define (gfilter pred gen)
  (lambda ()
    (let ((v (gen)))
      (if (eof-object? v)
          v
          (if (pred v)
              v
              ((gfilter pred gen)))))))

(define (gcombine proc init . gs)
  ; proc is (lambda (acm v) ...)
  (define gen (apply gzip gs))
  (lambda ()
    (let ((vs (gen)))
      (if (eof-object? vs)
          (let ((v init))
            (set! init (eof-object))
            v)
          (begin
            (set! init (apply proc (cons init vs)))
            init)))))

(define (gscan proc init gen)
  ;;; proc is associative
  (gcombine proc init gen))

;;; Consumer
(define (generator-last gen)
  (let loop ((prev (eof-object))
             (v (gen)))
    (if (eof-object? v)
        prev
        (loop v (gen)))))

(define (generator-fold-left proc init . gs)
  (generator-last (apply gcombine (list* proc init gs))))

(define (generator-for-each proc . gs)
  (define gen (apply gzip gs))
  (let loop ()
    (let ((vs (gen)))
      (if (eof-object? vs)
          (eof-object)
          (begin
            (apply proc vs)
            (loop))))))

(define generator->list
  (case-lambda
    ((gen)
     (let recur ()
      (let ((v (gen)))
        (if (eof-object? v)
            '()
            (cons v (recur))))))
    ((gen k)
     (generator->list (gtake gen k)))))
