#;(
  (expt 2 29)
  (ash 1 29)
)
;;; https://srfi.schemers.org/srfi-125/
(define **default-bound** (+ (ash 1 20) -1))
(define **default-table-size** 16)
(define modulo mod)

(define (vector-index-right pred v)
  (let iter ((i (sub1 (vector-length v))))
    (cond
      ((< i 0) #f)
      ((pred (vector-ref v i)) i)
      (else (iter (sub1 i))))))

(define (vector-fold-left f init v)
  (let iter ((i 0)
             (init init))
    (if (>= i (vector-length v))
        init
        (iter (+ 1 i) (f init (vector-ref v i))))))

(define (%string-hash s bound)
  ;;; Reference: DJB2
  (let loop ((hash 31)
             (index 0))
    (if (>= index (string-length s))
        (modulo hash bound)
        (loop (modulo (+ (* 37 hash)
                         (char->integer (string-ref s index)))
                      **default-bound**)
              (+ index 1)))))

(define (vector-hash v bound)
  (modulo
    (vector-fold-left
      (lambda (h w)
        (modulo (+ (* 257 h) (hash w **default-bound**))
                **default-bound**))
      571
      v)
    bound))

(define (%hash obj bound)
  (cond
    ((integer? obj) (modulo obj bound))
    ((string? obj) (%string-hash obj bound))
    ((symbol? obj) (modulo (symbol-hash obj) bound))
    ((char? obj) (modulo (char->integer obj) bound))
    ((null? obj) 0)
    ((pair? obj) (modulo (+ (hash (car obj) bound) (hash (cdr obj) bound)) bound))
    ((boolean? obj) (if obj 1 0))
    ((vector? obj) (vector-hash obj bound))
    ((procedure? obj)
     (error 'hash "procedure cannot be hashed" obj))
    (else (error 'hash 'unknown-object obj))))

(define hash
  (case-lambda
    ((obj)
     (%hash obj **default-bound**))
    ((obj bound)
     (%hash obj bound))))

;;; hash-table
(define (make-hash-table equiv? hash)
  (vector make-hash-table
          equiv?
          hash
          0
          (make-vector **default-table-size** (%make-rib))))

(define (hash-table? obj)
  (and (vector? obj)
       (eq? (vector-length obj) 5)
       (eq? (vector-ref obj 0) make-hash-table)))

(define (check-if-hash-table! $who hash-table)
  (if (not (hash-table? hash-table))
      (error $who 'expect-hash-table hash-table)))

(define (hash-table-equivalence-function t)
  (check-if-hash-table! 'hash-table-equivalence-function t)
  (vector-ref t 1))

(define (hash-table-hash-function t)
  (check-if-hash-table! 'hash-table-hash-function t)
  (vector-ref t 2))

(define (hash-table-size t)
  (check-if-hash-table! 'hash-table-size t)
  (vector-ref t 3))

(define (%hash-table-inc-size! t)
  (check-if-hash-table! '%hash-table-inc-size! t)
  (vector-set! t 3 (+ 1 (vector-ref t 3))))

(define (%hash-table-dec-size! t)
  (check-if-hash-table! '%hash-table-dec-size! t)
  (vector-set! t 3 (- (vector-ref t 3) 1)))

(define (%hash-table-entries t)
  (check-if-hash-table! '%hash-table-entries t)
  (vector-ref t 4))

(define (%hash-table-set-entries! t v)
  (check-if-hash-table! '%hash-table-set-entries! t)
  (vector-set! t 4 v))

(define (%hash-table-capacity t)
  (check-if-hash-table! '%hash-table-capacity t)
  (vector-length (vector-ref t 4)))

(define %node-key car)
(define %node-cached-hash cadr)
(define %node-val caddr)
(define (%make-node k h v) (list k h v))

(define (%make-rib) '())

(define (%empty-rib? r) (eq? r '()))

(define (%rib-ind equiv? hash-val rib key cont)
  ;;; cont : (lambda (maybe-node cons-rib) (cont maybe-node cons-rib))
  (let recur ((rib rib)
              (cons-rib (lambda (x) x)))
    (cond
      ((null? rib)
       (cont #f
             (case-lambda
              (() (cons-rib '()))
              ((val) (cons-rib (list (%make-node key hash-val val)))))))
      ((not (pair? rib))
       (error '%rib-ind 'ill-form rib))
      ((not (equiv? (%node-key (car rib)) key))
       (recur (cdr rib)
              (lambda (rib*) (cons-rib (cons (car rib) rib*)))))
      ((not (equal? hash-val (%node-cached-hash (car rib))))
       (error '%rib-ind 'hash-equality-violation hash-val (%node-cached-hash (car rib))))
      (else
       (cont (car rib)
             (case-lambda
              (() (cons-rib (cdr rib)))
              ((val) (cons-rib (cons (%make-node key hash-val val) (cdr rib))))))))))

(define (%rib-maybe-assoc equiv? hash-val r k)
  (%rib-ind equiv? hash-val r k
    (lambda (maybe-node cons-rib) maybe-node)))

(define hash-table-update!
  (case-lambda
    ((hash-table key updater)
     (hash-table-update!
        hash-table
        key
        updater
        (lambda () (error 'hash-table-update! 'unbound key))
        (lambda (x) x)))
    ((hash-table key updater failure)
     (hash-table-update! hash-table key updater failure (lambda (x) x)))
    ((hash-table key updater failure success)
     ; Semantically equivalent to, but may be more efficient than, the following code:
     ; (hash-table-set! hash-table key (updater (hash-table-ref hash-table key failure success)))
     (check-if-hash-table! 'hash-table-update! hash-table)
     (let* ((entries (%hash-table-entries hash-table))
            (hash-val ((hash-table-hash-function hash-table) key))
            (index (modulo hash-val (vector-length entries)))
            (rib (vector-ref entries index)))
        (%rib-ind
          (hash-table-equivalence-function hash-table)
          hash-val
          rib
          key
          (lambda (maybe-node cons-rib)
            (let ((val (updater (if maybe-node (success (%node-val maybe-node)) (failure)))))
              (cond
                ((eq? maybe-node #f)
                 (%hash-table-inc-size! hash-table)
                 (vector-set! entries index (cons (%make-node key hash-val val) rib))
                 (%hash-table-resize! hash-table))
                ((or (not (eq? (%node-val maybe-node) val))
                     ;;; current impl doesn't have eqv?
                     #;
                     (not (eqv? (%node-val maybe-node) val)))
                 (vector-set! entries index (cons-rib val))))
              val)))))))

(define (hash-table-intern! hash-table key thunk)
  (check-if-hash-table! 'hash-table-intern! hash-table)
  (hash-table-update! hash-table key (lambda (x) x) thunk (lambda (x) x)))

(define (hash-table-set! hash-table key value)
  (check-if-hash-table! 'hash-table-set! hash-table)
  (hash-table-update! hash-table key (lambda (x) x) (lambda () value) (lambda (_) value)))

(define (hash-table-delete! hash-table key)
  (let* ((hash-val ((hash-table-hash-function hash-table) key))
         (entries (%hash-table-entries hash-table))
         (index (modulo hash-val (vector-length entries))))
  (%rib-ind
    (hash-table-equivalence-function hash-table)
    hash-val 
    (vector-ref entries index)
    key
    (lambda (maybe-node cons-rib)
      (if maybe-node
          (begin
            (vector-set! entries index (cons-rib))
            (%hash-table-dec-size! hash-table)
            (%hash-table-resize! hash-table)
            (list (%node-key maybe-node) (%node-val maybe-node)))
          '())))))

(define (hash-table-pop! hash-table)
  (check-if-hash-table! 'hash-table-pop! hash-table)
  (let* ((entries (%hash-table-entries hash-table))
         (rib-i (vector-index-right pair? entries)))
    (if rib-i
        (let ((v (car (vector-ref entries rib-i))))
          (hash-table-delete! hash-table (%node-key v))
          v)
        (error 'hash-table-pop! 'empty-hash-table hash-table))))

(define (%hash-table-resize! hash-table)
  (define (%hash-table-add! entries equiv? hash-val n)
    (let ((index (modulo hash-val (vector-length entries))))
      (vector-set!
        entries
        index
        (cons n (vector-ref entries index)))))
  (check-if-hash-table! '%hash-table-resize! hash-table)
  (if (>= (* 2 (hash-table-size hash-table))
          (%hash-table-capacity hash-table))
      (let ((new-entries (make-vector (* 2 (hash-table-size hash-table)) (%make-rib)))
            (old-entries (%hash-table-entries hash-table))
            (equiv? (hash-table-equivalence-function hash-table)))
        (%hash-table-set-entries! hash-table new-entries)
        (vector-for-each
          (lambda (rib)
            (for-each
              (lambda (n)
                (%hash-table-add! new-entries equiv? (%node-cached-hash n) n))
              rib))
          old-entries))
      hash-table))

(define hash-table-ref
  (case-lambda
    ((hash-table key)
     (hash-table-ref hash-table key (lambda () (error 'hash-table-ref 'unbound key)) (lambda (x) x)))
    ((hash-table key failure success)
     (check-if-hash-table! 'hash-table-ref hash-table)
     (let* ((entries (%hash-table-entries hash-table))
            (hash ((hash-table-hash-function hash-table) key))
            (equiv? (hash-table-equivalence-function hash-table))
            (rib (vector-ref entries (modulo hash (vector-length entries))))
            (maybe-node (%rib-maybe-assoc equiv? hash rib key)))
        (if maybe-node
            (success (%node-val maybe-node))
            (failure))))))

(define (hash-table-exists? hash-table key)
  (hash-table-ref hash-table key (lambda () #f) (lambda (v) #t)))

(define (hash-table-walk hash-table f)
  (check-if-hash-table! 'hash-table-walk hash-table)
  (hash-table-fold hash-table (lambda (key val acm) (f key val)) 0))

(define (hash-table-fold hash-table f init)
  (check-if-hash-table! 'hash-table-fold hash-table)
  (vector-fold-left
    (lambda (init rib)
      (fold-left
        (lambda (init kv) (f (%node-key kv) (%node-val kv) init))
        init
        rib))
    init
    (%hash-table-entries hash-table)))

(define (hash-table->alist hash-table)
  (check-if-hash-table! 'hash-table->alist hash-table)
  (hash-table-fold hash-table (lambda (key val acm) (cons (list key val) acm)) '()))

(define (alist->hash-table alist)
  (let ((ht (make-hash-table symbol=? symbol-hash)))
    (for-each (lambda (p) (hash-table-set! ht (car p) (cadr p))) alist)
    ht))

(define (hash-table-keys hash-table)
  (hash-table-fold hash-table (lambda (k v init) (cons k init)) '()))
