(let ()
(define (ash n m)
    (if (< m 0)
        (ashr n (- m))
        (ashl n m)))

(define (abs n) (if (< n 0) (- 0 n) n))

(define (vector-for-each f v)
  (let iter ((i 0))
    (if (>= i (vector-length v))
        v
        (begin
          (f (vector-ref v i))
          (iter (+ i 1))))))

(define (fold-left f init xs)
  (if (pair? xs)
      (fold-left f (f init (car xs)) (cdr xs))
      init))

(define (map f xs)
  (if (pair? xs) (cons (f (car xs)) (map f (cdr xs))) '()))

(define (for-each f xs)
  (if (pair? xs) (begin (f (car xs)) (for-each f (cdr xs))) '()))

(define (string-copy s)
  (let ((s* (make-string (string-length s) #\nul)))
    (string-copy! s 0 s* 0 (string-length s))
    s*))

(define (string-copy! src src-start dst dst-start n)
  (if (= n 0)
      dst
      (begin
        (string-set! dst dst-start (string-ref src src-start))
        (string-copy! src (add1 src-start) dst (add1 dst-start) (sub1 n)))))

(define (string-append . ss)
  (let ((res (make-string (fold-left + 0 (map string-length ss)) #\nul)))
    (let loop ((ss ss) (split 0))
      (if (pair? ss)
          (begin
            (string-copy! (car ss) 0 res split (string-length (car ss)))
            (loop (cdr ss) (+ (string-length (car ss)) split)))
          res))))

(define (string=? s1 s2)
  (and
    (string? s1)
    (string? s2)
    (or
      (eq? s1 s2)
      (and
        (= (string-length s1) (string-length s2))
        (let loop ((i 0))
            (cond
              ((eq? i (string-length s1))
                #t)
              ((eq? (string-ref s1 i) (string-ref s2 i))
                (loop (add1 i)))
              (else #f)))))))

(define (assp p xs)
  (cond
    ((not (pair? xs)) #f)
    ((p (car xs)) (car xs))
    (else (assp p (cdr xs)))))

(define SYMBOL-TBL-SIZE 0)

(define SYMBOL-STR-TABLE (make-vector 8 '()))

(define **default-bound** (+ (ash 1 29) -1))

(define (%string-hash s)
  ;;; Reference: DJB2
  (let loop ((hash 31)
             (index 0))
    (if (>= index (string-length s))
        (abs hash)
        (loop (mod (+ (* 37 hash)
                      (char->integer (string-ref s index)))
                      **default-bound**)
              (+ index 1)))))

(define %gensym
  (let ()
    (define (get-process-id)
      (foreign-call s_getpid))

    (define (make-lcg multiplier increment modulus x)
      (lambda ()
        (set! x (mod (+ increment (* multiplier x)) modulus))
        x))

    (define rand
      (make-lcg 75 74 (+ (ash 2 16) 1) (get-process-id)))

    (define (random-bool)
      (= (mod (rand) 2) (mod (rand) 2)))

    (define (random-string len)
      (let loop ((s (make-string len #\nul))
                (i (- len 1)))
        (if (< i 0)
            s
            (begin
              (string-set! s i (integer->char
                                (+ (if (random-bool) 97 65)
                                   (mod (rand) 26))))
              (loop s (- i 1))))))

    (let ((x 0)
          (rdm-str (random-string 4)))
      (case-lambda
        (()
         (%gensym "g"))
        ((prefix)
         (let ((str (string-append prefix rdm-str (number->string x))))
            (set! x (add1 x))
            (string->symbol str)))))))

(define (%make-symbol str hash)
  (vector str hash))

(define (%symbol-string n)
  (vector-ref n 0))

(define (%symbol-hash n)
  (vector-ref n 1))

(define (assoc-string k ss)
  (assp (lambda (n) (string=? (%symbol-string n) k)) ss))

(define (%SYMBOL-STR-TABLE-resize!)
  (define (%hash-table-add! entries hash-val n)
    (let ((index (mod hash-val (vector-length entries))))
      (vector-set!
        entries
        index
        (cons n (vector-ref entries index)))))
  (if (>= SYMBOL-TBL-SIZE (* 2 (vector-length SYMBOL-STR-TABLE)))
      (let ((old SYMBOL-STR-TABLE)
            (new (make-vector (* 2 (vector-length SYMBOL-STR-TABLE)) '())))
          (vector-for-each
            (lambda (rib)
              (for-each (lambda (n) (%hash-table-add! new (%symbol-hash n) n)) rib))
            old)
          (set! SYMBOL-STR-TABLE new))
      #f))

(define (%%string->symbol str)
  (let* ((hash-val (%string-hash str))
         (idx (mod hash-val (vector-length SYMBOL-STR-TABLE)))
         (rib (vector-ref SYMBOL-STR-TABLE idx))
         (maybe (assoc-string str rib)))
    (if maybe
        (%string->symbol (%symbol-string maybe))
        (let ((n (%make-symbol str hash-val)))
          (vector-set! SYMBOL-STR-TABLE idx (cons n rib))
          (set! SYMBOL-TBL-SIZE (+ SYMBOL-TBL-SIZE 1))
          (%SYMBOL-STR-TABLE-resize!)
          (%string->symbol (%symbol-string n))))))

(define (%%symbol->string sym)
  (string-copy (%symbol->string sym)))

(set! symbol->string %%symbol->string)
(set! string->symbol %%string->symbol)
(set! gensym %gensym)
)