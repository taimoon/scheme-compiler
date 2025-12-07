(let ()
(define (abort)
  (foreign-call abort))

(define (error . args)
  (foreign-call s_writeln args (label stderr))
  (abort))

(define (string-copy s)
  (let ((s* (make-string (string-length s))))
    (string-copy! s 0 s* 0 (string-length s))
    s*))

(define (string-copy! src src-start dst dst-start n)
  (if (= n 0)
      dst
      (begin
        (string-set! dst dst-start (string-ref src src-start))
        (string-copy! src (+ 1 src-start) dst (+ 1 dst-start) (sub1 n)))))

(define (string-append s1 s2)
  (let ((s (make-string (+ (string-length s1) (string-length s2)))))
    (string-copy! s1 0 s 0 (string-length s1))
    (string-copy! s2 0 s (string-length s1) (string-length s2))
    s))

(define (string=? s1 s2)
  (and
    (string? s1)
    (string? s2)
    (= (string-length s1) (string-length s2))
    (or
      (eq? s1 s2)
      (let loop ((i 0))
        (cond
          ((eq? i (string-length s1))
           #t)
          ((eq? (string-ref s1 i) (string-ref s2 i))
           (loop (+ 1 i)))
          (else #f))))))

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

    (define (int->char x)
      (integer->char (+ x (char->integer #\0))))

    (define (number->string x)
      (let recur ((x (abs x))
                  (i 0))
          (cond
            ((> x 0)
             (let ((s (recur (div x 10) (+ 1 i))))
               (string-set! s (- (- (string-length s) 1) i) (int->char (mod x 10)))
               s))
            ((= i 0) "0")
            (else (make-string i #\0)))))
    
    (define (*->string s)
      (cond
        ((symbol? s) (%symbol->string s))
        ((string? s) s)
        (else (error "*->string" "bad input" s))))

    (let ((x 0)
          (rdm-str (random-string 4)))
      (case-lambda
        (()
         (%gensym "g"))
        ((prefix)
         (let ((str (string-append (string-append (*->string prefix) rdm-str) (number->string x))))
            (set! x (+ 1 x))
            (%string->symbol str)))))))

(define (assoc-string k ss)
  (cond
    ((not (pair? ss)) #f)
    ((string=? k (%symbol-name (car ss)))
     (car ss))
    (else (assoc-string k (cdr ss)))))

(define %SYMBOL-STR-TABLE-resize! (let ()
  (define (vector-for-each f v)
    (let iter ((i 0))
      (if (>= i (vector-length v))
          v
          (begin
            (f (vector-ref v i))
            (iter (+ i 1))))))
  (define (for-each f xs)
    (if (pair? xs) (begin (f (car xs)) (for-each f (cdr xs))) '()))
  (define (%hash-table-add! entries hash-val n)
    (let ((index (mod hash-val (vector-length entries))))
      (vector-set!
        entries
        index
        (cons n (vector-ref entries index)))))
  (lambda ()
    (if (>= SYMBOL-TBL-SIZE (* 2 (vector-length SYMBOL-STR-TABLE)))
      (let ((old SYMBOL-STR-TABLE))
          (set! SYMBOL-STR-TABLE (make-vector (* 2 (vector-length SYMBOL-STR-TABLE)) '()))
          (vector-for-each
            (lambda (rib)
              (for-each (lambda (n) (%hash-table-add! SYMBOL-STR-TABLE (%symbol-hash n) n)) rib))
            old))))))

(define (%string->symbol str)
  (let* ((hash-val (%string-hash str))
         (idx (mod hash-val (vector-length SYMBOL-STR-TABLE)))
         (rib (vector-ref SYMBOL-STR-TABLE idx))
         (maybe (assoc-string str rib)))
    (if maybe
        maybe
        (let ((n (%make-symbol hash-val str)))
          (vector-set! SYMBOL-STR-TABLE idx (cons n rib))
          (set! SYMBOL-TBL-SIZE (+ SYMBOL-TBL-SIZE 1))
          (%SYMBOL-STR-TABLE-resize!)
          n))))

(define (%symbol->string sym) (string-copy (%symbol-name sym)))

(foreign-call set_s_str2sym %string->symbol)

(%symbol-value-set! (%string->symbol "string->symbol") %string->symbol)
(%symbol-value-set! (%string->symbol "symbol->string") %symbol->string)
(%symbol-value-set! (%string->symbol "gensym") %gensym)
)
