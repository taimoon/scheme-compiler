;;;; number
(define (abs x) (if (< x 0) (- 0 x) x))

;;;; cxr
(define caar (lambda (x) (car (car x))))
(define caaar (lambda (x) (car (car (car x)))))
(define caaaar (lambda (x) (car (car (car (car x))))))
(define cdaaar (lambda (x) (cdr (car (car (car x))))))
(define cdaar (lambda (x) (cdr (car (car x)))))
(define cadaar (lambda (x) (car (cdr (car (car x))))))
(define cddaar (lambda (x) (cdr (cdr (car (car x))))))
(define cdar (lambda (x) (cdr (car x))))
(define cadar (lambda (x) (car (cdr (car x)))))
(define caadar (lambda (x) (car (car (cdr (car x))))))
(define cdadar (lambda (x) (cdr (car (cdr (car x))))))
(define cddar (lambda (x) (cdr (cdr (car x)))))
(define caddar (lambda (x) (car (cdr (cdr (car x))))))
(define cdddar (lambda (x) (cdr (cdr (cdr (car x))))))
(define cadr (lambda (x) (car (cdr x))))
(define caadr (lambda (x) (car (car (cdr x)))))
(define caaadr (lambda (x) (car (car (car (cdr x))))))
(define cdaadr (lambda (x) (cdr (car (car (cdr x))))))
(define cdadr (lambda (x) (cdr (car (cdr x)))))
(define cadadr (lambda (x) (car (cdr (car (cdr x))))))
(define cddadr (lambda (x) (cdr (cdr (car (cdr x))))))
(define cddr (lambda (x) (cdr (cdr x))))
(define caddr (lambda (x) (car (cdr (cdr x)))))
(define caaddr (lambda (x) (car (car (cdr (cdr x))))))
(define cdaddr (lambda (x) (cdr (car (cdr (cdr x))))))
(define cdddr (lambda (x) (cdr (cdr (cdr x)))))
(define cadddr (lambda (x) (car (cdr (cdr (cdr x))))))
(define cddddr (lambda (x) (cdr (cdr (cdr (cdr x))))))

;;;; list
(define (list? xs)
  (if (pair? xs)
      (list? (cdr xs))
      (null? xs)))

(define (fold-left proc init xs)
  (if (pair? xs)
      (fold-left proc (proc init (car xs)) (cdr xs))
      init))

(define (fold-right proc init xs)
  (if (pair? xs)
      (proc (car xs) (fold-right proc init (cdr xs)))
      init))

(define (append xs . xss)
  (if (null? xss)
      xs
      (let recur ((xs xs) (xss xss))
        (if (null? xss)
            xs
            (fold-right cons (recur (car xss) (cdr xss)) xs)))))

(define (reverse xs)
  (fold-left (lambda (acm x) (cons x acm)) '() xs))

(define (length xs)
  (fold-left (lambda (acm _) (add1 acm)) 0 xs))

(define (andmap p xs)
  (let loop ((xs xs))
    (cond
      ((not (pair? xs)) #t)
      ((not (p (car xs))) #f)
      (else (loop (cdr xs))))))

(define (ormap p xs)
  (let loop ((xs xs))
    (cond
      ((not (pair? xs)) #f)
      ((p (car xs)) #t)
      (else (loop (cdr xs))))))

(define iota
  (case-lambda
    ((n) (iota 0 n 1))
    ((start end)
     (iota start end 1))
    ((start end step)
     (let recur ((start start))
        (if (>= start end)
            '()
            (cons start (recur (+ start step))))))))

(define (assp p xs)
  (cond
    ((null? xs)
     #f)
    ((not (and (pair? xs) (pair? (car xs))))
     (error "assp" "improperly formed alist" xs))
    ((p (caar xs))
     (car xs))
    (else (assp p (cdr xs)))))

(define (assq x xs)
  (assp (lambda (y) (eq? x y)) xs))

(define (assoc x xs)
  (assp (lambda (y) (equal? x y)) xs))

(define (memp p xs)
  (cond
    ((null? xs)
     #f)
    ((not (pair? xs))
     (error "memp" "improperly formed list" xs))
    ((p (car xs))
     xs)
    (else (memp p (cdr xs)))))

(define (memq x xs)
  (memp (lambda (y) (eq? x y)) xs))

(define (member x xs)
  (memp (lambda (y) (equal? x y)) xs))

(define (map f ls . more)
  (if (null? more)
      (let map1 ((ls ls))
        (if (null? ls)
            '()
            (cons (f (car ls))
                  (map1 (cdr ls)))))
      (let map-more ((ls ls) (more more))
        (if (null? ls)
            '()
            (cons
              (apply f (cons (car ls) (map car more)))
              (map-more (cdr ls) (map cdr more)))))))

(define (filter pred xs)
  (cond
    ((not (pair? xs)) '())
    ((pred (car xs))
     (cons (car xs) (filter pred (cdr xs))))
    (else
     (filter pred (cdr xs)))))

(define (for-each f xs . xss)
  (if (null? xss)
      (let loop ((xs xs))
        (if (pair? xs)
            (begin  (f (car xs))
                    (loop (cdr xs)))
            '()))
      (let loop ((xs xs) (xss xss))
        (if (null? xs)
            '()
            (begin
              (apply f (cons (car xs) (map car xss)))
              (loop (cdr xs) (map cdr xss)))))))

(define (for-all pred xs)
  (if (not (pair? xs))
      #t
      (if (pred (car xs))
          (for-all pred (cdr xs))
          #f)))

(define (exists pred xs) (not (for-all pred xs)))

;;; vector
(define (vector-equal? v w)
  (and (eq? v w)
       (eq? (vector-length v)
            (vector-length w))
    (let loop ((i 0))
      (if (>= i (vector-length v))
          #t
          (and
            (equal? (vector-ref v i) (vector-ref w i))
            (loop (add1 i)))))))

(define (vector->list v)
  (let recur ((i 0))
    (if (= i (vector-length v))
        '()
        (cons (vector-ref v i) (recur (add1 i))))))

(define (list->vector xs)
  (let recur ((xs xs) (sz 0))
    (if (pair? xs)
        (let ((v (recur (cdr xs) (add1 sz))))
          (vector-set! v sz (car xs))
          v)
        (make-vector sz))))

;;;; ASCII character libraries
(define (char=? c1 c2)
  (and (char? c1) (char? c2)
       (= (char->integer c1) (char->integer c2))))
(define (char>? c1 c2)
  (and (char? c1) (char? c2)
       (> (char->integer c1) (char->integer c2))))
(define (char<? c1 c2)
  (and (char? c1) (char? c2)
       (< (char->integer c1) (char->integer c2))))
(define (char<=? c1 c2)
  (and (char? c1) (char? c2)
       (<= (char->integer c1) (char->integer c2))))
(define (char>=? c1 c2)
  (and (char? c1) (char? c2)
       (>= (char->integer c1) (char->integer c2))))
(define (char- c1 c2)
  (integer->char (- (char->integer c1) (char->integer c2))))
(define (char+ c1 c2)
  (integer->char (+ (char->integer c1) (char->integer c2))))
(define (char-downcase c)
  (if (char-upper-case? c)
      (char+ (char- c #\A) #\a)
      c))
(define (char-upcase c)
  (if (char-lower-case? c)
      (char+ (char- c #\a) #\A)
      c))
(define (char-lower-case? c)
  (and (char<=? #\a c)
       (char<=? c #\z)))
(define (char-upper-case? c)
  (and (char<=? #\A c)
       (char<=? c #\Z)))
(define (char-alphabetic? c)
  (or (char-lower-case? c) (char-upper-case? c)))
(define (char-alphanumeric? c)
  (or (char-alphabetic? c) (char-numeric? c)))
(define (char-numeric? c)
  (and (char<=? #\0 c)
       (char<=? c #\9)))
(define char-whitespace?
  ; (#\space #\newline #\return #\linefeed #\tab)
  (let ((whitespaces (map integer->char '(32 10 13 10 9))))
    (lambda (c) (if (memq c whitespaces) #t #f))))

;;; string
(define (string . xs)
  (list->string xs))

(define (substring s i j)
  (let* ((len (- j i))
         (t (make-string len #\nul)))
    (let loop ((i i) (k 0))
      (if (>= i j)
          t
          (begin
            (string-set! t k (string-ref s i))
            (loop (add1 i) (add1 k)))))))

(define (string=? s1 s2)
  (or (eq? s1 s2)
      (and (string? s1) (string? s2)
           (= (string-length s1) (string-length s2))
           (let loop ((i 0))
             (cond
               ((eq? i (string-length s1))
                 #t)
               ((eq? (string-ref s1 i) (string-ref s2 i))
                 (loop (add1 i)))
               (else #f))))))

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

(define (list->string xs)
  (let recur ((xs xs) (i 0))
    (cond 
      ((not (pair? xs))
       (make-string i #\nul))
      ((not (char? (car xs)))
       (error "list->string" "expect char" (car xs)))
      (else
        (let ((s (recur (cdr xs) (add1 i))))
          (string-set! s i (car xs))
          s)))))

(define (string->list s)
  (let recur ((i 0))
    (if (= i (string-length s))
        '()
        (cons (string-ref s i)
              (recur (add1 i))))))

(define (number->string i)
  (if (integer? i)
      (integer->string i)
      (error "number->string" "implement only integer")))

(define (string->number s radix)
  (define (char->int x)
    (set! x (char-downcase x))
    (if (and (char<=? #\a x) (char<=? x #\f))
        (+ 10 (- (char->integer x) (char->integer #\a)))
        (- (char->integer x) (char->integer #\0))))
  (define (loop i res)
    (if (>= i (string-length s))
        res
        (loop (add1 i)
          (+ (* res radix) (char->int (string-ref s i))))))
  (if (eq? (string-ref s 0) #\-)
      (- 0 (loop 1 0))
      (loop 0 0)))

(define (integer->string i)
  (define (int->char x)
    (integer->char (+ x (char->integer #\0))))
  (cond
    ((= i 0) "0")
    ((< i 0)
     (string-append "-" (integer->string (- i))))
    (else
      (let loop ((i i) (acm '()))
        (if (= i 0)
            (list->string acm)
            (loop (div i 10) (cons (int->char (mod i 10)) acm)))))))

;;;; formatter
(define (obj->repr obj w?)
  (let obj->repr ([obj obj])
   (cond
     [(null? obj) "()"]
     [(integer? obj)
      (if (< obj 0)
          (string-append "-" (number->string (- obj)))
          (number->string obj))]
     [(char? obj) (string-append "#\\" (string obj))]
     [(boolean? obj) (if obj "#t" "f")]
     [(and (string? obj) w?) (string-append "\"" obj "\"")]
     [(string? obj) obj]
     [(symbol? obj) (symbol->string obj)]
     [(pair? obj)
      (string-append "("
       (obj->repr (car obj))
       (let loop ([obj (cdr obj)])
         (cond [(pair? obj)
                (string-append
                 " "
                 (obj->repr (car obj))
                 (loop (cdr obj)))]
               [(null? obj) ")"]
               [else (string-append " . " (obj->repr obj) ")")])))]
     [else (error "obj->repr" "unknown-obj" obj)])))

(define (format f . args)
  (define (maybe-ref s i)
    (if (< i (string-length s))
        (string-ref s i)
        #f))
  (define (loop s i j args acm)
    (cond
      [(>= i (string-length s))
       (if (pair? args)
           (error "format" "too few control string"))
       (apply string-append (reverse (cons (substring s j (string-length s)) acm)))]
      [(and (eq? (maybe-ref s i) #\~)
            (eq? (maybe-ref s (+ i 1)) #\~))
        (loop s (+ i 2) (+ i 2) args (cons "~" (cons (substring s j i) acm)))]
      [(and (eq? (maybe-ref s i) #\~)
            (eq? (maybe-ref s (+ i 1)) #\a))
        (if (not (pair? args))
            (error "format" "too many control string"))
        (loop s (+ i 2) (+ i 2) (cdr args) (cons (obj->repr (car args) #f) (cons (substring s j i) acm)))]
      [(and (eq? (maybe-ref s i) #\~)
            (eq? (maybe-ref s (+ i 1)) #\s))
        (if (not (pair? args))
            (error "format" "too many control string"))
        (loop s (+ i 2) (+ i 2) (cdr args) (cons (obj->repr (car args) #t) (cons (substring s j i) acm)))]
      [else (loop s (add1 i) j args acm)]))
  (cond [(string? f) (loop f 0 0 args '())]
        [(and (port? f) (pair? args) (string? (car args)))
         (display (loop (car args) 0 0 (cdr args) '()) f)]
        [(and (boolean? f) f (pair? args) (string? (car args)))
         (display (loop (car args) 0 0 (cdr args) '()) (current-output-port))]
        [(and (boolean? f) (not f) (pair? args) (string? (car args)))
         (loop (car args) 0 0 (cdr args) '())]
        [else (error "format" "unknown args" f args)]))


;;;; misc
(define (equal? x y)
  (cond ((eq? x y) #t)
        ((and (pair? x) (pair? y))
         (and (equal? (car x) (car y))
              (equal? (cdr x) (cdr y))))
        ((and (vector? x) (vector? y))
         (vector-equal? x y))
        ((and (string? x) (string? y))
         (string=? x y))
        (else #f)))

(define gensym
  (let ((x 0))
    (define (make-lcg multiplier increment modulus x)
      (lambda ()
        (set! x (mod (+ increment (* multiplier x)) modulus))
        x))

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

    (define rand
      (make-lcg 75 74 (+ (ash 2 16) 1) (get-process-id)))
    
    (lambda ()
      (set! x (add1 x))
      (string->symbol (string-append "g" (number->string x) "-" (random-string 16))))))