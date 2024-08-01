;;; misc
(define gensym
  (let ([x 0])
    (lambda args
      (set! x (add1 x))
      (cond [(pair? args)
             (if (string? (car args))
                 (string->symbol
                  (string-append (string-append (car args) "_")
                                 (string-append (integer->string (get-process-id)) (integer->string x))))
                 (error "gensym" "gensym expect string"))]
            [else (string->symbol
                    (string-append "g"
                                   (string-append (integer->string (get-process-id))
                                                  (integer->string x))))]))))

(define (gensym->unique-string s)
  (symbol->string s))

(define (equal? x y)
  (cond [(or (and (integer? x) (integer? y))
             (and (boolean? x) (boolean? y))
             (and (char? x) (char? y)))
         (eq? x y)]
        [(and (null? x) (null? y)) #t]
        [(and (pair? x) (pair? y))
         (and (equal? (car x) (car y))
              (equal? (cdr x) (cdr y)))]
        [(and (vector? x) (vector? y))
         (vector-equal? x y)]
        [(and (string? x) (string? y))
         (string-equal? x y)]
        [else (eq? x y)]))

;;; number
(define (abs x) (if (< x 0) (- 0 x) x))

;;; cons
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

;;; list
(define (list? xs)
  (if (pair? xs)
      (list? (cdr xs))
      (null? xs)))

(define (cons* . xs)
  (cond [(null? xs) xs]
        [(and (pair? xs) (null? (cdr xs)))
         (car xs)]
        [else
          (let recur ([xs xs])
            (if (and (pair? xs) (null? (cdr xs)))
                (car xs)
                (cons (car xs) (recur (cdr xs))))
              )]))
(define list* cons*)

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
      (let recur ([xs xs] [xss xss])
        (if (null? xss)
            xs
            (fold-right cons (recur (car xss) (cdr xss)) xs))
        )))

(define (reverse xs)
  (fold-left (lambda (acm x) (cons x acm)) '() xs))

(define (length xs)
  (fold-left (lambda (acm _) (add1 acm)) 0 xs))

(define (iota n)
  (let loop ([i 0] [res '()])
    (if (eq? i n)
        (reverse res) 
        (loop (add1 i) (cons i res)))))

(define (assoc x xs)
  (if (pair? xs)
      (if (equal? x (caar xs))
          (car xs)
          (assoc x (cdr xs)))
      #f))

(define (member x xs)
  (if (pair? xs)
      (if (equal? x (car xs))
          xs
          (member x (cdr xs)))
      #f))

(define (map f ls . more)
  (if (null? more)
      (let map1 ([ls ls])
        (if (null? ls)
            '()
            (cons (f (car ls))
                  (map1 (cdr ls)))))
      (let map-more ([ls ls] [more more])
        (if (null? ls)
            '()
            (cons
              (apply f (cons (car ls) (map car more)))
              (map-more (cdr ls) (map cdr more)))))))

(define (filter pred xs)
  (if (pair? xs)
      (if (pred (car xs))
          (cons (car xs) (filter pred (cdr xs)))
          (filter pred (cdr xs)))
      xs))

(define (for-each f xs . xss)
  (if (null? xss)
      (let loop ([xs xs])
        (if (pair? xs)
            (begin  (f (car xs))
                    (loop (cdr xs)))
            '()))
      (let loop ([xs xs] [xss xss])
        (if (null? xs)
            '()
            (begin
              (apply f (cons (car xs) (map car xss)))
              (loop (cdr xs) (map cdr xss)))))))

;;; vector
(define (vector-equal? x y)
  (and (vector? x) (vector? y)
    (let ([s (vector-length x)]
          [t (vector-length y)])
      (and (eq? s t)
        (let loop ([i 0])
          (if (eq? i s)
              #t
              (and (equal? (vector-ref x i) (vector-ref y i))
                   (loop (add1 i)))))))))

(define (vector-map f vs)
  (let* ([end (vector-length vs)]
         [ws (make-vector end)])
    (let loop ([i 0])
      (if (eq? i end)
          ws
          (begin
            (vector-set! ws i (f (vector-ref vs i)))
            (loop (add1 i)))))))

(define (vector-map! f vs)
  (let loop ([i 0])
    (if (eq? i (vector-length vs))
        0
        (begin
          (vector-set! vs i (f (vector-ref vs i)))
          (loop (add1 i))))))

(define (vector-for-each f vs)
  (let* ([end (vector-length vs)])
    (let loop ([i 0])
      (if (eq? i end)
          vs
          (begin
            (f (vector-ref vs i))
            (loop (add1 i)))))))

(define (list->vector xs)
  (let ([vs (make-vector (length xs))])
    (let loop ([xs xs] [i 0])
      (if (pair? xs)
          (begin
            (vector-set! vs i (car xs))
            (loop (cdr xs) (add1 i)))
          vs))))

(define (vector->list vs)
  (let loop ([acm '()] [i 0])
    (if (eq? (vector-length vs) i)
        (reverse acm)
        (loop (cons (vector-ref vs i) acm) (add1 i)))))

(define (vector-append vs ws)
  (list->vector (append (vector->list vs) (vector->list ws))))

(define (vector-fill! vs v)
  (vector-map! (lambda (x) v) vs))

(define (vector-copy vs)
  (vector-map (lambda (x) x) vs))

;;; ASCII character libraries
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
  (let ([whitespaces '(#\space #\newline #\return #\linefeed #\tab)])
    (lambda (c) (if (member c whitespaces) #t #f))))

;;; string
; https://scheme.com/tspl4/examples.html#./examples:s37
(define (format f . args)
  #|
  ; https://cisco.github.io/ChezScheme/csug9.6/io.html#./io:s106
    ; When the first argument to format is a string or #f (first and second forms above), format constructs an output string from format-string and the objects obj .... Characters are copied from format-string to the output string from left to right, until format-string is exhausted. The format string may contain one or more format directives, which are multi-character sequences prefixed by a tilde ( ~ ). Each directive is replaced by some other text, often involving one or more of the obj ... arguments, as determined by the semantics of the directive.
    ; procedure: (format format-string obj ...)
    ; procedure: (format #f format-string obj ...)
    ; procedure: (format #t format-string obj ...)
    ; procedure: (format textual-output-port format-string obj ...)
  |#
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
            (or (eq? (maybe-ref s (+ i 1)) #\a)
                (eq? (maybe-ref s (+ i 1)) #\s)))
        (if (not (pair? args))
            (error "format" "too many control string"))
        (loop s (+ i 2) (+ i 2) (cdr args) (cons (obj->repr (car args)) (cons (substring s j i) acm)))]
      [else (loop s (add1 i) j args acm)]))
  
  (cond [(string? f) (loop f 0 0 args '())]
        [(and (port? f) (pair? args) (string? (car args)))
         (display (loop (car args) 0 0 (cdr args) '()) f)]
        [(and (boolean? f) f (pair? args) (string? (car args)))
         (display (loop (car args) 0 0 (cdr args) '()) (current-output-port))]
        [(and (boolean? f) (not f) (pair? args) (string? (car args)))
         (loop (car args) 0 0 (cdr args) '())]
        [else (error "format" "unknown args" f args)]))

(define (obj->repr obj)
  (define (number->string n) (integer->string n))
  (cond
    [(integer? obj)
     (if (< obj 0)
         (string-append "-" (integer->string (- obj)))
         (integer->string obj))]
    [(char? obj) (string-append "#\\" (make-string obj))]
    [(boolean? obj) (if obj "#t" "f")]
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
    [else (error "obj->repr" "unknown-obj" obj)]))

(define (string-equal? x y)
  (and (string? x) (string? y)
    (let ([s (string-length x)]
          [t (string-length y)])
      (and (eq? s t)
        (let loop ([i 0])
          (if (eq? i s)
              #t
              (and (eq? (string-ref x i) (string-ref y i)) (loop (add1 i)))))))))

(define (list->string xs)
  (for-each
    (lambda (c)
      (if (not (char? c))
          (error "list->string" "list->string expect char")
          #t))
    xs)
  
  (let* ([str-len (length xs)]
         [s (make-string str-len)])
    (let loop ([xs xs] [i 0])
      (if (pair? xs)
          (begin
            (string-set! s i (car xs))
            (loop (cdr xs) (add1 i)))
          s))))

(define (string->list s)
  (let loop ([xs '()] [i 0])
    (if (eq? i (string-length s))
        (reverse xs)
        (loop (cons (string-ref s i) xs) (add1 i)))))

(define (substring s i j)
  (let* ([len (- j i)]
         [t (make-string len)])
    (let loop ([i i] [k 0])
      (if (>= i j)
          t
          (begin
            (string-set! t k (string-ref s i))
            (loop (add1 i) (add1 k)))))))

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

(define (string-copy s)
  (let ([s* (make-string (string-length s))])
    (string-copy! s 0 s* 0 (string-length s))
    s*))

(define (string-copy! src src-start dst dst-start n)
  (if (= n 0)
      dst
      (begin
        (string-set! dst dst-start (string-ref src src-start))
        (string-copy! src (add1 src-start) dst (add1 dst-start) (sub1 n)))))

(define (string-fill! s c)
  (let loop ([i 0])
    (if (eq? i (string-length s))
        0
        (begin
          (string-set! s i c)
          (loop (add1 i))))))

(define (string-append . ss)
  (let ([res (make-string (fold-left + 0 (map string-length ss)))])
    (let loop ([ss ss] [split 0])
      (if (pair? ss)
          (begin
            (string-copy! (car ss) 0 res split (string-length (car ss)))
            (loop (cdr ss) (+ (string-length (car ss)) split)))  
          res))))

(define (string . xs)
  (list->string xs))

(define (integer->string i)
  (define (int->char x)
    (integer->char (+ x (char->integer #\0))))
  (let loop ([i i] [acm '()])
    (if (= i 0)
        (if (pair? acm)
            (list->string (map int->char acm))
            "0")
        (loop (div i 10) (cons (mod i 10) acm)))))
