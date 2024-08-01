#|
  https://conservatory.scheme.org/schemers/Documents/Standards/R5RS/r5rs.pdf
  https://www.r6rs.org/final/r6rs.pdf
  
  / - forward slash or simply slash, mostly used in *nix
  \ - backslash, used in windows


  #' #` #, #,@ are for syntatic abstraction, not implemented

  number supports only fixnum with optional prefixed radix
|#
(load "match.scm")
(load "utils.scm")

(define (generate-tmp-filename)
  (string-append "/tmp/scheme_port_tmp"
                 (gensym->unique-string (gensym)) 
                 ".txt"))

(define (read-file->string filename)
  (let* ([p (open-input-file filename)]
         [res (read-string p)])
  (close-port p)
  res))

(define (write-string->file filename s)
  (let* ([p (open-output-file filename)])
  (write-string s p)
  (close-port p)))

#;(define (write-string s port)
  (let ([write-char (lambda (c) (write-char c port))])
    (let loop ([i 0])
      (if (>= i (string-length s))
          (eof-object)
          (begin (write-char (string-ref s i))
                 (loop (add1 i)))))))

(define (read-string port)
  (let ([read-char (lambda () (read-char port))])
    (let loop ([c (read-char)] [ss '()])
      (if (eof-object? c)
          (apply string (reverse ss))
          (loop (read-char) (cons c ss))))))

(define (string->input-port s)
  (let ([tmp (generate-tmp-filename)])
    (write-string->file tmp s)
    (open-input-file tmp)))


(define extended-alphabets
  (list #\! #\$ #\% #\& #\* #\+ #\- #\. #\/ #\: #\< #\= #\> #\? #\@
    #\^ #\_ #\~))

(define str-buf (make-string 512))

(define (next-token p)
  #|
    return datum as datum; otherwise list-structured token such as
    (left-paren)
    (right-paren)
    (quote) (unquote) (unquote-splicing) (quasiquote)
  |#
  (let ([c (read-char p)])
    (cond 
      [(eof-object? c) c]
      [(eq? c #\') (list 'quote)]
      [(eq? c #\`) (list 'quasiquote)]
      [(eq? c #\,)
       (if (eq? (peek-char p) #\@)
            (begin (read-char p) (list 'unquote-splicing))
            (list 'unquote))]
      [(eq? c #\.)
       (let ([c (peek-char p)])
        (if (or (char-alphabetic? c) (member? c extended-alphabets))
            (begin (unread-char c p) (read-identifier p))
            (list 'dot)))]
      [(eq? c #\") (read-quote-delimited-string p)]
      [(eq? c #\;) (read-comment p)]
      [(eq? c #\#) (read-hash-prefixed p)]
      [(eq? c #\-)
       (if (not (char-numeric? (peek-char p)))
           (begin (unread-char c p) (read-identifier p))
           (- (read-number p 10)))]
      [(char-numeric? c) (unread-char c p) (read-number p 10)]
      [(or (char-alphabetic? c) (member? c extended-alphabets))
       (unread-char c p)
       (read-identifier p)]
      [(left-paren? c) (list 'left-paren)]
      [(right-paren? c) (list 'right-paren)]
      [(char-whitespace? c) (next-token p)]
      [else (error next-token 'unknown:lexeme c)])))

(define (read-quote-delimited-string p)
  (let loop ([i 0] [c (read-char p)])
    (cond 
      [(eof-object? c) (error 'read-quote-delimited-string 'open-string c)]
      [(eq? c #\\)
       (string-set! str-buf i
        (match (read-char p)
          [#\" #\"]
          [#\a #\alarm]
          [#\b #\backspace]
          [#\t #\tab]
          [#\n #\linefeed]
          [#\v #\vtab]
          [#\f #\page]
          [#\r #\return]
          [#\\ #\\]
          [,c (error "unknown escape character" c)]))
        (loop (add1 i) (read-char p))]
      [(eq? c #\") (substring str-buf 0 i)]
      [(or (char-whitespace? c)
           (char? c))
       (string-set! str-buf i c)
       (loop (add1 i) (read-char p))]
      [else (error 'read-quote-delimited-string 'unknown:lexeme c)])))

(define (read-comment p)
  (let loop ([c (read-char p)])
    (if (or (eof-object? c) (eq? c #\newline))
        (next-token p)
        (loop (read-char p)))))

(define (read-block-comment p)
  (let loop ([c (read-char p)])
    (cond 
      [(eof-object? c)
       (error 'read-block-comment "expect closing block comment")]
      [(and (eq? c #\|) (eq? (peek-char p) #\#))
       (read-char p) (next-token p)]
      [(and (eq? c #\#) (eq? (peek-char p) #\|))
       (read-char p)
       (read-block-comment p)
       (read-block-comment p)]
      [else (loop (read-char p))])))

(define (read-hash-prefixed p)
  (let ([c (read-char p)])
    (cond 
      [(eof-object? c) c]
      [(eq? (char-downcase c) #\t) #t]
      [(eq? (char-downcase c) #\f) #f]
      [(eq? (char-downcase c) #\b) (read-number p 2)]
      [(eq? (char-downcase c) #\o) (read-number p 8)]
      [(eq? (char-downcase c) #\d) (read-number p 10)]
      [(eq? (char-downcase c) #\x) (read-number p 16)]
      [(eq? c #\\) (read-literal-char p)]
      [(eq? c #\|) (read-block-comment p)]
      [(eq? c #\;) (parse (tokenizer p)) (next-token p)]
      [else (error 'read-hash-prefixed 'unknown:lexeme c)])))

(define named-characters
  (map (lambda (b) (append (string->list (car b)) (list (cadr b))))
    '(
      ("alarm" #\alarm)
      ("backspace" #\backspace)
      ("tab" #\tab)
      ("space" #\space)
      ("newline" #\newline)
      ("linefeed" #\linefeed)
      ("vtab" #\vtab)
      ("page" #\page)
      ("return" #\return)
    )))

(define (read-named-char c name p)
  (if (pair? name)
      (if (eq? c (car name))
          (if (and (pair? (cdr name)) (not (pair? (cdr (cdr name)))))
              (car (cdr name))
              (read-named-char (read-char p) (cdr name) p))
          (error "read-name-char" "mismatch-lexeme" c (car name)))
      (begin (unread-char c p) name)))

(define (read-literal-char p)
  (let* ([c (read-char p)]
         [name (assoc c named-characters)])
    (cond [(and name (eq? (cadr name) (peek-char p)))
           (read-named-char (read-char p) (cdr name) p)]
          [else c])))

(define (read-number p radix)
  (let loop ([i 0]
             [c (read-char p)])
  (cond [(eof-object? c)
         (string->number (substring str-buf 0 i) radix)]
        [(or (and (= radix 16)
                  (or (and (char<=? #\0 c) (char<=? c #\9))
                      (and (char<=? #\a (char-downcase c)) (char<=? (char-downcase c) #\f))))
             (and (= radix 10) (char<=? #\0 c)  (char<=? c #\9))
             (and (= radix 8)  (char<=? #\0 c) (char<=? c #\7))
             (and (= radix 2)  (char<=? #\0 c) (char<=? c #\1)))
         (string-set! str-buf i c)
         (loop (add1 i) (read-char p))]
        [else (unread-char c p) (string->number (substring str-buf 0 i) radix)])))

(define (read-identifier p)
  (let loop ([i 0]
             [c (read-char p)])
  (cond [(eof-object? c)
         (string->symbol (substring str-buf 0 i))]
        [(or (char-alphabetic? c)
             (char-numeric? c)
             (member? c extended-alphabets))
         (string-set! str-buf i c)
         (loop (add1 i) (read-char p))]
        [else (unread-char c p) (string->symbol (substring str-buf 0 i))])))

(define (member? x s)
  (if (member x s) #t #f))

(define left-paren
  '(#\( #\[))

(define right-paren
  '(#\) #\]))

(define (left-paren? x)
  (member? x left-paren))

(define (right-paren? x)
  (member? x right-paren))

(define (tokenizer p)
  (let ([buf '()])
    (lambda (msg)
      (case msg
        [(peek)
         (if (pair? buf)
             (car buf)
             (let ([res (next-token p)])
              (set! buf (list res))
              res))]
        [(next)
         (if (pair? buf)
             (let ([res (car buf)])
              (set! buf '())
              res)
             (next-token p))]
        [else (error 'tokenizer "unknown msg" msg)]))))

(define (tokenizer-next tk) (tk 'next))

(define (tokenizer-peek tk) (tk 'peek))

(define (parse-list tk)
  (match (tokenizer-peek tk)
    [(right-paren) (tokenizer-next tk) '()]
    [(dot)
     (tokenizer-next tk)
     (let* ([res (parse tk)]
            [c (tokenizer-peek tk)])
       (if (not (equal?  c '(right-paren)))
           (error "parse-list" "expect closing right-paren" c)
           (tokenizer-next tk))
       res)]
    [,v
     (let* ([left (parse tk)]
            [right (parse-list tk)])
      (cons left right))]))

(define (lexing-all p)
  (let loop ([c (next-token p)] [acm '()])
    (if (eof-object? c)
        (reverse acm)
        (loop (next-token p) (cons c acm)))))

(define (parse tk)
  (match (tokenizer-next tk)
    [(,abbrv)
     (guard (member abbrv '(quote unquote quasiquote unquote-splicing)))
     (list abbrv (parse tk))]
    [,v
     (guard (or (string? v) (char? v) (boolean? v) (integer? v) (symbol? v) (eof-object? v)))
     v]
    [(left-paren)
     (parse-list tk)]
    [,v (error "parse" "parse-error" v)]))


(define (read-sexps-from-path filename)
  ; impl
  (let* ([ip (open-input-file filename)]
         [ip (tokenizer ip)])
    (let recur ([sexp (parse ip)])
      (if (eof-object? sexp)
          '()
          (cons sexp (recur (parse ip)))))))
