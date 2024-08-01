(load "parser.scm")

(define (read*-sexps-from-path filename)
  ; snarfed
  (let ([ip (open-input-file filename)])
    (let recur ([sexp (read ip)])
      (if (eof-object? sexp)
          '()
          (cons sexp (recur (read ip)))))))

(define prog "( 
  string->input-port
  string 123 -233 #b111 #B111 #o777 #O777 
  ; string 123 -233 #b111 #B111 #o777 #O777
  #d999 #D999 999 #xfff #XFFF
  -#d101
  #\\a #\\t #\\f
  #\\alarm #\\backspace #\\tab #\\space #\\linefeed #\\vtab #\\page #\\return
  '[ ]
  `z 'y ,x ,@sth `,x `,@sth
  \"hakurei\"
  )")

(define datum (list
  123 -233 #b111 #B111 #o777 #O777 
  #d999 #D999 999 #xfff #XFFF
  #\alarm
  #\backspace
  #\tab
  #\space
  #\newline
  #\linefeed
  #\vtab
  #\page
  #\return
))

; test which unmatch using chez scheme reader
#;(let* ([f "log.scm"]
       [_ (pretty-print (lexing-all (open-input-file f)))]
       [p (open-input-file f)]
       [q (open-input-file f)])
  (let loop ([x (parse (tokenizer p))] [y (read q)])
    (cond [(and (eof-object? x) (eof-object? y)) #t]
          [(not (equal? x y))
           (pretty-print x) (pretty-print y)
           (error "test-parser" "unmatch")]
          [else (set! y (read q))
                (loop (parse (tokenizer p)) y)])))

; test against chez scheme reader
(for-each pretty-print
  (map (lambda (f) (cons f (equal? (read-sexps-from-path f) (read*-sexps-from-path f))))
  (list 
    "utils.scm"
    "parser.scm"
    "top-program.scm"
    "match.scm"
    "preprocessor.scm"
    "scheme-libs.scm"
    )))