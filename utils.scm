(define generate-label
  (let ([counter 0])
    (lambda (prefix)
      (set! counter (add1 counter))
      (string->symbol (format "~a_~a_~a" prefix (get-process-id) counter)))))

(define (string->id-string str)
  (define (integer->alphabet-str n)
    (string #\_ (integer->char (+ n (char->integer #\a))) #\_))
  (define extended-alphabets
    (let ([xs (list #\! #\$ #\% #\& #\* #\+ #\- #\. #\/ #\: #\< #\= #\> #\? #\@
      #\^ #\_ #\~)])
      (map cons
           xs
           (map integer->alphabet-str (iota (length xs))))))
  (define (numerir-char? ch)
    (and (char<=? #\0 ch) (char<=? ch #\9)))
  (define (valid-id-char? ch)
    (or (and (char<=? #\a ch) (char<=? ch #\z))
        (and (char<=? #\A ch) (char<=? ch #\Z))
        (char=? #\_ ch)
        (numerir-char? ch)))
  (define underscore-str (string #\_))
  (define (char->valid-id-str ch)
    (if (valid-id-char? ch)
        (string ch)
        (let ([res (assoc ch extended-alphabets)])
          (if res
              (cdr res)
              underscore-str))))
  (apply string-append (map char->valid-id-str (string->list str))))

(define (symbol->id-symbol sym)
  (string->symbol (string->id-string (symbol->string sym))))

(define (read-sexps-from-path filename)
  (let ([ip (open-input-file filename)])
   (let recur ([sexp (read ip)])
      (if (eof-object? sexp)
          '()
          (cons sexp (recur (read ip)))))))

(define (write-sexps-to-path sexps filename)
  (let ([op (open-output-file filename)])
    (for-each (lambda (sexp) (write sexp op)) sexps)
    (close-output-port op)))

(define (make-env) '())

(define (assoc-env x env)
  (assoc x env))

(define (apply-env x env)
  (let ([res (assoc x env)])
    (if res
        (cdr res)
        (error "apply-env" "unbound-variable" x))))

(define (extend-env xs vs env)
  (append (map cons xs vs) env))

(define (count-cons xs)
  (if (pair? xs)
      (add1 (+ (count-cons (car xs)) (count-cons (cdr xs))))
      0))

(define (estimate-exp-size e)
  (define wordsize 4)
  (cond
    [(or (integer? e) (char? e) (boolean? e) (null? e)) wordsize]
    [(pair? e)
     (+ wordsize
        (+ (estimate-exp-size (car e))
           (estimate-exp-size (cdr e))))]
    [(vector? e)
     (let loop ([acm 0] [i 0])
        (if (eq? (vector-length e) i)
            (+ wordsize acm)
            (loop (+ acm (estimate-exp-size (vector-ref e i))) (add1 i))))]
    [(symbol? e) (estimate-exp-size (symbol->string e))]
    [(string? e) (+ wordsize (string-length e))]
    [else (error "estimate-exp-size" "unknown object" e)]))

#;((fold-left cons '() '(1 2 3 4))
   ; (fold-left (lambda (acm x) ...) init seq)
   => '((((() . 1) . 2) . 3) . 4)
   ; (fold-right (lambda (x acm) ...) init seq)
   (fold-right cons '() '(1 2 3 4))
   => '(1 2 3 4))

(define (list->set xs)
  (fold-right
    (lambda (x ss) (if (member x ss) ss (cons x ss)))
    '()
    xs))

(define (make-set . xs)
  (list->set xs))

(define (set-empty? s)
  (eq? s '()))

(define (set-add-elem x xs)
  (if (member x xs)
      xs
      (cons x xs)))

(define (set-union xs ys)
  (fold-right set-add-elem xs ys))

(define (set-diff xs ys)
  (filter (lambda (x) (not (member x ys))) xs))

(define (set-intersection xs ys)
  (set-diff ys (set-diff ys xs)))

(define (set-symmetric-diff xs ys)
  (set-union (set-diff xs ys) (set-diff ys xs)))

(define (set-equal? xs ys)
  (set-empty? (set-symmetric-diff xs ys)))

(define (subset>=? xs ys)
  ; subset and greater than share the same partial order properties
  (set-empty? (set-diff xs ys)))

(define (subset<=? xs ys)
  (subset>=? ys xs))

(define (improper->list xs)
  (if (pair? xs)
      (cons (car xs) (improper->list (cdr xs)))
      (if (null? xs)
          '()
          (list xs))))

(define (file-exist? path)
  (eq? 0 (system (format "test -f ~a" path))))