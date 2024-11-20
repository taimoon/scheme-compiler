(define %r6rs-named-characters
  (list
    (list #\nul "nul" 0)
    (list #\alarm "alarm" 7)
    (list #\backspace "backspace" 8)
    (list #\tab "tab" 9)
    (list #\linefeed "linefeed" 10)
    (list #\newline "newline" 10)
    (list #\vtab "vtab" 11)
    (list #\page "page" 12)
    (list #\return "return" 13)
    (list #\esc "esc" 27)
    (list #\space "space" 32)
    (list #\delete "delete" 127)))

(define %escaped-string-element
  (list
    (list #\\ "\\\\")
    (list #\" "\\\"")
    (list #\tab "\\t")
    (list #\newline "\\n")))

(define (string-repr s)
  (define (char->string-repr ch)
    (let ((res (assq ch %escaped-string-element)))
      (if res
          (cadr res)
          (string ch))))
  (let loop ((i 0) (acm "\""))
    (if (= i (string-length s))
        (string-append acm "\"")
        (loop (add1 i)
              (string-append acm
                             (char->string-repr (string-ref s i)))))))

(define (put-string s op)
  (let loop ((i 0))
    (if (>= i (string-length s))
        #t
        (begin
          (write-char (string-ref s i) op)
          (loop (add1 i))))))

(define (char-name ch)
  (let ((name (assq ch %r6rs-named-characters)))
    (if (pair? name)
        (cadr name)
        name)))

(define (%display x op)
  (cond
    ((char? x)
     ;;; see r5rs, r7rs
     (write-char x))
    ((integer? x)
     (%display (number->string x) op))
    ((boolean? x)
     (write-char #\# op)
     (if x
         (write-char #\t op)
         (write-char #\f op)))
    ((vector? x)
     (%display "#(" op)
     (let loop ((i 0))
      (if (>= i (sub1 (vector-length x)))
          (begin
            (%display (vector-ref x i) op)
            (%display ")" op))
          (begin
            (%display (vector-ref x i) op)
            (write-char #\space op)
            (loop (add1 i))))))
    ((string? x)
     (put-string x op))
    ((symbol? x)
     (%display (symbol->string x)))
    ((null? x)
     (%display "()" op))
    ((procedure? x)
     (%display "#<procedure>" op))
    ((pair? x)
     (write-char #\( op)
     (%display (car x) op)
     (let loop ((x (cdr x)))
      (cond
        ((pair? x)
         (write-char #\space op)
         (%display (car x) op)
         (loop (cdr x)))
        ((null? x)
         (write-char #\) op))
        (else
          (write-char #\space op)
          (write-char #\. op)
          (write-char #\space op)
          (%display x op)
          (write-char #\) op)))))
    (else (%display "#<unknown>" op))))

(define (%write x op)
  (cond
    ((integer? x)
     (put-string (number->string x) op))
    ((boolean? x)
     (write-char #\# op)
     (if x
         (write-char #\t op)
         (write-char #\f op)))
    ((char? x)
     (put-string "#\\" op)
     (let ((maybe-name (char-name x)))
      (if maybe-name
          (put-string maybe-name op)
          (write-char x op))))
    ((vector? x)
     (put-string "#(" op)
     (if (> (vector-length x) 0)
         (let loop ((i 0))
          (if (>= i (sub1 (vector-length x)))
              (%write (vector-ref x i) op)
              (begin
                (%write (vector-ref x i) op)
                (write-char #\space op)
                (loop (add1 i)))))
         0)
     (put-string ")" op))
    ((string? x)
     (put-string (string-repr x) op))
    ((symbol? x)
     (put-string (symbol->string x) op))
    ((null? x)
     (put-string "()" op))
    ((procedure? x)
     (put-string "#<procedure>" op))
    ((pair? x)
     (write-char #\( op)
     (%write (car x) op)
     (let loop ((x (cdr x)))
      (cond
        ((pair? x)
         (write-char #\space op)
         (%write (car x) op)
         (loop (cdr x)))
        ((null? x)
         (write-char #\) op))
        (else
          (put-string " . " op)
          (%write x op)
          (write-char #\) op)))))
    (else (put-string "#<unknown>" op))))

(define display
  (case-lambda
    ((x) (%display x (current-output-port)))
    ((x op) (%display x op))))

(define write
  (case-lambda
    ((x) (%write x (current-output-port)))
    ((x op) (%write x op))))
