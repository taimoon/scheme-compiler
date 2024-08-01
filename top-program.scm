(define SYMBOL-STR-TABLE '())

(define (string->symbol str)
  (define (string-equal? s1 s2)
    (and (string? s1) (string? s2)
      (= (string-length s1) (string-length s2))
      (let loop ([i 0])
        (if (eq? i (string-length s1))
            #t
            (if (eq? (string-ref s1 i) (string-ref s2 i))
                (loop (add1 i))
                #f)))))
  (let loop ([xs SYMBOL-STR-TABLE])
    (if (null? xs)
        (begin
          (set! SYMBOL-STR-TABLE (cons str SYMBOL-STR-TABLE))
          (str->sym str))
        (if (string-equal? (car xs) str)
            (str->sym (car xs))
            (loop (cdr xs))))))

(define (symbol->string sym)
  (define (string-copy s)
    (let ([t (make-string (string-length s))])
      (let loop ([i 0])
        (if (eq? (string-length s) i)
            t
            (begin (string-set! t i (string-ref s i))
                   (loop (add1 i)))))))
  (string-copy (sym->str sym)))

(define (display obj . p)
  (if (and (pair? p) (port? (car p)))
      (foreign-call s_display obj (port-fptr (car p)))
      (foreign-call s_display obj (port-fptr (current-output-port)))))

(define (newline . p)
  (if (and (pair? p) (port? (car p)))
      (foreign-call s_fnewline (port-fptr (car p)))
      (foreign-call s_newline)))

(define (pretty-print e . p)
  (cond [(and (pair? p) (port? (car p)))
         (foreign-call s_fshow e (port-fptr (car p)))
         (foreign-call s_fnewline (port-fptr (car p)))]
        [else
          (foreign-call s_show e)
          (foreign-call s_newline)])
  ;;; TODO: (newline p) sometimes doesn't put newline to p, but to stdout
  #;(if (and (pair? p) (port? (car p)))
      (foreign-call s_fshow e (port-fptr (car p)))
      (foreign-call s_show e))
  #;(newline p))

(define (error who . msg)
  (foreign-call s_show "who: ") (foreign-call s_show who) (foreign-call s_newline)
  (foreign-call s_show "message ") (foreign-call s_show msg) (foreign-call s_newline)
  (exit))

(define (command-line-arguments) (foreign-call s_cmd_ln))
(define (system s) (foreign-call s_system s))
(define (exit) (foreign-call s_exit))
(define (div n d) (foreign-call s_div n d))
(define (mod n d) (foreign-call s_mod n d))
(define (get-process-id) (foreign-call s_getpid))

(define (number? x) (integer? x))
(define (list . x) x)
(define (ash x y)
  (cond [(= y 0) x]
        [(< y 0) (ashr x y)]
        [else (ashl x y)]))

(define port-id "port_1")
(define eof-object-id "eof_object_1")

(define (eof-object)
  (vector eof-object-id))

(define (eof-object? p)
  (and
    (vector? p)
    (= (vector-length p) 1)
    (eq? (vector-ref p 0) eof-object-id)))

(define (make-port file-name file-descriptor fptr)
  (vector port-id fptr file-name file-descriptor (make-string 0) 0 0))

(define (current-input-port)
  (standard-input-port))
(define (current-output-port)
  (standard-output-port))

(define (standard-input-port)
  (make-port 0 0 (foreign-call s_stdin)))
(define (standard-output-port)
  (make-port 0 0 (foreign-call s_stdout)))
(define (standard-error-port)
  (make-port 0 0 (foreign-call s_stderr)))

(define (port? p)
  (and
    (vector? p)
    (= (vector-length p) 7)
    (eq? (vector-ref p 0) port-id)))
(define (port-fptr p)
  (vector-ref p 1))
(define (port-file-name p)
  (vector-ref p 2))
(define (port-file-descriptor p)
  (vector-ref p 3))
(define (port-buffer p)
  (vector-ref p 4))
(define (port-index p)
  (vector-ref p 5))
(define (port-buffer-size p)
  (vector-ref p 6))
(define (open-file path options)
  (make-port path options (foreign-call s_fopen path options)))
(define (close-port port)
  (if (port? port)
      (foreign-call s_fclose (port-fptr port))
      (error "close-port" "expect port")))
(define close-output-port close-port)
(define close-input-port close-port)

(define (open-input-file path)
  (open-file path "r"))
(define (open-output-file path)
  (open-file path "w"))
(define (open-input-output-file path)
  (open-file path "w+"))

(define (peek-char port)
  (let ([ch (read-char port)])
    (unread-char ch port)
    ch))

(define (unread-char c port)
  (if (port? port)
      (foreign-call s_ungetc c (port-fptr port))
      (error "read-char" "expect port" port)))

(define (read-char port)
  (if (port? port)
      (let ([res (foreign-call s_fgetc (port-fptr port))])
        (cond [(not (integer? res)) (error "read-char" "expect-return-integer")]
              [(< res 0) (eof-object)]
              [else (integer->char res)]))
      (error "read-char" "expect port" port)))

(define (write . p)
  (error "write" "not implemented"))

(define (write-char c port)
  (if (port? port)
      (foreign-call s_fputc c (port-fptr port))
      (error "write-char" "expect port" port)))

(define (write-string s port)
  (if (port? port)
      (foreign-call s_fputs s (port-fptr port))
      (error "write-string" "expect port" port)))

(define (get-string-all port)
  (list->string
    (let recur [(ch (read-char port))]
      (if (eof-object? ch)
          '()
          (cons ch (recur (read-char port)))))))

(define (file-position port pos)
  (if (port? port)
      (foreign-call s_fseek (port-fptr port) pos)
      (error "file-position" "expect port" port)))