(define (get-process-id)
  (foreign-call s_getpid))

(define exit
  (case-lambda
    (() (foreign-call s_exit 0))
    ((x) (if (integer? x) (foreign-call s_exit x) (foreign-call s_exit 1)))))

(define (system cmd)
  (foreign-call s_system cmd))

(define (getenv name)
  (if (not (string? name))
      (error "getenv" "expect-string" name)
      (foreign-call s_getenv name)))

(define command-line
  (let ()
    (define (vector->list v)
      (let recur ((i 0))
        (if (= i (vector-length v))
            '()
            (cons (vector-ref v i) (recur (add1 i))))))
    (let ((cmdln (vector->list (foreign-call s_cmd_ln))))
      (lambda () cmdln))))

(define **port** "**port**")

(define (make-port p*)
  (list **port** p*))

(define (port? p)
  (and (pair? p) (eq? (car p) **port**)))

(define (%port p)
  (car (cdr p)))

(define (open-port filename mode)
  (let ((r (foreign-call s_fopen filename mode)))
    (if (eq? r 0)
        (error "open-port" "cannot open file" filename)
        (make-port r))))

(define (close-port p)
  (foreign-call s_fclose (%port p)))

(define (open-input-file filename)
  (open-port filename "r"))

(define (open-output-file filename)
  (open-port filename "w"))

(define close-output-port close-port)
(define close-input-pourt close-port)

(define current-output-port
  (let ((p (make-port (foreign-call s_stdout))))
    (lambda () p)))

(define current-error-port
  (let ((p (make-port (foreign-call s_stderr))))
    (lambda () p)))

(define current-input-port
  (let ((p (make-port (foreign-call s_stdin))))
    (lambda () p)))

(define (read-char ip)
  (let ((res (foreign-call s_read_char (%port ip))))
    (if (eq? res -1)
        (eof-object)
        (integer->char res))))

(define (unread-char ch ip)
  (foreign-call s_ungetc ch (%port ip)))

(define (peek-char port)
  (let ((ch (read-char port)))
    (unread-char ch port)
    ch))

(define write-char
  (case-lambda
    ((ch) (foreign-call s_fwrite_char ch (%port (current-output-port))))
    ((ch op) (foreign-call s_fwrite_char ch (%port op)))))

(define %ffi-c-write
  (case-lambda
    ((x) (foreign-call s_fwrite x (%port (current-output-port))))
    ((x op) (foreign-call s_fwrite x (%port op)))))

(define newline
  (case-lambda
    (() (write-char #\newline))
    ((op) (write-char #\newline op))))

(define (error . args)
  (%ffi-c-write (cons "error" args) (current-error-port))
  (newline (current-error-port))
  (exit 1))

(define (ash n m)
  (if (< m 0)
      (ashr n (- m))
      (ashl n m)))
