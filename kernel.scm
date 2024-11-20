(define (get-process-id)
  (foreign-call s_getpid))

(define (exit x)
  (foreign-call s_exit x))

(define (system cmd)
  (foreign-call s_system cmd))

(define command-line
  (let ((res (foreign-call s_init_cmd_ln)))
    (lambda () res)))

(define **port** '**port**)

(define (make-port p*)
  (list **port** p*))

(define (port? p)
  (and (pair? p) (eq? (car p) **port**)))

(define (%port p)
  (car (cdr p)))

(define (standard-output-port) (make-port (foreign-call s_stdout)))
(define (current-output-port) (make-port (foreign-call s_stdout)))

(define (open-port filename mode)
  (make-port (foreign-call s_fopen filename mode)))

(define (close-port p)
  (foreign-call s_fclose (%port p)))

(define (open-input-file filename)
  (open-port filename "r"))

(define (open-output-file filename)
  (open-port filename "w"))

(define close-output-port close-port)
(define close-input-pourt close-port)

(define **eof-object** '**eof-object**)

(define (eof-object) **eof-object**)

(define (eof-object? x) (eq? (eof-object) x))

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
    ((ch) (foreign-call s_write_char ch (%port (standard-output-port))))
    ((ch p) (foreign-call s_write_char ch (%port p)))))

(define (list . x) x)

(define %ffi-c-write
  (case-lambda
    ((x) (foreign-call s_write x (%port (standard-output-port))))
    ((x op) (foreign-call s_write x (%port op)))))

(define newline
  (case-lambda
    (() (write-char #\newline))
    ((op) (write-char #\newline op))))

(define (error . args)
  (%ffi-c-write "error")
  (newline)
  (%ffi-c-write args)
  (exit 1))

(define (ash n m)
  (if (< m 0)
      (ashr n (- m))
      (ashl n m)))
