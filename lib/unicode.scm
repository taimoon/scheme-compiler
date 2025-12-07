(define (abort)
  (foreign-call abort))

(define exit
  (case-lambda
    (() (foreign-call exit 0))
    ((n) (foreign-call s_exit n))))

(define (error . args)
  (foreign-call s_writeln args (label stderr))
  (abort))

(define (%utf32->utf8! c32 buf off)
  (let ((c32 (char->integer c32)))
    (cond
      ((<= c32 #x7F)
        (bytevector-u8-set! buf (+ off 0) c32)
        1)
      ((<= c32 #x7FF)
        (bytevector-u8-set! buf (+ off 0) (bitwise-ior #xC0 (ash c32 -6)))
        (bytevector-u8-set! buf (+ off 1) (bitwise-ior #x80 (bitwise-and c32 #x3F)))
        2)
      ((and (<= c32 #xFFFF)
            (>= c32 #xD800)
            (<= c32 #xDFFF))
        0)
      ((<= c32 #xFFFF)
        (bytevector-u8-set! buf (+ off 0) (bitwise-ior #xE0 (ash c32 -12)))
        (bytevector-u8-set! buf (+ off 1) (bitwise-ior #x80 (bitwise-and (ash c32 -6) #x3F)))
        (bytevector-u8-set! buf (+ off 2) (bitwise-ior #x80 (bitwise-and c32 #x3F)))
        3)
      ((<= c32 #x10FFFF)
        (bytevector-u8-set! buf (+ off 0) (bitwise-ior #xF0 (ash c32 -18)))
        (bytevector-u8-set! buf (+ off 1) (bitwise-ior #x80 (bitwise-and (ash c32 -12) #x3F)))
        (bytevector-u8-set! buf (+ off 2) (bitwise-ior #x80 (bitwise-and (ash c32 -6) #x3F)))
        (bytevector-u8-set! buf (+ off 3) (bitwise-ior #x80 (bitwise-and c32 #x3F)))
        4)
      (else 0))))

(define (char-utf-8-length c)
  (let ((c32 (char->integer c)))
    (cond
      ((<= c32 #x7F) 1)
      ((<= c32 #x7FF) 2)
      ((and (<= c32 #xFFFF)
            (>= c32 #xD800)
            (<= c32 #xDFFF))
       0)
      ((<= c32 #xFFFF) 3)
      ((<= c32 #x10FFFF) 4)
      (else 0))))

(define %string->utf8 (let ()
  (define (string-fold f init s)
    (let loop ((i 0)
               (init init))
        (if (>= i (string-length s))
            init
            (loop (+ i 1) (f init (string-ref s i))))))

  (define (string-utf8-length s)
    (string-fold (lambda (s c) (+ s (char-utf-8-length c))) 0 s))

  (define (%string->utf8 s null-terminate?)
    (let loop ((i 0)
               (j 0)
               (buf (make-bytevector (+ (string-utf8-length s) (if null-terminate? 1 0)) 0)))
      (if (>= i (string-length s))
          buf
          (loop (+ i 1)
                (+ j (%utf32->utf8! (string-ref s i) buf j))
                buf))))
  %string->utf8))

(define (system cmd)
  (foreign-call s_system (%string->utf8 cmd #t)))

(define (string->utf8 s)
  (%string->utf8 s #f))

(define %read-char (let ()
(define (%read-char-u8 ip)
  (let ((b (read-u8 ip)))
    (cond
      ((eof-object? b) (error "%read-char" "expect-more-byte"))
      ((not (= (ash b -6) #b10)) (error "%read-char" "bad-byte" (ash b -6) b))
      (else b))))
(define (%read-char ip)
    (let ((b0 (read-u8 ip)))
      (cond
        ((eof-object? b0) b0)
        ((= 0 (ash b0 -7)) (integer->char b0))
        ((= #b110 (ash b0 -5))
         (let* ((b1 (%read-char-u8 ip))
                (xxx (bitwise-and #b111 (ash b0 -2)))
                (yyyy
                 (bitwise-ior
                   (ash (bitwise-and #b11 b0) 2)
                   (bitwise-and #b11 (ash b1 -4))))
                (zzzz (bitwise-and #b1111 b1)))
          (integer->char
               (bitwise-ior
                 (ash xxx 8)
                 (ash yyyy 4)
                 zzzz))))
        ((= #b1110 (ash b0 -4))
         (let* ((b1 (%read-char-u8 ip))
                (b2 (%read-char-u8 ip))
                (zzzz (bitwise-and #b1111 b2))
                (yyyy
                  (bitwise-ior
                    (ash (bitwise-and #b11 b1) 2)
                    (bitwise-and #b11 (ash b2 -4))))
                (xxxx (bitwise-and #b1111 (ash b1 -2)))
                (wwww (bitwise-and #b1111 b0)))
            (integer->char
              (bitwise-ior
                (ash wwww 12)
                (ash xxxx 8)
                (ash yyyy 4)
                zzzz))))
        ((= #b11110 (ash b0 -3))
         (let* ((b1 (%read-char-u8 ip))
                (b2 (%read-char-u8 ip))
                (b3 (%read-char-u8 ip))
                (zzzz (bitwise-and #b1111 b3))
                (xxxx (bitwise-and #b1111 (ash b2 -2)))
                (yyyy
                 (bitwise-ior
                    (ash (bitwise-and #b11 b2) 2)
                    (bitwise-and #b11 (ash b3 -4))))
                (wwww (bitwise-and #b1111 b1))
                (vvvv
                  (bitwise-ior
                    (ash (bitwise-and #b11 b0) 2)
                    (bitwise-and #b11 (ash b1 -4))))
                (u (bitwise-and #b1 (ash b0 -2))))
            (integer->char
              (bitwise-ior
                (ash u 20)
                (ash vvvv 16)
                (ash wwww 12)
                (ash xxxx 8)
                (ash yyyy 4)
                zzzz))
            ))
        (else (error %read-char "unknown-utf-8")))))
%read-char))

(define (make-port p handler buf)
  (vector make-port p 0 handler buf))

(define (port? p)
  (and (vector? p)
       (= (vector-length p) 5)
       (eq? (vector-ref p 0) make-port)))

(define (port-handler p)
  (if (port? p)
      (vector-ref p 3)
      (error "port-handler" "expect-port" p)))

(define (set-port-handler! p handler)
  (if (port? p)
      (vector-set! p 3 handler)
      (error "set-port-handler!" "expect-port" p)))

(define (port-input-buffer p)
  (if (port? p)
      (vector-ref p 4)
      (error "port-input-buffer" "expect-port" p)))

(define (port-output-buffer p)
  (if (port? p)
      (vector-ref p 4)
      (error "port-output-buffer" "expect-port" p)))

(define (port-input-index p)
  (if (port? p)
      (vector-ref p 2)
      (error "port-input-index" "expect-port" p)))

(define (set-port-input-index! p i)
  (if (port? p)
      (vector-set! p 2 i)
      (error "set-port-input-index!" "expect-port" p)))

(define (%port p) (vector-ref p 1))

(define %binary-input-port-handler
  (let ((char-buf '()))
    (define (%binary-input-port-handler msg ip . args)
      (match (cons msg args)
        ((read-u8)
         (let* ((buf (port-input-buffer ip))
                 (sz (foreign-call s_fread buf 0 1 (%port ip))))
           (cond
             ((eof-object? sz) (eof-object))
             ((integer? sz) (bytevector-u8-ref buf 0))
             (else (error "%binary-input-port-handler" "error")))))
        ((unread-char ,ch)
         (set! char-buf (cons ch char-buf)))
        ((peek-char)
         (if (pair? char-buf)
             (car char-buf)
             (let ((ch (read-char ip)))
              (set! char-buf (cons ch char-buf))
              ch)))
        ((read-char)
         (if (pair? char-buf)
             (let ((ch (car char-buf)))
              (set! char-buf (cdr char-buf))
              ch)
             (%read-char ip)))
        ((,msg . ,())
         (error "%binary-input-port-handler" "unknown-message" msg))))
  %binary-input-port-handler))

(define (%binary-output-port-handler msg op . args)
  (match (cons msg args)
    ((write-char ,ch)
     (let ((buf (port-output-buffer op)))
      (foreign-call s_fwrite buf 0 (%utf32->utf8! ch buf 0) (%port op))))
    ((,msg . ,())
     (error "%binary-output-port-handler" "unknown-message" msg))))

(define current-input-port
  (let ((ip (make-port (label stdin) %binary-input-port-handler (make-bytevector 1))))
    (lambda () ip)))

(define current-error-port
  (let ((op (make-port (label stderr) %binary-output-port-handler (make-bytevector 8))))
    (lambda () op)))

(define current-output-port
  (let ((op (make-port (label stdout) %binary-output-port-handler (make-bytevector 8))))
    (lambda () op)))

#;(
  ; R7RS name
  (open-input-file filename)
  (open-binary-input-file filename)
  (open-output-file filename)
  (open-binary-output-file filename)
  ; R6RS name
  (open-file-input-port filename file-options buffer-mode maybe-transcoder)
  (open-file-output-port filename file-options buffer-mode maybe-transcoder)
  #|
  The file-options argument, which may determine various
  aspects of the returned port (see section 8.2.2), defaults to
  the value of (file-options).

  If maybe-transcoder is #f or absent, the port will be
  a binary port and will support the port-position
  and set-port-position! operations. Otherwise the
  port will be a textual port, and whether it supports
  the port-position and set-port-position! operations
  is implementation-dependent (and possibly transcoder dependent).

  (file-options <file-options symbol> ...)
  <file-options symbol> : 'no-create | 'no-fail | 'no-truncate
  
  (buffer-mode <buffer-mode symbol>)
  <buffer-mode symbol> : 'none | 'line | 'none
  |#
)

(define (fopen path mode)
  (foreign-call s_fopen (%string->utf8 path #t) (%string->utf8 mode #t)))

(define (file-exists? path)
  (let ((fp (fopen path "r")))
    (if (= fp 0)
        #f
        (begin (foreign-call s_fclose fp) #t))))

(define open-input-file
  (lambda (path)
    (let ((fp (fopen path "r")))
      (if (= fp 0)
          (error "open-input-file" "file does not exists" path)
          (make-port fp %binary-input-port-handler (make-bytevector 1))))))

(define open-output-file
  (lambda (path)
    (if (file-exists? path)
        (error "open-output-file" "file exists" path)
        (make-port (fopen path "w") %binary-output-port-handler (make-bytevector 8)))))

(define open-binary-input-file
  (lambda (path)
    (if (file-exists? path)
        (make-port (fopen path "rb") %binary-input-port-handler (make-bytevector 1))
        (error "open-binary-input-file" "file does not exists" path))))

(define open-binary-output-file
  (lambda (path)
    (if (file-exists? path)
        (error "open-binary-output-file" "file exists" path)
        (make-port (fopen path "wb") %binary-output-port-handler (make-bytevector 8)))))

(define (close-port fp)
  (if (integer? (%port fp))
      (foreign-call s_fclose (%port fp))))

(define (read-u8 ip)
  (if (not (port? ip))
      (error "read-u8" "expect port" ip)
      ((port-handler ip) 'read-u8 ip)))

(define (%bytevector-input-handler msg ip . args)
  (match (cons msg args)
    ((read-u8)
     (let ((buf (%port ip))
           (i (port-input-index ip)))
      (if (>= i (bytevector-length buf))
          (eof-object)
          (let ((r (bytevector-u8-ref buf i)))
            (set-port-input-index! ip (add1 i))
            r))))
    ((read-char)
     (%read-char ip))
    (,() (error "%bytevector-input-handler" "unknown message" msg))))

(define (open-bytevector-input-port bytevector)
  (make-port bytevector %bytevector-input-handler #f))

(define read-char
  (case-lambda
    (() (read-char (current-input-port)))
    ((ip) ((port-handler ip) 'read-char ip))))

(define (unread-char ch ip)
  ((port-handler ip) 'unread-char ip ch))

(define (peek-char ip)
  ((port-handler ip) 'peek-char ip))

(define write-char
  (case-lambda
    ((ch) (write-char ch (current-output-port)))
    ((ch op) ((port-handler op) 'write-char op ch))))

(define newline
  (case-lambda
    (() (write-char #\newline (current-output-port)))
    ((op) (write-char #\newline op))))

(define (utf8->string b)
  (define ip (open-bytevector-input-port b))
  (let recur ((i 0)
              (ch (read-char ip)))
    (if (eof-object? ch)
        (make-string i)
        (let ((s (recur (add1 i) (read-char ip))))
          (string-set! s i ch)
          s))))

(define (getenv name)
  (if (not (string? name))
      (error "getenv" "expect-string" name)
      (let* ((name (%string->utf8 name #t))
             (_ (collect #f))
             (r (foreign-call s_getenv name)))
        (if r
            (utf8->string r)
            r))))

(define (command-line)
  (define (args->str args)
    (if (pair? args)
        (cons (utf8->string (car args))
              (args->str (cdr args)))
        '()))
  (define args (args->str (label ARGS)))
  (set! command-line (lambda () args))
  args)

(define (get-process-id)
  (foreign-call s_getpid))

(define (dlopen inp)
  (define entry (%string->utf8 (symbol->string (car (read-meta inp))) #t))
  (define inp* (%string->utf8 inp #t))
  (collect)
  (foreign-call s_dlopen inp* entry))

(define (eval e)
  (define tmp (make-tempname "eval" "s"))
  (define obj (make-tempname "eval" "o"))
  (define so (make-tempname "eval" "so"))
  (compile-prog (list 'begin e) obj tmp)
  (system* "~a ~a -shared -o ~a ~a" CC CFLAGS so obj)
  (system* "rm -f ~a" obj)
  (let ((v ((dlopen so))))
    (system* "rm -f ~a" so)
    v))
