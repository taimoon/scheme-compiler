(define (align-to-multiple alignment offset)
  (bitwise-and (+ offset (- alignment 1)) (- alignment)))

(define (make-lcg multiplier increment modulus x)
  (lambda ()
    (set! x (mod (+ increment (* multiplier x)) modulus))
    x))

(define rand
  (make-lcg 75 74 (+ (ash 2 16) 1) (get-process-id)))

(define (random-string len)
  (let loop ((s (make-string len #\nul))
             (i (- len 1)))
    (if (< i 0)
        s
        (begin
          (string-set! s i (integer->char (+ 97 (mod (rand) 26))))
          (loop s (- i 1))))))

;;; gensym* is like gensym but C-identifier syntax
(define gensym*
  (let ((count 0)
        (rdm-str (random-string 8)))
    (lambda (prefix)
      (set! count (+ count 1))
      (string->symbol (format "~a_~a_~a" prefix rdm-str count)))))

(define (make-env) '())

(define (extend-env xs vs env)
  (cond
    ((null? xs) env)
    ((symbol? xs)
     (cons (list xs vs) env))
    ((pair? xs)
     (if (pair? vs)
         (cons (list (car xs) (car vs))
               (extend-env (cdr xs) (cdr vs) env))
         (error "extend-env" "bad value list" vs)))
    (else
     (error "extend-env" "bad symbol list" xs))))

(define (extend-env* bs env)
  (fold-right cons env bs))

(define (maybe-apply-env x env)
  (cond
    ((null? env) #f)
    ((not (pair? env))
     (error "maybe-apply-env" "ill-formed" env))
    ((not (pair? (car env)))
     (error "maybe-apply-env" "improperly formed env"))
    ((equal? x (caar env))
     (car env))
    (else (maybe-apply-env x (cdr env)))))

(define (apply-env x env)
  (let ((res (maybe-apply-env x env)))
    (if res
        (cadr res)
        (error "apply-env" "unbound" x (map car env)))))

(define (apply-env* x env $who)
  (let ((res (maybe-apply-env x env)))
    (if res
        (cadr res)
        (error "apply-env" $who "unbound" x (map car env)))))

(define (improper-list? xs)
  (if (pair? xs)
      (improper-list? (cdr xs))
      (not (null? xs))))

(define (improper->proper xs)
  (cond
    ((null? xs) '())
    ((pair? xs) (cons (car xs) (improper->proper (cdr xs))))
    (else (list xs))))

;;; SRFI-13
(define (string-index-right s ch/pred)
  (define pred
    (cond
      ((char? ch/pred) (lambda (ch) (char=? ch ch/pred)))
      ((procedure? ch/pred) ch/pred)
      (else (error 'string-index-right
                   "uknown argument ch/pred"
                   ch/pred))))
  (let loop ((i (sub1 (string-length s))))
    (cond
      ((< i 0) #f)
      ((pred (string-ref s i)) i)
      (else (loop (sub1 i))))))


;;; filesystem path
#|
  https://en.cppreference.com/w/cpp/filesystem/path#Decomposition
  https://www.boost.org/doc/libs/1_86_0/libs/filesystem/doc/tutorial.html#Class%20path-iterators-etc
  
  c:\foo\bar\baa.txt
  /foo/bar/baa.txt

  Part            Windows          Posix
--------------  ---------------  ---------------
Root name       c:               <empty>
Root directory  \                /
Root path       c:\              /
Relative path   foo\bar\baa.txt  foo/bar/baa.txt
Parent path     c:\foo\bar       /foo/bar
Filename        baa.txt          baa.txt
Stem            baa              baa
Extension       .txt             .txt
|#

(define (path-parent s)
  (let ((maybe-idx (string-index-right s #\/)))
    (if maybe-idx
        (substring s 0 maybe-idx)
        "")))

(define (path-filename s)
  (let ((maybe-idx (string-index-right s #\/)))
    (if maybe-idx
        (substring s (add1 maybe-idx) (string-length s))
        s)))

(define (path-extension s)
  (let* ((s (path-filename s))
         (maybe-idx (string-index-right s #\.)))
    (if maybe-idx
        (substring s (add1 maybe-idx) (string-length s))
        "")))

(define (path-filestem s)
  (let* ((s (path-filename s))
         (maybe-idx (string-index-right s #\.)))
    (if maybe-idx
        (substring s 0 maybe-idx)
        s)))

(define (flatmap f xs . xss)
  (if (null? xss)
      (let recur ((xs xs))
        (if (pair? xs)
            (append (f (car xs)) (recur (cdr xs)))
            '()))
      (let recur ((xs xs) (xss xss))
        (cond
          ((pair? xs)
           (append (apply f (car xs) (map car xss))
                    (recur (cdr xs) (map cdr xss))))
          ((not (and (null? xs) (andmap null? xss)))
           (error "flatmap" "expect-lists" xs xss))
          (else '())))))

(define (zip xs ys)
  (if (and (pair? xs) (pair? ys))
      (cons (list (car xs) (car ys))
            (zip (cdr xs) (cdr ys)))
      '()))

(define (applicate f)
  (lambda (vs) (apply f vs)))

(define make-tempname
  (let ((rdm-str (random-string 8))
        (count 0))
    (lambda (filestem ext)
      (define filename
        (string-append
          (format "/tmp/scm-build-~a-~a-~a" rdm-str count filestem)
            (if (equal? ext "") ext (string-append "." ext))))
      (set! count (add1 count))
      (if (file-exists? filename)
          (make-tempname filestem ext)
          filename))))

(define (system* cmd . args)
  (let ((r (system (apply format (cons cmd args)))))
    (if (eq? r 0)
        r
        (exit r))))

(define (maybe-getenv var default)
  (let ((val (getenv var)))
    (if val
        val
        default)))

(define (read-sexps-from-path path)
  (let ((ip (open-input-file path)))
    (let recur ((obj (read ip)))
      (if (eof-object? obj)
          (begin
            (close-port ip)
            '())
          (cons obj (recur (read ip)))))))
