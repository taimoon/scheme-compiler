(define (writeln obj . args)
  (let ((op (if (pair? args) (car args) (current-output-port))))
    (write obj op)
    (newline op)))

(define (make-env) '())

(define (extend-env xs vs env)
  (cond
    ((symbol? xs)
     (cons (list xs vs) env))
    ((pair? xs)
     (if (pair? vs)
         (cons (list (car xs) (car vs))
               (extend-env (cdr xs) (cdr vs) env))
         (error "extend-env" "bad value list" vs)))
    (else env)))

(define (extend-env* bs env)
  (fold-right cons env bs))

(define (maybe-apply-env x env)
  (cond
    ((null? env) #f)
    ((not (and (pair? env) (pair? (car env))))
     (error "maybe-apply-env" "improperly formed env"))
    ((equal? x (caar env))
     (car env))
    (else (maybe-apply-env x (cdr env)))))

(define (apply-env x env)
  (let ((res (maybe-apply-env x env)))
    (if res
        (cadr res)
        (error "apply-env" "unbound" x (map car env)))))

(define (partition-k pred xs k)
  (let recur ((xs xs) (k k))
    (cond
      ((not (pair? xs))
       (k '() '()))
      ((pred (car xs))
       (recur (cdr xs)
              (lambda (ts fs) (k (cons (car xs) ts)
                                 fs))))
      (else
       (recur (cdr xs)
              (lambda (ts fs) (k ts
                                 (cons (car xs) fs))))))))

(define (system* cmd . args)
  (let ((r (system (apply format (cons cmd args)))))
    (if (eq? r 0)
        r
        (exit r))))

(define (read-sexps-from-path filename)
  (let ((inp (open-input-file filename)))
    (let recur ((obj (read inp)))
      (if (eof-object? obj)
          '()
          (cons obj (recur (read inp)))))))

(define (align-to-multiple alignment offset)
  (bitwise-and (+ offset (- alignment 1)) (- alignment)))

(define (make-lcg multiplier increment modulus x)
  (lambda ()
    (set! x (mod (+ increment (* multiplier x)) modulus))
    x))

(define (random-bool)
  (= (mod (rand) 2) (mod (rand) 2)))

(define (random-string len)
  (let loop ((s (make-string len #\nul))
             (i (- len 1)))
    (if (< i 0)
        s
        (begin
          (string-set! s i (integer->char
                            (+ (if (random-bool) 97 65)
                               (mod (rand) 26))))
          (loop s (- i 1))))))

(define rand
  (make-lcg 75 74 (+ (ash 2 16) 1) (get-process-id)))

(define string->id-string
  (let ()
    (define extended-named-alphabets (map (lambda (b) (list (car b) (string-append "_" (cadr b) "_")))
  '(
    (#\! "bang")
    (#\$ "dollar")
    (#\% "cent")
    (#\& "ampersand")
    (#\* "star")
    (#\+ "plus")
    (#\- "dash")
    (#\. "dot")
    (#\/ "slash")
    (#\: "colon")
    (#\< "lt")
    (#\= "eq")
    (#\> "gt")
    (#\? "query")
    (#\@ "at")
    (#\^ "hat")
    (#\~ "tilde")
    (#\_ "ul")
    )))
    (define (numeric-char? ch)
      (and (char<=? #\0 ch) (char<=? ch #\9)))
    (define (valid-id-char? ch)
      (or (and (char<=? #\a ch) (char<=? ch #\z))
          (and (char<=? #\A ch) (char<=? ch #\Z))
          (numeric-char? ch)))
    (define (char->valid-id-str ch)
      (if (valid-id-char? ch)
          (string ch)
          (let ((res (assoc ch extended-named-alphabets)))
            (if res
                (cadr res)
                "_"))))
  (lambda (str) (apply string-append (cons "_" (map char->valid-id-str (string->list str)))))))

(define (symbol->id-symbol sym)
  (string->symbol (string->id-string (symbol->string sym))))

(define generate-label
  (let ((counter 0)
        (rdm-str (random-string 4)))
    (lambda (prefix)
      (set! counter (add1 counter))
      (string->symbol (format "~a_~a_~a" prefix rdm-str counter)))))

(define (string-index s ch/pred)
  (define pred
    (cond
      ((char? ch/pred) (lambda (ch) (char=? ch ch/pred)))
      ((procedure? ch/pred) ch/pred)
      (else (error 'string-index
                   "uknown argument ch/pred"
                   ch/pred))))
  (let loop ((i 0))
    (cond
      ((>= i (string-length s)) #f)
      ((pred (string-ref s i)) i)
      (else (loop (add1 i))))))

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
(define (path-last s)
  (let ((maybe-idx (string-index-right s #\/)))
    (if maybe-idx
        (substring s (add1 maybe-idx) (string-length s))
        s)))

(define (path-first s)
  (let ((maybe-idx (string-index-right s #\/)))
    (if maybe-idx
        (substring s 0 maybe-idx)
        "")))

(define (path-filename s)
  (path-last s))

(define (path-extension s)
  (let* ((s (path-last s))
         (maybe-idx (string-index-right s #\.)))
    (if maybe-idx
        (substring s (add1 maybe-idx) (string-length s))
        "")))

(define (path-fileroot s)
  (let* ((s (path-last s))
         (maybe-idx (string-index-right s #\.)))
    (if maybe-idx
        (substring s 0 maybe-idx)
        s)))

;;; for perf measurement 
(define (count-cons e)
  (if (pair? e)
      (add1 (+ (count-cons (car e)) (count-cons (cdr e))))
      0))

(define (improper-list? xs)
  (cond
    ((null? xs) #f)
    ((pair? xs) (improper-list? (cdr xs)))
    (else #t)))

(define (improper->proper xs)
  (cond
    ((null? xs) xs)
    ((symbol? xs) (list xs))
    ((pair? xs) (cons (car xs) (improper->proper (cdr xs))))
    (else (error "improper->proper" "unknown object" xs))))

(define (rpad s pad-ch pad-sz)
  (if (< (string-length s) pad-sz)
      (string-append s (make-string (- pad-sz (string-length s)) pad-ch))
      s))

(define (measure-pass $who)
  (lambda (e) (format (current-error-port) "~a: ~a\n" (rpad (format "~a" $who) #\space 32) (count-cons e)) e))
