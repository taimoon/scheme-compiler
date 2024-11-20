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

(define (system* cmd . args)
  (system (apply format (cons cmd args))))

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
  (let loop ((s (make-string len))
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

(define (string->id-string str)
  (define (integer->alphabet-str n)
    (string #\_ (integer->char (+ n (char->integer #\a))) #\_))
  (define extended-alphabets
    (let ((xs (list #\! #\$ #\% #\& #\* #\+ #\- #\. #\/ #\: #\< #\= #\> #\? #\@ #\^ #\_ #\~)))
      (map cons
           xs
           (map integer->alphabet-str (iota (length xs))))))
  (define (numeric-char? ch)
    (and (char<=? #\0 ch) (char<=? ch #\9)))
  (define (valid-id-char? ch)
    (or (and (char<=? #\a ch) (char<=? ch #\z))
        (and (char<=? #\A ch) (char<=? ch #\Z))
        (char=? #\_ ch)
        (numeric-char? ch)))
  (define underscore-str (string #\_))
  (define (char->valid-id-str ch)
    (if (valid-id-char? ch)
        (string ch)
        (let ((res (assoc ch extended-alphabets)))
          (if res
              (cdr res)
              underscore-str))))
  (apply string-append (cons underscore-str (map char->valid-id-str (string->list str)))))

(define (symbol->id-symbol sym)
  (string->symbol (string->id-string (symbol->string sym))))

(define generate-label
  (let ((counter 0))
    (lambda (prefix)
      (set! counter (add1 counter))
      (string->symbol (format "~a_~a_~a" prefix (random-string 8) counter)))))

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
        "")))