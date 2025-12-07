(let ()
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
  (if (pair? bs)
      (cons (car bs) (extend-env* (cdr bs) env))
      env))

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

(define (interp-each es env)
  (if (pair? es)
      (cons (interp (car es) env)
            (interp-each (cdr es) env))
      '()))

(define (interp e env)
  (match e
    (,()
     (guard (or (integer? e) (boolean? e)))
     e)
    (,()
     (guard (symbol? e))
     (apply-env e env))
    ((if ,e1 ,e2 ,e3)
     (if (interp e1 env)
         (interp e2 env)
         (interp e3 env)))
    ((lambda ,params ,e)
     (lambda vs (interp e (extend-env params vs env))))
    ((let ,bs ,e)
     (let iter ((bs bs) (env* env))
      (if (pair? bs)
          (iter (cdr bs) (extend-env (caar bs) (interp (cadar bs) env) env*))
          (interp e env*))))
    ((,e . ,es)
     (apply (interp e env) (interp-each es env)))
    (,() (error "interp" "unmatch" e))))

(define (init-env)
  (extend-env*
    `((<= ,<=)
      (- ,-)
      (+ ,+))
    (make-env)))

(define (writeln e) (write e) (newline))

(define (interp* e)
  (interp e (init-env)))

(define Z
  '(lambda (f)
    ((lambda (x) (f (lambda (v) ((x x) v))))
     (lambda (x) (f (lambda (v) ((x x) v)))))))

(define fib*
  '(lambda (f)
    (lambda (n)
      (if (<= n 1) n (+ (f (- n 1)) (f (- n 2)))))))

(define (fib n)
  (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2)))))

(writeln
  (= (interp*
      `(let ((fib (,Z ,fib*)))
          (fib 25)))
     (fib 25)))
)