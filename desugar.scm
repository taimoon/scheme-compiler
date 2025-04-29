(import
  (only (match) match)
  (only (set) make-set set-union)
  (only (utils)
    make-env extend-env maybe-apply-env apply-env
    read-sexps-from-path symbol->id-symbol generate-label
    system* writeln improper-list? improper->proper))

;;; uniquify
(define primitives
  (let* ((inp (open-input-file "primitives.scm"))
         (primitives (cdr (read inp))))
  (close-port inp)
  primitives))

(define (write-primitive-lib output-path)
  (system* "rm -f ~a" output-path)
  (let ((out (open-output-file output-path)))
    (for-each
      (lambda (p)
        (cond
          ((symbol? p) #t)
          ((and (pair? p) (eq? (car p) 'define))
           (writeln p out))
          (else
           (writeln `(define ,p (prim-call . ,p)) out))))
      primitives)))

(define (immediate? e)
  (or (integer? e) (char? e) (boolean? e)))

(define (linearize-seq es)
  (define (linearize e)
    (if (and (pair? e) (eq? (car e) 'begin))
        (linearize-seq (cdr e))
        (list e)))
  (fold-right (lambda (e acm) (append (linearize e) acm)) '() es))

(define (make-begin es)
  (let ((es (linearize-seq es)))
    (if (and (pair? es) (not (pair? (cdr es))))
        (car es)
        (cons 'begin es))))

(define (make-let inits e)
  (if (pair? inits) `(let ,inits ,e) e))

;; sugars
(define uniquify-program (let ()
(define keywords
  '(begin let if
    let*
    cond not and or
    lambda letrec case-lambda
    set! foreign-call prim-call
    quote quasiquote unquote unquote-splicing
    match import include))

(define (init-uenv)
  (define (make-prim p)
    `(prim ,p))
  (define (make-prim-binding defn)
    (cond
      ((symbol? defn) defn)
      ((and (pair? defn) (eq? (car defn) 'define))
       (cadr defn))
      (else (car defn))))
  (extend-env
    keywords
    (map (lambda _ 'keyword) keywords)
    (let ((primitives (map make-prim-binding primitives)))
      (extend-env primitives (map make-prim primitives) (make-env)))))

(define (keyword? kw env)
  (and (symbol? kw)
       (let ((res (maybe-apply-env kw env)))
        (and (pair? res)
             (eq? (cadr res) 'keyword)))))

(include "lib/match-defmacro.scm")
(define (let*->let e)
  (if (null? (cadr e))
      (cons 'begin (cddr e))
      (let*->let-aux (cadr e) (cddr e))))

(define (let*->let-aux bindings body)
  (if (null? (cdr bindings))
      (list* 'let (list (car bindings)) body)
      (list 'let (list (car bindings)) (let*->let-aux (cdr bindings) body))))

(define (cond->ifs e)
  (if (or (not (pair? e)) (null? (cdr e)))
      (error "cond" "syntax error" e)
      (cond-clauses->ifs (cdr e))))

(define (cond-clauses->ifs clauses)
  (if (pair? clauses)
      (let ((pred (caar clauses))
            (conseq (cons 'begin (cdar clauses)))
            (clauses (cdr clauses)))
        (if (eq? pred 'else)
            (if (pair? clauses)
                (error "cond-clauses->ifs" "misplaced else" clauses)
                conseq)
            (list 'if pred
                      conseq
                      (cond-clauses->ifs clauses))))
      #f))

(define (not->if e)
  (list 'if e #f #t))

(define (or->ifs es)
  (if (pair? es)
      (list 'if (car es) #t (or->ifs (cdr es)))
      #f))

(define (and->ifs es)
  (if (pair? es)
      (list 'if (car es) (and->ifs (cdr es)) #f)
      #t))

(define (list->cons es)
  (if (pair? es)
      (list 'cons (car es) (list->cons (cdr es)))
      '(list)))

(define (list*->cons es)
  (if (not (pair? es))
      (error "list*->cons" "length of list at least 1" es)
      (let recur ((e (car es)) (es (cdr es)))
        (if (pair? es)
            (list 'cons e (recur (car es) (cdr es)))
            e))))

(define (quote->cons q)
  (cond
    ((null? q) ''())
    ((pair? q)
     (list 'cons
           (quote->cons (car q))
           (quote->cons (cdr q))))
    ((symbol? q) (list 'quote q))
    (else q)))

(define (expand-qq form)
  (cond 
    ((not (pair? form)) (list 'quote form))
    ((eq? 'quasiquote (car form)) (expand-qq (cadr form)))
    ((eq? 'unquote (car form)) (cadr form))
    (else (qq-list form))))

(define (tail-unquote? form)
  (and
    (pair? form)
    (pair? (cdr form))
    (null? (cddr form))
    (eq? (car form) 'unquote)))

(define (tail-unquote a)
  (cadr a))

(define (qq-list form)
  (cond 
    ((null? form) ''())
    ((not (pair? form))
     (list 'list (expand-qq form)))
    ((tail-unquote? form)
     (tail-unquote form))
    ((and (pair? (car form))
          (eq? 'unquote-splicing (caar form)))
      (list 'append (cadar form) (qq-list (cdr form))))
    (else (list 'append (list 'list (expand-qq (car form))) (qq-list (cdr form))))))

(define (simplify-qq e)
  (match e
    ('() '(list))
    ((list ,e)
     (list 'list (simplify-qq e)))
    ((append ,e1 ,e2)
     (match (list (simplify-qq e1) (simplify-qq e2))
      ((,e1 (list))
       e1)
      (((list) ,e2)
       e2)
      (((list . ,xs) (list . ,ys))
       (cons 'list (append xs ys)))
      (((list . ,xs) (list* . ,ys))
       `(list* ,@xs ,@ys))
      (((list . ,es) ,e2)
       `(list* ,@es ,e2))
      ((,e1 ,e2) (list 'append e1 e2))))
    (,() e)))

(define (uniquify-each es env add-global!)
  (map (lambda (e) (uniquify e env add-global!)) es))

(define (expand-include-seq es env)
    (define (expand-include e)
      (match e
        ((import . ,()) '())
        ((include ,s)
         (expand-include-seq (linearize-seq (read-sexps-from-path s)) env))
        (,() (list e))))
    (if (keyword? 'include env)
        (apply append (map expand-include es))
        es))

(define (definition->binding d)
  (match d
    ((define (,fn . ,params) . ,body)
      `(,fn (lambda ,params . ,body)))
    ((define ,x ,e)
      `(,x ,e))
    (,() (error "definition->binding" "unmatch" d))))

(define (uniquify-seq es env add-global!)
  (collect-definition (expand-include-seq (linearize-seq es) env)
    (lambda (defns es) 
      (cond
        ((pair? defns)
         (list (uniquify `(letrec ,(map definition->binding defns) . ,es) env add-global!)))
        (else (uniquify-each es env add-global!))))))

(define (uniquify-lambda params body env add-global!)
  (let* ((variadic? (improper-list? params))
         (params (improper->proper params))
         (params* (map generate-label params))
         (env (extend-env params params* env))
         (fn (generate-label "fn")))
   `(lambda ,params* ,variadic? ,fn ,(make-begin (uniquify-seq body env add-global!)))))

(define (uniquify-case-lambda clauses env add-global!)
  `(case-lambda ,(generate-label "fn")
      .
      ,(map (lambda (c) (uniquify-lambda (car c) (cdr c) env add-global!)) clauses)))

(define (make-prim-call pr . es)
  `(call (prim ,pr) . ,es))

(define (make-prim-call* pr es)
  `(call (prim ,pr) . ,es))

(define (uniquify e env add-global!)
(let uniquify ((e e) (env env))
  (match e
    (,()
     (guard (symbol? e))
     (let ((res (maybe-apply-env e env)))
      (if res
          (cadr res)
          ;;; WORKAROUND
          (let ((lbl (symbol->id-symbol e)))
            (add-global! #f lbl)
            (make-prim-call 'car `(global ,lbl))))))
    (,()
     (guard (string? e))
     e)
    (,()
     (guard (vector? e))
     (uniquify `(vector . ,(vector->list e)) env))
    (,()
     (guard (immediate? e))
     e)
    ((,e . ,es)
     (guard (not (keyword? e env)))
     (if (improper-list? es)
         ;;; non-standard: (fn x y . zs) == (apply fn (list* x y zs))
         (uniquify `(apply ,e (list* . ,(improper->proper es))) env)
         (match (uniquify e env)
            ((prim add1)
             (make-prim-call '+ (uniquify (car es) env) 1))
            ((prim sub1)
             (make-prim-call '- (uniquify (car es) env) 1))
            ((prim list)
             (if (pair? es)
                  (uniquify (list->cons es) env)
                  '(quote ())))
            ((prim list*)
             (uniquify (list*->cons es) env))
            ((prim make-string)
             (match es
              ((,sz)
               (make-prim-call 'make-string (uniquify sz env) #\nul))
              ((,sz ,ch)
               (make-prim-call 'make-string (uniquify sz env) (uniquify ch env)))
              (,es
               (error "uniquify" "make-string:unknown-args" es))))
            ((prim ,p)
             (make-prim-call* p (uniquify-each es env add-global!)))
            (,op
             `(call #f ,op . ,(uniquify-each es env add-global!))))))
    ((prim-call ,p . ,es)
     ;;; this is for the primitive library
     (match p
      (add1
       (make-prim-call '+ (uniquify (car es) env) 1))
      (sub1
       (make-prim-call '- (uniquify (car es) env) 1))
      (,()
       (make-prim-call* p (uniquify-each es env add-global!)))))
    ((quote ,e*)
     (if (or (null? e*) (symbol? e*))
         e
         `(quote ,(uniquify (quote->cons e*) env))))
    ((quasiquote ,x)
     (uniquify (simplify-qq (expand-qq x)) env))
    ((begin . ,es)
      (make-begin (uniquify-seq es env add-global!)))
    ((if ,pred ,conseq)
     (uniquify `(if ,pred ,conseq #f) env))
    ((if ,pred ,conseq ,altern)
     (match (uniquify pred env) 
      (#f
       (uniquify altern env))
      (#t
       (uniquify conseq env))
      (,pred
       `(if ,pred
            ,(uniquify conseq env)
            ,(uniquify altern env)))))
    ((not ,e)
     (uniquify (not->if e) env))
    ((and . ,es)
     (uniquify (and->ifs es) env))
    ((or . ,es)
     (uniquify (or->ifs es) env))
    ((cond . ,clauses)
     (uniquify (cond-clauses->ifs clauses) env))
    ((let ,fn ,inits . ,es)
     (guard (symbol? fn))
     (let ((params (map car inits))
           (args (map cadr inits)))
      (uniquify
        `(letrec ((,fn (lambda ,params . ,es)))
          (,fn . ,args))
        env)))
    ((let () . ,es)
     (uniquify `(begin . ,es) env))
    ((let ,bs . ,es)
     (let* ((xs (map car bs))
            (xs* (map generate-label (map car bs)))
            (inner-env (extend-env xs xs* env))
            (inits (uniquify-each (map cadr bs) env add-global!)))
      `(let ,(map list xs* inits)
            ,(make-begin (uniquify-seq es inner-env add-global!)))))
    ((let* ,bs . ,es)
     (uniquify (let*->let-aux bs es) env))
    ((letrec () . ,es)
     (uniquify `(begin . ,es) env))
    ((letrec ,bs . ,es)
     (let* ((xs (map car bs))
            (xs* (map generate-label (map car bs)))
            (env (extend-env xs xs* env))
            (inits (map (lambda (b) (uniquify (cadr b) env)) bs))
            (es (uniquify-seq es env add-global!)))
        `(letrec ,(map list xs* inits) ,(make-begin es))))
    ((lambda ,params . ,body)
     (uniquify-lambda params body env add-global!))
    ((case-lambda . ,clauses)
     (uniquify-case-lambda clauses env add-global!))
    ((set! ,x ,e)
     (define e* (uniquify e env))
     (match (maybe-apply-env x env)
      ((,() ,x)
       (guard (symbol? x))
       `(set! ,x ,e*))
      ((,() (call (prim car) (global ,x)))
       (guard (symbol? x))
       ;;; WORKAROUND
       (make-prim-call 'set-car! `(global ,x) e*))
      (#f
       ;;; WORKAROUND
       (let ((lbl (symbol->id-symbol x)))
          (add-global! #t lbl)
          (make-prim-call 'set-car! `(global ,lbl) e*)))
      (,res (error "uniquify" "unmatch" res))))
    ((foreign-call ,fn . ,es)
     `(foreign-call ,fn . ,(uniquify-each es env add-global!)))
    ((match . ,())
     (uniquify (compile-match e) env))
    (,() (error "uniquify" "unmatch" e)))))

(define (definition-variable defn)
  (match defn
    ((define (,fn . ,()) . ,()) fn)
    ((define ,fn ,()) fn)
    (,() (error "definition-variable" "unmatch" defn))))

(define (definition-value defn)
  (match defn
    ((define (,() . ,params) . ,body)
     `(lambda ,params  . ,body))
    ((define ,() ,e)
     e)
    (,() (error "definition-value" "unmatch" defn))))

(define (collect-definition es cont)
  (if (pair? es)
      (match (car es)
        ((define . ,())
          (collect-definition (cdr es)
           (lambda (defns es*)
             (cont (cons (car es) defns) es*))))
        (,e (collect-definition (cdr es)
              (lambda (defns es) (cont defns (cons e es))))))
      (cont '() '())))

(define (uniquify-program prog)
  (match prog
    ((begin . ,es)
     (collect-definition (expand-include-seq (linearize-seq es) (init-uenv))
      (lambda (defns es)
        (let* ((vars (map definition-variable defns))
               (vals (map definition-value defns))
               (vars* (map symbol->id-symbol vars))
               (var->glb (lambda (v) (make-prim-call 'car `(global ,v))))
               (env (extend-env vars (map var->glb vars*) (init-uenv)))
               (unbounds (make-set))
               (bounds (make-set))
               (add-global!
                (lambda (ref? lbl)
                  (if ref?
                      (set! bounds (set-union bounds (make-set lbl)))
                      (set! unbounds (set-union unbounds (make-set lbl))))))
               (vals* (uniquify-each vals env add-global!))
               (es (uniquify-each es env add-global!)))
          `(program (labels) (data ,@vars* ,@bounds) (unbounds . ,unbounds)
              ,(make-begin (append (map (lambda (v w) (make-prim-call 'set-car! `(global ,v) w)) vars* vals*) es)))))))
    (,() (error "uniquify-program" "unmatch" prog))))

uniquify-program
))
