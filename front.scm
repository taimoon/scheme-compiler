(import
  (only (match) match)
  (only (set) list->set make-set set-union set-diff set-intersection)
  (only (utils)
    gensym*
    make-env extend-env extend-env* maybe-apply-env apply-env
    improper-list? improper->proper
    read-sexps-from-path))

(include "primitive.scm")

(define (generate-primitive-top-bindings)
  (define (make-init e)
    (match e
      ((,nm ,() ,() #f) #f)
      ((,name ,() ,() (,name* . ,params))
       (guard (eq? name* name))
       `(%symbol-value-set! ',name (lambda ,params (,name . ,params))))
      ((,name ,() ,() ,e)
       `(%symbol-value-set! ',name ,e))
      (,() (error "generate-primitive-top-bindings" "unmatch" e))))
  (cons 'begin (filter (lambda (x) x) (map make-init primitive-meta-table))))

(define get-prim-impl
  (let ((prim-impl (map (lambda (pr) (list (car pr) (caddr pr))) primitive-meta-table)))
    (lambda (pr) (apply-env pr prim-impl))))

(define preprocess (let ()

(define (make-primcall pr . es)
  `(call (prim ,pr) . ,es))

(define (make-prim pr)
  (list 'prim pr))

(define variable? symbol?)

(define (immediate? e)
  (match e
    ((quote ,q)
     (or (null? q) (eof-object? q) (eq? (void) q)))
    (,() (or (integer? e) (char? e) (boolean? e)))))

(define (self-evaluating? e)
  (match e
    ;;; local-label
    ((label* ,()) #t)
    ;;; global-label
    ((label ,()) #t)
    ((prim ,()) #t)
    (,() (or (variable? e) (immediate? e)))))

(include "lib/hashtables.scm")

(include "lib/graph.scm")

(define (make-let bs e)
  (if (pair? bs)
      `(let ,bs ,e)
      e))

(define (make-letrec bs e)
  (if (pair? bs)
      `(letrec ,bs ,e)
      e))

(define (make-begin es)
  (let ((es (linearize-seq es)))
    (if (pair? (cdr es))
        (cons 'begin es)
        (car es))))

(define keywords
  '(quote let letrec set! lambda case-lambda begin let* if cond and or
    define foreign-call label include quasiquote
    let-values let*-values
    match import
    ))

(define (make-keyword kw)
  (list make-keyword))

(define (keyword? e env)
  (let ((kw (if (symbol? e) (maybe-apply-env e env) #f)))
    (if kw
        (and (pair? (cadr kw))
             (eq? (car (cadr kw)) make-keyword))
        #f)))

(define init-env
  (let* ((env (make-env))
         (env (extend-env* (map (lambda (pr) (list pr (make-prim pr))) (map car primitive-meta-table)) env))
         (env (extend-env* (map (lambda (kw) (list kw (make-keyword kw))) keywords) env)))
    (lambda () env)))

(define (let*->let bindings body)
  (if (null? bindings)
      (cons 'begin body)
      (if (null? (cdr bindings))
          (list* 'let (list (car bindings)) body)
          (list 'let (list (car bindings)) (let*->let (cdr bindings) body)))))

(define (or->ifs es)
  (if (pair? es)
      (list 'if (car es) #t (or->ifs (cdr es)))
      #f))

(define (and->ifs es)
  (if (pair? es)
      (list 'if (car es) (and->ifs (cdr es)) #f)
      #t))

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
            (if (pair? clauses)
                (list 'if pred
                          conseq
                          (cond-clauses->ifs clauses))
                (list 'if pred conseq))))
      #f))

(define (quote->cons q)
  (cond
    ((or (integer? q) (boolean? q) (char? q) (string? q))
     q)
    ((null? q) '(quote ()))
    ((symbol? q) `(quote ,q))
    ((vector? q)
     `(vector . ,(map quote->cons (vector->list q))))
    ((pair? q)
     `(cons ,(quote->cons (car q))
            ,(quote->cons (cdr q))))
    (else (error "quote->cons" "unknown" q))))

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

(define (simplify-qq-more e)
  (match e
    ((list . ,es)
     (fold-right (lambda (e p) `(cons ,e ,p)) ''() (map simplify-qq-more es)))
    ((list* ,e . ,es)
     (let recur ((e (simplify-qq-more e)) (es (map simplify-qq-more es)))
      (if (pair? es)
          `(cons ,e ,(recur (car es) (cdr es)))
          e)))
    ((append ,e1 ,e2)
     `(append ,(simplify-qq-more e1) ,(simplify-qq-more e2)))
    (,() e)))

(define (expand-qq form)
  (define (expand-qq form)
    (cond 
      ((not (pair? form)) (list 'quote form))
      ((eq? 'quasiquote (car form)) (expand-qq (cadr form)))
      ((eq? 'unquote (car form)) (cadr form))
      (else (qq-list form))))
  (simplify-qq-more (simplify-qq (expand-qq form))))

(include "lib/match-defmacro.scm")

(define (linearize-seq es)
  (define (linearize e)
    (if (and (pair? e) (eq? (car e) 'begin))
        (linearize-seq (cdr e))
        (list e)))
  (fold-right (lambda (e acm) (append (linearize e) acm)) '() es))

(define (uniquify-each es env)
  (map (lambda (e) (uniquify e env)) es))

(define (lift-internal-definition es cont)
  (let iter ((es es) (cont cont))
    (if (pair? es)
        (match (car es)
          ((include ,path)
           (if (not (string? path))
               (error "lift-internal-definition" "include-only-string-path" path)
               (iter (append (linearize-seq (read-sexps-from-path path)) (cdr es)) cont)))
          ((define (,fn . ,params) . ,body)
           (define b `(,fn (lambda ,params . ,body)))
           (iter (cdr es) (lambda (bs es*) (cont (cons b bs) es*))))
          ((define ,var ,val)
           (define b `(,var ,val))
           (iter (cdr es) (lambda (bs es*) (cont (cons b bs) es*))))
          ((begin . ,es*)
           (iter (append es* (cdr es)) cont))
          (,()
           (iter (cdr es) (lambda (bs es*) (cont bs (cons (car es) es*))))))
        (cont '() '()))))

(define (uniquify-seq es env)
  (match (lift-internal-definition es (lambda (bs es) (make-letrec bs (make-begin es))))
    ((begin . ,es)
     (uniquify-each es env))
    (,e
     (list (uniquify e env)))))

(define (uniquify e env)
  (match e
    (,()
     (guard (or (symbol? e) (boolean? e) (char? e) (integer? e) (string? e)))
     (if (symbol? e)
         (let ((r (maybe-apply-env e env)))
          (if r
              (cadr r)
              (uniquify `(%symbol-value ',e) env)))
         e))
    ((,e1 . ,es)
     (guard (not (keyword? e1 env)))
     `(call ,(uniquify e1 env) . ,(uniquify-each es env)))
    ((quote ,q)
     (cond
      ((null? q) '(quote ()))
      ((symbol? q) e)
      ((or (integer? q) (boolean? q) (char? q) (string? q)) q)
      (else `(quote ,(uniquify (quote->cons q) env)))))
    ((label ,s)
     (guard (symbol? s))
     `(label ,s))
    ((begin . ,es)
     (make-begin (uniquify-seq es env)))
    ((let ,fn ,inits . ,body)
     (guard (symbol? fn))
     (let ((params (map car inits))
           (es (map cadr inits)))
      (uniquify
        `(letrec ((,fn (lambda ,params . ,body)))
          (,fn . ,es))
        env)))
    ((let ,bs . ,body)
     (let* ((vars (map car bs))
            (inits (map cadr bs))
            (vars* (map gensym* vars))
            (inits* (uniquify-each inits env)))
      (make-let (map list vars* inits*)
        (make-begin (uniquify-seq body (extend-env vars vars* env))))))
    ((letrec ,bs . ,body)
     (let* ((vars (map car bs))
            (inits (map cadr bs))
            (vars* (map gensym* vars))
            (env (extend-env vars vars* env))
            (inits* (uniquify-each inits env)))
      (make-letrec (map list vars* inits*)
        (make-begin (uniquify-seq body (extend-env vars vars* env))))))
    ((let-values ((,params ,e)) . ,es)
     (uniquify
      `(call-with-values
        (lambda () ,e)
        (lambda ,params . ,es))
      env))
    ((let*-values ,bs . ,es)
     (uniquify
      (fold-right
        (lambda (b e) `(let-values (,b) ,e))
        (make-begin es)
        bs)
      env))
    ((set! ,x ,e)
     (let ((r (maybe-apply-env x env))
           (e (uniquify e env)))
      (if r
          `(set! ,(cadr r) ,e)
          (make-primcall '%symbol-value-set! `(quote ,x) e))))
    ((if ,e1 ,e2)
     `(if ,(uniquify e1 env)
          ,(uniquify e2 env)
          (call (prim void))))
    ((if ,e1 ,e2 ,e3)
     `(if ,(uniquify e1 env)
          ,(uniquify e2 env)
          ,(uniquify e3 env)))
    ((lambda ,params . ,body)
     (let* ((variadic? (improper-list? params))
            (params (improper->proper params))
            (params* (map gensym* params)))
      `(lambda ,params* ,variadic? ,(make-begin (uniquify-seq body (extend-env params params* env))))))
    ((case-lambda ,clause)
     (uniquify `(lambda . ,clause) env))
    ((case-lambda . ,clauses)
     `(case-lambda . ,(uniquify-each (map (lambda (c) (cons 'lambda c)) clauses) env)))
    ((foreign-call ,fn . ,es)
     (guard (symbol? fn))
     `(foreign-call (label ,fn) . ,(uniquify-each es env)))
    ((let* ,bs . ,body)
     (uniquify (let*->let bs body) env))
    ((and . ,es)
     (uniquify (and->ifs es) env))
    ((or . ,es)
     (uniquify (or->ifs es) env))
    ((cond . ,es)
     (uniquify (cond->ifs e) env))
    ((quasiquote ,qq)
     (uniquify (expand-qq qq) env))
    ((match . ,())
     (uniquify (compile-match e) env))
    ((import . ,()) 0)
    (,() (error "uniquify" "unmatch" e))))

(define (uniquify-prog prog)
  (match (lift-internal-definition (cdr prog) list)
    ((,bs ,es)
     (make-begin
      (uniquify-seq (append (map (lambda (b) `(%symbol-value-set! ',(car b) ,(cadr b))) bs) es) (init-env))))
    ((() ,es)
     (make-begin (uniquify-seq es (init-env))))
    (,() (error "uniquify-prog" "unmatch" prog))))

;;; quotify-immediate
(define (quotify-immediate e)
  (match e
    (,()
     (guard (or (variable? e) (boolean? e) (char? e) (integer? e) (string? e)))
     e)
    ((prim ,()) e)
    ((label ,()) e)
    ((quote . ,()) e)
    ((call (prim void))
     `(quote ,(void)))
    ((call (prim eof-object))
     `(quote ,(eof-object)))
    ((,form . ,es)
     (guard (memq form '(call foreign-call begin if case-lambda set!)))
     `(,form . ,(map quotify-immediate es)))
    ((,form ,bs ,e)
     (guard (memq form '(let letrec)))
     `(,form ,(map (lambda (b) (list (car b) (quotify-immediate (cadr b)))) bs)
        ,(quotify-immediate e)))
    ((lambda ,params ,variadic? ,e)
     `(lambda ,params ,variadic? ,(quotify-immediate e)))
    (,() (error "quotify-immediate" "unmatch" e))))

;;; purify-letrec
(define (simple-primitive? pr)
  (let ((r (maybe-apply-env pr primitive-meta-table)))
    (if r
        (cadr r)
        (error "simple-primitive?" "unknown" pr))))

(define (letrec-simple-exp? e vars)
  (let letrec-simple-exp? ((e e))
    (match e
      (,()
       (guard (or (string? e) (self-evaluating? e)))
       (not (and (variable? e) (memq e vars))))
      ((quote . ,()) #t)
      ((call (prim ,pr) . ,es)
       (and (simple-primitive? pr) (andmap letrec-simple-exp? es)))
      ((,form . ,es)
       (guard (memq form '(call foreign-call lambda case-lambda letrec let)))
       #f)
      ((,form . ,es)
       (guard (memq form '(if begin)))
       (andmap letrec-simple-exp? es))
      (,() (error "letrec-simple-exp?" "unmatch" e)))))

(define (comb-2-each* f xs)
  (let iter ((xs xs))
    (if (pair? xs)
        (begin
          (for-each (lambda (x) (f (car xs) x)) xs)
          (iter (cdr xs))))))

(define (letrec->fix vars inits body set!-vars)
  ;;; unreferenced
  ;;; simple
  ;;; lambda
  ;;; complex
  (define (assigned? x) (if (memq x set!-vars) #t #f))
  (define unreferenced?
    (let ((fvs
           (fold-right set-union (free-vars body) (map free-vars inits))))
      (lambda (x) (not (memq x fvs)))))
  (define (complex-expr? x e)
    (or (assigned? x)
        (not (or (unreferenced? x)
                 (letrec-simple-exp? e vars)
                 (lambda-expr? e)))))
  (define (make-dep-node-attr odr rhs fvs)
    (vector odr rhs fvs))
  (define order-of (lambda (v) (vector-ref v 0)))
  (define init-of (lambda (v) (vector-ref v 1)))
  (define fvs-of (lambda (v) (vector-ref v 2)))
  (define dep-g
    (let ((dep-g (make-graph)))
      (for-each
        (lambda (i v init)
          (graph-add-node! dep-g v)
          (graph-node-set-attr! dep-g v (make-dep-node-attr i init (free-vars init))))
        (iota (length vars))
        vars
        inits)
      (comb-2-each*
        (lambda (v w)
          (let ((v-prop (graph-node-attr dep-g v))
                (w-prop (graph-node-attr dep-g w)))
            (if (memq v (fvs-of w-prop))
                (graph-add-edge! dep-g w v))
            (if (memq w (fvs-of v-prop))
                (graph-add-edge! dep-g v w))
            (if (and (> (order-of w-prop) (order-of v-prop))
                     (complex-expr? v (init-of v-prop))
                     (complex-expr? w (init-of w-prop)))
                (graph-add-edge! dep-g w v))))
        vars)
    dep-g))
  (define (transform-single-binding v rest)
    (let* ((node-attr (graph-node-attr dep-g v))
           (init (init-of node-attr))
           (fvs (fvs-of node-attr)))
      (cond
        ((and (not (assigned? v))
              (lambda-expr? init))
         ((if (memq v fvs) make-letrec make-let) (list (list v init)) rest))
        ((not (memq v fvs))
         (make-let (list (list v init)) rest))
        (else
         (make-let (list (list v #f))
          (make-begin
            (list
              `(set! ,v ,init)
              rest)))))))
  (define (transform-bindings vs rest)
    (define (pure-lambda? v)
      (and (not (assigned? v))
           (lambda-expr? (init-of (graph-node-attr dep-g v)))))
    (define lambdas (filter pure-lambda? vs))
    (define complexs (filter (lambda (v) (not (pure-lambda? v))) vs))
    (define complexs<
          (lambda (c0 c1) (< (order-of (graph-node-attr dep-g c0)) (order-of (graph-node-attr dep-g c1)))))
    (let ((complexs (list-sort complexs< complexs)))
      (make-let (map (lambda (c) (list c #f)) complexs)
        (make-letrec
          (map (lambda (v) (list v (init-of (graph-node-attr dep-g v))))
            lambdas)
          (make-begin
            (append
              (map (lambda (c) `(set! ,c ,(init-of (graph-node-attr dep-g c)))) complexs)
              (list rest)))))))
  (define (SCC->fix SCC)
    (cond
      ((not (pair? SCC)) body)
      ((not (pair? (cdar SCC)))
       (transform-single-binding (caar SCC) (SCC->fix (cdr SCC))))
      (else
       (transform-bindings (car SCC) (SCC->fix (cdr SCC))))))
  (SCC->fix (kosaraju-components dep-g)))

(define (purify-letrec-each es set!-vars)
  (map (lambda (e) (purify-letrec e set!-vars)) es))

(define (purify-letrec e set!-vars)
  (match e
    (,()
     (guard (or (self-evaluating? e) (string? e)))
     e)
    ((quote . ,()) e)
    ((,form . ,es)
     (guard (memq form '(call foreign-call if begin case-lambda set!)))
     `(,form . ,(purify-letrec-each es set!-vars)))
    ((let ,bs ,e)
     (make-let (map (lambda (b) (list (car b) (purify-letrec (cadr b) set!-vars))) bs)
      (purify-letrec e set!-vars)))
    ((letrec ,bs ,e)
     (letrec->fix (map car bs) (purify-letrec-each (map cadr bs) set!-vars) (purify-letrec e set!-vars) set!-vars))
    ((lambda ,params ,variadic? ,e)
     `(lambda ,params ,variadic? ,(purify-letrec e set!-vars)))
    (,() (error "purify-letrec" "unmatch" e))))

(define (purify-letrec-prog e)
  (purify-letrec e (mut-vars e)))

;;; abstract-fold

(define (foldable-value? e)
  (match e
    ((quote ,q)
     (or (null? q) (eof-object? q) (eq? q (void))))
    (,() (or (integer? e) (char? e) (boolean? e)))))

(define (foldable-value->value e)
  (match e
    ((quote ,q) q)
    (,() e)))

(define (value->foldable-value e)
  (cond
    ((null? e) ''())
    ((eof-object? e) `(quote ,(eof-object)))
    ((eq? (void) e) `(quote ,(void)))
    ((or (integer? e) (char? e) (boolean? e)) e)
    (else (error "value->foldable-value" "bad-value" e))))

(define (abstract-fold-each es env set!-vars)
  (map (lambda (e) (abstract-fold e env set!-vars)) es))

(define (abstract-fold e env set!-vars)
  (match e
    (,()
     (guard (or (self-evaluating? e) (string? e)))
     (if (variable? e) (apply-env e env) e))
    ((quote ,()) e)
    ((call . ,es) 
     (match `(call . ,(abstract-fold-each es env set!-vars))
      ((call (prim ,pr) . ,es)
       (guard (andmap foldable-value? es))
       (let ((pr-impl (get-prim-impl pr)))
        (if pr-impl
            (let ((v (value->foldable-value (apply pr-impl (map foldable-value->value es)))))
              (if (and (integer? v)
                       (not (and (<= (- (ash 1 24)) v) (<= v (ash 1 24)))))
                  `(call (prim ,pr) . ,es)
                  v))
            `(call (prim ,pr) . ,es))))
      (,e e)))
    ((,form . ,es)
     (guard (memq form '(foreign-call begin case-lambda set!)))
     `(,form . ,(abstract-fold-each es env set!-vars)))
    ((let ,bs ,e)
     (let iter ((xs (map car bs))
                (es (abstract-fold-each (map cadr bs) env set!-vars))
                (bs '())
                (env env))
      (cond
        ((not (pair? xs))
         (make-let bs (abstract-fold e env set!-vars)))
        ((and (self-evaluating? (car es))
              (not (memq (car xs) set!-vars))
              (not (and (variable? (car es)) (memq (car es) set!-vars))))
         (iter (cdr xs) (cdr es) bs (extend-env (car xs) (car es) env)))
        (else (iter (cdr xs) (cdr es) (cons (list (car xs) (car es)) bs) (extend-env (car xs) (car xs) env))))))
    ((letrec ,bs ,e)
     (define vars (map car bs))
     (define env* (extend-env vars vars env))
     (make-letrec
      (map (lambda (b) (list (car b) (abstract-fold (cadr b) env* set!-vars)))
           bs)
      (abstract-fold e env* set!-vars)))
    ((if ,e1 ,e2 ,e3)
     (let ((e1 (abstract-fold e1 env set!-vars))
           (e2 (abstract-fold e2 env set!-vars))
           (e3 (abstract-fold e3 env set!-vars)))
      (match e1
        (#t e2)
        (#f e3)
        ((call (prim not) ,e1)
         (abstract-fold `(if ,e1 ,e3 ,e2) env set!-vars))
        (,()
         (if (equal? e2 e3)
             `(begin ,e1 ,e2)
             `(if ,e1 ,e2 ,e3))))))
    ((lambda ,params ,variadic? ,e)
     `(lambda ,params ,variadic? ,(abstract-fold e (extend-env params params env) set!-vars)))
    (,() (error "abstract-fold" "unmatch" e))))

(define (abstract-fold-prog e)
  (abstract-fold e (make-env) (mut-vars e)))

(define (remove-dead-vars e)
  (match e
    (,()
     (guard (or (string? e) (self-evaluating? e)))
     e)
    ((quote . ,()) e)
    ((let ,bs ,e)
     (let* ((e (remove-dead-vars e))
            (refs (free-vars e))
            (es (map remove-dead-vars (map cadr bs)))
            (xs (map car bs)))
        (let iter ((es es) (xs xs) (bs '()) (es* '()))
          (cond
            ((not (pair? xs))
             (make-let (reverse bs)
                       (make-begin (reverse (cons e es*)))))
            ((memq (car xs) refs)
             (iter (cdr es) (cdr xs) (cons (list (car xs) (car es)) bs) es*))
            (else
             (iter (cdr es) (cdr xs) bs (cons (car es) es*)))))))
    ((letrec ,bs ,e)
     (let* ((e (remove-dead-vars e))
            (es (map remove-dead-vars (map cadr bs)))
            (refs (fold-left set-union (free-vars e) (map free-vars es))))
        (call-with-values
          (lambda ()
            (partition
              (lambda (b) (memq (car b) refs))
              (map list (map car bs) es)))
          (lambda (bs bs*)
            (make-letrec bs
              (make-begin (append (map cadr bs*) (list e))))))))
    ((,form . ,es)
     (guard (memq form '(if call foreign-call begin set! case-lambda)))
     `(,form . ,(map remove-dead-vars es)))
    ((lambda ,params ,variadic? ,e)
     `(lambda ,params ,variadic? ,(remove-dead-vars e)))
    (,() (error "remove-dead-vars" "unmatch" e))))

;;; simplify-primitive
(define (%symbol-val? e)
  (match e
    ((quote ,e) (symbol? e))
    (,() #f)))

(define (%make-or e . es)
  (fold-left (lambda (e* e) `(if ,e* #t ,e)) e es))

(define (%maybe-intro-names es k)
  (let iter ((es es) (es* '()) (bs '()))
    (cond
      ((not (pair? es))
       (make-let bs (k (reverse es*))))
      ((or (self-evaluating? (car es)) (%symbol-val? (car es)))
       (iter (cdr es) (cons (car es) es*) bs))
      (else
       (let ((v (gensym* "v")))
        (iter (cdr es) (cons v es*) (cons (list v (car es)) bs)))))))

(define (applicate fn)
  (lambda (args) (apply fn args)))

(include "lib/cxr-builder.scm")

(define cxr-builder
  (map
    (lambda (cxr f) (list cxr f))
    cxrs
    (cxr-builder-maker
      (lambda (e) (make-primcall 'car e))
      (lambda (e) (make-primcall 'cdr e)))))

(define (simplify-primitive e)
  (match e
    (,()
     (guard (or (self-evaluating? e) (string? e)))
     e)
    ((quote ,q)
     (if (symbol? q)
         e
         `(quote ,(simplify-primitive q))))
    ((call (prim ,cxr) ,e1)
     (guard (assq cxr cxr-builder))
     (simplify-primitive ((apply-env cxr cxr-builder) e1)))
    ((call (prim add1) ,e1)
     (simplify-primitive `(call (prim +) ,e1 1)))
    ((call (prim sub1) ,e1)
     (simplify-primitive `(call (prim -) ,e1 1)))
    ((call (prim -) ,e1)
     `(call (prim -) 0 ,(simplify-primitive e1)))
    ((call (prim make-string) ,e1)
     (simplify-primitive `(call (prim make-string) ,e1 #\nul)))
    ((call (prim make-vector) ,e1)
     (simplify-primitive `(call (prim make-vector) ,e1 0)))
    ((call (prim bitwise-ior) ,e1 . ,es)
     (let ((e1 (simplify-primitive e1))
           (es (map simplify-primitive es)))
      (fold-left (lambda (e* e) `(call (prim bitwise-ior) ,e* ,e)) e1 es)))
    ((call (prim list) . ,es)
     (simplify-primitive (fold-right (lambda (e r) `(call (prim cons) ,e ,r)) ''() es)))
    ((call (prim symbol-hash) . ,es)
     (simplify-primitive `(call (prim %symbol-hash) . ,es)))
    ;;; NOTE: strength-reduction example
    ((call (prim equal?) ,e1 ,e2)
     (let ((e1 (simplify-primitive e1))
           (e2 (simplify-primitive e2)))
      (cond
        ((or (and (immediate? e1) (immediate? e2))
             (and (string? e1) (string? e2))
             (and (%symbol-val? e1) (%symbol-val? e2)))
         (equal? e1 e2))
        ((or (string? e1) (string? e2))
         `(call ,(make-primcall '%symbol-value ''string=?) ,e1 ,e2))
        (else
         (%maybe-intro-names (list e1 e2)
          (applicate
            (lambda (e1 e2)
              (%make-or (make-primcall 'eq? e1 e2)
                        `(call ,(make-primcall '%symbol-value ''equal?) ,e1 ,e2)))))))))
    ((call (prim <=) ,e1 ,e2 . ,es)
     (define es* (map simplify-primitive (list* e1 e2 es)))
     (define (pairing es)
      (cond
        ((not (pair? es)) '())
        ((not (pair? (cdr es))) '())
        (else
          (cons (list (car es) (cadr es))
                (pairing (cdr es))))))
     (fold-right
      (lambda (e r)
        `(if (call (prim <=) . ,e) ,r #f))
      #t
      (pairing es*)))
    ((call (prim memq) ,e1 ,e2)
     (define (*->symbol-list e)
      (match e
        ((quote ()) '())
        ((quote ,e)
         (let recur ((e e))
          (match e
            ((quote ()) '())
            ((quote ,q)
             (guard (symbol? q))
             q)
            ((call (prim cons) ,e1 ,e2)
             (let ((e1 (recur e1))
                   (e2 (recur e2)))
                (if (and (symbol? e1) (or (pair? e2) (null? e2)))
                    (cons e1 e2)
                    #f)))
            (,() (error "*->symbol-list" "unmatch" e)))))
        (,() #f)))
     (let* ((e1 (simplify-primitive e1))
            (e2 (simplify-primitive e2))
            (r (*->symbol-list e2)))
      (if r
          (%maybe-intro-names (list e1)
            (lambda (es) (define e1 (car es)) (apply %make-or
                (map (lambda (s) `(call (prim eq?) ,e1 (quote ,s))) r))))
          `(call ,(make-primcall '%symbol-value ''memq) ,e1 ,e2))))
    ((call (prim write) ,e1)
     `(foreign-call (label s_write) ,(simplify-primitive e1) (label stdout)))
    ((call (prim newline))
     `(foreign-call (label s_newline) (label stdout)))
    ((call (prim $call/cc) ,e1)
     `(call (prim call/cc) ,(simplify-primitive e1)))
    ((,form . ,es)
     (guard (memq form '(begin if call foreign-call set! case-lambda)))
     `(,form . ,(map simplify-primitive es)))
    ((lambda (,x) ,variadic? (call (prim apply) (prim values*) ,y))
     (if (and variadic? (eq? x y) (variable? y))
         `(lambda (,x) ,variadic? (call (prim values*) ,y))
         (error "simple-primitive" "bad-form" e)))
    ((lambda ,params ,variadic? ,e)
     `(lambda ,params ,variadic? ,(simplify-primitive e)))
    ((let ,bs ,e)
     (make-let
      (map (lambda (b) (list (car b) (simplify-primitive (cadr b)))) bs)
      (simplify-primitive e)))
    ((letrec ,bs ,e)
     (make-letrec
      (map (lambda (b) (list (car b) (simplify-primitive (cadr b)))) bs)
      (simplify-primitive e)))
    (,() (error "simplify-primitive" "unmatch" e))))

;;; reorder-expr
(define (sethi-ullman-number e)
  (match e
    (,()
     (guard (self-evaluating? e))
     0)
    (,()
     (guard (string? e))
     1)
    ((quote ,q)
     (sethi-ullman-number q))
    ((,call . ,es)
     (guard (memq call '(call foreign-call)))
     (let ((es (filter (lambda (e) (not (self-evaluating? e))) es)))
      (fold-left max (length es) (map sethi-ullman-number es))))
    ((begin . ,es)
     (fold-left max 0 (map sethi-ullman-number es)))
    ((set! ,x ,e1)
     (max 1 (sethi-ullman-number e1)))
    ((lambda ,params ,variadic? ,e) (length (free-vars e)))
    ((case-lambda . ,es)
     (fold-left max (length es) (map sethi-ullman-number es)))
    ((if ,e1 ,e2 ,e3)
     (max (sethi-ullman-number e1)
          (max (sethi-ullman-number e2)
               (sethi-ullman-number e3))))
    ((letrec ,bs ,e)
     (max (length bs) (sethi-ullman-number e)))
    ;;; let case is approximation here
    ((let ,bs ,e)
     (fold-left max
      (sethi-ullman-number e)
      (map +
        (list-sort
          >
          (map sethi-ullman-number (map cadr bs)))
        (iota (length bs)))))
    (,() (error "sethi-ullman-number" "unmatch" e))))

(define (let-intro-name es k)
  (let iter ((es es) (bs '()) (es* '()))
    (cond
      ((not (pair? es)) (k bs (reverse es*)))
      ((self-evaluating? (car es))
       (iter (cdr es) bs (cons (car es) es*)))
      (else
       (let ((v (gensym* "v")))
        (iter (cdr es) (cons (list v (car es)) bs) (cons v es*)))))))

(define (reorder-expr e)
  (match e
    (,()
     (guard (or (self-evaluating? e) (string? e)))
     e)
    ((quote ,q)
     `(quote ,(reorder-expr q)))
    ((,form . ,es)
     (guard (memq form '(begin if case-lambda set!)))
     `(,form . ,(map reorder-expr es)))
    ((,call . ,es)
     (guard (memq call '(call foreign-call)))
     (let-intro-name (map reorder-expr es)
      (lambda (bs* es)
        (define bs
          (map cdr
            (list-sort
              (lambda (b0 b1) (> (car b0) (car b1)))
              (map (lambda (b) (cons (sethi-ullman-number (cadr b)) b)) bs*))))
        (make-let bs `(,call . ,es)))))
    ((let ,bs ,e)
     (make-let
      (map cdr
           (list-sort
             (lambda (b0 b1) (> (car b0) (car b1)))
             (map (lambda (b) (cons (sethi-ullman-number (cadr b)) b))
                   (map (lambda (b) (list (car b) (reorder-expr (cadr b)))) bs))))
      (reorder-expr e)))
    ((letrec ,bs ,e)
     (make-letrec (map (lambda (b) (list (car b) (reorder-expr (cadr b)))) bs)
      (reorder-expr e)))
    ((lambda ,params ,variadic? ,e)
     `(lambda ,params ,variadic? ,(reorder-expr e)))
    (,() (error "reorder-expr" "unmatch" e))))

;;; lift-const

(define (lift-const-simple-cont mk)
  (lambda (v s c) (values (mk v) s c)))

(define (lift-const-each es)
  (if (pair? es)
      (let*-values ([(e s c) (lift-const (car es))]
                    [(es s* c*) (lift-const-each (cdr es))])
        (values (cons e es) (set-union s s*) (append c c*)))
      (values '() (make-set) (list))))

(define (lift-const e)
  (match e
    (,()
     (guard (self-evaluating? e))
     (match e
      ((prim ,pr)
       (values e (make-set e) (list)))
      (,()
       (values e (make-set) (list)))))
    (,()
     (guard (string? e))
     (define v (gensym* "const"))
     (values `(label* ,v) (make-set) (list (list v e))))
    ((quote ,q)
     (cond
      ((symbol? q)
       (values e (make-set q) (list)))
      ((pair? q)
       (let ((v (gensym* "const")))
        (values `(label* ,v) (make-set) (list (list v q)))))
      (else (error "lift-const" "unknown quote" e))))
    ((,form ,bs ,e)
     (guard (memq form '(let letrec)))
     (call-with-values
      (lambda () (lift-const-each (cons e (map cadr bs))))
      (lift-const-simple-cont
        (lambda (e:es) `(,form ,(map list (map car bs) (cdr e:es)) ,(car e:es))))))
    ((call (prim ,pr) . ,es)
     (call-with-values
      (lambda () (lift-const-each es))
      (lift-const-simple-cont (lambda (es) `(call (prim ,pr) . ,es)))))
    ((,form . ,es)
     (guard (memq form '(if call foreign-call begin set! case-lambda)))
     (call-with-values
      (lambda () (lift-const-each es))
      (lift-const-simple-cont (lambda (es) `(,form . ,es)))))
    ((lambda ,params ,variadic? ,e)
     (call-with-values
      (lambda () (lift-const e))
      (lift-const-simple-cont (lambda (e) `(lambda ,params ,variadic? ,e)))))
    (,() (error "lift-const" "unmatch" e))))

(define (convert-symbol e env)
  (match e
    (,()
     (guard (or (self-evaluating? e) (string? e)))
     (match e
      ((prim ,pr)
       (apply-env e env))
      (,() e)))
    ((quote ,q)
     (cond
      ((symbol? q)
       (let ((r (maybe-apply-env q env)))
        (if r
            (cadr r)
            `(call (label _s_str2sym) ,(symbol->string q)))))
      (else (error "convert-symbol" "quote unknown value" e))))
    ((call (prim ,pr) . ,es)
     `(call (prim ,pr) . ,(map (lambda (e) (convert-symbol e env)) es)))
    ((,form . ,es)
     (guard (memq form '(call foreign-call if begin case-lambda set!)))
     `(,form . ,(map (lambda (e) (convert-symbol e env)) es)))
    ((,form ,bs ,e)
     (guard (memq form '(let letrec)))
     `(,form
        ,(map (lambda (b) (list (car b) (convert-symbol (cadr b) env))) bs)
        ,(convert-symbol e env)))
    ((lambda ,params ,variadic? ,e)
     `(lambda ,params ,variadic? ,(convert-symbol e env)))
    (,() (error "convert-symbol" "unmatch" e))))

(define (lift-const-prog e)
  (define (make-init e)
    (match e
      ((prim ,pr)
       (make-primcall '%symbol-value `(call (label _s_str2sym) ,(symbol->string pr))))
      (,()
       (if (symbol? e)
           `(call (label _s_str2sym) ,(symbol->string e))
           ;;; TODO: workaround to split live-range between complex data initialization
           `(call (lambda () #f ,(convert-symbol e (make-env))))))))
  (define vs (call-with-values (lambda () (lift-const e)) vector))
  (define e* (vector-ref vs 0))
  (define consts (vector-ref vs 1))
  (define complexs (vector-ref vs 2))
  (define vars (map (lambda (_) (gensym* "const")) consts))
  (define inits
    (append
      (map (lambda (v e) `(call (prim %set-label!) (label* ,v) ,(make-init e))) (map car complexs) (map cadr complexs))
      (map (lambda (v e) `(call (prim %set-label!) (label* ,v) ,(make-init e))) vars consts)))
  (define labels (append vars (map car complexs)))
  (define env (extend-env consts (map (lambda (l) `(label* ,l)) labels) (make-env)))
  `(program ,labels
    ,(if (pair? inits)
         (make-begin
          (append
            (cons (make-primcall 'push-constant-table) inits)
            (list (convert-symbol e* env))))
         (convert-symbol e* env))))

;;; uncover-set!
(define (lambda-expr? e)
  (match e
    ((lambda . ,()) #t)
    ((case-lambda . ,()) #t)
    (,() #f)))

(define (mut-vars e)
  (match e
    (,()
     (guard (or (self-evaluating? e) (string? e)))
     (make-set))
    ((quote . ,())
     (make-set))
    ((,form . ,es)
     (guard (memq form '(if call foreign-call begin case-lambda)))
     (fold-left set-union (make-set) (map mut-vars es)))
    ((,form ,bs ,e)
     (guard (memq form '(let letrec)))
     (fold-left set-union (mut-vars e) (map (lambda (b) (mut-vars (cadr b))) bs)))
    ((lambda ,params ,variadic? ,e)
     (mut-vars e))
    ((set! ,x ,e)
     (set-union (make-set x) (mut-vars e)))
    (,() (error "mut-vars" "unmatch" e))))

(define (uncover-set!-each es set!-vars)
  (map (lambda (e) (uncover-set! e set!-vars)) es))

(define (uncover-set!-params params set!-vars cont)
  (cond
    ((null? params)
     (cont '() '()))
    ((memq (car params) set!-vars)
     (uncover-set!-params (cdr params) set!-vars
      (lambda (params* inits)
        (define param (car params))
        (define param* (gensym* param))
        (cont (cons param* params*)
              (cons (list param (make-primcall 'cons param* 0)) inits)))))
    (else
      (uncover-set!-params (cdr params) set!-vars
        (lambda (params* inits)
          (cont (cons (car params) params*)
                inits))))))

(define (uncover-set! e set!-vars)
  (match e
    (,()
     (guard (variable? e))
     (if (memq e set!-vars)
         (make-primcall 'car e)
         e))
    (,()
     (guard (or (self-evaluating? e) (string? e)))
     e)
    ((,form ,bs ,e)
     (guard (memq form '(let letrec)))
     (define (uncover-binding b)
      (define var (car b))
      (define init (uncover-set! (cadr b) set!-vars))
      (list var
            (if (memq var set!-vars)
                (make-primcall 'cons init 0)
                init)))
     ((if (eq? form 'let) make-let make-letrec) (map uncover-binding bs) (uncover-set! e set!-vars)))
    ((set! ,x ,e)
     (guard (variable? x))
     (make-primcall 'set-car! x (uncover-set! e set!-vars)))
    ((,form . ,es)
     (guard (memq form '(if call foreign-call begin case-lambda)))
     `(,form . ,(uncover-set!-each es set!-vars)))
    ((lambda ,params ,variadic? ,e)
     (uncover-set!-params params set!-vars
      (lambda (params inits)
        `(lambda ,params ,variadic? ,(make-let inits (uncover-set! e set!-vars))))))
    (,() (error "uncover-set!" "unmatch" e))))

(define (uncover-set!-prog prog)
  (match prog
    ((program ,labels ,e)
     `(program ,labels ,(uncover-set! e (mut-vars e))))
    (,() (error "uncover-set!-prog" "unmatch" prog))))

(define (free-vars e)
  (match e
    (,()
     (guard (variable? e))
     (make-set e))
    (,()
     (guard (or (self-evaluating? e) (string? e)))
     (make-set))
    ((quote . ,()) (make-set))
    ((,form . ,es)
     (guard (memq form '(call foreign-call if begin)))
     (fold-left set-union (make-set) (map free-vars es)))
    ((let ,bs ,e)
     (fold-left
      set-union
      (set-diff (free-vars e) (list->set (map car bs)))
      (map free-vars (map cadr bs))))
    ((letrec ,bs ,e)
     (set-diff
      (fold-left set-union
        (free-vars e)
        (map free-vars (map cadr bs)))
      (map car bs)))
    ((lambda ,params ,variadic? ,body)
     (set-diff (free-vars body) (list->set params)))
    ((case-lambda . ,clauses)
     (fold-left set-union (make-set) (map free-vars clauses)))
    ((set! ,x ,e)
     (guard (variable? x))
     (set-union (make-set x) (free-vars e)))
    (,() (error "free-vars" "unmatch" e))))

(define (uncover-free e)
  (match e
    (,()
     (guard (or (self-evaluating? e) (string? e)))
     e)
    ((,form . ,es)
     (guard (memq form '(call foreign-call if begin)))
     `(,form . ,(map uncover-free es)))
    ((,form ,bs ,e)
     (guard (memq form '(let letrec)))
     ((if (eq? form 'let) make-let make-letrec)
      (map (lambda (b) (list (car b) (uncover-free (cadr b)))) bs)
      (uncover-free e)))
    ((lambda ,params ,variadic? ,body)
     `(lambda ,params ,variadic? ,(free-vars e) ,(uncover-free body)))
    ((case-lambda . ,clauses)
     `(case-lambda . ,(map uncover-free clauses)))
    (,() (error "uncover-free" "unmatch" e))))

(define (uncover-free-prog prog)
  (match prog
    ((program ,labels ,e)
     `(program ,labels ,(uncover-free e)))
    (,() (error "uncover-free-prog" "unmatch" prog))))

(define (convert-closure-func x e env)
  (define clos (gensym* 'clos))
  (define make-clos-ref (lambda (i) (make-primcall 'closure-ref clos i)))
  (match e
    ((case-lambda . ,clauses)
     `(case-lambda . ,(map (lambda (e) (convert-closure-func #f e env)) clauses)))
    ((lambda ,params ,variadic? ,fvs ,e)
     (define self-ref? (and (symbol? x) (memq x fvs)))
     (define fvs* (if self-ref? (set-diff fvs (make-set x)) fvs))
     (define env*
      (extend-env
        fvs*
        (map make-clos-ref (iota (length fvs*)))
        (extend-env
          params
          params
          (if self-ref?
              (extend-env x (make-clos-ref -1) env)
              env))))
     `(lambda (,clos . ,params) ,variadic? ,(convert-closure-each fvs* env)
        ,(convert-closure e env*)))
    (,() (error "convert-closure-func" "expect-lambda" e))))

(define (convert-closure-each es env)
  (map (lambda (e) (convert-closure e env)) es))

(define (convert-closure e env)
  (match e
    (,()
     (guard (variable? e))
     (apply-env e env))
    (,()
     (guard (or (self-evaluating? e) (string? e)))
     e)
    ((,form . ,es)
     (guard (memq form '(call foreign-call if begin)))
     `(,form . ,(convert-closure-each es env)))
    ((let ,bs ,e)
     (make-let
      (map (lambda (b) (list (car b) (convert-closure (cadr b) env))) bs)
      (convert-closure e (extend-env* (map (lambda (b) (list (car b) (car b))) bs) env))))
    ((letrec ,bs ,e)
     (let* ((vars (map car bs))
            (env (extend-env vars vars env))
            (inits (map (lambda (b) (convert-closure-func (car b) (cadr b) env)) bs)))
      (make-letrec
        (map list vars inits)
        (convert-closure e (extend-env vars vars env)))))
    ((lambda . ,())
     (convert-closure-func #f e env))
    ((case-lambda . ,())
     (convert-closure-func #f e env))
    (,() (error "convert-closure" "unmatch" e))))

(define (convert-closure-prog prog)
  (match prog
    ((program ,labels ,e)
     `(program ,labels ,(convert-closure e (make-env))))
    (,() (error "convert-closure-prog" "unmatch" prog))))


(define letrec->closure (let ()
  (define (make-closure-exp fn fvs)
    (apply make-primcall 'closure `(label ,fn) fvs))

  (define (closure->init x clos)
    (match clos
      ((call (prim closure) (label ,fn) . ,fvs)
       (define fvs-cnt (length fvs))
       (cons
         (make-primcall 'make-closure `(label ,fn) fvs-cnt)
         (map (lambda (i fv) (make-primcall 'closure-set! x i fv))
              (iota fvs-cnt)
              fvs)))
      (,() (error "closure->init" "expect-closure-exp" clos))))
  
  (define (letrec->closure xs inits rest)
    (define clos-inits (map closure->init xs inits))
    (define clos-allocs (map car clos-inits))
    (define clos-set-seq (apply append (map cdr clos-inits)))
    (define body (make-begin (append clos-set-seq (list rest))))
    (make-let (map list xs clos-allocs) body))
  letrec->closure))


(define (reveal-function-each es k)
  (if (pair? es)
      (reveal-function (car es)
        (lambda (e codes-1)
          (reveal-function-each (cdr es)
            (lambda (es codes-2) (k (cons e es) (append codes-1 codes-2))))))
      (k '() '())))

(define (reveal-function e k)
  (match e
    (,()
     (guard (or (self-evaluating? e) (string? e)))
     (k e '()))
    ((,form . ,es)
     (guard (memq form '(call foreign-call if begin)))
     (reveal-function-each es
       (lambda (es codes) (k `(,form . ,es) codes))))
    ((let ,bs ,e)
     (reveal-function-each (cons e (map cadr bs))
      (lambda (e::es codes)
        (k (make-let (map list (map car bs) (cdr e::es)) (car e::es))
           codes))))
    ((letrec ,bs ,e)
     (reveal-function-each (cons e (map cadr bs))
      (lambda (e::es codes)
        (k (letrec->closure (map car bs) (cdr e::es) (car e::es))
           codes))))
    ((lambda ,params ,variadic? ,fvs ,e)
     (define fn (gensym* "lmb"))
     (reveal-function e
      (lambda (e codes)
        (k  (apply make-primcall 'closure `(label ,fn) fvs)
            (cons `(,fn (lambda ,params ,variadic? ,fn ,fvs ,e))
                  codes)))))
    ((case-lambda . ,clauses)
     (define fn (gensym* "clmb"))
     (define (extract-info clause)
      (match clause
        ((lambda ,params ,variadic? ,fvs . ,()) (list variadic? (length params)))
        (,() (error "reveal-function" "extract-info: unknown" clause))))
     (define (extract-fn clos)
      (match clos
        ((call (prim closure) ,fn . ,()) fn)
        (,() (error "reveal-function" "extract-fn: unknown" clos))))
     (reveal-function-each clauses
      (lambda (es codes)
        (k (apply make-primcall 'closure `(label ,fn) es)
           (cons `(,fn (case-lambda . ,(map list* (iota (length es)) (map extract-fn es) (map extract-info clauses))))
                 codes)))))
    (,() (error "reveal-function" "unmatch" e))))

(define (reveal-function-prog prog)
  (match prog
    ((program ,labels ,e)
     (reveal-function e
        (lambda (e codes)
          (define main (gensym* "main"))
          (define clos (gensym* "clos"))
          `(program ,main ,labels ((,main (lambda (,clos) #f ,main () ,e)) . ,codes)))))
    (,() (error "reveal-function-prog" "unmatch" prog))))

(define (seq->let es)
  (let recur ((e (car es)) (es (cdr es)))
    (if (pair? es)
        `(let ((_ ,e)) ,(recur (car es) (cdr es)))
        e)))

(define (rco-intro-name e k)
  (let ((v (gensym* "v")))
    `(let ((,v ,e)) ,(k v))))

(define (named-rco e k)
  (rco e
       (lambda (e)
         (if (self-evaluating? e)
             (k e)
             (rco-intro-name e k)))))

(define (rco-exps es k)
  (if (pair? es)
      (named-rco (car es)
        (lambda (e)
          (rco-exps (cdr es)
            (lambda (es) (k (cons e es))))))
      (k '())))

(define (rco-val e)
  (match e
    (,()
     (guard (or (self-evaluating? e) (string? e)))
     e)
    (,() (error "rco-val" "unmatch" e))))

(define (rco-let bs e k)
  (let recur ((bs bs) (acm '()) (k k))
    (cond
      ((not (pair? bs))
       (make-let acm (rco e k)))
      ((or (string? (cadar bs))
           (lambda-expr? (cadar bs))
           (self-evaluating? (cadar bs)))
       (recur (cdr bs) (cons (list (caar bs) (rco-val (cadar bs))) acm) k))
      (else
       (rco (cadr (car bs))
            (lambda (v)
              `(let ((,(car (car bs)) ,v))
                ,(recur (cdr bs) acm k))))))))

(define (rco e k)
  (match e
    (,()
     (guard (self-evaluating? e))
     (k e))
    (,()
     (guard (string? e))
     (rco-intro-name e k))
    ((,form . ,es)
     (guard (or (eq? form 'call) (eq? form 'foreign-call)))
     (rco-exps es
      (lambda (es)
        (k `(,form . ,es)))))
    ((let ,bs ,e)
     (rco-let bs e k))
    ((begin . ,es)
     (rco (seq->let es) k))
    ((if ,e1 ,e2 ,e3)
     (named-rco e1
      (lambda (e1)
        (k `(if ,e1
                ,(rco e2 (lambda (e) e))
                ,(rco e3 (lambda (e) e)))))))
    (,() (error "rco" "unmatch" e))))

(define (rco-code c)
  (match (cadr c)
    ((lambda ,params ,variadic? ,fn ,fvs ,e)
     (list (car c) `(lambda ,params ,variadic? ,fn ,fvs ,(rco e (lambda (e) e)))))
    ((case-lambda . ,clauses) c)
    (,() (error "rco-code" "unmatch" c))))

(define (remove-complex-operand prog)
  (match prog
    ((program ,main ,labels ,codes)
     `(program ,main ,labels ,(map rco-code codes)))
    (,() (error "remove-complex-operand" "unmatch" prog))))

;;; (make-live-set x xs)
(define make-live-set cons)
(define live-set-defvar car)
(define live-set-alives cdr)

(define (propagate-live-set lv live-sets)
  (if (pair? live-sets)
      (match (live-set-alives (car live-sets))
        ((if ,csq-live-sets ,alt-live-sets)
         (cons
          (make-live-set
            (live-set-defvar (car live-sets))
            `(if ,(propagate-live-set lv csq-live-sets)
                 ,(propagate-live-set lv alt-live-sets)))
          (propagate-live-set lv (cdr live-sets))))
        (,alives
         (cons
          (make-live-set
            (live-set-defvar (car live-sets))
            (set-union lv alives))
          (propagate-live-set lv (cdr live-sets)))))
      '()))

(define (uncover-live-exp e)
  (match e
    (,()
     (guard (or (string? e) (self-evaluating? e)))
     (if (variable? e) (make-set e) (make-set)))
    ((,form . ,es)
     (guard (memq form '(call foreign-call)))
     (list->set (filter variable? es)))
    (,() (error "uncover-live-exp" "unmatch" e))))

(define (uncover-live e)
  (match e
    ((let ((,x (if ,pred ,conseq ,altern))) ,e2)
     (let* ((live-sets (uncover-live e2))
            (live-set-after-if (set-diff (live-set-alives (car live-sets)) (make-set x)))
            (csq-live-sets (propagate-live-set live-set-after-if (uncover-live conseq)))
            (alt-live-sets (propagate-live-set live-set-after-if (uncover-live altern))))
      `(
        ,(make-live-set
          #f
          (set-union
            (uncover-live-exp pred)
            (live-set-alives (car csq-live-sets))
            (live-set-alives (car alt-live-sets))))
        ,(make-live-set
          #f
          `(if ,csq-live-sets ,alt-live-sets))
        ,(make-live-set (if (eq? x '_) #f x) live-set-after-if)
        ,@live-sets
      )))
    ((let ((,x ,e1)) ,e2)
     (let* ((live-sets (uncover-live e2)))
        (cons
          (make-live-set (if (eq? x '_) #f x)
            (set-union
              (set-diff (live-set-alives (car live-sets)) (make-set x))
              (uncover-live-exp e1)))
          live-sets)))
    ((let ,bs ,e)
     (uncover-live (fold-right (lambda (b e) (make-let (list b) e)) e bs)))
    ((if ,pred ,conseq ,altern)
     (let ((csq-live-sets (uncover-live conseq))
           (alt-live-sets (uncover-live altern)))
     (list
      (make-live-set #f
        (set-union (uncover-live-exp pred)
                   (live-set-alives (car csq-live-sets))
                   (live-set-alives (car alt-live-sets))))
      (make-live-set #f `(if ,csq-live-sets ,alt-live-sets)))))
    (,()
     (list (make-live-set #f (uncover-live-exp e))))
    (,() (error "uncover-live" "unmatch" e))))

(define (%shift-live-set-next live-set)
  (match (live-set-alives live-set)
    ((if ,csq-lv ,alt-lv)
     (set-union (live-set-alives (car csq-lv)) (live-set-alives (car alt-lv))))
    (,lv lv)))

(define (shift-live-sets live-sets)
  (cond
    ((not (pair? live-sets)) '())
    ((not (pair? (cdr live-sets)))
      (match (live-set-alives (car live-sets))
       ((if ,csq-lv ,alt-lv)
        (list (make-live-set (live-set-defvar (car live-sets))
                             `(if ,(shift-live-sets csq-lv) ,(shift-live-sets alt-lv)))))
       (,() live-sets)))
    (else
      (match (live-set-alives (car live-sets))
        ((if ,csq-lv ,alt-lv)
         (cons
          (make-live-set
            (live-set-defvar (car live-sets))
            `(if ,(shift-live-sets csq-lv) ,(shift-live-sets alt-lv)))
          (shift-live-sets (cdr live-sets))))
        (,lv
         (cons
          (make-live-set
            (live-set-defvar (car live-sets))
            (set-diff
              (set-intersection lv (%shift-live-set-next (cadr live-sets)))
              (make-set (live-set-defvar (car live-sets)))))
          (shift-live-sets (cdr live-sets))))))))

(define (strip-live-sets live-sets)
  (if (pair? live-sets)
      (match (live-set-alives (car live-sets))
        ((if ,csq-lv ,alt-lv)
         (cons (make-live-set
                  (live-set-defvar (car live-sets))
                  `(if ,(strip-live-sets csq-lv) ,(strip-live-sets alt-lv)))
               (strip-live-sets (cdr live-sets))))
        (,()
         (define r (strip-live-sets (cdr live-sets)))
         (if (live-set-defvar (car live-sets))
             (cons (car live-sets) r)
             r)))
      '()))

(include "regalloc.scm")

(define (assign-home-each es env)
  (map (lambda (e) (assign-home e env)) es))

(define (assign-home e env)
  (match e
    (,()
     (guard (variable? e))
     `(call (prim local-ref) ,(apply-env e env)))
    (,()
     (guard (or (self-evaluating? e) (string? e)))
     e)
    ((,form . ,es)
     (guard (memq form '(if call foreign-call)))
     `(,form . ,(assign-home-each es env)))
    ((let ((,x ,e1)) ,e2)
     (let* ((e1 (assign-home e1 env))
            (e1 (if (eq? x '_)
                    e1
                    `(call (prim local-set!) ,(apply-env x env) ,e1)))
            (e2 (assign-home e2 env)))
      `(let ((_ ,e1)) ,e2)))
    ((let ,bs ,body)
     (assign-home (fold-right (lambda (b r) (make-let (list b) r)) body bs) env))
    (,() (error "assign-home" "unmatch" e))))

(define (assign-home-code code)
  (match code
    ((,fn (lambda ,params ,variadic? ,fn ,fvs ,e))
     (let* ((live-sets (cons (make-live-set #f params) (strip-live-sets (shift-live-sets (uncover-live e)))))
            (env (linear-alloc live-sets))
            (alloca-n (+ 1 (fold-left max 0 (map cadr env))))
            (e* (assign-home e env)))
      `(,fn
        (lambda ,params ,variadic? ,fn ,fvs
          (let ((_ (call (prim alloca) ,alloca-n)))
            ,e*)))))
    ((,fn (case-lambda ,fn ,fvs . ,clauses))
     code)
    (,() (error "assign-home-code" "unmatch" code))))

(define (assign-home-prog prog)
  (match prog
    ((program ,main ,labels ,codes)
     `(program ,main ,labels ,(map assign-home-code codes)))
    (,() (error "assign-home-prog" "unmatch" prog))))

(define (explicate-each es)
  (map (lambda (e) (explicate-control e #f)) es))

(define (explicate-control e tail?)
  (match e
    (,()
     (guard (or (self-evaluating? e) (string? e)))
     e)
    ((,form . ,es)
     (guard (memq form '(call foreign-call)))
     `(,form ,tail? . ,(explicate-each es)))
    ((if ,e1 ,e2 ,e3)
     `(if ,(explicate-control e1 #f)
          ,(explicate-control e2 tail?)
          ,(explicate-control e3 tail?)))
    ((let ,bs ,e)
     (make-let
      (map (lambda (b) (list (car b) (explicate-control (cadr b) #f))) bs)
      (explicate-control e tail?)))
    ((lambda ,params ,variadic? ,fn ,fvs ,e)
     `(lambda ,params ,variadic? ,fn ,fvs ,(explicate-control e #t)))
    ((case-lambda ,fn ,fvs . ,clauses) e)
    (,() (error "explicate-control" "unmatch" e))))

(define (explicate-control-prog prog)
  (match prog
    ((program ,main ,labels ,codes)
     `(program ,main ,labels ,(map (lambda (c) (list (car c) (explicate-control (cadr c) #t))) codes)))
    (,() (error "explicate-control-prog" "unmatch" prog))))

(define (%flatten-let e)
  (match e
    (,()
     (guard (or (string? e) (self-evaluating? e)))
     e)
    ((quote ,q)
     (cond
      ((eof-object? q) '(eof-object))
      ((null? q) e)
      ((eq? (void) q) '(void))
      (else e)))
    ((call (prim ,pr) . ,es)
     `(,pr . ,(map %flatten-let es)))
    ((,form . ,es)
     (guard (memq form '(call foreign-call if begin case-lambda set!)))
     `(,form . ,(map %flatten-let es)))
    ((let ,bs ,e)
     (%flatten-let `(let* ,bs ,e)))
    ((letrec ,bs ,e)
     (make-letrec (map (lambda (b) (list (car b) (%flatten-let (cadr b)))) bs) (%flatten-let e)))
    ((let* ,bs ,e)
     (define bs* (map (lambda (b) (list (car b) (%flatten-let (cadr b)))) bs))
     (define (make-let* bs e) (if (pair? bs) `(let* ,bs ,e) e))
     (match (%flatten-let e)
      ((let* ,cs ,e)
       (make-let* (append bs* cs) e))
      (,e (make-let* bs* e))))
    ((lambda ,params ,variadic? ,e)
     `(lambda ,params ,variadic? ,(%flatten-let e)))
    ((lambda ,params ,variadic? ,fn ,fvs ,e)
     `(lambda ,params ,variadic? ,fn ,fvs ,(%flatten-let e)))
    ((program ,labels ,e)
     `(program ,labels ,(%flatten-let e)))
    ((program ,main ,labels ,codes)
     `(program ,main ,labels ,(map (lambda (c) (list (car c) (%flatten-let (cadr c)))) codes)))
    (,() (error "%flatten-let" "unmatch" e))))

(define (apply-passes e . passes)
  (fold-left (lambda (e p) (p e)) e passes))

(define (measure-pass name f)
  (lambda (e) (time (let ((e (f e))) (pretty-print name) e))))

(define (print-expr e)
  (pretty-print (%flatten-let e) (current-error-port)) e)

(define (preprocess e)
  (apply-passes e
    uniquify-prog
    quotify-immediate
    purify-letrec-prog
    abstract-fold-prog
    remove-dead-vars
    simplify-primitive
    reorder-expr
    lift-const-prog
    uncover-set!-prog
    uncover-free-prog
    convert-closure-prog
    reveal-function-prog
    remove-complex-operand
    assign-home-prog
    explicate-control-prog
    ))
preprocess
))