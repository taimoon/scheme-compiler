(load "match.scm")
(load "utils.scm")
#|
  desugar ; return only well-defined AST (collect-definition uniqufy collect-unbound tag-primitive convert-variadic)
  reveal-function (collect-lambda)
  convert-assignment (uncover-get! collect-set!)
  explicate-control (explicate-seq)

  compiler expression:
  (datum)
  (labelcall ,e ,es)
  (label ,lbl)
  (funcall ,e ,es)
  (primcall ,symbolic-op ,es)
  (foreign-call ,symbolic-op ,es)
  (prim ,op ,top-label)
  (code ,params . ,body)
  (code ,varadic? ,params . ,body)
  (lambda ,variadic? ,params . ,body)
  (program ,entry ,defns ,unbounds ,datums)
  | usage
    entry - linker will call the entry label
    defns - used in dependency check; not implemented
    unbounds - used in dependency check; not implemented
    datums - linker will initialize each label of the datums with (vector 0)
  |
|#

(define (let*->let e)
  (if (null? (cadr e))
      (cons 'begin (cddr e))
      (let*->let-aux (cadr e) (cddr e))))

(define (let*->let-aux bindings body)
  (if (null? (cdr bindings))
      (list* 'let (list (car bindings)) body)
      (list 'let (list (car bindings)) (let*->let-aux (cdr bindings) body))))

(define (case->if e)
  (let ((sym (gensym)))
    (list* 'let (list (list sym (cadr e)))
      (list (case->if-aux sym (cddr e))))))

(define (case->if-aux pred-sym clauses)
  (let ([clause (car clauses)]
        [rest (cdr clauses)])
    (if (eq? (caar rest) 'else)
        (list 'if (list 'member pred-sym (list 'quote (car clause)))
              (cons 'begin (cdr clause))
              (cons 'begin (cdar rest)))
        (list 'if (list 'member pred-sym (list 'quote (car clause)))
              (cons 'begin (cdr clause))
              (if (null? (cdr rest)) #f (case->if-aux pred-sym rest))))))

(define (or->ifs es)
  (if (pair? es)
      (list 'if (car es) #t (or->ifs (cdr es)))
      #f))

(define (and->ifs es)
  (if (pair? es)
      (list 'if (car es) (and->ifs (cdr es)) #f)
      #t))

(define (cond->ifs clauses)
  (let ([clause (car clauses)]
        [rest (cdr clauses)])
    (if (eq? (caar rest) 'else)
        (list 'if (car clause)
              (cons 'begin (cdr clause))
              (cons 'begin (cdar rest)))
        (list 'if (car clause)
                  (cons 'begin (cdr clause))
                  (if (null? (cdr rest)) #f (cond->ifs rest))))))

#;(`(a . ,c) ===> (cons 'a c))
#;(`(a b ,c) ===> (list `a `b c))
#;(`(a ,b ,@c) ===> (append (list 'a) (list b) c))
#;(`(a . ,@c) ===> (list 'a 'unquote-splicing 'c))
#;(`(a . ,@c) ==> `(a . (unquote-splicing c)) ===> `(a unquote-splicing c))
(define (expand-quasiquote form)
  (expand-qq (cadr form)))

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
    [(null? form) ''()]
    [(not (pair? form))
     (list 'list (expand-qq form))]
    [(tail-unquote? form)
     (tail-unquote form)]
    [(and (pair? (car form))
          (eq? 'unquote-splicing (caar form)))
      (list 'append (cadar form) (qq-list (cdr form)))]
    [else (list 'append (list 'list (expand-qq (car form))) (qq-list (cdr form)))]))

(define (quote->cons q)
  (match q
    [(quote (,left . ,right))
     `(cons ,(quote->cons `(quote ,left)) ,(quote->cons `(quote ,right)))]
    [(quote ()) ''()]
    [(quote ,q) (guard (not (symbol? q))) q]
    [(quote ,q) `(string->symbol ,(symbol->string q))]
    [,() (quote->cons 'unknown:quotation q)]))

(define (linearize-seq es)
  (fold-right (lambda (e acm) (append (linearize-begin e) acm)) '() es))

(define (linearize-begin e)
  (match e
    [(begin . ,es) (linearize-seq es)]
    [,() (list e)]))

(define (collect-definition-seq es)
  (fold-left set-union (make-set) (map collect-definition es)))

(define (collect-definition e)
  (match e
    [,() (guard (not (pair? e))) (make-set)]
    [(define (,fn . ,()) . ,()) (make-set fn)]
    [(define ,x . ,()) (make-set x)]
    [(begin . ,es) (collect-definition-seq es)]
    [,es (make-set)]))

(define primitives
  '(
    (char? x)
    (integer? x)
    (boolean? x)
    (null? x)
    (pair? x)
    (string? x)
    (vector? x)
    (procedure? x)
    (symbol? x)
    (char->integer x)
    (integer->char x)
    (add1 x)
    (sub1 x)
    (zero? x)
    (+ x y)
    (- x y)
    (* x y)
    (< x y)
    (<= x y)
    (= x y)
    (> x y)
    (>= x y)
    (logand x y)
    (logior x y)
    (ashl x y)
    (ashr x y)
    (eq? x y)
    (make-string k)
    (string-ref s i)
    (string-set! s i c)
    (string-length s)
    (cons x y)
    (car p)
    (cdr p)
    (set-car! p y)
    (set-cdr! p y)
    (vector . x)
    (make-vector k)
    (vector-length vs)
    (vector-ref vs i)
    (vector-set! vs i v)

    (str->sym str)
    (sym->str sym)
    (gc-flip sz)
    ; (display x)
    ; (newline)
    ; (pretty-print x)

    (apply f xs)
  ))

(define (mangling-top-definition defn-sym)
  ;;; prevent the mangler mangling C functions
  (string->symbol
    (string->id-string
      (string-append "_" (symbol->string defn-sym)))))

(set! primitives
  (map (lambda (e) (list 'prim (car e) (mangling-top-definition (car e))))
       primitives))

(define macrco-keyword-id (generate-label 'macro))

(define (macrco-keyword-id? t)
  (eq? t macrco-keyword-id))

(define keywords
  '(begin set! lambda
    define let let* letrec
    if cond else case not and or
    quote
    quasiquote unquote-splicing match
    load define-syntax
    list
    
    labels code primcall labelcall foreign-call global-set!
    ))

(define (collect-globals e)
  (match e
    [,() (guard (not (pair? e))) (make-set)]
    [(quote ()) (make-set)]
    [(set! ,() ,e) (collect-globals e)]
    [(if ,pred ,conseq ,alter)
     (set-union (collect-globals pred)
      (set-union (collect-globals conseq) (collect-globals alter)))]
    [(begin . ,es)
     (fold-left set-union (make-set) (map collect-globals es))]
    [(let ,bindings . ,body)
     (set-union
     (fold-left set-union (make-set) (map (lambda (b) (collect-globals (cadr b))) bindings))
      (set-diff (collect-globals (cons 'begin body)) (map car bindings)))]
    [(lambda ,() ,() . ,body)
     (fold-left set-union (make-set) (map collect-globals body))]
    [(primcall ,() ,es)
     (fold-left set-union (make-set) (map collect-globals es))]
    [(unbound ,x ,x*) (make-set `(,x . ,x*))]
    [(global-set! (unbound ,x ,x*) ,e)
      (set-add-elem `(,x . ,x*) (collect-globals e))]
    [(global-set! ,() ,e) (collect-globals e)]
    [(global ,()) (make-set)]
    [(prim . ,()) (make-set)]
    [(labels . ,()) (make-set)]
    [(code . ,()) (make-set)]
    [(label ,()) (make-set)]
    [(labelcall ,op ,es)
     (fold-left set-union (collect-globals op) (map collect-globals es))]
    [(funcall ,op ,es)
     (fold-left set-union (collect-globals op) (map collect-globals es))]
    [(foreign-call ,op ,es)
     (fold-left set-union (make-set) (map collect-globals es))]
    [,() (error "collect-globals" "unknown" e)]))

(define (stripping-global e)
  (match e
    [,() (guard (not (pair? e))) e]
    [(global-set! (unbound ,x ,x*) ,e)
     `(global-set! ,x* ,(stripping-global e))]
    [(unbound ,x ,x*) `(global ,x*)]
    [,() (map stripping-global e)]))

(define (desugar-top e)
  (let* ([env (map (lambda (p) (cons (cadr p) p)) primitives)]
         [env (extend-env keywords (map (lambda _ macrco-keyword-id) keywords) env)])
    (match e
      [(begin . ,es)
       (let* ([es (linearize-seq es)]
              [defns (collect-definition-seq es)]
              [defns* (map mangling-top-definition defns)]
              [entry (generate-label 'main)]
              [defns (cons entry defns)]
              [defns* (cons entry defns*)]
              [env* (extend-env defns (map (lambda (defn*) `(global ,defn*)) defns*) env)]
              [es (map (lambda (e) (desugar e env*)) es)]
              [globals (collect-globals `(begin . ,es))]
              [defns-corspd (map cons defns defns*)])
          `(program ,entry ,defns-corspd ,globals ,(stripping-global `(begin . ,es))))]
      [,() (error "desugar-top" "unknown expression" e)])))

(define (macro-keyword? v env)
  (let ([res (assoc-env v env)])
    (if res
        (macrco-keyword-id? (cdr res))
        res)))

(define (desugar e env)
  (match e
    [,v (guard (symbol? v))
     (let ([res (assoc-env v env)])
      (cond [(not (pair? res))
             `(unbound ,v ,(mangling-top-definition v))]
            [(macrco-keyword-id? (cdr res))
             ;;; TODO: workaround
             (if (eq? v 'list)
                 (desugar '(lambda x x) env)
                 (error "desugar" "syntax-error" v))]
            [(pair? res) (cdr res)]
            [else (error "desugar" "unknown symbol" v)]))]
    [,v (guard (not (pair? v))) v]
    [(,e . ,es)
     (guard (not (macro-keyword? e env)))
     (let* ([op (desugar e env)]
            [es (map (lambda (e) (desugar e env)) es)])
      (cond
        [(and (pair? op) (eq? (car op) 'prim))
         (list 'primcall (cadr op) es)]
        [(and (pair? op) (eq? (car op) 'label))
         (list 'labelcall op es)]
        [else
          `(funcall ,op ,es)
          ; the check will slow down the make-compiler stuck forever
          #;(let ([f (generate-label 'f)])
          `(let ([,f ,op])
            (if (primcall procedure? (,f))
                (funcall ,f ,es)
                (foreign-call s_function_error (,f)))))]))]
    [(define-syntax . ,()) 0]
    [(quote ()) '(quote ())]
    [(quote ,()) (desugar (quote->cons e) env)]
    [(quasiquote ,q) (desugar (expand-qq q) env)]
    [(if ,e1 ,e2) ;;; TODO: workaround
     `(if ,(desugar e1 env)
          ,(desugar e2 env)
          #f)]
    [(if ,e1 ,e2 ,e3)
     `(if ,(desugar e1 env)
          ,(desugar e2 env)
          ,(desugar e3 env))]
    [(set! ,x ,e)
     (match (desugar x env)
      [(unbound ,x ,x*)
       `(global-set! (unbound ,x ,x*) ,(desugar e env))]
      [(global ,x*)
       `(global-set! ,x* ,(desugar e env))]
      [,x*
       (guard (symbol? x*))
       `(set! ,x* ,(desugar e env))]
      [,x*
       (error "desugar" "unknown variable" x*)])]
    [(begin . ,es)
     (let* ([es (linearize-seq es)]
            [defns (collect-definition-seq es)]
            [defns* (map generate-label defns)]
            [env* (extend-env defns defns* env)]
            [es (map (lambda (e) (desugar e env*)) es)])
      (if (null? defns)
          (cons 'begin es)
          `(let ,(map (lambda (var) (list var 0)) defns*) . ,es)))]
    [(lambda ,params . ,body)
     (let* ([body (linearize-seq body)]
            [defns (collect-definition-seq body)]
            [defns* (map generate-label defns)]
            [params* (improper->list params)]
            [params** (map generate-label params*)]
            [env* (extend-env params* params** env)]
            [env* (extend-env defns defns* env*)]
            [body (map (lambda (e) (desugar e env*)) body)])
      `(lambda ,(not (list? params)) ,params**
        ,@(if (null? defns*)
             body
             `((let ,(map (lambda (v) (list v 0)) defns*) . ,body)))))]
    [(let ,loop ,bindings . ,body)
     (guard (symbol? loop))
     (let ([init (map cadr bindings)]
           [params (map car bindings)])
      (desugar 
        `(letrec ([,loop (lambda ,params . ,body)])
            (,loop . ,init))
        env))]
    [(let ,bindings . ,body)
     (let* ([defns (collect-definition-seq body)]
            [defns* (map generate-label defns)]
            [vars (map generate-label (map car bindings))]
            [env* (extend-env (map car bindings) vars env)]
            [inner-env (extend-env defns defns* env*)]
            [body (linearize-seq body)]
            [body (map (lambda (e) (desugar e inner-env)) body)]
            [bindings* (map (lambda (v b) (list v (desugar (cadr b) env))) vars bindings)])
      `(let
          (,@bindings*
           ,@(if (null? defns*) '() (map (lambda (v) (list v 0)) defns*)))
          ,@body))]
    [(letrec ,bindings . ,body)
     (desugar
      `(let ,(map (lambda (b) (list (car b) 0)) bindings)
            ,@(append (map (lambda (b) `(set! ,(car b) ,(cadr b))) bindings) body))
      env)]
    [(define (,fn . ,params) . ,body)
     (desugar `(set! ,fn (lambda ,params . ,body)) env)]
    [(define ,x ,e)
     (desugar `(set! ,x ,e) env)]
    [(define (,fn . ,params) . ,body)
     `(set! ,(desugar fn env) ,(desugar `(lambda ,params . ,body) env))]
    [(define ,x ,e) `(set! ,(desugar x env) ,(desugar e env))]
    [(list) ''()]
    [(list ,e . ,es)
     (desugar `(cons ,e (list . ,es)) env)]
    [(let* . ,()) (desugar (let*->let e) env)]
    [(case . ,()) (desugar (case->if e) env)]
    [(cond . ,()) (desugar (cond->ifs (cdr e)) env)]
    [(or . ,()) (desugar (or->ifs (cdr e)) env)]
    [(and . ,()) (desugar (and->ifs (cdr e)) env)]
    [(not . ,()) (desugar (list 'if (cadr e) #f #t) env)]
    [(match . ,()) (desugar (compile-match e) env)]
    [(load . ,()) 0] ; TODO: NOT IMPLEMENTED
    [(global-set! ,lbl ,e) ;;; TODO: it is a workaround; not intended to make it part of scheme
     `(global-set! ,lbl ,(desugar e env))]
    [(code ,params . ,body)
     (let* ([params* (improper->list params)]
            [params** (map generate-label params*)]
            [env* (extend-env params* params** env)]
            [body (map (lambda (e) (desugar e env*)) body)])
      `(code ,(not (list? params)) ,params** . ,body))]
    [(labels ,bindings . ,body)
     (let* ([vars (map car bindings)]
            [env* (extend-env vars (map (lambda (v) (list 'label v)) vars) env)])
      `(labels ,(map list vars (map (lambda (e) (desugar (cadr e) env*)) bindings))
          ,@(map (lambda (e) (desugar e env*)) body)))]
    [(primcall ,p . ,es)
     `(primcall ,p ,(map (lambda (e) (desugar e env)) es))]
    [(labelcall ,e . ,es)
     `(labelcall ,(desugar e env) ,(map (lambda (e) (desugar e env)) es))]
    [(foreign-call ,op . ,es)
     (guard (symbol? op))
     `(foreign-call ,op ,(map (lambda (e) (desugar e env)) es))]
    [,e (error "desugar" 'unknown:e e)]))

(define (free-vars-lambda e)
  (match e
    [(lambda ,variadic? ,params . ,body)
     (guard (boolean? variadic?))
     (set-diff (fold-left set-union (make-set) (map free-vars body)) params)]
    [,() (error "free-vars-lambda" "unknown-lambda-form" e)]))

(define (free-vars e)
  (match e
    [,() (guard (symbol? e)) (make-set e)]
    [,() (guard (not (pair? e))) (make-set)]
    [(quote ()) (make-set)]
    [(set! ,x ,e)
     (set-add-elem x (free-vars e))]
    [(if ,pred ,conseq ,alter)
     (set-union (free-vars pred)
      (set-union (free-vars conseq) (free-vars alter)))]
    [(begin . ,es)
     (fold-left set-union (make-set) (map free-vars es))]
    [(let ,bindings . ,body)
     (set-union
     (fold-left set-union (make-set) (map (lambda (b) (free-vars (cadr b))) bindings))
      (set-diff (free-vars (cons 'begin body)) (map car bindings)))]
    [(lambda . ,()) (free-vars-lambda e)]
    [(primcall ,op ,es)
     (fold-left set-union (make-set) (map free-vars es))]
    [(global ,v) (make-set v)]
    [(global-set! ,x ,e)
     (set-add-elem x (free-vars e))]
    [(prim . ,()) (make-set)]
    [(labels . ,()) (make-set)]
    [(code . ,()) (make-set)]
    [(label ,lbl) (make-set)]
    [(labelcall ,op ,es)
     (fold-left set-union (free-vars op) (map free-vars es))]
    [(funcall ,op ,es)
     (fold-left set-union (free-vars op) (map free-vars es))]
    [(foreign-call ,op ,es)
     (fold-left set-union (make-set) (map free-vars es))]
    [,() (error "free-vars" "unknown" e)]))

(define (reveal-function-seq es)
  (let ([ans (map reveal-function es)])
      (cons
        (map car ans)
        (fold-left append '() (map cdr ans)))))

(define (reveal-function e)
  (match e
    [,() (guard (symbol? e)) (cons e '())]
    [,() (guard (not (pair? e))) (cons e '())]
    [(quote ()) (cons '(quote ()) '())]
    [(global ,v)
     (cons `(global ,v) `((,v (datum))))]
    [(global-set! ,x ,e)
     (let ([res (reveal-function e)])
      (cons `(global-set! ,x ,(car res))
            (cons `(,x (datum)) (cdr res))))]
    [(set! ,x ,e)
     (let ([res (reveal-function e)])
      (cons `(set! ,x ,(car res))
            (cdr res)))]
    [(if ,pred ,conseq ,alter)
     (let ([pred (reveal-function pred)]
           [conseq (reveal-function conseq)]
           [alter (reveal-function alter)])
      (cons
        (list 'if (car pred) (car conseq) (car alter))
        (append (cdr pred) (cdr conseq) (cdr alter))))]
    [(begin . ,es)
     (let ([ans (reveal-function-seq es)])
      (cons (cons 'begin (car ans)) (cdr ans)))]
    [(lambda ,variadic? ,params . ,body)
     (guard (boolean? variadic?))
     (let* ([params (cons (generate-label "closure") params)]
            [label (generate-label "lambda")]
            [body-ans (reveal-function-seq body)]
            [body (car body-ans)]
            [top-bindings (cdr body-ans)]
            [free-vars (free-vars-lambda e)]
            [binding `(,label (code ,variadic? ,free-vars ,params . ,body))])
      (cons `(closure ,label ,free-vars) (cons binding top-bindings)))]
    [(let ,bindings . ,body)
     (let* ([es (map (lambda (b) (reveal-function (cadr b))) bindings)]
            [body-ans (reveal-function-seq body)]
            [top-bindings (append (fold-left append '() (map cdr es)) (cdr body-ans))]
            [bindings (map list (map car bindings) (map car es))]
            [body (car body-ans)])
      (cons `(let ,bindings . ,body) top-bindings))]
    [(labels ,bindings . ,body)
     (let* ([bindings*
             (map (lambda (b)
                    (match b
                      [(,label (code ,variadic? ,params . ,body))
                       (guard (boolean? variadic?))
                       `(,label (code ,variadic? () ,params . ,body))]
                      [,() (error "reveal-function" "unknown-bindings" b)]))
                  bindings)]
            [es* (reveal-function-seq body)]
            [inner-bindings (append bindings* (cdr es*))])
      (cons (cons 'begin (car es*)) inner-bindings))]
    [(prim . ,()) (cons e '())]
    [(primcall ,op ,es)
     (let ([es (map reveal-function es)])
      (cons `(primcall ,op ,(map car es))
            (fold-left append '() (map cdr es))))]
    [(label ,lbl) (cons e '())]
    [(labelcall ,op ,es)
     (let ([op (reveal-function op)]
           [es (map reveal-function es)])
      (cons `(labelcall ,(car op) ,(map car es))
            (fold-left append (cdr op) (map cdr es))))]
    [(funcall ,op ,es)
     (let ([op (reveal-function op)]
           [es (map reveal-function es)])
      (cons `(funcall ,(car op) ,(map car es))
            (fold-left append (cdr op) (map cdr es))))]
    [(foreign-call ,op ,es)
     (let ([es (map reveal-function es)])
      (cons `(foreign-call ,op ,(map car es))
            (fold-left append '() (map cdr es))))]
    [,() (error reveal-function 'unknown e)]))

(define (reveal-function-top e)
  (match e
    [(program ,entry ,defns ,globals ,e)
     (let* ([res (reveal-function e)]
           [top-bindings (cdr res)]
           [body (car res)]
           [datum-binding?
             (lambda (b)
               (match b
                 [(,label (datum)) #t]
                 [(,label (code ,() ,() ,() . ,())) #f]
                 [,() (error "reveal-function-top" "unknown binding" b)]))]
           [datums (filter datum-binding? top-bindings)]
           [top-bindings (filter (lambda (b) (not (datum-binding? b))) top-bindings)])
       ;;; TODO: it is workaround to avoid duplicate datums collected
       `(program ,entry ,defns ,globals (labels ,(list->set (map car datums)) ,top-bindings ,body)))]
    [,() (error "reveal-function-top" "unknown binding" e)]))

(define (convert-assignment e)
  (match e
    [(program ,entry ,defns ,globals ,e)
     `(program ,entry ,defns ,globals ,(uncover-get! e (collect-set! e)))]
    [,() (error "convert-assignment" "unknown bindings" e)]))

(define (collect-set! e)
  (match e
    [,() (guard (symbol? e)) (make-set)]
    [,() (guard (not (pair? e))) (make-set)]
    [(quote . ,()) (make-set)]
    [(if ,pred ,conseq ,alter)
     (set-union (collect-set! pred) (set-union (collect-set! conseq) (collect-set! alter)))]
    [(set! ,x ,e) (set-add-elem x (collect-set! e))]
    [(begin . ,es)
     (fold-left set-union (make-set) (map collect-set! es))]
    [(let ,bindings . ,body)
     (set-union
      (fold-left set-union (make-set) (map collect-set! (map cadr bindings)))
      (fold-left set-union (make-set) (map collect-set! body)))]
    [(labels ,datums ,bindings . ,body)
     (set-union
      (fold-left set-union (make-set) (map collect-set! (map cadr bindings)))
      (fold-left set-union (make-set) (map collect-set! body)))]
    [(prim . ,()) e]
    [(primcall ,op ,es)
     (fold-left set-union (make-set) (map collect-set! es))]
    [(code ,variadic? ,free-vars ,params . ,body)
     (guard (boolean? variadic?))
     (fold-left set-union (make-set) (map collect-set! body))]
    [(label . ,()) (make-set)]
    [(labelcall ,label ,es)
     (fold-left set-union (collect-set! label) (map collect-set! es))]
    [(global ,v) (make-set v)]
    [(global-set! ,x ,e) (set-add-elem x (collect-set! e))]
    [(funcall ,op ,es)
     (fold-left set-union (collect-set! op) (map collect-set! es))]
    [(foreign-call ,op ,es)
     (fold-left set-union (make-set) (map collect-set! es))]
    [(closure ,() ,()) e]
    [,() (error "collect-set!" 'unknown e)]))

(define (uncover-get! e set!-vars)
  (match e
    [,() (guard (symbol? e))
     (if (member e set!-vars)
         `(primcall vector-ref (,e 0))
         e)]
    [,() (guard (not (pair? e))) e]
    [(quote ()) e]
    [(if ,pred ,conseq ,alter)
     `(if ,(uncover-get! pred set!-vars) 
          ,(uncover-get! conseq set!-vars) 
          ,(uncover-get! alter set!-vars))]
    [(set! ,x ,e)
     `(primcall vector-set! (,x 0 ,(uncover-get! e set!-vars)))]
    [(begin . ,es)
     (cons 'begin (map (lambda (e) (uncover-get! e set!-vars)) es))]
    [(let ,bindings . ,body)
     `(let 
        ,(map (lambda (b)
                (let ([v (car b)]
                      [e (uncover-get! (cadr b) set!-vars)])
                  (list v
                        (if (member v set!-vars) `(primcall vector (,e)) e))))
                bindings)
        . ,(map (lambda (e) (uncover-get! e set!-vars)) body))]
    [(prim ,() ,()) e]
    [(primcall ,op ,es)
     `(primcall ,op ,(map (lambda (e) (uncover-get! e set!-vars)) es))]
    [(labels ,datums ,bindings . ,body)
     `(labels ,datums ,(map (lambda (b) (list (car b) (uncover-get! (cadr b) set!-vars))) bindings)
        . ,(map (lambda (e) (uncover-get! e set!-vars)) body))]
    [(code ,variadic? ,free-vars ,params . ,body)
     (guard (boolean? variadic?))
     (let* ([env (map (lambda (x) (cons x (if (member x set!-vars) (generate-label x) x))) params)]
            [params* (map cdr env)]
            [header (filter (lambda (x) (member x set!-vars)) params)]
            [header (map (lambda (x) (list x `(primcall vector (,(apply-env x env))))) header)]
            [body (map (lambda (e) (uncover-get! e set!-vars)) body)])
      (if (null? header)
          `(code ,variadic? ,free-vars ,params . ,body)
          `(code ,variadic? ,free-vars ,params* (let ,header . ,body))))]
    [(label ,label)
     `(label ,label)]
    [(global ,v)
     `(primcall vector-ref (,v 0))]
    [(global-set! ,x ,e)
     `(primcall vector-set! (,x 0 ,(uncover-get! e set!-vars)))]
    [(labelcall ,e ,es)
     `(labelcall ,(uncover-get! e set!-vars) ,(map (lambda (e) (uncover-get! e set!-vars)) es))]
    [(funcall ,op ,es)
     `(funcall ,(uncover-get! op set!-vars) ,(map (lambda (e) (uncover-get! e set!-vars)) es))]
    [(foreign-call ,op ,es)
     `(foreign-call ,op ,(map (lambda (e) (uncover-get! e set!-vars)) es))]
    [(closure ,() ,()) e]
    [,() (error "uncover-get!" 'unknown e)]))

(define (explicate-seq es tail?)
  (if (null? (cdr es))
      (list (explicate-control (car es) tail?))
      (cons (explicate-control (car es) #f)
            (explicate-seq (cdr es) tail?))))

(define (explicate-control e tail?)
  (match e
    [,() (guard (not (pair? e))) e]
    [(quote ()) e]
    [(if ,pred ,conseq ,alter)
     `(if ,(explicate-control pred #f) 
          ,(explicate-control conseq tail?) 
          ,(explicate-control alter tail?))]
    [(begin . ,es)
     (cons 'begin (explicate-seq es tail?))]
    [(let ,bindings . ,body)
     `(let ,(map (lambda (b) (list (car b) (explicate-control (cadr b) #f))) bindings)
        ,@(explicate-seq body tail?))]
    [(prim ,() ,()) e]
    [(primcall ,op ,es)
     `(primcall ,op ,(map (lambda (e) (explicate-control e #f)) es))]
    [(labels ,datums ,bindings . ,body)
     `(labels ,datums ,(map (lambda (b) (list (car b) (explicate-control (cadr b) #t))) bindings)
        . ,(explicate-seq body tail?))]
    [(code ,variadic? ,free-vars ,params . ,body)
     (guard (boolean? variadic?))
     `(code ,variadic? ,free-vars ,params . ,(explicate-seq body #t))]
    [(label ,label) `(label ,label)]
    [(global ,()) e]
    [(labelcall ,e ,es)
     `(labelcall ,(explicate-control e #f) ,(map (lambda (e) (explicate-control e #f)) es))]
    [(funcall ,op ,es)
     (let ([op (explicate-control op #f)]
           [es (map (lambda (e) (explicate-control e #f)) es)])
      (if tail?
         `(tailcall ,op ,es)
         `(funcall ,op ,es)))]
    [(foreign-call ,op ,es)
     `(foreign-call ,op ,(map (lambda (e) (explicate-control e #f)) es))]
    [(closure ,() ,()) e]
    [,() (error "explicate-control" "unknown" e)]))

(define (explicate-control-top e)
  (match e
    [(program ,entry ,defns ,globals ,e)
     `(program ,entry ,defns ,globals ,(explicate-control e #t))]
    [,() (error "convert-assignment" "unknown bindings" e)]))