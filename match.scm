(define-syntax match
  (syntax-rules ()
    ((_ e . clauses)
     (let ((value e))
      (match-evaluated value . clauses)))))

(define-syntax match-evaluated
  (syntax-rules (guard)
    ((_ value)
     (error 'match 'no-matching-clauses value))
    ((_ value . ((pattern (guard predicate) . expressions) . clauses))
     (match-clause
        ((pattern value))
        (and)
        ()
        expressions
        (match-evaluated value . clauses)
        predicate))
    ((_ value . ((pattern . expressions) . clauses))
     (match-clause
        ((pattern value))
        (and)
        ()
        expressions
        (match-evaluated value . clauses)
        #t))))

(define-syntax match-clause
  (syntax-rules (unquote)
    #;(match-clause pairs condition bindings expressions alternative predicate)
    ((_ () condition bindings expressions alternative #t)
     (if condition
          (let bindings . expressions)
          alternative))
    ((_ () condition bindings expressions alternative predicate)
     (let ([alter (lambda () alternative)])
        (if condition
          (let bindings (if predicate (begin . expressions) (alter)))
          (alter))))
    ;;; underscore is reserved for syntax-rules
    ;;; underscore cannot be used as empty pattern
    ;;; instead, any non-identifier can be used. ,() is a great choice
    ((_ ((,() root) . rest) condition bindings expressions alternative predicate)
     (match-clause 
      rest
      condition
      bindings
      expressions
      alternative
      predicate))
    ((_ ((,variable root) . rest) condition bindings expressions alternative predicate)
     (match-clause 
      rest
      condition
      ((variable root) . bindings)
      expressions
      alternative
      predicate))
    ((_ (((left . right) root) . rest) (and condition ...) bindings expressions alternative predicate)
     (match-clause 
      ((left (car root)) (right (cdr root)) . rest)
      (and condition ... (pair? root))
      bindings
      expressions
      alternative
      predicate))
    ((_ ((literal root) . rest) (and condition ...) bindings expressions alternative predicate)
     (match-clause 
      rest
      (and condition ... (equal? (quote literal) root))
      bindings
      expressions
      alternative
      predicate))))

;;; match-defmacro
(define (match-expression? e) (and (pair? e) (eq? (car e) 'match)))
(define (match-input-expr e) (cadr e))
(define (match-clauses e) (cddr e))
(define (first-clause cs) (car cs))
(define (guard-clause? c)
  (and (pair? (cadr c)) (eq? (car (cadr c)) 'guard)))
(define (guard-clause-expr c)
  (if (guard-clause? c)
      (cadadr c)
      #t))
(define (rest-clause cs) (cdr cs))
(define (clause-pattern c) (car c))
(define (clause-expr c)
  (if (guard-clause? c)
      (cddr c)
      (cdr c)))
(define (unquoted-empty? e)
  (and (pair? e) (eq? (car e) 'unquote) (equal? (cdr e) '(()))))
(define (empty-pat? e) (or (unquoted-empty? e) (eq? e ',_)))
(define (var-pat? e) 
  (and (pair? e) (eq? (car e) 'unquote) (symbol? (cadr e))))
(define (var-pat e) (cadr e))
(define (empty-list-pat? e)
  (and (pair? e) (eq? (car e) 'quote) (null? (cadr e))))
(define (datum-pattern? pattern)
  (or (null? pattern) (symbol? pattern) (not (pair? pattern))))
(define (datum-pattern pattern)
  (if (or (null? pattern) (symbol? pattern))
      (list 'quote pattern)
      pattern))
(define (compile-match p)
  (let ((evaluated (gensym)))
    `(let ([,evaluated ,(match-input-expr p)])
      ,(compile-clauses evaluated (match-clauses p)))))
(define (compile-clauses value clauses)
  (if (null? clauses)
      `'(error 'no-matching-pattern ,value)
      (compile-clause
        (list (list (clause-pattern (first-clause clauses)) value))
        '(and)
        '()
        (clause-expr (first-clause clauses))
        (compile-clauses value (rest-clause clauses))
        (guard-clause-expr (first-clause clauses)))))

(define (compile-clause pat-val-pairs condition bindings conseq alter guard-pred)
  (cond
    [(null? pat-val-pairs)
      (if (eq? guard-pred #t)
          `(if ,condition (let ,bindings . ,conseq) ,alter)
          (let ((alter-f (gensym)))
            `(let ((,alter-f (lambda () ,alter)))
              (if ,condition
                  (let ,bindings 
                    (if ,guard-pred 
                        (begin . ,conseq)
                        (,alter-f)))
                  (,alter-f))))
          )]
    [(empty-pat? (caar pat-val-pairs))
     (compile-clause (cdr pat-val-pairs)
                     condition
                     bindings
                     conseq
                     alter
                     guard-pred)]
    [(var-pat? (caar pat-val-pairs))
     (compile-clause (cdr pat-val-pairs)
                     condition
                     (cons (list (var-pat (caar pat-val-pairs)) (cadar pat-val-pairs)) bindings)
                     conseq
                     alter
                     guard-pred)]
    [(datum-pattern? (caar pat-val-pairs))
     (compile-clause (cdr pat-val-pairs)
                     (append condition 
                             `((equal? ,(datum-pattern (caar pat-val-pairs)) ,(cadar pat-val-pairs))))
                     bindings
                     conseq
                     alter
                     guard-pred)]
    [(pair? (caar pat-val-pairs))
     (let* 
      ([first (car pat-val-pairs)]
       [rest (cdr pat-val-pairs)]
       [pat (car first)]
       [val (cadr first)]
       [left (list (car pat) (list 'car val))]
       [right (list (cdr pat) (list 'cdr val))])
      (compile-clause (append (list left right) rest)
                      (append condition `((pair? ,val)))
                      bindings
                      conseq
                      alter
                      guard-pred))]
    [else (error compile-clause '())]))
