;;; match main program
(define-syntax match
  (syntax-rules ()
    ((_ exp . clauses)
     (let ((evaluated exp))
       (match-evaluated evaluated clauses)))))

(define-syntax match-evaluated
  (syntax-rules (guard)
    ((_ val ())
     (error 'match 'unmatch val))
    ((_ val ((pat (guard pred) . exp) . clauses))
     (let ((alter (lambda () (match-evaluated val clauses))))
      (if (match? val pat)
          (pattern->bindings val pat (if pred (let () . exp) (alter)))
          (alter))))
    ((_ val ((pat . exp) . clauses))
     (if (match? val pat)
         (pattern->bindings val pat (let () . exp))
         (match-evaluated val clauses)))))

(define-syntax match?
  (syntax-rules (unquote unquote-splicing)
    ((_ val ()) (null? val))
    ((_ val ,_) #t)
    ((_ val (hd . tl))
     (and (pair? val)
          (match? (car val) hd)
          (match? (cdr val) tl)))
    ((_ val lit) (equal? val (quote lit)))))

(define-syntax pattern->bindings
  (syntax-rules (unquote unquote-splicing)
    ((_ val ,() conseq) conseq)
    ((_ val ,var conseq) (let ((var val)) conseq))
    ((_ val (hd . tl) conseq)
     (pattern->bindings
        (car val)
        hd
        (pattern->bindings (cdr val) tl conseq)))
    ((_ val lit conseq) conseq)))