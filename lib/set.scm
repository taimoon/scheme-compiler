(define (list->set xs)
  (fold-right
    (lambda (x ss) (if (member x ss) ss (cons x ss)))
    '()
    xs))

(define (make-set . xs)
  (list->set xs))

(define (set-empty? s)
  (eq? s '()))

(define (set-adjoin x xs)
  (if (member x xs)
      xs
      (cons x xs)))

(define (set-union . xss)
  (define (set-union xs ys)
    (fold-right set-adjoin xs ys))
  (fold-right set-union (make-set) xss))

(define (set-diff xs ys)
  (filter (lambda (x) (not (member x ys))) xs))

(define (set-intersection xs ys)
  (set-diff ys (set-diff ys xs)))

(define (set-symmetric-diff xs ys)
  (set-union (set-diff xs ys) (set-diff ys xs)))

(define (set-equal? xs ys)
  (set-empty? (set-symmetric-diff xs ys)))

(define (subset>=? xs ys)
  ; subset and greater than share the same partial order properties
  (set-empty? (set-diff xs ys)))

(define (subset<=? xs ys)
  (subset>=? ys xs))

(define (combs xs)
  (if (pair? xs)
      (let ((acm (combs (cdr xs))))        
        (append acm
                (map (lambda (x) (cons (car xs) x))
                     acm)))
      (list (list))))

(define (powerset xs)
  (list->set (map list->set (combs (list->set xs)))))
