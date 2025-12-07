(let ((log '()))
  (define (push x)
    (set! log (cons x log)))

  (dynamic-wind
    (lambda () (push 'before))
    (lambda () (push 'during))
    (lambda () (push 'after)))

  (write (reverse log))
  (newline))
;; Expected: (before during after)

(let ((log '()))
  (define (push x)
    (set! log (cons x log)))

  (call/cc
   (lambda (k)
     (dynamic-wind
       (lambda () (push 'enter))
       (lambda ()
         (push 'body)
         (k 'escape))      ;; non-local exit
       (lambda () (push 'exit)))))

  (write (reverse log))
  (newline))
;; Expected: (enter body exit)

(let ((log '()))
  (define (push x)
    (set! log (cons x log)))

  (dynamic-wind
    (lambda () (push 'outer-enter))
    (lambda ()
      (dynamic-wind
        (lambda () (push 'inner-enter))
        (lambda () (push 'inner-body))
        (lambda () (push 'inner-exit))))
    (lambda () (push 'outer-exit)))

  (write (reverse log))
  (newline))
;; Expected:
;; (outer-enter inner-enter inner-body inner-exit outer-exit)
