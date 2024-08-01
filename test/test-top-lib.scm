(load "convert-labels.scm")

(pretty-print
  (map (lambda (x) (* x x)) (list 2 3 5 7 11)))

(pretty-print
  (vector-map (lambda (x) (* x x))
    (vector 2 3 5 7 11))
)


(vector-for-each pretty-print
  (vector-map make-vector
  (vector 2 3 5 7 11)))


(pretty-print
  (list->string
    (map integer->char (map (lambda (x) (+ (char->integer #\a) x)) (iota 26))))
)

(pretty-print
  (string-append "something " "weird"))


(pretty-print
  (let* ([s "bad-apple"])
    (string-fill! s (string-ref s 0))
    s))

(pretty-print
  (vector-append
    (vector 1 2 3)
    (vector #t #f #\t #\f)))

(pretty-print
  (let ([vs (list->vector (iota 10))]
        [sqr (lambda (x) (* x x))])
    (vector-map! sqr vs)
    vs
    ))

(let ([vs (list->vector (iota 5))]
        [sqr (lambda (x) (* x x))])
    (vector-fill! vs (vector-copy vs))
  (vector-for-each pretty-print vs))
  
(pretty-print
  (integer->string 3824))

(pretty-print
  (let loop ([xs (map (lambda (x) (gensym "r")) (iota 10))])
    (if (and (pair? xs) (pair? (cdr xs)))
        (and (not (eq? (car xs) (cadr xs))) (loop (cdr xs)))
        #t)))
