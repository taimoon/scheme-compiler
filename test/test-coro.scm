(define g (gdrop (make-range-generator 0 10 1) 3))
(define counter (make-counter))
(define (even? n) (eq? (mod n 2) 0))
(writeln (generator->list g))
(writeln (generator->list (list->generator '(1 2 3))))
(writeln (generator->list
  (gzip
    (make-counter)
    (make-range-generator 10 20 2))))
(writeln
  (generator->list
    (gmap (lambda (x) (* x x)) (make-range-generator 10))))
(writeln (generator->list counter 3))
(writeln (generator->list counter 3))
(writeln (generator->list counter 3))
(writeln (generator->list
  (gappend (make-range-generator 1 10 2)
           (make-range-generator 0 10 2))))
(writeln (generator->list (gcombine + 0 (make-counter)) 10))
(writeln (generator->list (repeat 3 3)))
(writeln
  (generator->list
    (greplicate (make-iota 4) (make-iota 4))))
(writeln (generator->list (gfilter even? (make-iota 10))))
(writeln (generator->list (gtake-while (lambda (x) (< x 5)) (make-iota 10))))
(writeln (generator->list (gdrop-while (lambda (x) (< x 5)) (make-iota 10))))
(writeln
  (generator->list (genum (gmap * (make-iota 10) (repeat 2)))))
(writeln
  (generator-fold-left * 1 (gmap add1 (make-iota 10))))
(generator-for-each
  (lambda (x y) (writeln (list x y)))
  (make-range-generator 100 110 2) (make-counter))
(writeln (generator->list (gselect (make-iota 10) (gmap even? (make-iota 10)))))
(writeln (generator->list (gtake (make-iota 3) 5 '())))
(writeln (generator->list (gtake (make-iota 3) 10)))