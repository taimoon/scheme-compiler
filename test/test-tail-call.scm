(pretty-print
  (letrec (
    [fact (lambda (n) (if (eq? n 0) 1 (* n (fact (sub1 n)))))]
    [add (lambda (n m) (if (eq? m 0) n (add (add1 n) (sub1 m))))]
  )
    (add (fact 10) (fact 10))
  ))