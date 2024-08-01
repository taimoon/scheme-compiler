(pretty-print (string? ""))
(pretty-print "compile-and-run")
(let ([s "compile-and-run"])
    (string-set! s 0 #\C)
    (string-set! s (+ 2 1) #\P)
    (pretty-print s))
(pretty-print
  (string-length "compile-and-run")
)
(pretty-print
  (string-ref "abcd" 3)
)