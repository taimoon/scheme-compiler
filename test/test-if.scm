(pretty-print (if (and (integer? 0) (zero? 0)) #\0 #\1))
(pretty-print (if (and (integer? #\0) (zero? #\0)) #\0 #\1))
(pretty-print (if (< -2 -3) #\L #\s))
(pretty-print (if (<= -3 -2) #\L #\s))
(pretty-print
  (cond [(eq? #\t #t) 6]
         [(eq? #f #\f) 4]
         [else 7]))