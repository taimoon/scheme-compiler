(let ()
  (define (%sum n)
    (if (<= n 0)
        0
        (+ n (%sum (sub1 n)))))
  (write (%sum 13)) (newline)
  (set! sum %sum)
  (write (sum 13)) (newline)
)

(write (sum 13)) (newline)