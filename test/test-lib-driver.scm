(write "start") (newline)
(write-line IMM-SHIFT)
(set! IMM-SHIFT 123)
(write-line IMM-SHIFT)
(write-line
  (and (even? 2) (odd? 3) (odd? 5)))