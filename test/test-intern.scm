(let ((s ((label _s_str2sym) "list")))
  (%symbol-value-set! s (lambda x x))
  (write ((%symbol-value s) 1 2 3))
  (newline))
