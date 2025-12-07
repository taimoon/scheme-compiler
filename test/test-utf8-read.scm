(define (puts ip)
  (let loop ((ch (%read-char ip)))
    (if (not (eof-object? ch))
        (begin
          (write-char ch (current-output-port))
          (loop (%read-char ip))))))
(define fp (open-input-file "test/始计篇.txt"))
(puts fp)
(close-port fp)
