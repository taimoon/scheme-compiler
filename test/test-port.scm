(load "convert-labels.scm")
(define (get-string-all port)
  (list->string (let recur [(ch (read-char port))]
    (if (eof-object? ch)
        '()
        (cons ch (recur (read-char port)))))))

(let ()
  (write-string "hakurei reimu" (current-output-port))
  (write-char #\newline (current-output-port)))

(let* ([f "./test/test-port-tmp-out.txt"]
       [p (open-input-output-file f)])
  (write-string "hakurei reimu" p)
  (write-char #\newline p)
  (file-position p 0)
  (write-string (get-string-all p) (current-output-port))
  (close-port p)
  (system (string-append "rm -f " f)))

(let ([p (open-input-file "./test/test-port.scm")])
  (write-string (get-string-all p) (current-output-port))
  (write-char #\newline (current-output-port))
  (close-port p))
