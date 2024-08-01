(define tests (list
  "test-unary"
  "test-binary"
  "test-let"
  "test-if"
  "test-cons"
  "test-string"
  "test-vector"
  "test-assignment"
  "test-label"
  "test-lambda"
  "test-closure"
  "test-clos-assign"
  "test-letrec"
  "test-top-definition"
  "test-variadic"
  "test-quotation"
  "test-primitive-proc"
  "test-apply"
))

(define lib-tests (list
  "test-ffi"
  "test-symbol"
  "test-top-lib"
  "test-port"
  "test-qq"
  "test-match"
  "test-reader"
  "test-more-primitive"
))

(define gc-tests (list
  "test-simple-gc-1"
  "test-simple-gc-2"
  "test-simple-gc-3"
  "test-simple-gc-4"
  "test-simple-gc-5"
))

(define (run-a-test compile-file f)
  (pretty-print (format "./test/~a.scm" f))
  (compile-file (format "./test/~a.scm" f) "./a.out")
  (system (format "./a.out > ./test/~a-out.txt" f))
  (system (format "scheme --script ./test/~a.scm > ./test/~a.txt" f f))
  (let ([res (system (format "diff ./test/~a-out.txt ./test/~a.txt" f f))])
    (if (eq? res 0)
        (system (format "rm -f ./test/~a-out.txt" f))
        (exit)))
  )
(define (test-parser)
  (let ([f "test-parser"])
    (pretty-print (format "./test/~a.scm" f))
    (system (format "scheme --script ./test/~a.scm > ./test/~a-out.txt" f f))
    (let ([res (system (format "diff ./test/~a-out.txt ./test/~a.txt" f f))])
      (if (eq? res 0)
          (system (format "rm -f ./test/~a-out.txt" f))
          (exit)))))

(define (test-cmd-ln compile-file)
  (let ([f "test-cmd-ln"])
    (pretty-print (format "./test/~a.scm" f))
    (compile-file (format "./test/~a.scm" f) "./a.out")
    (system (format "./a.out hakurei-reimu kirisame-marisa > ./test/~a-out.txt" f))
    (system (format "scheme --script ./test/~a.scm hakurei-reimu kirisame-marisa > ./test/~a.txt" f f))
    (let ([res (system "diff ./test/test-cmd-ln-out.txt ./test/test-cmd-ln.txt")])
      (if (eq? res 0)
          (system (format "rm -f ./test/~a-out.txt" f))
          (exit)))
    ))

(define (test-proc-check compile-file)
  (let ([f "test-proc-check"])
    (pretty-print (format "./test/~a.scm" f))
    (compile-file (format "./test/~a.scm" f) "./a.out")
    (system (format "./a.out 1> ./test/~a-out.txt" f))
    (system (format "./a.out 2>> ./test/~a-out.txt" f))
    (system (format "scheme --script ./test/~a.scm 1> ./test/~a.txt" f f))
    (system (format "scheme --script ./test/~a.scm 2>> ./test/~a.txt" f f))
    (let ([res (system (format "diff ./test/~a-out.txt ./test/~a.txt" f f))])
      (if (eq? res 0)
          (system (format "rm -f ./test/~a-out.txt" f))
          (exit)))))

(test-parser)
(system "scheme --script compiler-entry.scm recompile-runtime")

(define matryoshka 0)
(let ([args (command-line-arguments)])
  (if (pair? args)
      (set! matryoshka (string->number (car args) 10))
      #t))

(define (run-tests compile-file compile-file-w/o-lib gc-test?)
  (let ([run-a-test* (lambda (f) (run-a-test compile-file f))]
        [run-a-test** (lambda (f) (run-a-test compile-file-w/o-lib f))])
    (for-each run-a-test** tests)
    (run-a-test** "test-tail-call")
    (for-each run-a-test* lib-tests)
    ; (test-proc-check compile-file)
    (if gc-test?
        (begin
          (for-each run-a-test* gc-tests)
          (run-a-test* "test-auto-gc")))
    (test-cmd-ln compile-file)))

(define (test-comp)
  (system "scheme --script compiler-entry.scm recompile-top-prog")
  (run-tests
    (lambda (inp out)
      (system (format "scheme --script compiler-entry.scm ~a ~a link-with-top-prog" inp out)))
    (lambda (inp out)
      (system (format "scheme --script compiler-entry.scm ~a ~a link-with-top-prog" inp out)))
    #t))

(define (run-matryoshka-test n)
  (define compile-file
    (if (<= n 0)
        (lambda (inp out) (system (format "./compiler-~a.out ~a ~a link-with-top-prog" 0 inp out)))
        (lambda (inp out) (system (format "./compiler-~a.out ~a ~a link-with-top-prog" n inp out)))))
  (set! n (if (<= n 0) 0 n))
  (display (format "=====test-compiler-~a=====\n" n))
  (system (format "scheme --script make-compiler.scm ~a" n))
  (system (format "./compiler-~a.out recompile-top-prog" n))
  (run-tests compile-file compile-file #f))

(system "rm -f compiler-*.out")

(begin
(if (or (= matryoshka -1) (= matryoshka 0))
    (test-comp))
(if (or (= matryoshka -1) (= matryoshka 1))
    (run-matryoshka-test 0))
(if (or (= matryoshka -1) (= matryoshka 2))
    (run-matryoshka-test 1))
(if (or (= matryoshka -1) (= matryoshka 3))
    (run-matryoshka-test 2))
)
