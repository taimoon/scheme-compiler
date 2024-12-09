(define (tests) (list
  "test-lit"
  "test-unary"
  "test-binary-cmp"
  "test-arithmetic"
  "test-let"
  "test-if"
  "test-logic-conn"
  "test-cond"
  "test-pair"
  "test-string"
  "test-vector"
  "test-simple-procedure"
  "test-simple-rec-proc"
  "test-mutual-procedure"
  "test-procedure-ref"
  "test-lambda"
  "test-lambda-1"
  "test-tail-sum"
  "test-tail-add"
  "test-simple-set"
  "test-free-var-set"
  "test-letrec"
  "test-named-let"
  "test-simple-define"
  '("test-lib-lib" "test-lib-driver")
  '("test-lib-free-lib" "test-lib-free-driver")
  "test-symbol"
  test-ffi-io
  "test-bitwise"
  "test-vararg"
  "test-apply"
  "test-tail-apply"
  "test-simple-gc-0"
  "test-simple-gc-1"
  "test-simple-gc-2"
  "test-simple-gc-3"
  "test-case-lambda-0"
  "test-case-lambda-1"
  "test-case-lambda-2"
  test-command-line
  "test-operand-prim"
  "test-quasiquote"
  "test-match"
  "test-reader"
  ))

(define (system* cmd  . args)
  (let* ((cmd (apply format (cons cmd args)))
         (res (system cmd)))
    (if (eq? res 0)
        #t
        (exit res))))

(define (last xs)
  (cond
    ((null? xs) xs)
    ((and (pair? xs) (null? (cdr xs)))
     (car xs))
    (else (last (cdr xs)))))

(define (run-multi-files compile . filenames)
  (define inputs (map (lambda (f) (format "test/~a.scm" f)) filenames))
  (define res-filename (last filenames))
    (display (format "test/~a.scm\n" res-filename))
  ;;; test -o option is working
  (compile
    (format "-o /tmp/scm-build/~a.out lib.o ~a" 
            res-filename
            (apply string-append
                   (map (lambda (s) (string-append " \"" s "\" ")) inputs))))
  (system* "git diff test/~a.txt <(/tmp/scm-build/~a.out)"
           res-filename res-filename))

(define (run-a-file compile f)
  (display (format "test/~a.scm\n" f))
  (compile (format "-o /tmp/scm-build/~a.out lib.o test/~a.scm" f f))
  (system* "git diff test/~a.txt <(/tmp/scm-build/~a.out)" f f))

(define (test-ffi-io compile)
  (display "test/test-ffi-io.scm\n")
  (compile
    "-o test/test-ffi-io.out lib.o test/test-ffi-io.scm")
  (system* "diff test/test-ffi-io.scm <(test/test-ffi-io.out)")
  (system* "diff test/test-ffi-io.scm test/test-ffi-io.txt"))

(define (test-command-line compile)
  (define filename "test/test-command-line")
  (display (format "~a.scm" filename)) (newline)
  (compile
    (format "-o ~a.out lib.o ~a.scm" filename filename))
  (system* "diff ~a.txt <(./~a.out reimu marisa -19 2 3 5)" filename filename))

(define (compiler-0 cmd)
  (system* "scheme --script compiler.so ~a" cmd))

(define (compiler-1 cmd)
  (system* "./compiler.out ~a" cmd))

(define (compiler-2 cmd)
  (system* "./compiler-1.out ~a" cmd))

(define (run-all-tests compiler)
  (display "====compiling library====\n")
  (compiler "--make-prim-lib primitives.scm")
  (compiler "--combine lib.o intern.scm kernel.scm primitives.scm lib/scheme-libs.scm lib/writer.scm lib/reader.scm")
  (display "====done compiling library====\n")
  (system* "mkdir -p /tmp/scm-build")
  (for-each
    (lambda (f)
      (cond
        ((pair? f) (apply run-multi-files (cons compiler f)))
        ((procedure? f) (f compiler))
        (else (run-a-file compiler f))))
    (tests))
  (system* "rm -f primitives.scm")
  (system* "rm -f /tmp/scm-build/*")
  (system* "rm -f test/*.out"))

(import (match))
(match (cdr (command-line))
  ((,bootstrapper-cmd ,comp-name ,comp-cmd)
   (format #t "===bootstrapping===\n")
   (format #t "~a\n" bootstrapper-cmd)
   (system* "scheme --script make-compiler.scm ~s ~s" bootstrapper-cmd comp-name)
   (format #t "===done bootstrapping===\n")
   (run-all-tests (lambda (cmd) (system (format comp-cmd cmd)))))
  ((,comp-cmd)
   (format #t "===~a===" comp-cmd)
   (run-all-tests (lambda (cmd) (system (format comp-cmd cmd)))))
  (,cmd-ln
   (format #t "unknown argument, procceed to default\n")
   (display "====compiler.so====\n")
   (system* "scheme --script make-compiler.scm")
   (run-all-tests (lambda (cmd) (system (format "scheme --script compiler.so ~a" cmd))))

   (display "====compiler.out====\n")
   (system* "scheme --script make-compiler.scm \"scheme --script compiler.so ~~a\" compiler.out")
   (display "====done bootstrap compiler.out from compiler.so====\n")
   (run-all-tests (lambda (cmd) (system (format "./compiler.out ~a" cmd))))

   (display "====compiler-1.out====\n")
   (system* "scheme --script make-compiler.scm \"./compiler.out ~~a\" compiler-1.out")
   (display "====done bootstrap compiler-1.out from compiler.out====\n")
   (run-all-tests (lambda (cmd) (system (format "./compiler-1.out ~a" cmd))))

   (display "====compiler-2.out====\n")
   (system* "scheme --script make-compiler.scm \"./compiler-1.out ~~a\" compiler-2.out")
   (display "====done bootstrap compiler-2.out from compiler-1.out====\n")
   (run-all-tests (lambda (cmd) (system (format "./compiler-2.out ~a" cmd))))))