(define (tests) (list
  "test-lit"
  "test-unary"
  "test-binary-cmp"
  "test-arithmetic"
  "test-fx-0"
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
  "test-include-driver"
  '("test-lib-mut" "test-lib-mut-driver")
  '("test-lib-lib" "test-lib-driver")
  '("test-lib-free-lib" "test-lib-free-driver")
  "test-top-level-set"
  "test-symbol"
  test-ffi-io
  "test-bitwise"
  "test-vararg"
  "test-dotted-app"
  "test-simple-gc-0"
  "test-simple-gc-1"
  "test-simple-gc-2"
  "test-simple-gc-3"
  "test-apply"
  "test-tail-apply"
  "test-case-lambda-0"
  "test-case-lambda-1"
  "test-case-lambda-2"
  "test-case-lambda-3"
  test-command-line
  "test-operand-prim"
  "test-quasiquote"
  "test-match"
  "test-reader"
  test-coro
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
    (format "-o /tmp/scm-build-~a.out lib.o ~a" 
            res-filename
            (apply string-append
                   (map (lambda (s) (string-append " \"" s "\" ")) inputs))))
  (system* "git diff test/~a.txt <(/tmp/scm-build-~a.out)"
           res-filename res-filename))

(define (run-a-file compile f)
  (display (format "test/~a.scm\n" f))
  (compile (format "-o /tmp/scm-build-~a.out lib.o test/~a.scm" f f))
  (system* "git diff test/~a.txt <(/tmp/scm-build-~a.out)" f f))

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

(define (test-coro compile)
  (define filename "test/test-coro")
  (display (format "~a.scm" filename)) (newline)
  (compile
    (format "-o ~a.out lib.o lib/coro.scm ~a.scm" filename filename))
  (system* "diff ~a.txt <(./~a.out)" filename filename)
  )

(define (run-all-tests compiler)
  (display "====compiling library====\n")
  (compiler "--make-prim-lib prim.scm")
  (compiler "-o lib.o intern.scm kernel.scm prim.scm lib/scheme-libs.scm lib/writer.scm lib/reader.scm")
  (display "====done compiling library====\n")
  (for-each
    (lambda (f)
      (cond
        ((pair? f) (apply run-multi-files (cons compiler f)))
        ((procedure? f) (f compiler))
        (else (run-a-file compiler f))))
    (tests))
  (system*
    (string-append
      "rm -f prim.scm &"
      "rm -f test/*.out &"
      "rm -f /tmp/scm-build-*.out")))

(define (boostrap-test comp-name)
  (format #t "====~s====\n" comp-name)
  (run-all-tests (lambda (cmd) (system* "~a ~a" comp-name cmd)))

  (display "====\"compiler-0.out ~a\"====\n")
  (system* "time (make clean; make new-compiler SCM_CC=~a SCM_NCC=compiler-0.out -j)" comp-name)
  (run-all-tests (lambda (cmd) (system* "./compiler-0.out ~a" cmd)))

  (display "====\"compiler-1.out ~a\"====\n")
  (system* "time (make clean; make new-compiler SCM_CC=~a SCM_NCC=compiler-1.out -j)" "./compiler-0.out")
  (display "====done bootstrap compiler-1.out from compiler-0.out====\n")
  (run-all-tests (lambda (cmd) (system* "./compiler-1.out ~a" cmd))))

(import (match))
(match (cdr (command-line))
  ((,bootstrapper-cmd ,comp-name ,comp-cmd)
   (format #t "===bootstrapping===\n")
   (format #t "~a\n" bootstrapper-cmd)
   (system* "./make-compiler.out ~s ~s" bootstrapper-cmd comp-name)
   (format #t "===done bootstrapping===\n")
   (run-all-tests (lambda (cmd) (system* comp-cmd cmd))))
  ((,comp-name "--bootstrap")
   (boostrap-test comp-name))
  (("--chez")
   (system "scheme --script make-compiler-chez.scm")
   (run-all-tests (lambda (cmd) (system* "scheme --script compiler.so ~a" cmd)))
   (system* "make clean; make new-compiler SCM_CC=~s SCM_NCC=compiler-new.out -j" "scheme --script compiler.so")
   (boostrap-test "./compiler-new.out"))
  ((,comp-name)
   (format #t "===~a===\n" comp-name)
   (run-all-tests (lambda (cmd) (system* "~a ~a" comp-name cmd))))
  (,cmd-ln
   (format #t "unknown argument, procceed to default\n")
   (boostrap-test "./compiler.out")))