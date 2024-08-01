(load "utils.scm")
(load "match.scm")

(define (make-compiler* compiler-entry compiler-output-path)
  (define files (list
    "primitives"
    "top-program"
    "scheme-libs"
    "parser"
    "parser-entry"
    "utils"
    "match"
    "preprocessor"
    "compiler"
    "compiler-entry"))
  (for-each
    (lambda (f)
      (compiler-entry (format "-o ~a ~a.scm" f f)))
    files)
  (compiler-entry "recompile-top-prog")
  (compiler-entry "recompile-runtime")
  (compiler-entry
    (format "-link ~a ~a" compiler-output-path
      (fold-right (lambda (p ps) (string-append p " " ps))
                  ""
                  (map (lambda (f) (format "~a.linker" f)) files))))
  (for-each (lambda (f) (system (format "rm -f ~a.s ~a.o ~a.linker" f f f))) files)
  )

(define (meta-compiler-name x)
  (if (< x 0)
      (error "meta-compiler-name" "expect exact nonnegative integer" x)
      (format "compiler-~a.out" x)))

(define (make-n-compiler n)
  (cond [(file-exist? (meta-compiler-name n)) #t]
        [(<= n 0)
         (make-compiler*
          (lambda (args-str) (system (format "scheme --script compiler-entry.scm ~a" args-str)))
          (meta-compiler-name 0))]
        [else
          (make-n-compiler (sub1 n))
          (make-compiler*
            (lambda (args-str) (system (format "./~a ~a" (meta-compiler-name (sub1 n)) args-str)))
            (meta-compiler-name n))]
      ))

(match (command-line-arguments)
  [(,n)
   (make-n-compiler (string->number n 10))])