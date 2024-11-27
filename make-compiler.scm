(import (match))
(let ()
(define (make-compiler-from-chez)
  (define (string-split s sep)
    (let recur ((i 0) (j 0))
      (cond
        ((>= i (string-length s))
        (list (substring s j i)))
        ((eq? (string-ref s i) sep)
        (cons (substring s j i)
              (recur (add1 i) (add1 i))))
        (else (recur (add1 i) j)))))
  (system "gcc -fno-omit-frame-pointer -m32 runtime.c -c -o runtime.o")

  (system "rm -f compiler.so")
  (system (format #f "cp -rf -T lib/ build/"))
  (system (format #f "cp front.sls front.scm build/"))
  (system (format #f "cp compiler.scm build/"))

  (current-directory "build/")
  (make-boot-file "compiler.so"
    '("scheme" "petite")
    "set.sls"
    "match.sls"
    "utils.sls"
    "front.sls"
    "compiler.scm")
  (system "mv compiler.so ../")
  (current-directory "..")
  (system "rm -rf build/")
  )

(define (make-meta-compiler compile out)
  (system "gcc -fno-omit-frame-pointer -m32 runtime.c -c -o runtime.o")
  (for-each
    compile
    (list
      "--make-prim-lib primitives.scm"
      "--combine lib.o intern.scm kernel.scm primitives.scm lib/scheme-libs.scm lib/writer.scm lib/reader.scm"
      "--combine cplib.o lib/match-defmacro.scm lib/set.scm lib/utils.scm"
      "--combine front.o front.scm"
      "--combine compiler.o compiler.scm"
      (format "-o ~a lib.o cplib.o front.o compiler.o" out)))
  (system "rm -f cplib.o front.o compiler.o primitives.scm")
  )

(match (cdr (command-line))
  ((,compiler ,out)
    (make-meta-compiler
      (lambda (cmd) (system (format compiler cmd)))
      out))
  (,() (make-compiler-from-chez)))
)