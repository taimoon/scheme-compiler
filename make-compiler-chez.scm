(current-directory "./lib")
(compile-library "match.sls" "match.so")
(compile-library "utils.sls" "utils.so")
(compile-library "set.sls" "set.so")
(concatenate-object-files "lib.so" "match.so" "utils.so" "set.so")
(current-directory "..")
(define rider (cadr (command-line)))
(define backend (caddr (command-line)))
(make-boot-file "compiler.so"
  '("scheme" "petite")
  "lib/lib.so"
  "front.sls"
  backend
  rider)
(system "rm -f lib/*.so")