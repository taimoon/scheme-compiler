(current-directory "./lib")
(compile-library "match.sls" "match.so")
(compile-library "utils.sls" "utils.so")
(compile-library "set.sls" "set.so")
(concatenate-object-files "lib.so" "match.so" "utils.so" "set.so")
(current-directory "..")
(make-boot-file "compiler.so"
  '("scheme" "petite")
  "lib/lib.so"
  "desugar.sls"
  "front.sls"
  "compiler.scm")
(system "rm -f lib/*.so")