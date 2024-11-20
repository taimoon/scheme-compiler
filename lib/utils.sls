(library
  (utils)
  (export
    make-env
    extend-env
    maybe-apply-env
    apply-env
    
    read-sexps-from-path
    align-to-multiple
    make-lcg
    system*

    random-string
    symbol->id-symbol
    string->id-string
    generate-label

    path-fileroot
    path-filename
    path-extension)
  (import
    (rnrs (6))
    (only (chezscheme)
      include add1 sub1 system format
      string-set! ash get-process-id iota))
  (include "utils.scm"))
