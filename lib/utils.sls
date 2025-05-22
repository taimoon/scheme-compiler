(library
  (utils)
  (export
    improper-list?
    improper->proper
    writeln
    make-env
    extend-env
    extend-env*
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
    path-extension
    count-cons
    rpad
    measure-pass
    lsort
    partition
    zip-k
    mk-tmpname)
  (import
    (except (rnrs (6)) partition)
    (only (chezscheme)
      include add1 sub1 system format gensym
      string-set! ash get-process-id iota))
  (include "utils.scm"))
