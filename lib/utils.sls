(library
  (utils)
  (export
    align-to-multiple
    gensym*
    make-env
    extend-env
    extend-env*
    maybe-apply-env
    apply-env
    apply-env*
    improper-list?
    improper->proper
    flatmap
    zip
    applicate
    path-parent
    path-filename
    path-extension
    path-filestem
    read-sexps-from-path
    make-tempname
    system*
    maybe-getenv
    )
  (import
    (rnrs (6))
    (only (chezscheme)
      get-process-id system getenv
      include format ash string-set! gensym
      andmap
      sub1 add1))
  (include "utils.scm"))
