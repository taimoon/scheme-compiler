(import
  (only (match) match)
  (only (front) generate-primitive-top-bindings)
  (only (utils) system* path-filestem path-extension make-tempname))

(define (rider compile-prog compile-main concat-object)
  (define (common-compile-option out inps)
    (cond
      ((equal? (path-extension out) "o")
       (apply concat-object out inps))
      ((equal? (path-extension out) "out")
       (let ((tmp (make-tempname (path-filestem out) "o")))
         (apply concat-object tmp inps)
         (compile-main out tmp #t)
         (system* "rm -f ~a" tmp)))
      (else (error "compiler-rider.scm" "unknown" out))))

  (match (cdr (command-line))
    (("-o" ,out . ,inps)
     ;;; either .out or .o
     (common-compile-option out inps))
    (("-c" ,out . ,inps)
     ;;; .o and finalize the object but don't link with runtime
     (if (not (equal? (path-extension out) "o"))
         (error "compiler-rider.scm" "only object file" out))
     (let ((tmp (make-tempname (path-filestem out) "o")))
        (apply concat-object tmp inps)
        (compile-main out tmp #f)
        (system* "rm -f ~a" tmp)))
    (("--prim" ,out)
     (compile-prog (generate-primitive-top-bindings) out "prim"))
    (,inps
     (define out (make-tempname "main" "out"))
     (common-compile-option out inps)
     (system* "~a" out)
     (system* "rm -f ~a" out))
    (,cmd-ln (error "compiler" "unknown" cmd-ln))))
