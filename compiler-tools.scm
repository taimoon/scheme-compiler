(define (read-meta obj)
  (define OBJCOPY (maybe-getenv "OBJCOPY" "objcopy"))
  (let* ((tmp (make-tempname "read-meta" "txt"))
         (obj* (make-tempname "obj" "o"))
         (_ (system* "cp ~a ~a" obj obj*))
         (_ (system* "~a --dump-section .main.entry=~a ~a"
                     OBJCOPY tmp obj*))
         (inp (open-input-file tmp))
         (meta (read inp)))
    (close-port inp)
    (system* "rm -f ~a" tmp)
    (system* "rm -f ~a" obj*)
    meta))

(define (write-meta obj meta)
  (define OBJCOPY (maybe-getenv "OBJCOPY" "objcopy"))
  (system* "bash -c '~a --remove-section=.main.entry --add-section .main.entry=<(echo ~s) ~a'"
           OBJCOPY
           (format "~s" meta)
           obj))

(define (compile-prog prog out tmp)
  (define (program-main e)
    (match e
      ((program ,main . ,()) main)
      (,() (error "program-main" "unmatch" e))))
  (let* ((prog (preprocess prog))
         (ir-seq (gen-unit prog))
         (asm (make-tempname (path-filestem tmp) "s"))
         (op (open-output-file asm)))
    (emit-ir-seq op ir-seq)
    (close-port op)
    ; (emit-ir-seq (current-error-port) ir-seq)
    (system* "~a ~a -c ~a -o ~a" CC CFLAGS asm out)
    (write-meta out (list (program-main prog)))
    (system* "rm -f ~a" asm)))

(define (compile-object out inp)
  (compile-prog (cons 'begin (read-sexps-from-path inp)) out inp))

(define (concat-object object . inps)
  (define (compile-file inp)
    (cond
      ((equal? (path-extension inp) "o")
       (list inp (read-meta inp) 0))
      ((equal? (path-extension inp) "scm")
       (let ((out (make-tempname (path-filestem inp) "o")))
        (compile-object out inp)
        (list out (read-meta out) (lambda () (system* "rm -f ~a" out)))))
      (else (error "compile-file" "unknown-file-type" inp))))
  (define res (map compile-file inps))
  (define objs (map car res))
  (define entries (fold-right append '() (map cadr res)))
  (define reclaims (filter procedure? (map caddr res)))
  (system* "~a ~a -r ~a -o ~a"
           CC CFLAGS
           (apply string-append
                  (map (lambda (s) (string-append " \"" s "\" ")) objs))
           object)
  (write-meta object entries)
  (for-each (lambda (f) (f)) reclaims))

(define (compile-main out obj link-runtime?)
  (let* ((ir-seq (gen-main (read-meta obj)))
         (asm (make-tempname (path-filestem out) "s"))
         (op (open-output-file asm))
         (rts (getenv "SCM_RUNTIME"))
         (rts
          (cond
            ((not link-runtime?) #f)
            ((string? rts)
             (if (file-exists? rts)
                 rts
                 (error "compile-main" "SCM_RUNTIME runtime path doesn't exist" rts)))
            ((file-exists? "runtime.so") "runtime.so")
            ((file-exists? "runtime.o") "runtime.o")
            (else (error "compile-main" "need runtime path at SCM_RUNTIME")))))
    (emit-ir-seq op ir-seq)
    (close-port op)
    ; (emit-ir-seq (current-error-port) ir-seq)
    (if (eq? rts #f)
        (system* "~a ~a ~a ~a -r -o ~a" CC CFLAGS obj asm out)
        (system* "~a ~a ~a ~a -rdynamic ~a -o ~a" CC CFLAGS obj asm rts out))
    (system* "rm -f ~a" asm)))
