(define cxrs '(
  caar
  caaar
  caaaar
  cdaaar
  cdaar
  cadaar
  cddaar
  cdar
  cadar
  caadar
  cdadar
  cddar
  caddar
  cdddar
  cadr
  caadr
  caaadr
  cdaadr
  cdadr
  cadadr
  cddadr
  cddr
  caddr
  caaddr
  cdaddr
  cdddr
  cadddr
  cddddr
))

(define (parse-cxr cxr kons knil)
  (define s (symbol->string cxr))
  (let recur ((i 1))
    (cond
      ((>= i (string-length s)) (error "parse-cxr" "bad-cxr" cxr))
      ((char=? (string-ref s i) #\r) knil)
      ((char=? (string-ref s i) #\a)
       (kons #t (recur (add1 i))))
      ((char=? (string-ref s i) #\d)
       (kons #f (recur (add1 i))))
      (else (error "parse-cxr" "bad-cxr" cxr)))))

(define (cxr-builder-maker kar kdr)
  (define kar* (lambda (f) (lambda (e) (kar (f e)))))
  (define kdr* (lambda (f) (lambda (e) (kdr (f e)))))
  (map (lambda (cxr)
        (parse-cxr cxr
          (lambda (sel f) ((if sel kar* kdr*) f))
          (lambda (v) v)))
       cxrs))

(define (cxr-test-inputs)
  (map (lambda (cxr)
        (fold-left
          (lambda (e i)
            (if i
                `(cons ,e #t)
                `(cons #t ,e)))
          #f
          (parse-cxr cxr cons '())))
       cxrs))
