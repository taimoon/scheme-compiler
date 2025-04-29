(define string->symbol
  (let ((SYMBOL-STR-TABLE '())
        (interned-size 0))
    (define (string=? s1 s2)
      (or (eq? s1 s2)
          (and (string? s1) (string? s2)
                (= (string-length s1) (string-length s2))
            (let loop ((i 0))
              (cond
                ((eq? i (string-length s1))
                  #t)
                ((eq? (string-ref s1 i) (string-ref s2 i))
                  (loop (add1 i)))
                (else #f))))))
    (define (string->symbol str)
      (let loop ((xs SYMBOL-STR-TABLE))
        (cond
          ((not (pair? xs))
          (set! interned-size (add1 interned-size))
          (set! SYMBOL-STR-TABLE (cons str SYMBOL-STR-TABLE))
          (%string->symbol str))
          ((string=? (car xs) str)
          (%string->symbol (car xs)))
          (else
          (loop (cdr xs))))))
    string->symbol))

(define symbol->string
  (let ()
    (define (string-copy s)
      (let ((s* (make-string (string-length s) #\nul)))
        (let loop ((i 0))
          (if (eq? (string-length s) i)
              s*
              (begin
                (string-set! s* i (string-ref s i))
                (loop (add1 i)))))))
    
    (define (symbol->string sym)
      (string-copy (%symbol->string sym)))
    symbol->string))