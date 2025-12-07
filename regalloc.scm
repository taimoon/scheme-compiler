(define (find-smallest-color colors)
  (let iter ((i 0))
    (if (hash-table-exists? colors i)
        (iter (add1 i))
        i)))

(define (pre-color-params live-sets)
  (let recur ((xs (live-set-alives (car live-sets)))
              (i 0))
    (if (pair? xs)
        (cons (list (car xs) i) (recur (cdr xs) (add1 i)))
        '())))

;;; naive impl of linear alloc
(define (linear-alloc live-sets)
  (define id (lambda (x) x))
  (define colors (make-hash-table symbol=? symbol-hash))
  (for-each
    (lambda (v::c)
      (hash-table-intern! colors (car v::c) (lambda () (cadr v::c))))
    (pre-color-params live-sets))
  (let recur ((live-sets live-sets))
    (if (pair? live-sets)
        (match (live-set-alives (car live-sets))
          ((if ,csq-lv* ,alt-lv*)
           (recur csq-lv*)
           (recur alt-lv*)
           (recur (cdr live-sets)))
          (,lv
           (let ((v (live-set-defvar (car live-sets))))
            (if v
                (let ((color
                      (find-smallest-color
                        (fold-left
                          (lambda (ht v)
                            (let ((r (hash-table-ref colors v)))
                              (hash-table-intern! ht r (lambda () r)))
                            ht)
                          (make-hash-table = id)
                          lv))))
                  (hash-table-update! colors v id (lambda () color) (lambda (color*) (error "linear-alloc" "recoloring?" v color* color))))))
           (recur (cdr live-sets))))
        #f))
  (hash-table->alist colors))
