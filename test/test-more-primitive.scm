(define fixnum-shift 2)

(define char-shift 8)
(define char-tag 7)

(define bool-mask 255)
(define bool-shift 8)
(define bool-tag 15)
(define bool-false-imm bool-tag)
(define bool-true-imm (logior (ash 1 bool-shift) bool-tag))


(define (immediate-rep x)
  (cond
    [(integer? x) (ash x fixnum-shift)]
    [(char? x) (logior (ash (char->integer x) char-shift) char-tag)]
    [(boolean? x) (if x bool-true-imm bool-false-imm)]
    [else (error "immediate-rep" "unknown-immediate" x)]))

(pretty-print (immediate-rep 123))
(pretty-print (immediate-rep -123))

(pretty-print (immediate-rep #t))
(pretty-print (immediate-rep #f))
(pretty-print (immediate-rep #\t))
(pretty-print (immediate-rep #\f))