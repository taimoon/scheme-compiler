#;'(REGISTERS AND RUNTIMES
  (emit "movl label,%eax")  ; deref and get the value
  (emit "movl $label,%eax") ; get the address of value
  (caller-save eax ecx edx)
  (callee-save ebp ebx edi esi esp)
  (invariant
    (register ebp points to old-ebp)
    (4(%ebp) always points to return-address)
    (return-address is read-only))
  
  ; TCO will no work for greater arities function under cdecl convention
  ; because you cannot overwrite the return address
  ; in short, cdecl is not compatible with TCO for stack-based calling convention
  ; therefore, a custom calling convention is adopted for this implementation
  ; where arguments are pushed after ebp (i.e.: equivalent to 0 argument in cdecl)
  #;((4 ret-addr)
     (0(ebp) old-ebp) <--- esp
     (-4 arg-N) 
     ... 
     (-4*(n-k) arg-k)
     ...
     (-4*n arg-0)
     locals ...)
  (x86
    (
    (4*n+8 argument-N)
    ...
    (16 argument-1)
    (8  argument-0)
    (4  ret-addr)
    (0(ebp)     old-ebp)        <-- ebp
    (-4         callee-save-0)  <-- esp*
    ...
    (-4*(n+1)   callee-save-n)
    (-4*(n+2)   local-var-0)
    ...
    (-4*(n+m+2) local-var-m)    <-- esp
    ))
  (x86
    (
    (4*n+8 argument-N)
    ...
    (16 argument-1)
    (8  argument-0)
    (4  ret-addr)
    (0(ebp)     old-ebp)      <-- ebp
    (-4 edi)
    (-8 esi)
    (-16 ebx)
    (-4*(n+5) local-var-0)
    ...
    (-4*(n+5) local-var-m)    <--- esp
    ))
  (TCC*
    ((-si ret-addr)
     (-si-1*wordsize old-ebp)
     (-si-2*wordsize arg-0)
     ...
     (-si-(1+i)*wordsize arg-i)
     ...
     (-si-(1+n)*wordsize eval-si))
    ((4 ret-addr)
     (0(ebp) old-ebp) <--- esp
     (-4 arg-0)
     ... 
     (-4*(k+1) arg-k)
     ...
     (-4*(n+1) si)))
  (eax "accumulator register" "return value")
  (ecx "counter register" "store argument counts")
  (edx "data register" "dividend register")
  (ebx "base register" "local register")
  (esp "stack pointer register")
  (ebp "stack base pointer register" "stack frame pointer")
  (edi "destination index register" "local register")
  (esi "source index register" "local register")
  (eip "instruction pointer register")
  (free ... -8 -4 0(%esp) 4 8 ... used)

  (max-int on 64 bits
    (define max-int (sub1 (expt 2 (- 63 3))))
    (fixnum? max-int)
    (fixnum? (add1 max-int))
  )
)
(load "utils.scm")
(load "match.scm")
(load "preprocessor.scm")

(define wordsize 4)

(define fixnum-shift 2)
(define fixnum-mask 3)

(define char-mask 255) ; character type mask
(define char-shift 8)
(define char-tag 7)

(define bool-mask 255)
(define bool-shift 8)
(define bool-tag 15)
(define bool-false-imm bool-tag)
(define bool-true-imm (logior (ash 1 bool-shift) bool-tag))


(define ptr-mask 7) ; mask for pointer type tag
; the number is too large for the implementation
; (define ptr-mask-inv #xfffffff8) ; mask for pointer value

(define pair-tag 1)
(define vec-tag 2)
(define str-tag 3)
(define sym-tag 5)
(define closure-tag 6)

(define emit
  (lambda args
    (apply format (cons #t args))
    (newline)))

(define (immediate? x)
  (or (integer? x) (char? x) (boolean? x)))

(define (immediate-rep x)
  (cond
    [(integer? x)
     ;;; TODO: CALC THE MAX-INT AND MIN-INT
     (if (<= x #xffffff)
         (ash x fixnum-shift)
         (error "immediate-rep" "integer-too-large" x))]
    [(char? x) (logior (ash (char->integer x) char-shift) char-tag)]
    [(boolean? x) (if x bool-true-imm bool-false-imm)]
    [else (error "immediate-rep" "unknown-immediate" x)]))

(define (emit-is-eax-equal-to val)
  (emit "cmpl $~a, %eax" val)        ; check eax against val
  (emit "movl $0, %eax")             ; zero eax but leaving equal flag in place
  (emit "sete %al")                  ; set low bit of eax if they were equal
  (emit "sall $~a, %eax" bool-shift) ; shift the bit up to the bool position
  (emit "orl $~a, %eax" bool-tag))   ; add boolean type tag

(define suffix-cmpl
  '((< l)
    (<= le)
    (= e)
    (> g)
    (>= ge)))

(define (compile-gc-flip sz si)
  (emit "movl %ebp, (EBP)")
  (emit "subl $~a, %esp" (- (abs si) wordsize))
  (emit "movl %esp, (ESP)")
  (cond [(integer? sz) (emit "movl $~a, %eax" (immediate-rep sz))]
        [(eq? sz 'eax) #t]
        [else (emit "movl $~a, %eax" (immediate-rep -1))])

  (emit "pushl %eax")
  (emit "call s_gc_flip")
  (emit "popl %ecx")
  (emit "addl $~a, %esp" (- (abs si) wordsize)))

(define (compile-prim expr si env)
  (match expr
    [(procedure? ,k)
     (compile-expr k si env)
     (emit "andl $~a, %eax" ptr-mask)
     (emit-is-eax-equal-to closure-tag)]
    [(,cmp ,v ,w)
     (guard (member cmp (map car suffix-cmpl)))
     (compile-expr w si env)
     (emit "movl %eax, ~a(%esp)" si)
     (compile-expr v (- si wordsize) env)
     (emit "cmpl ~a(%esp), %eax" si)
     (emit "movl $0, %eax")
     (emit "set~a %al" (car (apply-env cmp suffix-cmpl)))
     (emit "sall $~a, %eax" bool-shift)
     (emit "orl $~a, %eax" bool-tag)]
    [(newline)
     (emit "subl $~a, %esp" (abs si))
     (emit "call s_newline")
     (emit "addl $~a, %esp" (abs si))]
    [(pretty-print ,v)
     (compile-prim `(display ,v) si env)
     (compile-prim `(newline) si env)]
    [(display ,e)
     (compile-expr e si env)
     (emit "subl $~a, %esp" (abs si))
     (emit "pushl %eax")
     (emit "call s_show")
     (emit "addl $~a, %esp" (+ (abs si) wordsize))]
    [(char->integer ,v)
     (compile-expr v si env)
     (emit "shrl $~a, %eax" (- char-shift fixnum-shift))]
    [(integer->char ,v)
     (compile-expr v si env)
     (emit "sall $~a, %eax" (- char-shift fixnum-shift))
     (emit "orl $~a, %eax" char-tag)]
    [(add1 ,v)
     (compile-expr v si env)
     (emit "addl $~a,%eax" (immediate-rep 1))]
    [(sub1 ,v)
     (compile-expr v si env)
     (emit "subl $~a,%eax" (immediate-rep 1))]
    [(char? ,v)
     (compile-expr v si env)
     (emit "andl $~a, %eax" char-mask)
     (emit-is-eax-equal-to char-tag)]
    [(integer? ,v)
     (compile-expr v si env)
     (emit "andl $~a, %eax" fixnum-mask)
     (emit-is-eax-equal-to 0)]
    [(boolean? ,v)
     (compile-expr v si env)
     (emit "andl $~a, %eax" bool-mask)
     (emit-is-eax-equal-to bool-tag)]
    [(char? ,v)
     (compile-expr v si env)
     (emit "andl $~a, %eax" char-mask)
     (emit-is-eax-equal-to char-tag)]
    [(zero? ,v)
     (compile-expr v si env)
     (emit-is-eax-equal-to 0)]
    [(eq? ,v ,w)
     (compile-expr w si env)
     (emit "movl %eax, ~a(%esp)" si)
     (compile-expr v (- si wordsize) env)
     (emit "cmpl %eax, ~a(%esp)" si)
     (emit "movl $0, %eax")
     (emit "sete %al")
     (emit "sall $~a, %eax" bool-shift)
     (emit "orl $~a, %eax" bool-tag)]
    [(vector? ,k)
     (compile-expr k si env)
     (emit "andl $~a, %eax" ptr-mask)
     (emit-is-eax-equal-to vec-tag)]
    [(vector . ,xs)
     (compile-prim (list 'make-vector (length xs)) si env)
     (emit "movl %eax, ~a(%esp)" si)
     (for-each
      (lambda (x i)
        (compile-expr x (- si wordsize) env)
        (emit "movl ~a(%esp), %ecx" si)
        (emit "movl %eax, ~a(%ecx)" (- (* wordsize (add1 i)) vec-tag))
        )
      xs
      (iota (length xs)))
     (emit "movl ~a(%esp),%eax" si)]
    [(make-vector ,k) (guard (integer? k))
     (compile-gc-flip (* wordsize (add1 k)) si)
     (emit "movl free_ptr, %eax")   ; store the pointer
     (emit "movl $~a, 0(%eax)" k)   ; store the length
     (emit "orl $~a, %eax" vec-tag) ; tag the pointer
     (emit "addl $~a, (free_ptr)" (logand -8 (+ 11 (* wordsize k)))) ; advance allocation pointer
     ]
    [(make-vector ,k)
     (compile-expr k si env)
     (emit "movl %eax, ~a(%esp)" si)
     (emit "addl $~a, %eax" (immediate-rep 1))
     (emit "imul $~a, %eax" wordsize)
     (compile-gc-flip 'eax (- si wordsize))
     (emit "movl ~a(%esp), %eax" si)

     (emit "shr $~a, %eax" fixnum-shift)  ; remove mask
     (emit "movl free_ptr, %ecx")
     (emit "movl %eax, 0(%ecx)")    ; store the length
     (emit "movl %eax, %ecx")       ; store the length again
     (emit "movl free_ptr, %eax")   ; store the pointer
     (emit "orl $~a, %eax" vec-tag) ; tag the pointer

     (emit "sall $2, %ecx")         ; multiply by 4 to get bytes 
     (emit "addl $11, %ecx")
     (emit "andl $-8, %ecx")
     (emit "addl %ecx, (free_ptr)") ; advance allocation pointer
     ]
    [(vector-ref ,v ,k)
     (guard (integer? k))
     (compile-expr v si env)
     (emit "movl ~a(%eax), %eax" (- (* (add1 k) wordsize) vec-tag))]
    [(vector-ref ,v ,k)
     (compile-expr v si env)
     (emit "movl %eax, ~a(%esp)" si)
     (compile-expr k (- si wordsize) env)
     (emit "shrl $~a, %eax" (- fixnum-shift 2)) ; fixnum->idx (*4)
     (emit "subl $~a, ~a(%esp)" vec-tag si)     ; untag
     (emit "addl $~a, ~a(%esp)" wordsize si)    ; skip the length
     (emit "addl ~a(%esp), %eax" si) ; add offset
     (emit "movl (%eax),%eax")]
    [(vector-set! ,s ,k ,c)
     (guard (integer? k))
     (compile-expr c si env) ; value
     (emit "movl %eax, ~a(%esp)" si)
     (compile-expr s si env) ; vector
     (emit "movl ~a(%esp), %ecx" si)
     (emit "movl %ecx, ~a(%eax)" (- (* (add1 k) wordsize) vec-tag))
     (emit "xorl %eax, %eax")]
    [(vector-set! ,s ,k ,c)
     (compile-expr s si env)
     (emit "movl %eax, ~a(%esp)" si)  ; vector
     (compile-expr k (- si wordsize) env)
     (emit "movl %eax, ~a(%esp)" (- si wordsize)) ; index
     (compile-expr c (- si (* 2 wordsize)) env) ; value
     ; store
     (emit "movl ~a(%esp), %ecx" si)
     (emit "subl $~a, %ecx" vec-tag)  ; untag
     (emit "addl $~a, %ecx" wordsize) ; skip the length
     (emit "shrl $~a, ~a(%esp)" (- fixnum-shift 2) (- si wordsize)) ; fixnum->idx (*4)
     (emit "addl ~a(%esp), %ecx" (- si wordsize))
     (emit "movl %eax, (%ecx)")
     ; return 0
     (emit "xorl %eax, %eax")]
    [(vector-length ,v)
     (compile-expr v si env)
     (emit "movl ~a(%eax), %eax" (- vec-tag))
     (emit "sall $~a, %eax" fixnum-shift) ; conver to fixnum
     ]
    [(string? ,k)
     (compile-expr k si env)
     (emit "andl $~a, %eax" ptr-mask)
     (emit-is-eax-equal-to str-tag)]
    [(make-string ,k) (guard (integer? k))
     (compile-gc-flip (+ (add1 k) wordsize) si)
     (emit "movl free_ptr, %eax")    ; store the pointer
     (emit "movl $~a, 0(%eax)" k)    ; store the length
     (emit "orl $~a, %eax" str-tag)  ; tag the pointer
     ; 1 extra space for null
     ; advance allocation pointer
     (emit "addl $~a, (free_ptr)" (logand -8 (+ 11 (+ 1 k))))]
    [(make-string ,k)
     (compile-expr k si env)

     (emit "movl %eax, ~a(%esp)" si)
     (emit "addl $~a, %eax" (immediate-rep wordsize))
     (compile-gc-flip 'eax (- si wordsize))
     (emit "movl ~a(%esp), %eax" si)

     (emit "shrl $~a, %eax" fixnum-shift) ; remove mask
     (emit "movl free_ptr, %ecx")
     (emit "movl %eax, 0(%ecx)")    ; store the length
     (emit "movl %eax, %ecx")       ; store the length again
     (emit "movl free_ptr, %eax")   ; store the pointer
     (emit "orl $~a, %eax" str-tag) ; tag the pointer
     
     (emit "addl $1, %ecx")         ; extra space for null
     (emit "addl $11, %ecx")
     (emit "andl $-8, %ecx")
     (emit "addl %ecx, (free_ptr)") ; advance allocation pointer
     ]
    [(string-length ,v)
     (compile-expr v si env)
     (emit "movl ~a(%eax), %eax" (- str-tag))
     (emit "sall $~a, %eax" fixnum-shift) ; conver to fixnum
     ]
    [(string-ref ,v ,k)
     (compile-expr v si env)
     (emit "movl %eax, ~a(%esp)" si)
     (compile-expr k (- si wordsize) env)
     (emit "shrl $~a, %eax" fixnum-shift)    ; fixnum->index
     (emit "subl $~a, ~a(%esp)" str-tag si)  ; untag
     (emit "addl $~a, ~a(%esp)" wordsize si) ; skip the length
     (emit "addl ~a(%esp), %eax" si)         ; add offset
     (emit "movzbl (%eax),%eax")
     (emit "sall $~a, %eax" char-shift)
     (emit "orl $~a, %eax" char-tag)
    ]
    [(string-set! ,s ,k ,c)
     (compile-expr s si env)
     (emit "movl %eax, ~a(%esp)" si)
     (compile-expr k (- si wordsize) env)
     (emit "movl %eax, ~a(%esp)" (- si wordsize))
     (compile-expr c (- si (* 2 wordsize)) env)
     (emit "shrl $~a, %eax" char-shift) ; get character value
     ; get index
     (emit "movl ~a(%esp), %ecx" si)  ; store
     (emit "subl $~a, %ecx" str-tag)  ; untag
     (emit "addl $~a, %ecx" wordsize) ; skip the length
     (emit "shrl $~a, ~a(%esp)" fixnum-shift (- si wordsize)) 
     (emit "addl ~a(%esp), %ecx" (- si wordsize))
     (emit "movb %al, (%ecx)")
     (emit "xorl %eax, %eax") ; return 0
     ]
    [(symbol? ,s)
     (compile-expr s si env)
     (emit "andl $~a, %eax" ptr-mask)
     (emit-is-eax-equal-to sym-tag)]
    [(str->sym ,s)
     (compile-expr s si env)
     (emit "addl $~a, %eax" (- sym-tag str-tag))]
    [(sym->str ,s)
     (compile-expr s si env)
     (emit "addl $~a, %eax" (- str-tag sym-tag))]
    [(gc-flip ,v)
     (compile-expr v si env)
     (emit "movl %ebp, (EBP)")
     (emit "subl $~a, %esp" (- (abs si) wordsize))
     (emit "movl %esp, (ESP)")
     (emit "pushl %eax")
     (emit "call s_gc_flip")
     (emit "popl %ecx")
     (emit "addl $~a, %esp" (- (abs si) wordsize))]
    [(cons ,v ,w)
     (compile-expr v si env)
     (emit "movl %eax, ~a(%esp)" si)
     (compile-expr w (- si wordsize) env)
     (emit "movl %eax, ~a(%esp)" (- si wordsize))
     (compile-gc-flip (* 2 wordsize) (- si (* 2 wordsize)))
     
     (emit "movl ~a(%esp), %eax" (- si wordsize))
     ; inline allocation on global pointer
     (emit "movl free_ptr,%ecx")
     (emit "addl $~a,(free_ptr)" (* 2 wordsize))
     
     ; store cdr
     (emit "movl %eax, ~a(%ecx)" wordsize)
     ; store car
     (emit "movl ~a(%esp), %eax" si)
     (emit "movl %eax, 0(%ecx)")
     ; store the pointer and tag it
     (emit "movl %ecx, %eax")
     (emit "orl $~a, %eax" pair-tag)]
    [(pair? ,k)
     (let ([null-pred-lbl (generate-label "null_pred_lbl")]
           [end (generate-label "pair_pred_end")])
     (compile-expr k si env)
     (emit "movl %eax, %ecx")
     (emit "movl $~a, %edx" ptr-mask)
     (emit "not %edx")
     (emit "andl %edx, %ecx")   ; get pointer address
     (emit "cmpl $0, %ecx")
     (emit "je ~a" null-pred-lbl)
     (emit "andl $~a, %eax" ptr-mask)   ; unmask to get the tag
     (emit "cmpl $~a, %eax" pair-tag)   ; check eax against val
     (emit "movl $0, %eax")             ; zero eax but leaving equal flag in place
     (emit "sete %al")                  ; set low bit of eax if they were equal 
     (emit "jmp ~a" end)
     (emit "~a: " null-pred-lbl)
     (emit "movl $0, %eax")
     (emit "~a: " end)
     (emit "sall $~a, %eax" bool-shift) ; shift the bit up to the bool position
     (emit "orl $~a, %eax" bool-tag)    ; add boolean type tag
     )]
    [(null? ,v)
     (compile-expr v si env)
     (emit-is-eax-equal-to pair-tag)]
    [(car ,v)
     (compile-expr v si env)
     (emit "movl ~a(%eax), %eax" (- pair-tag))]
    [(cdr ,v)
     (compile-expr v si env)
     (emit "movl ~a(%eax), %eax" (- wordsize pair-tag))]
    [(set-car! ,p ,v)
     (compile-expr p si env)
     (emit "movl %eax, ~a(%esp)" si)
     (compile-expr v (- si wordsize) env)
     (emit "movl ~a(%esp), %edx" si)
     (emit "movl %eax, ~a(%edx)" (- pair-tag))]
    [(set-cdr! ,p ,v)
     (compile-expr p si env)
     (emit "movl %eax, ~a(%esp)" si)
     (compile-expr v (- si wordsize) env)
     (emit "movl ~a(%esp), %edx" si)
     (emit "movl %eax, ~a(%edx)" (- wordsize pair-tag))]
    [(+ ,v ,w)
     (compile-expr w si env)
     (emit "movl %eax, ~a(%esp)" si)
     (compile-expr v (- si wordsize) env)
     (emit "addl ~a(%esp), %eax" si)]
    [(+ ,v)
     (compile-expr v (- si wordsize) env)]
    [(- ,v)
     (compile-expr v si env)
     (emit "neg %eax")]
    [(- ,v ,w)
     (compile-expr w si env)
     (emit "movl %eax, ~a(%esp)" si)
     (compile-expr v (- si wordsize) env)
     (emit "subl ~a(%esp), %eax" si)]
    [(* ,v ,w)
     (compile-expr w si env)
     (emit "movl %eax, ~a(%esp)" si)
     (compile-expr v (- si wordsize) env)
     (emit "shrl $~a, %eax" fixnum-shift)
     (emit "imul ~a(%esp), %eax" si)]
    [(logand ,v ,w)
     (compile-expr w si env)
     (emit "movl %eax, ~a(%esp)" si)
     (compile-expr v (- si wordsize) env)
     (emit "andl ~a(%esp), %eax" si)]
    [(logior ,v ,w)
     (compile-expr w si env)
     (emit "movl %eax, ~a(%esp)" si)
     (compile-expr v (- si wordsize) env)
     (emit "orl ~a(%esp), %eax" si)]
    [(ashr ,v ,w)
     (compile-expr w si env)
     (emit "movl %eax, ~a(%esp)" si)
     (compile-expr v (- si wordsize) env)
     (emit "movl ~a(%esp), %ecx" si)
     (emit "shrl $~a, %ecx" fixnum-shift)
     (emit "shrl %cl, %eax")]
    [(ashl ,v ,w)
     (compile-expr w si env)
     (emit "movl %eax, ~a(%esp)" si)
     (compile-expr v (- si wordsize) env)
     (emit "movl ~a(%esp), %ecx" si)
     (emit "shrl $~a, %ecx" fixnum-shift)
     (emit "sall %cl, %eax")]
    [(apply ,f ,es)
     (compile-apply f es si env)]
    [,() (pretty-print expr) (error "compile-prim" "unknown-expression" expr)]
    ))

(define (compile-closure label free-vars si env)
  (let ([var-n (length free-vars)])
    ; creating closure
    (compile-gc-flip (* wordsize (+ 2 var-n)) si)
    (emit "movl free_ptr, %ecx")
    (emit "movl $~a, 0(%ecx)" label)
    (emit "movl $~a, 4(%ecx)" var-n)
    (for-each
      (lambda (v i)
        (emit (apply-env v env))
        (emit "movl %eax, ~a(%ecx)" (+ (* wordsize (+ 2 i)))))
      free-vars (iota var-n))
    (emit "movl %ecx, %eax")
    (emit "orl $~a, %eax" closure-tag) ; tag the pointer
    (emit "addl $~a, (free_ptr)" (logand -8 (+ 11 (* wordsize (+ 2 var-n))))) ; advance allocation pointer
    ))

(define (initialize-offsets offsets)
  (for-each (lambda (offset) (emit "movl $0, ~a(%esp)" offset)) offsets))

(define (compile-expr expr si env)
  (match expr
    [,v (guard (symbol? v))
     (emit (apply-env v env))]
    [,() (guard (immediate? expr))
     (emit "movl $~a, %eax" (immediate-rep expr))]
    ['() (emit "movl $~a, %eax" pair-tag)]
    [,() (guard (string? expr))
     (compile-prim `(make-string ,(string-length expr)) si env)
     (let ([end (string-length expr)])
        (let loop ([i 0])
          (if (>= i end)
              #t
              (begin
                (emit "movb $~a, ~a(%eax)"
                  (char->integer (string-ref expr i))
                  (+ (+ wordsize i) (- str-tag)))
                (loop (add1 i))))))]
    [(begin . ,es)
     (for-each (lambda (e) (compile-expr e si env)) es)]
    [(if ,pred ,conseq ,alter)
     (let ([end-label (generate-label "end")]
           [alter-label (generate-label "alter")])
      (compile-expr pred si env)
      (emit "cmpl $~a, %eax" (immediate-rep #f))
      (emit "je ~a" alter-label)
      (compile-expr conseq si env)
      (emit "jmp ~a" end-label)
      (emit "~a:" alter-label)
      (compile-expr alter si env)
      (emit "~a:" end-label))]
    [(let ,bindings . ,body)
     (let* (
        [var-n (length bindings)]
        [offsets (map (lambda (x) (- si (* x wordsize))) (iota var-n))]
        [names (map car bindings)]
        [es (map cadr bindings)]
        [inner-si (- si (* var-n wordsize))]
        [compiled-offsets
         (map (lambda (offset)
                (format "movl ~a(%esp), %eax" offset))
              offsets)]
        [inner-env (extend-env names compiled-offsets env)])
      (initialize-offsets offsets)
      (for-each
        (lambda (expr offset)
          (compile-expr expr inner-si env)
          (emit "movl %eax, ~a(%esp)" offset))
        es
        offsets)
      (compile-expr `(begin . ,body) inner-si inner-env))]
    [(labelcall ,lbl ,es)
     (compile-labelcall lbl es si env)]
    [(label ,lbl)
     (emit "movl $~a, %eax" lbl)]
    [(prim ,() ,lbl)
     (emit "movl ~a, %eax" lbl)
     ;;; TODO: this is workaround when primitive is initialize using global construct
     ;;; get the first element ; untag and skip the length
     (emit "movl ~a(%eax),%eax" (- wordsize vec-tag))]
    [(primcall ,op ,es)
     (compile-prim (cons op es) si env)]
    [(closure ,label ,free-vars)
     (compile-closure label free-vars si env)]
    [(funcall ,f ,es)
     (compile-funcall f es si env)]
    [(tailcall ,f ,es)
     (compile-tailcall f es si env)]
    [(foreign-call ,f ,es)
     (compile-foreigncall f es si env)]
    [,() (error "compile-expr" "unknown-expression" expr)]))

(define (compile-tailcall f es si env)
  (let* ([es (cons f es)]
         [args-n (length es)]
         [args-start si]
         [offsets (map (lambda (i) (- args-start (* wordsize i)))
                       (iota args-n))]
         [eval-si (- args-start (* wordsize args-n))])
    (initialize-offsets offsets)
    (for-each
      (lambda (e offset)
        (compile-expr e eval-si env)
        (emit "movl %eax, ~a(%esp)" offset))
      es
      offsets)
    (compile-gc-flip (* wordsize args-n) eval-si)
    
    (emit "movl ~a(%esp), %eax" args-start)
    (emit "movl ~a(%eax), %eax" (- closure-tag))
    
    (for-each
      (lambda (i offset)
        (emit "movl ~a(%esp), %ecx" offset)
        (emit "movl %ecx, ~a(%ebp)" (* (- wordsize) (+ 1 i)))
      )
      (iota args-n) offsets)
    
    (emit "movl %ebp, %esp")
    (emit "popl %ebp")
    (emit "movl $~a, %ecx" args-n)
    (emit "jmp *%eax")))

(define (compile-funcall f es si env)
  (let* ([es (cons f es)]
         [args-n (length es)]
         ; 2 bytes for ret-addr and old-ebp
         [args-start (- si (* 2 wordsize))]
         [offsets (map (lambda (i) (- args-start (* wordsize i)))
                       (iota args-n))]
         [eval-si (- args-start (* wordsize args-n))])
    (initialize-offsets (append (list si (- si wordsize) args-start) offsets))
    (for-each
      (lambda (e offset)
        (compile-expr e eval-si env)
        (emit "movl %eax, ~a(%esp)" offset))
      es
      offsets)
    (compile-gc-flip (* wordsize args-n) eval-si)
    
    ; stack pointer from ... -si+4 are used up, then we substract the pointer by si-4
    (emit "movl ~a(%esp), %eax" args-start)
    (emit "movl ~a(%eax), %eax" (- closure-tag))
    (emit "subl $~a, %esp" (- (abs si) wordsize))
    (emit "movl $~a, %ecx" args-n)
    (emit "call *%eax")
    (emit "addl $~a, %esp" (- (abs si) wordsize))
    ))

(define (compile-labelcall label es si env)
  (let* ([args-n (length es)]
         ; 2 bytes for ret-addr and old-ebp
         [args-start (- si (* 2 wordsize))]
         [offsets (map (lambda (i) (- args-start (* wordsize i)))
                       (iota args-n))]
         [eval-si (- args-start (* wordsize args-n))])
    (initialize-offsets (append (list si (- si wordsize) args-start) offsets))
    (for-each
      (lambda (e offset)
        (compile-expr e eval-si env)
        (emit "movl %eax, ~a(%esp)" offset))
      es
      offsets)
    (emit "movl $~a, %ecx" args-n)
    ; stack pointer from ... -si+4 are used up, then we substract the pointer by si-4
    (if (and (pair? label) (eq? (car label) 'label))
        (begin
          (emit "subl $~a, %esp" (- (abs si) wordsize))
          (emit "call ~a" (cadr label)))
        (begin
          (compile-expr label eval-si env)
          (emit "subl $~a, %esp" (- (abs si) wordsize))
          (emit "call *%eax")))
    (emit "addl $~a, %esp" (- (abs si) wordsize))))

(define (compile-foreigncall f es si env)
  (let* ([es (reverse es)]
         [args-n (length es)]
         [args-start si]
         [offsets (map (lambda (i) (- args-start (* wordsize i)))
                       (iota args-n))]
         [eval-si (- args-start (* wordsize args-n))])
    (initialize-offsets (cons eval-si offsets))
    (for-each
      (lambda (e offset)
        (compile-expr e eval-si env)
        (emit "movl %eax, ~a(%esp)" offset))
      es
      offsets)
    
    (emit "subl $~a, %esp" (abs (+ eval-si wordsize)))
    (emit "call ~a" f)
    (emit "addl $~a, %esp" (abs (+ eval-si wordsize)))))


(define (emit-L-apply)
  #;(
    (0(%esp) ret-addr)
    (-4 old-ebp)
    (-8 closure) ; confirmed
    (-12 arg-1)  ; confirmed by empty list
  )
  (let ([loop (generate-label 'loop)]
        [end (generate-label 'end)])
    ; eax scratch
    ; ecx arg-n
    ; edx holding list-args
    ; esp moving pointer (invariant, used as scratch, shall be restored)
    (emit ".globl L_apply")
    (emit "L_apply:")
    
    (emit "movl $1, %ecx")

    (emit "movl %ebp, ~a(%esp)" (- wordsize)) ; store old-ebp
    (emit "movl %esp, %ebp")                  ; store the old-esp
    
    (emit "movl ~a(%esp), %edx" (* 3 (- wordsize))) ; store arg-1 into %edx
    
    (emit "subl $~a, %esp" (* 3 wordsize))
    
    
    (emit "~a: " loop)
    (emit "cmpl $~a, %edx" pair-tag)
    (emit "je ~a" end)
    (emit "movl ~a(%edx), %eax" (- pair-tag))
    (emit "movl %eax, (%esp)")

    (emit "movl ~a(%edx), %edx" (- wordsize pair-tag))
    (emit "subl $~a, %esp" wordsize)
    (emit "inc %ecx")
    (emit "jmp ~a" loop)
    (emit "~a: " end)

    (emit "movl %ebp, %esp") ; restore old-esp
    (emit "movl ~a(%esp), %ebp" (- wordsize)) ; restore old-ebp
    (emit "movl ~a(%esp), %eax" (* 2 (- wordsize)))
    (emit "movl ~a(%eax), %eax" (- closure-tag))
    (emit "jmp *%eax")
  ))

(define (compile-apply f e si env)
  (let* ([es (list f e)]
         [args-n (length es)]
         ; 2 bytes for ret-addr and old-ebp
         [args-start (- si (* 2 wordsize))]
         [offsets (map (lambda (i) (- args-start (* wordsize i)))
                       (iota args-n))]
         [eval-si (- args-start (* wordsize args-n))])
    (initialize-offsets (append (list si (- si wordsize) args-start) offsets))
    (for-each
      (lambda (e offset)
        (compile-expr e eval-si env)
        (emit "movl %eax, ~a(%esp)" offset))
      es
      offsets)

    (emit "subl $~a, %esp" (- (abs si) wordsize))
    (emit "movl $~a, %ecx" args-n)
    (emit "call L_apply")
    (emit "addl $~a, %esp" (- (abs si) wordsize))
    ))

(define (compile-vararg-construct params-sz)
  (let* ([loop (generate-label 'loop)]
         [end (generate-label 'end)])
    ; ebp - stack base pointer (invariant)
    ; eax - scratch
    ; ecx - argument size
    ; edx - accumulate vararg
    ; esp - stack pointer (but will be used as scratch here then restored)
    
    ;;; convert array to list
    (emit "movl %ebp, %esp")
    (emit "movl %ecx, %eax")
    (emit "addl $1, %eax")
    (emit "imull $~a, %eax" wordsize)
    (emit "subl %eax, %esp")
    
    (emit "movl $~a, (%esp)" pair-tag)
    (emit "addl $~a, %esp" wordsize)

    (emit "~a: " loop)
    (emit "cmpl $~a, %ecx" params-sz)
    (emit "jl ~a" end)
    
    (emit "movl free_ptr, %edx")
    (emit "addl $~a, (free_ptr)" (* 2 wordsize))
    
    (emit "movl ~a(%esp), %eax" (- wordsize))
    (emit "movl %eax, ~a(%edx)" wordsize) ; store-cdr
    (emit "movl (%esp), %eax")
    (emit "movl %eax, (%edx)")            ; store-car

    (emit "orl $~a, %edx" pair-tag)       ; tag pointer
    (emit "movl %edx, (%esp)")

    (emit "addl $~a, %esp" wordsize)
    (emit "subl $1, %ecx")
    (emit "jmp ~a" loop)
    (emit "~a: " end)
    (emit "movl %ebp, %esp")
    ))

(define (compile-code label free-vars params body variadic?)
  (emit "~a: " label)
  (emit "pushl %ebp")
  (emit "movl %esp, %ebp")
  (if variadic?
      (compile-vararg-construct (length params)))
  (compile-expr
    (cons 'begin body)
    (* (- wordsize) (add1 (length params)))
    (append
      (map (lambda (v i)
            (cons v
                  (string-append
                    (format "movl -4(%ebp), %eax\n")
                    (format "movl ~a(%eax), %eax" (- (* wordsize (+ 2 i)) closure-tag)))))
           free-vars
           (iota (length free-vars)))
      (map (lambda (v i)
            (cons v 
                  (format "movl ~a(%ebp),%eax"
                          (* (- wordsize) (add1 i)))))
           params
           (iota (length params)))))
  (emit "popl %ebp")
  (emit "ret"))

(define (compile-labels datums bindings body)
  (emit ".text")
  (emit ".p2align 4,,15")
  (emit "scheme_entry:")
  ; handle incoming call from C
  (emit "movl %ebp, (PREV_EBP)")
  (emit "pushl %ebp")
  (emit "movl %esp, %ebp")
  (emit "movl %ebp, (TOP_EBP)")
  (for-each
    (lambda (lbl)
      (compile-prim '(vector 0) -4 '())
      (emit "movl %eax, ~a" lbl)
      ;;; TODO: it is workaround to push global to CONSTANT pointer so GC can collect these
      (emit "movl $~a, %eax" lbl)
      (emit "movl CONSTANTS, %ecx")
      (emit "movl %eax, (%ecx)")
      (emit "addl $~a, (CONSTANTS)" wordsize))
    datums)

  (compile-expr
    (cons 'begin body)
    (- wordsize)
    (append
      (map (lambda (dat) (cons dat (format "movl ~a, %eax" dat))) datums)
      (map (lambda (b) (list (car b)
              (format "movl ~a, %eax" (car b))))
           bindings)))
  (emit "popl %ebp")
  (emit "ret")
  (let ()
    (for-each (lambda (b)
      (match (cadr b)
        [(code ,variadic? ,free-vars ,params . ,body)
         (guard (boolean? variadic?))
         (compile-code (car b) free-vars params body variadic?)]
        [,bb (error "compile-labels" "unknown" bb)]))
      bindings))
  (emit-L-apply))

(define (compile-entry entry defns globals datums bindings body linker-out)
  (emit ".extern L_apply")
  (emit ".globl ~a" entry)
  ; (for-each (lambda (dat) (emit ".extern ~a" dat)) datums)
  
  (for-each
    (lambda (b)
      (match (cadr b)
        [(code ,variadic? ,free-vars ,params . ,body)
          (guard (boolean? variadic?))
          (compile-code (car b) free-vars params body variadic?)]
        [,bb (error "compile-labels" "unknown" bb)]))
    bindings)
  (emit ".text")
  (emit "~a: " entry)
  ;;; copied from compile-code
  (emit "pushl %ebp")
  (emit "movl %esp, %ebp")
  
  (compile-expr
    (cons 'begin body)
    (- wordsize)
    (append
      (map (lambda (dat) (cons dat (format "movl ~a, %eax" dat))) datums)
      (map (lambda (b) (list (car b)
              (format "movl ~a, %eax" (car b))))
           bindings)))
  (emit "popl %ebp")
  (emit "ret")

  (pretty-print `(link-info ,entry ,defns ,globals ,datums) linker-out)
  )

(define (compile-lib program linker-out)
  (match program
    [(program ,entry ,defns ,globals (labels ,datums ,bindings (begin . ,es)))
     (compile-entry entry defns globals datums bindings es linker-out)]
    [,() 
      (pretty-print program)
      (error "compile-lib" "unknown program" program)]))

(define (preprocess program)
  (let* (
         [program (desugar-top program)]
         [program (reveal-function-top program)]
         [program (convert-assignment program)]
         [program (explicate-control-top program)]
         )
    program))

(define (linking-files-to-exe! output-path . files)
  (define (load-linker file)
    (let* ([ip (open-input-file file)]
           [ld (list (read ip))]
           [ld (cons (read ip) ld)])
      (close-input-port ip)
      (reverse ld)))
  (define get-obj-path car)
  (define (get-entry lk) (cadr (cadr lk)))
  (define (get-defns lk) (caddr (cadr lk)))
  (define (get-globals lk) (cadddr (cadr lk)))
  (define (get-datums lk) (car (cddddr (cadr lk))))
  (define linkers (map load-linker files))
  (for-each
    (lambda (lk)
      (match lk
        [(,obj-path (link-info ,entry ,defns ,globals ,datums))
         (guard (string? obj-path))
         lk]
        [,() (error "linking-files-to-exe!" "unknown linker" lk)]))
    linkers)
  (system "rm -f ./kompil.s")
  (let ([entries (map get-entry linkers)]
        [globals (fold-left set-union (make-set) (map get-globals linkers))]
        [defns (fold-left set-union (make-set) (map get-defns linkers))]
        [datums (fold-left set-union (make-set) (map get-datums linkers))]
        [obj-paths (map get-obj-path linkers)]
        [prev-emit emit]
        [op (open-output-file "./kompil.s")])
    (if (not (subset>=? globals defns))
        (begin
          (pretty-print (list 'set-diff  (set-diff globals defns)))
          (error "linking-files-to-exe!" "missing dependecies")))
    (set! emit  (lambda args (apply format (cons op args)) (newline op)))
    (for-each (lambda (dat) (emit ".globl ~a" dat)) datums)
    (if (not (null? datums))
        (begin
          (emit ".data")
          (emit ".p2align 4,,15")
          (for-each (lambda (d) (emit (format "~a: .byte 0,0,0,0" d))) datums)))
    (emit ".globl scheme_entry")
    (emit ".p2align 4,,15")
    (emit ".text")
    (emit-L-apply)
    (emit "scheme_entry:")
    ; handle incoming call from C
    (emit "movl %ebp, (PREV_EBP)")
    (emit "pushl %ebp")
    (emit "movl %esp, %ebp")
    (emit "movl %ebp, (TOP_EBP)")
    
    (for-each
      (lambda (lbl)
        (compile-prim '(vector 0) -4 '())
        (emit "movl %eax, ~a" lbl)
        ;;; TODO: it is workaround to push global to CONSTANT pointer so GC can collect these
        (emit "movl $~a, %eax" lbl)
        (emit "movl CONSTANTS, %ecx")
        (emit "movl %eax, (%ecx)")
        (emit "addl $~a, (CONSTANTS)" wordsize))
      datums)
    (for-each (lambda (entry) (emit "call ~a" entry)) entries)
    (emit "popl %ebp")
    (emit "ret")
    (close-output-port op)
    (set! emit prev-emit)
    (system (format "gcc -fomit-frame-pointer -m32 -o ~a ./kompil.s ~a ./runtime.o" output-path
            (fold-right (lambda (p ps) (string-append p " " ps)) "" obj-paths)))   
    )
  )

(define (make-obj! program filename)
  (system (format "rm -f ./~a.s ./~a.o ./~a.linker" filename filename filename))
  (let ([op (open-output-file (format "./~a.s" filename))]
        [linker-out (open-output-file (format "./~a.linker" filename))]
        [prev-emit emit]
        [program (preprocess program)])
    (pretty-print (format "./~a.o" filename) linker-out)
    (set! emit 
          (lambda args (apply format (cons op args)) (newline op)))
    (compile-lib program linker-out)
    (close-output-port op)
    (close-output-port linker-out)
    (set! emit prev-emit)
    (system (format "gcc -fomit-frame-pointer -m32 ./~a.s -c -g -o ./~a.o" filename filename))
    (system (format "rm -f ./~a.s" filename))
    ))

(define (compile-runtime)
  (system "gcc -fomit-frame-pointer -m32 -c -g runtime.c -o runtime.o"))

(if (not (file-exist? "./runtime.o"))
    (compile-runtime))

(define (compile-top-prog)
  (define top-program
    `(begin
      ,@(read-sexps-from-path "primitives.scm")
      ,@(read-sexps-from-path "top-program.scm")
      ,@(read-sexps-from-path "scheme-libs.scm")
      ,@(read-sexps-from-path "parser.scm")
      ,@(read-sexps-from-path "parser-entry.scm")
      ))
  (make-obj! top-program "top-prog"))