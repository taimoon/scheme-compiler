(import
  (only (desugar) write-primitive-lib)
  (only (front) preprocess)
  (only (match) match)
  (only (set) make-set set-union set-diff)
  (only (utils)
    make-env extend-env maybe-apply-env apply-env
    read-sexps-from-path align-to-multiple
    random-string symbol->id-symbol string->id-string generate-label
    system*
    path-fileroot path-filename))

;;; alignment formula
; (off + (8 - 1)) & -8 = (off & 7) + -8
#|INTERNALS:
;;;; Calling convention
%ebp is used as closure pointer but caller-save.

(i686
  (caller-save eax ecx edx)
  (callee-save ebp edi esi esp))
(x64
  (caller-save
    rax rcx rdx rsi rdi r8 r9 r10 r11)
  (callee-save
    rbx rsp rbp r12 r13 r14 r15)
)
;;;; Data Repr
;;; Tag allocations
fixnum
  (b snnn nnnn nnnn nnnn nn00)
immediate
  (b xxxx xxxx xxxx iiii i111)
pointer
  (
  type      ttt
  fx        000
  pair      001
  vector    010
  string    011
  fx        100
  symbol    101
  closure   110
  imm       111
  )
  (b pppp pppp pppp pppp pttt)
;;; Composite Data Representation
; We see that fx-* can further encode certain information
; like immutable or mutable
; like static or dynamic allocated

; fx-free-vars-counts is not queryable programmatically
; being used for garbage collector
; encode whether it is for closure or static
; workaround optimization is to initalize the labels statically
pair
  (car cdr)
vector
  (fx-vector-length a0 a1 ...)
string
  (fx-str-length c0 c1 ... #\nul)
symbol
  ->string
closure
  ; allocated lambda
  (code-pointer fx-free-vars-counts fv0 fv1 ...)
|#

#|TODO:
- %esi shall be used as the heap pointer
- inline requested size check rather than rely on gc-flip
|#

#;
(calling layout
  "si points to free cell; starts with -4(%esp).
   Current impl allocate location for locals without mutating the stack pointer.
   In other words, current frame does not mutate esp.
   Therefore, esp here will act frame pointer for gc tracing purpose.
   That is esp always points to old esp.
   Since ebp is used for closure pointer, the caller need to save the frame pointer.
  "
  before-call
  (
    +4    ret-addr*
    esp*  fp
    -4    arg*-0
    -8    arg*-1
    ...
    +4    local-n
    si    _
  )
  prepare-argument
  (
    +4    ret-addr*
    esp*  fp
    -4    arg*-0
    -8    arg*-1
    ...
    +4    local-n
    si    _     ;;; to be return address
    -4    esp*  ;;; push by caller
    -8    arg-0
    -16   arg-1
    ...
  )
  prepare-to-call
  (
    +4    ret-addr*
    _     fp
    -4    arg*-0
    -8    arg*-1
    ...
    esp   local-n
    -4    _       ;;; to be return address
    -8    esp*    ;;; push by caller
    -16   arg-0
    -24   arg-1
    ...
  )
  enter-callee
  (
    esp ret-addr
    _   old-fp
    _   arg-0
    _   arg-1
    _   arg-2
    ...
  )
  evaluate-callee-body
  substract esp
  (
    +4    ret-addr
    esp   old-fp
    -4    arg-0
    -8    arg-1
    -16   arg-2
    ...
    +4    arg-n
    si    _
  )
)
(let ()
(define FIXNUM-MASK   #b00000011)
(define FIXNUM-TAG    #b00000000)
(define FIXNUM-SHIFT  2)
(define IMM-SHIFT     8)
(define IMM-MASK      #b11111111)
(define IMM-TAG       #b00000111)
(define EOF-TAG       #b00000111)
(define CHAR-TAG      #b00001111)
(define BOOL-TAG      #b00011111)
(define NIL-TAG       #b00111111)
(define FALSE-IMM     BOOL-TAG)
(define TRUE-IMM      (bitwise-ior (ash 1 IMM-SHIFT) BOOL-TAG))
(define PTR-MASK      #b00000111)
(define PAIR-TAG      #b00000001)
(define VEC-TAG       #b00000010)
(define STR-TAG       #b00000011)
(define SYM-TAG       #b00000101)
(define CLOS-TAG      #b00000110)

(define WORDSIZE 4)

;;; emit
(define (emit op . args)
  (apply format (cons op args))
  (newline op))

(define (immediate? v)
  (or (integer? v) (boolean? v) (char? v)))

(define (immediate-rep imm)
  (cond
    ((char? imm)
     (bitwise-ior CHAR-TAG
                  (ash (char->integer imm)
                       IMM-SHIFT)))
    ((boolean? imm)
     (if imm TRUE-IMM FALSE-IMM))
    ((integer? imm)
     (ash imm FIXNUM-SHIFT))
    ((null? imm)
     NIL-TAG)
    (else (error "immediate-rep" "unknown immediate" imm))))

(define (emit-eax=? op v)
  (emit op "cmp $~a, %eax" v)
  (emit op "sete %al")
  (emit op "sal $~a, %eax" IMM-MASK)
  (emit op "or $~a, %eax" BOOL-TAG))

(define suffix-cmp
  '((< setl jl)
    (<= setle jle)
    (= sete je)
    (> setg jg)
    (>= setge jge)))

(define (emit-cmp op cmp e1 e2 si env)
  (emit-expr op e1 si env)
  (emit op "mov %eax, ~a(%esp)" si)
  (emit-expr op e2 (- si WORDSIZE) env)
  (emit op "cmp %eax, ~a(%esp)" si)
  (emit op "mov $0, %eax")
  (emit op "~a %al" (cadr (assq cmp suffix-cmp)))
  (emit op "sal $~a, %eax" IMM-SHIFT)
  (emit op "or $~a, %eax" BOOL-TAG))

(define (emit-let op bindings body si env)
 (let recur ((bindings bindings)
             (si si)
             (inner-env env))
    (if (pair? bindings)
        (begin
          (emit-expr op (cadar bindings) si env)
          (emit op "mov %eax, ~a(%esp)" si)
          (recur
            (cdr bindings)
            (- si WORDSIZE)
            (extend-env (caar bindings) (format "~a(%esp)" si) inner-env)))
        (emit-expr op body si inner-env))))

(define (emit-if op pred conseq altern si env)
  (let ((end-label (generate-label "end"))
        (altern-label (generate-label "altern")))
    (emit-expr op pred si env)
    (emit op "cmp $~a, %eax" FALSE-IMM)
    (emit op "je ~a" altern-label)
    (emit-expr op conseq si env)
    (emit op "jmp ~a" end-label)
    (emit op "~a: " altern-label)
    (emit-expr op altern si env)
    (emit op "~a: " end-label)))

(define (emit-str-lit op s si)
  (emit op "mov (free_ptr), %eax")
  (emit op "addl $~a, (free_ptr)"
           (align-to-multiple 8 (+ WORDSIZE (+ 1 (string-length s)))))
  (emit op "movl $~a, (%eax)" (immediate-rep (string-length s)))
  (for-each
    (lambda (i)
      (emit op "movb $~a, ~a(%eax)"
               (char->integer (string-ref s i))
               (+ WORDSIZE i)))
    (iota (string-length s)))
  (emit op "movb $0, ~a(%eax)" (+ WORDSIZE (string-length s)))
  (emit op "or $~a, %eax" STR-TAG))

(define (emit-vector op es si env)
  ;;; es are list of atomized subexpression
  (let ((sz (length es)))
    (emit op "mov (free_ptr), %ecx")
    (emit op "movl $~a, (%ecx)" (immediate-rep sz))
    (emit op "addl $~a, (free_ptr)" (align-to-multiple 8 (* (+ sz 1) WORDSIZE)))
    (for-each
      (lambda (i e)
        (emit-expr op e si env)
        (emit op "mov %eax, ~a(%ecx)" (* (+ i 1) WORDSIZE)))
      (iota sz)
      es)
    (emit op "mov %ecx, %eax")
    (emit op "or $~a, %eax" VEC-TAG)))

(define (emit-prim op e si env)
  (match e
    ((%walk-stack)
     (emit-walk-stack op -1 si env))
    ((%walk-stack ,e)
     (emit-walk-stack op e si env))
    ((align-to-multiple 8 ,e)
     (emit-expr op e si env)
     (emit op "add $~a, %eax" (immediate-rep (- 8 1)))
     (emit op "and $~a, %eax" (immediate-rep -8)))
    ((eof-object)
     (emit op "mov $~a, %eax" EOF-TAG))
    ((eof-object? ,e)
     (emit-expr op e si env)
     (emit op "and $~a, %eax" IMM-MASK)
     (emit-eax=? op EOF-TAG))
    ((integer? ,e)
     (emit-expr op e si env)
     (emit op "and $~a, %eax" FIXNUM-MASK)
     (emit-eax=? op FIXNUM-TAG))
    ((boolean? ,e)
     (emit-expr op e si env)
     (emit op "and $~a, %eax" IMM-MASK)
     (emit-eax=? op BOOL-TAG))
    ((char? ,e)
     (emit-expr op e si env)
     (emit op "and $~a, %eax" IMM-MASK)
     (emit-eax=? op CHAR-TAG))
    ((char->integer ,e)
     (emit-expr op e si env)
     (emit op "sar $~a, %eax" (- IMM-SHIFT FIXNUM-SHIFT)))
    ((integer->char ,e)
     (emit-expr op e si env)
     (emit op "sal $~a, %eax" (- IMM-SHIFT FIXNUM-SHIFT))
     (emit op "or $~a, %eax" CHAR-TAG))
    ((eq? ,e1 ,e2)
     (emit-cmp op '= e1 e2 si env))
    ((,cmp ,e1 ,e2)
     (guard (assoc cmp suffix-cmp))
     (emit-cmp op cmp e1 e2 si env))
    ((- ,e)
     (emit-expr op e si env)
     (emit op "neg %eax"))
    ((- ,e1 ,e2)
     (emit-expr op e2 si env)
     (emit op "mov %eax, ~a(%esp)" si)
     (emit-expr op e1 (- si WORDSIZE) env)
     (emit op "sub ~a(%esp), %eax" si))
    ((+ ,e1 ,e2)
     (emit-expr op e2 si env)
     (emit op "mov %eax, ~a(%esp)" si)
     (emit-expr op e1 (- si WORDSIZE) env)
     (emit op "add ~a(%esp), %eax" si))
    ((* ,e1 ,e2)
     (emit-expr op e2 si env)
     (emit op "mov %eax, ~a(%esp)" si)
     (emit-expr op e1 (- si WORDSIZE) env)
     (emit op "sar $~a, %eax" FIXNUM-SHIFT)
     (emit op "imul ~a(%esp), %eax" si))
    ((fxabs ,e1)
     (emit-expr op e1 si env)
     (emit op "mov %eax, %ecx")
     (emit op "neg %ecx")
     (emit op "cmovg %ecx, %eax"))
    ((fxmax ,e1 ,e2)
     (emit-expr op e2 si env)
     (emit op "mov %eax, ~a(%esp)" si)
     (emit-expr op e1 (- si WORDSIZE) env)
     (emit op "cmp %eax, ~a(%esp)" si)
     (emit op "cmovg ~a(%esp), %eax" si))
    ((fixnum-width)
     (emit op "mov $~a, %eax" (immediate-rep (- (ash WORDSIZE 3) 2))))
    ((fxlength ,e1)
     (emit-prim op `(fxabs ,e1) si env)
     (emit op "or $~a, %eax" FIXNUM-MASK)
     (emit op "lzcnt %eax, %eax")
     (emit op "neg %eax")
     (emit op "add $~a, %eax" (- (ash WORDSIZE 3) 2))
     (emit op "sal $~a, %eax" FIXNUM-SHIFT))
    ((div ,dividend ,divisor)
     #;((div (* t x) (* t y))
        => (div x y)
        => (tag (div x y)))
     (emit-expr op divisor si env)
     (emit op "mov %eax, ~a(%esp)" si)
     (emit-expr op dividend (- si WORDSIZE) env)
     (emit op "mov ~a(%esp), %ecx" si)
     (emit op "cdq")
     (emit op "idiv %ecx")
     (emit op "sal $~a, %eax" FIXNUM-SHIFT))
    ((mod ,dividend ,divisor)
     ;;; TODO: don't know why this works?
     (emit-expr op divisor si env)
     (emit op "movl %eax, ~a(%esp)" si)
     (emit-expr op dividend (- si WORDSIZE) env)
     (emit op "mov ~a(%esp), %ecx" si)
     (emit op "cdq")
     (emit op "idiv %ecx")
     (emit op "mov %edx, %eax"))
    ((ashl ,e1 ,e2)
     (emit-expr op e2 si env)
     (emit op "mov %eax, ~a(%esp)" si)
     (emit-expr op e1 (- si WORDSIZE) env)
     (emit op "mov ~a(%esp), %ecx" si)
     (emit op "sar $~a, %ecx" FIXNUM-SHIFT)
     (emit op "sal %cl, %eax"))
    ((ashr ,e1 ,e2)
     (emit-expr op e2 si env)
     (emit op "mov %eax, ~a(%esp)" si)
     (emit-expr op e1 (- si WORDSIZE) env)
     (emit op "mov ~a(%esp), %ecx" si)
     (emit op "sar $~a, %ecx" FIXNUM-SHIFT)
     (emit op "sar $~a, %eax" FIXNUM-SHIFT)
     (emit op "sar %cl, %eax")
     (emit op "sal $~a, %eax" FIXNUM-SHIFT))
    ((bitwise-and ,e1 ,e2)
     (emit-expr op e2 si env)
     (emit op "mov %eax, ~a(%esp)" si)
     (emit-expr op e1 (- si WORDSIZE) env)
     (emit op "and ~a(%esp), %eax" si))
    ((bitwise-ior ,e1 ,e2)
     (emit-expr op e2 si env)
     (emit op "mov %eax, ~a(%esp)" si)
     (emit-expr op e1 (- si WORDSIZE) env)
     (emit op "or ~a(%esp), %eax" si))
    ((null? ,e)
     (emit-expr op e si env)
     (emit op "and $~a, %eax" IMM-MASK)
     (emit-eax=? op NIL-TAG))
    ((pair? ,e)
     (emit-expr op e si env)
     (emit op "and $~a, %eax" PTR-MASK)
     (emit-eax=? op PAIR-TAG))
    ((cons ,simp1 ,simp2)
     (emit op "mov (free_ptr), %ecx")
     (emit op "addl $~a, (free_ptr)" (* 2 WORDSIZE))
     (emit-expr op simp2 si env)
     (emit op "mov %eax, ~a(%ecx)" WORDSIZE)
     (emit-expr op simp1 si env)
     (emit op "mov %eax, (%ecx)")
     (emit op "mov %ecx, %eax")
     (emit op "or $~a, %eax" PAIR-TAG))
    ((car ,e)
     (emit-expr op e si env)
     (emit op "mov ~a(%eax), %eax" (- PAIR-TAG)))
    ((cdr ,e)
     (emit-expr op e si env)
     (emit op "mov ~a(%eax), %eax" (- WORDSIZE PAIR-TAG)))
    ((set-car! ,e ,v)
     (emit-expr op v si env)
     (emit op "mov %eax, ~a(%esp)" si)
     (emit-expr op e (- si WORDSIZE) env)
     (emit op "mov ~a(%esp), %ecx" si)
     (emit op "mov %ecx, ~a(%eax)" (- PAIR-TAG)))
    ((set-cdr! ,e ,v)
     (emit-expr op v si env)
     (emit op "mov %eax, ~a(%esp)" si)
     (emit-expr op e (- si WORDSIZE) env)
     (emit op "mov ~a(%esp), %ecx" si)
     (emit op "mov %ecx, ~a(%eax)" (- WORDSIZE PAIR-TAG)))
    ((string? ,e)
     (emit-expr op e si env)
     (emit op "and $~a, %eax" PTR-MASK)
     (emit-eax=? op STR-TAG))
    ((make-string ,sz ,ch)
     ;;; sz and ch are already values
     (emit op "mov (free_ptr), %edx")
     (emit op "mov (free_ptr), %edi")
     (emit-expr op sz si env)
     (emit op "mov %eax, (%edi)") ; store the length
     ; align and fill
     (emit op "sar $~a, %eax" FIXNUM-SHIFT)
     (emit op "mov %eax, %ecx")   ; store the length again
     (emit op "add $12, %eax")    ; extra space for null (11 + 1)
     (emit op "and $-8, %eax")
     (emit op "add %eax, (free_ptr)")
     (emit op "add $~a, %edi" WORDSIZE)
     (emit-expr op ch si env)
     (emit op "sar $~a, %eax" IMM-SHIFT)
     (emit op "cld")
     (emit op "rep stosb")
     (emit op "movb $~a, (%edi)" (char->integer #\nul))
     (emit op "mov %edx, %eax")
     (emit op "or $~a, %eax" STR-TAG))
    ((string-length ,s)
     (emit-expr op s si env)
     (emit op "mov ~a(%eax), %eax" (- STR-TAG)))
    ((string-set! ,s ,i ,v)
     (emit-expr op v si env)
     (emit op "mov %eax, ~a(%esp)" si)
     (emit-expr op i (- si WORDSIZE) env)
     (emit op "mov %eax, ~a(%esp)" (- si WORDSIZE))
     (emit-expr op s (- si (* 2 WORDSIZE)) env)
     (emit op "mov ~a(%esp), %ecx" (- si WORDSIZE))
     (emit op "sarl $~a, %ecx" FIXNUM-SHIFT) ; remove fixnum mask
     (emit op "add %ecx, %eax")
     (emit op "mov ~a(%esp), %ecx" si)
     (emit op "sar $~a, %ecx" IMM-SHIFT) ; remove char mask
     (emit op "movb %cl, ~a(%eax)" (- WORDSIZE STR-TAG))
     (emit op "xor %eax, %eax"))
    ((string-ref ,s ,i)
     (emit-expr op i si env)
     (emit op "mov %eax, ~a(%esp)" si)
     (emit-expr op s (- si WORDSIZE) env)
     (emit op "mov ~a(%esp), %ecx" si)
     (emit op "sarl $~a, %ecx" FIXNUM-SHIFT) ; remove fixnum mask
     (emit op "add %ecx, %eax")
     (emit op "movzbl ~a(%eax), %eax" (- WORDSIZE STR-TAG))
     (emit op "sal $~a, %eax" IMM-SHIFT)
     (emit op "or $~a, %eax" CHAR-TAG))
    ((vector? ,e)
     (emit-expr op e si env)
     (emit op "and $~a, %eax" PTR-MASK)
     (emit-eax=? op VEC-TAG))
    ((vector . ,es)
     (emit-vector op es si env))
    ((vector-length ,e)
     (emit-expr op e si env)
     (emit op "mov ~a(%eax), %eax" (- VEC-TAG)))
    ((make-vector ,sz)
     ;;; sz is already value
     (emit-expr op sz si env)
     (emit op "mov (free_ptr), %edx")
     (emit op "mov (free_ptr), %edi")
     (emit op "mov %eax, (%edi)") ; store the length
     (emit op "mov %eax, %ecx")
     (emit op "add $11, %ecx")
     (emit op "and $-8, %ecx")
     (emit op "add %ecx, (free_ptr)")
     (emit op "mov (free_ptr), %edi")
     (emit op "lea ~a(%edx), %edi" WORDSIZE)
     (emit op "mov %eax, %ecx")
     (emit op "sal $~a, %ecx" FIXNUM-SHIFT)
     (emit op "mov $0, %eax")
     (emit op "rep stosw")
     (emit op "mov %edx, %eax")
     (emit op "or $~a, %eax" VEC-TAG))
    ((vector-ref ,v ,i)
     (emit-expr op i si env)
     (emit op "mov %eax, ~a(%esp)" si)
     (emit-expr op v (- si WORDSIZE) env)
     (emit op "add ~a(%esp), %eax" si)
     (emit op "mov ~a(%eax), %eax" (- WORDSIZE VEC-TAG)))
    ((vector-set! ,v ,i ,e)
     (emit-expr op e si env)
     (emit op "mov %eax, ~a(%esp)" si)
     (emit-expr op i (- si WORDSIZE) env)
     (emit op "mov %eax, ~a(%esp)" (- si WORDSIZE))
     (emit-expr op v (- si (* WORDSIZE 2)) env)
     (emit op "add ~a(%esp), %eax" (- si WORDSIZE))
     (emit op "mov ~a(%esp), %ecx" si)
     (emit op "mov %ecx, ~a(%eax)" (- WORDSIZE VEC-TAG))
     (emit op "mov %ecx, %eax"))
    ((procedure? ,e)
     (emit-expr op e si env)
     (emit op "and $~a, %eax" PTR-MASK)
     (emit-eax=? op CLOS-TAG))
    ((symbol? ,e)
     (emit-expr op e si env)
     (emit op "and $~a, %eax" PTR-MASK)
     (emit-eax=? op SYM-TAG))
    ((%string->symbol ,e)
     (emit-expr op e si env)
     (emit op "add $~a, %eax" (- SYM-TAG STR-TAG)))
    ((%symbol->string ,e)
     (emit-expr op e si env)
     (emit op "sub $~a, %eax" (- SYM-TAG STR-TAG)))
    (,() (error "emit-prim" "unmatch" e))))

(define (emit-walk-stack op e si env)
  ;;; NOTE:
  ;;; The procedure assume esp is the frame pointer
  ;;; And top stack pointer implied by the si parameter
  ;;; gc_flip(word_t fx_size, word_t *esp, word_t *ebp)
  (if (not (eq? e '%eax))
      (emit-expr op e si env))
  (cond
    ((integer? si)
     (emit op "lea ~a(%esp), %ecx" (- (- (abs si) WORDSIZE)))  ; store the stack pointer
     (emit op "mov %esp, %edx")                                ; store the frame pointer
     (emit op "sub $~a, %esp" (- (abs si) WORDSIZE))           ; substract esp
     (emit op "push %edx") ; *ebp
     (emit op "push %ecx") ; *esp
     (emit op "push %eax") ; fx_size
     (emit op "call gc_flip")
     (emit op "add $~a, %esp" (+ (- (abs si) WORDSIZE) (* 3 WORDSIZE)))
     (emit op "mov ~a(%esp), %ebp" (- WORDSIZE)))
    ((eq? si '%ecx)
     ;;; %ecx : amount of words on stack from frame pointer
     ;;; Do gc here before constructing vararg
     ; So the %ecx here is callee save
     ; Have to save the **incoming** argument count to avoid being destroyed by gc
     ; see emit-walk-stack for implementation.
     ; Because incoming argument count cannot be known in advance
     ; causing the stack pointer cannot be calculated in advance
     ; therefore, have to calculate the stack pointer dynamically
     ; hence requiring special implementation here
     (emit op "mov %esp, %edx")    ; store the frame pointer for gc

     (emit op "sal $~a, %ecx" FIXNUM-SHIFT)
     (emit op "sub %ecx, %esp")
     (emit op "mov %ecx, ~a(%esp)" (- WORDSIZE))
     ;;; call to gc
     ;;; calculate (align 8 (* 2 WORDSIZE argc))
     ; operand of imul don't need to be tagged again
     (emit op "mov %ecx, %eax")
     (emit op "imul $~a, %eax" (* 2 WORDSIZE))
     (emit op "add $~a, %eax" (immediate-rep (- 8 1)))
     (emit op "and $~a, %eax" (immediate-rep -8))
    
     (emit op "sub $~a, %esp" WORDSIZE)
     (emit op "lea (%esp), %ecx")
     (emit op "push %edx")
     (emit op "push %ecx")
     (emit op "push %eax")
     (emit op "call gc_flip")
     (emit op "add $~a, %esp" (* 4 WORDSIZE)) ; pop out args and argc
     ;;; end of gc
     ; restore
     (emit op "mov ~a(%esp), %ecx" (- WORDSIZE))
     (emit op "add %ecx, %esp")
     (emit op "sar $~a, %ecx" FIXNUM-SHIFT))
    (else (error "emit-walk-stack" "unknown" si))))

(define (emit-tail-call op maybe-decl fn args si env)
  (let* ((argc (length (cons fn args)))
         (calc-offset (lambda (i) (- si (* WORDSIZE i)))))
    (for-each
      (lambda (offset arg)
        (emit-expr op arg offset env)
        (emit op "mov %eax, ~a(%esp)" offset))
      (map calc-offset (iota argc))
      (cons fn args))
    ;;; copying the memory
    (for-each
      (lambda (i)
        (emit op "mov ~a(%esp), %eax" (calc-offset i))
        (emit op "mov %eax, ~a(%esp)" (* (- WORDSIZE) (+ i 1))))
      (iota argc))
    (emit op "mov $~a, %ecx" argc)
    (match maybe-decl
      ((label ,lbl)
       (emit op "add $~a, %esp" WORDSIZE)
       (emit op "jmp ~a" lbl))
      (#f
       (emit op "mov ~a(%esp), %eax" (- WORDSIZE))
       (emit op "mov ~a(%eax), %eax" (- CLOS-TAG))
       (emit op "add $~a, %esp" WORDSIZE)
       (emit op "jmp *%eax"))
      (,() (error "emit-tail-call" "unmatch" maybe-decl)))))

(define (emit-call op maybe-decl fn args si env)
  (let* ((argc (length (cons fn args)))
         (start-si (- si (* 2 WORDSIZE)))
         (calc-offset (lambda (i) (- start-si (* WORDSIZE i)))))
    (emit op "movl $0, ~a(%esp)" si)
    ;;; this store the frame pointer
    (emit op "movl %esp, ~a(%esp)" (- si WORDSIZE))
    (for-each
      (lambda (offset arg)
        (emit-expr op arg offset env)
        (emit op "mov %eax, ~a(%esp)" offset))
      (map calc-offset (iota argc))
      (cons fn args))
    (emit op "mov $~a, %ecx" argc)
    (match maybe-decl
      ((label ,lbl)
       (emit op "sub $~a, %esp" (- (abs si) WORDSIZE))
       (emit op "call ~a" lbl))
      (#f
       (emit op "mov ~a(%esp), %eax" start-si)
       (emit op "mov ~a(%eax), %eax" (- CLOS-TAG))
       (emit op "sub $~a, %esp" (- (abs si) WORDSIZE))
       (emit op "call *%eax"))
      (,() (error "emit-call" "unmatch" maybe-decl)))
    (emit op "add $~a, %esp" (- (abs si) WORDSIZE))
    (emit op "mov ~a(%esp), %ebp" (- WORDSIZE))))

(define (emit-foreign-call op fn args si env)
  (let* ((args (reverse args))
         (argc (length args))
         (calc-offset (lambda (i) (- si (* WORDSIZE i))))
         (end-si (- si (* WORDSIZE (- argc 1)))))
    (for-each
      (lambda (offset arg)
        (emit-expr op arg offset env)
        (emit op "mov %eax, ~a(%esp)" offset))
      (map calc-offset (iota argc))
      args)
    (emit op "sub $~a, %esp" (abs end-si))
    (emit op "call ~a" fn)
    (emit op "add $~a, %esp" (abs end-si))))

(define (emit-tail-apply op fn es si env)
  (emit-expr op fn si env)
  (emit op "mov %eax, ~a(%esp)" si)
  (emit-expr op es (- si WORDSIZE) env)
  (emit op "mov %eax, ~a(%esp)"  (- (* 2 WORDSIZE)))
  (emit op "mov ~a(%esp), %eax" si)
  (emit op "mov %eax, ~a(%esp)" (- WORDSIZE))
  (emit op "add $~a, %esp" WORDSIZE)
  (emit op "jmp L_apply"))

(define (emit-apply op fn es si env)
  (emit op "movl $0, ~a(%esp)" si)
  (emit op "movl $0, ~a(%esp)" (- si WORDSIZE))
  (emit-expr op fn (- si (* 2 WORDSIZE)) env)
  (emit op "mov %eax, ~a(%esp)" (- si (* 2 WORDSIZE)))
  (emit-expr op es (- si (* 3 WORDSIZE)) env)
  (emit op "mov %eax, ~a(%esp)" (- si (* 3 WORDSIZE)))
  (emit op "mov %esp, ~a(%esp)" (- si WORDSIZE))
  (emit op "sub $~a, %esp" (- (abs si) WORDSIZE))
  (emit op "call L_apply")
  (emit op "add $~a, %esp" (- (abs si) WORDSIZE))
  (emit op "mov ~a(%esp), %ebp" (- WORDSIZE)))

(define (emit-L-apply op)
  (let ((loop (generate-label "unstack"))
        (end (generate-label "call")))
    (emit op ".p2align 3")
    (emit op "L_apply: ")
    (emit op "mov ~a(%esp), %eax" (- (* 3 WORDSIZE)))
    (emit op "lea ~a(%esp), %edi" (- (* 3 WORDSIZE)))
    (emit op "mov $1, %ecx")
    (emit op "~a: " loop)
    (emit op "cmp $~a, %eax" NIL-TAG)
    (emit op "je ~a" end)

    (emit op "mov ~a(%eax), %edx" (- PAIR-TAG))
    (emit op "mov %edx, (%edi)")

    (emit op "mov ~a(%eax), %eax" (- WORDSIZE PAIR-TAG))
    (emit op "sub $~a, %edi" WORDSIZE)
    (emit op "add $1, %ecx")
    (emit op "jmp ~a" loop)
    (emit op "~a: " end)
    (emit op "mov ~a(%esp), %eax" (- (* 2 WORDSIZE)))
    (emit op "mov ~a(%eax), %eax" (- CLOS-TAG))
    (emit op "jmp *%eax")
  ))

(define (emit-construct-vararg op argc si)
  #;(
      [z 1]
      ()  0
      (1) 1

      [(x y . z) 3]
      (0)           1   err
      (0 1)         2   (0 1 ())
      (0 1 2)       3   (0 1 (2))
      (0 1 2 3)     4   (0 1 (2 3))
      (0 1 2 3 4)   5   (0 1 (2 3 4))
      
      example
      inp         cons    argc  sz
      (0 1 2 3 4) ()      5     3
      (0 1 2 3)   (4)     4     3
      (0 1 2)     (3 4)   3     3
      (0 1)       (2 3 4) 2     3
  )
  (let ((loop (generate-label 'loop))
        (cont-cons (generate-label 'conseq))
        (end (generate-label 'end)))
    
    (emit op "cmp $~a, %ecx" (sub1 argc))
    (emit op "jge ~a" cont-cons)
    (emit-foreign-call op 's_proc_arg_err '() si (make-env))
    (emit op "~a: " cont-cons)
    (emit-walk-stack op '%eax '%ecx (make-env))
    ;;;; counter
    ;;; ecx is arg-length/ length
    ;;; eax is accumulator
    ;;; edx is source from stack
    ;;;; temp
    ;;; edi destination
    (emit op "mov %ecx, %eax")
    (emit op "imul $~a, %eax" WORDSIZE)
    (emit op "mov %esp, %edx")
    (emit op "sub %eax, %edx")
    
    (emit op "mov $~a, %eax" NIL-TAG)
    
    (emit op "~a:" loop)
    (emit op "cmp $~a, %ecx" argc)
    (emit op "jl ~a" end)

    (emit op "mov (free_ptr), %edi")
    (emit op "addl $~a, (free_ptr)" (* 2 WORDSIZE))
    
    (emit op "mov %eax, ~a(%edi)" WORDSIZE)
    (emit op "mov (%edx), %eax")
    (emit op "mov %eax, (%edi)")
    (emit op "or $~a, %edi" PAIR-TAG)
    
    (emit op "mov %edi, %eax")
    (emit op "sub $1, %ecx")
    (emit op "add $~a, %edx" WORDSIZE)
    (emit op "jmp ~a" loop)
    
    (emit op "~a: " end)
    (emit op "mov %eax, ~a(%edx)" (- WORDSIZE))))

(define (emit-procedure op fn params variadic? fvs body)
  (let* ((argc (length params))
         (calc-offset
          (lambda (i) (format "~a(%esp)" (* (- WORDSIZE) (+ i 1)))))
         (offsets (map calc-offset (iota argc)))
         (env (extend-env params offsets (make-env)))
         (si (* (- WORDSIZE) (+ argc 1))))
    (emit op ".p2align 3")
    (emit op "~a: " fn)
    (emit op "sub $~a, %esp" WORDSIZE)
    (if (not variadic?)
        (let ((end (generate-label 'conseq)))
          (emit op "cmp $~a, %ecx" argc)
          (emit op "je ~a" end)
          (emit-foreign-call op 's_proc_arg_err '() si env)
          (emit op "~a: " end))
        (emit-construct-vararg op argc si))
    (emit op "mov ~a(%esp), %ebp" (- WORDSIZE))
    (emit-expr op body si env)
    (emit op "add $~a, %esp" WORDSIZE)
    (emit op "ret")))

(define (emit-variadic-procedure op fn fvs clauses)
  (let* ((env (make-env)))
    (emit op ".p2align 3")
    (emit op "~a: " fn)
    (emit op "mov ~a(%esp), %ebp" (- (* WORDSIZE 2)))
    (for-each
      (lambda (clause)
        (match clause
          ((closure ,fn ,fvs ,variadic? ,argc)
           (let ((next (generate-label "case_lambda_altern")))
            (cond
              (variadic?
               (emit op "cmp $~a, %ecx" (sub1 argc))
               (emit op "jl ~a" next))
              (else
               (emit op "cmp $~a, %ecx" argc)
               (emit op "jne ~a" next)))
            (emit op "mov %ecx, %edx")
            (emit-closure op fn fvs #f env)
            (emit op "mov %eax, ~a(%esp)" (- (* WORDSIZE 2)))
            (emit op "mov %edx, %ecx")
            (emit op "jmp ~a" fn)
            (emit op "~a: " next)))
          (,() (error "emit-variadic-procedure" "unknown clause" clause))))
      clauses)
    (emit-foreign-call op 's_proc_arg_err '() (- (* 2 WORDSIZE)) env)))

(define (emit-closure op fn fvs si env)
  (define fvs-count (length fvs))
  (emit op "mov (free_ptr), %ecx")
  (emit op "addl $~a, (free_ptr)"
            (align-to-multiple 8 (* WORDSIZE (+ 2 fvs-count))))
  (emit op "mov $~a, %eax" fn)
  (emit op "mov %eax, (%ecx)")
  (emit op "movl $~a, ~a(%ecx)" (immediate-rep fvs-count) WORDSIZE)
  (for-each
    (lambda (fv i)
      (emit-expr op fv si env)
      (emit op "mov %eax, ~a(%ecx)"
              (* WORDSIZE (+ 2 i))))
    fvs
    (iota fvs-count))
  (emit op "mov %ecx, %eax")
  (emit op "or $~a, %eax" CLOS-TAG))

(define (emit-expr op e si env)
  (match e
    (,()
     (guard (immediate? e))
     (emit op "mov $~a, %eax" (immediate-rep e)))
    (,()
     (guard (string? e))
     (emit-str-lit op e si))
    (,()
     (guard (symbol? e))
     (emit op "mov ~a, %eax" (apply-env e env)))
    ((quote ())
     (emit op "mov $~a, %eax" (immediate-rep '())))
    ((collect ,sz)
     (emit-expr op sz si env)
     (emit-walk-stack op '%eax si env))
    ((global ,lbl)
     (emit op "mov ~a, %eax" lbl))
    ((label ,lbl)
     (emit op "mov ~a, %eax" lbl))
    ((closure-ref ,() ,i)
     (guard (integer? i))
     (emit op "mov ~a(%ebp), %eax" (- (* WORDSIZE (+ i 2)) CLOS-TAG)))
    ((set-label! ,lbl ,e)
     (emit-expr op e si env)
     (emit op "mov %eax, ~a" lbl))
    ((begin . ,es)
     (for-each
      (lambda (e) (emit-expr op e si env))
      es))
    ((let ,bindings ,body)
     (emit-let op bindings body si env))
    ((if ,pred ,conseq ,altern)
     (emit-if op pred conseq altern si env))
    ((closure ,fn ,fvs)
     (emit-closure op fn fvs si env))
    ((call ,tail? (prim apply) ,fn ,es)
     ((if tail? emit-tail-apply emit-apply) op fn es si env))
    ((call ,() (prim ,pr) . ,es)
     (emit-prim op (cons pr es) si env))
    ((call ,tail? ,maybe-decl ,fn . ,es)
     ((if tail? emit-tail-call emit-call) op maybe-decl fn es si env))
    ((foreign-call ,fn . ,es)
     (emit-foreign-call op fn es si env))
    (,() (error "emit-expr" "unmatch" e))))

(define (emit-code op entry-name prog)
  (match prog
    ((program (labels . ,labels) (data . ,data) (unbounds . ,unbounds) ,body)
     ;;; NOTE:
     ;;; referenced operand position primitives are not externed here
     ;;; but dont know what are the risk
     (emit op ".globl ~a" entry-name)
     (emit op ".extern L_apply")
     (for-each (lambda (g) (emit op ".extern ~a" g)) (set-union data unbounds))
     (emit op ".section .text")
     (for-each
       (lambda (label)
         (match label
           ((,name (code ,params ,variadic? ,fvs ,e))
            (emit-procedure op name params variadic? fvs e))
           ((,name (case-code ,fvs . ,clauses))
            (emit-variadic-procedure op name fvs clauses))
           (,() (error "emit-code" "unmatch" label))))
       labels)
     (emit op "~a:" entry-name)
     (emit op "push %ebp")
     (emit op "mov %esp, %ebp")
     (emit-expr op body (- WORDSIZE) (make-env))
     (emit op "mov $0, %eax")
     (emit op "pop %ebp")
     (emit op "ret"))
    (,() (error "emit-code" "unmatch" prog))))

(define (program->data prog)
  (match prog
    ((program (labels . ,()) (data . ,data) (unbounds . ,()) ,()) data)
    (,() (error "program->data" "unmatch" prog))))

(define (emit-obj input-path output-path k)
  (define tmp-filename (mk-tmpname (path-fileroot input-path) "s"))
  (let* ((prog (preprocess (cons 'begin (read-sexps-from-path input-path))))
         (entry-name (format "~a.main" (string->id-string (path-fileroot input-path))))
         (data (program->data prog))
         (op (open-output-file tmp-filename)))
    (emit-code op entry-name prog)
    (close-output-port op)
    #;
    (system* "gcc -fno-omit-frame-pointer -m32 -c ~a -o ~a"
             tmp-filename output-path)
    (system* "as --32 -o ~a ~a" output-path tmp-filename)
    (system* "rm -f ~a" tmp-filename)
    (k entry-name data)))

(define (read-meta obj-file)
  (let* ((filename (mk-tmpname "read-meta" "txt"))
          (_ (system* "objcopy --dump-section .main_entry=~a ~a"
                      filename obj-file))
          (inp (open-input-file filename))
          (meta (read inp)))
    (close-port inp)
    (system* "rm -f ~a" filename)
    meta))

(define (map3-k f xs k)
  (let recur ((xs xs) (k k))
    (if (not (pair? xs))
        (k '() '() '())
        (f (car xs)
          (lambda (x y z)
            (recur (cdr xs)
              (lambda (xs ys zs)
                (k (cons x xs) (cons y ys) (cons z zs)))))))))

(define (mk-tmpname f ext)
  (format "/dev/shm/scm-build-~a-~a.~a" (path-fileroot f) (random-string 8) ext))

(define (emit-objs input-paths k)
  (map3-k
    (lambda (i k)
      (if (equal? (path-extension i) "o")
          (let ((out (mk-tmpname i "o")))
            (system* "cp ~a ~a" i out)
            (k out (read-meta out) (lambda () (system* "rm -f ~a" out))))
          (let ((out (mk-tmpname i "o")))
            (emit-obj i out
              (lambda (entry-name data) (k out (list (list entry-name) data) (lambda () (system* "rm -f ~a" out))))))))
    input-paths
    k))

(define (combine-objs output-path objs metas)
  (define entries (apply append (map car metas)))
  (define globals (apply set-union (map cadr metas)))
  #;
  (system* "gcc -m32 -r -o ~a ~a"
           output-path
           (apply string-append
                  (map (lambda (s) (string-append " \"" s "\" ")) objs)))
  (system* "ld -m elf_i386 -r -o ~a ~a"
           output-path
           (apply string-append
                  (map (lambda (s) (string-append " \"" s "\" ")) objs)))
  (system* "objcopy --remove-section=.main_entry --add-section .main_entry=<(echo ~s) ~a"
           (format "~s" (list entries globals))
           output-path))

(define (linking-to-exe output-path objs metas)
  (define (make-prim-call pr . es)
    `(call #f (prim ,pr) . ,es))
  (define (make-box-init e)
    (make-prim-call 'cons e '(quote ())))
  (define entries (apply append (map car metas)))
  (define globals (apply set-union (map cadr metas)))
  (define tmp-filename (mk-tmpname (path-fileroot output-path) "s"))
  (let* ((op (open-output-file tmp-filename)))
    (emit op ".globl main")
    (emit op ".globl L_apply")
    (for-each (lambda (g) (emit op ".globl ~a" g)) globals)

    (emit op ".section .data")
    (emit op ".globl global_table")
    (emit op "global_table:")
    (for-each
      (lambda (g) (emit op ".~abyte ~a" WORDSIZE g))
      globals)
    (emit op ".~abyte 0" WORDSIZE)
    (for-each
      (lambda (g)
      (match g
        (,()
         (guard (symbol? g))
         (emit op ".p2align 3")
         (emit op "~a: .~abyte 0" g WORDSIZE))
        (,() (error "linking-to-exe" "unmatch" g))))
      globals)
    (emit op ".section .text")
    (emit-L-apply op)
    (emit op "main:")
    (emit op "mov %esp, BOT_EBP")
    (emit op "mov %esp, %ebp")
    (emit op "mov ~a(%esp), %eax" (* WORDSIZE 2))
    (emit op "mov %eax, ARGV")
    (emit op "mov ~a(%esp), %eax" (* WORDSIZE 1))
    (emit op "mov %eax, ARGC")
    (emit op "call init_heap")
    (for-each
      (lambda (g)
        (emit-expr op `(set-label! ,g ,(make-box-init 0)) (- WORDSIZE) #f))
      globals)
    (for-each
      (lambda (entry) (emit op "call ~a" entry))
      entries)
    (emit op "mov $0, %eax")
    (emit op "ret")
    (close-output-port op))
  (system* "gcc -fno-omit-frame-pointer -m32 ~a ~a runtime.o -o ~a"
           tmp-filename
           (apply string-append
                  (map (lambda (s) (string-append " \"" s "\" ")) objs))
            output-path)
  (system* "rm -f ~a" tmp-filename))

(match (cdr (command-line))
  (("-o" ,output-path . ,input-paths)
   (emit-objs input-paths
    (lambda (objs metas frees)
      ((if (equal? (path-extension output-path) "o") combine-objs linking-to-exe) output-path objs metas)
      (map (lambda (f) (f)) frees))))
  (("--make-prim-lib" ,output-path)
   (write-primitive-lib output-path))
  (,input-paths
   (emit-objs input-paths
    (lambda (objs metas frees)
      (let ((output-path (mk-tmpname "tmp" "out")))
        (linking-to-exe output-path objs metas)
        (system* output-path)
        (system* "rm -f ~a" output-path)
        (map (lambda (f) (f)) frees)))))
  (,cmd-ln (error (car (command-line)) "unmatch" cmd-ln)))
)