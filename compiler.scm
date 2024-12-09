(import
  (front)
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
  (callee-save ebp ebx edi esi esp))
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
  (code-pointer fx-free-vars-counts&lambda fv0 fv1 ...)
  ; static
  (code-pointer 1)
|#

#|TODO:
- optimize known call like label-call, how it can be tail call
- simpler implementation of converting letrec
- %esi shall be used as the heap pointer
- inline requested size check rather than rely on gc-flip
|#

(define FIXNUM-MASK   #b00000011)
(define FIXNUM-TAG    #b00000000)
(define FIXNUM-SHIFT  2)
(define IMM-SHIFT     8)
(define IMM-MASK      #b11111111)
(define CHAR-TAG      #b00001111)
(define BOOL-TAG      #b00011111)
(define NIL-TAG       #b01011111)
(define FALSE-IMM BOOL-TAG)
(define TRUE-IMM (bitwise-ior (ash 1 IMM-SHIFT) BOOL-TAG))
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

(define (emit-seq op es si env)
  (for-each
    (lambda (e) (emit-expr op e si env))
    es))

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
            (extend-env (caar bindings) (format "mov ~a(%esp), %eax" si) inner-env)))
        (emit-seq op body si inner-env))))

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
  (emit-walk-stack op (+ (add1 (string-length s)) WORDSIZE) si (make-env))
  (emit op "mov free_ptr, %eax")
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
  (let ((sz (length es)))
    (emit-walk-stack op (* (+ sz 1) WORDSIZE) si (make-env))
    (emit op "mov free_ptr, %eax")
    (emit op "movl $~a, (%eax)" (immediate-rep sz))
    (emit op "addl $~a, free_ptr" (align-to-multiple 8 (* (+ sz 1) WORDSIZE)))
    (emit op "or $~a, %eax" VEC-TAG)
    (emit op "mov %eax, ~a(%esp)" si)
    (if (<= sz 0)
        #t
        (begin
          (for-each
            (lambda (i e)
              (emit-expr op e (- si WORDSIZE) env)
              (emit op "mov ~a(%esp), %ecx" si)
              (emit op "mov %eax, ~a(%ecx)" (- (* (+ i 1) WORDSIZE) VEC-TAG)))
            (iota sz)
            es)
          (emit op "mov %ecx, %eax")))))

(define (emit-prim op e si env)
  (match e
    ((%walk-stack)
     (emit-walk-stack op -1 si env))
    ((%walk-stack ,e)
     (emit-walk-stack op e si env))
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
    ((cons ,e1 ,e2)
     (emit-expr op e1 si env)
     (emit op "mov %eax, ~a(%esp)" si)
     (emit-expr op e2 (- si WORDSIZE) env)
     
     (emit op "mov %eax, ~a(%esp)" (- si WORDSIZE))
     (emit-walk-stack op (* 2 WORDSIZE) (- si (* 2 WORDSIZE)) (make-env))
     
     (emit op "mov free_ptr, %ecx")
     (emit op "addl $~a, (free_ptr)" (* 2 WORDSIZE))
     (emit op "mov ~a(%esp), %eax" (- si WORDSIZE))
     (emit op "mov %eax, ~a(%ecx)" WORDSIZE)
     (emit op "mov ~a(%esp), %eax" si)
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
    ((make-string ,k)
     (emit-expr op `(prim-call make-string ,k #\nul) si env))
    ((make-string ,k ,ch)
     (emit-expr op ch si env)
     (emit op "mov %eax, ~a(%esp)" si)
     (emit-expr op k (- si WORDSIZE) env)
     (emit op "mov %eax, ~a(%esp)" (- si WORDSIZE))

     (emit op "add $~a, %eax" (immediate-rep (add1 WORDSIZE)))
     (emit-walk-stack op '%eax (- si (* 2 WORDSIZE)) (make-env))
     
     (emit op "mov free_ptr, %edx")
     (emit op "mov free_ptr, %edi")
     (emit op "mov ~a(%esp), %eax" (- si WORDSIZE))
     (emit op "mov %eax, (%edi)") ; store the length
     ; align and fill
     (emit op "sar $~a, %eax" FIXNUM-SHIFT)
     (emit op "mov %eax, %ecx")   ; store the length again
     (emit op "add $12, %eax")    ; extra space for null (11 + 1)
     (emit op "and $-8, %eax")
     (emit op "add %eax, (free_ptr)")
     (emit op "add $~a, %edi" WORDSIZE)
     (emit op "mov ~a(%esp), %eax" si)  ; restore the character
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
    ((make-vector ,k)
     (emit-expr op k si env)
     (emit op "mov %eax, ~a(%esp)" si)

     (emit op "imul $~a, %eax" WORDSIZE)
     (emit op "add $~a, %eax" (immediate-rep WORDSIZE))
     (emit-walk-stack op '%eax (- si WORDSIZE) (make-env))
     
     (emit op "mov ~a(%esp), %eax" si)
     (emit op "mov free_ptr, %edx")
     (emit op "mov free_ptr, %edi")
     (emit op "mov %eax, (%edi)") ; store the length
     (emit op "mov %eax, %ecx")
     (emit op "add $11, %ecx")
     (emit op "and $-8, %ecx")
     (emit op "add %ecx, (free_ptr)")
     (emit op "mov free_ptr, %edi")
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
  (cond
    ((not e)
     (emit op "mov $~a, %eax" (immediate-rep -1)))
    ((eq? e '%eax) #t)
    (else (emit-expr op e si env)))
  (emit op "lea ~a(%esp), %ecx" (+ si WORDSIZE))
  (emit op "mov %esp, %edx")
  (emit op "sub $~a, %esp" (- (abs si) WORDSIZE))
  (emit op "push %edx")
  (emit op "push %ecx")
  (emit op "push %eax")
  (emit op "call walk_stack")
  (emit op "add $~a, %esp" (+ (abs si) (* 2 WORDSIZE)))
  (emit op "mov ~a(%esp), %ebp" (- WORDSIZE))
  )

(define (emit-tail-call op fn args si env)
  (let* ((argc (length (cons fn args)))
         (calc-offset (lambda (i) (- si (* WORDSIZE i)))))
    (for-each
      (lambda (offset arg)
        (emit-expr op arg offset env)
        (emit op "mov %eax, ~a(%esp)" offset))
      (map calc-offset (iota argc))
      (cons fn args))
    
    ;;; NOTE: It's easier to collect before entering variadic function.
    (emit-walk-stack op argc (calc-offset argc) env)
    ;;; copying the memory
    (for-each
      (lambda (i)
        (emit op "mov ~a(%esp), %eax" (calc-offset i))
        (emit op "mov %eax, ~a(%esp)" (* (- WORDSIZE) (+ i 1))))
      (iota argc))
    #;(begin
      #|
      ;;; copying the memory (alternative impl)
      but it slow down the compiler significantly (4x times)
      possible reason:
      - alignment
      - function usually is not called with too many arguments
      |#
      (emit op "lea ~a(%esp), %esi" si)
      (emit op "lea ~a(%esp), %edi" (- WORDSIZE))
      (emit op "mov $~a, %ecx" argc)
      (emit op "std") ;;; set the flag to decrement stack pointer while copying
      (emit op "rep movsl")
      (emit op "cld") ;;; maintain the invariant that copying is incrementing
    )
    
    (emit op "mov $~a, %ecx" argc)
    (emit op "mov ~a(%esp), %eax" (- WORDSIZE))
    (emit op "mov ~a(%eax), %eax" (- CLOS-TAG))
    (emit op "add $~a, %esp" WORDSIZE)
    (emit op "jmp *%eax")))

(define (emit-call op fn args si env)
  (let* ((argc (length (cons fn args)))
         (start-si (- si (* 2 WORDSIZE)))
         (calc-offset (lambda (i) (- start-si (* WORDSIZE i)))))
    (emit op "movl $0, ~a(%esp)" si)
    (emit op "movl %esp, ~a(%esp)" (- si WORDSIZE))
    (for-each
      (lambda (offset arg)
        (emit-expr op arg offset env)
        (emit op "mov %eax, ~a(%esp)" offset))
      (map calc-offset (iota argc))
      (cons fn args))
    ;;; NOTE: It's easier to collect before entering variadic function.
    (emit-walk-stack op argc (calc-offset argc) env)
    (emit op "mov ~a(%esp), %eax" start-si)
    (emit op "mov ~a(%eax), %eax" (- CLOS-TAG))
    (emit op "mov $~a, %ecx" argc)
    (emit op "sub $~a, %esp" (- (abs si) WORDSIZE))
    (emit op "call *%eax")
    (emit op "add $~a, %esp" (- (abs si) WORDSIZE))
    (emit op "mov ~a(%esp), %ebp" (- WORDSIZE))
    ))

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

    (emit op "mov free_ptr, %edi")
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
          (lambda (i) (format "mov ~a(%esp), %eax" (* (- WORDSIZE) (+ i 1)))))
         (calc-free-offset
          (lambda (i)
            (format "mov ~a(%ebp), %eax"
                    (- (* WORDSIZE (+ i 2)) CLOS-TAG))))
         (offsets (map calc-offset (iota argc)))
         (env (extend-env params offsets (make-env)))
         (env (extend-env fvs (map calc-free-offset (iota (length fvs))) env))
         (si (* (- WORDSIZE) (+ argc 1))))
    (emit op "~a: " fn)
    (emit op "sub $~a, %esp" WORDSIZE)
    (if (not variadic?)
        (let ((end (generate-label 'conseq)))
          (emit op "cmp $~a, %ecx" (length params))
          (emit op "je ~a" end)
          (emit-foreign-call op 's_proc_arg_err '() si env)
          (emit op "~a: " end))
        (emit-construct-vararg op argc si))
    (emit op "mov ~a(%esp), %ebp" (- WORDSIZE))
    (emit-seq op body si env)
    (emit op "add $~a, %esp" WORDSIZE)
    (emit op "ret")))

(define (emit-variadic-procedure op fn fvs clauses)
  (let* ((calc-free-offset
          (lambda (i)
            (format "mov ~a(%esp), %eax\nmov ~a(%eax), %eax"
                    (- (* WORDSIZE 2)) (- (* WORDSIZE (+ i 2)) CLOS-TAG))))
         (env (extend-env fvs (map calc-free-offset (iota (length fvs))) (make-env)))
         (si (- (* 3 WORDSIZE))))
    (emit op "~a: " fn)
    (for-each
      (lambda (clause)
        (match clause
          ((,argc ,variadic? closure ,fn ,fvs)
           (let ((next (generate-label "case_lambda_altern")))
            
            (if variadic?
                (begin
                  (emit op "cmp $~a, %ecx" (sub1 argc))
                  (emit op "jl ~a" next))
                (begin
                  (emit op "cmp $~a, %ecx" argc)
                  (emit op "jne ~a" next)))
            (emit op "mov %ecx, %edx")
            (emit-closure op fn fvs si env)
            (emit op "mov %eax, ~a(%esp)" (- (* WORDSIZE 2)))
            (emit op "mov %edx, %ecx")
            (emit op "jmp ~a" fn)
            (emit op "~a: " next)))
          (,() (error "emit-variadic-procedure" "unknown clause" clause))))
      clauses)
    (emit-foreign-call op 's_proc_arg_err '() (- (* 2 WORDSIZE)) env)))

(define (emit-closure op fn fvs si env)
  (define fvs-count (length fvs))
  (emit op "mov free_ptr, %ecx")
  (emit op "addl $~a, (free_ptr)"
            (align-to-multiple 8 (* WORDSIZE (+ 2 fvs-count))))
  (emit op "mov $~a, %eax" fn)
  (emit op "mov %eax, (%ecx)")
  (emit op "movl $~a, ~a(%ecx)" (immediate-rep fvs-count) WORDSIZE)
  (for-each
    (lambda (fv i)
      (emit op (apply-env fv env))
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
    ('()
     (emit op "mov $~a, %eax" (immediate-rep '())))
    ((global ,lbl ,())
     (emit op "mov ~a, %eax" lbl))
    ((var ,x)
     (emit op (apply-env x env)))
    ((prim ,p)
     (emit-prim op `(car (global ,(symbol->id-symbol p) ,p)) si env))
    ((set-label! ,lbl ,e)
     (emit-expr op e si env)
     (emit op "mov %eax, ~a" lbl))
    ((begin . ,es)
     (emit-seq op es si env))
    ((let ,bindings . ,body)
     (emit-let op bindings body si env))
    ((if ,pred ,conseq ,altern)
     (emit-if op pred conseq altern si env))
    ((closure ,fn ,fvs)
     (emit-walk-stack op (+ 2 (length fvs)) si env)
     (emit-closure op fn fvs si env))
    ((prim-call . ,e)
     (emit-prim op e si env))
    ((call ,fn . ,es)
     (emit-call op fn es si env))
    ((foreign-call ,fn . ,es)
     (emit-foreign-call op fn es si env))
    ((tail-call ,fn . ,es)
     (emit-tail-call op fn es si env))
    ((tail-apply ,fn ,es)
     (emit-tail-apply op fn es si env))
    ((apply ,fn ,es)
     (emit-apply op fn es si env))
    (,() (error "emit-expr" "unmatch" e))))

(define (emit-main op entry-name body env)
  (emit op "~a:" entry-name)
  (emit op "push %ebp")
  (emit op "mov %esp, %ebp")
  (emit-expr op body (- WORDSIZE) env)
  (emit op "mov $0, %eax")
  (emit op "pop %ebp")
  (emit op "ret"))

(define (emit-data op data)
  (emit op ".section .data")
  (for-each
    (lambda (datum)
     (match datum
      (,x
       (guard (symbol? x))
       (emit op ".p2align 3")
       (emit op "~a: .~abyte 0" x WORDSIZE))
      (,() (error "emit-data" "unmatch" datum))))
    data))

(define (emit-code op entry-name prog)
  (match prog
    ((program (labels . ,labels) (data . ,data) (unbounds . ,unbounds) . ,body)
     (let* ((mov-label-txt (lambda (l) (format "mov ~a, %eax" l)))
            (datum-labels (set-union data unbounds))
            (env (extend-env datum-labels
                             (map mov-label-txt datum-labels)
                             (make-env))))
      (for-each (lambda (g) (emit op ".extern ~a" g)) datum-labels)
      (emit op ".extern L_apply")
      (emit op ".section .text")
      (emit op ".globl ~a" entry-name)
      (for-each
        (lambda (label)
          (match label
            ((,name (code ,params ,variadic? ,fvs . ,es))
             (emit-procedure op name params variadic? fvs es))
            ((,name (case-code ,fvs . ,clauses))
             (emit-variadic-procedure op name fvs clauses))
            (,() (error "emit-code" "unmatch" label))))
        labels)
      (emit-main op entry-name (cons 'begin body) env)))
    (,() (error "emit-code" "unmatch" prog))))

(define (program->data prog)
  (match prog
    ((program (labels . ,()) (data . ,data) (unbounds . ,()) . ,()) data)
    (,() (error "program->data" "unmatch" prog))))

(define (emit-obj input-path output-path)
  (define tmp-filename
    (format "/tmp/scm-build/~a-~a.s"
            (path-fileroot input-path)
            (random-string 8)))
  (system "mkdir -p /tmp/scm-build")
  (system* "rm -f ~a" tmp-filename)
  (let* ((prog
          (preprocess
            (list (lambda (path)
                    (cons 'begin (read-sexps-from-path input-path)))
                  uniquify-program
                  lift-symbol-program
                  convert-assignment-program
                  explicate-program
                  uncover-free-program
                  reveal-function-program)
            input-path))
         (entry-name (format "~a.main" (string->id-string (path-fileroot input-path))))
         (data (program->data prog))
         (op (open-output-file tmp-filename)))
    (emit-code op entry-name prog)
    (close-output-port op)
    (system* "gcc -fomit-frame-pointer -m32 -c ~a -o ~a"
             tmp-filename output-path)
    (system* "rm -f ~a" tmp-filename)
    (system* "objcopy --add-section .main_entry=<(echo ~s) ~a"
             (format "~s" (list (list entry-name) data))
             output-path)))

(define (read-meta obj-filenames)
  (define (read-meta obj-file)
    (let* ((filename (format "/tmp/scm-build/~a.txt" (random-string 8)))
           (_ (system* "objcopy --dump-section .main_entry=~a ~a"
                       filename obj-file))
           (inp (open-input-file filename))
           (meta (read inp)))
      (close-port inp)
      meta))
  (define metas (map read-meta obj-filenames))
  (define entries (apply append (map car metas)))
  (define globals (apply set-union (map cadr metas)))
  (list entries globals))

(define (emit-objs input-paths)
 (system "mkdir -p /tmp/scm-build/")
 (map
    (lambda (i)
      (if (equal? (path-extension i) "o")
          i
          (let ((out
                 (format "/tmp/scm-build/~a-~a.o" (path-fileroot i) (random-string 8))))
            (emit-obj i out)
            out)))
    input-paths))

(define (combine-objs output-path . objs)
  (define meta (read-meta objs))
  (define entries (car meta))
  (define globals (cadr meta))
  (system* "gcc -m32 -r -o ~a ~a"
           output-path
           (apply string-append
                  (map (lambda (s) (string-append " \"" s "\" ")) objs)))
  (system* "objcopy --update-section .main_entry=<(echo ~s) ~a"
           (format "~s" (list entries globals))
           output-path))

(define (linking-to-exe output-path . objs)
  (define meta (read-meta objs))
  (define entries (car meta))
  (define globals (cadr meta))
  (define tmp-filename
    (format "/tmp/scm-build/~a-~a.s"
            (path-fileroot output-path)
            (random-string 8)))
  (system* "rm -f ~a" tmp-filename)
  (let* ((op (open-output-file tmp-filename)))
    (emit-data op globals)
    (emit op ".section .text")
    (emit op ".globl main")
    (emit op ".globl L_apply")
    (for-each
      (lambda (g) (emit op ".globl ~a" g))
      globals)
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
        (emit-expr op `(set-label! ,g (prim-call cons 0 '())) (- WORDSIZE) #f)
        (emit op "mov $~a, %eax" g)
        (emit op "mov GLOBALS, %ecx")
        (emit op "mov %eax, (%ecx)")
        (emit op "addl $~a, GLOBALS" WORDSIZE))
      globals)
    (for-each
      (lambda (entry) (emit op "call ~a" entry))
      entries)
    (emit op "mov $0, %eax")
    (emit op "ret")
    (close-output-port op))
  (system* "gcc -fomit-frame-pointer -m32 ~a ~a runtime.o -o ~a"
           tmp-filename
           (apply string-append
                  (map (lambda (s) (string-append " \"" s "\" ")) objs))
            output-path)
  (system* "rm -f ~a" tmp-filename))

(match (cdr (command-line))
  (("--combine" ,output-path . ,input-paths)
   (apply combine-objs (cons output-path (emit-objs input-paths))))
  (("-o" ,output-path . ,input-paths)
   (apply linking-to-exe (cons output-path (emit-objs input-paths))))
  (("--make-prim-lib" ,output-path)
   (write-primitive-lib output-path))
  (,cmd-ln (error "compiler.scm" "unmatch" cmd-ln)))
