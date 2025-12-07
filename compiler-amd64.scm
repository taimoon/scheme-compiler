(import
  (only (match) match)
  (only (utils)
    align-to-multiple gensym*
    flatmap zip applicate
    read-sexps-from-path path-filestem path-extension
    make-tempname system* maybe-getenv)
  (only (front) preprocess))

(define REGISTERS
  '(%rax %rbx %rcx %rdx %rsi %rdi
    %rbp %rsp
    %r8 %r9 %r10 %r11 %r12 %r13 %r14 %r15
    %eax %ecx %edx
    %cl %dl))

(define code-gen (let ()
(define FIXNUM-TAG        #b000)
(define PAIR-TAG          #b001)
(define FLO-TAG           #b010)
#;(define FIXNUM-OCCUPIED #b100) ; OCCUPIED by fixnums on 32-bits machines
(define SYM-TAG           #b011)
(define CLOS-TAG          #b101)
(define IMM-TAG           #b110)
(define OBJ-TAG           #b111)

(define PTR-MASK      #b111)
(define WORDSIZE      8)
(define FIXNUM-SHIFT  3)
(define FIXNUM-MASK   #b111)
(define IMM-SHIFT     8)
(define IMM-MASK      #b11111111)
(define BOOL-TAG      #b00000110)
(define CHAR-TAG      #b00001110)
(define NIL-TAG       #b00010110)
(define EOF-TAG       #b00011110)
(define VOID-TAG      #b00100110)
(define UNBOUND-TAG   #b01000110)
(define ENTRY-MARK    (bitwise-ior (ash 1 IMM-SHIFT) UNBOUND-TAG))

(define FALSE-IMM     BOOL-TAG)
(define TRUE-IMM      (bitwise-ior (ash 1 IMM-SHIFT) BOOL-TAG))

;;; meta tag for object
(define VEC-TAG     #b000)
(define BYTEVEC-TAG #b001)
(define STR-TAG     #b010)

;;; others
(define OBJ-MASK      #b111)
(define STR-MASK      FIXNUM-MASK)
(define VEC-MASK      FIXNUM-MASK)
(define BYTEVEC-MASK  FIXNUM-MASK)

(define (immediate-repr e)
  (cond
    ((integer? e) (ash e FIXNUM-SHIFT))
    ((char? e) (bitwise-ior (ash (char->integer e) IMM-SHIFT) CHAR-TAG))
    ((boolean? e) (if e TRUE-IMM FALSE-IMM))
    ((null? e) NIL-TAG)
    ((eq? e (void)) VOID-TAG)
    ((eq? e (eof-object)) EOF-TAG)
    (else (error "immediate-repr" "unknown" e))))

(define CALLEE-SAVE-REG
  '(%rbx %rbp %rsp %r12 %r13 %r14 %r15))
(define CALLER-SAVE-REG
  '(%rax %rcx %rdx %rsi %rdi %r8 %r9 %r10 %r11))
(define ARGS-REG
  '(%rdi %rsi %rdx %rcx %r8 %r9))

(define AP-REG '%r12)
(define FP-REG '%rbp)

(define (gen-simple* e dst tmp)
  (define mem? pair?)
  (define mov2dst
    (if (mem? dst)
        `((mov ,tmp ,dst))
        '()))
  (match e
    (,()
     (guard (or (integer? e) (char? e) (boolean? e)))
     `((movq ,(immediate-repr e) ,dst)))
    ((quote ,q)
     `((movq ,(immediate-repr q) ,dst)))
    ((label ,lbl)
     `((mov (GOTPCREL ,lbl %rip) ,tmp)
       (mov (,tmp) ,tmp)
       . ,mov2dst))
    ((label* ,lbl)
     `((movq (,lbl %rip) ,tmp)
       . ,mov2dst))
    ((call ,tail? (prim local-ref) ,i)
     `((mov (,FP-REG ,(* (- WORDSIZE) (+ i 2))) ,tmp)
       . ,mov2dst))
    ((call ,tail? (prim closure-ref) ,clos -1)
     `((mov (,FP-REG ,(* (- WORDSIZE) 2)) ,tmp)
       . ,mov2dst))
    ((call ,tail? (prim closure-ref) ,clos ,i)
     (guard (>= i 0))
     `((mov (,FP-REG ,(* (- WORDSIZE) 2)) ,tmp)
       (mov (,tmp ,(- (* (+ i 2) WORDSIZE) CLOS-TAG)) ,tmp)
       . ,mov2dst))
    (,() (error "gen-simple*" "unmatch" e))))

(define gen-simple
  (let ((mem? pair?))
    (case-lambda
      ((e dst)
       (if (mem? dst)
           (error "gen-simple" "temp reg needed for mem dst")
           (gen-simple* e dst dst)))
      ((e dst tmp)
       (if (mem? dst)
           (gen-simple* e dst tmp)
           (error "gen-simple" "temp reg is not needed for reg dst"))))))

(define (gen-align align off)
  `(
    (add ,(- align 1) ,off)
    (and ,(- align) ,off)
    ))

(define (gen-eq dst src val tmp)
  `(
    (cmp ,val ,src)
    (mov ,FALSE-IMM ,tmp)
    (mov ,TRUE-IMM ,dst)
    (cmovne ,tmp ,dst)
    ))

(define (gen-obj-test e mask tag)
  (define csq (gensym* "conseq"))
  (define alt (gensym* "altern"))
  (define end (gensym* "end"))
  `(
      ,@(gen-simple e '%rax)
      (mov %rax %rcx)
      (and ,OBJ-MASK %rcx)
      (cmp ,OBJ-TAG %rcx)
      (jne ,alt)
      ,csq
      (mov (%rax ,(- OBJ-TAG)) %rax)
      (and ,mask %rax)
      (cmp ,tag %rax)
      (mov ,TRUE-IMM %rcx)
      (mov ,FALSE-IMM %rax)
      (cmove %rcx %rax)
      (jmp ,end)
      ,alt
      (mov ,FALSE-IMM %rax)
      ,end
    ))


;;; TODO: fix the size here
(define (gen-str vs si)
  (define sz (align-to-multiple 8 (+ WORDSIZE (* (length vs) 4))))
  `(
    ,@(gen-collect (ash sz (- FIXNUM-SHIFT)) si)
    (mov (,AP-REG) %rax)
    (addq ,sz (,AP-REG))
    (movq ,(bitwise-ior STR-TAG (immediate-repr (length vs))) (%rax))
    ,@(flatmap
        (lambda (i v)
          `(
            ,@(gen-simple v '%rcx)
            (movl %ecx (%rax ,(+ (* i 4) WORDSIZE)))
          ))
        (iota (length vs))
        vs)
    (or ,OBJ-MASK %rax)
  ))

(define (gen-vec vs si)
  (define vlen (length vs))
  (define sz (align-to-multiple 8 (* (+ vlen 1) WORDSIZE)))
  `(
    ,@(gen-collect sz si)
    (mov (,AP-REG) %rax)
    (addq ,sz (,AP-REG))
    (movq ,(bitwise-ior VEC-TAG (immediate-repr vlen)) (%rax))
    ,@(flatmap
        (lambda (i v)
          (gen-simple v `(%rax ,(* (+ i 1) WORDSIZE)) '%rcx))
        (iota vlen)
        vs)
    (or ,OBJ-MASK %rax)
  ))

(define (gen-foreign-call fn args si)
  (define argc (length args))
  (if (not (< argc 6))
      (error "foreign-call" "support-only-at-most-6-args"))
  `(
    ,@(flatmap
        (applicate (lambda (arg dst) (gen-simple arg dst)))
        (zip args ARGS-REG))
    (call (PLT ,fn))
    ))

(define cmp-suffix
  '(
    (< jl cmovl)
    (<= jle cmovle)
    (eq? je cmove)
    (= je cmove)
    (>= jge cmovge)
    (> jg cmovg)
  ))

(define (gen-cmp cmp e1 e2 si)
  (define cmov (caddr (assq cmp cmp-suffix)))
  `(
    ,@(gen-simple e2 '%rcx)
    ,@(gen-simple e1 '%rax)
    (cmp %rcx %rax)
    (mov ,TRUE-IMM %rcx)
    (mov ,FALSE-IMM %rax)
    (,cmov %rcx %rax)
  ))

(define (gen-prim e si)
  (match e
    ((local-set! ,i ,e1)
     `(
      ,@(gen-expr e1 si)
      (mov %rax (,FP-REG ,(* (- WORDSIZE) (+ i 2))))
      ))
    ((eq? ,e1 ,e2)
     (gen-cmp 'eq? e1 e2 si))
    ((boolean? ,e1)
     `(
      ,@(gen-simple e1 '%rax)
      (and ,IMM-MASK %rax)
      ,@(gen-eq '%rax '%rax BOOL-TAG '%rcx)
      ))
    ((not ,e1)
     `(
        ,@(gen-simple e1 '%rax)
        (cmp ,FALSE-IMM %rax)
        (mov ,FALSE-IMM %rax)
        (mov ,TRUE-IMM %rcx)
        (cmove %rcx %rax)
      ))
    ((eof-object? ,e1)
     `(
      ,@(gen-simple e1 '%rax)
      (and ,IMM-MASK %rax)
      ,@(gen-eq '%rax '%rax EOF-TAG '%rcx)
      ))
    ((null? ,e1)
     `(
      ,@(gen-simple e1 '%rax)
      (and ,IMM-MASK %rax)
      ,@(gen-eq '%rax '%rax NIL-TAG '%rcx)
      ))
    ((char? ,e1)
     `(
      ,@(gen-simple e1 '%rax)
      (and ,IMM-MASK %rax)
      ,@(gen-eq '%rax '%rax CHAR-TAG '%rcx)
      ))
    ((char->integer ,e1)
     `(
      ,@(gen-simple e1 '%rax)
      (sar ,(- IMM-SHIFT FIXNUM-SHIFT) %rax)
      ))
    ((integer->char ,e1)
     `(
      ,@(gen-simple e1 '%rax)
      (sal ,(- IMM-SHIFT FIXNUM-SHIFT) %rax)
      (or ,CHAR-TAG %rax)
      ))
    ((integer? ,e1)
     `(
      ,@(gen-simple e1 '%rax)
      (and ,FIXNUM-MASK %rax)
      ,@(gen-eq '%rax '%rax FIXNUM-TAG '%rcx)
      ))
    ((,cmp ,e1 ,e2)
     (guard (assq cmp cmp-suffix))
     (gen-cmp cmp e1 e2 si))
    ((- ,e1 ,e2)
     `(
      ,@(gen-simple e2 '%rcx)
      ,@(gen-simple e1 '%rax)
      (sub %rcx %rax)
      ))
    ((+ ,e1 ,e2)
     `(
      ,@(gen-simple e2 '%rcx)
      ,@(gen-simple e1 '%rax)
      (add %rcx %rax)
      ))
    ((* ,e1 ,e2)
     `(
      ,@(gen-simple e2 '%rcx)
      ,@(gen-simple e1 '%rax)
      (sar ,FIXNUM-SHIFT %rax)
      (imul %rcx %rax)
      ))
    ((div ,dividend ,divisor)
     #;((div (* t x) (* t y))
        => (div x y)
        => (tag (div x y)))
     `(
        ,@(gen-simple divisor '%rcx)
        ,@(gen-simple dividend '%rax)
        (cqo)
        (idiv %rcx)
        (sal ,FIXNUM-SHIFT %rax)
      ))
    ((mod ,dividend ,divisor)
     ;;; TODO: don't know why this works?
     `(
        ,@(gen-simple divisor '%rcx)
        ,@(gen-simple dividend '%rax)
        (cqo)
        (idiv %rcx)
        (mov %rdx %rax)
     ))
    ((abs ,e1)
     `(
      ,@(gen-simple e1 '%rax)
      (mov %rax %rcx)
      (neg %rcx)
      (cmp 0 %rax)
      (cmovl %rcx %rax)
      ))
    ((max ,e1 ,e2)
     `(
      ,@(gen-simple e2 '%rcx)
      ,@(gen-simple e1 '%rax)
      (cmp %rcx %rax)
      (cmovl %rcx %rax)
      ))
    ((ash ,e1 ,e2)
     (define altern (gensym* "altern"))
     (define end (gensym* "end"))
     `(
      ,@(gen-simple e2 '%rcx)
      ,@(gen-simple e1 '%rax)
      (sar ,FIXNUM-SHIFT %rcx)
      (cmp 0 %rcx)
      (jl ,altern)
      ; e2 >= 0
      (sal %cl %rax)
      (jmp ,end)
      ,altern
      ; e2 < 0
      (neg %rcx)
      (sar ,FIXNUM-SHIFT %rax)
      (sar %cl %rax)
      (sal ,FIXNUM-SHIFT %rax)
      ,end
     ))
    ((bitwise-and ,e1 ,e2)
     `(
      ,@(gen-simple e2 '%rcx)
      ,@(gen-simple e1 '%rax)
      (and %rcx %rax)
     ))
    ((bitwise-ior ,e1 ,e2)
     `(
      ,@(gen-simple e2 '%rcx)
      ,@(gen-simple e1 '%rax)
      (or %rcx %rax)
     ))
    ((pair? ,e1)
     `(
      ,@(gen-simple e1 '%rax)
      (and ,OBJ-MASK %rax)
      ,@(gen-eq '%rax '%rax PAIR-TAG '%rcx)
      ))
    ((cons ,e1 ,e2)
     `(
      ,@(gen-collect 2 si)
      ,@(gen-simple e2 '%rdx)
      ,@(gen-simple e1 '%rcx)
      (mov (,AP-REG) %rax)
      (addq ,(* 2 WORDSIZE) (,AP-REG))
      (mov %rcx (%rax))
      (mov %rdx (%rax ,WORDSIZE))
      (lea (%rax ,PAIR-TAG) %rax)
      ))
    ((car ,e)
     `(
      ,@(gen-simple e '%rax)
      (mov (%rax ,(- PAIR-TAG)) %rax)
      ))
    ((cdr ,e)
     `(
      ,@(gen-simple e '%rax)
      (mov (%rax ,(- WORDSIZE PAIR-TAG)) %rax)
      ))
    ((set-car! ,e1 ,e2)
     `(
      ,@(gen-simple e2 '%rcx)
      ,@(gen-simple e1 '%rax)
      (mov %rcx (%rax ,(- PAIR-TAG))
      )))
    ((set-cdr! ,e1 ,e2)
     `(
      ,@(gen-simple e2 '%rcx)
      ,@(gen-simple e1 '%rax)
      (mov %rcx (%rax ,(- WORDSIZE PAIR-TAG)))
      ))
    ((string? ,e1)
     (gen-obj-test e1 STR-MASK STR-TAG))
    ((string . ,es)
     (gen-str es si))
    ((string-length ,e1)
     `(
      ,@(gen-simple e1 '%rax)
      (mov (%rax ,(- OBJ-TAG)) %rax)
      (sub ,STR-TAG %rax)
     ))
    ((string-ref ,e1 ,e2)
     `(
      ,@(gen-simple e1 '%rax)
      ,@(gen-simple e2 '%rcx)
      (sar 1 %rcx)
      (add ,(- WORDSIZE OBJ-TAG) %rcx)
      (movl (%rcx %rax) %eax)
     ))
    ((string-set! ,e1 ,e2 ,e3)
     `(
      ,@(gen-simple e1 '%rax)
      ,@(gen-simple e2 '%rcx)
      ,@(gen-simple e3 '%rdx)
      (sar 1 %rcx)
      (add ,(- WORDSIZE OBJ-TAG) %rcx)
      (mov %edx (%rcx %rax))
      (mov ,VOID-TAG %rax)
     ))
    ((make-string ,e1 ,e2)
     (define lp (gensym* "loop"))
     (define end (gensym* "end"))
     `(
      ,@(gen-simple e1 '%rax)
      (sar 1 %rax)
      (add ,WORDSIZE %rax)
      ,@(gen-align 8 '%rax)
      ,@(gen-collect '%rax si)
      ,@(gen-simple e1 '%rcx)
      (mov (,AP-REG) %rax)
      #;(addq ,(align-to-multiple 8 (+ WORDSIZE (* (string-length s) 4))) (,AP-REG))
      (sar 1 %rcx)
      (add ,WORDSIZE %rcx)
      ,@(gen-align 8 '%rcx)
      (add %rcx (,AP-REG))
      ,@(gen-simple e1 '%rcx)
      (or ,STR-TAG %rcx)
      (mov %rcx (%rax))
      ,@(gen-simple e1 '%rcx)
      ,@(gen-simple e2 '%rdx)
      ,lp
      ; %rcx < WORDSIZE
      (cmp ,WORDSIZE %rcx)
      (jl ,end)
      (mov %edx (%rax %rcx))
      (sub 4 %rcx)
      (jmp ,lp)
      ,end
      (or ,OBJ-MASK %rax)
     ))
    ((vector . ,es)
     (gen-vec es si))
    ((vector? ,e1)
     (gen-obj-test e1 VEC-MASK VEC-TAG))
    ((vector-length ,e1)
     `(
      ,@(gen-simple e1 '%rax)
      (mov (%rax ,(- OBJ-TAG)) %rax)
      (sub ,VEC-TAG %rax)
     ))
    ((vector-ref ,e1 ,e2)
     `(
      ,@(gen-simple e1 '%rax)
      ,@(gen-simple e2 '%rcx)
      (add ,(- WORDSIZE OBJ-TAG) %rcx)
      (mov (%rcx %rax) %rax)
     ))
    ((vector-set! ,e1 ,e2 ,e3)
     `(
      ,@(gen-simple e1 '%rax)
      ,@(gen-simple e2 '%rcx)
      ,@(gen-simple e3 '%rdx)
      (add ,(- WORDSIZE OBJ-TAG) %rcx)
      (mov %rdx (%rcx %rax))
     ))
    ((make-vector ,e1 ,e2)
     (define lp (gensym* "loop"))
     (define end (gensym* "end"))
     `(
      ,@(gen-simple e1 '%rax)
      (add ,WORDSIZE %rax)
      ,@(gen-align 8 '%rax)
      ,@(gen-collect '%rax si)
      (mov (,AP-REG) %rax)
      #;(addq ,(align-to-multiple 8 (+ WORDSIZE (* (vector-length v) WORDSIZE))) (,AP-REG))
      ,@(gen-simple e1 '%rcx)
      (add ,WORDSIZE %rcx)
      ,@(gen-align 8 '%rcx)
      (add %rcx (,AP-REG))
      ,@(gen-simple e1 '%rcx)
      (or ,VEC-TAG %rcx)
      (mov %rcx (%rax))
      ,@(gen-simple e1 '%rcx)
      ,@(gen-simple e2 '%rdx)
      ,lp
      ; %rcx < WORDSIZE
      (cmp ,WORDSIZE %rcx)
      (jl ,end)
      (mov %rdx (%rax %rcx))
      (sub ,WORDSIZE %rcx)
      (jmp ,lp)
      ,end
      (or ,OBJ-MASK %rax)
     ))
    ((bytevector? ,e1)
     (gen-obj-test e1 BYTEVEC-MASK BYTEVEC-TAG))
    ((make-bytevector ,e1 . ,maybe-e2)
     (define e2 (if (pair? maybe-e2) (car maybe-e2) #f))
     (define lp (gensym* "loop"))
     (define end (gensym* "end"))
     `(
      ,@(gen-simple e1 '%rax)
      (sar ,FIXNUM-SHIFT %rax)
      (add ,WORDSIZE %rax)
      ,@(gen-align 8 '%rax)
      ,@(gen-collect '%rax si)
      (mov (,AP-REG) %rax)
      #;(addq ,(align-to-multiple 8 (* (bytevector-length v) WORDSIZE)) (,AP-REG))
      ,@(gen-simple e1 '%rcx)
      (mov %rcx %rdx)
      (sar ,FIXNUM-SHIFT %rdx)
      (add ,WORDSIZE %rdx)
      ,@(gen-align 8 '%rdx)
      (add %rdx (,AP-REG))
      (or ,BYTEVEC-TAG %rcx)
      (mov %rcx (%rax))
      ,@(if e2
            `(
              ,@(gen-simple e1 '%rcx)
              (sar ,FIXNUM-SHIFT %rcx)
              (add ,(- WORDSIZE 1) %rcx)
              ,@(gen-simple e2 '%rdx)
              (sar ,FIXNUM-SHIFT %rdx)
              ,lp
              ; %rcx < WORDSIZE
              (cmp ,WORDSIZE %rcx)
              (jl ,end)
              (movb %dl (%rax %rcx))
              (sub 1 %rcx)
              (jmp ,lp)
              ,end)
            '())
      (or ,OBJ-MASK %rax)
      ))
    ((bytevector-length ,e1)
     `(
      ,@(gen-simple e1 '%rax)
      (mov (%rax ,(- OBJ-TAG)) %rax)
      (sub ,BYTEVEC-TAG %rax)
     ))
    ((bytevector-u8-ref ,e1 ,e2)
     `(
      ,@(gen-simple e1 '%rax)
      ,@(gen-simple e2 '%rcx)
      (sar ,FIXNUM-SHIFT %rcx)
      (add ,(- WORDSIZE OBJ-TAG) %rcx)
      (movzbq (%rcx %rax) %rax)
      (sal ,FIXNUM-SHIFT %rax)
     ))
    ((bytevector-u8-set! ,e1 ,e2 ,e3)
     `(
      ,@(gen-simple e1 '%rax)
      ,@(gen-simple e2 '%rcx)
      ,@(gen-simple e3 '%rdx)
      (sar ,FIXNUM-SHIFT %rcx)
      (add ,(- WORDSIZE OBJ-TAG) %rcx)
      (sar ,FIXNUM-SHIFT %rdx)
      (movb %dl (%rcx %rax))
     ))
    ((symbol? ,e1)
     `(
      ,@(gen-simple e1 '%rax)
      (and ,OBJ-MASK %rax)
      ,@(gen-eq '%rax '%rax SYM-TAG '%rcx)
      ))
    ((%make-symbol ,hash ,name)
     `(
      ,@(gen-collect 4 si)
      (mov (,AP-REG) %rax)
      (addq ,(* 4 WORDSIZE) (,AP-REG))
      ,@(gen-simple name '%rdx)
      ,@(gen-simple hash '%rcx)
      (mov %rdx (%rax ,(* 2 WORDSIZE)))
      (mov %rcx (%rax ,WORDSIZE))
      (movq ,UNBOUND-TAG (%rax))
      (or ,SYM-TAG %rax)
     ))
    ((%symbol-name ,e1)
     `(
      ,@(gen-simple e1 '%rax)
      (mov (%rax ,(- (* 2 WORDSIZE) SYM-TAG)) %rax)
      ))
    ((%symbol-hash ,e1)
     `(
      ,@(gen-simple e1 '%rax)
      (mov (%rax ,(- (* 1 WORDSIZE) SYM-TAG)) %rax)
      ))
    ((%symbol-value ,e1)
     (define end (gensym* "end"))
     `(
      ,@(gen-simple e1 '%rcx)
      (mov (%rcx ,(- (* 0 WORDSIZE) SYM-TAG)) %rax)
      (cmp ,UNBOUND-TAG %rax)
      (jne ,end)
      (mov %rcx ,(list-ref ARGS-REG 0))
      (call (PLT s_unbound_global))
      ,end
      ))
    ((%symbol-value-set! ,e1 ,e2)
     `(
      ,@(gen-simple e2 '%rcx)
      ,@(gen-simple e1 '%rax)
      (mov %rcx (%rax ,(- SYM-TAG)))
      ))
    ((procedure? ,e1)
     `(
      ,@(gen-simple e1 '%rax)
      (and ,PTR-MASK %rax)
      ,@(gen-eq '%rax '%rax CLOS-TAG '%rcx)
      ))
    ((make-closure (label ,fn) ,sz)
     (append 
      (gen-collect (+ 2 sz) si)
      (gen-closure fn (iota sz))))
    ((closure (label ,fn) . ,fvs)
     (append
      (gen-collect (+ 2 (length fvs)) si)
      (gen-closure fn fvs)))
    ((closure-ref ,clos ,i)
     (gen-simple `(call #f (prim closure-ref) ,clos ,i) '%rax))
    ((closure-set! ,e1 ,i ,e2)
     (append
      (gen-simple e1 '%rax)
      (gen-simple e2 `(%rax ,(- (* (+ i 2) WORDSIZE) CLOS-TAG)) '%rcx)))
    ((collect)
     (gen-collect #f si))
    ((collect ,e1)
     (append
      (gen-simple e1 '%rax)
      (gen-collect '%rax si)))
    ((local-ref ,i)
     (gen-simple `(call #f (prim local-ref) ,i) '%rax))
    (,() (error "gen-prim" "unmatch" e))))

(define (gen-if pred conseq altern si)
  (define csq (gensym* "csq"))
  (define alt (gensym* "alt"))
  (define end (gensym* "end"))
  `(
    ,@(gen-simple pred '%rax)
    (cmp ,FALSE-IMM %rax)
    (je ,alt)
    ,csq
    ,@(gen-expr conseq si)
    (jmp ,end)
    ,alt
    ,@(gen-expr altern si)
    ,end
  ))

(define (gen-collect sz si)
  `(
    (mov %rbp ,(list-ref ARGS-REG 2))
    (mov %rsp ,(list-ref ARGS-REG 1))
    ,@(if (eq? sz '%rax)
          `((mov %rax ,(list-ref ARGS-REG 0)))
          (gen-simple sz (list-ref ARGS-REG 0)))
    (call (PLT s_collect))
  ))

#;(
  ;;; stack layout
  (high ... low)
  pop  : sub
  push : add
  ;;; **XXX** - placeholder
  ;;; ((name ...) addr content)
  ;;; before passing arguments
  (... ret-addr^ ((SP FP) fp0 fp-1) local-0 ... local-n
       ((SP*) <top>))
  ;;; before jump
  (... ret-addr^ ((SP FP) fp0 fp-1) local-0 ... local-n
       **ret-addr** **old-fp** arg-0 ... arg-n)
  (... ret-addr* ((FP) fp0 fp-1) local-0 ... ((SP) local-n)
       **ret-addr** (**FP** fp1 fp0) arg-0 ... arg-n)
  argc -> %rcx
  ;;; in callee: before adjust the frame-pointer and stack-pointer
  (... ret-addr^ ((FP) fp0 fp-1) local-0 ... local-n
       ((SP) ret-addr) (_ fp1 fp0) arg-0 ... arg-n ((SP*) <top>))
  ;;; in callee: after adjust the frame-pointer and stack-pointer
  (... ret-addr^ fp-1 local-0 ... local-n
       ret-addr ((SP FP) fp1 fp0) arg-0 ... arg-n ((SP*) <top>))
)
(define (gen-procedure fn params variadic? body)
  (define argc (length params))
  (define csq (gensym* "conseq"))
  (match body
    ((let ((_ (call #f (prim alloca) ,n*))) ,e)
     ;;; 16 bytes == 2 * 8
     (define n (div (align-to-multiple 16 (* (+ n* 1) WORDSIZE)) WORDSIZE))
     `(
        ,fn
        (sub ,WORDSIZE %rsp)
        (mov %rbp (%rsp))
        (mov %rsp %rbp)
        ,@(if variadic?
              (gen-construct-vararg argc)
              `((cmp ,argc %rcx)
                (je ,csq)
                (mov %rcx %rax)
                (mov %rax ,(list-ref ARGS-REG 1))
                (mov ,argc ,(list-ref ARGS-REG 0))
                (call (PLT s_bad_monovariadic_call))
                ,csq))
        ,@(map
            (lambda (i)
              `(movq 0 (%rsp ,(* (+ i 2) (- WORDSIZE)))))
            (filter (lambda (i) (>= i argc)) (iota n)))
        (lea (%rsp ,(* n (- WORDSIZE))) %rsp)
        ,@(gen-expr e #f)
        (mov %rbp %rsp)
        (mov (%rbp) %rbp) ;;; restore frame pointer
        (add ,WORDSIZE %rsp)
        (mov 1 %rcx)
        (ret)
      ))
    (,() (error "gen-procedure" "unknown" body))))

(define (gen-L-apply)
  (define loop (gensym* "unstack"))
  (define loop-end (gensym* "unstack"))
  (define end (gensym* "call"))
  `(
    (:global L_apply)
    L_apply

    (sub ,WORDSIZE %rsp)
    (mov %rbp (%rsp))
    (mov %rsp %rbp)

    (mov %rcx %rax) ; argc
    (inc %rax)
    (sal ,FIXNUM-SHIFT %rax)
    (neg %rax)
    ; (+ %rsp (* (- WORDSIZE) argc))
    (mov (%rsp %rax) %rcx)
    (lea (%rsp %rax) %rdx)
    ,loop
    (mov %rcx %rax)
    (and ,OBJ-MASK %rax)
    (cmp ,PAIR-TAG %rax)
    (jne ,loop-end)

    (mov (%rcx ,(- PAIR-TAG)) %rax)           ; (car %rcx) -> %rax
    (mov %rax (%rdx))

    (mov (%rcx ,(- WORDSIZE PAIR-TAG)) %rcx)  ; (cdr %rcx) -> %rcx
    (sub ,WORDSIZE %rdx)
    (jmp ,loop)
    ,loop-end
    (cmp ,NIL-TAG %rcx)
    (je ,end)
    (jmp (PLT s_bad_apply_call))
    ,end
    (lea (%rsp ,(- (* 2 WORDSIZE))) %rcx)
    (sub %rdx %rcx)
    (sar ,FIXNUM-SHIFT %rcx)
    (mov (%rsp ,(- (* 2 WORDSIZE))) %rax)
    (mov (%rax ,(- CLOS-TAG)) %rax)
    (mov (%rbp) %rbp) ;;; restore frame pointer
    (add ,WORDSIZE %rsp)
    (jmp (reg %rax))
  ))

(define (gen-polyvariadic-proc fn clauses)
  (define (free-vars-of-clause clause)
    (match clause
      ((call ,tail? (prim closure*) ,variadic? ,argc (label ,fn). ,fvs)
       fvs)
      (,() (error "free-vars-of-clause" "unmatch" clause))))
  (define (gen-clause clause)
    (define nxt (gensym* "altern"))
    (match clause
      ((,i (label ,fn) ,variadic? ,argc)
       `(
          ,@(if variadic?
                `((cmp ,(- argc 1) %rcx)
                  (jl ,nxt))
                `((cmp ,argc %rcx)
                  (jne ,nxt)))
          (mov (%rsp ,(* (- WORDSIZE) 2)) %rax)
          (mov (%rax ,(- (* WORDSIZE (+ i 2)) CLOS-TAG)) %rax)
          (mov %rax (%rsp ,(* (- WORDSIZE) 2)))
          (mov (%rbp) %rbp) ;;; restore frame pointer
          (add ,WORDSIZE %rsp)
          (jmp ,fn)
          ,nxt
       ))
      (,() (error "gen-clause" "unmatch" clause))
      ))
  `(
    ,fn
    (sub ,WORDSIZE %rsp)
    (mov %rbp (%rsp))
    (mov %rsp %rbp)
    ,@(flatmap gen-clause clauses)
    (jmp (PLT s_bad_polyvariadic_call))
  ))

(define (gen-closure fn fvs)
  (define fvs-cnt (length fvs))
  `(
    (mov (,AP-REG) %rax)
    (addq ,(align-to-multiple 8 (* (+ fvs-cnt 2) WORDSIZE)) (,AP-REG))
    ,@(flatmap
        (lambda (i fv)
          (gen-simple fv (list '%rax (* (+ i 2) WORDSIZE)) '%rdx))
        (iota (length fvs))
        fvs)
    (movq ,(immediate-repr fvs-cnt) (%rax ,WORDSIZE))
    (lea (,fn %rip) %rdx)
    (mov %rdx (%rax))
    (or ,CLOS-TAG %rax)
  ))

(define (gen-tail-call apply? fn es si)
  (define argc (length (cons fn es)))
  (define argi (iota argc))
  ;;; TODO: not really right here
  (define offsets (map (lambda (i) (* (- WORDSIZE) (+ i 2))) argi))
  (define dests (map (lambda (i) (* (- WORDSIZE) (+ i 2))) argi))
  `(
    ,@(flatmap
        (lambda (off e) (gen-simple e (list '%rsp off) '%rax))
        offsets
        (cons fn es))
    ,@(flatmap
        (lambda (off dst)
          `((mov (%rsp ,off)  %rax)
            (mov %rax (%rbp ,dst))))
        offsets
        dests)
    (mov %rbp %rsp)
    (mov (%rbp) %rbp) ;;; restore frame pointer
    (mov ,argc %rcx)
    ,@(if apply?
          `((add ,WORDSIZE %rsp)
            (jmp (PLT L_apply)))
          `((mov (%rsp ,(* 2 (- WORDSIZE))) %rax)
            (mov (%rax ,(- CLOS-TAG)) %rax)
            (add ,WORDSIZE %rsp)
            (jmp (reg %rax))))
  ))

(define (gen-call apply? fn es si)
  (define argc (length (cons fn es)))
  `(
    ,@(flatmap
        (lambda (i e)
          (gen-simple e (list '%rsp (* (+ i 4) (- WORDSIZE))) '%rax))
        (iota argc)
        (cons fn es))
    (mov ,argc %rcx)
    ,@(if apply?
          `((call (PLT L_apply)))
          `(,@(gen-simple fn '%rax)
            (mov (%rax ,(- CLOS-TAG)) %rax)
            (call (reg %rax))))
  ))

(define (gen-construct-vararg argc)
  #;(
      [z 1]
      ()  0
      (1) 1

      (x y . z)
      inp           sz  argc  cons
      (0)           3   1     err
      (0 1)         3   2     (0 1 ())
      (0 1 2)       3   3     (0 1 (2))
      (0 1 2 3)     3   4     (0 1 (2 3))
      (0 1 2 3 4)   3   5     (0 1 (2 3 4))
  )
  `(
    (mov %rcx %rax)
    (inc %rax)
    (sal ,FIXNUM-SHIFT %rax)
    (sub %rax %rsp)
    (mov %rcx %rax)
    (mov %rbp ,(list-ref ARGS-REG 3))   ; fp
    (mov %rsp ,(list-ref ARGS-REG 2))   ; sp
    (mov %rax ,(list-ref ARGS-REG 1))   ; argc
    (mov ,argc ,(list-ref ARGS-REG 0))  ; expected argc
    (sub ,WORDSIZE %rsp)
    (and -16 %rsp)
    (call (PLT construct_vararg))
    (mov %rbp %rsp)  ; restore the frame pointer
    (mov %rax (%rsp ,(* (+ 1 argc) (- WORDSIZE))))
  ))

(define (gen-values es si)
  (define argc (length es))
  (define argi (iota argc))
  (define offsets
    (map (lambda (i) (* (+ i 3) (- WORDSIZE))) argi))
  (define dests
    (map (lambda (i) (* (+ i 3) (- WORDSIZE))) argi))
  `(
    ,@(flatmap
        (lambda (off e)
          (gen-simple e (list '%rsp off) '%rax))
        offsets es)
    ,@(flatmap
        (lambda (off dst)
          `((mov (%rsp ,off) %rax)
            (mov %rax (%rbp ,dst))))
        offsets dests)
    ,@(if (pair? es) `((mov (%rbp ,(car dests)) %rax)) '())
    (mov ,argc %rcx)
    (mov %rbp %rsp)
    (mov (%rbp) %rbp) ;;; restore frame pointer
    (add ,WORDSIZE %rsp)
    (ret)
  ))

(define (gen-call-with-values tail? e1 e2 si)
  `(
    ,@(gen-simple e1 (list '%rsp (* 4 (- WORDSIZE))) '%rax)
    (mov 1 %rcx)
    (mov (%rax ,(- CLOS-TAG)) %rax)
    (call (reg %rax))
    (mov %rax (%rsp ,(* 5 (- WORDSIZE))))
    ,@(gen-simple e2 (list '%rsp (* 4 (- WORDSIZE))) '%rax)
    (add 1 %rcx)
    ,@(if tail?
          `(
            (mov %rcx %rax)
            (add 3 %rax)
            (sal ,FIXNUM-SHIFT %rax)
            (neg %rax)
            (lea (%rsp %rax) %rsp)
            (mov %rcx %rax)
            (mov %rbp ,(list-ref ARGS-REG 2))   ; fp
            (mov %rsp ,(list-ref ARGS-REG 1))   ; sp
            (mov %rax ,(list-ref ARGS-REG 0))   ; argc
            (sub ,WORDSIZE %rsp)
            (and -16 %rsp)
            (call (PLT shift_values))
            (mov %rax %rcx) ;;; restore argc
            (mov (%rbp ,(* 2 (- WORDSIZE))) %rax) ;;; restore closure
            (lea (%rbp ,WORDSIZE) %rsp) ;;; restore sp
            (mov (%rbp) %rbp) ;;; restore frame pointer
            (mov (%rax ,(- CLOS-TAG)) %rax)
            (jmp (reg %rax))
          )
          `(
            (mov (%rax ,(- CLOS-TAG)) %rax)
            (call (reg %rax))
          ))))

(define (gen-call/cc tail? e si)
  (define cont (gensym* "cont"))
  `(
    (mov (GOTPCREL BOT_FP %rip) %rax)
    (mov (%rax) %rax)
    (sub %rsp %rax)
    (add ,(* 9 WORDSIZE) %rax)
    ,@(gen-collect '%rax si)
    ,@(if tail?
          `((mov %rbp ,(list-ref ARGS-REG 3))               ;;; fp
            (mov %rbp ,(list-ref ARGS-REG 2))               ;;; sp
            (mov (%rbp ,WORDSIZE) ,(list-ref ARGS-REG 1))   ;;; ret
            )
          `((mov %rbp ,(list-ref ARGS-REG 3))               ;;; fp
            (mov %rsp ,(list-ref ARGS-REG 2))               ;;; sp
            (lea (,cont %rip) ,(list-ref ARGS-REG 1))       ;;; ret
            ))
    ,@(gen-simple e (list-ref ARGS-REG 0))  ;;; clos
    (lea (%rsp ,(* 16 (- WORDSIZE))) %rsp)  ;;; extra space for shrinking
    (call (PLT s_reify_cont))
    (mov (GOTPCREL BOT_FP %rip) %rbp)
    (mov (%rbp) %rbp)
    (lea (%rbp ,(* 3 (- WORDSIZE))) %rsp)
    (mov (%rax ,(- CLOS-TAG)) %rax)
    (mov 2 %rcx)
    (jmp (reg %rax))
    ,cont
  ))

(define (gen-L-restore-stack)
  (define loop (gensym* "loop"))
  (define end (gensym* "end"))
  `(
    (:global L_explicit_restore_stack)
    (:global L_implicit_restore_stack)
    L_implicit_restore_stack
    (cmp 1 %rcx)
    (jne (PLT s_bad_imp_cont))
    (mov %rax (%rsp ,(- WORDSIZE)))
    (add ,(* 3 WORDSIZE) %rsp)
    (mov 2 %rcx)

    L_explicit_restore_stack
    (sub ,WORDSIZE %rsp)
    (mov %rbp (%rsp))
    (mov %rsp %rbp)

    ;;; copy-args
    (mov (GOTPCREL tospace_start %rip) %rdx)
    (mov (%rdx) %rdx)
    (sal ,FIXNUM-SHIFT %rcx)
    (mov %rcx (%rdx))
    (mov %rcx %rsi)
    (neg %rsi)
    (lea (%rbp ,(- WORDSIZE)) %rbp)

    ,loop
    (cmp 0 %rcx)
    (jle ,end)
    (mov (%rbp %rsi) %rax)
    (mov %rax (%rdx %rcx))
    (sub ,WORDSIZE %rcx)
    (add ,WORDSIZE %rsi)
    (jmp ,loop)
    ,end

    ;;; cont, calculating the stack_size
    (mov (%rsp ,(* 2 (- WORDSIZE))) %rax)
    (mov (%rax ,(- (* 1 WORDSIZE) CLOS-TAG)) %rax)
    (sub ,(* 5 WORDSIZE) %rax)

    (mov (GOTPCREL BOT_FP %rip) %rsp)
    (mov (%rsp) %rsp)
    (neg %rax)
    (lea (%rsp %rax) %rsp)
    (lea (%rsp ,(* 2 (- WORDSIZE))) %rsp)

    (mov (%rdx) %rcx) ;;; argc
    (neg %rcx)
    (lea (%rsp %rcx) %rsp)
    (mov %rsp ,(list-ref ARGS-REG 0))
    (lea (%rsp ,(* 16 (- WORDSIZE))) %rsp)
    (call (PLT s_apply_cont))
    (mov (,AP-REG) %rdx)
    (mov (%rdx ,(* 0 WORDSIZE)) %rcx) ; argc
    (mov (%rdx ,(* 1 WORDSIZE)) %rsp) ; stack pointer
    (mov (%rdx ,(* 2 WORDSIZE)) %rbp) ; frame pointer
    (mov (%rdx ,(* 4 WORDSIZE)) %rdx) ; ret
    (jmp (reg %rdx))
  ))

(define (gen-expr e si)
  (match e
    (,()
     (guard (or (integer? e) (char? e) (boolean? e)))
     (gen-simple e '%rax))
    (,()
     (guard (string? e))
     (gen-str (string->list e) si))
    ((quote ,q)
     `((mov ,(immediate-repr q) %rax)))
    ((label ,lbl)
     (gen-simple e '%rax))
    ((label* ,lbl)
     (gen-simple e '%rax))
    ((call ,tail? (prim push-constant-table))
     `((lea (.constant_table %rip) %rax)
       (mov (GOTPCREL LBL_TBL %rip) %rdx)
       (mov (%rdx) %rcx)
       (mov %rcx (%rax))
       (mov %rax (%rdx))))
    ((call ,tail? (prim %set-label!) (label* ,x) ,e)
     (gen-simple e `(,x %rip) '%rax))
    ((call ,tail? (prim values) . ,es)
     (if (not tail?)
         (error "gen-expr" "only-tail-values" e))
     (gen-values es si))
    ((call ,tail? (prim call-with-values) ,e1 ,e2)
     (gen-call-with-values tail? e1 e2 si))
    ((call ,tail? (prim apply) ,fn . ,es)
     ((if tail? gen-tail-call gen-call) #t fn es si))
    ((call ,tail? (prim call/cc) ,e1)
     (gen-call/cc tail? e1 si))
    ((call ,tail? (prim ,pr) . ,es)
     (gen-prim (cons pr es) si))
    ((call ,tail? ,fn . ,es)
     ((if tail? gen-tail-call gen-call) #f fn es si))
    ((foreign-call ,() (label ,fn) . ,es)
     (gen-foreign-call fn es si))
    ((let ((_ ,e1)) ,e2)
     (append (gen-expr e1 si) (gen-expr e2 si)))
    ((if ,pred ,conseq ,altern)
     (gen-if pred conseq altern si))
    (,() (error "gen-expr" "unmatch" e))))

(define (gen-code code)
  (match code
    ((,fn (lambda ,params #t ,() ()
            (let ,bs (call #t (prim values*) ,e))))
      `(,fn
        (sub 1 %rcx)
        (mov (%rsp ,(* 4 (- WORDSIZE))) %rax)
        (ret)))
    ((,fn (lambda ,params ,variadic? ,() ,fvs ,body))
     (gen-procedure fn params variadic? body))
    ((,fn (case-lambda . ,clauses))
     (gen-polyvariadic-proc fn clauses))
    (,() (error "gen-code" "unmatch" code))))

(define (gen-data labels)
  (if (pair? labels)
      `(
        (:section data)
        (:p2align 3)
        (:local .constant_table)
        ,@(map (lambda (l) `(:local ,l)) labels)
        .constant_table
        (:byte ,WORDSIZE 0)
        (:byte ,WORDSIZE ,(length labels))
        ,@(flatmap (lambda (l) `(,l (:byte ,WORDSIZE 0))) labels)
      )
      '()))

(define (gen-unit e)
  (match e
    ((program ,main ,labels ,codes)
      `(
        (:extern stdout)
        (:extern free_ptr)
        (:extern L_apply)
        ,@(gen-data labels)
        (:global ,main)
        (:section text)
        ,@(flatmap gen-code codes)
        ))
    (,() (error "gen-unit" "unmatch" e))))

(define (gen-main entries)
  `(
    (:global main)
    ,@(gen-L-apply)
    ,@(gen-L-restore-stack)
    main
    (push %rbp)
    (mov %rsp %rbp)
    (push %rbx)
    (pushq 0) ;;; for alignment
    (mov (GOTPCREL BOT_FP %rip) %rax)
    (mov %rbp (%rax))
    (call (PLT _scm_init))
    (mov (GOTPCREL free_ptr %rip) ,AP-REG)
    ,@(flatmap
        (lambda (e)
          `((movq ,ENTRY-MARK (%rsp ,(* 4 (- WORDSIZE))))
            (mov 1 %rcx)
            (call (PLT ,e))))
        entries)
    (pop %rax)
    (pop %rbx)
    (pop %rbp)
    (ret)
  ))

(vector gen-unit gen-main)
)) ;;; code-gen

(define gen-unit (vector-ref code-gen 0))
(define gen-main (vector-ref code-gen 1))

(define CC (maybe-getenv "CC" "gcc"))
(define CFLAGS (maybe-getenv "CFLAGS" "-g -Wall -fno-omit-frame-pointer"))
(define OBJCOPY (maybe-getenv "OBJCOPY" "objcopy"))

(define emit-ir-seq (let ()
  (define (emit op . args)
    (apply format (cons op args))
    (newline op))
  (define REGISTERS
    '(%rax %rbx %rcx %rdx %rsi %rdi
      %rbp %rsp
      %r8 %r9 %r10 %r11 %r12 %r13 %r14 %r15
      %eax %ebx %ecx %edx %esi %edi %esp %ebp
      %cl %dl))
  #|
  (imm <int|sym>)
  (reg <reg>)
  (label <sym>)
  (mem <displacement> <reg-base> <reg-index> <scale>) ; canonical form
  (mem <reg-base>)
  (mem <reg-base> <displacement>)
  (mem <reg-base> <reg-index>)
  (mem <reg-base> <reg-index> <scale>)
  (mem <reg-base> <reg-index> <scale>)
  |#
  (define (reg? r) (memq r REGISTERS))

  (define (arg->str arg)
    (match arg
      (,()
       (guard (integer? arg))
       (format "$~a" arg))
      (,()
       (guard (reg? arg))
       (format "~a" arg))
      (($ ,txt)
       txt)
      ((GOTPCREL ,lbl %rip)
       (format "~a@GOTPCREL(%rip)" lbl))
      ((,lbl %rip)
       (guard (symbol? lbl))
       (format "~a(%rip)" lbl))
      ((,arg)
       (guard (reg? arg))
       (format "(~a)" arg))
      ((,arg ,off)
       (guard (and (integer? off)
                   (reg? arg)))
       (format "~a(~a)" off arg))
      ((,reg-base ,reg-index)
       (guard (and (reg? reg-base) (reg? reg-index)))
       (format "(~a, ~a)" reg-base reg-index))
      (,() (error "arg->str" "unmatch" arg))))

  (define (emit-ir op instr)
    (match instr
      ((:global ,lbl)
       (emit op ".globl ~a" lbl))
      ((:local ,lbl)
       (emit op ".local ~a" lbl))
      ((:extern ,lbl)
       (emit op ".extern ~a" lbl))
      ((:section ,s)
       (emit op ".section .~a" s))
      ((:p2align ,s)
       (emit op ".p2align ~a" s))
      ((:byte ,w ,v)
       (emit op ".~abyte ~a" w v))
      (,lbl
       (guard (symbol? lbl))
       (emit op "~a: " lbl))
      ((,unary-op ,e1)
       (guard (memq unary-op '(push pushq pop neg inc idiv)))
       (emit op "~a ~a" unary-op (arg->str e1)))
      ((,binary-op ,e1 ,e2)
       (guard (memq binary-op '(mov movb movl movq movzbl movzbq lea
                                cmovne cmovl cmovle cmove cmove cmovge cmovg
                                add addl addq sub imul
                                and sal sar or
                                cmp)))
       (emit op "~a ~a, ~a" binary-op (arg->str e1) (arg->str e2)))
      ((,ctrl ,to)
       (guard (memq ctrl '(call jne jmp jl jle je je jge jg)))
       (match to
        ((PLT ,lbl)
         (emit op "~a ~a@PLT" ctrl lbl))
        ((reg ,r)
         (emit op "~a *~a" ctrl r))
        (,()
         (guard (symbol? to))
         (emit op "~a ~a" ctrl to))
        (,() (error "emit-ir" "control: unknown operand" to))))
      ((ret)
       (emit op "ret"))
      ((cqo)
       (emit op "cqo"))
      (,() (error "emit-ir" "unmatch" instr))))

  (define (emit-ir-seq op instrs)
    (if (pair? instrs)
        (begin
          (emit-ir op (car instrs))
          (emit-ir-seq op (cdr instrs)))))

  emit-ir-seq)) ;;; emit-ir-seq


(include "compiler-tools.scm")
