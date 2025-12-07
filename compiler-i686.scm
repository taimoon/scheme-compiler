(import
  (only (match) match)
  (only (utils)
    align-to-multiple gensym*
    flatmap read-sexps-from-path path-filestem path-extension
    make-tempname system* maybe-getenv)
  (only (front) preprocess))

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
(define WORDSIZE 4)
(define FIXNUM-SHIFT  2)
(define FIXNUM-MASK   #b11)
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

(define AP-REG '%edi)
(define FP-REG '%ebp)

(define (gen-simple* e dst tmp)
  (define mem? pair?)
  (define mov2dst
    (if (mem? dst)
        `((mov ,tmp ,dst))
        '()))
  (match e
    (,()
     (guard (or (integer? e) (char? e) (boolean? e)))
     `((movl ,(immediate-repr e) ,dst)))
    ((quote ,q)
     `((movl ,(immediate-repr q) ,dst)))
    ((,form ,lbl)
     (guard (memq form '(label label*)))
     `((mov (GOT ,lbl %ebx) ,tmp)
       (mov (,tmp) ,tmp)
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
      ,@(gen-simple e '%eax)
      (mov %eax %ecx)
      (and ,OBJ-MASK %ecx)
      (cmp ,OBJ-TAG %ecx)
      (jne ,alt)
      ,csq
      (mov (%eax ,(- OBJ-TAG)) %eax)
      (and ,mask %eax)
      (cmp ,tag %eax)
      (mov ,TRUE-IMM %ecx)
      (mov ,FALSE-IMM %eax)
      (cmove %ecx %eax)
      (jmp ,end)
      ,alt
      (mov ,FALSE-IMM %eax)
      ,end
    ))

(define (gen-str vs si)
  (define sz (align-to-multiple 8 (+ WORDSIZE (* (length vs) 4))))
  `(
    ,@(gen-collect (ash sz (- FIXNUM-SHIFT)) si)
    (mov (,AP-REG) %eax)
    (addl ,sz (,AP-REG))
    (movl ,(bitwise-ior STR-TAG (immediate-repr (length vs))) (%eax))
    ,@(flatmap
        (lambda (i v)
          (gen-simple v `(%eax ,(+ (* i 4) WORDSIZE)) '%ecx))
        (iota (length vs))
        vs)
    (or ,OBJ-MASK %eax)
  ))

(define (gen-vec vs si)
  (define vlen (length vs))
  (define sz (align-to-multiple 8 (* (+ vlen 1) WORDSIZE)))
  `(
    ,@(gen-collect sz si)
    (mov (,AP-REG) %eax)
    (addl ,sz (,AP-REG))
    (movl ,(bitwise-ior VEC-TAG (immediate-repr vlen)) (%eax))
    ,@(flatmap
        (lambda (i v)
          (gen-simple v `(%eax ,(* (+ i 1) WORDSIZE)) '%ecx))
        (iota vlen)
        vs)
    (or ,OBJ-MASK %eax)
  ))

(define (gen-foreign-call fn args si)
  (define argc (length args))
  (define calc-offset (lambda (i) (* (- WORDSIZE) (add1 i))))
  (define end-si (calc-offset (- argc 1)))
  `(
    ,@(flatmap
        (lambda (off arg)
          (gen-simple arg (list '%esp off) '%eax))
        (map calc-offset (iota argc))
        (reverse args))
    (sub ,(abs end-si) %esp)
    (call (PLT ,fn))
    (add ,(abs end-si) %esp)
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
    ,@(gen-simple e2 '%ecx)
    ,@(gen-simple e1 '%eax)
    (cmp %ecx %eax)
    (mov ,TRUE-IMM %ecx)
    (mov ,FALSE-IMM %eax)
    (,cmov %ecx %eax)
  ))

(define (gen-prim e si)
  (match e
    ((local-set! ,i ,e1)
     `(
      ,@(gen-expr e1 si)
      (mov %eax (,FP-REG ,(* (- WORDSIZE) (+ i 2))))
      ))
    ((eq? ,e1 ,e2)
     (gen-cmp 'eq? e1 e2 si))
    ((boolean? ,e1)
     `(
      ,@(gen-simple e1 '%eax)
      (and ,IMM-MASK %eax)
      ,@(gen-eq '%eax '%eax BOOL-TAG '%ecx)
      ))
    ((not ,e1)
     `(
        ,@(gen-simple e1 '%eax)
        (cmp ,FALSE-IMM %eax)
        (mov ,FALSE-IMM %eax)
        (mov ,TRUE-IMM %ecx)
        (cmove %ecx %eax)
      ))
    ((eof-object? ,e1)
     `(
      ,@(gen-simple e1 '%eax)
      (and ,IMM-MASK %eax)
      ,@(gen-eq '%eax '%eax EOF-TAG '%ecx)
      ))
    ((null? ,e1)
     `(
      ,@(gen-simple e1 '%eax)
      (and ,IMM-MASK %eax)
      ,@(gen-eq '%eax '%eax NIL-TAG '%ecx)
      ))
    ((char? ,e1)
     `(
      ,@(gen-simple e1 '%eax)
      (and ,IMM-MASK %eax)
      ,@(gen-eq '%eax '%eax CHAR-TAG '%ecx)
      ))
    ((char->integer ,e1)
     `(
      ,@(gen-simple e1 '%eax)
      (sar ,(- IMM-SHIFT FIXNUM-SHIFT) %eax)
      ))
    ((integer->char ,e1)
     `(
      ,@(gen-simple e1 '%eax)
      (sal ,(- IMM-SHIFT FIXNUM-SHIFT) %eax)
      (or ,CHAR-TAG %eax)
      ))
    ((integer? ,e1)
     `(
      ,@(gen-simple e1 '%eax)
      (and ,FIXNUM-MASK %eax)
      ,@(gen-eq '%eax '%eax FIXNUM-TAG '%ecx)
      ))
    ((,cmp ,e1 ,e2)
     (guard (assq cmp cmp-suffix))
     (gen-cmp cmp e1 e2 si))
    ((- ,e1 ,e2)
     `(
      ,@(gen-simple e2 '%ecx)
      ,@(gen-simple e1 '%eax)
      (sub %ecx %eax)
      ))
    ((+ ,e1 ,e2)
     `(
      ,@(gen-simple e2 '%ecx)
      ,@(gen-simple e1 '%eax)
      (add %ecx %eax)
      ))
    ((* ,e1 ,e2)
     `(
      ,@(gen-simple e2 '%ecx)
      ,@(gen-simple e1 '%eax)
      (sar ,FIXNUM-SHIFT %eax)
      (imul %ecx %eax)
      ))
    ((div ,dividend ,divisor)
     #;((div (* t x) (* t y))
        => (div x y)
        => (tag (div x y)))
     `(
        ,@(gen-simple divisor '%ecx)
        ,@(gen-simple dividend '%eax)
        (cdq)
        (idiv %ecx)
        (sal ,FIXNUM-SHIFT %eax)
      ))
    ((mod ,dividend ,divisor)
     ;;; TODO: don't know why this works?
     `(
        ,@(gen-simple divisor '%ecx)
        ,@(gen-simple dividend '%eax)
        (cdq)
        (idiv %ecx)
        (mov %edx %eax)
     ))
    ((abs ,e1)
     `(
      ,@(gen-simple e1 '%eax)
      (mov %eax %ecx)
      (neg %ecx)
      (cmp 0 %eax)
      (cmovl %ecx %eax)
      ))
    ((max ,e1 ,e2)
     `(
      ,@(gen-simple e2 '%ecx)
      ,@(gen-simple e1 '%eax)
      (cmp %ecx %eax)
      (cmovl %ecx %eax)
      ))
    ((ash ,e1 ,e2)
     (define altern (gensym* "altern"))
     (define end (gensym* "end"))
     `(
      ,@(gen-simple e2 '%ecx)
      ,@(gen-simple e1 '%eax)
      (sar ,FIXNUM-SHIFT %ecx)
      (cmp 0 %ecx)
      (jl ,altern)
      ; e2 >= 0
      (sal %cl %eax)
      (jmp ,end)
      ,altern
      ; e2 < 0
      (neg %ecx)
      (sar ,FIXNUM-SHIFT %eax)
      (sar %cl %eax)
      (sal ,FIXNUM-SHIFT %eax)
      ,end
     ))
    ((bitwise-and ,e1 ,e2)
     `(
      ,@(gen-simple e2 '%ecx)
      ,@(gen-simple e1 '%eax)
      (and %ecx %eax)
     ))
    ((bitwise-ior ,e1 ,e2)
     `(
      ,@(gen-simple e2 '%ecx)
      ,@(gen-simple e1 '%eax)
      (or %ecx %eax)
     ))
    ((pair? ,e1)
     `(
      ,@(gen-simple e1 '%eax)
      (and ,OBJ-MASK %eax)
      ,@(gen-eq '%eax '%eax PAIR-TAG '%ecx)
      ))
    ((cons ,e1 ,e2)
     `(
      ,@(gen-collect 2 si)
      ,@(gen-simple e2 '%edx)
      ,@(gen-simple e1 '%ecx)
      (mov (,AP-REG) %eax)
      (addl ,(* 2 WORDSIZE) (,AP-REG))
      (mov %ecx (%eax))
      (mov %edx (%eax ,WORDSIZE))
      (lea (%eax ,PAIR-TAG) %eax)
      ))
    ((car ,e)
     `(
      ,@(gen-simple e '%eax)
      (mov (%eax ,(- PAIR-TAG)) %eax)
      ))
    ((cdr ,e)
     `(
      ,@(gen-simple e '%eax)
      (mov (%eax ,(- WORDSIZE PAIR-TAG)) %eax)
      ))
    ((set-car! ,e1 ,e2)
     `(
      ,@(gen-simple e2 '%ecx)
      ,@(gen-simple e1 '%eax)
      (mov %ecx (%eax ,(- PAIR-TAG))
      )))
    ((set-cdr! ,e1 ,e2)
     `(
      ,@(gen-simple e2 '%ecx)
      ,@(gen-simple e1 '%eax)
      (mov %ecx (%eax ,(- WORDSIZE PAIR-TAG)))
      ))
    ((string? ,e1)
     (gen-obj-test e1 STR-MASK STR-TAG))
    ((string . ,es)
     (gen-str es si))
    ((string-length ,e1)
     `(
      ,@(gen-simple e1 '%eax)
      (mov (%eax ,(- OBJ-TAG)) %eax)
      (sub ,STR-TAG %eax)
     ))
    ((string-ref ,e1 ,e2)
     `(
      ,@(gen-simple e1 '%eax)
      ,@(gen-simple e2 '%ecx)
      (add ,(- WORDSIZE OBJ-TAG) %ecx)
      (mov (%ecx %eax) %eax)
     ))
    ((string-set! ,e1 ,e2 ,e3)
     `(
      ,@(gen-simple e1 '%eax)
      ,@(gen-simple e2 '%ecx)
      ,@(gen-simple e3 '%edx)
      (add ,(- WORDSIZE OBJ-TAG) %ecx)
      (mov %edx (%ecx %eax))
      (mov ,VOID-TAG %eax)
     ))
    ((make-string ,e1 ,e2)
     (define lp (gensym* "loop"))
     (define end (gensym* "end"))
     `(
      ,@(gen-simple e1 '%eax)
      (add ,WORDSIZE %eax)
      ,@(gen-align 8 '%eax)
      ,@(gen-collect '%eax si)
      ,@(gen-simple e1 '%ecx)
      (mov (,AP-REG) %eax)
      #;(addl ,(align-to-multiple 8 (+ WORDSIZE (* (string-length s) 4))) (,AP-REG))
      (add ,WORDSIZE %ecx)
      ,@(gen-align 8 '%ecx)
      (add %ecx (,AP-REG))
      ,@(gen-simple e1 '%ecx)
      (or ,STR-TAG %ecx)
      (mov %ecx (%eax))
      ,@(gen-simple e1 '%ecx)
      ,@(gen-simple e2 '%edx)
      ,lp
      ; %ecx < WORDSIZE
      (cmp ,WORDSIZE %ecx)
      (jl ,end)
      (mov %edx (%eax %ecx))
      (sub 4 %ecx)
      (jmp ,lp)
      ,end
      (or ,OBJ-MASK %eax)
     ))
    ((vector . ,es)
     (gen-vec es si))
    ((vector? ,e1)
     (gen-obj-test e1 VEC-MASK VEC-TAG))
    ((vector-length ,e1)
     `(
      ,@(gen-simple e1 '%eax)
      (mov (%eax ,(- OBJ-TAG)) %eax)
      (sub ,VEC-TAG %eax)
     ))
    ((vector-ref ,e1 ,e2)
     `(
      ,@(gen-simple e1 '%eax)
      ,@(gen-simple e2 '%ecx)
      (add ,(- WORDSIZE OBJ-TAG) %ecx)
      (mov (%ecx %eax) %eax)
     ))
    ((vector-set! ,e1 ,e2 ,e3)
     `(
      ,@(gen-simple e1 '%eax)
      ,@(gen-simple e2 '%ecx)
      ,@(gen-simple e3 '%edx)
      (add ,(- WORDSIZE OBJ-TAG) %ecx)
      (mov %edx (%ecx %eax))
     ))
    ((make-vector ,e1 ,e2)
     (define lp (gensym* "loop"))
     (define end (gensym* "end"))
     `(
      ,@(gen-simple e1 '%eax)
      (add ,WORDSIZE %eax)
      ,@(gen-align 8 '%eax)
      ,@(gen-collect '%eax si)
      (mov (,AP-REG) %eax)
      #;(addl ,(align-to-multiple 8 (+ WORDSIZE (* (vector-length v) WORDSIZE))) (,AP-REG))
      ,@(gen-simple e1 '%ecx)
      (add ,WORDSIZE %ecx)
      ,@(gen-align 8 '%ecx)
      (add %ecx (,AP-REG))
      ,@(gen-simple e1 '%ecx)
      (or ,VEC-TAG %ecx)
      (mov %ecx (%eax))
      ,@(gen-simple e1 '%ecx)
      ,@(gen-simple e2 '%edx)
      ,lp
      ; %ecx < WORDSIZE
      (cmp ,WORDSIZE %ecx)
      (jl ,end)
      (mov %edx (%eax %ecx))
      (sub ,WORDSIZE %ecx)
      (jmp ,lp)
      ,end
      (or ,OBJ-MASK %eax)
     ))
    ((bytevector? ,e1)
     (gen-obj-test e1 BYTEVEC-MASK BYTEVEC-TAG))
    ((make-bytevector ,e1 . ,maybe-e2)
     (define e2 (if (pair? maybe-e2) (car maybe-e2) #f))
     (define lp (gensym* "loop"))
     (define end (gensym* "end"))
     `(
      ,@(gen-simple e1 '%eax)
      (sar ,FIXNUM-SHIFT %eax)
      (add ,WORDSIZE %eax)
      ,@(gen-align 8 '%eax)
      ,@(gen-collect '%eax si)
      (mov (,AP-REG) %eax)
      #;(addl ,(align-to-multiple 8 (* (bytevector-length v) WORDSIZE)) (,AP-REG))
      ,@(gen-simple e1 '%ecx)
      (mov %ecx %edx)
      (sar ,FIXNUM-SHIFT %edx)
      (add ,WORDSIZE %edx)
      ,@(gen-align 8 '%edx)
      (add %edx (,AP-REG))
      (or ,BYTEVEC-TAG %ecx)
      (mov %ecx (%eax))
      ,@(if e2
            `(
              ,@(gen-simple e1 '%ecx)
              (sar ,FIXNUM-SHIFT %ecx)
              (add ,(- WORDSIZE 1) %ecx)
              ,@(gen-simple e2 '%edx)
              (sar ,FIXNUM-SHIFT %edx)
              ,lp
              ; %ecx < WORDSIZE
              (cmp ,WORDSIZE %ecx)
              (jl ,end)
              (movb %dl (%eax %ecx))
              (sub 1 %ecx)
              (jmp ,lp)
              ,end)
            '())
      (or ,OBJ-MASK %eax)
      ))
    ((bytevector-length ,e1)
     `(
      ,@(gen-simple e1 '%eax)
      (mov (%eax ,(- OBJ-TAG)) %eax)
      (sub ,BYTEVEC-TAG %eax)
     ))
    ((bytevector-u8-ref ,e1 ,e2)
     `(
      ,@(gen-simple e1 '%eax)
      ,@(gen-simple e2 '%ecx)
      (sar ,FIXNUM-SHIFT %ecx)
      (add ,(- WORDSIZE OBJ-TAG) %ecx)
      (movzbl (%ecx %eax) %eax)
      (sal ,FIXNUM-SHIFT %eax)
     ))
    ((bytevector-u8-set! ,e1 ,e2 ,e3)
     `(
      ,@(gen-simple e1 '%eax)
      ,@(gen-simple e2 '%ecx)
      ,@(gen-simple e3 '%edx)
      (sar ,FIXNUM-SHIFT %ecx)
      (add ,(- WORDSIZE OBJ-TAG) %ecx)
      (sar ,FIXNUM-SHIFT %edx)
      (movb %dl (%ecx %eax))
     ))
    ((symbol? ,e1)
     `(
      ,@(gen-simple e1 '%eax)
      (and ,OBJ-MASK %eax)
      ,@(gen-eq '%eax '%eax SYM-TAG '%ecx)
      ))
    ((%make-symbol ,hash ,name)
     `(
      ,@(gen-collect 4 si)
      (mov (,AP-REG) %eax)
      (addl ,(* 4 WORDSIZE) (,AP-REG))
      ,@(gen-simple name '%edx)
      ,@(gen-simple hash '%ecx)
      (mov %edx (%eax ,(* 2 WORDSIZE)))
      (mov %ecx (%eax ,WORDSIZE))
      (movl ,UNBOUND-TAG (%eax))
      (or ,SYM-TAG %eax)
     ))
    ((%symbol-name ,e1)
     `(
      ,@(gen-simple e1 '%eax)
      (mov (%eax ,(- (* 2 WORDSIZE) SYM-TAG)) %eax)
      ))
    ((%symbol-hash ,e1)
     `(
      ,@(gen-simple e1 '%eax)
      (mov (%eax ,(- (* 1 WORDSIZE) SYM-TAG)) %eax)
      ))
    ((%symbol-value ,e1)
     (define end (gensym* "end"))
     `(
      ,@(gen-simple e1 '%ecx)
      (mov (%ecx ,(- (* 0 WORDSIZE) SYM-TAG)) %eax)
      (cmp ,UNBOUND-TAG %eax)
      (jne ,end)
      (mov %ecx (%esp ,(- WORDSIZE)))
      (sub ,WORDSIZE %esp)
      (call (PLT s_unbound_global))
      (add ,WORDSIZE %esp)
      ,end
      ))
    ((%symbol-value-set! ,e1 ,e2)
     `(
      ,@(gen-simple e2 '%ecx)
      ,@(gen-simple e1 '%eax)
      (mov %ecx (%eax ,(- SYM-TAG)))
      ))
    ((procedure? ,e1)
     `(
      ,@(gen-simple e1 '%eax)
      (and ,PTR-MASK %eax)
      ,@(gen-eq '%eax '%eax CLOS-TAG '%ecx)
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
     (gen-simple `(call #f (prim closure-ref) ,clos ,i) '%eax))
    ((closure-set! ,e1 ,i ,e2)
     (append
      (gen-simple e1 '%eax)
      (gen-simple e2 `(%eax ,(- (* (+ i 2) WORDSIZE) CLOS-TAG)) '%ecx)))
    ((collect)
     (gen-collect #f si))
    ((collect ,e1)
     (append
      (gen-simple e1 '%eax)
      (gen-collect '%eax si)))
    ((local-ref ,i)
     (gen-simple `(call #f (prim local-ref) ,i) '%eax))
    (,() (error "gen-prim" "unmatch" e))))

(define (gen-if pred conseq altern si)
  (define csq (gensym* "csq"))
  (define alt (gensym* "alt"))
  (define end (gensym* "end"))
  `(
    ,@(gen-simple pred '%eax)
    (cmp ,FALSE-IMM %eax)
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
    (mov %ebp (%esp ,(* 1 (- WORDSIZE))))
    (mov %esp (%esp ,(* 2 (- WORDSIZE))))
    ,@(if (eq? sz '%eax)
          `((mov %eax (%esp ,(* 3 (- WORDSIZE)))))
          (gen-simple sz `(%esp ,(* 3 (- WORDSIZE))) '%eax))
    (sub ,(* 3 WORDSIZE) %esp)
    (call (PLT s_collect))
    (add ,(* 3 WORDSIZE) %esp)
  ))

#;(
  (%eax %ebx %ecx %edx %esi %edi %esp %ebp)
  ; caller save
  (%eax %ecx %edx)
  ; callee save
  (%ebx %esi %edi %esp %ebp)
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
  argc -> %ecx
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
        (sub ,WORDSIZE %esp)
        (mov %ebp (%esp))
        (mov %esp %ebp)
        (mov %ebx %eax)
        ,@(gen-load-GOT-ebx)
        (mov %eax (%ebp ,(- WORDSIZE)))
        ,@(if variadic?
              (gen-construct-vararg argc)
              `((cmp ,argc %ecx)
                (je ,csq)
                (mov %ecx (%esp ,(* 1 (- WORDSIZE))))
                (movl ,argc (%esp ,(* 2 (- WORDSIZE))))
                (sub ,(* 2 WORDSIZE) %esp)
                (call (PLT s_bad_monovariadic_call))
                ,csq))
        ,@(map
            (lambda (i)
              `(movl 0 (%esp ,(* (+ i 2) (- WORDSIZE)))))
            (filter (lambda (i) (>= i argc)) (iota n)))
        (lea (%esp ,(* n (- WORDSIZE))) %esp)
        ,@(gen-expr e #f)
        (mov 1 %ecx)
        (mov %ebp %esp)
        (mov (%ebp ,(- WORDSIZE)) %ebx) ;;; restore got pointer
        (mov (%ebp) %ebp) ;;; restore frame pointer
        (add ,WORDSIZE %esp)
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

    (sub ,WORDSIZE %esp)
    (mov %ebp (%esp))
    (mov %esp %ebp)
    (mov %ebx %eax)
    ,@(gen-load-GOT-ebx)
    (mov %eax (%ebp ,(- WORDSIZE)))

    (mov %ecx %eax) ; argc
    (inc %eax)
    (sal ,FIXNUM-SHIFT %eax)
    (neg %eax)
    ; (+ %esp (* (- WORDSIZE) argc))
    (mov (%esp %eax) %ecx)
    (lea (%esp %eax) %edx)

    ,loop
    (mov %ecx %eax)
    (and ,OBJ-MASK %eax)
    (cmp ,PAIR-TAG %eax)
    (jne ,loop-end)

    (mov (%ecx ,(- PAIR-TAG)) %eax)           ; (car %ecx) -> %eax
    (mov %eax (%edx))

    (mov (%ecx ,(- WORDSIZE PAIR-TAG)) %ecx)  ; (cdr %ecx) -> %ecx
    (sub ,WORDSIZE %edx)
    (jmp ,loop)
    ,loop-end
    (cmp ,NIL-TAG %ecx)
    (je ,end)
    (jmp (PLT s_bad_apply_call))
    ,end
    (lea (%esp ,(- (* 2 WORDSIZE))) %ecx)
    (sub %edx %ecx)
    (sar ,FIXNUM-SHIFT %ecx)
    (mov (%esp ,(- (* 2 WORDSIZE))) %eax)
    (mov (%eax ,(- CLOS-TAG)) %eax)
    (mov (%ebp ,(- WORDSIZE)) %ebx) ;;; restore got pointer
    (mov (%ebp) %ebp) ;;; restore frame pointer
    (add ,WORDSIZE %esp)
    (jmp (reg %eax))
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
                `((cmp ,(- argc 1) %ecx)
                  (jl ,nxt))
                `((cmp ,argc %ecx)
                  (jne ,nxt)))
          (mov (%esp ,(* (- WORDSIZE) 2)) %eax)
          (mov (%eax ,(- (* WORDSIZE (+ i 2)) CLOS-TAG)) %eax)
          (mov %eax (%esp ,(* (- WORDSIZE) 2)))
          (mov (%ebp ,(- WORDSIZE)) %ebx) ;;; restore got pointer
          (mov (%ebp) %ebp) ;;; restore frame pointer
          (add ,WORDSIZE %esp)
          (jmp ,fn)
          ,nxt
       ))
      (,() (error "gen-clause" "unmatch" clause))
      ))
  `(
    ,fn
    (sub ,WORDSIZE %esp)
    (mov %ebp (%esp))
    (mov %esp %ebp)
    (mov %ebx %eax)
    ,@(gen-load-GOT-ebx)
    (mov %eax (%ebp ,(- WORDSIZE)))
    ,@(flatmap gen-clause clauses)
    (jmp (PLT s_bad_polyvariadic_call))
  ))

(define (gen-closure fn fvs)
  (define fvs-cnt (length fvs))
  `(
    (mov (,AP-REG) %eax)
    (addl ,(align-to-multiple 8 (* (+ fvs-cnt 2) WORDSIZE)) (,AP-REG))
    ,@(flatmap
        (lambda (i fv)
          (gen-simple fv (list '%eax (* (+ i 2) WORDSIZE)) '%edx))
        (iota (length fvs))
        fvs)
    (movl ,(immediate-repr fvs-cnt) (%eax ,WORDSIZE))
    (mov (GOT ,fn %ebx) %edx)
    (mov %edx (%eax))
    (or ,CLOS-TAG %eax)
  ))

(define (gen-tail-call apply? fn es si)
  (define argc (length (cons fn es)))
  (define argi (iota argc))
  ;;; TODO: not really right here
  (define offsets (map (lambda (i) (* (- WORDSIZE) (+ i 2))) argi))
  (define dests (map (lambda (i) (* (- WORDSIZE) (+ i 2))) argi))
  `(
    ,@(flatmap
        (lambda (off e) (gen-simple e (list '%esp off) '%eax))
        offsets
        (cons fn es))
    ,@(flatmap
        (lambda (off dst)
          `((mov (%esp ,off)  %eax)
            (mov %eax (%ebp ,dst))))
        offsets
        dests)
    (mov %ebp %esp)
    (mov (%ebp ,(- WORDSIZE)) %ebx) ;;; restore got pointer
    (mov (%ebp) %ebp) ;;; restore frame pointer
    (mov ,argc %ecx)
    ,@(if apply?
          `((add ,WORDSIZE %esp)
            (jmp (PLT L_apply)))
          `((mov (%esp ,(* 2 (- WORDSIZE))) %eax)
            (mov (%eax ,(- CLOS-TAG)) %eax)
            (add ,WORDSIZE %esp)
            (jmp (reg %eax))))
  ))

(define (gen-call apply? fn es si)
  (define argc (length (cons fn es)))
  `(
    ,@(flatmap
        (lambda (i e)
          (gen-simple e (list '%esp (* (+ i 4) (- WORDSIZE))) '%eax))
        (iota argc)
        (cons fn es))
    (mov ,argc %ecx)
    ,@(if apply?
          `((call (PLT L_apply)))
          `(,@(gen-simple fn '%eax)
            (mov (%eax ,(- CLOS-TAG)) %eax)
            (call (reg %eax))))
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
    (mov %ecx %eax)
    (inc %eax)
    (sal ,FIXNUM-SHIFT %eax)
    (sub %eax %esp)
    (mov %ecx %eax)
    (mov %ebp (%esp ,(* 1 (- WORDSIZE))))   ; fp
    (mov %esp (%esp ,(* 2 (- WORDSIZE))))   ; sp
    (mov %eax (%esp ,(* 3 (- WORDSIZE))))   ; argc
    (movl ,argc (%esp ,(* 4 (- WORDSIZE))))  ; expected argc
    (sub ,(* 4 WORDSIZE) %esp)
    (call (PLT construct_vararg))
    (mov %ebp %esp)  ; restore the frame pointer
    (mov %eax (%esp ,(* (+ 1 argc) (- WORDSIZE))))
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
          (gen-simple e (list '%esp off) '%eax))
        offsets es)
    ,@(flatmap
        (lambda (off dst)
          `((mov (%esp ,off) %eax)
            (mov %eax (%ebp ,dst))))
        offsets dests)
    ,@(if (pair? es) `((mov (%ebp ,(car dests)) %eax)) '())
    (mov ,argc %ecx)
    (mov %ebp %esp)
    (mov (%ebp ,(- WORDSIZE)) %ebx) ;;; restore got pointer
    (mov (%ebp) %ebp) ;;; restore frame pointer
    (add ,WORDSIZE %esp)
    (ret)
  ))

(define (gen-call-with-values tail? e1 e2 si)
  `(
    ,@(gen-simple e1 (list '%esp (* 4 (- WORDSIZE))) '%eax)
    (mov 1 %ecx)
    (mov (%eax ,(- CLOS-TAG)) %eax)
    (call (reg %eax))
    (mov %eax (%esp ,(* 5 (- WORDSIZE))))
    ,@(gen-simple e2 (list '%esp (* 4 (- WORDSIZE))) '%eax)
    (add 1 %ecx)
    ,@(if tail?
          `(
            (mov %ecx %eax)
            (add 3 %eax)
            (sal ,FIXNUM-SHIFT %eax)
            (neg %eax)
            (lea (%esp %eax) %esp)
            (mov %ebp (%esp ,(* 1 (- WORDSIZE))))
            (mov %esp (%esp ,(* 2 (- WORDSIZE))))
            (mov %ecx (%esp ,(* 3 (- WORDSIZE))))
            (lea (%esp ,(* 3 (- WORDSIZE))) %esp)
            (call (PLT shift_values))
            (mov %eax %ecx) ;;; restore argc
            (lea (%ebp ,WORDSIZE) %esp) ;;; restore sp
            (mov (%ebp ,(* 2 (- WORDSIZE))) %eax) ;;; restore closure
            (mov (%ebp ,(- WORDSIZE)) %ebx) ;;; restore got pointer
            (mov (%ebp) %ebp) ;;; restore frame pointer
            (mov (%eax ,(- CLOS-TAG)) %eax)
            (jmp (reg %eax))
          )
          `(
            (mov (%eax ,(- CLOS-TAG)) %eax)
            (call (reg %eax))
          ))))

(define (gen-call/cc tail? e si)
  (define cont (gensym* "cont"))
  `(
    (mov (GOT BOT_FP %ebx) %eax)
    (mov (%eax) %eax)
    (sub %esp %eax)
    (cmp 0 %eax)
    (add ,(* 9 WORDSIZE) %eax)
    ,@(gen-collect '%eax si)
    ,@(gen-simple e '%eax)
    (mov (,AP-REG) %edx)
    (mov %ebx (%edx))
    ,@(if tail?
          `((mov (%ebp ,WORDSIZE) %edx)
            (lea (%esp ,(* 16 (- WORDSIZE))) %esp) ;;; extra space for shrinking
            (mov %ebp (%esp ,(* 1 (- WORDSIZE))))  ;;; fp
            (mov %ebp (%esp ,(* 2 (- WORDSIZE))))  ;;; sp
            (mov %edx (%esp ,(* 3 (- WORDSIZE))))  ;;; ret
            (mov %eax (%esp ,(* 4 (- WORDSIZE))))  ;;; clos
            )
          `((mov (GOT ,cont %ebx) %edx)
            (mov %esp %ecx)
            (lea (%esp ,(* 16 (- WORDSIZE))) %esp) ;;; extra space for shrinking
            (mov %ebp (%esp ,(* 1 (- WORDSIZE))))  ;;; fp
            (mov %ecx (%esp ,(* 2 (- WORDSIZE))))  ;;; sp
            (mov %edx (%esp ,(* 3 (- WORDSIZE))))  ;;; ret
            (mov %eax (%esp ,(* 4 (- WORDSIZE))))  ;;; clos
            ))
    (lea (%esp ,(* 4 (- WORDSIZE))) %esp)
    (call (PLT s_reify_cont))
    (mov (GOT BOT_FP %ebx) %ebp)
    (mov (%ebp) %ebp)
    (lea (%ebp ,(* 3 (- WORDSIZE))) %esp)
    (mov (%eax ,(- CLOS-TAG)) %eax)
    (mov 2 %ecx)
    (jmp (reg %eax))
    ,cont
  ))

(define (gen-L-restore-stack)
  (define loop (gensym* "loop"))
  (define end (gensym* "end"))
  `(
    (:global L_explicit_restore_stack)
    (:global L_implicit_restore_stack)
    L_implicit_restore_stack
    (cmp 1 %ecx)
    (jne (PLT s_bad_imp_cont))
    (mov %eax (%esp ,(- WORDSIZE)))
    (add ,(* 3 WORDSIZE) %esp)
    (mov 2 %ecx)

    L_explicit_restore_stack
    (sub ,WORDSIZE %esp)
    (mov %ebp (%esp))
    (mov %esp %ebp)
    (mov %ebx %eax)
    ,@(gen-load-GOT-ebx)
    (mov %eax (%ebp ,(- WORDSIZE)))

    ;;; copy-args
    (lea (%ebp ,(- WORDSIZE)) %ebp)
    (mov (GOT tospace_start %ebx) %edx)
    (mov (%edx) %edx)
    (sal ,FIXNUM-SHIFT %ecx)
    (mov %ecx (%edx))

    ,loop
    (cmp 0 %ecx)
    (jle ,end)
    (neg %ecx)
    (mov (%ebp %ecx) %eax)
    (neg %ecx)
    (mov %eax (%edx %ecx))
    (sub ,WORDSIZE %ecx)
    (jmp ,loop)
    ,end

    ;;; cont, calculating the stack_size
    (mov (%esp ,(* 2 (- WORDSIZE))) %eax)
    (mov (%eax ,(- (* 1 WORDSIZE) CLOS-TAG)) %eax)
    (sub ,(* 5 WORDSIZE) %eax)
    (neg %eax)
    
    (mov (GOT BOT_FP %ebx) %esp)
    (mov (%esp) %esp)
    (lea (%esp %eax) %esp)
    (lea (%esp ,(* 2 (- WORDSIZE))) %esp)

    (mov (%edx) %ecx) ;;; argc
    (neg %ecx)
    (lea (%esp %ecx) %esp)
    (lea (%esp ,(* 16 (- WORDSIZE))) %ebp)
    (mov %esp (%ebp ,(* 1 (- WORDSIZE))))
    (lea (%ebp ,(* 1 (- WORDSIZE))) %esp)
    (call (PLT s_apply_cont))
    (mov (,AP-REG) %edx)
    (mov (%edx ,(* 0 WORDSIZE)) %ecx) ; argc
    (mov (%edx ,(* 1 WORDSIZE)) %esp) ; stack pointer
    (mov (%edx ,(* 2 WORDSIZE)) %ebp) ; frame pointer
    (mov (%edx ,(* 3 WORDSIZE)) %ebx) ; got pointer
    (mov (%edx ,(* 4 WORDSIZE)) %edx) ; ret
    (jmp (reg %edx))
  ))

(define (gen-expr e si)
  (match e
    (,()
     (guard (or (integer? e) (char? e) (boolean? e)))
     (gen-simple e '%eax))
    (,()
     (guard (string? e))
     (gen-str (string->list e) si))
    ((quote ,q)
     `((mov ,(immediate-repr q) %eax)))
    ((label ,lbl)
     (gen-simple e '%eax))
    ((label* ,lbl)
     (gen-simple e '%eax))
    ((call ,tail? (prim push-constant-table))
     `((mov (GOT .constant_table %ebx) %eax)
       (mov (GOT LBL_TBL %ebx) %edx)
       (mov (%edx) %ecx)
       (mov %ecx (%eax))
       (mov %eax (%edx))))
    ((call ,tail? (prim %set-label!) (label* ,x) ,e)
     `(,@(gen-simple e '%eax)
       (mov (GOT ,x %ebx) %ecx)
       (mov %eax (%ecx))))
    ((call ,tail? (prim apply) ,fn . ,es)
     ((if tail? gen-tail-call gen-call) #t fn es si))
    ((call ,tail? (prim values) . ,es)
     (if (not tail?)
         (error "gen-expr" "only-tail-values" e))
     (gen-values es si))
    ((call ,tail? (prim call-with-values) ,e1 ,e2)
     (gen-call-with-values tail? e1 e2 si))
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
        (sub 1 %ecx)
        (mov (%esp ,(* 4 (- WORDSIZE))) %eax)
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

(define (gen-load-GOT-ebx)
  (define get_pc_bx (gensym* "get_pc_bx"))
  `((call ,get_pc_bx)
    ,get_pc_bx
    (pop %ebx)
    (addl ($ ,(format "$_GLOBAL_OFFSET_TABLE_+(.-~a)" get_pc_bx)) %ebx)))

(define (gen-main entries)
  `(
    (:global main)
    ,@(gen-L-apply)
    ,@(gen-L-restore-stack)
    main
    (push %ebp)
    (mov %esp %ebp)
    (push %ebx)
    ,@(gen-load-GOT-ebx)
    (mov (GOT BOT_FP %ebx) %eax)
    (mov %ebp (%eax))
    (mov (%esp ,(* 4 WORDSIZE)) %ecx)
    (mov (%esp ,(* 3 WORDSIZE)) %eax)
    (push %ecx)
    (push %eax)
    (call (PLT _scm_init))
    (add ,(* 2 WORDSIZE) %esp)
    (mov (GOT free_ptr %ebx) ,AP-REG)
    (mov #xbeef %ebx)
    ,@(flatmap
        (lambda (e)
          `((movl ,ENTRY-MARK (%esp ,(* 4 (- WORDSIZE))))
            (mov 1 %ecx)
            (call (PLT ,e))))
        entries)
    (cmp #xbeef %ebx)
    (jne (PLT abort))
    (pop %ebx)
    (pop %ebp)
    (mov 0 %eax)
    (ret)
  ))

(vector gen-unit gen-main)
)) ;;; code-gen

(define gen-unit (vector-ref code-gen 0))
(define gen-main (vector-ref code-gen 1))

(define CC (maybe-getenv "CC" "gcc"))
(define CFLAGS (maybe-getenv "CFLAGS" "-g -Wall -m32 -fno-omit-frame-pointer"))
(define OBJCOPY (maybe-getenv "OBJCOPY" "objcopy"))

(define emit-ir-seq (let ()
  (define (emit op . args)
    (apply format (cons op args))
    (newline op))
  (define REGISTERS
    '(%eax %ebx %ecx %edx %esi %edi %esp %ebp
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
      ((GOT ,lbl ,reg)
       (format "~a@GOT(~a)" lbl reg))
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
      ((cdq)
       (emit op "cdq"))
      (,() (error "emit-ir" "unmatch" instr))))

  (define (emit-ir-seq op instrs)
    (if (pair? instrs)
        (begin
          (emit-ir op (car instrs))
          (emit-ir-seq op (cdr instrs)))))

  emit-ir-seq)) ;;; emit-ir-seq

(include "compiler-tools.scm")
