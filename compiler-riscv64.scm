(import
  (only (match) match)
  (only (utils)
    align-to-multiple gensym*
    flatmap zip applicate
    read-sexps-from-path path-filestem path-extension
    make-tempname system* maybe-getenv)
  (only (front) preprocess))

#|
Name ABI  Mnemonic  Meaning Preserved across calls?
a0        zero      Zero — (Immutable)
a1        ra        Return address No
x2        sp        Stack pointer Yes
x3        gp        Global pointer — (Unallocatable)
x4        tp        Thread pointer — (Unallocatable)
x5 - x7   t0 - t2   Temporary registers No
x8 - x9   s0 - s1   Callee-saved registers Yes
a10 - a17 a0 - a7   Argument registers No
a18 - x27 s2 - s11  Callee-saved registers Yes
x28 - x31 t3 - t6   Temporary registers No
|#
(define ABI-MNEMONIC-CORPSD
  '(
    (x0 zero)
    (x1 ra)
    (x2 sp)
    (x3 gp)
    (x4 tp)
    (x5 t0)
    (x6 t1)
    (x7 t2)
    (x8 s0 fp)
    (x9 s1)
    (a10 a0)
    (a11 a1)
    (a12 a2)
    (a13 a3)
    (a14 a4)
    (a15 a5)
    (a16 a6)
    (a17 a7)
    (a18 s2)
    (a19 s3)
    (x20 s4)
    (x21 s5)
    (x22 s6)
    (x23 s7)
    (x24 s8)
    (x25 s9)
    (x26 s10)
    (x27 s11)
    (x28 t3)
    (x29 t4)
    (x30 t5)
    (x31 t6)
  ))
(define ARGS-REG '(a0 a1 a2 a3 a4 a5 a6 a7))
(define REGISTERS (fold-left append '() ABI-MNEMONIC-CORPSD))

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

(define (gen-address-of-global dst sym)
  (define tmp (gensym* ".Ltmp"))
  `(,tmp
    (auipc ,dst (%got_pcrel_hi ,sym))
    (addi ,dst ,dst (%pcrel_lo ,tmp))
    (ld ,dst (,dst))))

(define (gen-address-of-local dst sym)
  (define tmp (gensym* ".Ltmp"))
  `(,tmp
    (auipc ,dst (%pcrel_hi ,sym))
    (addi ,dst ,dst (%pcrel_lo ,tmp))))

(define (immediate-repr e)
  (cond
    ((integer? e) (ash e FIXNUM-SHIFT))
    ((char? e) (bitwise-ior (ash (char->integer e) IMM-SHIFT) CHAR-TAG))
    ((boolean? e) (if e TRUE-IMM FALSE-IMM))
    ((null? e) NIL-TAG)
    ((eq? e (void)) VOID-TAG)
    ((eq? e (eof-object)) EOF-TAG)
    (else (error "immediate-repr" "unknown" e))))

(define AP-REG 's1)
(define FP-REG 's0)
(define BOT-FP-REG 's2)
(define ARGC-REG 't0)

(define (gen-simple e dst)
  (match e
    (,()
     (guard (or (integer? e) (char? e) (boolean? e)))
     `((li ,dst ,(immediate-repr e))))
    ((quote ,q)
     `((li ,dst ,(immediate-repr q))))
    ((label ,lbl) ;;; global-label
     `(,@(gen-address-of-global dst lbl)
       (ld ,dst (,dst))))
    ((label* ,lbl) ;;; local-label
     `((ld ,dst (label ,lbl))))
    ((call ,tail? (prim local-ref) ,i)
     `((addi ,dst fp ,(* (- WORDSIZE) (+ i 2)))
       (ld ,dst (,dst))))
    ((call ,tail? (prim closure-ref) ,clos -1)
     `((ld ,dst (fp ,(* (- WORDSIZE) 2)))))
    ((call ,tail? (prim closure-ref) ,clos ,i)
     (guard (>= i 0))
     `((ld ,dst (fp ,(* (- WORDSIZE) 2)))
       (addi ,dst ,dst ,(- (* (+ i 2) WORDSIZE) CLOS-TAG))
       (ld ,dst (,dst))))
    (,() (error "gen-simple" "unmatch" e))))

(define (gen-align align off)
  `(
    (addi ,off ,off ,(- align 1))
    (andi ,off ,off ,(- align))
    ))

(define (gen-eq dst src tmp val)
  `(
    (addi ,dst ,src ,(- val))
    (seqz ,dst ,dst)
    (slli ,dst ,dst ,IMM-SHIFT)
    (ori ,dst ,dst ,BOOL-TAG)
    ))

(define (gen-obj-test e mask tag)
  (define alt (gensym* ".Lalt"))
  (define end (gensym* ".Lend"))
  `(
    ,@(gen-simple e 'a0)
    (andi a1 a0 ,OBJ-MASK)
    (li a2 ,OBJ-TAG)
    (bne a1 a2 ,alt)
    (ld a0 (a0 ,(- OBJ-TAG)))
    (andi a0 a0 ,mask)
    (addi a0 a0 ,(- tag))
    (seqz a0 a0)
    (slli a0 a0 ,IMM-SHIFT)
    (ori a0 a0 ,BOOL-TAG)
    (j ,end)
    ,alt
    (li a0 ,FALSE-IMM)
    ,end
    ))

;;; TODO: fix the size here
(define (gen-str vs si)
  (define sz (align-to-multiple 8 (+ WORDSIZE (* (length vs) 4))))
  `(
    ,@(gen-collect (ash sz (- FIXNUM-SHIFT)) si)
    (ld a0 (,AP-REG))
    (li t0 ,sz)
    (add t0 a0 t0)
    (sd (,AP-REG) t0)
    (li t0 ,(bitwise-ior STR-TAG (immediate-repr (length vs))))
    (sd (a0) t0)
    ,@(flatmap
        (lambda (i v)
          `(
            ,@(gen-simple v 'a1)
            (li t0 ,(+ (* i 4) WORDSIZE))
            (add t0 a0 t0)
            (sd (t0) a1)
          ))
        (iota (length vs))
        vs)
    (ori a0 a0 ,OBJ-MASK)
  ))

(define (gen-vec vs si)
  (define vlen (length vs))
  (define sz (align-to-multiple 8 (* (+ vlen 1) WORDSIZE)))
  `(
    ,@(gen-collect sz si)
    (ld a0 (,AP-REG))
    (li t0 ,sz)
    (add t0 a0 t0)
    (sd (,AP-REG) t0)
    (li t0 ,(bitwise-ior VEC-TAG (immediate-repr (length vs))))
    (sd (a0) t0)
    ,@(flatmap
        (lambda (i v)
          `(
            ,@(gen-simple v 'a1)
            (li t0 ,(* (+ i 1) WORDSIZE))
            (add t0 a0 t0)
            (sd (t0) a1)
          ))
        (iota vlen)
        vs)
    (ori a0 a0 ,OBJ-MASK)
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
    (< blt)
    (<= ble)
    (eq? beq)
    (= beq)
    (>= bge)
    (> bgt)
  ))

(define (gen-cmp cmp e1 e2 si)
  (define csq (gensym* ".Lcsq"))
  (define end (gensym* ".Lend"))
  `(
    ,@(gen-simple e2 'a1)
    ,@(gen-simple e1 'a0)
    (,(cadr (assq cmp cmp-suffix)) a0 a1 ,csq)
    (li a0 ,FALSE-IMM)
    (j ,end)
    ,csq
    (li a0 ,TRUE-IMM)
    ,end
  ))

(define (gen-prim e si)
  (match e
    ((local-set! ,i ,e1)
     `(
      ,@(gen-expr e1 si)
      (li a1 ,(* (- WORDSIZE) (+ i 2)))
      (add a1 fp a1)
      (sd (a1) a0)
      ))
    ((eq? ,e1 ,e2)
     (gen-cmp 'eq? e1 e2 si))
    ((boolean? ,e1)
     `(
      ,@(gen-simple e1 'a0)
      (andi a0 a0 ,IMM-MASK)
      ,@(gen-eq 'a0 'a0 't0 BOOL-TAG)
      ))
    ((not ,e1)
     `(
      ,@(gen-simple e1 'a0)
      ,@(gen-eq 'a0 'a0 't0 FALSE-IMM)
     ))
    ((eof-object? ,e1)
     `(
      ,@(gen-simple e1 'a0)
      (andi a0 a0 ,IMM-MASK)
      ,@(gen-eq 'a0 'a0 't0 EOF-TAG)
      ))
    ((null? ,e1)
     `(
      ,@(gen-simple e1 'a0)
      (andi a0 a0 ,IMM-MASK)
      ,@(gen-eq 'a0 'a0 't0 NIL-TAG)
      ))
    ((char? ,e1)
     `(
      ,@(gen-simple e1 'a0)
      (andi a0 a0 ,IMM-MASK)
      ,@(gen-eq 'a0 'a0 't0 CHAR-TAG)
      ))
    ((char->integer ,e1)
     `(
      ,@(gen-simple e1 'a0)
      (srai a0 a0 ,(- IMM-SHIFT FIXNUM-SHIFT))
      ))
    ((integer->char ,e1)
     `(
      ,@(gen-simple e1 'a0)
      (slli a0 a0 ,(- IMM-SHIFT FIXNUM-SHIFT))
      (ori a0 a0 ,CHAR-TAG)
      ))
    ((integer? ,e1)
     `(
      ,@(gen-simple e1 'a0)
      (andi a0 a0 ,FIXNUM-MASK)
      ,@(gen-eq 'a0 'a0 't0 FIXNUM-TAG)
      ))
    ((,cmp ,e1 ,e2)
     (guard (assq cmp cmp-suffix))
     (gen-cmp cmp e1 e2 si))
    ((- ,e1 ,e2)
     `(
      ,@(gen-simple e2 'a1)
      ,@(gen-simple e1 'a0)
      (sub a0 a0 a1)
      ))
    ((+ ,e1 ,e2)
     `(
      ,@(gen-simple e2 'a1)
      ,@(gen-simple e1 'a0)
      (add a0 a0 a1)
      ))
    ((* ,e1 ,e2)
     `(
      ,@(gen-simple e2 'a1)
      ,@(gen-simple e1 'a0)
      (srai a0 a0 ,FIXNUM-SHIFT)
      (mul a0 a0 a1)
      ))
    ((div ,dividend ,divisor)
     #;((div (* t x) (* t y))
        => (div x y)
        => (tag (div x y)))
     `(
        ,@(gen-simple divisor 'a1)
        ,@(gen-simple dividend 'a0)
        (div a0 a0 a1)
        (slli a0 a0 ,FIXNUM-SHIFT)
      ))
    ((mod ,dividend ,divisor)
     ;;; TODO: don't know why this works?
     `(
        ,@(gen-simple divisor 'a1)
        ,@(gen-simple dividend 'a0)
        (rem a0 a0 a1)
     ))
    ((abs ,e1)
     (define end (gensym* ".Lend"))
     `(
      ,@(gen-simple e1 'a0)
      (blt x0 a0 ,end)
      (neg a0 a0)
      ,end
      ))
    ((max ,e1 ,e2)
     (define end (gensym* ".Lend"))
     `(
       ,@(gen-simple e2 'a1)
       ,@(gen-simple e1 'a0)
       (blt a1 a0 ,end)
       (mv a0 a1)
       ,end
       ))
    ((ash ,e1 ,e2)
     (define alt (gensym* ".Lalt"))
     (define end (gensym* ".Lend"))
     `(
      ,@(gen-simple e2 'a1)
      ,@(gen-simple e1 'a0)
      (srai a1 a1 ,FIXNUM-SHIFT)
      (blt a1 x0 ,alt)
      ; e2 >= 0 - left shift
      (sll a0 a0 a1)
      (j ,end)
      ,alt
      ; e2 < 0 - right shift
      (neg a1 a1)
      (srai a0 a0 ,FIXNUM-SHIFT)
      (sra a0 a0 a1)
      (slli a0 a0 ,FIXNUM-SHIFT)
      ,end
     ))
    ((bitwise-and ,e1 ,e2)
     `(
      ,@(gen-simple e2 'a1)
      ,@(gen-simple e1 'a0)
      (and a0 a0 a1)
     ))
    ((bitwise-ior ,e1 ,e2)
     `(
      ,@(gen-simple e2 'a1)
      ,@(gen-simple e1 'a0)
      (or a0 a0 a1)
     ))
    ((pair? ,e1)
     `(
      ,@(gen-simple e1 'a0)
      (andi a0 a0 ,OBJ-MASK)
      ,@(gen-eq 'a0 'a0 't0 PAIR-TAG)
      ))
    ((cons ,e1 ,e2)
     `(
      ,@(gen-collect 2 si)
      ,@(gen-simple e2 'a2)
      ,@(gen-simple e1 'a1)
      (ld a0 (,AP-REG))
      (addi t0 a0 ,(* 2 WORDSIZE))
      (sd (,AP-REG) t0)
      (sd (a0) a1)
      (sd (a0 ,WORDSIZE) a2)
      (ori a0 a0 ,PAIR-TAG)
      ))
    ((car ,e)
     `(
      ,@(gen-simple e 'a0)
      (ld a0 (a0 ,(- PAIR-TAG)))
      ))
    ((cdr ,e)
     `(
      ,@(gen-simple e 'a0)
      (ld a0 (a0 ,(- WORDSIZE PAIR-TAG)))
      ))
    ((set-car! ,e1 ,e2)
     `(
      ,@(gen-simple e2 'a1)
      ,@(gen-simple e1 'a0)
      (sd (a0 ,(- PAIR-TAG)) a1)
      ))
    ((set-cdr! ,e1 ,e2)
     `(
      ,@(gen-simple e2 'a1)
      ,@(gen-simple e1 'a0)
      (sd (a0 ,(- WORDSIZE PAIR-TAG)) a1)
      ))
    ((string? ,e1)
     (gen-obj-test e1 STR-MASK STR-TAG))
    ((string . ,es)
     (gen-str es si))
    ((string-length ,e1)
     `(
      ,@(gen-simple e1 'a0)
      (ld a0 (a0 ,(- OBJ-TAG)))
      (addi a0 a0 ,(- STR-TAG))
     ))
    ((string-ref ,e1 ,e2)
     `(
      ,@(gen-simple e1 'a0)
      ,@(gen-simple e2 'a1)
      (srai a1 a1 1)
      (add a0 a0 a1)
      (lw a0 (a0 ,(- WORDSIZE OBJ-TAG)))
     ))
    ((string-set! ,e1 ,e2 ,e3)
     `(
      ,@(gen-simple e1 'a0)
      ,@(gen-simple e2 'a1)
      ,@(gen-simple e3 'a2)
      (srai a1 a1 1)
      (add a0 a0 a1)
      (sw (a0 ,(- WORDSIZE OBJ-TAG)) a2)
      (li a0 ,VOID-TAG)
     ))
    ((make-string ,e1 ,e2)
     `(,@(gen-simple e1 'a0)
       (srai a0 a0 1)
       (addi a0 a0 ,WORDSIZE)
       ,@(gen-align 8 'a0)
       ,@(gen-collect 'a0 si)
       ,@(gen-foreign-call 's_make_string (list e1 e2) si)))
    ((vector . ,es)
     (gen-vec es si))
    ((vector? ,e1)
     (gen-obj-test e1 VEC-MASK VEC-TAG))
    ((vector-length ,e1)
     `(
      ,@(gen-simple e1 'a0)
      (ld a0 (a0 ,(- OBJ-TAG)))
      (addi a0 a0 ,(- VEC-TAG))
     ))
    ((vector-ref ,e1 ,e2)
     `(
      ,@(gen-simple e1 'a0)
      ,@(gen-simple e2 'a1)
      (add a0 a0 a1)
      (ld a0 (a0 ,(- WORDSIZE OBJ-TAG)))
     ))
    ((vector-set! ,e1 ,e2 ,e3)
     `(
      ,@(gen-simple e1 'a0)
      ,@(gen-simple e2 'a1)
      ,@(gen-simple e3 'a2)
      (add a0 a0 a1)
      (sd (a0 ,(- WORDSIZE OBJ-TAG)) a2)
      (li a0 ,VOID-TAG)
     ))
    ((make-vector ,e1 ,e2)
     `(
      ,@(gen-simple e1 'a0)
      (addi a0 a0 ,WORDSIZE)
      ,@(gen-align 8 'a0)
      ,@(gen-collect 'a0 si)
      ,@(gen-foreign-call 's_make_vector (list e1 e2) si)
     ))
    ((bytevector? ,e1)
     (gen-obj-test e1 BYTEVEC-MASK BYTEVEC-TAG))
    ((make-bytevector ,e1 . ,maybe-e2)
     (define e2 (if (pair? maybe-e2) (car maybe-e2) #f))
     `(
      ,@(gen-simple e1 'a0)
      (srai a0 a0 ,FIXNUM-SHIFT)
      (addi a0 a0 ,WORDSIZE)
      ,@(gen-align 8 'a0)
      ,@(gen-collect 'a0 si)
      ,@(gen-foreign-call 's_make_bytevector (list e1 e2) si)
      ))
    ((bytevector-length ,e1)
     `(
      ,@(gen-simple e1 'a0)
      (ld a0 (a0 ,(- OBJ-TAG)))
      (addi a0 a0 ,(- BYTEVEC-TAG))
     ))
    ((bytevector-u8-ref ,e1 ,e2)
     `(
      ,@(gen-simple e1 'a0)
      ,@(gen-simple e2 'a1)
      (srai a1 a1 ,FIXNUM-SHIFT)
      (add a0 a0 a1)
      (lbu a0 (a0 ,(- WORDSIZE OBJ-MASK)))
      (slli a0 a0 ,FIXNUM-SHIFT)
     ))
    ((bytevector-u8-set! ,e1 ,e2 ,e3)
     `(
      ,@(gen-simple e1 'a0)
      ,@(gen-simple e2 'a1)
      ,@(gen-simple e3 'a2)
      (srai a1 a1 ,FIXNUM-SHIFT)
      (add a0 a0 a1)
      (srai a2 a2 ,FIXNUM-SHIFT)
      (sb (a0 ,(- WORDSIZE OBJ-TAG)) a2)
     ))
    ((symbol? ,e1)
     `(
      ,@(gen-simple e1 'a0)
      (andi a0 a0 ,OBJ-MASK)
      ,@(gen-eq 'a0 'a0 't0 SYM-TAG)
      ))
    ((%make-symbol ,hash ,name)
     `(
      ,@(gen-collect 4 si)
      (ld a0 (,AP-REG))
      (addi t0 a0 ,(* 4 WORDSIZE))
      (sd (,AP-REG) t0)
      ,@(gen-simple name 'a2)
      ,@(gen-simple hash 'a1)
      (li a3 ,UNBOUND-TAG)
      (sd (a0 ,(* 2 WORDSIZE)) a2)
      (sd (a0 ,WORDSIZE) a1)
      (sd (a0) a3)
      (addi a0 a0 ,SYM-TAG)
     ))
    ((%symbol-name ,e1)
     `(
      ,@(gen-simple e1 'a0)
      (ld a0 (a0 ,(- (* 2 WORDSIZE) SYM-TAG)))
      ))
    ((%symbol-hash ,e1)
     `(
      ,@(gen-simple e1 'a0)
      (ld a0 (a0 ,(- (* 1 WORDSIZE) SYM-TAG)))
      ))
    ((%symbol-value ,e1)
     (define end (gensym* ".Lend"))
     `(
      ,@(gen-simple e1 'a0)
      (ld a0 (a0 ,(- SYM-TAG)))
      (li t0 ,UNBOUND-TAG)
      (bne a0 t0 ,end)
      (call (PLT s_unbound_global))
      ,end
      ))
    ((%symbol-value-set! ,e1 ,e2)
     `(
      ,@(gen-simple e2 'a1)
      ,@(gen-simple e1 'a0)
      (sd (a0 ,(- SYM-TAG)) a1)
      ))
    ((procedure? ,e1)
     `(
      ,@(gen-simple e1 'a0)
      (andi a0 a0 ,PTR-MASK)
      ,@(gen-eq 'a0 'a0 't0 CLOS-TAG)
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
     (gen-simple `(call #f (prim closure-ref) ,clos ,i) 'a0))
    ((closure-set! ,e1 ,i ,e2)
     `(
        ,@(gen-simple e1 'a0)
        ,@(gen-simple e2 'a1)
        (li a2 ,(- (* (+ i 2) WORDSIZE) CLOS-TAG))
        (add a0 a0 a2)
        (sd (a0) a1)
       ))
    ((collect)
     (gen-collect #f si))
    ((collect ,e1)
     (append
      (gen-simple e1 'a0)
      (gen-collect 'a0 si)))
    ((local-ref ,i)
     (gen-simple `(call #f (prim local-ref) ,i) 'a0))
    (,() (error "gen-prim" "unmatch" e))))

(define (gen-if pred conseq altern si)
  (define csq (gensym* ".Lcsq"))
  (define alt (gensym* ".Lalt"))
  (define end (gensym* ".Lend"))
  `(
    ,@(gen-simple pred 'a0)
    (li t0 ,FALSE-IMM)
    (beq a0 t0 ,alt)
    ,csq
    ,@(gen-expr conseq si)
    (j ,end)
    ,alt
    ,@(gen-expr altern si)
    ,end
  ))

(define (gen-collect sz si)
  `(
    (mv a2 fp)
    (mv a1 sp)
    ,@(if (eq? sz 'a0)
          `()
          (gen-simple sz 'a0))
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
  argc -> a1
  ;;; in callee: before adjust the frame-pointer and stack-pointer
  (... ret-addr^ ((FP) fp0 fp-1) local-0 ... local-n
       ((SP) ret-addr) (_ fp1 fp0) arg-0 ... arg-n ((SP*) <top>))
  ;;; in callee: after adjust the frame-pointer and stack-pointer
  (... ret-addr^ fp-1 local-0 ... local-n
       ret-addr ((SP FP) fp1 fp0) arg-0 ... arg-n ((SP*) <top>))
)
(define (gen-procedure fn params variadic? body)
  (define argc (length params))
  (define csq (gensym* ".Lcsq"))
  (match body
    ((let ((_ (call #f (prim alloca) ,n*))) ,e)
     ;;; 16 bytes == 2 * 8
     (define n (div (align-to-multiple 16 (* (+ n* 1) WORDSIZE)) WORDSIZE))
     `(
        ,fn
        (sd (sp ,(* 1 (- WORDSIZE))) ra)
        (sd (sp ,(* 2 (- WORDSIZE))) fp)
        (addi sp sp ,(* 2 (- WORDSIZE)))
        (mv fp sp)
        ,@(if variadic?
              (gen-construct-vararg argc)
              `((li t1 ,argc)
                (beq ,ARGC-REG t1 ,csq)
                (mv a1 ,ARGC-REG)
                (li a0 ,argc)
                (call (PLT s_bad_monovariadic_call))
                ,csq))
        ,@(flatmap
            (lambda (i)
              `((li t0 ,(* (+ i 2) WORDSIZE))
                (sub t0 sp t0)
                (sd (t0) x0)))
            (filter (lambda (i) (>= i argc)) (iota n)))
        (li t0 ,(* n WORDSIZE))
        (sub sp sp t0)
        ,@(gen-expr e #f)
        ;;; restore stack pointer
        (addi sp fp ,(* 2 WORDSIZE))
        ;;; restore frame pointer and return address
        (ld ra (sp ,(* 1 (- WORDSIZE))))
        (ld fp (sp ,(* 2 (- WORDSIZE))))
        (li ,ARGC-REG 1)
        (ret)
      ))
    (,() (error "gen-procedure" "unknown" body))))

(define (gen-L-apply)
  (define loop (gensym* ".Lunstack"))
  (define loop-end (gensym* ".Lunstack"))
  (define end (gensym* ".Lend"))
  ;;; TODO: ra and fp sequence can be omitted here.
  `((:global L_apply)
    L_apply
    (sd (sp ,(* 1 (- WORDSIZE))) ra)
    (sd (sp ,(* 2 (- WORDSIZE))) fp)
    (addi sp sp ,(* 2 (- WORDSIZE)))
    (mv fp sp)
    ;;; a0 - moving stack pointer
    ;;; a1 - list
    (addi a0 ,ARGC-REG 1)
    (slli a0 a0 ,FIXNUM-SHIFT)
    (sub a0 sp a0)
    (ld a1 (a0))
    (li t2 ,PAIR-TAG)
    ,loop
    (andi t1 a1 ,OBJ-MASK)
    (bne t1 t2 ,loop-end)                 ; (pair? args)
    (ld t1 (a1 ,(- PAIR-TAG)))            ; (car args)
    (sd (a0) t1)                          ; (sp) <- (car args)
    (addi a0 a0 ,(- WORDSIZE))            ; decrement sp
    (ld a1 (a1 ,(- WORDSIZE PAIR-TAG)))   ; args <- (cdr cdr)
    (j ,loop)
    ,loop-end
    (li t0 ,NIL-TAG)
    (beq a1 t0 ,end)
    (call (PLT s_bad_apply_call))
    ,end
    (sub ,ARGC-REG sp a0)
    (addi ,ARGC-REG ,ARGC-REG ,(* 2 (- WORDSIZE)))
    (srai ,ARGC-REG ,ARGC-REG ,FIXNUM-SHIFT)
    (addi a0 sp ,(* 2 (- WORDSIZE)))
    (ld a0 (a0))
    (ld a0 (a0 ,(- CLOS-TAG)))
    ;;; restore stack pointer
    (addi sp fp ,(* 2 WORDSIZE))
    ;;; restore frame pointer and return address
    (ld ra (sp ,(* 1 (- WORDSIZE))))
    (ld fp (sp ,(* 2 (- WORDSIZE))))
    (jr a0)
    ))

(define (gen-polyvariadic-proc fn clauses)
  ;;; TODO: ra and fp sequence can be omitted here.
  (define (free-vars-of-clause clause)
    (match clause
      ((call ,tail? (prim closure*) ,variadic? ,argc (label ,fn). ,fvs)
       fvs)
      (,() (error "free-vars-of-clause" "unmatch" clause))))
  (define (gen-clause clause)
    (define nxt (gensym* ".Lalt"))
    (match clause
      ((,i (label ,fn) ,variadic? ,argc)
       `(
          ,@(if variadic?
                `((li t1 ,(- argc 1))
                  (blt ,ARGC-REG t1 ,nxt))
                `((li t1 ,argc)
                  (bne ,ARGC-REG t1 ,nxt)))
          (ld a0 (fp ,(* (- WORDSIZE) 2)))
          (addi a0 a0 ,(- (* WORDSIZE (+ i 2)) CLOS-TAG))
          (ld a0 (a0))
          (sd (fp ,(* (- WORDSIZE) 2)) a0)
          ;;; restore stack pointer
          (addi sp fp ,(* 2 WORDSIZE))
          ;;; restore frame pointer and return address
          (ld ra (sp ,(* 1 (- WORDSIZE))))
          (ld fp (sp ,(* 2 (- WORDSIZE))))
          (j ,fn)
          ,nxt
       ))
      (,() (error "gen-clause" "unmatch" clause))
      ))
  `(
    ,fn
    (sd (sp ,(* 1 (- WORDSIZE))) ra)
    (sd (sp ,(* 2 (- WORDSIZE))) fp)
    (addi sp sp ,(* 2 (- WORDSIZE)))
    (mv fp sp)
    ,@(flatmap gen-clause clauses)
    (call (PLT s_bad_polyvariadic_call))
  ))

(define (gen-closure fn fvs)
  (define fvs-cnt (length fvs))
  (define sz (align-to-multiple 8 (* (+ fvs-cnt 2) WORDSIZE)))
  `(
    (ld a0 (,AP-REG))
    (li t0 ,sz)
    (add t0 a0 t0)
    (sd (,AP-REG) t0)
    ,@(flatmap
        (lambda (i fv)
          `(,@(gen-simple fv 'a1)
            (li t0 ,(* (+ i 2) WORDSIZE))
            (add t0 a0 t0)
            (sd (t0) a1)
            ))
        (iota (length fvs)) fvs)

    (li t0 ,(immediate-repr fvs-cnt))
    (sd (a0 ,WORDSIZE) t0)

    (la a1 (label ,fn))
    (sd (a0) a1)

    (addi a0 a0 ,CLOS-TAG)
  ))

(define (gen-tail-call apply? fn es si)
  (define argc (length (cons fn es)))
  (define argi (iota argc))
  ;;; TODO: not really right here
  (define offsets (map (lambda (i) (* (- WORDSIZE) (+ i 2))) argi))
  (define dests (map (lambda (i) (* (- WORDSIZE) (+ i 2))) argi))
  `(
    ,@(flatmap
        (lambda (off e)
          `(
            ,@(gen-simple e 'a0)
            (li t0 ,off)
            (add t0 sp t0)
            (sd (t0) a0)
          ))
        offsets
        (cons fn es))
    ,@(flatmap
        (lambda (off dst)
          `((li t0 ,off)
            (add t0 sp t0)
            (ld t0 (t0))
            (li t1 ,dst)
            (add t1 fp t1)
            (sd (t1) t0)))
        offsets
        dests)
    (ld a0 (fp ,(* (- WORDSIZE) 2)))
    ;;; restore stack pointer
    (addi sp fp ,(* 2 WORDSIZE))
    ;;; restore frame pointer and return address
    (ld ra (sp ,(* 1 (- WORDSIZE))))
    (ld fp (sp ,(* 2 (- WORDSIZE))))
    (li ,ARGC-REG ,argc)
    ,@(if apply?
          `((tail (PLT L_apply)))
          `((ld t1 (a0 ,(- CLOS-TAG)))
            (jr t1)))
  ))

(define (gen-call apply? fn es si)
  (define argc (length (cons fn es)))
  `(
    ,@(flatmap
        (lambda (i e)
          `(
            ,@(gen-simple e 'a0)
            (li t0 ,(* (+ i 4) (- WORDSIZE)))
            (add t0 sp t0)
            (sd (t0) a0)
          ))
        (iota argc)
        (cons fn es))
    (li ,ARGC-REG ,argc)
    ,@(if apply?
          `((call (PLT L_apply)))
          `(,@(gen-simple fn 'a0)
            (ld t1 (a0 ,(- CLOS-TAG)))
            (jalr t1)))
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
    (addi t1 ,ARGC-REG 1)
    (slli t1 t1 ,FIXNUM-SHIFT)
    (sub sp sp t1)
    (mv a3 fp)
    (mv a2 sp)
    (mv a1 ,ARGC-REG)
    (li a0 ,argc)
    (andi sp sp ,-16)
    (call (PLT construct_vararg))
    (mv sp fp)
    (sd (fp ,(* (+ 1 argc) (- WORDSIZE))) a0)
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
          `(,@(gen-simple e 'a0)
            (li t0 ,off)
            (add t0 sp t0)
            (sd (t0) a0)))
        offsets es)
    ,@(flatmap
        (lambda (off dst)
          `((li t0 ,off)
            (add t0 sp t0)
            (li t1 ,dst)
            (add t1 fp t1)
            (ld t0 (t0))
            (sd (t1) t0)))
        offsets dests)
    ,@(if (pair? es) `((ld a0 (fp ,(car dests)))) '())
    (li ,ARGC-REG ,argc)
    ;;; restore stack pointer
    (addi sp fp ,(* 2 WORDSIZE))
    ;;; restore frame pointer and return address
    (ld ra (sp ,(* 1 (- WORDSIZE))))
    (ld fp (sp ,(* 2 (- WORDSIZE))))
    (ret)
  ))

(define (gen-call-with-values tail? e1 e2 si)
  `(
    ,@(gen-simple e1 'a0)
    (sd (sp ,(* 4 (- WORDSIZE))) a0)
    (li ,ARGC-REG 1)
    (ld a0 (a0 ,(- CLOS-TAG)))
    (jalr a0)
    (sd (sp ,(* 5 (- WORDSIZE))) a0)
    ,@(gen-simple e2 'a0)
    (sd (sp ,(* 4 (- WORDSIZE))) a0)
    (addi ,ARGC-REG ,ARGC-REG 1)
    ,@(if tail?
          `(
            (mv a2 fp)
            (addi a1 ,ARGC-REG 3)
            (slli a1 a1 ,FIXNUM-SHIFT)
            (sub a1 sp a1)   ;;; sp
            (mv a0 ,ARGC-REG)    ;;; argc
            (addi sp a1 ,(- WORDSIZE))
            (andi sp sp -16)
            (call (PLT shift_values))
            (mv ,ARGC-REG a0)
            (ld a0 (fp ,(* 2 (- WORDSIZE))))
            (addi sp fp ,(* 2 WORDSIZE))
            ;;; restore frame pointer and return address
            (ld ra (sp ,(* 1 (- WORDSIZE))))
            (ld fp (sp ,(* 2 (- WORDSIZE))))
            (ld a0 (a0 ,(- CLOS-TAG)))
            (jr a0)
          )
          `(
            (ld a0 (a0 ,(- CLOS-TAG)))
            (jalr a0)
          ))))

(define (gen-call/cc tail? e si)
  (define cont (gensym* ".Lcont"))
  `(
    (mv a0 sp)
    (sub a0 ,BOT-FP-REG a0)
    (addi a0 a0 ,(* 9 WORDSIZE))
    ,@(gen-collect 'a0 si)
    ,@(gen-simple e 'a0)  ;;; clos
    ,@(if tail?
          `((mv a3 fp)
            (mv a2 fp)
            (ld a1 (fp ,WORDSIZE)))
          `((mv a3 fp)
            (mv a2 sp)
            ,@(gen-address-of-local 'a1 cont)))
    (addi sp sp ,(* 16 (- WORDSIZE))) ;;; extra space for shrinking
    (call (PLT s_reify_cont))
    (mv fp ,BOT-FP-REG)
    (addi sp ,BOT-FP-REG ,(* 2 (- WORDSIZE)))
    ,@(gen-address-of-global 'ra 'L_implicit_restore_stack)
    (ld a0 (a0 ,(- CLOS-TAG)))
    (li ,ARGC-REG 2)
    (jr a0)
    ,cont
  ))

(define (gen-L-restore-stack)
  (define loop (gensym* ".Lloop"))
  (define end (gensym* ".Lend"))
  (define good-imp-cont (gensym* ".Lend"))
  `(
    (:global L_explicit_restore_stack)
    (:global L_implicit_restore_stack)
    L_implicit_restore_stack
    (li t1 1)
    (beq ,ARGC-REG t1 ,good-imp-cont)
    (call (PLT s_bad_imp_cont))
    ,good-imp-cont
    (sd (fp ,(* 3 (- WORDSIZE))) a0)
    (addi sp fp ,(* 2 WORDSIZE))
    (li ,ARGC-REG 2)
    L_explicit_restore_stack
    (sd (sp ,(* 1 (- WORDSIZE))) ra)
    (sd (sp ,(* 2 (- WORDSIZE))) fp)
    (addi sp sp ,(* 2 (- WORDSIZE)))
    (mv fp sp)

    ;;; copy-args
    ,@(gen-simple '(label tospace_start) 'a1)
    (slli ,ARGC-REG ,ARGC-REG ,FIXNUM-SHIFT)
    (sd (a1) ,ARGC-REG)
    (addi a2 fp ,(* 2 (- WORDSIZE)))
    (addi a1 a1 ,WORDSIZE)
    (mv a3 ,ARGC-REG)
    ;;; a1 - moving tospace
    ;;; a2 - moving stack pointer
    ;;; a3 - moving counter
    ,loop
    (blt a3 x0 ,end)
    (ld a0 (a2))
    (sd (a1) a0)
    (addi a1 a1 ,WORDSIZE)
    (addi a2 a2 ,(- WORDSIZE))
    (addi a3 a3 ,(- WORDSIZE))
    (j ,loop)
    ,end
    
    ;;; cont, calculating the stack_size
    (ld a1 (fp ,(* 2 (- WORDSIZE))))
    (ld a1 (a1 ,(- (* 1 WORDSIZE) CLOS-TAG)))
    (addi a1 a1 ,(* 5 (- WORDSIZE)))

    (sub a0 ,BOT-FP-REG a1)
    (addi a0 a0 ,(* 2 (- WORDSIZE)))
    (sub a0 a0 ,ARGC-REG)

    (addi sp a0 ,(* 16 (- WORDSIZE)))
    (andi sp sp -16)
    (call (PLT s_apply_cont))
    (ld a1 (,AP-REG))
    (ld ,ARGC-REG (a1 ,(* 0 WORDSIZE)))   ; argc
    (ld sp (a1 ,(* 1 WORDSIZE)))          ; stack pointer
    (ld fp (a1 ,(* 2 WORDSIZE)))          ; frame pointer
    (ld ra (fp ,WORDSIZE))
    (ld a1 (a1 ,(* 4 WORDSIZE)))          ; ret
    (jr a1)
    ))

(define (gen-expr e si)
  (match e
    (,()
     (guard (or (integer? e) (char? e) (boolean? e)))
     (gen-simple e 'a0))
    (,()
     (guard (string? e))
     (gen-str (string->list e) si))
    ((quote ,q)
     (gen-simple e 'a0))
    ((label ,lbl)
     (gen-simple e 'a0))
    ((label* ,lbl)
     (gen-simple e 'a0))
    ((call ,tail? (prim push-constant-table))
     (define tmp1 (gensym* ".Ltmp"))
     (define tmp2 (gensym* ".Ltmp"))
     `(
       ,tmp1
       (auipc a0 (%pcrel_hi .constant_table))
       (addi a0 a0 (%pcrel_lo ,tmp1))
       ,@(gen-address-of-global 'a1 'LBL_TBL)
       (ld a2 (a1))
       (sd (a0) a2)
       (sd (a1) a0)
       ))
    ((call ,tail? (prim %set-label!) (label* ,lbl) ,e)
     `(,@(gen-simple e 'a0)
       (sd (label ,lbl) a0 t0)))
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
        (addi ,ARGC-REG ,ARGC-REG ,(- 1))
        (ld a0 (sp ,(* 5 (- WORDSIZE))))
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
    (sd (sp ,(* 1 (- WORDSIZE))) ra)
    (sd (sp ,(* 2 (- WORDSIZE))) fp)
    (addi sp sp ,(* 2 (- WORDSIZE)))
    (mv fp sp)
    (mv ,BOT-FP-REG fp)
    ,@(gen-address-of-global 't0 'BOT_FP)
    (sd (t0) fp)
    (call (PLT _scm_init))
    ,@(gen-address-of-global AP-REG 'free_ptr)
    ,@(flatmap
      (lambda (ent)
        `((li ,ARGC-REG 1)
          (call (PLT ,ent))))
      entries)
    ;;; restore stack pointer
    (addi sp fp ,(* 2 WORDSIZE))
    ;;; restore frame pointer and return address
    (ld ra (sp ,(* 1 (- WORDSIZE))))
    (ld fp (sp ,(* 2 (- WORDSIZE))))
    (li a0 0)
    (ret)
  ))

(vector gen-unit gen-main)
)) ;;; code-gen

(define gen-unit (vector-ref code-gen 0))
(define gen-main (vector-ref code-gen 1))

(define CC (maybe-getenv "CC" "gcc"))
(define CFLAGS (maybe-getenv "CFLAGS" "-Wall -fno-omit-frame-pointer"))
(define OBJCOPY (maybe-getenv "OBJCOPY" "objcopy"))

(define emit-ir-seq (let ()
  (define (emit op . args)
    (apply format (cons op args))
    (newline op))

  (define (reg? r) (memq r REGISTERS))

  (define (arg->str arg)
    (match arg
      (,()
       (guard (integer? arg))
       (format "~a" arg))
      ((label ,lbl)
       (format "~a" lbl))
      (,()
       (guard (reg? arg))
       (format "~a" arg))
      ((,reg)
       (guard (reg? reg))
       (format "(~a)" reg))
      ((,reg ,off)
       (guard (integer? off))
       (format "~a(~a)" off reg))
      ((,reg (%pcrel_lo ,lbl))
       (format " %pcrel_lo(~a)(~a)" lbl reg))
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
      ((li ,dst ,imm)
       (guard (reg? dst))
       (emit op "li ~a, ~a" dst imm))
      ((mv ,dst ,src)
       (guard (and (reg? dst) (reg? src)))
       (emit op "mv ~a, ~a" dst src))
      ((,opr ,rd ,rs)
       (guard (and (memq opr '(neg seqz)) (reg? rd) (reg? rs)))
       (emit op "~a ~a, ~a" opr rd rs))
      ((addi ,rd ,rs (%pcrel_lo ,lbl))
       (guard (and (reg? rd) (reg? rs) (symbol? lbl)))
       (emit op "addi ~a, ~a, %pcrel_lo(~a)" rd rs lbl))
      ((,opr ,rd ,rs ,imm)
       (guard (and (memq opr '(srai slli addi andi ori)) (reg? rd) (reg? rs) (integer? imm)))
       (emit op "~a ~a, ~a, ~a" opr rd rs imm))
      ((,opr ,rd ,rs1 ,rs2)
       (guard (and (memq opr '(sra sll add sub mul and or div rem)) (reg? rd) (reg? rs1) (reg? rs2)))
       (emit op "~a ~a, ~a, ~a" opr rd rs1 rs2))
      ((mv ,dst ,src)
       (guard (and (reg? dst) (reg? src)))
       (emit op "mv ~a, ~a" dst src))
      ((,branch ,src0 ,src1 ,lbl)
       (guard (and (memq branch '(blt ble beq bne bge bgt)) (reg? src0) (reg? src1)))
       (emit op "~a ~a, ~a, ~a" branch src0 src1 lbl))
      ((j ,lbl)
       (guard (symbol? lbl))
       (emit op "j ~a" lbl))
      ((,ctrl ,rs)
       (guard (and (memq ctrl '(jalr jr)) (reg? rs)))
       (emit op "~a ~a" ctrl rs))
      ((,ctrl (PLT ,lbl))
       (guard (memq ctrl '(call tail)))
       (emit op "~a ~a@plt" ctrl lbl))
      ((sub ,rd ,rs1 ,rs2)
       (guard (and (reg? rd) (reg? rs1) (reg? rs2)))
       (emit op "sub ~a, ~a, ~a" rd rs1 rs2))
      ((auipc ,dst (%got_pcrel_hi ,lbl))
       (guard (and (reg? dst)))
       (emit op "auipc ~a, %got_pcrel_hi(~a)" dst lbl))
      ((auipc ,dst (%pcrel_hi ,lbl))
       (guard (and (reg? dst)))
       (emit op "auipc ~a, %pcrel_hi(~a)" dst lbl))
      ((ld ,rd ,rs (%pcrel_lo ,lbl))
       (guard (and (reg? rd) (reg? rs)))
       (emit op "ld ~a, ~a, %pcrel_lo(~a)" rd rs lbl))
      ((,ldr ,rd ,arg)
       (guard (and (reg? rd) (memq ldr '(ld lw lbu))))
       (emit op "~a ~a, ~a" ldr rd (arg->str arg)))
      ((la ,rd (label ,lbl))
       (guard (and (reg? rd) (symbol? lbl)))
       (emit op "la ~a, ~a" rd lbl))
      ((sd (label ,lbl) ,rs ,rt)
      (guard (and (reg? rs) (reg? rt) (symbol? lbl)))
       (emit op "sd ~a, ~a, ~a" rs lbl rt))
      ((,str ,dst ,rs)
       (guard (and (reg? rs) (memq str '(sd sw sb))))
       (emit op "~a ~a, ~a" str rs (arg->str dst)))
      (,lbl
       (guard (symbol? lbl))
       (emit op "~a: " lbl))
      ((ret) (emit op "ret"))
      (,() (error "emit-ir" "unmatch" instr))))

  (define (emit-ir-seq op instrs)
    (if (pair? instrs)
        (begin
          (emit-ir op (car instrs))
          (emit-ir-seq op (cdr instrs)))))

  emit-ir-seq)) ;;; emit-ir-seq

(include "compiler-tools.scm")
