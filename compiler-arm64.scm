(import
  (only (match) match)
  (only (utils)
    align-to-multiple gensym*
    flatmap zip applicate
    read-sexps-from-path path-filestem path-extension
    make-tempname system* maybe-getenv)
  (only (front) preprocess))

(define ARGS-REG '(x0 x1 x2 x3 x4 x5 x6 x7))
(define INDIRECT-RES-LOC-REG 'r8)
(define TEMP-REG '(x9 x10 x11 x12 x13 x14 x15))
(define ARGC-REG 'x15)
(define IP0 'x16) ;;; intra-procedure-call
(define IP1 'x17) ;;; intra-procedure-call
(define PLATFORM-REG 'x18) ;;; platform register if need; otherwise a temporary register
(define CALLEE-SAVE-REG '(x19 x20 x21 x22 x23 x24 x25 x26 x27 x28))
(define FP-REGS '(x29 fp)) ;;; frame pointer
(define LN-REGS '(x30 lr)) ;;; link register
(define FP-REG 'fp)
(define LN-REG 'lr)
(define SP-REG 'sp)
(define REGISTERS
  (append ARGS-REG (list INDIRECT-RES-LOC-REG) TEMP-REG (list IP0) (list IP1) (list PLATFORM-REG) CALLEE-SAVE-REG FP-REGS LN-REGS (list SP-REG)
    '(w0 w1 w2)))

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

(define AP-REG 'x19)
(define FP-REG 'fp)
(define BOT-FP-REG 'x28)

(define (gen-li dst fx)
  (cond
    ((not (integer? fx))
     (error "gen-li" "expect-only-fixnum" fx))
    ((< fx 0)
     `(,@(gen-li dst (- fx))
       (neg ,dst ,dst)))
    ((< fx (ash 1 16))
     `((movz ,dst ,fx)))
    ((< fx (ash 1 32))
     (let ((lo (bitwise-and fx (- (ash 1 16) 1)))
           (hi (ash fx -16)))
      `((movz ,dst ,lo)
        (movk ,dst ,hi (lsl 16)))))
    (else
     `((ldr ,dst ,fx)))))

(define (gen-simple e dst)
  (match e
    (,()
     (guard (or (integer? e) (char? e) (boolean? e)))
     (gen-li dst (immediate-repr e)))
    ((quote ,q)
     (gen-li dst (immediate-repr q)))
    ((label ,lbl) ;;; global-label
     `((adrp ,dst (:got ,lbl))
       (ldr ,dst (,dst (:got_lo12 ,lbl)))
       (ldr ,dst (,dst))))
    ((label* ,lbl) ;;; local-label
     `((adrp ,dst ,lbl)
       (ldr ,dst (,dst (:lo12 ,lbl)))))
    ((call ,tail? (prim local-ref) ,i)
     `((add ,dst fp ,(* (- WORDSIZE) (+ i 2)))
       (ldr ,dst (,dst))))
    ((call ,tail? (prim closure-ref) ,clos -1)
     `((ldr ,dst (fp ,(* (- WORDSIZE) 2)))))
    ((call ,tail? (prim closure-ref) ,clos ,i)
     (guard (>= i 0))
     `((ldr ,dst (fp ,(* (- WORDSIZE) 2)))
       (add ,dst ,dst ,(- (* (+ i 2) WORDSIZE) CLOS-TAG))
       (ldr ,dst (,dst))))
    (,() (error "gen-simple" "unmatch" e))))

(define (gen-align align off)
  `(
    (add ,off ,off ,(- align 1))
    (and ,off ,off ,(- align))
    ))

(define (gen-eq dst src val)
  `(
    (cmp ,src ,val)
    (cset ,dst eq)
    (lsl ,dst ,dst ,IMM-SHIFT)
    (orr ,dst ,dst ,BOOL-TAG)
    ))

(define (gen-obj-test e mask tag)
  (define csq (gensym* "conseq"))
  (define alt (gensym* "altern"))
  (define end (gensym* "end"))
  `(
      ,@(gen-simple e 'x0)
      (mov x1 x0)
      (and x1 x1 ,OBJ-MASK)
      (cmp x1 ,OBJ-TAG)
      (bne ,alt)
      ,csq
      (ldr x0 (x0 ,(- OBJ-TAG)))
      (and x0 x0 ,mask)
      (cmp x0 ,tag)
      (cset x0 eq)
      (lsl x0 x0 ,IMM-SHIFT)
      (orr x0 x0 ,BOOL-TAG)
      (b ,end)
      ,alt
      ,@(gen-li 'x0 FALSE-IMM)
      ,end
    ))

(define (gen-alloc dst sz)
  (define t0 (list-ref TEMP-REG 0))
  `(
    (ldr ,t0 (,AP-REG))
    (mov ,dst ,t0)
    (add ,t0 ,t0 ,sz)
    (str (,AP-REG) ,t0)
  ))

;;; TODO: fix the size here
(define (gen-str vs si)
  (define sz (align-to-multiple 8 (+ WORDSIZE (* (length vs) 4))))
  (define t0 (list-ref TEMP-REG 0))
  `(
    ,@(gen-collect (ash sz (- FIXNUM-SHIFT)) si)
    (ldr x0 (,AP-REG))
    (add ,t0 x0 ,sz)
    (str (,AP-REG) ,t0)
    ,@(gen-li t0 (bitwise-ior STR-TAG (immediate-repr (length vs))))
    (str (x0) ,t0)
    ,@(flatmap
        (lambda (i v)
          `(
            ,@(gen-simple v 'x1)
            (add x2 x0 ,(+ (* i 4) WORDSIZE))
            (str (x2) w1)
          ))
        (iota (length vs))
        vs)
    (orr x0 x0 ,OBJ-MASK)
  ))

(define (gen-vec vs si)
  (define vlen (length vs))
  (define sz (align-to-multiple 8 (* (+ vlen 1) WORDSIZE)))
  (define t0 (list-ref TEMP-REG 0))
  `(
    ,@(gen-collect sz si)
    (ldr x0 (,AP-REG))
    (add ,t0 x0 ,sz)
    (str (,AP-REG) ,t0)
    ,@(gen-li t0 (bitwise-ior VEC-TAG (immediate-repr (length vs))))
    (str (x0) ,t0)
    ,@(flatmap
        (lambda (i v)
          `(
            ,@(gen-simple v 'x1)
            (add x2 x0 ,(* (+ i 1) WORDSIZE))
            (str (x2) x1)
          ))
        (iota vlen)
        vs)
    (orr x0 x0 ,OBJ-MASK)
  ))

(define (gen-foreign-call fn args si)
  (define argc (length args))
  (if (not (< argc 6))
      (error "foreign-call" "support-only-at-most-6-args"))
  `(
    ,@(flatmap
        (applicate (lambda (arg dst) (gen-simple arg dst)))
        (zip args ARGS-REG))
    (bl ,fn)
    ))

(define cmp-suffix
  '(
    (< blt lt)
    (<= ble le)
    (eq? beq eq)
    (= beq eq)
    (>= bge ge)
    (> bg gt)
  ))

(define (gen-cmp cmp e1 e2 si)
  `(
    ,@(gen-simple e2 'x1)
    ,@(gen-simple e1 'x0)
    (cmp x0 x1)
    (cset x0 ,(caddr (assq cmp cmp-suffix)))
    (lsl x0 x0 ,IMM-SHIFT)
    (orr x0 x0 ,BOOL-TAG)
  ))

(define (gen-prim e si)
  (match e
    ((local-set! ,i ,e1)
     `(
      ,@(gen-expr e1 si)
      (add x1 fp ,(* (- WORDSIZE) (+ i 2)))
      (str (x1) x0)
      ))
    ((eq? ,e1 ,e2)
     (gen-cmp 'eq? e1 e2 si))
    ((boolean? ,e1)
     `(
      ,@(gen-simple e1 'x0)
      (and x0 x0 ,IMM-MASK)
      ,@(gen-eq 'x0 'x0 BOOL-TAG)
      ))
    ((not ,e1)
     `(
      ,@(gen-simple e1 'x0)
      ,@(gen-eq 'x0 'x0 FALSE-IMM)
     ))
    ((eof-object? ,e1)
     `(
      ,@(gen-simple e1 'x0)
      (and x0 x0 ,IMM-MASK)
      ,@(gen-eq 'x0 'x0 EOF-TAG)
      ))
    ((null? ,e1)
     `(
      ,@(gen-simple e1 'x0)
      (and x0 x0 ,IMM-MASK)
      ,@(gen-eq 'x0 'x0 NIL-TAG)
      ))
    ((char? ,e1)
     `(
      ,@(gen-simple e1 'x0)
      (and x0 x0 ,IMM-MASK)
      ,@(gen-eq 'x0 'x0 CHAR-TAG)
      ))
    ((char->integer ,e1)
     `(
      ,@(gen-simple e1 'x0)
      (asr x0 x0 ,(- IMM-SHIFT FIXNUM-SHIFT))
      ))
    ((integer->char ,e1)
     `(
      ,@(gen-simple e1 'x0)
      (lsl x0 x0 ,(- IMM-SHIFT FIXNUM-SHIFT))
      (orr x0 x0 ,CHAR-TAG)
      ))
    ((integer? ,e1)
     `(
      ,@(gen-simple e1 'x0)
      (and x0 x0 ,FIXNUM-MASK)
      ,@(gen-eq 'x0 'x0 FIXNUM-TAG)
      ))
    ((,cmp ,e1 ,e2)
     (guard (assq cmp cmp-suffix))
     (gen-cmp cmp e1 e2 si))
    ((- ,e1 ,e2)
     `(
      ,@(gen-simple e2 'x1)
      ,@(gen-simple e1 'x0)
      (sub x0 x0 x1)
      ))
    ((+ ,e1 ,e2)
     `(
      ,@(gen-simple e2 'x1)
      ,@(gen-simple e1 'x0)
      (add x0 x0 x1)
      ))
    ((* ,e1 ,e2)
     `(
      ,@(gen-simple e2 'x1)
      ,@(gen-simple e1 'x0)
      (asr x0 x0 ,FIXNUM-SHIFT)
      (mul x0 x0 x1)
      ))
    ((div ,dividend ,divisor)
     #;((div (* t x) (* t y))
        => (div x y)
        => (tag (div x y)))
     `(
        ,@(gen-simple divisor 'x1)
        ,@(gen-simple dividend 'x0)
        (sdiv x0 x0 x1)
        (lsl x0 x0 ,FIXNUM-SHIFT)
      ))
    ((mod ,dividend ,divisor)
     ;;; TODO: don't know why this works?
     `(
        ,@(gen-simple divisor 'x1)
        ,@(gen-simple dividend 'x0)
        (sdiv x2 x0 x1)
        (msub x0 x2 x1 x0)
     ))
    ((abs ,e1)
     `(
      ,@(gen-simple e1 'x0)
      (cmp x0 0)
      (cneg x0 x0 lt)
      ))
    ((max ,e1 ,e2)
     `(
       ,@(gen-simple e2 'x1)
       ,@(gen-simple e1 'x0)
       (cmp x0 x1)
       (csel x0 x0 x1 ge)
       ))
    ((ash ,e1 ,e2)
     (define altern (gensym* "altern"))
     (define end (gensym* "end"))
     `(
      ,@(gen-simple e2 'x1)
      ,@(gen-simple e1 'x0)
      (asr x1 x1 ,FIXNUM-SHIFT)
      (cmp x1 0)
      (blt ,altern)
      ; e2 >= 0 - left shift
      (lsl x0 x0 x1)
      (b ,end)
      ,altern
      ; e2 < 0 - right shift
      (neg x1 x1)
      (asr x0 x0 ,FIXNUM-SHIFT)
      (asr x0 x0 x1)
      (lsl x0 x0 ,FIXNUM-SHIFT)
      ,end
     ))
    ((bitwise-and ,e1 ,e2)
     `(
      ,@(gen-simple e2 'x1)
      ,@(gen-simple e1 'x0)
      (and x0 x0 x1)
     ))
    ((bitwise-ior ,e1 ,e2)
     `(
      ,@(gen-simple e2 'x1)
      ,@(gen-simple e1 'x0)
      (orr x0 x0 x1)
     ))
    ((pair? ,e1)
     `(
      ,@(gen-simple e1 'x0)
      (and x0 x0 ,OBJ-MASK)
      ,@(gen-eq 'x0 'x0 PAIR-TAG)
      ))
    ((cons ,e1 ,e2)
     `(
      ,@(gen-collect 2 si)
      ,@(gen-simple e2 'x2)
      ,@(gen-simple e1 'x1)
      (ldr x0 (,AP-REG))
      (add x3 x0 ,(* 2 WORDSIZE))
      (str (,AP-REG) x3)
      (str (x0) x1)
      (str (x0 ,WORDSIZE) x2)
      (orr x0 x0 ,PAIR-TAG)
      ))
    ((car ,e)
     `(
      ,@(gen-simple e 'x0)
      (ldr x0 (x0 ,(- PAIR-TAG)))
      ))
    ((cdr ,e)
     `(
      ,@(gen-simple e 'x0)
      (ldr x0 (x0 ,(- WORDSIZE PAIR-TAG)))
      ))
    ((set-car! ,e1 ,e2)
     `(
      ,@(gen-simple e2 'x1)
      ,@(gen-simple e1 'x0)
      (str (x0 ,(- PAIR-TAG)) x1)
      ))
    ((set-cdr! ,e1 ,e2)
     `(
      ,@(gen-simple e2 'x1)
      ,@(gen-simple e1 'x0)
      (str (x0 ,(- WORDSIZE PAIR-TAG)) x1)
      ))
    ((string? ,e1)
     (gen-obj-test e1 STR-MASK STR-TAG))
    ((string . ,es)
     (gen-str es si))
    ((string-length ,e1)
     `(
      ,@(gen-simple e1 'x0)
      (ldr x0 (x0 ,(- OBJ-TAG)))
      (sub x0 x0 ,STR-TAG)
     ))
    ((string-ref ,e1 ,e2)
     `(
      ,@(gen-simple e1 'x0)
      ,@(gen-simple e2 'x1)
      (asr x1 x1 1)
      (add x1 x1 ,(- WORDSIZE OBJ-TAG))
      (add x1 x0 x1)
      (ldr w0 (x1))
     ))
    ((string-set! ,e1 ,e2 ,e3)
     `(
      ,@(gen-simple e1 'x0)
      ,@(gen-simple e2 'x1)
      ,@(gen-simple e3 'x2)
      (asr x1 x1 1)
      (add x1 x1 ,(- WORDSIZE OBJ-TAG))
      (add x1 x0 x1)
      (str (x1) w2)
      ,@(gen-li 'x0 VOID-TAG)
     ))
    ((make-string ,e1 ,e2)
     (define lp (gensym* "loop"))
     (define end (gensym* "end"))
     (define t0 (list-ref TEMP-REG 0))
     (define t1 (list-ref TEMP-REG 1))
     `(,@(gen-simple e1 'x0)
       (asr x0 x0 1)
       (add x0 x0 ,WORDSIZE)
       ,@(gen-align 8 'x0)
       ,@(gen-collect 'x0 si)
       ,@(gen-foreign-call 's_make_string (list e1 e2) si)))
    ((vector . ,es)
     (gen-vec es si))
    ((vector? ,e1)
     (gen-obj-test e1 VEC-MASK VEC-TAG))
    ((vector-length ,e1)
     `(
      ,@(gen-simple e1 'x0)
      (ldr x0 (x0 ,(- OBJ-TAG)))
      (sub x0 x0 ,VEC-TAG)
     ))
    ((vector-ref ,e1 ,e2)
     `(
      ,@(gen-simple e1 'x0)
      ,@(gen-simple e2 'x1)
      (add x0 x0 x1)
      (add x0 x0 ,(- WORDSIZE OBJ-TAG))
      (ldr x0 (x0))
     ))
    ((vector-set! ,e1 ,e2 ,e3)
     `(
      ,@(gen-simple e1 'x0)
      ,@(gen-simple e2 'x1)
      ,@(gen-simple e3 'x2)
      (add x0 x0 x1)
      (add x0 x0 ,(- WORDSIZE OBJ-TAG))
      (str (x0) x2)
      ,@(gen-li 'x0 VOID-TAG)
     ))
    ((make-vector ,e1 ,e2)
     (define lp (gensym* "loop"))
     (define end (gensym* "end"))
     `(
      ,@(gen-simple e1 'x0)
      (add x0 x0 ,WORDSIZE)
      ,@(gen-align 8 'x0)
      ,@(gen-collect 'x0 si)
      ,@(gen-foreign-call 's_make_vector (list e1 e2) si)
     ))
    ((bytevector? ,e1)
     (gen-obj-test e1 BYTEVEC-MASK BYTEVEC-TAG))
    ((make-bytevector ,e1 . ,maybe-e2)
     (define e2 (if (pair? maybe-e2) (car maybe-e2) #f))
     (define lp (gensym* "loop"))
     (define end (gensym* "end"))
     `(
      ,@(gen-simple e1 'x0)
      (asr x0 x0 ,FIXNUM-SHIFT)
      (add x0 x0 ,WORDSIZE)
      ,@(gen-align 8 'x0)
      ,@(gen-collect 'x0 si)
      ,@(gen-foreign-call 's_make_bytevector (list e1 e2) si)
      ))
    ((bytevector-length ,e1)
     `(
      ,@(gen-simple e1 'x0)
      (ldr x0 (x0 ,(- OBJ-TAG)))
      (sub x0 x0 ,BYTEVEC-TAG)
     ))
    ((bytevector-u8-ref ,e1 ,e2)
     `(
      ,@(gen-simple e1 'x0)
      ,@(gen-simple e2 'x1)
      (asr x1 x1 ,FIXNUM-SHIFT)
      (add x1 x1 ,(- WORDSIZE OBJ-TAG))
      (add x0 x0 x1)
      (ldrb w0 (x0))
      (lsl x0 x0 ,FIXNUM-SHIFT)
     ))
    ((bytevector-u8-set! ,e1 ,e2 ,e3)
     `(
      ,@(gen-simple e1 'x0)
      ,@(gen-simple e2 'x1)
      ,@(gen-simple e3 'x2)
      (asr x1 x1 ,FIXNUM-SHIFT)
      (add x1 x1 ,(- WORDSIZE OBJ-TAG))
      (add x0 x0 x1)
      (asr x2 x2 ,FIXNUM-SHIFT)
      (strb (x0) w2)
     ))
    ((symbol? ,e1)
     `(
      ,@(gen-simple e1 'x0)
      (and x0 x0 ,OBJ-MASK)
      ,@(gen-eq 'x0 'x0 SYM-TAG)
      ))
    ((%make-symbol ,hash ,name)
     `(
      ,@(gen-collect 4 si)
      (ldr x0 (,AP-REG))
      (add x1 x0 ,(* 4 WORDSIZE))
      (str (,AP-REG) x1)
      ,@(gen-simple name 'x2)
      ,@(gen-simple hash 'x1)
      ,@(gen-li 'x3 UNBOUND-TAG)
      (str (x0 ,(* 2 WORDSIZE)) x2)
      (str (x0 ,WORDSIZE) x1)
      (str (x0) x3)
      (add x0 x0 ,SYM-TAG)
     ))
    ((%symbol-name ,e1)
     `(
      ,@(gen-simple e1 'x0)
      (ldr x0 (x0 ,(- (* 2 WORDSIZE) SYM-TAG)))
      ))
    ((%symbol-hash ,e1)
     `(
      ,@(gen-simple e1 'x0)
      (ldr x0 (x0 ,(- (* 1 WORDSIZE) SYM-TAG)))
      ))
    ((%symbol-value ,e1)
     (define end (gensym* "end"))
     `(
      ,@(gen-simple e1 'x0)
      (ldr x0 (x0 ,(- SYM-TAG)))
      (cmp x0 ,UNBOUND-TAG)
      (bne ,end)
      (bl s_unbound_global)
      ,end
      ))
    ((%symbol-value-set! ,e1 ,e2)
     `(
      ,@(gen-simple e2 'x1)
      ,@(gen-simple e1 'x0)
      (str (x0 ,(- SYM-TAG)) x1)
      ))
    ((procedure? ,e1)
     `(
      ,@(gen-simple e1 'x0)
      (and x0 x0 ,PTR-MASK)
      ,@(gen-eq 'x0 'x0 CLOS-TAG)
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
     (gen-simple `(call #f (prim closure-ref) ,clos ,i) 'x0))
    ((closure-set! ,e1 ,i ,e2)
     `(
        ,@(gen-simple e1 'x0)
        ,@(gen-simple e2 'x1)
        (add x0 x0 ,(- (* (+ i 2) WORDSIZE) CLOS-TAG))
        (str (x0) x1)
       ))
    ((collect)
     (gen-collect #f si))
    ((collect ,e1)
     (append
      (gen-simple e1 'x0)
      (gen-collect 'x0 si)))
    ((local-ref ,i)
     (gen-simple `(call #f (prim local-ref) ,i) 'x0))
    (,() (error "gen-prim" "unmatch" e))))

(define (gen-if pred conseq altern si)
  (define csq (gensym* "csq"))
  (define alt (gensym* "alt"))
  (define end (gensym* "end"))
  `(
    ,@(gen-simple pred 'x0)
    (cmp x0 ,FALSE-IMM)
    (beq ,alt)
    ,csq
    ,@(gen-expr conseq si)
    (b ,end)
    ,alt
    ,@(gen-expr altern si)
    ,end
  ))

(define (gen-collect sz si)
  `(
    (mov x2 fp)
    (mov x1 sp)
    ,@(if (eq? sz 'x0)
          `()
          (gen-simple sz 'x0))
    (bl s_collect)
  ))

(define (gen-procedure fn params variadic? body)
  (define argc (length params))
  (define csq (gensym* "conseq"))
  (define t0 (list-ref TEMP-REG 0))
  (define t1 (list-ref TEMP-REG 1))
  (match body
    ((let ((_ (call #f (prim alloca) ,n*))) ,e)
     ;;; 16 bytes == 2 * 8
     (define n (div (align-to-multiple 16 (* (+ n* 1) WORDSIZE)) WORDSIZE))
     `(
        ,fn
        (stp (sp ,(* 2 (- WORDSIZE))) fp ,LN-REG)
        (sub sp sp ,(* 2 WORDSIZE))
        (mov fp sp)
        ,@(if variadic?
              (gen-construct-vararg argc)
              `((cmp ,ARGC-REG ,argc)
                (beq ,csq)
                (mov x1 ,ARGC-REG)
                (mov x0 ,argc)
                (bl s_bad_monovariadic_call)
                ,csq))
        (mov ,t0 0)
        ,@(flatmap
            (lambda (i)
              `((sub ,t1 sp ,(* (+ i 2) WORDSIZE))
                (str (,t1) ,t0)))
            (filter (lambda (i) (>= i argc)) (iota n)))
        (sub sp sp ,(* n WORDSIZE))
        ,@(gen-expr e #f)
        ;;; restore stack pointer
        (add sp fp ,(* 2 WORDSIZE))
        ;;; restore frame pointer and link register
        (ldp fp ,LN-REG (sp ,(* 2 (- WORDSIZE))))
        (mov ,ARGC-REG 1)
        (ret)
      ))
    (,() (error "gen-procedure" "unknown" body))))

(define (gen-L-apply)
  (define loop (gensym* "unstack"))
  (define loop-end (gensym* "unstack"))
  (define end (gensym* "end"))
  `((:global L_apply)
    L_apply
    (stp (sp ,(* 2 (- WORDSIZE))) fp ,LN-REG)
    (sub sp sp ,(* 2 WORDSIZE))
    (mov fp sp)

    (add x1 ,ARGC-REG 1)
    (lsl x1 x1 ,FIXNUM-SHIFT)
    (sub x1 sp x1)
    (ldr x0 (x1))
    ;;; x1 ~ moving stack pointer
    ;;; x0 ~ list to be unfolded
    ,loop
    (and x2 x0 ,OBJ-MASK)
    (cmp x2 ,PAIR-TAG)
    (bne ,loop-end)
    (ldr x2 (x0 ,(- PAIR-TAG)))           ; (car *)
    (str (x1) x2)
    (ldr x0 (x0 ,(- WORDSIZE PAIR-TAG)))  ; (cdr *)
    (sub x1 x1 ,WORDSIZE)
    (b ,loop)
    ,loop-end
    (cmp x0 ,NIL-TAG)
    (beq ,end)
    (bl s_bad_apply_call)
    ,end
    (sub ,ARGC-REG sp ,(* 2 WORDSIZE))
    (sub ,ARGC-REG ,ARGC-REG x1)
    (asr ,ARGC-REG ,ARGC-REG ,FIXNUM-SHIFT)
    (ldr x0 (sp ,(- (* 2 WORDSIZE))))
    (ldr x0 (x0 ,(- CLOS-TAG)))
    ;;; restore stack pointer
    (add sp fp ,(* 2 WORDSIZE))
    ;;; restore frame pointer and link register
    (ldp fp ,LN-REG (sp ,(* 2 (- WORDSIZE))))
    (br x0)))

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
                `((cmp ,ARGC-REG ,(- argc 1))
                  (blt ,nxt))
                `((cmp ,ARGC-REG ,argc)
                  (bne ,nxt)))
          (ldr x0 (fp ,(* (- WORDSIZE) 2)))
          (add x0 x0 ,(- (* WORDSIZE (+ i 2)) CLOS-TAG))
          (ldr x0 (x0))
          (str (fp ,(* (- WORDSIZE) 2)) x0)
          ;;; restore stack pointer
          (add sp fp ,(* 2 WORDSIZE))
          ;;; restore frame pointer and link register
          (ldp fp ,LN-REG (sp ,(* 2 (- WORDSIZE))))
          (b ,fn)
          ,nxt
       ))
      (,() (error "gen-clause" "unmatch" clause))
      ))
  `(
    ,fn
    (stp (sp ,(* 2 (- WORDSIZE))) fp ,LN-REG)
    (sub sp sp ,(* 2 WORDSIZE))
    (mov fp sp)
    ,@(flatmap gen-clause clauses)
    (bl s_bad_polyvariadic_call)
  ))

(define (gen-closure fn fvs)
  (define fvs-cnt (length fvs))
  (define sz (align-to-multiple 8 (* (+ fvs-cnt 2) WORDSIZE)))
  (define t0 (list-ref TEMP-REG 0))
  `(
    (ldr ,t0 (,AP-REG))
    (mov x0 ,t0)
    (add ,t0 ,t0 ,sz)
    (str (,AP-REG) ,t0)
    ,@(flatmap
        (lambda (i fv)
          `(,@(gen-simple fv 'x1)
            (add x2 x0 ,(* (+ i 2) WORDSIZE))
            (str (x2) x1)
            ))
        (iota (length fvs)) fvs)

    ,@(gen-li t0 (immediate-repr fvs-cnt))
    (str (x0 ,WORDSIZE) ,t0)

    (adrp x1 ,fn)
    (add x1 x1 (:lo12 ,fn))
    (str (x0) x1)

    (add x0 x0 ,CLOS-TAG)
  ))

(define (gen-tail-call apply? fn es si)
  (define argc (length (cons fn es)))
  (define argi (iota argc))
  ;;; TODO: not really right here
  (define offsets (map (lambda (i) (* (- WORDSIZE) (+ i 2))) argi))
  (define dests (map (lambda (i) (* (- WORDSIZE) (+ i 2))) argi))
  (define t0 (list-ref TEMP-REG 0))
  (define t1 (list-ref TEMP-REG 1))
  `(
    ,@(flatmap
        (lambda (off e)
          `(
            ,@(gen-simple e 'x0)
            (add ,t0 sp ,off)
            (str (,t0) x0)
          ))
        offsets
        (cons fn es))
    ,@(flatmap
        (lambda (off dst)
          `((add ,t0 sp ,off)
            (ldr ,t0 (,t0))
            (add ,t1 fp ,dst)
            (str (,t1) ,t0)))
        offsets
        dests)
    (ldr x0 (fp ,(* (- WORDSIZE) 2)))
    ;;; restore stack pointer
    (add sp fp ,(* 2 WORDSIZE))
    ;;; restore frame pointer and link register
    (ldp fp ,LN-REG (sp ,(* 2 (- WORDSIZE))))
    (mov ,ARGC-REG ,argc)
    ,@(if apply?
          `((b L_apply))
          `((ldr x0 (x0 ,(- CLOS-TAG)))
            (br x0)))
  ))

(define (gen-call apply? fn es si)
  (define argc (length (cons fn es)))
  `(
    ,@(flatmap
        (lambda (i e)
          `(
            ,@(gen-simple e 'x0)
            (add x1 sp ,(* (+ i 4) (- WORDSIZE)))
            (str (x1) x0)
          ))
        (iota argc)
        (cons fn es))
    (mov ,ARGC-REG ,argc)
    ,@(if apply?
          `((bl L_apply))
          `(,@(gen-simple fn 'x0)
            (ldr x0 (x0 ,(- CLOS-TAG)))
            (blr x0)))
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
  (define t0 (list-ref TEMP-REG 0))
  `(
    (mov ,t0 ,ARGC-REG)
    (add ,t0 ,t0 1)
    (lsl ,t0 ,t0 ,FIXNUM-SHIFT)
    (sub sp sp ,t0)
    (mov ,t0 ,ARGC-REG)
    (mov x3 fp)    ; fp
    (mov x2 sp)    ; sp
    (mov x1 ,t0)        ; argc
    (mov x0 ,argc)      ; expected argc
    (sub sp sp ,WORDSIZE)
    (mov ,t0 sp)
    (and ,t0 ,t0 ,-16)
    (mov sp ,t0)
    (bl construct_vararg)
    (mov sp fp)
    (add ,t0 sp ,(* (+ 1 argc) (- WORDSIZE)))
    (str (,t0) x0)
  ))

(define (gen-values es si)
  (define argc (length es))
  (define argi (iota argc))
  (define offsets
    (map (lambda (i) (* (+ i 3) (- WORDSIZE))) argi))
  (define dests
    (map (lambda (i) (* (+ i 3) (- WORDSIZE))) argi))
  (define t0 (list-ref TEMP-REG 0))
  (define t1 (list-ref TEMP-REG 1))
  `(
    ,@(flatmap
        (lambda (off e)
          `(,@(gen-simple e 'x0)
            (add ,t0 sp ,off)
            (str (,t0) x0)))
        offsets es)
    ,@(flatmap
        (lambda (off dst)
          `((add ,t0 sp ,off)
            (add ,t1 fp ,dst)
            (ldr ,t0 (,t0))
            (str (,t1) ,t0)))
        offsets dests)
    ,@(if (pair? es) `((ldr x0 (fp ,(car dests)))) '())
    (mov ,ARGC-REG ,argc)
    ;;; restore stack pointer
    (add sp fp ,(* 2 WORDSIZE))
    ;;; restore frame pointer and link register
    (ldp fp ,LN-REG (sp ,(* 2 (- WORDSIZE))))
    (ret)
  ))

(define (gen-call-with-values tail? e1 e2 si)
  `(
    ,@(gen-simple e1 'x0)
    (str (sp ,(* 4 (- WORDSIZE))) x0)
    (mov ,ARGC-REG 1)
    (ldr x0 (x0 ,(- CLOS-TAG)))
    (blr x0)
    (str (sp ,(* 5 (- WORDSIZE))) x0)
    ,@(gen-simple e2 'x0)
    (str (sp ,(* 4 (- WORDSIZE))) x0)
    (add ,ARGC-REG ,ARGC-REG 1)
    ,@(if tail?
          `(
            (mov x2 fp)      ;;; fp
            (add x1 ,ARGC-REG 3)
            (lsl x1 x1 ,FIXNUM-SHIFT)
            (sub x1 sp x1)   ;;; sp
            (mov x0 ,ARGC-REG)    ;;; argc
            (sub x3 x1 ,WORDSIZE)
            (and x3 x3 -16)
            (mov sp x3)
            (bl shift_values)
            (mov ,ARGC-REG x0)
            (ldr x0 (fp ,(* 2 (- WORDSIZE))))
            (add sp fp ,(* 2 WORDSIZE))
            ;;; restore frame pointer and link register
            (ldp fp ,LN-REG (sp ,(* 2 (- WORDSIZE))))
            (ldr x0 (x0 ,(- CLOS-TAG)))
            (br x0)
          )
          `(
            (ldr x0 (x0 ,(- CLOS-TAG)))
            (blr x0)
          ))))

(define (gen-call/cc tail? e si)
  (define cont (gensym* "cont"))
  `(
    (mov x0 sp)
    (sub x0 ,BOT-FP-REG x0)
    (add x0 x0 ,(* 9 WORDSIZE))
    ,@(gen-collect 'x0 si)
    ,@(gen-simple e 'x0)  ;;; clos
    ,@(if tail?
          `((mov x3 fp)
            (mov x2 fp)
            (ldr x1 (fp ,WORDSIZE)))
          `((mov x3 fp)
            (mov x2 sp)
            (adr x1 ,cont)))
    (sub sp sp ,(* 16 WORDSIZE)) ;;; extra space for shrinking
    (bl s_reify_cont)
    (mov fp ,BOT-FP-REG)
    (sub sp ,BOT-FP-REG ,(* 2 WORDSIZE))
    (adrp ,LN-REG (:got L_implicit_restore_stack))
    (ldr ,LN-REG (,LN-REG (:got_lo12 L_implicit_restore_stack)))
    (ldr x0 (x0 ,(- CLOS-TAG)))
    (mov ,ARGC-REG 2)
    (br x0)
    ,cont
  ))

(define (gen-L-restore-stack)
  (define loop (gensym* "loop"))
  (define end (gensym* "end"))
  (define good-imp-cont (gensym* "end"))
  `(
    (:global L_explicit_restore_stack)
    (:global L_implicit_restore_stack)
    L_implicit_restore_stack
    (cmp ,ARGC-REG 1)
    (beq ,good-imp-cont)
    (bl s_bad_imp_cont)
    ,good-imp-cont
    (str (fp ,(* 3 (- WORDSIZE))) x0)
    (add sp fp ,(* 2 WORDSIZE))
    (mov ,ARGC-REG 2)
    L_explicit_restore_stack
    (stp (sp ,(* 2 (- WORDSIZE))) fp ,LN-REG)
    (sub sp sp ,(* 2 WORDSIZE))
    (mov fp sp)

    ;;; copy-args
    ,@(gen-simple '(label tospace_start) 'x1)
    (lsl ,ARGC-REG ,ARGC-REG ,FIXNUM-SHIFT)
    (str (x1) ,ARGC-REG)
    (sub x2 fp ,(* 2 WORDSIZE))
    (add x1 x1 ,WORDSIZE)
    (mov x3 ,ARGC-REG)
    ;;; x1 - moving tospace
    ;;; x2 - moving stack pointer
    ;;; x3 - moving counter
    ,loop
    (cmp x3 0)
    (blt ,end)
    (ldr x0 (x2))
    (str (x1) x0)
    (add x1 x1 ,WORDSIZE)
    (sub x2 x2 ,WORDSIZE)
    (sub x3 x3 ,WORDSIZE)
    (b ,loop)
    ,end
    ;;; cont, calculating the stack_size
    (ldr x1 (fp ,(* 2 (- WORDSIZE))))
    (ldr x1 (x1 ,(- (* 1 WORDSIZE) CLOS-TAG)))
    (sub x1 x1 ,(* 5 WORDSIZE))

    (sub x0 ,BOT-FP-REG x1)
    (sub x0 x0 ,(* 2 WORDSIZE))
    (sub x0 x0 ,ARGC-REG)

    (sub x1 x0 ,(* 16 WORDSIZE))
    (and x1 x1 -16)
    (mov sp x1)
    (bl s_apply_cont)
    (ldr x1 (,AP-REG))
    (ldr ,ARGC-REG (x1 ,(* 0 WORDSIZE)))  ; argc
    (ldr x2 (x1 ,(* 1 WORDSIZE)))         ; stack pointer
    (mov sp x2)
    (ldr fp (x1 ,(* 2 WORDSIZE)))    ; frame pointer
    (ldr ,LN-REG (fp ,WORDSIZE))
    (ldr x1 (x1 ,(* 4 WORDSIZE)))    ; ret
    (br x1)
    ))

(define (gen-expr e si)
  (match e
    (,()
     (guard (or (integer? e) (char? e) (boolean? e)))
     (gen-simple e 'x0))
    (,()
     (guard (string? e))
     (gen-str (string->list e) si))
    ((quote ,q)
     (gen-simple e 'x0))
    ((label ,lbl)
     (gen-simple e 'x0))
    ((label* ,lbl)
     (gen-simple e 'x0))
    ((call ,tail? (prim push-constant-table))
     `(
       (adrp x0 .constant_table)
       (add x0 x0 (:lo12 .constant_table))
       
       (adrp x1 (:got LBL_TBL))
       (ldr x1 (x1 (:got_lo12 LBL_TBL)))
       
       (ldr x2 (x1))
       (str (x0) x2)
       (str (x1) x0)
       ))
    ((call ,tail? (prim %set-label!) (label* ,lbl) ,e)
     `(,@(gen-simple e 'x0)
       (adrp x1 ,lbl)
       (add x1 x1 (:lo12 ,lbl))
       (str (x1) x0)))
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
        (sub ,ARGC-REG ,ARGC-REG 1)
        (ldr x0 (sp ,(* 5 (- WORDSIZE))))
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
  (define t0 (list-ref TEMP-REG 0))
  `(
    (:global main)
    ,@(gen-L-apply)
    ,@(gen-L-restore-stack)
    main
    (mov ,t0 0)
    (stp (sp ,(* 2 (- WORDSIZE))) fp ,LN-REG)
    (sub sp sp ,(* 2 WORDSIZE))
    (mov fp sp)
    (mov ,BOT-FP-REG fp)
    (adrp ,t0 (:got BOT_FP))
    (ldr ,t0 (,t0 (:got_lo12 BOT_FP)))
    (str (,t0) fp)
    (bl _scm_init)
    (adrp ,AP-REG (:got free_ptr))
    (ldr ,AP-REG (,AP-REG (:got_lo12 free_ptr)))
    ,@(flatmap
      (lambda (ent)
        `((mov ,ARGC-REG 1)
          (bl ,ent)))
      entries)
    ;;; (stp (fp -2) ebx 0) ; to be compatible with i686
    (stp (fp ,(* 2 (- WORDSIZE))) ,t0 ,t0)
    (mov x0 0)
    (add sp sp ,(* 2 WORDSIZE))
    (ldp fp ,LN-REG (sp ,(* 2 (- WORDSIZE))))
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
       (format "~a" arg))
      (,()
       (guard (reg? arg))
       (format "~a" arg))
      ((,reg)
       (guard (reg? reg))
       (format "[~a]" reg))
      ((,reg ,off)
       (guard (integer? off))
       (format "[~a, ~a]" reg off))
      ((:lo12 ,lbl)
       (guard (symbol? lbl))
       (format ":lo12:~a" lbl))
      (,() (error "arg->str" "unmatch" arg))))
  
  (define (ldr-src->str arg)
    (match arg
      (,()
       (guard (integer? arg))
       (format "=~a" arg))
      ((,reg)
       (guard (reg? reg))
       (format "[~a]" reg))
      ((,reg ,off)
       (guard (and (reg? reg) (integer? off)))
       (format "[~a, ~a]" reg off))
      ((,reg (:lo12 ,lbl))
       (guard (and (reg? reg) (symbol? lbl)))
       (format "[~a, :lo12:~a]" reg lbl))
      ((,reg (:got_lo12 ,lbl))
       (guard (and (reg? reg) (symbol? lbl)))
       (format "[~a, :got_lo12:~a]" reg lbl))
      (,() (error "ldr-src->str" "unmatch" arg))))
  
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
      ((cneg ,dst ,src ,cmp)
       (emit op "cneg ~a, ~a, ~a" dst src cmp))
      ((,binary-op ,dst ,src-0 ,src-1)
       (guard (memq binary-op '(add sub and orr lsl asr mul sdiv)))
       (emit op "~a ~a, ~a, ~a" binary-op dst src-0 (arg->str src-1)))
      ((,unary-op ,dst ,src)
       (guard (and (memq unary-op '(neg)) (reg? dst) (reg? src)))
       (emit op "~a ~a, ~a" unary-op dst src))
      ((msub ,dst ,src-0 ,src-1 ,src-2)
       (guard (andmap reg? (list dst src-0 src-1 src-2)))
       (emit op "msub ~a, ~a, ~a, ~a" dst src-0 src-1 src-2))
      ((cmp ,v ,w)
       (emit op "cmp ~a, ~a" v w))
      ((,ctrl ,lbl)
       (guard (and (memq ctrl '(blr blt ble b br beq bne bge bg)) (symbol? lbl)))
       (emit op "~a ~a" ctrl lbl))
      ((csel ,r0 ,r1 ,r2 ,cmp)
       (guard (and (reg? r0) (reg? r1) (reg? r2)))
       (emit op "csel ~a, ~a, ~a, ~a" r0 r1 r2 cmp))
      ((cset ,reg ,cmp)
       (emit op "cset ~a, ~a" reg cmp))
      ((mov ,dst ,src)
       (emit op "mov ~a, ~a" dst (arg->str src)))
      ((movz ,dst ,src)
       (emit op "mov ~a, ~a" dst src))
      ((movk ,dst ,src (lsl ,amt))
       (emit op "movk ~a, ~a, lsl ~a" dst src amt))
      ((ldr ,reg ,src)
       (emit op "ldr ~a, ~a" reg (ldr-src->str src)))
      ((ldrb ,reg ,src)
       (emit op "ldrb ~a, ~a" reg (ldr-src->str src)))
      ((str ,dst ,reg)
       (emit op "str ~a, ~a" reg (ldr-src->str dst)))
      ((strb ,dst ,reg)
       (emit op "strb ~a, ~a" reg (ldr-src->str dst)))
      ((stp ,dst ,reg-0 ,reg-1)
       (emit op "stp ~a, ~a, ~a" (arg->str reg-0) (arg->str reg-1) (arg->str dst)))
      ((ldp ,reg-0 ,reg-1 ,src)
       (emit op "ldp ~a, ~a, ~a" (arg->str reg-0) (arg->str reg-1) (arg->str src)))
      ((adr ,reg ,lbl)
       (guard (symbol? lbl))
       (emit op "adr ~a, ~a" reg lbl))
      ((adrp ,reg ,lbl)
       (guard (symbol? lbl))
       (emit op "adrp ~a, ~a" reg lbl))
      ((adrp ,reg (:got ,lbl))
       (guard (reg? reg))
       (emit op "adrp ~a, :got:~a" reg lbl))
      ((bl ,fn)
       (emit op "bl ~a" fn))
      ((brk ,imm)
       (emit op "brk ~a" imm))
      ((ret) (emit op "ret"))
      (,() (error "emit-ir" "unmatch" instr))))

  (define (emit-ir-seq op instrs)
    (if (pair? instrs)
        (begin
          (emit-ir op (car instrs))
          (emit-ir-seq op (cdr instrs)))))

  emit-ir-seq)) ;;; emit-ir-seq

(include "compiler-tools.scm")
