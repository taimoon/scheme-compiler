# Calling Convention
```
#;
(calling layout
  "si points to free cell; starts with -4(%rsp).
   Current impl allocate location for locals without mutating the stack pointer.
   In other words, current frame does not mutate esp.
   Therefore, esp here will act frame pointer for gc tracing purpose.
   That is esp always points to old esp.
   Since ebp is used for closure pointer, the caller need to save the frame pointer.
   Note that -4(%rsp) always points to closure pointer.
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
```

```
;;; alignment formula
; (off + (8 - 1)) & -8 = (off & 7) + -8
#|INTERNALS:
;;;; Calling convention
%ebp is used as closure pointer but caller-save.

(i686
  (caller-save eax ecx edx)
  (callee-save ebx ebp edi esi esp))
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
```