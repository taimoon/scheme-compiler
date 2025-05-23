# Data Representation
## Memory Layout
```
      type      | 31            bit             0
-------------------------------------------------
        integer | iiiiiiiiiiiiiiiiiiiiiiiiiiiii00
           char | 000000000000000cccccccc00000111
        boolean | 0000000000000000000000b00001111
   pair pointer | pppppppppppppppppppppppppppp001
 vector pointer | pppppppppppppppppppppppppppp010
 string pointer | pppppppppppppppppppppppppppp011
 symbol pointer | pppppppppppppppppppppppppppp101
closure pointer | pppppppppppppppppppppppppppp110
```

## String Representation
```
+----------+----+----+-----+-------+
| fxlength | c0 | c1 | ... | #\nul |
+----------+----+----+-----+-------+
```
The extra null character is convenient in casting to C-string when interfacing with C functions.

## Vector Representation
```
+----------+--------+--------+--------+
| fxlength |   v0   |   v1   |   ...  |
+----------+--------+--------+--------+
```

## Closure Representation
```
+------------+------------+------------+------------+------------+
|code pointer|  fxlength  |     fv1    |    fv2     |     ...    |
+------------+------------+------------+------------+------------+
```

# Calling Convention
```
#;
(calling layout for x64, x86
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
