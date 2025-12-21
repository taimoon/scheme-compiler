#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>
#include <inttypes.h>
#include <string.h>

#if defined(__x86_64__) || defined(__aarch64__) || (defined(__riscv) && (__riscv_xlen == 64))
#define FIXNUM_MASK     0b00000111
#define FIXNUM_SHIFT    3
#else
#define FIXNUM_MASK     0b00000011
#define FIXNUM_SHIFT    2
#endif

typedef intptr_t word_t;
#define PTR_MASK        0b111

#define FIXNUM_TAG      0b000
#define PAIR_TAG        0b001
#define FLO_TAG         0b010
#define SYM_TAG         0b011
// OCCUPIED             0b100 // OCCUPIED by fixnums on 32-bits machines
#define CLOS_TAG        0b101
#define IMM_TAG         0b110
#define OBJ_TAG         0b111
// IMM_TAG              0bxxxxx110
#define BOOL_TAG        0b00000110
#define CHAR_TAG        0b00001110
#define NIL_TAG         0b00010110
#define EOF_TAG         0b00011110
#define VOID_TAG        0b00100110
#define UNBOUND_TAG     0b01000110

#define IMM_SHIFT       8
#define IMM_MASK        0b11111111
#define FALSE_IMM       BOOL_TAG
#define TRUE_IMM        ((1 << IMM_SHIFT) | BOOL_TAG)

#define PAIR_MASK       PTR_MASK
#define OBJ_MASK        0b111
#define VEC_TAG         0b000
#define BYTEVEC_TAG     0b001
#define STR_TAG         0b010
#define STR_MASK        FIXNUM_MASK
#define VEC_MASK        FIXNUM_MASK
#define BYTEVEC_MASK    FIXNUM_MASK

word_t HEAP_WORDS_MAX = 4 << 20;
word_t HEAP_WORDS = 20 << 10; // NOTE: this is minimum required to bootstrap the lib
word_t *free_ptr, *scan_ptr;
word_t *fromspace_start, *fromspace_end;
word_t *tospace_start, *tospace_end;
static bool *gc_markers;
word_t _s_str2sym = 0;
word_t ARGS = NIL_TAG;
word_t *BOT_FP = NULL;
word_t *LBL_TBL = NULL;
bool _DEBUG_LIVE_RATIO_FLAG = false;

word_t s_collect(word_t sz, word_t * const sp, word_t * const fp);
void s_trace_stack(word_t * const sp, word_t * const fp, word_t * const bot_fp);
void s_profile_heap(word_t sz);

static inline word_t align_to_multiple(word_t alignment, word_t offset) {
    return (offset + (alignment - 1)) & -alignment;
}

static inline  word_t min(word_t x, word_t y) {
    return x < y ? x : y;
}

static inline word_t max(word_t x, word_t y) {
    return x > y ? x : y;
}

word_t s_cstr_to_bytevec(const char *s) {
    word_t len = strlen(s);
    if(fromspace_end - free_ptr < sizeof(word_t) + len) {
        fprintf(stderr, "[%s:%d] %s: ", __FILE__, __LINE__, __FUNCTION__);
        fprintf(stderr, "insufficient memory\n");
        abort();
    }
    word_t *_w = free_ptr;
    free_ptr = (word_t*)align_to_multiple(8, (word_t)free_ptr + sizeof(word_t) + len);
    char *dst = (char*)(_w + 1);
    _w[0] = (len << FIXNUM_SHIFT) | BYTEVEC_TAG;
    for(int i = 0; i < len; ++i) {
        dst[i] = s[i];
    }
    return (word_t)_w | OBJ_TAG;
}

void _scm_init(word_t argc, char ** argv) {
    char *v;
    v = getenv("HEAP_WORDS_MAX");
    HEAP_WORDS_MAX = v == NULL ? HEAP_WORDS_MAX : strtol(v, NULL, 10) << 10;
    v = getenv("HEAP_WORDS");
    HEAP_WORDS = v == NULL ? HEAP_WORDS : strtol(v, NULL, 10) << 10;
    v = getenv("_DEBUG_LIVE_RATIO_FLAG");
    _DEBUG_LIVE_RATIO_FLAG = v != NULL && strcmp(v, "ON") == 0;
    fromspace_start = aligned_alloc(8, HEAP_WORDS * sizeof(word_t));
    fromspace_end = fromspace_start + HEAP_WORDS;
    tospace_start = aligned_alloc(8, HEAP_WORDS * sizeof(word_t));
    tospace_end = tospace_start + HEAP_WORDS;
    gc_markers = aligned_alloc(8, HEAP_WORDS * sizeof(bool));
    free_ptr = fromspace_start;
    ARGS = NIL_TAG;
    for(int i = argc - 1; i >= 0; --i) {
        word_t tmp = s_cstr_to_bytevec(argv[i]);
        free_ptr[1] = ARGS;
        free_ptr[0] = tmp;
        ARGS = (word_t)free_ptr | PAIR_TAG;
        free_ptr += 2;
    }
}

int utf32_to_utf8(uint32_t c32, uint8_t *buf) {
    if(c32 <= 0x7F) {
        buf[0] = (uint8_t)c32;
        return 1;
    } 
    else if(c32 <= 0x7FF) {
        buf[0] = 0xC0 | (c32 >> 6);
        buf[1] = 0x80 | (c32 & 0x3F);
        return 2;
    } 
    else if(c32 <= 0xFFFF) {
        // Exclude UTF-16 surrogate range (invalid in UTF-8)
        if (c32 >= 0xD800 && c32 <= 0xDFFF) return 0;
        buf[0] = 0xE0 | (c32 >> 12);
        buf[1] = 0x80 | ((c32 >> 6) & 0x3F);
        buf[2] = 0x80 | (c32 & 0x3F);
        return 3;
    } 
    else if (c32 <= 0x10FFFF) {
        buf[0] = 0xF0 | (c32 >> 18);
        buf[1] = 0x80 | ((c32 >> 12) & 0x3F);
        buf[2] = 0x80 | ((c32 >> 6) & 0x3F);
        buf[3] = 0x80 | (c32 & 0x3F);
        return 4;
    } 
    else {
        // Invalid Unicode scalar value
        return 0;
    }
}

static word_t s_write_char(word_t v, FILE* fptr) {
    assert((v & IMM_MASK) == CHAR_TAG);
    uint32_t ch = v >> IMM_SHIFT;
    uint8_t buf[4];
    int len = utf32_to_utf8(ch, buf);
    if(len == 0) {
        // � to indicate unknown utf32
        len = utf32_to_utf8(0xFFFD, buf);
    }
    fwrite(buf, 1, len, fptr);
    return VOID_TAG;
}

word_t s_write(word_t v, FILE* fptr) {
    if((v & FIXNUM_MASK) == FIXNUM_TAG) {
        fprintf(fptr, "%" PRIdPTR, v >> FIXNUM_SHIFT);
    }
    else if((v & PAIR_MASK) == PAIR_TAG) {
        word_t *ptr = (word_t*)(v - PAIR_TAG);
        word_t car = ptr[0];
        word_t cdr = ptr[1];
        
        fprintf(fptr, "(");
        s_write(car, fptr);
        
        while((cdr & PAIR_MASK) == PAIR_TAG) {
            fprintf(fptr, " ");
            ptr = (word_t*)(cdr - PAIR_TAG);
            car = ptr[0];
            cdr = ptr[1];
            s_write(car, fptr);
        }
        if((cdr & IMM_MASK) == NIL_TAG) {
            fprintf(fptr, ")");
        }
        else {
            fprintf(fptr, " . ");
            s_write(cdr, fptr);
            fprintf(fptr, ")");
        }
    }
    else if((v & PTR_MASK) == CLOS_TAG) {
        fprintf(fptr, "#<procedure %p>", (void*)*(word_t*)(v - CLOS_TAG));
    }
    else if((v & PTR_MASK) == SYM_TAG) {
        v = ((word_t*)(v - SYM_TAG))[2];
        assert((v & OBJ_MASK) == OBJ_TAG && (*(word_t*)(v - OBJ_TAG) & STR_MASK) == STR_TAG);
        word_t len = *(word_t*)(v - OBJ_TAG) - STR_TAG;
        uint32_t *ptr = (uint32_t*)((word_t*)(v - OBJ_TAG) + 1);
        assert((len & FIXNUM_MASK) == FIXNUM_TAG);
        assert(len >= 0);
        len = len >> FIXNUM_SHIFT;
        for(int i = 0; i < len; ++i) {
            assert((ptr[i] & IMM_MASK) == CHAR_TAG);
            s_write_char(ptr[i], fptr);
        }
    }
    else if((v & IMM_MASK) == CHAR_TAG) {
        uint32_t ch = v >> IMM_SHIFT;
        if(ch == ' '){
            fprintf(fptr, "#\\space");
        }
        else if(ch == '\0'){
            fprintf(fptr, "#\\nul");
        }
        else if(ch == '\n'){
            fprintf(fptr, "#\\newline");
        }
        else{
            fprintf(fptr, "#\\");
            s_write_char(v, fptr);
        }
    }
    else if((v & IMM_MASK) == BOOL_TAG) {
        fprintf(fptr, v >> IMM_SHIFT ? "#t" : "#f");
    }
    else if((v & IMM_MASK) == NIL_TAG) {
        fprintf(fptr, "()");
    }
    else if((v & IMM_MASK) == EOF_TAG) {
        fprintf(fptr, "#!eof");
    }
    else if((v & IMM_MASK) == VOID_TAG) {
        fprintf(fptr, "#<void>");
    }
    else if((v & IMM_MASK) == UNBOUND_TAG) {
        fprintf(fptr, (v >> IMM_SHIFT) == 1 ? "#<ENTRY-MARK>" : "#<UNBOUND>");
    }
    else if ((v & OBJ_MASK) == OBJ_TAG && (*(word_t*)(v - OBJ_TAG) & STR_MASK) == STR_TAG) {
        word_t len = *(word_t*)(v - OBJ_TAG) - STR_TAG;
        uint32_t *ptr = (uint32_t*)((word_t*)(v - OBJ_TAG) + 1);
        assert((len & FIXNUM_MASK) == FIXNUM_TAG);
        assert(len >= 0);
        len = len >> FIXNUM_SHIFT;
        putc('"', fptr);
        for(int i = 0; i < len; ++i) {
            assert((ptr[i] & IMM_MASK) == CHAR_TAG);
            uint32_t ch = ptr[i] >> IMM_SHIFT;
            if(ch == '\n') fprintf(fptr, "\\n");
            else if(ch == '\t') fprintf(fptr, "\\t");
            else if(ch == '\r') fprintf(fptr, "\\r");
            else if(ch == '\"') fprintf(fptr, "\\\"");
            else if(ch == '\\') fprintf(fptr, "\\\\");
            else {
                s_write_char(ptr[i], fptr);
            }
        }
        putc('"', fptr);
    }
    else if ((v & OBJ_MASK) == OBJ_TAG && (*(word_t*)(v - OBJ_TAG) & VEC_MASK) == VEC_TAG) {
        word_t len = *(word_t*)(v - OBJ_TAG) - VEC_TAG;
        word_t *ptr = ((word_t*)(v - OBJ_TAG) + 1);
        assert((len & FIXNUM_MASK) == FIXNUM_TAG);
        assert(len >= 0);
        len = len >> FIXNUM_SHIFT;
        fprintf(fptr, "#(");
        for(int i = 0; i < len; ++i) {
            s_write(ptr[i], fptr);
            if(i != len - 1)
                fprintf(fptr, " ");
        }
        fprintf(fptr, ")");
    }
    else if ((v & OBJ_MASK) == OBJ_TAG && (*(word_t*)(v - OBJ_TAG) & BYTEVEC_MASK) == BYTEVEC_TAG) {
        word_t len = *(word_t*)(v - OBJ_TAG) - BYTEVEC_TAG;
        uint8_t *ptr = (uint8_t*)((word_t*)(v - OBJ_TAG) + 1);
        assert((len & FIXNUM_MASK) == FIXNUM_TAG);
        assert(len >= 0);
        len = len >> FIXNUM_SHIFT;
        fprintf(fptr, "#vu8(");
        for(int i = 0; i < len; ++i) {
            fprintf(fptr, "%d", ptr[i]);
            if(i != len - 1)
                fprintf(fptr, " ");
        }
        fprintf(fptr, ")");
    }
    else {
        fprintf(stderr, "[%s:%d] %s: unknown object %p\n", __FILE__, __LINE__, __FUNCTION__, (void*) v);
        abort();
    }
    return VOID_TAG;
}

word_t s_writeln(word_t v, FILE* fptr) {
    s_write(v, fptr);
    putc('\n', fptr);
    return VOID_TAG;
}

word_t s_newline(FILE* fptr) {
    putc('\n', fptr);
    return VOID_TAG;
}

word_t construct_vararg(int target_argc, int argc, word_t *sp, word_t *fp) {
    assert(argc >= 0 && target_argc >= 0);
    assert((fp - 1) > sp && ((fp - 1) - sp) == argc);
    assert(target_argc <= argc + 1);
    // (1 2 3 4) => (cons 1 (cons 2 (cons 3 (cons 4 '()))))
    // 2 * n
    s_collect(2 * sizeof(word_t) * ((argc + 1 - target_argc) + 1), sp ,fp);
    word_t v = NIL_TAG;
    for(int i = 0; i < (argc + 1 - target_argc); ++i) {
        free_ptr[1] = v;
        free_ptr[0] = sp[i];
        v = (word_t)free_ptr | PAIR_TAG;
        free_ptr += 2;
    }
    return v;
}

word_t shift_values(int argc, word_t * const sp, word_t * const fp) {
    for(int i = 0; i < argc; ++i) {
        fp[-(i+2)] = sp[(argc - 1) - i];
    }
    return argc;
}

word_t set_s_str2sym(word_t v) {
    _s_str2sym = v;
    return VOID_TAG;
}

typedef struct s_bytevec {
    word_t len;
    uint8_t *ptr;
} s_bytevec;

static inline word_t _s_fixnum_to_word(word_t v) {
    assert((v & FIXNUM_MASK) == FIXNUM_TAG);
    return v >> FIXNUM_SHIFT;
}

word_t s_make_string(word_t fx, word_t c){
    uint32_t wc = (uint32_t)c;
    int len = _s_fixnum_to_word(fx);
    assert(len >= 0);
    word_t *_w = free_ptr;
    free_ptr = (word_t*)align_to_multiple(8, (word_t)free_ptr + sizeof(word_t) + sizeof(uint32_t) * len);
    _w[0] = fx | STR_TAG;
    uint32_t *dst = (uint32_t*)(_w + 1);
    for (int i = 0; i < len; ++i) {
        dst[i] = wc;
    }
    word_t w = (word_t)_w | OBJ_TAG;
    return w;
}

word_t s_make_vector(word_t fx, word_t x){
    int len = _s_fixnum_to_word(fx);
    assert(len >= 0);
    word_t *_w = free_ptr;
    free_ptr = (word_t*)align_to_multiple(8, (word_t)(free_ptr + len + 1));
    _w[0] = fx | VEC_TAG;
    for (int i = 0; i < len; ++i) {
        _w[i+1] = x;
    }
    word_t w = (word_t)_w | OBJ_TAG;
    return w;
}

word_t s_make_bytevector(word_t fx, word_t x){
    int len = _s_fixnum_to_word(fx);
    assert(len >= 0);
    word_t *_w = free_ptr;
    free_ptr = (word_t*)align_to_multiple(8, (word_t)free_ptr + sizeof(word_t) + len);
    uint8_t *dst = (uint8_t*)(_w + 1);
    _w[0] = (len << FIXNUM_SHIFT) | BYTEVEC_TAG;
    if (x != FALSE_IMM) {
        uint8_t b = (uint8_t)_s_fixnum_to_word(x);
        for(int i = 0; i < len; ++i) {
            dst[i] = b;
        }
    }
    word_t w = (word_t)_w | OBJ_TAG;
    return w;
}

static inline s_bytevec _s_bytevec_to_cstruct(word_t v) {
    assert((v & OBJ_MASK) == OBJ_TAG);
    assert((*(word_t*)(v - OBJ_TAG) & BYTEVEC_MASK) == BYTEVEC_TAG);
    word_t len = *(word_t*)(v - OBJ_TAG) - BYTEVEC_TAG;
    uint8_t *ptr = (uint8_t*)((word_t*)(v - OBJ_TAG) + 1);
    assert((len & FIXNUM_MASK) == FIXNUM_TAG && len >= 0);
    len = len >> FIXNUM_SHIFT;
    return (s_bytevec){.len = len, .ptr = ptr};
}

static inline char* _s_bytevec_to_cstr(word_t v) {
    s_bytevec buf = _s_bytevec_to_cstruct(v);
    assert(buf.ptr[buf.len - 1] == '\0');
    return (char*)buf.ptr;
}

extern void L_implicit_restore_stack(void);
extern void L_explicit_restore_stack(void);

static const int CONT_HDR_SZ = 5;

static inline int _s_cont_stack_size(word_t* cont) {
    assert(cont[0] == (word_t)L_explicit_restore_stack);
    return (cont[1] >> FIXNUM_SHIFT) - CONT_HDR_SZ;
}

static inline word_t* _s_cont_stack_sp(word_t* cont) {
    assert(cont[0] == (word_t)L_explicit_restore_stack);
    return cont + 2 + CONT_HDR_SZ;
}

static inline word_t* _s_cont_stack_fp(word_t* cont) {
    assert(cont[0] == (word_t)L_explicit_restore_stack);
    return _s_cont_stack_sp(cont) + cont[2];
}

static inline word_t* _s_cont_stack_bot_fp(word_t* cont) {
    assert(cont[0] == (word_t)L_explicit_restore_stack);
    return _s_cont_stack_sp(cont) + _s_cont_stack_size(cont) - 2;
}

void s_trace_cont(word_t _cont) {
    assert((_cont & PTR_MASK) == CLOS_TAG);
    word_t *cont = (word_t*)(_cont - CLOS_TAG);
    s_trace_stack(_s_cont_stack_sp(cont), _s_cont_stack_fp(cont), _s_cont_stack_bot_fp(cont));
}

word_t s_reify_cont(word_t clos, word_t ret, word_t *sp, word_t *fp) {
    bool is_tail = sp == fp;
    word_t ebx = free_ptr[0];
    assert(BOT_FP >= fp && fp >= sp);
    bool is_tail_cc = (
        is_tail
        && fp == BOT_FP - 4
        && fp[5] == (word_t)abort
        && fp[3] == 0xCA11
        && fp[1] == (word_t)L_implicit_restore_stack
        && fp[0] == (word_t)BOT_FP
    );
    if(is_tail_cc) {
        word_t v = BOT_FP[-2];
        assert((v & PTR_MASK) == CLOS_TAG);
        assert(*(word_t*)(v - CLOS_TAG) == (word_t)L_explicit_restore_stack);
        ebx = fp[-1];
        BOT_FP[-5] = ebx;
        BOT_FP[-6] = clos;
        BOT_FP[-7] = v;
        return clos;
    }
    if(is_tail) {
        assert(ret == fp[1]);
        ebx = fp[-1];
        sp = fp + 2;
        fp = (word_t*)*fp;
    }
    // s_trace_stack(sp, fp, BOT_FP);
    int stack_size = (BOT_FP - sp) + 2;
    assert(BOT_FP + 1 == sp + stack_size - 1);
    word_t *cont = free_ptr;
    int free_sz = fromspace_end - free_ptr;
    int req_sz = 2 + CONT_HDR_SZ + stack_size;
    if(free_sz < req_sz) {
        fprintf(stderr, "[%s:%d] %s: ", __FILE__, __LINE__, __FUNCTION__);
        fprintf(stderr, "insufficient memory\n");
        s_profile_heap(req_sz << FIXNUM_SHIFT);
        abort();
    }
    free_ptr = (word_t*)align_to_multiple(8, (word_t)(free_ptr + 2 + CONT_HDR_SZ + stack_size));
    cont[0] = (word_t)L_explicit_restore_stack;
    cont[1] = (CONT_HDR_SZ + stack_size) << FIXNUM_SHIFT;
    cont[2] = fp - sp;
    cont[3] = ret;
    cont[4] = *BOT_FP;
    cont[5] = (word_t)sp;
    cont[6] = ebx;
    word_t *cont_sp = _s_cont_stack_sp(cont);
    for(int i = 0; i < stack_size; ++i) {
        cont_sp[i] = sp[i];
    }
    for(word_t *_fp = fp; _fp < BOT_FP; _fp = (word_t*)*_fp) {
        int curr_frame_size = _fp - sp;
        int next_frame_size = (word_t*)*_fp - sp;
        cont_sp[curr_frame_size] = (word_t)(cont_sp + next_frame_size);
    }
    word_t v = (word_t)cont | CLOS_TAG;
    // s_trace_cont(v);
    BOT_FP[1] = (word_t)abort;
    BOT_FP[0] = (word_t)fp;
    BOT_FP[-1] = 0xCA11;
    BOT_FP[-2] = v;
    BOT_FP[-3] = (word_t)L_implicit_restore_stack;
    BOT_FP[-4] = (word_t)BOT_FP;
    BOT_FP[-5] = ebx;
    BOT_FP[-6] = clos;
    BOT_FP[-7] = v;
    return clos;
}

word_t s_apply_cont(word_t * const _sp) {
    word_t len = _s_fixnum_to_word(tospace_start[0]);
    word_t _cont = tospace_start[1];
    word_t val = tospace_start[2];
    // fprintf(stderr, "len = %d\n", len);
    // for(int i = 0; i < len; ++i) {
    //     s_writeln(tospace_start[i+1], stderr);
    // }
    // s_trace_cont(_cont);
    assert((_cont & PTR_MASK) == CLOS_TAG);
    word_t *cont = (word_t*)(_cont - CLOS_TAG);
    assert(cont[0] == (word_t)L_explicit_restore_stack);
    word_t *cont_stk = _s_cont_stack_sp(cont);
    int stack_size = _s_cont_stack_size(cont);
    assert((BOT_FP - _sp) + 2 == (stack_size + (len - 1) + 5));
    assert((BOT_FP - (_sp + (len - 1) + 5)) + 2 == stack_size);
    word_t * const sp = _sp + (len - 1) + 5;
    // NOTE:
    // sp , ret , fp  , ebx , clos  , v0  , v1  , ...
    // 0  , 1   , 2   , 3   , 4     , 5   , 6   , ...
    // why `len - 1` and `i + 2`?
    // Incoming len includes continuation which we don't want to copy over
    for(int i = 0; i < len - 1; ++i) {
        sp[-i-5] = tospace_start[i + 2];
    }
    for(int i = 0; i < stack_size; ++i) {
        sp[i] = cont_stk[i];
    }
    word_t *fp = _s_cont_stack_fp(cont);
    word_t *bot_fp = _s_cont_stack_bot_fp(cont);
    for(word_t* _fp = fp; _fp < bot_fp; _fp = (word_t*)*_fp){
        int cur_frame_sz = _fp - cont_stk;
        int nxt_frame_sz = (word_t*)*_fp - cont_stk;
        sp[cur_frame_sz] = (word_t)(sp + nxt_frame_sz);
    }
    *BOT_FP = cont[4];
    // s_trace_stack(sp, sp + cont[2], BOT_FP);
    free_ptr[0] = len - 1;
    free_ptr[1] = cont[5]; // sp
    free_ptr[2] = (word_t)(sp + cont[2]); // fp
    free_ptr[3] = cont[6]; // ebx
    free_ptr[4] = cont[3]; // ret
    return val;
}

void s_exit(word_t v) {
    exit((v & FIXNUM_MASK) == FIXNUM_TAG ? v >> FIXNUM_SHIFT : v);
}

word_t s_system(word_t cmd) {
    return system(_s_bytevec_to_cstr(cmd)) << FIXNUM_SHIFT;
}

#include <sys/unistd.h>
word_t s_getpid() {
    return getpid() << FIXNUM_SHIFT;
}

word_t s_getenv(word_t x){
    const char* val = getenv(_s_bytevec_to_cstr(x));
    return val == NULL ? FALSE_IMM : s_cstr_to_bytevec(val);
}

word_t s_fopen(word_t filename, word_t mode) {
    FILE *fptr = fopen(_s_bytevec_to_cstr(filename), _s_bytevec_to_cstr(mode));
    assert(((word_t)fptr % 8) == 0);
    return (word_t)fptr;
}

word_t s_fclose(FILE *fptr) {
    fclose(fptr);
    return VOID_TAG;
}

word_t s_fread(word_t v, word_t fx_start, word_t fx_end, FILE* fptr) {
    word_t start = _s_fixnum_to_word(fx_start);
    word_t end = _s_fixnum_to_word(fx_end);
    s_bytevec buf = _s_bytevec_to_cstruct(v);
    assert(start >= 0 && end >= 0 && start <= end);
    assert(end - start <= buf.len);
    size_t sz = fread(buf.ptr + start, 1, end - start, fptr);
    if(sz == end - start) {
        return sz << FIXNUM_SHIFT;
    }
    else if(feof(fptr)) {
        return EOF_TAG;
    }
    else {
        return VOID_TAG;
    }
}

word_t s_fwrite(word_t v, word_t fx_start, word_t fx_end, FILE* fptr) {
    word_t start = _s_fixnum_to_word(fx_start);
    word_t end = _s_fixnum_to_word(fx_end);
    s_bytevec buf = _s_bytevec_to_cstruct(v);
    assert(start >= 0 && end >= 0 && start <= end);
    assert(end - start <= buf.len);
    fwrite(buf.ptr + start, 1, end - start, fptr);
    return VOID_TAG;
}

#include <dlfcn.h>
word_t s_dlopen(word_t inp, word_t entry) {
    const char *_inp = _s_bytevec_to_cstr(inp);
    const char *_ent = _s_bytevec_to_cstr(entry);
    void *handle = dlopen(_inp, RTLD_NOW);
    if (!handle) {
        fprintf(stderr, "\n[%s:%d] %s: dlopen failed: %s %s\n", __FILE__, __LINE__, __func__, _inp, dlerror());
        abort();
    }
    void *_entry_fn = dlsym(handle, _ent);
    if (!_entry_fn) {
        fprintf(stderr, "\n[%s:%d] %s: dlsym failed: %s %s\n", __FILE__, __LINE__, __func__, _ent, dlerror());
        dlclose(handle);
        abort();
    }
    free_ptr[0] = (word_t) _entry_fn;
    free_ptr[1] = 0;
    word_t res = (word_t)free_ptr | CLOS_TAG;
    free_ptr = (word_t*)align_to_multiple(8, (word_t)(free_ptr + 2));
    return res;
}

// GC
static inline bool is_nonheap(word_t v) {
    return (v & FIXNUM_MASK) == FIXNUM_TAG || (v & OBJ_MASK) == IMM_TAG;
}

word_t s_copy(const word_t v) {
    if(is_nonheap(v)) {
        return v;
    }
    word_t *ptr = (word_t*)(v & ~PTR_MASK);
    assert((word_t)free_ptr % 8 == 0);
    assert(fromspace_start <= free_ptr && free_ptr < fromspace_end);
    assert(fromspace_start <= scan_ptr && scan_ptr <= fromspace_end);
    if(free_ptr >= (scan_ptr - 1)) {
        fprintf(stderr, "[%s:%d] %s: ", __FILE__, __LINE__, __FUNCTION__);
        fprintf(stderr, "scan_ptr touches free_ptr :(\n");
        abort();
    }
    assert(tospace_start <= ptr && ptr < tospace_end);
    int idx = ptr - tospace_start;
    if(gc_markers[idx]) {
        return *ptr;
    }
    gc_markers[idx] = true;
    if((v & PAIR_MASK) == PAIR_TAG) {
        word_t *ptr = (word_t*)(v - PAIR_TAG);
        word_t *_w = free_ptr;
        free_ptr = (word_t*)align_to_multiple(8, (word_t)(free_ptr + 2));
        _w[0] = ptr[0];
        _w[1] = ptr[1];
        word_t w = (word_t)_w | PAIR_TAG;
        *ptr = w;
        *--scan_ptr = w;
        return w;
    }
    else if((v & PTR_MASK) == CLOS_TAG) {
        word_t *ptr = (word_t*)(v - CLOS_TAG);
        word_t len = _s_fixnum_to_word(ptr[1]);
        word_t *_w = free_ptr;
        free_ptr = (word_t*)align_to_multiple(8, (word_t)(free_ptr + len + 2));
        _w[0] = ptr[0];
        _w[1] = ptr[1];
        for(int i = 0; i < len; ++i) {
            _w[i + 2] = ptr[i + 2];
        }
        if(ptr[0] == (word_t)L_explicit_restore_stack) {
            word_t *bot_fp = _s_cont_stack_bot_fp(ptr);
            word_t *sp = _s_cont_stack_sp(ptr);
            word_t *cont_sp = _s_cont_stack_sp(_w);
            for(word_t *_fp = _s_cont_stack_fp(ptr); _fp < bot_fp; _fp = (word_t*)*_fp) {
                int curr_frame_size = _fp - sp;
                int next_frame_size = (word_t*)*_fp - sp;
                cont_sp[curr_frame_size] = (word_t)(cont_sp + next_frame_size);
            }
        }
        word_t w = (word_t)_w | CLOS_TAG;
        *ptr = w;
        *--scan_ptr = w;
        return w;
    }
    else if((v & PTR_MASK) == SYM_TAG) {
        word_t *ptr = (word_t*)(v - SYM_TAG);
        word_t *_w = free_ptr;
        free_ptr = (word_t*)align_to_multiple(8, (word_t)(free_ptr + 4));
        word_t val = ptr[0];
        word_t hash = ptr[1];
        word_t name = ptr[2];
        _s_fixnum_to_word(hash);
        _w[0] = val;
        _w[1] = hash;
        _w[2] = name;
        _w[3] = 0;
        word_t w = (word_t)_w | SYM_TAG;
        *ptr = w;
        *--scan_ptr = w;
        return w;
    }
    else if ((v & OBJ_MASK) == OBJ_TAG && (*(word_t*)(v - OBJ_TAG) & VEC_MASK) == VEC_TAG) {
        word_t len = _s_fixnum_to_word(*(word_t*)(v - OBJ_TAG) - VEC_TAG);
        assert(len >= 0);
        word_t *_w = free_ptr;
        free_ptr = (word_t*)align_to_multiple(8, (word_t)(free_ptr + len + 1));
        word_t *src = ((word_t*)(v - OBJ_TAG) + 1);
        _w[0] = (len << FIXNUM_SHIFT) | VEC_TAG;
        word_t *dst = _w + 1;
        for(int i = 0; i < len; ++i) {
            dst[i] = src[i];
        }
        word_t w = (word_t)_w | OBJ_TAG;
        word_t *ptr = (word_t*)(v - OBJ_TAG);
        *ptr = w;
        *--scan_ptr = w;
        return w;
    }
    else if((v & OBJ_MASK) == OBJ_TAG && (*(word_t*)(v - OBJ_TAG) & STR_MASK) == STR_TAG) {
        word_t len = _s_fixnum_to_word(*(word_t*)(v - OBJ_TAG) - STR_TAG);
        assert(len >= 0);
        uint32_t *src = (uint32_t*)((word_t*)(v - OBJ_TAG) + 1);
        word_t *_w = free_ptr;
        free_ptr = (word_t*)align_to_multiple(8, (word_t)free_ptr + sizeof(word_t) + sizeof(uint32_t) * len);
        uint32_t *dst = (uint32_t*)(_w + 1);
        _w[0] = (len << FIXNUM_SHIFT) | STR_TAG;
        for(int i = 0; i < len; ++i) {
            dst[i] = src[i];
        }
        word_t w = (word_t)_w | OBJ_TAG;
        word_t *ptr = (word_t*)(v - OBJ_TAG);
        *ptr = w;
        return w;
    }
    else if((v & OBJ_MASK) == OBJ_TAG && (*(word_t*)(v - OBJ_TAG) & BYTEVEC_MASK) == BYTEVEC_TAG) {
        word_t len = _s_fixnum_to_word(*(word_t*)(v - OBJ_TAG) - BYTEVEC_TAG);
        assert(len >= 0);
        uint8_t *src = (uint8_t*)((word_t*)(v - OBJ_TAG) + 1);
        word_t *_w = free_ptr;
        free_ptr = (word_t*)align_to_multiple(8, (word_t)free_ptr + sizeof(word_t) + len);
        uint8_t *dst = (uint8_t*)(_w + 1);
        _w[0] = (len << FIXNUM_SHIFT) | BYTEVEC_TAG;
        for(int i = 0; i < len; ++i) {
            dst[i] = src[i];
        }
        word_t w = (word_t)_w | OBJ_TAG;
        word_t *ptr = (word_t*)(v - OBJ_TAG);
        *ptr = w;
        return w;
    }
    else {
        fprintf(stderr, "\n[%s:%d] %s: unknown object %p %"PRIdPTR"\n", __FILE__, __LINE__, __func__, (void*) v, v & PTR_MASK);
        abort();
    }
}

static inline void _s_collect_stack(word_t * const sp, word_t * const fp, word_t * const bot_fp) {
    word_t *_fp;
    int i;
    for(i = 0, _fp = fp; sp + i < bot_fp; ++i) {
        if(sp + i == _fp - 1) {
            _fp = (word_t*)*_fp;
            i += 2;
        }
        else {
            sp[i] = s_copy(sp[i]);
        }
    }
}

void s_trace_stack(word_t * const sp, word_t * const fp, word_t * const bot_fp) {
    fprintf(stderr, "------------------------------------------\n");
    fprintf(stderr, "STACK(sp = %p fp = %p %s = %p)\n", sp, fp, bot_fp == BOT_FP ? "BOT_FP" : "bot_fp", bot_fp);
    int i;
    word_t *_fp;
    for(i = 0, _fp = fp; sp + i < bot_fp; ++i) {
        int local_th = (sp + i) - _fp;
        if(sp + i == _fp - 1) {
            fprintf(stderr, "%p: fp[%+d] = %p\n", sp + i, local_th, (word_t*)_fp[-1]);
            fprintf(stderr, "%p: fp[%+d] = %p\n", sp + i + 1, 0, (word_t*)_fp[0]);
            fprintf(stderr, "%p: fp[%+d] = %p\n", sp + i + 2, 1, (word_t*)_fp[1]);
            _fp = (word_t*)*_fp;
            i += 2;
            fprintf(stderr, "------------------------------------------\n");
        }
        else {
            fprintf(stderr, "%p: fp[%+d] = ", sp + i, local_th);
            s_writeln(sp[i], stderr);
        }
    }
    fprintf(stderr, "%s = %p\n", bot_fp == BOT_FP ? "BOT_FP" : "bot_fp", bot_fp);
}

static inline void _s_collect_scan_ptr() {
    while(scan_ptr < fromspace_end) {
        assert(free_ptr < scan_ptr);
        word_t v = *scan_ptr; ++scan_ptr;
        assert(!is_nonheap(v));
        if((v & PAIR_MASK) == PAIR_TAG) {
            word_t *ptr = (word_t*)(v - PAIR_TAG);
            ptr[0] = s_copy(ptr[0]);
            ptr[1] = s_copy(ptr[1]);
        }
        else if((v & PTR_MASK) == CLOS_TAG) {
            word_t *ptr = (word_t*)(v - CLOS_TAG);
            if(ptr[0] == (word_t)L_explicit_restore_stack) {
                _s_collect_stack(_s_cont_stack_sp(ptr), _s_cont_stack_fp(ptr), _s_cont_stack_bot_fp(ptr));
            }
            else {
            word_t len = _s_fixnum_to_word(ptr[1]);
            for(int i = 0; i < len; ++i) {
                ptr[i + 2] = s_copy(ptr[i + 2]);
                }
            }
        }
        else if((v & PTR_MASK) == SYM_TAG) {
            word_t *ptr = (word_t*)(v - SYM_TAG);
            ptr[0] = s_copy(ptr[0]);
            ptr[1] = s_copy(ptr[1]);
            ptr[2] = s_copy(ptr[2]);
            ptr[3] = s_copy(ptr[3]);
            word_t name = ptr[2];
            assert((name & OBJ_MASK) == OBJ_TAG && (*(word_t*)(name - OBJ_TAG) & STR_MASK) == STR_TAG);
        }
        else if ((v & OBJ_MASK) == OBJ_TAG && (*(word_t*)(v - OBJ_TAG) & VEC_MASK) == VEC_TAG) {
            word_t len = _s_fixnum_to_word(*(word_t*)(v - OBJ_TAG) - VEC_TAG);
            assert(len >= 0);
            word_t *ptr = ((word_t*)(v - OBJ_TAG) + 1);
            assert(len >= 0);
            for(int i = 0; i < len; ++i) {
                ptr[i] = s_copy(ptr[i]);
            }
        }
        else{
            fprintf(stderr, "\n[%s:%d] %s: unknown object %p %"PRIdPTR"\n", __FILE__, __LINE__, __func__, (void*) v, v & PTR_MASK);
            abort();
        }
    }
}

int HEAP_RESIZE_CNT = 0;
int FLIP_CNT = 0;

void s_profile_heap(word_t sz) {
    int used_space = free_ptr - fromspace_start;
    int marker_mem = fromspace_end - fromspace_start;
    int heap_mem =  ((fromspace_end - fromspace_start) + (tospace_end - tospace_start)) * sizeof(word_t) + marker_mem;
    int heap_mem_max =  2 * HEAP_WORDS_MAX * sizeof(word_t) + HEAP_WORDS_MAX;
    fprintf(stderr, " HEAP_WORDS (k) = %"PRIdPTR, HEAP_WORDS >> 10);
    fprintf(stderr, " markbit (b) = %d", marker_mem);
    fprintf(stderr, " used (kobj) = %d", used_space >> 10);
    fprintf(stderr, " from (kobj) = %d", marker_mem >> 10);
    fprintf(stderr, " to (kobj) = %"PRIdPTR, (tospace_end - tospace_start) >> 10);
    fprintf(stderr, " req (obj) = %"PRIdPTR, sz >> FIXNUM_SHIFT);
    fprintf(stderr, " max (mb) = %d", heap_mem_max >> 20);
    fprintf(stderr, " heap (mb) = %d", heap_mem >> 20);
    fprintf(stderr, " resize = %d", HEAP_RESIZE_CNT);
    fprintf(stderr, " flip = %d", FLIP_CNT);
    fprintf(stderr, " \n");
}

void s_trace_root(word_t * const sp, word_t * const fp) {
    fprintf(stderr, "------------------------------------------\n");
    int unit_cnt;
    word_t *ptr;
    fprintf(stderr, "LBL_TBL\n");
    for(ptr = LBL_TBL, unit_cnt = 0; ptr != NULL; ptr = (word_t*)ptr[0], ++unit_cnt) {
        fprintf(stderr, "%p: LBL_TBL[%d][0] = %p\n", ptr, unit_cnt, (word_t*)ptr[0]);
        fprintf(stderr, "%p: LBL_TBL[%d][1] = %"PRIdPTR"\n", ptr + 1, unit_cnt, ptr[1]);
        for(int i = 0; i < ptr[1]; ++i) {
            fprintf(stderr, "%p: LBL_TBL[%d][%d] = ", ptr + (i + 2), unit_cnt, i + 2);
            s_writeln(ptr[i+2], stderr);
        }
    }
    s_trace_stack(sp, fp, BOT_FP);
}

static inline void resize_tospace() {
    int tospace_words = (tospace_end - tospace_start);
    if(tospace_words >= HEAP_WORDS_MAX || tospace_words >= HEAP_WORDS) {
        return;
    }
    word_t *tospace_start_new = aligned_alloc(8, HEAP_WORDS * sizeof(word_t));
    bool *gc_markers_new = aligned_alloc(8, HEAP_WORDS * sizeof(bool));
    if(gc_markers_new != NULL && tospace_start_new != NULL) {
        free(tospace_start);
        free(gc_markers);
        tospace_start = tospace_start_new;
        tospace_end = tospace_start_new + HEAP_WORDS;
        gc_markers = gc_markers_new;
    }
    else {
        free(gc_markers_new);
        free(tospace_start_new);
        fprintf(stderr, "[%s:%d] %s: ", __FILE__, __LINE__, __FUNCTION__);
        fprintf(stderr, "cannot resize\n");
        abort();
    }
    tospace_start_new = NULL;
    gc_markers_new = NULL;
    assert(tospace_start != NULL && tospace_end != NULL && gc_markers != NULL);
}

// s_collect returns freed size in addition to garbage collection
// fx_sz : byte size requested; not in fixnum*
word_t s_collect(const word_t _sz, word_t * const sp, word_t * const fp) {
    bool forced_collect = _sz == FALSE_IMM;
    const word_t min_req_sz = 32 * sizeof(word_t);
    const word_t req_sz = forced_collect ? min_req_sz : max(_sz, min_req_sz);
    assert(_s_fixnum_to_word(req_sz) > 0);
    assert(sp < fp && fp < BOT_FP);
    assert(_s_fixnum_to_word(req_sz) > 0 && sp < fp && fp < BOT_FP);
    int free_space_old = fromspace_end - free_ptr;
    int used_space_old = free_ptr - fromspace_start;
    if((!forced_collect && req_sz <= (free_space_old * sizeof(word_t)))){
        return 0;
    }
    FLIP_CNT++;
    resize_tospace();
    for(int i = 0; i < HEAP_WORDS; ++i) {
        gc_markers[i] = false;
    }
    word_t *tmp;
    tmp = fromspace_start;
    fromspace_start = tospace_start;
    tospace_start = tmp;
    tmp = fromspace_end;
    fromspace_end = tospace_end;
    tospace_end = tmp;
    scan_ptr = fromspace_end;
    free_ptr = fromspace_start;
    _s_str2sym = s_copy(_s_str2sym);
    ARGS = s_copy(ARGS);
    for(word_t *ptr = LBL_TBL; ptr != NULL; ptr = (word_t*)ptr[0]) {
        for(int i = 0; i < ptr[1]; ++i) {
            ptr[i+2] = s_copy(ptr[i+2]);
        }
    }
    _s_collect_stack(sp, fp, BOT_FP);
    _s_collect_scan_ptr();
    int free_space_new = fromspace_end - free_ptr;
    int used_space_new = free_ptr - fromspace_start;
    int freed_size = used_space_old - used_space_new;
    
    if(_DEBUG_LIVE_RATIO_FLAG) {
        s_profile_heap(req_sz);
        fprintf(stderr, "live_ratio=%f\n", (float)used_space_new / (fromspace_end - fromspace_start));
    }
    /* The math here
    used_space_new / (fromspace_end - fromspace_start) > d / n
    n * used_space_new > d * (fromspace_end - fromspace_start)
     */
    int d = 50;
    int n = 100;
    if(n * used_space_new > d * (fromspace_end - fromspace_start) && HEAP_WORDS < HEAP_WORDS_MAX) {
        HEAP_WORDS = min(2 * HEAP_WORDS, HEAP_WORDS_MAX);
        HEAP_RESIZE_CNT++;
        resize_tospace();
        s_collect(FALSE_IMM, sp, fp);
        free_space_new = fromspace_end - free_ptr;
        used_space_new = free_ptr - fromspace_start;
        freed_size = used_space_old - used_space_new;
        assert(n * used_space_new < d * (fromspace_end - fromspace_start));
    }
    if(req_sz <= free_space_new * sizeof(word_t)) {
        return freed_size << FIXNUM_SHIFT;
    }
    else if((HEAP_WORDS_MAX - used_space_new) * sizeof(word_t) < req_sz) {
        fprintf(stderr, "[%s:%d] %s: ", __FILE__, __LINE__, __FUNCTION__);
        fprintf(stderr, "insufficient memory\n");
        s_profile_heap(req_sz);
        abort();
    }
    else {
        assert(HEAP_WORDS - used_space_new == free_space_new);
        while((HEAP_WORDS - used_space_new) * sizeof(word_t) < req_sz) {
            HEAP_WORDS = min(2 * HEAP_WORDS, HEAP_WORDS_MAX);
            if(HEAP_WORDS == HEAP_WORDS_MAX) break;
            HEAP_RESIZE_CNT++;
        }
        assert((HEAP_WORDS - used_space_new) * sizeof(word_t) >= req_sz);
        resize_tospace();
        assert((tospace_end - tospace_start) == HEAP_WORDS);
        s_collect(FALSE_IMM, sp, fp);
        assert((fromspace_end - fromspace_start) == HEAP_WORDS);
        return freed_size << FIXNUM_SHIFT;
    }
}

// MISC
void s_bad_monovariadic_call(word_t argc, word_t argc_got) {
    fprintf(stderr, "wrong number argument call %"PRIdPTR" but got %"PRIdPTR"\n", argc - 1, argc_got - 1);
    abort();
}

void s_bad_polyvariadic_call() {
    fprintf(stderr, "case-lambda wrong number argument call\n");
    abort();
}

void s_bad_apply_call() {
    fprintf(stderr, "bad apply call\n");
    abort();
}

void s_unbound_global(word_t v) {
    s_write(v, stderr);
    fprintf(stderr, " variable is unbound\n");
    abort();
}

void s_bad_imp_cont() {
    fprintf(stderr, "bad implicit continuation is not implemented\n");
    abort();
}