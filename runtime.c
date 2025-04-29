#include<stdlib.h>
#include<stdio.h>
#include<stdint.h>
#include<assert.h>
#include<stdbool.h>
#include<string.h>
typedef intptr_t word_t;
typedef intptr_t* ptr_t;
#if defined(__x86_64__)
#define FIXNUM_MASK     0b00000111
#define FIXNUM_TAG      0b00000000
#define FIXNUM_SHIFT    3
#else
#define FIXNUM_MASK     0b00000011
#define FIXNUM_TAG      0b00000000
#define FIXNUM_SHIFT    2
#endif
#define AVX_ALIGNMENT   32
#define IMM_SHIFT       8
#define IMM_TAG         0b00000111
#define IMM_MASK        0b11111111
#define EOF_TAG         0b00000111
#define CHAR_TAG        0b00001111
#define BOOL_TAG        0b00011111
#define NIL_TAG         0b00111111
#define PTR_MASK        0b00000111
#define PAIR_TAG        0b00000001
#define VEC_TAG         0b00000010
#define STR_TAG         0b00000011
#define SYM_TAG         0b00000101
#define CLOS_TAG        0b00000110
int FLIP_COUNT = 0;
int HEAP_SIZE;
word_t *free_ptr, *HEAP_START, *HEAP_END, *fromspace_start, *fromspace_end, *tospace_start, *tospace_end, *scan_ptr;
int ARGC;
char **ARGV;
word_t ARGS;
extern word_t* global_table[];
typedef struct GC_CELL_T {
    word_t obj;
    bool is_deleted;
} GC_CELL_T;
GC_CELL_T *GC_CELLS;
word_t *BOT_EBP;

inline static word_t align_to_multiple(word_t alignment, word_t offset){
    return (offset + (alignment - 1)) & -alignment;
}

void init_cmd_ln(){
    word_t *ptr;
    int len;
    ARGS = (word_t)free_ptr | VEC_TAG;
    ptr = free_ptr;
    free_ptr = (word_t*)align_to_multiple(8,(word_t)(free_ptr + ARGC + 1));

    ptr[0] = ARGC << FIXNUM_SHIFT;
    for(int i = ARGC-1; i >= 0; --i){
        ptr[i+1] = (word_t)free_ptr | STR_TAG;
        len = strlen(ARGV[i]);
        free_ptr[0] = len << FIXNUM_SHIFT;
        char *dest = (char*)(free_ptr + 1);
        strncpy(dest, ARGV[i], len + 1);

        free_ptr = free_ptr + 1;
        free_ptr = (word_t*)((char*)free_ptr + len + 1);
        free_ptr = (word_t*)align_to_multiple(AVX_ALIGNMENT, (word_t)free_ptr);
    }
}

void init_heap(){
    char *s = getenv("HEAP_SIZE");
    if(s != NULL){
        // sscanf(s, "%d", &HEAP_SIZE); doesn't work for x86_64
        HEAP_SIZE = atoi(s);
        assert(HEAP_SIZE > 0);
        HEAP_SIZE = align_to_multiple(8, HEAP_SIZE);
    }
    else{
        HEAP_SIZE = 0xffff8;    // 15 MB
        // HEAP_SIZE = 0x8ffff8;   // 150 MB
    }
    GC_CELLS = aligned_alloc(AVX_ALIGNMENT, HEAP_SIZE * sizeof(GC_CELL_T));
    free_ptr = aligned_alloc(AVX_ALIGNMENT, HEAP_SIZE * 2 * sizeof(word_t*));
    
    // double estimated_mem_sz = (HEAP_SIZE * (2 * sizeof(word_t*) + sizeof(GC_CELL_T))) / (1024 * 1024);
    // fprintf(stderr, "HEAP_SIZE:%d HEAP_MEM_SIZE: %.2fMB\n", HEAP_SIZE, estimated_mem_sz);
    HEAP_START = free_ptr;
    HEAP_END = HEAP_START + HEAP_SIZE * 2;
    fromspace_start = free_ptr;
    fromspace_end = free_ptr + HEAP_SIZE;
    tospace_start = free_ptr + HEAP_SIZE;
    tospace_end = free_ptr + HEAP_SIZE * 2;
    assert(sizeof(word_t) == sizeof(word_t*));
    assert(sizeof(word_t*) == sizeof(void*));
    assert((word_t)stdin % 8 == 0 && "unaligned stdin");
    assert((word_t)stdout % 8 == 0 && "unaligned stdout");
    assert((word_t)stderr % 8 == 0 && "unaligned stderr");
    assert((word_t)free_ptr % 8 == 0);
    assert((word_t)fromspace_start % 8 == 0);
    assert((word_t)fromspace_end % 8 == 0);
    assert((word_t)tospace_start % 8 == 0);
    assert((word_t)tospace_end % 8 == 0);
    init_cmd_ln();
    return;
}

// FFI
const char* to_cstr(word_t x){
    assert((x & PTR_MASK) == STR_TAG);
    word_t *ptr = (word_t*)(x - STR_TAG);
    char *s = (char*)(ptr+1);
    return s;
}

#include <sys/unistd.h>
word_t s_getpid(){
    return getpid() << FIXNUM_SHIFT;
}

void s_exit(word_t v){
    exit(v >> FIXNUM_SHIFT);
}

word_t s_system(word_t x){
    const char* cmd = to_cstr(x);
    return system(cmd) << FIXNUM_SHIFT;
}

word_t s_cmd_ln(){
    return ARGS;
}

word_t s_strcmp(word_t x, word_t y){
    assert((x & PTR_MASK) == STR_TAG);
    assert((y & PTR_MASK) == STR_TAG);
    word_t* xptr = (word_t*)(x - STR_TAG);
    word_t* yptr = (word_t*)(y - STR_TAG);
    if(xptr[0] != yptr[0]){
        return BOOL_TAG;
    }
    else{
        int res = strncmp((char*)(xptr + 1), (char*)(yptr + 1), xptr[0]);
        return ((res == 0) << IMM_SHIFT) | BOOL_TAG;
    }

}

void s_fwrite(word_t v, FILE *fptr){
    if((v & FIXNUM_MASK) == FIXNUM_TAG){
        fprintf(fptr, "%d", v >> FIXNUM_SHIFT);
    }
    else if((v & IMM_MASK) == CHAR_TAG){
        char ch = v >> IMM_SHIFT;
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
            fprintf(fptr, "#\\%c", ch);
        }
    }
    else if((v & IMM_MASK) == BOOL_TAG){
        if(v >> IMM_SHIFT){
            fprintf(fptr, "#t");
        }else{
            fprintf(fptr, "#f");
        }
    }
    else if((v & IMM_MASK) == NIL_TAG){
        fprintf(fptr, "()");
    }
    else if((v & PTR_MASK) == PAIR_TAG){
        word_t *ptr = (word_t*)(v - PAIR_TAG);
        word_t car = ptr[0];
        word_t cdr = ptr[1];
        
        fprintf(fptr, "(");
        s_fwrite(car, fptr);
        
        while((cdr & PTR_MASK) == PAIR_TAG){
            fprintf(fptr, " ");
            ptr = (word_t*)(cdr - PAIR_TAG);
            car = ptr[0];
            cdr = ptr[1];
            s_fwrite(car, fptr);
        }
        if((cdr & IMM_MASK) == NIL_TAG){
            fprintf(fptr, ")");
        }
        else{
            fprintf(fptr, " . ");
            s_fwrite(cdr, fptr);
            fprintf(fptr, ")");
        }
    }
    else if((v & PTR_MASK) == STR_TAG){
        word_t *ptr = (word_t*)(v - STR_TAG);
        assert((ptr[0] & FIXNUM_MASK) == FIXNUM_TAG);
        int len = ptr[0] >> FIXNUM_SHIFT;
        char *s = (char*)(ptr+1);
        putc('"', fptr);
        for(int i = 0; i < len && s[i] != '\0'; ++i){
            switch (s[i]) {
                case '\n': fprintf(fptr, "\\n"); break;
                case '\t': fprintf(fptr, "\\t"); break;
                case '\r': fprintf(fptr, "\\r"); break;
                case '\"': fprintf(fptr, "\\\""); break;
                case '\\': fprintf(fptr, "\\\\"); break;
                default:
                    // non-printable ASCII
                    if (s[i] < 32 || s[i] > 126){
                        fprintf(fptr, "\\x%02x", (unsigned char)s[i]);
                    }
                    else{
                        putc(s[i], fptr);
                    }
            }
        }
        putc('"', fptr);
    }
    else if((v & PTR_MASK) == SYM_TAG){
        word_t *ptr = (word_t*)(v - SYM_TAG);
        char *s = (char*)(ptr+1);
        fprintf(fptr, "%s", s);
    }
    else if((v & PTR_MASK) == VEC_TAG){
        word_t *ptr = (word_t*)(v - VEC_TAG);
        word_t len = ptr[0] >> FIXNUM_SHIFT;
        ptr = ptr+1;
        fprintf(fptr, "#(");
        for(;len > 0; --len){
            s_fwrite(*ptr++, fptr);
            if(len != 1){
                fprintf(fptr, " ");
            }
        }
        fprintf(fptr, ")");
    }
    else if((v & PTR_MASK) == CLOS_TAG){
        word_t *ptr = (word_t*)(v - CLOS_TAG);
        word_t len = ptr[1] >> FIXNUM_SHIFT;
        fprintf(fptr, "<procedure %x %d>", ptr[0], len);
    }
    else{
        fprintf(stderr, "%s unknown object: %x\n", __FUNCTION__, v);
        assert(false);
    }
}

void s_fwriteln(word_t v, FILE *fptr){
    s_fwrite(v, fptr);
    putc('\n', fptr);
}

word_t s_write(word_t v){
    s_fwrite(v, stdout);
    return 0;
}

word_t s_fnewline(FILE *fptr){
    putc('\n', fptr);
    return 0;
}

word_t s_newline(){
    putc('\n', stdout);
    return 0;
}

void s_proc_arg_err(){
    fprintf(stderr, "bad procedure calling\n");
    exit(1);
}

void s_make_string_size_err(word_t v){
    fprintf(stderr, "make-string expect non-negative fixnum but got: ");
    s_fwriteln(v, stderr);
    exit(1);
}

void s_make_vector_size_err(word_t v){
    fprintf(stderr, "make-vector expect non-negative fixnum but got: ");
    s_fwriteln(v, stderr);
    exit(1);
}

word_t s_stdin(){
    return (word_t)stdin;
}

word_t s_stdout(){
    return (word_t)stdout;
}

word_t s_stderr(){
    return (word_t)stderr;
}

word_t s_fopen(word_t f, word_t mode){
    FILE *fptr = fopen(to_cstr(f), to_cstr(mode));
    assert((word_t)fptr%8 == 0);
    return (word_t)fptr;
}

word_t s_fclose(word_t f){
    fclose((FILE*)f);
    return 0;
}

word_t s_read_char(FILE *fptr){
    return fgetc(fptr) << FIXNUM_SHIFT;
}

word_t s_fwrite_char(word_t ch, FILE *fptr){
    char c = (char)(ch >> IMM_SHIFT);
    putc(c, fptr);
    return 0;
}

word_t s_write_char(word_t ch){
    return s_fwrite_char(ch, stdout);
}

word_t s_ungetc(word_t ch, FILE *fptr){
    char c = (char)(ch >> IMM_SHIFT);
    int v = ungetc(c, fptr);
    return v << FIXNUM_SHIFT;
}

word_t s_make_string(word_t size, word_t ch){
    assert((size & FIXNUM_MASK) == FIXNUM_TAG);
    assert((ch & IMM_MASK) == CHAR_TAG);
    char c = ch >> IMM_SHIFT;
    int k = size >> FIXNUM_SHIFT;
    assert(k >= 0);

    word_t res = (word_t)free_ptr | STR_TAG;
    free_ptr[0] = size;
    char *d = (char*)(free_ptr + 1);
    for(int i = 0; i < k; ++i){
        d[i] = c;
    }
    d[k] = '\0';
    free_ptr = free_ptr + 1;
    free_ptr = (word_t*)((char*)free_ptr + k + 1);
    free_ptr = (word_t*)align_to_multiple(8,(word_t)free_ptr);
    return res;
}

word_t s_make_vector(word_t size){
    assert((size & FIXNUM_MASK) == FIXNUM_TAG);
    int k = size >> FIXNUM_SHIFT;
    assert(k >= 0);

    word_t res = (word_t)free_ptr | VEC_TAG;
    free_ptr[0] = size;
    for(int i = 0; i < k; ++i){
        free_ptr[i+1] = 0;
    }
    free_ptr = (word_t*)align_to_multiple(8,(word_t)(free_ptr + k + 1));
    return res;

}

// GC
bool is_immediate(word_t v){
    return ((v & FIXNUM_MASK) == FIXNUM_TAG) || ((v & PTR_MASK) == IMM_TAG);
}

word_t copy_obj(word_t v){
    if(is_immediate(v)){
        return v;
    }
    word_t *ptr = (word_t*)(v & ~PTR_MASK);
    bool is_heap_ptr = HEAP_START <= ptr && ptr < HEAP_END;
    if(!is_heap_ptr){
        fprintf(stderr, "%s not a heap pointer: %p 0x%lx\n", __FUNCTION__, ptr, v);
        assert(is_heap_ptr);
    }
    bool is_tospace_ptr = tospace_start <= ptr && ptr < tospace_end;
    if(!is_tospace_ptr){
        s_fwriteln(v, stderr);
        assert(is_tospace_ptr);
    }
    assert(free_ptr < scan_ptr);
    assert((word_t)free_ptr % 8 == 0);
    assert(fromspace_start <= free_ptr && free_ptr < fromspace_end);
    int pos = ptr - tospace_start;
    
    if((v & PTR_MASK) == SYM_TAG){
        word_t w = copy_obj(v - SYM_TAG + STR_TAG) - STR_TAG + SYM_TAG;
        return w;
    }
    if(GC_CELLS[pos].is_deleted){
        return GC_CELLS[pos].obj;
    }
    else if((v & PTR_MASK) == PAIR_TAG){
        word_t *w = free_ptr;
        free_ptr += 2;
        w[0] = ptr[0];
        w[1] = ptr[1];

        GC_CELLS[pos].obj = ((word_t)w) | PAIR_TAG;
        GC_CELLS[pos].is_deleted = true;
        *--scan_ptr = GC_CELLS[pos].obj;
        return GC_CELLS[pos].obj;
    }
    else if((v & PTR_MASK) == STR_TAG){
        word_t *ptr = (word_t*)(v - STR_TAG);
        word_t fx_len = ptr[0];
        char *s = (char*)(ptr+1);
        assert((fx_len & FIXNUM_MASK) == FIXNUM_TAG);
        int len = fx_len >> FIXNUM_SHIFT;
        assert(len >= 0 && s[len] == '\0');

        word_t *w = free_ptr;
        char *d = (char*)(free_ptr + 1);

        free_ptr = free_ptr + 1;
        free_ptr = (word_t*)((char*)free_ptr + len + 1);
        free_ptr = (word_t*)align_to_multiple(8,(word_t)free_ptr);

        w[0] = fx_len;

        strncpy(d, s, len+1);

        GC_CELLS[pos].obj = (word_t)w | STR_TAG;
        GC_CELLS[pos].is_deleted = true;
        return GC_CELLS[pos].obj;
    }
    else if((v & PTR_MASK) == SYM_TAG){
        word_t w = copy_obj(v - SYM_TAG + STR_TAG) - STR_TAG + SYM_TAG;
        return w;
    }
    else if((v & PTR_MASK) == VEC_TAG){
        word_t *ptr = (word_t*)(v - VEC_TAG);
        word_t fx_len = ptr[0];
        assert((fx_len & FIXNUM_MASK) == FIXNUM_TAG);
        int len = fx_len >> FIXNUM_SHIFT;

        word_t *w = free_ptr;

        free_ptr = free_ptr + 1 + len;
        free_ptr = (word_t*)align_to_multiple(8,(word_t)free_ptr);

        // to also copy the vector length
        for(int i = 0; i < len+1; ++i){
            w[i] = ptr[i];
        }
        GC_CELLS[pos].obj = (word_t)w | VEC_TAG;
        GC_CELLS[pos].is_deleted = true;
        *--scan_ptr = GC_CELLS[pos].obj;
        return GC_CELLS[pos].obj;
    }
    else if((v & PTR_MASK) == CLOS_TAG){
        word_t *ptr = (word_t*)(v - CLOS_TAG);
        assert((ptr[0] & PTR_MASK) == 0);
        word_t fx_len = ptr[1];
        assert((fx_len  & FIXNUM_MASK) == FIXNUM_TAG);
        int len = fx_len >> FIXNUM_SHIFT;

        word_t *dest = free_ptr;

        free_ptr = free_ptr + 2 + len;
        free_ptr = (word_t*)align_to_multiple(8,(word_t)free_ptr);

        for(int i = 0; i < len + 2; ++i){
            dest[i] = ptr[i];
        }
        GC_CELLS[pos].obj = (word_t)dest | CLOS_TAG;
        GC_CELLS[pos].is_deleted = true;
        *--scan_ptr = GC_CELLS[pos].obj;
        return GC_CELLS[pos].obj;
    }
    else {
        fprintf(stderr, "%s unknown object %p\n", __FUNCTION__, v);
        exit(1);
    }
}

static inline void collect_scan_pointer(){
    while(scan_ptr < fromspace_end){
        word_t v = *(scan_ptr++);
        assert(!is_immediate(v));
        word_t *ptr = (word_t*)(v & ~PTR_MASK);
        if((v & PTR_MASK) == PAIR_TAG){
            ptr[0] = copy_obj(ptr[0]);
            ptr[1] = copy_obj(ptr[1]);
        }
        else if((v & PTR_MASK) == VEC_TAG){
            word_t fx_len = ptr[0];
            assert((fx_len  & FIXNUM_MASK) == FIXNUM_TAG);
            int len = fx_len >> FIXNUM_SHIFT;
            for(int i = 0; i < len; ++i){
                ptr[i+1] = copy_obj(ptr[i+1]);
            }
        }
        else if((v & PTR_MASK) == CLOS_TAG){
            assert((ptr[0] & PTR_MASK) == 0);
            word_t fx_len = ptr[1];
            assert((fx_len  & FIXNUM_MASK) == FIXNUM_TAG);
            int len = fx_len >> FIXNUM_SHIFT;
            for(int i = 0; i < len; ++i){
                ptr[i+2] = copy_obj(ptr[i+2]);
            }
        }
        else if((v & PTR_MASK) == STR_TAG){
            assert((v & PTR_MASK) != STR_TAG);
        }
        else if((v & PTR_MASK) == SYM_TAG){
            assert((v & PTR_MASK) != SYM_TAG);
        }
        else{
            fprintf(stderr, "%s unknown object %p\n", __FUNCTION__, v);
            assert(false);
        }
    }
}

static inline word_t _max(word_t x, word_t y){
    return x < y ? y : x;
}

word_t gc_flip(word_t fx_size, word_t *esp, word_t *ebp){
    assert((fx_size & FIXNUM_MASK) == FIXNUM_TAG);
    int size = fx_size >> FIXNUM_SHIFT;
    assert(size == -1 || (size >= 0 && size % 8 == 0));
    int free_sz = fromspace_end - free_ptr;
    int min_size = 64;
    bool flip_condition;
    flip_condition = size < 0;
    size = size > 0 ? _max(size / 8, min_size) : min_size;
    flip_condition = flip_condition || (free_sz < size);
    if(!flip_condition){
        return 0;
    }
    word_t *tmp;
    tmp = fromspace_start;
    fromspace_start = tospace_start;
    tospace_start = tmp;
    tmp = fromspace_end;
    fromspace_end = tospace_end;
    tospace_end = tmp;

    free_ptr = fromspace_start;
    scan_ptr = fromspace_end;

    memset(GC_CELLS, 0, HEAP_SIZE * sizeof(GC_CELLS[0]));

    ARGS = copy_obj(ARGS);
    for(int i = 0; global_table[i] != 0; ++i) {
        *global_table[i] = copy_obj(*global_table[i]);
    }
    collect_scan_pointer();

    for(int i = 0; esp + i < BOT_EBP;){
        if(esp + i == ebp){
            ebp = (word_t*)esp[i];
            i += 2;
        } else {
            esp[i] = copy_obj(esp[i]);
            collect_scan_pointer();
            i += 1;
        }
    }
    free_sz = fromspace_end - free_ptr;
    // ++FLIP_COUNT;
    // fprintf(stderr, "flip: %d TIMES %d KB \n", FLIP_COUNT, free_sz * sizeof(word_t) / 1024);
    if(free_sz <= size){
        fprintf(stderr, "HEAP FULL! free=%d requested_size=%d\n", free_sz, size);
        exit(1);
    }
    return 0;
}

word_t construct_vararg(word_t target_argc, word_t argc, word_t *esp, word_t *ebp){
    assert((argc & FIXNUM_MASK) == FIXNUM_TAG);
    word_t size = argc >> FIXNUM_SHIFT;
    assert(size == ebp - esp);
    assert(target_argc - 1 <= size);
    gc_flip(((size - target_argc + 1) * 2 * sizeof(word_t) * 8) << FIXNUM_SHIFT, esp, ebp);
    word_t res = NIL_TAG;
    for(word_t i = 0; size - i >= target_argc; ++i){
        free_ptr[0] = esp[i];
        free_ptr[1] = res;
        res = (word_t)free_ptr | PAIR_TAG;
        free_ptr += 2;
    }
    return res;
}
