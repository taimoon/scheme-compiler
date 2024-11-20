#include<stdlib.h>
#include<stdio.h>
#include<stdint.h>
#include<assert.h>
#include<stdbool.h>
#include<string.h>
#define FIXNUM_MASK     0b00000011
#define FIXNUM_TAG      0b00000000
#define FIXNUM_SHIFT    2
#define IMM_SHIFT       8
#define IMM_TAG         0b00000111
#define IMM_MASK        0b11111111
#define CHAR_TAG        0b00001111
#define BOOL_TAG        0b00011111
#define NIL_TAG         0b01011111
#define PTR_MASK        0b00000111
#define PAIR_TAG        0b00000001
#define VEC_TAG         0b00000010
#define STR_TAG         0b00000011
#define SYM_TAG         0b00000101
#define CLOS_TAG        0b00000110
// bootstrapping compiler need hefty amount of memory
#define HEAP_SIZE       0x1fff8
#define GLOBAL_SIZE     0x1ff

int *fromspace_start, *fromspace_end, *tospace_start, *tospace_end, *scan_ptr;
int *free_ptr;
int ARGC;
char **ARGV;
int *_GLOBALS[GLOBAL_SIZE] = {0};
int **GLOBALS;
struct {
    int obj;
    bool is_deleted;
} GC_CELLS[HEAP_SIZE];
int *BOT_EBP;

inline static int align_to_multiple(int alignment, int offset){
    return (offset + (alignment - 1)) & -alignment;
}

int raw_print(int* v){
    fprintf(stdout, "%p\n", v);
    return 0;
}

void init_heap(){
    free_ptr = aligned_alloc(8, HEAP_SIZE * 2 * sizeof(int*));
    GLOBALS = _GLOBALS;
    fromspace_start = free_ptr;
    fromspace_end = free_ptr + HEAP_SIZE;
    tospace_start = free_ptr + HEAP_SIZE;
    tospace_end = free_ptr + HEAP_SIZE * 2;
    assert((int)free_ptr % 8 == 0);
    assert((int)fromspace_start % 8 == 0);
    assert((int)fromspace_end % 8 == 0);
    assert((int)tospace_start % 8 == 0);
    assert((int)tospace_end % 8 == 0);
    return;
}

const char* to_cstr(int x){
    assert((x & PTR_MASK) == STR_TAG);
    int *ptr = (int*)(x - STR_TAG);
    char *s = (char*)(ptr+1);
    return s;
}

void s_proc_arg_err(){
    fprintf(stderr, "bad procedure calling\n");
    exit(1);
}

#include <sys/unistd.h>
int s_getpid(){
    return getpid() << FIXNUM_SHIFT;
}

void s_exit(int v){
    exit(v >> FIXNUM_SHIFT);
}

int s_system(int x){
    const char* cmd = to_cstr(x);
    return system(cmd) << FIXNUM_SHIFT;
}

int s_init_cmd_ln(){
    int *ptr, len;
    int acm = NIL_TAG;
    for(int i = ARGC-1; i >= 0; --i){
        ptr = free_ptr;
        free_ptr += 2;

        ptr[1] = acm;
        ptr[0] = (int)free_ptr | STR_TAG;
        acm = (int)ptr | PAIR_TAG;
        
        len = strlen(ARGV[i]);

        ptr = free_ptr;
        free_ptr = free_ptr + 1;
        free_ptr = (int*)((char*)free_ptr + len + 1);
        free_ptr = (int*)align_to_multiple(8, (int)free_ptr);
        
        ptr[0] = len << FIXNUM_SHIFT;
        char *dest = (char*)(ptr + 1);
        strncpy(dest, ARGV[i], len);
    }
    return acm;
}

void s_write(int v, FILE *fptr){
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
        int *ptr = (int*)(v - PAIR_TAG);
        int car = ptr[0];
        int cdr = ptr[1];
        
        fprintf(fptr, "(");
        s_write(car,fptr);
        
        while((cdr & PTR_MASK) == PAIR_TAG){
            fprintf(fptr, " ");
            ptr = (int*)(cdr - PAIR_TAG);
            car = ptr[0];
            cdr = ptr[1];
            s_write(car,fptr);
        }
        if((cdr & IMM_MASK) == NIL_TAG){
            fprintf(fptr, ")");
        }
        else{
            fprintf(fptr, " . ");
            s_write(cdr,fptr);
            fprintf(fptr, ")");
        }
    }
    else if((v & PTR_MASK) == STR_TAG){
        int *ptr = (int*)(v - STR_TAG);
        char *s = (char*)(ptr+1);
        fprintf(fptr, "\"%s\"", s);
        return;
    }
    else if((v & PTR_MASK) == SYM_TAG){
        int *ptr = (int*)(v - SYM_TAG);
        char *s = (char*)(ptr+1);
        fprintf(fptr, "%s", s);
        return;
    }
    else if((v & PTR_MASK) == VEC_TAG){
        int *ptr = (int*)(v - VEC_TAG);
        int len = ptr[0] >> FIXNUM_SHIFT;
        ptr = ptr+1;
        fprintf(fptr, "#(");
        for(;len > 0; --len){
            s_write(*ptr++, fptr);
            if(len != 1){
                fprintf(fptr, " ");
            }
        }
        fprintf(fptr, ")");
        return;
    }
    else if((v & PTR_MASK) == CLOS_TAG){
        int *ptr = (int*)(v - CLOS_TAG);
        int len = ptr[1] >> FIXNUM_SHIFT;
        fprintf(fptr, "<procedure %x %d>", ptr[0], len);
    }
    else{
        fprintf(stderr, "unknown object: %x\n", v);
        exit(1);
    }
}

void newline(){
    putc('\n', stdout);
}

int s_stdout(){
    if((int)stdout%8 != 0){
        fprintf(stderr, "unaligned stdout %p\n", stdout);
        exit(1);
    }
    return (int)stdout;
}

int s_fopen(int f, int mode){
    FILE *fptr = fopen(to_cstr(f),to_cstr(mode));
    assert((int)fptr%8 == 0);
    return (int)fptr;
}

int s_fclose(int f){
    fclose((FILE*)f);
    return 0;
}

int s_read_char(FILE *fptr){
    int res = fgetc(fptr);
    return res << FIXNUM_SHIFT;
}

int s_write_char(int ch, FILE *fptr){
    char c = (char)(ch >> IMM_SHIFT);
    putc(c, fptr);
    return 0;
}

int s_ungetc(int ch, FILE *fptr){
    char c = (char)(ch >> IMM_SHIFT);
    int v = ungetc(c, fptr);
    return v << FIXNUM_SHIFT;
}

// GC
bool is_immediate(int v){
    return ((v & FIXNUM_MASK) == FIXNUM_TAG) || ((v & PTR_MASK) == IMM_TAG);
}

int copy_obj(int v){
    if(is_immediate(v)){
        return v;
    }
    int *ptr = (int*)(v & ~PTR_MASK);
    if(!(tospace_start <= ptr && ptr < tospace_end)){
        s_write(v, stderr);
        newline();
    }
    assert((int)free_ptr % 8 == 0);
    assert(tospace_start <= ptr);
    assert(ptr < tospace_end);
    int pos = ptr - tospace_start;
    
    if(GC_CELLS[pos].is_deleted && ((v & PTR_MASK) != SYM_TAG)){
        return GC_CELLS[pos].obj;
    }
    else if((v & PTR_MASK) == PAIR_TAG){
        int *w = free_ptr;
        free_ptr += 2;
        w[0] = ptr[0];
        w[1] = ptr[1];

        GC_CELLS[pos].obj = ((int)w) | PAIR_TAG;
        GC_CELLS[pos].is_deleted = true;
        *--scan_ptr = GC_CELLS[pos].obj;
        return GC_CELLS[pos].obj;
    }
    else if((v & PTR_MASK) == STR_TAG){
        int *ptr = (int*)(v - STR_TAG);
        int fx_len = ptr[0];
        char *s = (char*)(ptr+1);
        assert((fx_len  & FIXNUM_MASK) == FIXNUM_TAG);
        int len = fx_len >> FIXNUM_SHIFT;
        assert(len >= 0);
        assert(s[len] == '\0');

        int *w = free_ptr;
        char *d = (char*)(free_ptr + 1);

        free_ptr = free_ptr + 1;
        free_ptr = (int*)((char*)free_ptr + len + 1);
        free_ptr = (int*)align_to_multiple(8,(int)free_ptr);

        w[0] = fx_len;

        strncpy(d,s,len+1);

        GC_CELLS[pos].obj = (int)w | STR_TAG;
        GC_CELLS[pos].is_deleted = true;
        return GC_CELLS[pos].obj;
    }
    else if((v & PTR_MASK) == SYM_TAG){
        int w = copy_obj(v - SYM_TAG + STR_TAG) - STR_TAG + SYM_TAG;
        return w;
    }
    else if((v & PTR_MASK) == VEC_TAG){
        int *ptr = (int*)(v - VEC_TAG);
        int fx_len = ptr[0];
        assert((fx_len  & FIXNUM_MASK) == FIXNUM_TAG);
        int len = fx_len >> FIXNUM_SHIFT;

        int *w = free_ptr;

        free_ptr = free_ptr + 1 + len;
        free_ptr = (int*)align_to_multiple(8,(int)free_ptr);

        int *dest = w;
        int *src = ptr;

        // to also copy the vector length
        for(int i = 0; i < len+1; ++i){
            *dest = *src;
            ++dest; ++src;
        }
        GC_CELLS[pos].obj = (int)w | VEC_TAG;
        GC_CELLS[pos].is_deleted = true;
        *--scan_ptr = GC_CELLS[pos].obj;
        return GC_CELLS[pos].obj;
    }
    else if((v & PTR_MASK) == CLOS_TAG){
        int *ptr = (int*)(v - CLOS_TAG);
        int fx_len = ptr[1];
        assert((fx_len  & FIXNUM_MASK) == FIXNUM_TAG);
        int len = fx_len >> FIXNUM_SHIFT;

        int *w = free_ptr;

        free_ptr = free_ptr + 2 + len;
        free_ptr = (int*)align_to_multiple(8,(int)free_ptr);

        int *dest = w;
        int *src = ptr;

        for(int i = 0; i < len + 2; ++i){
            *dest = *src;
            ++dest; ++src;
        }
        GC_CELLS[pos].obj = (int)w | CLOS_TAG;
        GC_CELLS[pos].is_deleted = true;
        *--scan_ptr = GC_CELLS[pos].obj;
        return GC_CELLS[pos].obj;
    }
    else {
        fprintf(stderr, "unknown object: %x\n", v);
        exit(1);
    }
}

void collect_scan_pointer(){
    while(scan_ptr < fromspace_end){
        int v = *scan_ptr;
        ++scan_ptr;
        assert(!is_immediate(v));
        int *ptr = (int*)(v & ~PTR_MASK);
        if((v & PTR_MASK) == PAIR_TAG){
            ptr[0] = copy_obj(ptr[0]);
            ptr[1] = copy_obj(ptr[1]);
        }
        else if((v & PTR_MASK) == VEC_TAG){
            int fx_len = ptr[0];
            assert((fx_len  & FIXNUM_MASK) == FIXNUM_TAG);
            int len = fx_len >> FIXNUM_SHIFT;
            for(int i = 0; i < len; ++i){
                ptr[i+1] = copy_obj(ptr[i+1]);
            }
        }
        else if((v & PTR_MASK) == CLOS_TAG){
            int fx_len = ptr[1];
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
            fprintf(stderr, "unknown object %p\n", v);
            assert(false);
        }
    }
}

int walk_n_print_stack(int fx_size, int *esp, int *ebp){
    printf("=========\n");
    printf("BOT=%p\n", BOT_EBP);
    printf("GLOBALS=%p GLOBALS_N=%d\n", GLOBALS, GLOBALS - _GLOBALS);
    assert(esp <= ebp);
    assert(ebp <= BOT_EBP);
    printf("esp=%p ebp=%p sz=%lu\n", esp, ebp, BOT_EBP - esp);
    printf("*esp=%p *ebp=%p\n", *esp, *ebp);
    printf("---------\n");
    printf("GLOBALS\n");
    for(int i = 0; _GLOBALS + i < GLOBALS; ++i){
        s_write(*_GLOBALS[i], stdout);
        newline();
    }
    printf("---------\n");
    printf("---------\n");
    printf("FRAMES\n");
    int frame_cnt = 0;
    for(int i = 0; esp + i < BOT_EBP;){
        printf("*(esp+%d) = *%p = %p\t", i, esp + i, esp[i]);
        if(esp + i == ebp){
            frame_cnt += 1;
            ebp = (int*)esp[i];
            i += 2;
        } else {
            s_write(esp[i], stdout);
            i += 1;
        }
        newline();
    }
    printf("frame count = %d\n", frame_cnt);
    printf("---------\n");
    return 0;
}

int gc_flip(int fx_size, int *esp, int *ebp){
    assert((fx_size & FIXNUM_MASK) == FIXNUM_TAG);
    int size = fx_size >> FIXNUM_SHIFT;
    size = size < GLOBAL_SIZE ? GLOBAL_SIZE : size; 
    int free_sz = fromspace_end - free_ptr;
    if(free_sz > size){
        return 0;
    }

    int *tmp;
    tmp = fromspace_start;
    fromspace_start = tospace_start;
    tospace_start = tmp;
    tmp = fromspace_end;
    fromspace_end = tospace_end;
    tospace_end = tmp;

    free_ptr = fromspace_start;
    scan_ptr = fromspace_end;
    
    for(int i = 0; i < HEAP_SIZE; ++i){
        GC_CELLS[i].is_deleted = false;
        GC_CELLS[i].obj = 0;
    }
    
    
    for(int i = 0; _GLOBALS + i < GLOBALS; ++i){
        *_GLOBALS[i] = copy_obj(*_GLOBALS[i]);
    }
    
    
    for(int i = 0; esp + i < BOT_EBP;){
        if(esp + i == ebp){
            ebp = (int*)esp[i];
            i += 2;
        } else {
            esp[i] = copy_obj(esp[i]);
            i += 1;
        }
    }
    collect_scan_pointer();


    free_sz = fromspace_end - free_ptr;
    if(free_sz <= size){
        fprintf(stderr, "HEAP FULL! free=%llu requested_size=%llu", free_sz, size);
        exit(1);
    }
    return 0;
}

int walk_stack(int fx_size, int *esp, int *ebp){
    // walk_n_print_stack(fx_size, esp, ebp);
    gc_flip(fx_size, esp, ebp);
}
