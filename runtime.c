#define HEAP_SIZE       0xffff8
#define CONSTANTS_SIZE  512
#define FIXNUM_MASK     3
#define FIXNUM_TAG      0
#define FIXNUM_SHIFT    2

#define CHAR_MASK       0xff
#define CHAR_SHIFT      8
#define CHAR_TAG        7

#define BOOL_MASK       0xff
#define BOOL_SHIFT      8
#define BOOL_TAG        15

#define PTR_MASK        7
#define PAIR_TAG        1
#define VEC_TAG         2
#define STR_TAG         3
#define SYM_TAG         5
#define CLOSURE_TAG     6

#define newline() putchar('\n')

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <time.h>

typedef struct {
    bool is_deleted;
    int x;
} GC_CELL;

int *HEAP;
GC_CELL *GC_CELLS;
int *_CONSTANTS[CONSTANTS_SIZE];
int **CONSTANTS;
int *free_ptr,*scan_ptr;
int *fromspace_start,*fromspace_end,*tospace_start,*tospace_end;
int *ESP,*EBP;
int *PREV_EBP;
int *TOP_EBP;
int ARGC;
const char **ARGV;

void initialize();
const char* cast_obj_to_str(int s);
int cast_obj_to_int(int x);
char cast_obj_to_char(int x);
// foreign-functions
__attribute__((__cdecl__))
extern int scheme_entry();
// writer
__attribute__((__cdecl__))
int s_display(int x, int fptr);
__attribute__((__cdecl__))
int _s_display(int x, FILE *fptr);
__attribute__((__cdecl__))
int show(int x, FILE *fptr);
__attribute__((__cdecl__))
int s_fshow(int x, int _fptr);
__attribute__((__cdecl__))
int s_show(int x);
__attribute__((__cdecl__))
int s_fnewline(int _fptr);
__attribute__((__cdecl__))
int s_newline();
// system
int make_str(const char *src);
int make_pair(int a, int b);
__attribute__((__cdecl__))
int s_cmd_ln();
__attribute__((__cdecl__))
int s_system(int s);
__attribute__((__cdecl__))
void s_exit();
__attribute__((__cdecl__))
int s_getpid();
__attribute__((__cdecl__))
void s_function_error(int x);
// math
__attribute__((__cdecl__))
int s_div(int x, int d);
__attribute__((__cdecl__))
int s_mod(int x, int d);
// io
void check_sfptr(int fptr);
__attribute__((__cdecl__))
int s_fopen(int _path, int _mode);
__attribute__((__cdecl__))
int s_fclose(int fptr) ;
__attribute__((__cdecl__))
int s_fputc(int c, int fptr);
__attribute__((__cdecl__))
int s_fputs(int s, int fptr);
__attribute__((__cdecl__))
int s_fgetc(int fptr);
__attribute__((__cdecl__))
int s_ungetc(int c, int fptr);
__attribute__((__cdecl__))
int s_stdout();
__attribute__((__cdecl__))
int s_stdin();
__attribute__((__cdecl__))
int s_stderr();
__attribute__((__cdecl__))
int s_fseek(int fptr, int pos);

// garbage collector
#define BYTE_DIFF(a,b) (int)((char*)a - (char*)b)
inline static int align_to_multiple(int alignment, int offset);
inline static bool is_immediate(int x);
int copy_obj(int x);
int s_gc_visit_n_print();
__attribute__((__cdecl__))
int s_gc_flip(int byte_size);

inline static int align_to_multiple(int alignment, int offset){
    return (offset + (alignment - 1)) & -alignment;
}

inline static bool is_immediate(int x){
    return ((x & FIXNUM_MASK) == FIXNUM_TAG)
    || ((x & CHAR_MASK) == CHAR_TAG)
    || ((x & BOOL_MASK) == BOOL_TAG);
}

int copy_obj(int x){
    if(is_immediate(x)){
        return x;
    }
    int *ptr = (int*)(x & ~PTR_MASK);
    if(ptr == NULL){
        return x;
    }
    if((fromspace_start <= ptr) && (ptr < fromspace_end)){
        s_show(x);newline();
        assert(false);
    }
    if(!((tospace_start <= ptr) && (ptr < tospace_end))){
        assert(false);
    }
    assert(tospace_start <= ptr);
    assert(ptr < tospace_end);
    
    int pos = (int)(ptr - tospace_start);
    assert((int)free_ptr%8 == 0);
    
    if((x & PTR_MASK) == PAIR_TAG){
        if(GC_CELLS[pos].is_deleted == true){
            return GC_CELLS[pos].x;
        }
        GC_CELLS[pos].is_deleted = true;
        int* ptr = (int*)(x - PAIR_TAG);
        
        int *y = free_ptr;
        free_ptr = free_ptr + 2;
        y[0] = ptr[0];
        y[1] = ptr[1];

        GC_CELLS[pos].x = (int) y | PAIR_TAG;
        *--scan_ptr = GC_CELLS[pos].x;
        return GC_CELLS[pos].x;
    }else if((x & PTR_MASK) == STR_TAG){
        if(GC_CELLS[pos].is_deleted == true){
            return GC_CELLS[pos].x;
        }
        GC_CELLS[pos].is_deleted = true;
        
        int *ptr = (int*)(x - STR_TAG);
        int len = *ptr;
        char *body = (char*)(ptr+1);

        assert(len >= 0);
        /*
        if(body[len] != '\0'){
            printf("%.*s\n", len, body);
            printf("heap size = %d, free heap = %d, used heap= %d \n", HEAP_SIZE, fromspace_end - free_ptr, free_ptr - fromspace_start);
            assert(body[len] == '\0');
        }
        */
        
        int *y = free_ptr;
        char *dest = (char*)(y+1);
        *y = len;

        free_ptr = free_ptr+1;
        free_ptr = (int*)((char*)free_ptr + len + 1);
        free_ptr = (int*)align_to_multiple(8,(int)free_ptr);

        for(;len > 0;len--){
            *dest = *body;
            ++dest;
            ++body;
        }
        *dest = '\0';
        GC_CELLS[pos].x = (int)y | STR_TAG;
        return GC_CELLS[pos].x;
    }else if((x & PTR_MASK) == VEC_TAG){
        if(GC_CELLS[pos].is_deleted == true){
            return GC_CELLS[pos].x;
        }
        GC_CELLS[pos].is_deleted = true;

        int *ptr = (int*)(x - VEC_TAG);
        int len = *ptr++;
        int *y = free_ptr;

        free_ptr = free_ptr+1+len;
        free_ptr = (int*)align_to_multiple(8,(int)free_ptr);
        
        *y = len;
        int *dest = y+1;
        assert(len >= 0);
        for(;len > 0;len--){
            *dest = *ptr;
            ++ptr;
            ++dest;
        }
        GC_CELLS[pos].x = (int)y | VEC_TAG;
        *--scan_ptr = GC_CELLS[pos].x;
        return GC_CELLS[pos].x;
    }else if((x & PTR_MASK) == SYM_TAG){
        x = (x & ~PTR_MASK) | STR_TAG;
        x = copy_obj(x);
        x = (x & ~PTR_MASK) | SYM_TAG;
        return x;
    }else if((x & PTR_MASK) == CLOSURE_TAG){
        if(GC_CELLS[pos].is_deleted == true){
            return GC_CELLS[pos].x;
        }
        GC_CELLS[pos].is_deleted = true;
        int* ptr = (int*)(x - CLOSURE_TAG);
        int label = *ptr;
        int len = *(ptr+1);

        int *y = free_ptr;
        
        free_ptr = free_ptr+len+2;
        free_ptr = (int*)align_to_multiple(8,(int)free_ptr);
        
        y[0] = label;
        y[1] = len;

        int *dest= y+2;
        ptr = ptr+2;
        for(;len > 0;len--){
            *dest = *ptr;
            ++dest;
            ++ptr;
        }
        GC_CELLS[pos].x = (int)y | CLOSURE_TAG;
        *--scan_ptr = GC_CELLS[pos].x;
        return GC_CELLS[pos].x;
    }else{
        printf("#<unknown>");
        exit(0);
    }
    return 0;
}

int s_gc_visit_n_print(){
    int *esp = ESP;
    int *ebp = EBP;
    int sz = BYTE_DIFF(esp,ebp);
    printf("PREV_EBP = %x, TOP_EBP - PREV_EBP = %d\n", PREV_EBP, BYTE_DIFF(TOP_EBP,PREV_EBP));

    // ebp, esi, edi, edx
    assert(BYTE_DIFF(TOP_EBP,PREV_EBP) == -(4*8));
    
    printf("TOP_EBP = %x, esp = %x, ebp = %x, byte_diff(esp - ebp) = %d\n", TOP_EBP, esp, ebp, sz);
    printf("byte_diff(esp, TOP_EBP) =  %d\n", BYTE_DIFF(esp,TOP_EBP));
    for(int **ptr = _CONSTANTS; ptr < CONSTANTS; ++ptr){
        printf("%x %x\n",*ptr,**ptr);
        int *_ptr = (int*)(**ptr & ~PTR_MASK);
        assert(_ptr < fromspace_end);
        assert(fromspace_start <= _ptr);
    }
    printf("------------------\n");
    while(ebp <= TOP_EBP){
        for(esp; esp != ebp ; esp++){
            if(!is_immediate(*esp)){
                int* ptr = (int*)(*esp & ~PTR_MASK);
                if(ptr != NULL){
                    
                    printf("%x %x %x\n",HEAP, HEAP + HEAP_SIZE * 2, ptr);
                    assert(HEAP <= ptr);
                    assert(ptr < HEAP + HEAP_SIZE * 2);
                    assert(fromspace_start <= ptr);
                    assert(ptr < fromspace_end);
                }
            }
            printf("esp = %x; ", esp);s_show(*esp);newline();
        }
        printf("ebp = %x\n", ebp);
        ebp = (int*)*ebp;
        esp = esp + 2;
        printf("------------------\n");
    }
    return 0;
}

void collect_scan_ptr(){
    while(scan_ptr < fromspace_end){
        assert(!(free_ptr >= scan_ptr));
        int x = *scan_ptr;
        ++scan_ptr;
        assert(!is_immediate(x));
        if((x & ~PTR_MASK) == 0){
            continue;
        }
        if((x & PTR_MASK) == PAIR_TAG){
            int* ptr = (int*)(x - PAIR_TAG);
            ptr[0] = copy_obj(ptr[0]);
            ptr[1] = copy_obj(ptr[1]);
        }else if((x & PTR_MASK) == STR_TAG){
            assert(false);
        }else if((x & PTR_MASK) == VEC_TAG){
            int* ptr = (int*)(x - VEC_TAG);
            int len = *ptr++;
            for(;len > 0; len--, ptr++){
                *ptr = copy_obj(*ptr);
            }
        }else if((x & PTR_MASK) == SYM_TAG){
            assert(false);
        }else if((x & PTR_MASK) == CLOSURE_TAG){
            int* ptr = (int*)(x - CLOSURE_TAG);
            int label = *ptr;
            int len = *(ptr+1);
            ptr = ptr + 2;
            for(;len > 0;len--,ptr++){
                *ptr = copy_obj(*ptr);
            }
        }else{
            printf("#<unknown>");
            assert(false);
        }
    }
}

__attribute__((__cdecl__))
int s_gc_flip(int s_byte_size){
    int byte_size = cast_obj_to_int(s_byte_size);
    assert(byte_size == -1 || byte_size > 0);
    if(byte_size > 0){
        byte_size = align_to_multiple(8,byte_size);
        byte_size = byte_size/4 + 1;
        if(fromspace_end - free_ptr > byte_size && (fromspace_end - free_ptr) > 1024){
            return 0;
        }
    }
    assert(BYTE_DIFF(TOP_EBP,PREV_EBP) == -(4*8));
    for(int i = 0; i < HEAP_SIZE; i++){
        GC_CELLS[i] = (GC_CELL){.is_deleted = false, .x = 0};
        tospace_start[i] = 0;
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
    assert((int)free_ptr%8 == 0);
    int *esp = ESP;
    int *ebp = EBP;
    
    while(ebp <= TOP_EBP){
        for(esp; esp != ebp ; esp++){
            *esp = copy_obj(*esp);
        }
        ebp = (int*)*ebp;
        esp = esp + 2;
    }

    for(int **ptr = _CONSTANTS; ptr < CONSTANTS; ++ptr){
        **ptr = copy_obj(**ptr);
        int *_ptr = (int*)(**ptr & ~PTR_MASK);
        if(_ptr != NULL){
            assert(_ptr < fromspace_end);
            assert(fromspace_start <= _ptr);
        }
    }
    collect_scan_ptr();

    if(byte_size > 0 && !(fromspace_end - free_ptr > byte_size)){
        printf("out of heap: requested = %d bytes, free = %d bytes\n", byte_size, fromspace_end - free_ptr);
        assert(false);
    }
    for(int i = 0; i < HEAP_SIZE; i++){
        tospace_start[i] = 0;
    }
    assert((int)free_ptr%8==0);
    return 0;
}

void initialize(){
    CONSTANTS = _CONSTANTS;
    assert(HEAP_SIZE%8 == 0);
    HEAP = aligned_alloc(8,HEAP_SIZE * 2 * sizeof(int));
    GC_CELLS = calloc(HEAP_SIZE, sizeof(GC_CELL));

    fromspace_start = HEAP;
    fromspace_end = HEAP + HEAP_SIZE;
    tospace_start = fromspace_end;
    tospace_end = tospace_start + HEAP_SIZE;

    assert((int)HEAP%8 == 0);
    assert((int)fromspace_start%8 == 0);
    assert((int)fromspace_end%8 == 0);
    assert((int)tospace_start%8 == 0);
    assert((int)tospace_end%8 == 0);
    assert(tospace_end - tospace_start == HEAP_SIZE);
    assert(fromspace_end - fromspace_start == HEAP_SIZE);
    
    free_ptr = fromspace_start;
}

const char* cast_obj_to_str(int x){
    if((x & PTR_MASK) == STR_TAG){
        int* ptr = (int*)(x - STR_TAG);
        int len = *ptr;
        char *s = (char*)(ptr+1);
        assert(len >= 0);
        if(s[len] != '\0'){
            printf("%.*s", len, s);
            assert(s[len] == '\0');
        }
        return s;
    }else{
        printf("not a string\n");
        s_show(x);
        putchar('\n');
        exit(1);
    }
}

int cast_obj_to_int(int x){
    if((x & FIXNUM_MASK) == FIXNUM_TAG){
        return x >> FIXNUM_SHIFT;
    }else{
        printf("not a integer\n");
        s_show(x);
        putchar('\n');
        exit(1);
    }
}

char cast_obj_to_char(int x){
    if((x & CHAR_MASK) == CHAR_TAG){
        char ch = (char)(x >> CHAR_SHIFT);
    }else{
        printf("not a char\n");
        s_show(x);
        putchar('\n');
        exit(1);
    }
}

// ffi
int s_display(int x, int fptr){
    check_sfptr(fptr);
    _s_display(x,(FILE*)fptr);
}

int _s_display(int x, FILE *fptr){
    if((x & FIXNUM_MASK) == FIXNUM_TAG){
        // integer
        fprintf(fptr, "%d", x >> FIXNUM_SHIFT);
    }else if((x & CHAR_MASK) == CHAR_TAG){
        // character
        char ch = (char)(x >> CHAR_SHIFT);
        if(ch == 0){
            fprintf(fptr, "#\\nul");
        }else if(ch == '\n'){
            fprintf(fptr, "#\\newline");
        }else if(ch == ' '){
            fprintf(fptr, "#\\space");
        }else{
            fprintf(fptr, "#\\%c", ch);
        }
    }else if((x & BOOL_MASK) == BOOL_TAG){
        // boolean
        if((x >> BOOL_SHIFT) != 0) {
            fprintf(fptr, "#t");
        } else {
            fprintf(fptr, "#f");
        }
    }else if((x & PTR_MASK) == PAIR_TAG){
        int* ptr = (int*)(x - PAIR_TAG);
        if(ptr == NULL){
            fprintf(fptr, "()");
            return 0;
        }

        // either a list or a dotted pair
        int car = ptr[0];
        int cdr = ptr[1];
        fputc('(', fptr);
        _s_display(car,fptr);

        // show additional space-separated elems
        while((cdr & PTR_MASK) == PAIR_TAG){
            ptr = (int*)(cdr - PAIR_TAG);
            if(ptr == NULL) break;

            car = ptr[0];
            cdr = ptr[1];
            fputc(' ', fptr);
            _s_display(car,fptr);
        }

        // show dotted pair notation if relevant
        if((cdr & PTR_MASK) != PAIR_TAG){
            fprintf(fptr, " . ");
            _s_display(cdr,fptr);
        }
        fputc(')', fptr);
    }else if((x & PTR_MASK) == STR_TAG){
        int* ptr = (int*)(x - STR_TAG);
        int len = *ptr;
        char* body = (char*)(ptr+1);
        
        assert(len >= 0);
        // if(body[len] != '\0'){
        //     printf("%.*s\n", body);
        //     assert(body[len] == '\0');
        // }
        
        for(;len > 0;len--,body++){
            fputc(*body, fptr);
        }
    }else if((x & PTR_MASK) == VEC_TAG){
        int* ptr = (int*)(x - VEC_TAG);
        int len = *ptr++;

        fprintf(fptr, "#(");
        for(;len > 0;len--) {
            _s_display(*ptr++,fptr);
            if(len != 1) fputc(' ', fptr);
        }
        fprintf(fptr, ")");
    }else if((x & PTR_MASK) == SYM_TAG){
        int* ptr = (int*)(x - SYM_TAG);
        int len = *ptr;
        char* body = (char*)(ptr+1);
        for(;len > 0;len--) fputc(*body++, fptr);
    }else if((x & PTR_MASK) == CLOSURE_TAG){
        int* ptr = (int*)(x - CLOSURE_TAG);;
        int label = *ptr;
        int len = *(ptr+1);
        fprintf(fptr, "#<procedure label=%x len=%x>", label, len);
    }else{
        fprintf(fptr, "#<unknown>");
    }
    return 0;
}

int s_fnewline(int _fptr){
    check_sfptr(_fptr);
    putc('\n',(FILE*)_fptr);
    return 0;
}

int s_newline(){
    putchar('\n');
    return 0;
}

int s_show(int x){
    return show(x, stdout);
}

int s_fshow(int x, int _fptr){
    check_sfptr(_fptr);
    return show(x,(FILE*)_fptr);
}

int show(int x, FILE *fptr){
    if((x & FIXNUM_MASK) == FIXNUM_TAG){
        // integer
        fprintf(fptr, "%d", x >> FIXNUM_SHIFT);
    }else if((x & CHAR_MASK) == CHAR_TAG){
        // character
        char ch = (char)(x >> CHAR_SHIFT);
        if(ch == 0){
            fprintf(fptr, "#\\nul");
        }else if(ch == '\n'){
            fprintf(fptr, "#\\newline");
        }else if(ch == ' '){
            fprintf(fptr, "#\\space");
        }else{
            fprintf(fptr, "#\\%c", ch);
        }
    }else if((x & BOOL_MASK) == BOOL_TAG){
        // boolean
        if((x >> BOOL_SHIFT) != 0) {
            fprintf(fptr, "#t");
        } else {
            fprintf(fptr, "#f");
        }
    }else if((x & PTR_MASK) == PAIR_TAG){
        int* ptr = (int*)(x - PAIR_TAG);
        if(ptr == NULL){
            fprintf(fptr, "()");
            return 0;
        }

        // either a list or a dotted pair
        int car = ptr[0];
        int cdr = ptr[1];
        fputc('(', fptr);
        show(car,fptr);

        // show additional space-separated elems
        while((cdr & PTR_MASK) == PAIR_TAG){
            ptr = (int*)(cdr - PAIR_TAG);
            if(ptr == NULL) break;

            car = ptr[0];
            cdr = ptr[1];
            fputc(' ', fptr);
            show(car,fptr);
        }

        // show dotted pair notation if relevant
        if((cdr & PTR_MASK) != PAIR_TAG){
            fprintf(fptr, " . ");
            show(cdr,fptr);
        }
        fputc(')', fptr);
    }else if((x & PTR_MASK) == STR_TAG){
        int* ptr = (int*)(x - STR_TAG);
        int len = *ptr;
        char* body = (char*)(ptr+1);
        
        assert(len >= 0);
        if(body[len] != '\0'){
            printf("%.*s\n", body);
            assert(body[len] == '\0');
        }

        fputc('"', fptr);
        for(;len > 0;len--,body++){
            fputc(*body, fptr);
        }
        fputc('"', fptr);
    }else if((x & PTR_MASK) == VEC_TAG){
        int* ptr = (int*)(x - VEC_TAG);
        int len = *ptr++;

        fprintf(fptr, "#(");
        for(;len > 0;len--) {
            show(*ptr++,fptr);
            if(len != 1) fputc(' ', fptr);
        }
        fprintf(fptr, ")");
    }else if((x & PTR_MASK) == SYM_TAG){
        int* ptr = (int*)(x - SYM_TAG);
        int len = *ptr;
        char* body = (char*)(ptr+1);
        for(;len > 0;len--) fputc(*body++, fptr);
    }else if((x & PTR_MASK) == CLOSURE_TAG){
        int* ptr = (int*)(x - CLOSURE_TAG);;
        int label = *ptr;
        int len = *(ptr+1);
        fprintf(fptr, "#<procedure label=%x len=%x>", label, len);
    }else{
        fprintf(fptr, "#<unknown>");
    }
    return 0;
}

int make_str(const char *src){
    int *y = free_ptr;
    char *dest = (char*)(y+1);
    int len = strlen(src);
    *y = strlen(src);

    free_ptr = free_ptr+1;
    free_ptr = (int*)((char*)free_ptr + len + 1);
    free_ptr = (int*)align_to_multiple(8,(int)free_ptr);
    strcpy(dest,src);
    return (int)y | STR_TAG;
}
int make_pair(int a, int b){
    int *y = free_ptr;
    free_ptr = free_ptr + 2;
    y[0] = a;
    y[1] = b;
    return (int) y | PAIR_TAG;
}
int s_cmd_ln(){
    assert((ARGC*ARGC) < free_ptr - fromspace_start);
    int x = PAIR_TAG;
    for(int i = ARGC-1; i > 0; --i){
        x = make_pair(make_str(ARGV[i]),x);
    }
    return x;
}

void s_exit(){
    exit(0);
}

int s_system(int s){
    s = system(cast_obj_to_str(s));
    return s << FIXNUM_SHIFT;
}

int s_div(int x, int d){
    x = cast_obj_to_int(x);
    d = cast_obj_to_int(d);
    return (x/d) << FIXNUM_SHIFT;
}

int s_mod(int x, int d){
    x = cast_obj_to_int(x);
    d = cast_obj_to_int(d);
    return (x%d) << FIXNUM_SHIFT;
}
#include <sys/unistd.h>
int s_getpid(){
    return getpid() << FIXNUM_SHIFT;
}

// file IO
void check_sfptr(int fptr){
    if((fptr & FIXNUM_MASK) != FIXNUM_TAG) {
        printf("sfptr expect integer: ");
        s_show(fptr);newline();
        exit(EXIT_FAILURE);
    }
    assert(fptr%8 == 0);
}

int s_fopen(int _path, int _mode){
    /*
    r, w, a, r+, w+, a+
    */
    const char* path = cast_obj_to_str(_path);
    const char* mode = cast_obj_to_str(_mode);
    int fptr = (int)fopen(path,mode);
    assert(!feof((FILE*)fptr));
    assert(fptr%8 == 0);
    return fptr;
}

int s_fclose(int fptr) {
    return fclose((FILE*)fptr) << FIXNUM_SHIFT;
}

int s_fputc(int c, int fptr){
    check_sfptr(fptr);
    fputc(cast_obj_to_char(c),(FILE*) fptr);
    return 0;
}

int s_fputs(int s, int fptr){
    check_sfptr(fptr);
    fputs(cast_obj_to_str(s),(FILE*) fptr);
    return 0;
}

int s_fgetc(int fptr){
    check_sfptr(fptr);
    int c = fgetc((FILE*) fptr);
    return c << FIXNUM_SHIFT;
}

int s_ungetc(int c, int fptr){
    int t = ungetc(cast_obj_to_char(c),(FILE*) fptr);
    return t << FIXNUM_SHIFT;
}

int s_stdout(){
    assert((int)stdout%8 == 0);
    return (int)stdout;
}

int s_stdin(){
    assert((int)stdin%8 == 0);
    return (int)stdin;
}

int s_stderr(){
    assert((int)stderr%8 == 0);
    return (int)stderr;
}

int s_fseek(int fptr, int pos){
    check_sfptr(fptr);
    pos = fseek((FILE*)fptr,SEEK_SET,cast_obj_to_int(pos));
    return pos << FIXNUM_SHIFT;
}

void s_function_error(int x){
    fprintf(stderr, "Exception: attempt to apply non-procedure ");
    show(x, stderr);
    putc('\n', stderr);
    exit(1);
}

int main(int argc, const char **argv){
    ARGC = argc;
    ARGV = argv;
    assert(sizeof(char) == 1);
    assert(sizeof(int) == 4);
    assert(sizeof(void*) == 4);
    assert(sizeof(int*) == 4);
    assert(sizeof(char*) == 4);
    assert(sizeof(char*) == sizeof(char**));
    assert(sizeof(char**) == sizeof(char***));
    assert(sizeof(int*) == sizeof(int**));
    assert(sizeof(int**) == sizeof(int***));
    assert(sizeof(void*) == sizeof(void**));
    assert(sizeof(void**) == sizeof(void***));
    assert(sizeof(int) == 4);
    assert(sizeof(time_t) == 4);
    
    initialize();
    int val = scheme_entry();
    return 0;
}