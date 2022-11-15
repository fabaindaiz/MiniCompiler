#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <inttypes.h>
#include <sys/resource.h>


/* synonym to ease typing/reading */
typedef uint64_t u64;

/* configuration */
u64 STACK_SIZE = 0x800000;
u64 HEAP_SIZE = 16;
int USE_GC = 1;


/* externs */
//extern void error(u64 err_code, u64 val) asm("error");
//extern u64 print(u64 val) asm("print");
extern u64* try_gc(u64* alloc_ptr, u64 words_needed, u64* cur_frame, u64* cur_sp) asm("try_gc");
extern u64 our_code_starts_here(u64* heap) asm("our_code_starts_here");
extern void set_stack_bottom(u64* stack_bottom) asm("set_stack_bottom");
/* */


const uint64_t BOOL_TAG   = 0x0000000000000001;
const uint64_t TUPLE_TAG   = 0x000000000000005;
const uint64_t CLOSURE_TAG   = 0x000000000000007;
const u64 BOOL_TRUE  = 0x8000000000000001; // These must be the same values
const u64 BOOL_FALSE = 0x0000000000000001; // as chosen in compile.ml


bool is_tuple(u64 val) {
  return ((val & CLOSURE_TAG) == TUPLE_TAG);
}

bool is_closure(u64 val) {
  return ((val & CLOSURE_TAG) == CLOSURE_TAG);
}


void get_value(char* buffer, u64 val) {
  if (is_tuple(val)) { // val ends in 3 ==> tuple
    u64* point = (u64*)(val - TUPLE_TAG);
    int64_t end = *point;
    sprintf(buffer, "(tup");
    buffer+=4;
    for (int i = 1; i < end + 1; i++)
    {
      char buf[50];
      get_value(buf, point[i]);
      sprintf(buffer, " %s", buf);
      buffer+=strlen(buf) + 1;
    }
    sprintf(buffer, ")");
  } else if (is_closure(val)) { // TODO closure print
    u64* point = (u64*)(val - CLOSURE_TAG);
    sprintf(buffer, "<clos:%ld", ((int64_t)(point[0])));
    buffer+=7;
    /*
    // descomentar esto para ver la clausura completa en el print
    int64_t end = point[2];
    //printf("%ld",end);
    for (int i = 3; i < end + 3; i++)
    {
      char buf[50];
      get_value(buf, point[i]);
      sprintf(buffer, " %s", buf);
      buffer+=strlen(buf) + 1;
    }
    */
    sprintf(buffer, ">");
  } else if ((val & BOOL_TAG) == 0) { // val is even ==> number
    sprintf(buffer, "%ld", ((int64_t)(val)) / 2); // shift bits right to remove tag
  } else if (val == BOOL_TRUE) {
    sprintf(buffer, "true");
  } else if (val == BOOL_FALSE) {
    sprintf(buffer, "false");
  } else {
    sprintf(buffer, "0x%lx", val); // print unknown val in hex
  }
}


/*
  ALL YOUR OTHER RTSYS.C STUFF GOES HERE
  (constants, tagging, error handling, printing)
*/

/* GC */
u64* HEAP_START;
u64* HEAP_END;
u64* HEAP_MID;
u64* TO_SPACE;
u64* FROM_SPACE;
u64* ALLOC_PTR = 0;
u64* SCAN_PTR = 0;
u64* STACK_BOTTOM = 0;

void set_stack_bottom(u64* stack_bottom) {
  STACK_BOTTOM = stack_bottom;
}

bool is_heap_ptr(u64 val){
  return (u64*)val < HEAP_END && (u64*)val >= HEAP_START;
}

void print_stack(u64* rbp, u64* rsp) {
  printf("|------- frame %p to %p  ------\n", rsp, rbp);
  for (u64* cur_word = rsp; cur_word < rbp; cur_word++) {
    u64 val = (u64)*cur_word;
    printf("| %p: %p", cur_word, (u64*)*cur_word);
    if (is_heap_ptr(val)) {
      if (is_tuple(val)){ printf(" (tuple)"); }
      else if (is_closure(val)){ printf(" (closure)"); }
    }
    printf("\n");
  }
  if (rbp < STACK_BOTTOM) {
    print_stack((u64*)*rbp, rbp + 2);
  }
  else {
    printf("|------- bottom %p  ------\n\n", STACK_BOTTOM);
  }
}

void print_heap(u64* heap_start, u64* heap_end){
  printf("| Heap from %p to %p\n", heap_start, heap_end);
  for (u64 i = 0; i <= (u64)(heap_end - heap_start); i++) {
    printf("|  %lud/%p: %p \n", i, (heap_start + i), (u64*)*(heap_start + i));
  }
}

void print_heaps(){
  printf("|\n|=======HEAP 1==========\n");
  print_heap(HEAP_START, HEAP_MID-1);
  printf("|=======HEAP 2==========\n");
  print_heap(HEAP_MID, HEAP_END);
  printf("|=================\n\n");
}


u64* collect(u64* cur_frame, u64* cur_sp) {
  /* TBD: see https://en.wikipedia.org/wiki/Cheney%27s_algorithm */
  // swap from-space to-space
  // init spaces
  // scan stack and copy roots
  // scan objects in the heap
  // clean old space
  return ALLOC_PTR;
}

/* trigger GC if enabled and needed, out-of-memory error if insufficient */
u64* try_gc(u64* alloc_ptr, u64 words_needed, u64* cur_frame, u64* cur_sp) {
  if (USE_GC==1 && alloc_ptr + words_needed > FROM_SPACE + HEAP_SIZE) {
    printf("| need memory: GC!\n");
    alloc_ptr = collect(cur_frame, cur_sp);
  }
  if (alloc_ptr + words_needed > FROM_SPACE + HEAP_SIZE) {
    printf("| Error: out of memory!\n\n");
    print_stack(cur_frame, cur_sp);
    print_heaps();
    exit(-1);
  }
  return alloc_ptr;
}


const int ERR_NOT_NUMBER = 1;
const int ERR_NOT_BOOLEAN = 2;
const int ERR_NOT_TUPLE = 3;
const int ERR_NOT_CLOSURE = 4;

void error(int errCode, u64 val) {
  char buffer[50];
  get_value(buffer, val);

  if (errCode == ERR_NOT_NUMBER) {
    fprintf(stderr, "Type error: Expected integer but got %s", buffer);
  } else if (errCode == ERR_NOT_BOOLEAN) {
    fprintf(stderr, "Type error: Expected boolean but got %s", buffer);
  } else if (errCode == ERR_NOT_TUPLE) {
    fprintf(stderr, "Type error: Expected tuple but got %s", buffer);
  } else if (errCode == ERR_NOT_CLOSURE) {
    fprintf(stderr, "Type error: Expected closure but got %s", buffer);
  } else {
    printf("Unknown error: %d Value: %s", errCode, buffer); // print unknown val in hex
  }
  exit(errCode);
}


const int ERR_BAD_INDEX_LOW = 1;
const int ERR_BAD_INDEX_HIGH = 2;
const int ERR_ARITY_MISMATCH = 3;

void error2(int errCode, u64 val1, u64 val2) {
  char buffer1[50];
  char buffer2[50];
  get_value(buffer1, val1);
  get_value(buffer2, val2);

  if (errCode == ERR_BAD_INDEX_LOW) {
    fprintf(stderr, "Index out of bounds: Tried to access index %s of %s", buffer1, buffer2);
  } else if (errCode == ERR_BAD_INDEX_HIGH) {
    fprintf(stderr, "Index out of bounds: Tried to access index %s of %s", buffer1, buffer2);
  } else if (errCode == ERR_ARITY_MISMATCH) {
    fprintf(stderr, "Arity mismatch: closure expected %lu arguments but got %lu", val1, val2);
  } else {
    printf("Unknown error: %d Value: %s %s", errCode, buffer1, buffer2); // print unknown val in hex
  }
  exit(errCode);
}


u64 rawprint(u64 val) {
  printf("> 0x%lx\n", val);
  return val;
}

u64 print(u64 val) {
  char buffer[50];
  get_value(buffer, val);

  printf("> %s\n", buffer);
  return val;
}

u64 max(u64 val1, u64 val2) {
  return val1 > val2? val1 : val2;
}

u64 megamax(u64 v1, u64 v2, u64 v3, u64 v4, u64 v5, u64 v6, u64 v7, u64 v8){
  return max(max(max(max(max(max(max(v1 , v2),v3),v4),v5),v6),v7),v8);
}

void print_result(u64 val) {
  char buffer[50];
  get_value(buffer, val);

  printf("%s\n", buffer);
}


/* start */
int main(int argc, char** argv) {

  /* stack size config */
  char* stack_size_envvar = getenv("STACK_SIZE");
  if (stack_size_envvar) STACK_SIZE = strtoull(stack_size_envvar, NULL, 0);
  printf("| Setting stack size to %" PRId64 " .\n", STACK_SIZE);
  struct rlimit limit;
  getrlimit(RLIMIT_STACK, &limit);
  limit.rlim_cur = STACK_SIZE < limit.rlim_max ? STACK_SIZE : limit.rlim_max;
  int res = setrlimit(RLIMIT_STACK, &limit);
  if (res != 0) { printf("| Setting rlimit failed...\n") ; }

  /* GC config */
  char* use_gc_envvar = getenv("USE_GC");
  if (use_gc_envvar) USE_GC = strtoull(use_gc_envvar, NULL, 0);
  printf("| Use GC: %d\n", USE_GC);

  /* heap size config */
  char* heap_size_envvar = getenv("HEAP_SIZE");
  if (heap_size_envvar) HEAP_SIZE = strtoull(heap_size_envvar, NULL, 0);
  printf("| Heap size: %" PRId64 " .\n", HEAP_SIZE);

  /* setting up two space heap for GC */
  u64* heap = (u64*)calloc((HEAP_SIZE * 2) + 15, sizeof(u64));
  HEAP_START = (u64*)(((u64)heap + 15) & ~0xF);
  /* TBD: initialize HEAP_MID, HEAP_END, FROM_SPACE, TO_SPACE */
  HEAP_MID = 0;   /* TBD */
  HEAP_END = 0;   /* TBD */
  FROM_SPACE = 0; /* TBD */
  TO_SPACE = 0;   /* TBD */

  /* Go! */
  /* Q: when do you need to call `free(heap)`? */
  u64 result = our_code_starts_here(HEAP_START);
  print_result(result);
  printf("\n");
  
  free(HEAP_START);
  return 0;
}
