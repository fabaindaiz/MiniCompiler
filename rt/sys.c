#include "gc.c"


/* externs */
extern u64 our_code_starts_here(u64* heap) asm("our_code_starts_here");
/* */


/*
  ALL YOUR OTHER RTSYS.C STUFF GOES HERE
  (constants, tagging, error handling, printing)
*/

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
  return max(max(max(v1, v2), max(v3, v4)), max(max(v5, v6), max(v7, v8)));
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
  HEAP_MID = HEAP_START + HEAP_SIZE;   /* TBD */
  HEAP_END = HEAP_START + HEAP_SIZE * 2;   /* TBD */
  FROM_SPACE = HEAP_START; /* TBD */
  TO_SPACE = HEAP_MID;   /* TBD */

  /* Go! */
  /* Q: when do you need to call `free(heap)`? */
  u64 result = our_code_starts_here(HEAP_START);
  print_result(result);
  printf("\n");
  
  free(HEAP_START);
  return 0;
}
