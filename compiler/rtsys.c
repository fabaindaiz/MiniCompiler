#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>

typedef int64_t i64; 
typedef uint64_t u64;

extern i64 our_code_starts_here() asm("our_code_starts_here");

int main(int argc, char** argv) {
  i64 result = our_code_starts_here();
  printf("%" PRId64 "\n", result);
  return 0;
}
