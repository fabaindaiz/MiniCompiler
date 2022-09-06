#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>

typedef uint64_t VAL;

extern VAL our_code_starts_here() asm("our_code_starts_here");

const uint64_t BOOL_TAG   = 0x0000000000000001;
const VAL BOOL_TRUE  = 0x8000000000000001; // These must be the same values
const VAL BOOL_FALSE = 0x0000000000000001; // as chosen in compile.ml
VAL print(VAL val) {
  if ((val & BOOL_TAG) == 0) { // val is even ==> number
    printf("%ld", ((int64_t)(val)) / 2); // shift bits right to remove tag
  } else if (val == BOOL_TRUE) {
    printf("true");
  } else if (val == BOOL_FALSE) {
    printf("false");
  } else {
    printf("Unknown value: %lx", val); // print unknown val in hex
  }
  return val;
}
int main(int argc, char** argv) {
  VAL result = our_code_starts_here();
  print(result);
  return 0;
}
