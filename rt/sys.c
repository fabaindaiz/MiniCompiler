#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <inttypes.h>

typedef uint64_t VAL;
extern VAL our_code_starts_here() asm("our_code_starts_here");


const uint64_t BOOL_TAG   = 0x0000000000000001;
const VAL BOOL_TRUE  = 0x8000000000000001; // These must be the same values
const VAL BOOL_FALSE = 0x0000000000000001; // as chosen in compile.ml

void get_value(char* buffer, VAL val) {
  if ((val & BOOL_TAG) == 0) { // val is even ==> number
    sprintf(buffer, "%ld", ((int64_t)(val)) / 2); // shift bits right to remove tag
  } else if (val == BOOL_TRUE) {
    sprintf(buffer, "true");
  } else if (val == BOOL_FALSE) {
    sprintf(buffer, "false");
  } else {
    sprintf(buffer, "0x%lx", val); // print unknown val in hex
  }
}


const int ERR_NOT_NUMBER = 1;
const int ERR_NOT_BOOLEAN = 2;
// other error codes here

void error(int errCode, VAL val) {
  char buffer[50];
  get_value(buffer, val);

  if (errCode == ERR_NOT_NUMBER) {
    fprintf(stderr, "Expected number, but got %s\n", buffer);
  } else if (errCode == ERR_NOT_BOOLEAN) {
    fprintf(stderr, "Expected boolean, but got %s\n", buffer);
  } else {
    printf("Unknown error: %d Value: %s", errCode, buffer); // print unknown val in hex
  }
  exit(errCode);
}


VAL print(VAL val) {
  char buffer[50];
  get_value(buffer, val);

  printf("> %s\n", buffer);
  return val;
}

void print_result(VAL val) {
  char buffer[50];
  get_value(buffer, val);

  printf("%s\n", buffer);
}

int main(int argc, char** argv) {
  VAL result = our_code_starts_here();
  print_result(result);
  return 0;
}
