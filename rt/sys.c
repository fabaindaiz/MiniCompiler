#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <inttypes.h>

typedef uint64_t VAL;
extern VAL our_code_starts_here(uint64_t* HEAP) asm("our_code_starts_here");


const uint64_t BOOL_TAG   = 0x0000000000000001;
const uint64_t TUPLE_TAG   = 0x000000000000005;
const VAL BOOL_TRUE  = 0x8000000000000001; // These must be the same values
const VAL BOOL_FALSE = 0x0000000000000001; // as chosen in compile.ml

void get_value(char* buffer, VAL val) {
  if ((val & TUPLE_TAG) == TUPLE_TAG) { // val ends in 3 ==> tuple
    VAL* point = (VAL*)(val - TUPLE_TAG);
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


const int ERR_NOT_NUMBER = 1;
const int ERR_NOT_BOOLEAN = 2;
const int ERR_NOT_TUPLE = 3;
const int ERR_BAD_INDEX_LOW = 5;
const int ERR_BAD_INDEX_HIGH = 6;
// other error codes here

void error(int errCode, VAL val) {
  char buffer[50];
  get_value(buffer, val);

  if (errCode == ERR_NOT_NUMBER) {
    fprintf(stderr, "Type error: Expected integer but got %s", buffer);
  } else if (errCode == ERR_NOT_BOOLEAN) {
    fprintf(stderr, "Type error: Expected boolean but got %s", buffer);
  } else if (errCode == ERR_NOT_TUPLE) {
    fprintf(stderr, "Type error: Expected tuple but got %s", buffer);
  } else {
    printf("Unknown error: %d Value: %s", errCode, buffer); // print unknown val in hex
  }
  exit(errCode);
}

void error2(int errCode, VAL val1, VAL val2) {
  char buffer1[50];
  char buffer2[50];
  get_value(buffer1, val1);
  get_value(buffer2, val2);

  if (errCode == ERR_BAD_INDEX_LOW) {
    fprintf(stderr, "Index out of bounds: Tried to access index %s of %s", buffer1, buffer2);
  } else if (errCode == ERR_BAD_INDEX_HIGH) {
    fprintf(stderr, "Index out of bounds: Tried to access index %s of %s", buffer1, buffer2);
  } else {
    printf("Unknown error: %d Value: %s %s", errCode, buffer1, buffer2); // print unknown val in hex
  }
  exit(errCode);
}


VAL rawprint(VAL val) {
  printf("> 0x%lx\n", val);
  return val;
}

VAL print(VAL val) {
  char buffer[50];
  get_value(buffer, val);

  printf("> %s\n", buffer);
  return val;
}

VAL max(VAL val1, VAL val2) {
  return val1 > val2? val1 : val2;
}

VAL megamax(VAL v1, VAL v2, VAL v3, VAL v4, VAL v5, VAL v6, VAL v7, VAL v8){
  return max(max(max(max(max(max(max(v1 , v2),v3),v4),v5),v6),v7),v8);
}

void print_result(VAL val) {
  char buffer[50];
  get_value(buffer, val);

  printf("%s\n", buffer);
}

int main(int argc, char** argv) {
  uint64_t* HEAP = calloc(1024, sizeof(uint64_t)); // Allocate 8KB of memory for now
  VAL result = our_code_starts_here(HEAP);
  print_result(result);
  free(HEAP);
  return 0;
}
