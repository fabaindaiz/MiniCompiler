#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <inttypes.h>
#include <sys/resource.h>


/* synonym to ease typing/reading */
typedef uint64_t u64;

const uint64_t BOOL_TAG   = 0x0000000000000001;
const uint64_t TUPLE_TAG   = 0x000000000000005;
const uint64_t CLOSURE_TAG   = 0x000000000000007;
const uint64_t FORWARDER_TAG   = 0x000000000000003;

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


const int ERR_TUPLE_BAD_INDEX = 1;
const int ERR_ARITY_MISMATCH = 2;

void error2(int errCode, u64 val1, u64 val2) {
  char buffer1[50];
  char buffer2[50];
  get_value(buffer1, val1);
  get_value(buffer2, val2);

  if (errCode == ERR_TUPLE_BAD_INDEX) {
    fprintf(stderr, "Index out of bounds: Tried to access index %s of %s", buffer1, buffer2);
  } else if (errCode == ERR_ARITY_MISMATCH) {
    fprintf(stderr, "Arity mismatch: closure expected %lu arguments but got %lu", val1, val2);
  } else {
    printf("Unknown error: %d Value: %s %s", errCode, buffer1, buffer2); // print unknown val in hex
  }
  exit(errCode);
}
