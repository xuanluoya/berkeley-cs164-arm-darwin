#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

typedef enum NumFlag { NUM_SHIFT = 2, NUM_MASK = 0b11, NUM_TAG = 0b00 } NumFlag;

typedef enum BoolFlag {
  BOOL_SHIFT = 7,
  BOOL_MASK = 0b1111111,
  BOOL_TAG = 0b0011111
} BoolFlag;

typedef enum HeapFlag { HEAP_MASK = 0b111 } HeapFlag;

typedef enum PairFlag { PAIR_TAG = 0b10 } PairFlag;
typedef enum FuncFlag { FUNC_TAG = 0b110 } FuncFlag;

extern uint64_t entry(void *heap);

void print_value(uint64_t value) {
  if ((value & NUM_MASK) == NUM_TAG) {
    int64_t ivalue = (int64_t)value;
    printf("%" PRIi64, ivalue >> NUM_SHIFT);
  } else if ((value & BOOL_MASK) == BOOL_TAG) {
    if (value >> BOOL_SHIFT) {
      printf("<True>");
    } else {
      printf("<False>");
    }
  } else if ((value & HEAP_MASK) == PAIR_TAG) {
    uint64_t v1 = *(uint64_t *)(value - PAIR_TAG);
    uint64_t v2 = *(uint64_t *)(value - PAIR_TAG + 8);
    printf("(CONS ");
    print_value(v1);
    printf(" ");
    print_value(v2);
    printf(")");
  } else if ((value & HEAP_MASK) == FUNC_TAG) {
    printf("<Function>");
  } else {
    printf("BAD VALUE %" PRIu64, value);
  }
}

[[noreturn]]
void error(void) {
  printf("ERROR\n");
  exit(1);
}

uint64_t read_num(void) {
  int r;
  if (scanf("%d", &r) != 1) {
    printf("Error reading number\n");
  }
  return (uint64_t)(r) << NUM_SHIFT;
}

void print_newline(void) { printf("\n"); }

int main(int argc, char **argv) {
  void *heap = (void *)malloc(4096);
  entry(heap);
  return 0;
}
