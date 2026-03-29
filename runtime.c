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

extern uint64_t entry(void *heap);

void print_value(uint64_t value) {
  if ((value & NUM_MASK) == NUM_TAG) {
    uint64_t ivalue = (int64_t)value;
    printf("%" PRIu64, ivalue >> NUM_SHIFT);
  } else if ((value & BOOL_MASK) == BOOL_TAG) {
    if (value >> BOOL_SHIFT) {
      printf("True");
    } else {
      printf("False");
    }
  } else if ((value & HEAP_MASK) == PAIR_TAG) {
    // Pair 的最后两位是010，我们需要减去Tag获取真实地址
    uint64_t v1 = *(uint64_t *)(value - PAIR_TAG);
    uint64_t v2 = *(uint64_t *)(value - PAIR_TAG + 8);
    printf("(pair ");
    print_value(v1);
    printf(" ");
    print_value(v2);
    printf(")");
  } else {
    printf("BAD Value: %llu", value);
  }
}

[[noreturn]]
void error(void) {
  printf("ERROR\n");
  exit(1);
}

int main(int argc, char **argv) {
  void *heap = (void *)malloc(4096);
  print_value(entry(heap));
  return 0;
}
