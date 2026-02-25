#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>

typedef enum NumFlag { NUM_SHIFT = 2, NUM_MASK = 0b11, NUM_TAG = 0b00 } NumFlag;

typedef enum BoolFlag {
  BOOL_SHIFT = 7,
  BOOL_MASK = 0b1111111,
  BOOL_TAG = 0b0011111
} BoolFlag;

extern uint64_t entry(void);

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
  } else {
    printf("BAD Value: %llu", value);
  }
}

int main(int argc, char **argv) {
  print_value(entry());
  return 0;
}
