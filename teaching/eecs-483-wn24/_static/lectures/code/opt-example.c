#include<stdint.h>

int64_t foo(int64_t w) {
  int64_t x = 3 + 5;
  int64_t y = x * w;
  int64_t z = y - 0;
  return z * 4;
}
