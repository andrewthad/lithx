#include <stdint.h>

// C program that is similar to the lith program. Used to
// see what gcc and clang come up with.
int64_t example(const int64_t* restrict xs) {
  int64_t acc = 0;
  for(int i = 0; i < 10; i++) {
    acc = acc + (xs[i] * 2);
  }
  return acc;
}
