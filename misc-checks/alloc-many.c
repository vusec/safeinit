#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <stdio.h>

/* make random-sized allocations, check they're zero, fill them, free them */

int main() {
  int i;
  size_t s;
  // random sizes
  for (i = 0; i < 65536; ++i) {
    if (rand())
      s = rand() % (1024 * 1024 * 32);
    else
      s = (rand() % 1024) * (1024 * 32);
    printf("%d ", s);
    char *x = malloc(s);
    assert(x[0] == 0);
    assert(x[s-1] == 0);
    memset(x, 0xff, s);
    free(x);
  }
}

