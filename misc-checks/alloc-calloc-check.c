#include <string.h>
#include <assert.h>
#include <stdlib.h>

/* This is just a quick check that the allocator appears to be
 * actually zeroing blocks before returning them to us. */

int main() {
  for (unsigned n = 0; n < 1024; ++n) {
    char *mybuf = malloc(1024 * 1024);
    memset(mybuf, 0xff, 1024 * 1024);
    free(mybuf);
  }
  for (unsigned n = 0; n < 1024; ++n) {
    char *mybuf = calloc(1024 * 1024, 1);
    assert(mybuf[0] == 0);
    free(mybuf);
  }
}
