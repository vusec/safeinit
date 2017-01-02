#include <string.h>
#include <stdlib.h>

/* for checking behaviour with/without inlining */

char *otherfunc() {
  return malloc(64);
}

char *myfunc() {
  char *m = otherfunc();
  memset(m, 0, 64);
  return m;
}

int main() {
  volatile char *x = myfunc();
  *x = (char)x;
  return 0;
}
