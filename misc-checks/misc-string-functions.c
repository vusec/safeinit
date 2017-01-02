#include <string.h>
#include <stdio.h>
int main() {
  /* no need to initialize buf or buf2 */
  char buf[64], buf2[64], *buf3;
  strcpy(buf, "hello");
  sprintf(buf, "hi");
  strcpy(buf2, buf);
  printf(buf2);
  return (int)buf3; /* should return 0 :-) */
}
