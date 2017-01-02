#include <stdlib.h>
#include <string.h>
#include <vector>

struct Custom {
  char y;
  int x;
  char z;
  int q;
};

char *x;
void dirty() {
  memset(x, 0xcc, 200 * sizeof(Custom));
  delete[] x;
}

int main() {
  x = new char[200 * sizeof(Custom)];
  dirty();

  Custom y; y.y = 0; y.x = 0; y.z = 0; y.q = 0;
  std::vector<Custom> vec(200);
  vec[0] = y;
  char vec2[sizeof(Custom)*200];
  memcpy(vec2, &vec[0], sizeof(vec2));
  int total = 0;
  for (unsigned i = 0; i < sizeof(vec2); ++i)
    total += vec2[i];
  return total;
}
