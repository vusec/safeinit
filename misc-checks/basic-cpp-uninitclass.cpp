#include <stdio.h>
#include <string.h>

struct MyClass {
  char q;
  int x;
  char y;
  int z;

  MyClass() : q(0), x(0) /*, y(0)*/, z(0) { }
};

void func(char *buffer) {
  MyClass myclass;

  memcpy(buffer, &myclass, sizeof(MyClass));
}

void meep() {
  printf("hi hi\n");
}

int main() {
  char buffer[sizeof(MyClass)];
  meep();
  func(buffer);
/*  int total = 0;
  for (unsigned i = 0; i < sizeof(buffer); ++i)
    total += buffer[i];
  return total;*/
  return ((MyClass *)buffer)->y;
}
