/* code-generation test only */
/* non-constant initialization of an entire buffer */

int otherfunc(int *buf);
int testfunc(unsigned n) {
  int buf[n];
//  if (n == 0) return 0;
  for (unsigned i = 0; i < n; ++i)
    buf[i] = 1;
  otherfunc(buf);
}
