int otherfunc(char *buf);
int blub();

/* code-generation test only */
/* multiple non-constant memsets with non-escaping calls in between */
/* only the second memset should remain post-optimization */

int func(int n) {
  char buf[n];
  memset(buf, 0, sizeof(buf));
  if (n == 1) blub(); 
  memset(buf, 1, sizeof(buf));
  buf[0] = 0;
  return otherfunc(buf);
}
