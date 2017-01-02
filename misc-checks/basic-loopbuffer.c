void otherfunc(char *b);

/* code-generation test only */
/* obviously this whole buffer should be re-initialized every time */

int func() {
  for (int i = 0; i < 500; ++i) {
    char zb[512];
    otherfunc(zb);
  }
}
