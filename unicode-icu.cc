#include <iostream>
#include <cstdlib>
#include <unicode/unistr.h>
#include <unicode/ustring.h>
#include <unicode/ustdio.h>
#include <unicode/ustream.h>

int main()
{
#if 1
  UnicodeString euro("€");

  UFILE * out = u_finit(stdout, NULL, NULL);
  
  u_fprintf(out, "--------\n");
  u_fprintf(out, "[%6S]\n", euro.getBuffer());
#else
  UFILE * in = u_fopen("foo", "r", "UTF-8", NULL);

  UChar buf[256];
  u_fgets(buf, 255, in);

  UFILE * out = u_finit(stdout, NULL, NULL);
  
  u_fprintf(out, "--------\n", buf);
  u_fprintf(out, "[%6S]\n", buf);
#endif
}
