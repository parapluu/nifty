#include "arguments.h"

static int i;

void
f1(void)
{
  i = 0;
}

void f2()
{
  i++;
}

int 
f3(int a,int b,int c,int d)
{
  return a+b+c+d+i;
}

int
f4(int arg1, int arg2, int arg3, int arg4)
{
  return arg1+arg2+arg3+arg4;
}
