#include "builtin_types.h"

/* integer */
short signed int
f1(short signed int arg)
{
  return arg;
}

short unsigned int
f2(short unsigned int arg)
{
  return arg;
}

signed int
f3(signed int arg)
{
  return arg;
}

unsigned int 
f4(unsigned int arg)
{
  return arg;
}

long signed int
f5(long signed int arg)
{
  return arg;
}

long long int
f13(long long int arg)
{
  return arg;
}

long unsigned int
f6(long unsigned int arg)
{
  return arg;
}

signed char
f7(signed char arg)
{
  return arg;
}

unsigned char
f8(unsigned char arg)
{
  return arg;
}

/* float */
float 
f9(float arg)
{
  return arg;
}

double 
f10(double arg)
{
  return arg;
}

/* pointer */
void*
f11(void* arg)
{
  return arg;
}

/* arrays */
char* 
f12(char smth[10])
{
  return &smth;
}
