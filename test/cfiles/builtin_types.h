#ifndef NIFTY_BUILTIN_TYPES_TEST
#define NIFTY_BUILTIN_TYPES_TEST

/* integer */
extern short signed int f1(short signed int arg);
extern short unsigned int f2(short unsigned int arg);
extern signed int f3(signed int arg);
extern unsigned int f4(unsigned int arg);
extern long signed int f5(long signed int arg);
extern long unsigned int f6(long unsigned int arg);
extern signed char f7(signed char arg);
extern unsigned char f8(unsigned char arg);

/* float */
extern float f9(float arg);
extern double f10(double arg);

/* pointer */
extern void* f11(void* arg);

/* arrays */
extern char* f12(char smth[10]);

#endif  /* NIFTY_BUILTIN_TYPES_TEST */
