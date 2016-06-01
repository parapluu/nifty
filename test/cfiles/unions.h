#ifndef NIFTY_UNIONS_TEST
#define NIFTY_UNIONS_TEST

typedef union _u {
	int i;
	float f;
} union_t;

typedef struct _s {
	union_t u;
	int i;
} complex;

extern int check_i(union_t u, int i);
extern int check_f(complex s, float f);

#endif /* NIFTY_UNIONS_TEST */
