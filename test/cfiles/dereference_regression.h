#ifndef _DEREFERENCE_REGRESSION_H_
#define _DEREFERENCE_REGRESSION_H_


typedef struct s { int i; int j;} s;

extern int f(struct s **);

#endif /* _DEREFERENCE_REGRESSION_H_ */
