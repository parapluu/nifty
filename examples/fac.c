#include "fac.h"

/* Recursive Version */
unsigned int recursive_factorial(int n) 
{
	return n>=1 ? n * recursive_factorial(n-1) : 1;
}

/* Iterative Version */
unsigned int iter_factorial(int n) 
{
	int f = 1;
	int i;
	for(i = 1; i <= n; i++) 
	{
		f *= i;
	}
	return f;
}
