#include "structs.h"
#include "stdlib.h"

S* getS()
{
	S *tmp = malloc(sizeof(S));
	tmp->a = 1;
	tmp->b = 2;
	tmp->c = 3;

	return tmp;
}

int
alterS(S* s)
{
	s->a +=1;
	s->b +=1;
	s->c +=1;
	return 0;
}
