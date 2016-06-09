#include "unions.h"

int
check_i(union_t u) {
	return u.i;
}

float
check_f(complex s) {
	return s.u.f;
}
