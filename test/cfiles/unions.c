#include "unions.h"

int
check_i(union_t u) {
	return u.i;
}

float
check_f(union_t u) {
	return u.f;
}

float
check_sf(complex s) {
	return s.u.f;
}
