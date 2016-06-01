#include "unions.h"

int
check_i(union_t u, int i) {
	if (u.i == i) {
		return 1;
	} else {
		return 0;
	}
}

int
check_f(complex s, float f) {
	if ((s.u.f - f) < 0.0001) {
		return 1;
	} else {
		return 0;
	}
}
