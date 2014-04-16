#include "array.h"

int
sumarray(int arr[10]) {
	int i, s=0;
	for (i=0;i<10;i++) {
		s+=arr[i];
	}
	return s;
}

