#include "array.h"

int
sumarray(int arr[10]) {
  int i, sum=0;
  for (i=0; i<10; i++) {
    sum += arr[i];
  }
  return sum;
}

int
sumstruct_array(struct array_st* val) {
  int i, sum=0;
  for (i=0; i<10; i++) {
    sum += (val->start)[i] + (val->end)[i];
  }
  return sum;
}
