#ifndef NIFTY_ENUMS_TEST
#define NIFTY_ENUMS_TEST

enum enum0 {
  THE_VALUE
};

typedef enum enum1 {
  VALUE1,
  VALUE2,
  VALUE3
} enum1_t;

 enum enum2 {
  VALUE4 = 4,
  VALUE5,
  VALUE6 = 100
};

enum1_t f1(enum enum0 par1, enum enum2 par2) {
        return VALUE2;
}

#endif /* NIFTY_ENUMS_TEST */
