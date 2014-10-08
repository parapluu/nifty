#ifndef NIFTY_STRUCTS_TEST
#define NIFTY_STRUCTS_TEST

struct s1 {
  int f1;
  char f2;
  char* f3;
  char f4[10];
};

struct s2 {
  struct s1 f1;
  struct s1 *f2;
};


struct s3 {
  struct s2 f1;
  struct s1 f2;
};

struct s4 {
  float myfloat;
  short int myint;
};

typedef struct s4 t4;

struct s5 {
  unsigned long long bla;
};

struct s6 {
  struct s5* alot[10];
};

typedef struct s7 {
  int f1;
} s7_t;

struct s8 {
  s7_t f1;
}

/* No functions */




#endif /* NIFTY_STRUCTS_TEST */
