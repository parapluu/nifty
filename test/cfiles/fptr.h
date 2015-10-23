extern void f1(int a, int b);

extern int (*(*f2(int a, int aa))(int b, int bb))(int c, int cc);

extern int (*f3(int a, int z))(int (*b)(int c, int d), int g);

extern void f4(char g, int (*a)(int x, int z));

#include <stdarg.h>

struct s_fptr {
        void (*field1)(va_list mylist);
};
