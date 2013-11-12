struct foo {
	char* ptr;
};

struct bla {
	struct foo f1;
	struct foo *f2;
	int q;
	int w;
	char* e;
};

// typedef struct bla tbla;
// typedef int myint;
// 
// 
// tbla* something(tbla* foo, long no) {
// 	return foo;
// }
// 
// struct{int r1; int r2;} * bar(struct {int i; int j;} *in) {
// 	return (void*)0; 
// }
// 
