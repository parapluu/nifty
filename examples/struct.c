typedef struct {
	void* ptr;
} foo;

struct bla {
	foo f1;
	foo *f2;
	struct {
		int i; 
		int j;
		struct {
			int foo;
			int bar;
		} anno;
	} ff;
	int q;
	long int w;
	char* e;
};

typedef struct bla tbla;
typedef int myint;


tbla* something(tbla* foo, long no) {
	return foo;
}

struct{int r1; int r2;} * bar(struct {int i; int j;} *in) {
	return (void*)0; 
}

