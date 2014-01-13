all: clang_parse.so nifty.so
clang_parse.so: clang_parse.c
	clang clang_parse.c -shared  -fPIC -I/usr/lib/llvm-3.4/include/ -I/usr/local/lib/erlang/usr/include/  -L/usr/lib/llvm-3.4/lib/ -lclang -o clang_parse.so
nifty.so: nifty.c
	clang nifty.c -shared  -fPIC -I/usr/local/lib/erlang/usr/include/ -o nifty.so
