[![Build Status](https://travis-ci.org/parapluu/nifty.svg?branch=master)](https://travis-ci.org/parapluu/nifty)

# Nifty - Erlang NIF Wrapper Generator

Nifty is an interface generator that allows you to use C modules from Erlang.

Webpage: [http://parapluu.github.io/nifty/]

## A Simple Example

Let's say we have two C files **mylib.h** and **mylib.c** which we want to use in our Erlang application:

```C
/* mylib.h */
extern int fib(int n);

/* mylib.c */
int
fib(int n) {
  if (n<=2) {
    return 1;
  } else {
    return fib(n-1) + fib(n-2);
  }
}

```

We can generate a NIF interface and use it from from Erlang with the following command:

```Erlang
nifty:compile("mylib.h", mylib,
              nifty_utils:add_sources(["mylib.c"], [])).
5 = mylib:fib(5).
```

***compiler/3*** reads as the first argument a header or interface file and tries to generate an interface for all 
specified functions. The second argument specifies the Erlang module the interface is about to use and the third argument is used for additional options. These option are compatible with the ones rebar uses to compile NIF modules. 
In fact, Nifty uses rebar to compile the generated interface.

## Installation
After succesfully cloning enter the following commands

```
make
```

and include Nifty in your ERL_LIBS path.

## Unit Tests
Run the following command to check that everything works correct:

```
make tests
```

Make sure, that you have included <a href="http://proper.softlab.ntua.gr/">PropEr</a> in your **ERL_LIBS** path.

## Dependencies
+ **libclang** including the header files
+ **clang** compiler
+ **PropEr** for the unit tests

## Base Types

| C Types                                  | Erlang Types                 | Nifty Type Name
|------------------------------------------|------------------------------|---------------------------
| ```signed int``` or ```int```            | ```integer()```              | ```nifty.int```
| ```unsigned int```                       | ```integer()```              | ```nifty.unsigned int```
| ```char```                               | ```integer()```              | ```nifty.char```
| ```short```                              | ```integer()```              | ```nifty.short```
| ```long```                               | ```integer()```              | ```nifty.long```
| ```long long```                          | ```integer()```              | ```nifty.long long```
| ```float```                              | ```float()```                | ```nifty.float```
| ```double```                             | ```float()```                | ```nifty.double```
| ```<type> *```                           | ```{integer(), string()}```  | ```<module>.<type> *```

## Limitations
+ unions, enums and function pointers are not supported and Nifty will not be able to translate them correctly.
+ So far there is no support for anonymous struct. However, Nifty tries to recover from types that it cannot translate and prints an warning (r) during compilation. 
+ Variable arguments of functions (**va_list** or **...**) is not supported. If **va_list** as type is used, Nifty will print a warning. If **...** is used, then the function is translated **without** the variable arguments: **int printf(const char *format, ...)** will be translated into **printf/1**
+ The header files must be self contained which limits the usage of incomplete types. 
+ There is currently no nice way of using arrays although **nifty:mem_alloc/1** and **nifty:mem_read/1** allow basic usage.
+ Nifty has not been tested under Windows or 32 bit.
+ The C-Code Nifty creates contains many empty lines. If you want to inspect the Code or modify it, you should use a tool like astyle (with option -xe) to format the generated code. 

