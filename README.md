# Nifty - Erlang Native Interface Generator

Nifty is an interface generator that allows you to use C modules from Erlang.

## A simple Example

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
nifty_compiler:compile("mylib.h", mylib, [
    			   {port_specs,
    			     [{
    						".*",
    						"$NIF",	
    						["mylib.c"]
    				}]
    		}]

5 = mylib:fib(5)
```

***compiler/3*** reads as the first argument a header or interface file and tries to generate an interface for all 
specified functions. The second argument specifies the Erlang module the interface is about to use and the third argument is used for additional options. These option are compatible with the ones rebar uses to compile NIF modules. 
In fact, Nifty uses rebar to compile the generated interface.

## Installation
After succesfully cloning enter the following commands

```
./rebar get-deps
./rebar compile
```

and include Nifty in your ERL_LIBS path.

## Running Unit Tests
Run the following command to check that everything works correct:
```
./rebar clean compile eunit skip_deps=true
```

**libclang.so** has to be in your search-path and nifty and it's dependencies have to be in you **ERL_LIBS** path.

```
 LD_LIBRARY_PATH=<PATH_TO_LIBCLANG> ERL_LIBS=<PATH_TO_NIFTY>:<PATH_TO_NIFTY>/deps ./rebar clean compile eunit skip_deps=true
```

### Dependencies
+ **libclang** including the header files
+ **clang** compiler

### Types

| C Types                                  | Erlang Types                 |
|------------------------------------------|------------------------------|
| ```[unsigned/signed] [short/long] int``` | ```integer()```              |
| ```float```                              | ```float()```                |
| ```double```                             | ```float()```                |
| ```<type> *```                           | ```{integer(), string()}```  |
| ```struct name { ... }```                | erlang record                |

Pointers of types can be optained by Nifty's **pointer_of/2** method, which takes a value and a type (stirng()) and returns a pointer to this value*.

*This currently works only with base types and not with structs.

### Limitations
+ So far there is no support for unions and anonymous struct. However, Nifty tries to recover from types that it cannot translate and prints an warning (r) during compilation. 
+ Variable arguments of functions (**va_list** or **...**) is not supported. If **va_list** as type is used, Nifty will print a warning. If **...** is used, then the function is translated **without** the variable arguments: **int printf(const char *format, ...)** will be translated into **printf/1**
+ The header files must be self contained which limits the usage of incomplete types. 
+ There is currently no nice way of using arrays although **nifty:mem_alloc/1** and **nifty:mem_read/1** allow basic usage. 
+ Nifty has not been tested under Windows or 32 bit.
