# Nifty - Erlang Native Interface Generator

Nifty is an interface generator that allows you to use C modules from inside Erlang.

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

### Dependencies
+ **libclang** including the header files
+ **clang** compiler

### Limitations
So far there is no support for unions and anonymous struct. There is no nice way of using arrays although **nifty:mem_alloc/1** and **nifty:mem_read/1** allow to use them on a basic level. 
