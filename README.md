[![Travis][travis badge]][travis]
[![Erlang Versions][erlang versions badge]][erlang]
[![Clang Versions][clang versions badge]][clang]
[![Last Commit][commit badge]][commit]


# Nifty - Erlang NIF Wrapper Generator

Nifty is an interface generator that allows the use of C modules from Erlang.

Web page: [http://parapluu.github.io/nifty/]

## A Simple Example

Let's say we have two C files `mylib.h` and `mylib.c` which we want to use in our Erlang application:

```C
/* mylib.h */
extern int fib(int n);
```

```C
/* mylib.c */
int
fib(int n) {
  if (n <= 2) {
    return 1;
  } else {
    return fib(n-1) + fib(n-2);
  }
}

```

We can generate a NIF interface and use it from Erlang with the following command:

```Erlang
nifty:compile("mylib.h", mylib, nifty_utils:add_sources(["mylib.c"], [])).
5 = mylib:fib(5).
```

***nifty:compile/3*** gets as its first argument a header or interface
file and tries to generate an interface for all specified functions.
The second argument specifies the Erlang module the interface is about
to use and the third argument is used for additional options.  These
options are compatible with the ones rebar uses to compile NIF
modules.  In fact, Nifty uses rebar to compile the generated interface.

## Installation
After successfully cloning enter the following commands

```shell
make
```

and include Nifty in your ERL_LIBS path.

## Unit Tests
Run the following command to check that everything works correct:

```shell
make tests
```

## Dependencies
* The [clang](http://clang.llvm.org/) compiler and **libclang**, including the header files.
* **[Optional]** [PropEr](https://github.com/manopapad/proper) for the unit tests.

## Base Types

| C Types                        | Erlang Types                | Nifty Type Name
|--------------------------------|-----------------------------|--------------------------
| ```signed int``` or ```int```  | ```integer()```             | ```nifty.int```
| ```unsigned int```             | ```integer()```             | ```nifty.unsigned int```
| ```char```                     | ```integer()```             | ```nifty.char```
| ```short```                    | ```integer()```             | ```nifty.short```
| ```long```                     | ```integer()```             | ```nifty.long```
| ```long long```                | ```integer()```             | ```nifty.long long```
| ```float```                    | ```float()```               | ```nifty.float```
| ```double```                   | ```float()```               | ```nifty.double```
| ```<type> *```                 | ```{integer(), string()}``` | ```<module>.<type> *```

## Known Limitations
* Function pointers are only partially supported.
* There is no support for anonymous structs and unions.
* Functions using unsupported types are not translated and a warning is issued.
* Functions with a variable number of arguments (`va_list` or `...`) are not supported. If `va_list` as type is used, Nifty will print a warning. If `...` is used, then the function is translated **without** the variable arguments: `int printf(const char *format, ...)` will be translated into `printf/1`.
* The usage of incomplete types is limited.
* Nifty has not been tested under Windows.

<!-- Links (alphabetically) -->
[clang]: http://clang.llvm.org
[commit]: https://github.com/parapluu/nifty/commit/HEAD
[erlang]: http://www.erlang.org
[travis]: https://travis-ci.org/parapluu/nifty

<!-- Badges (alphabetically) -->
[clang versions badge]: https://img.shields.io/badge/clang-3.5.2%20to%207.0.0-ff69b4.svg?style=flat-square
[commit badge]: https://img.shields.io/github/last-commit/parapluu/nifty.svg?style=flat-square
[erlang versions badge]: https://img.shields.io/badge/erlang-18.0%20to%2021.2-blue.svg?style=flat-square
[travis badge]: https://img.shields.io/travis/parapluu/nifty.svg?branch=master?style=flat-square
