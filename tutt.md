---
layout: page
permalink: /tutorial_trouble/
title:  "Troubleshooting"
date:   2014-04-04 11:55:46
tags: [tutorial]
image:
  feature: nifty_trouble.png
---
# Missing Clang Library

{% highlight erlang %}
2> nifty:compile("fib.h", fib, [{port_specs, [{".*","$NIF",["fib.c"]}]}]).
generating fib.h -> fib_nif.c fib.erl 

=ERROR REPORT==== 4-Apr-2014::14:43:36 ===
The on_load function for module nifty_clangparse returned 
{error,
 {load_failed,
  "Failed to load NIF library .../nifty_clangparse: 'libclang.so: cannot open shared object file: No such file or directory'"}}
** exception error: undefined function nifty_clangparse:parse/1
     in function  nifty_compiler:render/4 (src/nifty_compiler.erl, line 15)
     in call from nifty_compiler:compile/3 (src/nifty_compiler.erl, line 114)
{% endhighlight %}

Nifty uses libclang to analyze the header files but the library is not found. Point your `LD_LIBRARY_PATH` environment variable to the folder containing `libclang.so`

# Anonymous Structs

{% highlight C++ %}
// anonymous struct; ignored by Nifty
// my_t gets removed since it depends 
// on the annoymous struct
typedef struct {
   int a;
   int b;
} my_t;

// named struct and typedef; gets removed since it depends on my_t
typedef struct S {
  my_t x;
  my_t y;
} smth_t;

// gets removed since it depends on types that get are unresolved
extern void missing_function(S in);
{% endhighlight %}

Nifty currently does not support anonymous structs. This means, that they are ignored while parsing the header files. Since
Nifty checks that the type information for the functions is complete and removes incomplete definition, functions depending
on anonymous functions are also removed. The same is true for incomplete types. An exception are pointers to those types.
While Nifty will not be able to dereference those pointers, it can handle them as pointers and pass or get them from functions.

To solve the issue with anonymous functions, just give them a name.

# Function Pointer and Callback Functions
Nifty handles function pointers by defaulting their type to `void *`. This means, that no typechecking or advanced handling
(like calling a function associated to a function pointer) are supported. However, function pointers can like any other pointer
be optained and passed to other functions. 

# Variable Argument lists
Nifty currently does not support variable argument lists of C functions. The variabel arguments are either missing (...) or
the function is not wrapped (va_list).
