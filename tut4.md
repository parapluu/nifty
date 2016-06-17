---
layout: page
permalink: /tutorial4/
title:  "Tutorial 4: Nifty Types"
date:   2014-04-04 11:55:46
tags: [tutorial]
image:
  feature: nifty_trouble.png
---

The main task of Nifty is to generate the type conversion code that translates
Erlang types into C types and vice versa. Nifty provides support for most of
C datatypes with a few notable exceptions. The type information is stored
in the module defining the type, with the exception of base types, which are
stored in Nifty's library module. Nifty's type representation is in form of a
string:

{% highlight Erlang %}
"<module>.<type>"
examples:
"nifty.int" %% base type
"nifty.long"
"mymodule.struct mystruct *" %% derived type
{% endhighlight %}

## Base Types
The following table shows, how C types and Erlang types correspond to each other:

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

Nifty performs no check that integer values fit into the datatype. Too large integers will overflow.

## Records
C records (structs) are translated into erlang records and every definition of a struct results
in a record definition. Nifty translates the values of each field according to the type
of the C record definition. Nested C records are translated into nested Erlang records.
Nifty stores the record definitions into an `hrl` file which can be easily included to use
record notion.

Hint: Nifty currently does not supports anonymous structs. They are excluded from the
internal type database together with all functions that depend on them.

## Enums
Nifty supports `enum` types. You can either use the numerical value directly or get the
numerical value associated with a given alias by calling `nifty:enum_value(module, "ALIAS")`.

See <a  href="{{ site.url }}/news/support-for-enums/">this</a> blog post for more information.

## Pointer

### Getting A Pointer To A Value
To get a pointer of a value Nifty provides the `nifty:pointer_of/2` function. The first argument
is the value and the second argument is the Nifty Type Name of the base type:

{% highlight Erlang %}
1> nifty:pointer_of(42, "nifty.int").
{140077923569704,"nifty.int *"}
{% endhighlight %}

For base types it is possible to omit the module name:

{% highlight Erlang %}
2> nifty:pointer_of(42, "long long").
{139660089364456,"nifty.long long *"}
{% endhighlight %}

Nifty tries to find the type specification in the given module. If the specification is found,
Nifty allocates memory according to the corresponding C type and stores the value in this memory
area. A pointer to this memory area is then returned annotated with the correct type information.

In order to get a pointer to a pointer, it is possible to omit the type information. Nifty
will then use the type information stored in the pointer:

{% highlight Erlang %}
3> P = nifty:pointer_of(24, "unsigned int").
{140097513065576,"nifty.unsigned int *"}
4> nifty:pointer_of(P).
{140097513065640,"nifty.unsigned int **"}
{% endhighlight %}

If you need a pointer to a preallocated memory area for a certain type, but you do not care about
the containing value, you can use `pointer/1`, which works like `pointer_of` without initialization:

{% highlight Erlang %}
5> Ptr = nifty:pointer("int").
{139715240792040,"nifty.int *"}
6> nifty:dereference(Ptr).
45350912 %% this is a random value
{% endhighlight %}

This will allocate memory of the size `sizeof(type)` and return a pointer casted to the specified type.

### Dereferencing A Pointer
The reverse function to `nifty:pointer_of/2` is `nifty:dereference`. This function takes a pointer
and translates the value it points to into an Erlang value:

{% highlight Erlang %}
1> Ptr = nifty:pointer_of(42, "int").
{139766325055464,"nifty.int *"}
2> nifty:dereference(Ptr).
42
{% endhighlight %}

This translation happens according to the type of the pointer:

{% highlight Erlang %}
3> Ptr = nifty:pointer_of(1000, "unsigned int").
{140313231101928,"nifty.unsigned int *"}
4> Ptr_Char = nifty:as_type(Ptr, "nifty.unsigned char *").
{140313231101928,"nifty.unsigned char *"}
5> nifty:dereference(Ptr).
1000
6> nifty:dereference(Ptr_Char).
232
{% endhighlight %}

### Casting A Pointer To A Different Type
As shown in the previous section, it is possible to cast a pointer to a different type. `nifty:as_type/2`
takes a pointer and a type and returns that pointer casted to the type. `as_type/2` also checks
if the type is known:

{% highlight Erlang %}
1> Ptr = nifty:pointer("int").
{139715240792040,"nifty.int *"}
3> nifty:as_type(Ptr, "nifty.char").
{139715240792040,"nifty.char"}
4> nifty:as_type(Ptr, "nifty.unknown").
undef %% type is unknown in the scope of nifty
{% endhighlight %}

### Function Pointers
Function pointers default to `void *` pointers and are handled as such. That means, that you are not able to dereference
them. You can however obtain them from functions or pass them to other functions.

## Memory Management

### Memory Allocation And free()
Nifty's `malloc()` is `nifty:malloc/1`. It works in the same way as `malloc()` as it allocates the
specified amount of memory and returns a `nifty.void *` pointer to it. In a similar manner Nifty's `free()`
is `nifty:free/1`, which deallocates the memory segment associated by the given pointer:

{% highlight Erlang %}
1> Big = nifty:malloc(1024).
{140078075875304,"nifty.void *"}
2> nifty:free(Big).
ok
{% endhighlight %}

All of by Nifty allocated memory is not garbage collected, so make sure to free all memory you used.

## Segfaults
Be careful when you are using pointers, memory allocation and so on. Like in C it is possible to access
invalid or protected memory areas that result in a memory access exception more commonly known as
segfault:

{% highlight Erlang %}
8> nifty:dereference({0, "nifty.int *"}).
Segmentation fault (core dumped)
$
{% endhighlight %}

As you will see, a segfault will crash the Erlang runtime. You can however catch segafaults as demonstrated in the
<a  href="{{ site.url }}/tutorial5">next tutorial</a>.


<table>
<tr>
<th><a  href="{{ site.url }}/tutorial3">Previous Tutorial</a> </th>
<th><a  href="{{ site.url }}/tutorial5">Next Tutorial</a></th>
</tr>
</table>
