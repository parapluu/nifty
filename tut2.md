---
layout: page
permalink: /tutorial2/
title:  "Tutorial 2: Compiling and Linking"
date:   2014-04-04 11:55:46
tags: [tutorial]
image:
  feature: nifty_trouble.png
---
<a  href="{{ site.url }}/files/tut2.tar.gz">**Files For This Tutorial**</a>

## Compile and Link Flags
In the <a href="{{ site.url }}/tutorial1">previous tutorial</a> we saw how we can
create simple NIF modules and compile them together with some C code. In reality
the process of compiling is not that simple. We might need to specify additional
paths for included files or link against a library. This tutorial shows how you
can set arbitrary compile options. 

## Setup

In this example we want to build a NIF module from the C library answer. The sources
of the library look like this:

```
.
├── include
│   ├── answer.h
│   └── types.h
├── libanswer.a
├── Makefile
└── src
    └── ...
```

We have two header files in `include/`, a `Makefile` and a static library that was built 
by invoking make. There are also a couple of C files in `src`.

## Setting Compiler Parameters
The header file `answer.h` defines one function:

{% highlight C++ %}
#include "types.h"

extern my_t life_universe_and_everything();
{% endhighlight %}

`types.h` contains the type `my_t` which `life_universe_and_everything()` uses as return type:

{% highlight C++ %}
typedef short my_t;
{% endhighlight %}

Nifty will scan all include files, but we have to tell it where to find them.
We also need to pass the same information to the C compiler when we compile the NIF module.
Typically we pass this information as parts of `CFLAGS` environment variable. The build system
takes care of the rest:

{% highlight erlang %}
1> nifty:compile("include/answer.h", 
                 answer,
                 [{port_specs,[{".*", "$NIF", [],
                               [{env,[{"CFLAGS","$CFLAGS -Iinclude/"}]}]}]}]).
{% endhighlight %}

We can add aditional compile options for our $NIF target. We set `CFLAGS` to the old `CFLAGS` plus
flag the compile needs to create the module.

In a similar way we can add options to the linker using `LDFLAGS` instead of `CFLAGS` to tell Nifty
that it should include `libanswer.a` when linking everything:


{% highlight erlang %}
1> nifty:compile("include/answer.h", 
                 answer,
                 [{port_specs,[{".*", "$NIF", [],
                               [{env,[{"CFLAGS","$CFLAGS -Iinclude/"},
                                      {"LDFLAGS", "$LDFLAGS $NIFTY_ROOT/libanswer.a"}]}]}]}]).
{% endhighlight %}

In order for Nifty to find `libanswer.a` we have to specify the correct path. Nifty does not create 
the NIF module directly in the currend working directory, but a complete package:

```
answer/
├── c_src
│   └── answer_nif.c
├── ebin
│   └── answer.app
├── include
│   └── answer.hrl
├── priv
├── rebar.config
└── src
    ├── answer_remote.erl
    └── answer.erl
```

When compiling the module Nifty changes to `answer/` and calls `rebar compile`. `rebar` now needs to find
all files from this directory. This means, that we either have to use absolute paths or relative paths starting
from `answer/`. Thanksfully Nifty provides the envrionment variable `NIFTY_ROOT` that points to the directory which you are
currently in, when you call `nifty:compile/3`. 

## Compile Hooks

In our example the library already has a build system
and the build process itself is probably not straight forward. Instead of
reinventing the wheel and reimplementing the build process, you should use compile hooks.
compile hooks are actions that are executed before or after compilation. In this example we
want to invoke `make` before compilation:

{% highlight erlang %}
1> nifty:compile("include/answer.h", 
                 answer,
                 [{port_specs,[{".*", "$NIF", [],
                               [{env,[{"CFLAGS","$CFLAGS -Iinclude/"},
                                      {"LDFLAGS", "$LDFLAGS $NIFTY_ROOT/libanswer.a"}]}]}]},
                  {pre_hooks, [{compile, "sh -c \"make\""}]}]).
{% endhighlight %}

This will result in an error, indicating that the makefile was not found:

```
make: *** No targets specified and no makefile found.  Stop.
ERROR: Command [compile] failed!
{error,compile}
```

We have to again adjust the path for make:

{% highlight erlang %}
1> nifty:compile("include/answer.h", 
                 answer,
                 [{port_specs,[{".*", "$NIF", [],
                               [{env,[{"CFLAGS","$CFLAGS -Iinclude/"},
                                      {"LDFLAGS", "$LDFLAGS $NIFTY_ROOT/libanswer.a"}]}]}]},
                  {pre_hooks, [{compile, "sh -c \"cd $NIFTY_ROOT && make\""}]}]).
{% endhighlight %}

In a similar way we can call `make clean` after we have built our NIF:

{% highlight erlang %}
1> nifty:compile("include/answer.h", 
                 answer,
                 [{port_specs,[{".*", "$NIF", [],
                               [{env,[{"CFLAGS","$CFLAGS -Iinclude/"},
                                      {"LDFLAGS", "$LDFLAGS $NIFTY_ROOT/libanswer.a"}]}]}]},
                  {pre_hooks, [{compile, "sh -c \"cd $NIFTY_ROOT && make\""}]},
                  {post_hooks, [{compile, "sh -c \"cd $NIFTY_ROOT && make clean\""}]}]).
{% endhighlight %}


| <a  href="{{ site.url }}/tutorial1">Previous Tutorial</a> | <a  href="{{ site.url }}/files/tut2.tar.gz">Tutorial Files</a> | <a  href="{{ site.url }}/tutorial3">Next Tutorial</a> |
|-----------------------------------------------------------|----------------------------------------------------------------|-------------------------------------------------------|
