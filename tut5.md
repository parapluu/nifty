---
layout: page
permalink: /tutorial5/
title:  "Tutorial 5: Dealing With Crashes"
date:   2014-04-04 11:55:46
tags: [tutorial]
image:
  feature: nifty_trouble.png
---

<a  href="{{ site.url }}/files/tut5.tar.gz">**Files For This Tutorial**</a>

## Setup

Let's say you want to create a NIF module from the following example C file `test.c`:

{% highlight C++ %}
#include "test.h"
#include "signal.h"

int
works()
{
  return 42;
}

void
fails()
{
  raise(SIGSEGV);
}
{% endhighlight %}

The header file `test.h` looks like this:

{% highlight C++ %}
extern int works();
extern void fails();
{% endhighlight %}

We can now create the NIF module as learned in <a href="{{ site.url }}/tutorial1">Tutorial 1</a>:

{% highlight erlang %}
1> nifty:compile("test.h", test, [{port_specs, [{".*","$NIF",["test.c"]}]}]).
generating test.h -> test_nif.c test.erl 
==> test (compile)
Compiled src/test_remote.erl
Compiled src/test.erl
Compiling c_src/test_nif.c
ok
{% endhighlight %}

## Segfault And Other Crashes

{% highlight erlang %}
2> test:works().
42
3> test:fails().
Segmentation fault (core dumped)
$ 
{% endhighlight %}

Now this is expected, since a segfault crashes the whole system process, but it breaks with the fault-tolerant nature of Erlang. Nifty
allows you to call the created NIF modules without crashing the process. For every NIF module, an additional module with the postfix `_remote` 
is created. This module exports the same functions as the NIF module plus `start\0`, `stop\0` and `restart\0`. Nifty starts a node in an external
process, which handles the NIF module:

{% highlight erlang %}
2> test_remote:start().
ok
(p33_p0_master@cola-light)3> test_remote:works().
42
(p33_p0_master@cola-light)4> test_remote:fails().
{error,node_crashed}
(p33_p0_master@cola-light)5> test_remote:stop().
ok
6> 
{% endhighlight %}

As you can see, `fails\0` now returns `{error, node_crashed}` instead of crashing Erlang. After the remote node has crashed, it is necessary to 
restart it with `restart\0`. Otherwise every function call will return `{error, node_down}`. You can also stop the remote node with `stop\0`. 
Restarting the or stopping is a hard reset of the NIF module. Static C variables and allocated memory will behave as you freshly
loaded the NIF module. This means, that all pointers that you still have are invalid. 

| <a  href="{{ site.url }}/tutorial4">Previous Tutorial</a> | <a  href="{{ site.url }}/files/tut5.tar.gz">Tutorial Files</a> | <a  href="{{ site.url }}/tutorial_trouble">Next Tutorial</a> |
|-----------------------------------------------------------|----------------------------------------------------------------|--------------------------------------------------------------|
