---
layout: page
permalink: /tutorial1/
title:  "Tutorial 1: First Steps"
date:   2014-04-04 11:55:46
tags: [tutorial]
image:
  feature: nifty_trouble.png
---

<a  href="{{ site.url }}/files/tut1.tar.gz">**Files For This Tutorial**</a>

## A simple Example

Let's say you want to create a NIF module from the following example C file `fib.c`:

{% highlight C++ %}
int
fib(int n) {
  if (n<=2) {
    return 1;
  } else {
    return fib(n-1) + fib(n-2);
  }
}
{% endhighlight %}

Nifty needs a header file to create a NIF module, so let's write one and store it under `fib.h`

{% highlight C++ %}
extern int fib(int n);
{% endhighlight %}

Now we can create a NIF module, using Nifty:

{% highlight erlang %}
1> nifty:compile("fib.h", fib, []).
generating fib.h -> fib_nif.c fib.erl 
==> fib (compile)
Compiled src/fib_remote.erl
Compiled src/fib.erl
Compiling c_src/fib_nif.c
ok 
{% endhighlight %}

Now let's try to load the module:

{% highlight erlang %}
2> l(fib).
=ERROR REPORT==== 4-Apr-2014::14:30:16 ===
The on_load function for module fib returned 
{error,
 {load_failed,
  "Failed to load NIF library: '.../fib/priv/fib_nif.so: undefined symbol: fib'"}}
{% endhighlight %}

The loader reports, that it cannot find the function fib. What happened? When we generated the NIF, we forgot to tell
Nifty that it should also compile `fib.c`:

{% highlight erlang %}
3> nifty:compile("fib.h", fib, nifty_utils:add_sources(["fib.c"], [])).
generating fib.h -> fib_nif.c fib.erl 
==> fib (compile)
Compiled src/fib_remote.erl
Compiled src/fib.erl
Compiling c_src/fib_nif.c
Compiling .../fib.c
ok
4> l(fib).
{module,fib}
5> fib:fib(10).
55
{% endhighlight %}

The third arguments specifies the compile options which are equivalent to the options that you can set in a `rebar.config`. 
To add a source file or compile options to your NIF module, you have to use the target
`{".*", "$NIF", [<source files>], <optional options>}` in the port_specs option. Fortunatley, Nifty comes with 
some utility functions, that help creating such a configuration. We want to add a source file to an empty configuration. an empty 
configuration is `[]` and we can add the source `fib.c` with `nifty_utils:add_sources`.

When you leave the interactive interpreter, you will see that Nifty created a complete Erlang project:

```
fib
├── c_src
│   └── fib_nif.c
├── ebin
│   └── fib.app
├── include
│   └── fib.hrl
├── priv
├── rebar.config
└── src
    ├── fib_remote.erl
    └── fib.erl
```


| <a  href="{{ site.url }}/files/tut1.tar.gz">Tutorial Files</a> | <a  href="{{ site.url }}/tutorial2">Next Tutorial</a> |
|----------------------------------------------------------------|-------------------------------------------------------|


