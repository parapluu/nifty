---
layout: page
permalink: /tutorialc/
title:  "Tutorial C: Creating NIFs for Contiki"
date:   2014-04-04 11:55:46
tags: [tutorial]
image:
  feature: nifty_trouble.png
---

## Compiling
Nifty is able to create bindings for precompiled static libraries. The only restriction is, that the 
library must be compiled with the `-fpic` flag. Contiki's build system is capable of creating such a 
static library. 

```
$ make TARGET=native contiki-native.a
```

This should work with your standard Contiki Makefile. A library created like this, is created without 
the `-fPIC` flag. This can be fixed by adding

```
CFLAGS += -fPIC
```

to your Makefile. 

You can now create a NIF module with Nifty. Note, that in this example make is invoked as pre-compile hook:

{% highlight erlang %}
1> nifty:compile("list.h", conli, [{port_specs,[{".*","$NIF",[],
					 [{env,[{"LDFLAGS","$LDFLAGS contiki-native.a"}]}]}]},
				   {pre_hooks,[{compile,"sh -c \"make -f $NIFTY_ROOT/Makefile TARGET=native contiki-native.a\""}]}]).

{% endhighlight %}

| <a  href="{{ site.url }}/tutorial3">Previous Tutorial</a> | <a  href="{{ site.url }}/tutorial_trouble">Next Tutorial</a> |
|-----------------------------------------------------------|--------------------------------------------------------------|

