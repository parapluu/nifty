---
layout: post
title: "Support for enums"
modified: 2015-10-21 09:44:41 +0200
category: [news]
tags: [news,enum,feature]
image:
  feature:
  credit:
  creditlink:
comments:
share:
---
Nifty has now support for `enum`s. Lets wrap the following code into the
module `my_module`:

{% highlight C %}
enum my_enum {
        A_VALUE,
        ANOTHER_VALUE = 100
};

void function_using_an_enum(enum my_enum parameter);
{% endhighlight %}

Now we can call the function using `enum`s as following:

{% highlight erlang %}
> my_module:function_using_an_enum(0).
ok
> nifty:enum_value(my_module, "ANOTHER_VALUE")
100
> my_module:function_using_an_enum(nifty:enum_value(my_module, "A_VALUE")).
ok

{% endhighlight%}
