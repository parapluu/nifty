---
layout: post
title: "Support for dirty schedulers"
modified: 2014-10-27 15:30:22 +0100
category: [news]
tags: [fetures,news]
image:
  feature: 
  credit: 
  creditlink: 
comments: 
share: 
---
Nifty now has experimental support for dirty schedulers. Just add `schedule_dirty` to your `nifty` options:

{% highlight erlang %}
1> nifty:compile("long_running.h",
                 long_running,
				 nifty_utils:add_sources(["long_running"],
				                         [{nifty, [schedule_dirty]}])).
{% endhighlight %}

If you don't have dirty schedulers supported in your OTP yet, then Nifty will give you a warning and compile
without using them.

