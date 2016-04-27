---
layout: post
title: "More Dirty Scheduler Support"
modified: 2016-04-27 15:33:46 +0200
category: []
tags: [news]
image:
  feature:
  credit:
  creditlink:
comments:
share:
---
You can now specify dirty schedulers on a function level. To do so you need to add `functions_options` to the `nifty` options:

{% highlight erlang %}
1> CompileOptions = nifty_utils:add_sources(["long_running.c"], []),
   NiftyOptions = [{nifty, [{functions_options, [{"func1", [schedule_dirty_io]},
                                                 "func2", [schedule_dirty_cpu]]}]}],
   nifty:compile("long_running.h",
                 long_running,
                 CompileOptions ++ NiftyOptions).
{% endhighlight %}

If the global `schedule_dirty` option is used, then the function options overwrite the global option.
