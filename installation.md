---
layout: page
permalink: /install/
title: Install Nifty
tags: [about,tutorial]
#image:
#  feature: nifty_process.png
---

## Dependencies
+ Erlang OTP 17.0 (if you are using an older version, you can use <a href="https://github.com/parapluu/nifty/archive/pre-otp-17.tar.gz">this</a> release of Nifty)
+ **libclang** including the header files (libclang-dev)
+ **clang** compiler
+ <a href="http://proper.softlab.ntua.gr/">**PropEr**</a> for the unit tests

In Ubuntu you can install the dependencies with this command:

{% highlight bash %}
sudo apt-get install libclang-3.4-dev libclang1-3.4 clang-3.4
{% endhighlight %}

You can set `CLANG_INCLUDE` and `CLANG_LIBRARY` to point to `libclang.so` and its header files.

## Installation
Clone Nifty from the official repository, change to the directory and run make:

{% highlight bash %}
git clone https://github.com/parapluu/nifty.git
cd nifty/
make
{% endhighlight %}

Now include Nifty in your ERL_LIBS path and you are ready to go.

## Unit Tests
Make sure, that you have included <a href="http://proper.softlab.ntua.gr/">PropEr</a> in your **ERL_LIBS** path.

Run the following command to check that everything works correct:

{% highlight bash %}
make tests
{% endhighlight %}
