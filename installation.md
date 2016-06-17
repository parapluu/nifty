---
layout: page
permalink: /install/
title: Install Nifty
tags: [about,tutorial]
#image:
#  feature: nifty_process.png
---

## Dependencies
+ Erlang OTP 17.0 or newer (if you are using an older version, you can use an <a href="https://github.com/parapluu/nifty/archive/pre-otp-17.tar.gz">old version of Nifty</a>)
+ **libclang** including the header files (libclang-dev)
+ **llvm**

In Ubuntu you can install the dependencies with this command:

{% highlight bash %}
sudo apt-get install libclang-dev libclang1 llvm
{% endhighlight %}

## Installation
Clone Nifty from the official repository, change to the directory and run make:

{% highlight bash %}
git clone https://github.com/parapluu/nifty.git
cd nifty/
make
{% endhighlight %}

Now include Nifty in your ERL_LIBS path and you are ready to go.
