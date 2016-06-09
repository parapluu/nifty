#!/bin/sh
echo $LLVM_DIR
which llvm-config
echo "LDFLAGS:"
llvm-config --ldflags
echo "CFLAGS:"
travis/safe-cflags.sh llvm-config
echo "GLIBCXX"
strings /usr/lib/x86_64-linux-gnu/libstdc++.so.6  | grep GLIBCXX
echo "env"
env
