TARGETS = ../priv/nifty.so ../priv/nifty_clangparse.so

CURDIR := $(shell pwd)
BASEDIR := $(abspath $(CURDIR)/..)

ERTS_INCLUDE_DIR ?= $(shell erl -noshell -s init stop -eval "io:format(\"~s/erts-~s/include/\", [code:root_dir(), erlang:system_info(version)]).")
ERL_INTERFACE_INCLUDE_DIR ?= $(shell erl -noshell -s init stop -eval "io:format(\"~s\", [code:lib_dir(erl_interface, include)]).")
ERL_INTERFACE_LIB_DIR ?= $(shell erl -noshell -s init stop -eval "io:format(\"~s\", [code:lib_dir(erl_interface, lib)]).")

C_SRC_DIR = $(CURDIR)

# System type and C compiler/flags.

UNAME_SYS := $(shell uname -s)
ifeq ($(UNAME_SYS), Darwin)
	CC ?= cc
	CFLAGS ?= -O3 -std=c99 -arch x86_64 -finline-functions -Wall -Wmissing-prototypes
	LDFLAGS ?= -arch x86_64 -flat_namespace -undefined suppress
else ifeq ($(UNAME_SYS), FreeBSD)
	CC ?= cc
	CFLAGS ?= -O3 -std=c99 -finline-functions -Wall -Wmissing-prototypes
else ifeq ($(UNAME_SYS), Linux)
	CC ?= gcc
	CFLAGS ?= -O3 -std=c99 -finline-functions -Wall -Wmissing-prototypes
endif

# LLVM config
ifdef LLVM_VERSION
	NIFTY_LLVM_CONFIG := llvm-config-$(NIFTY_LLVM_VERSION)
else
	NIFTY_LLVM_CONFIG := llvm-config
endif

CFLAGS += -fPIC -I $(ERTS_INCLUDE_DIR) -I $(ERL_INTERFACE_INCLUDE_DIR)
CFLAGS_CP = $(CFLAGS) `$(BASEDIR)/travis/safe-cflags.sh $(NIFTY_LLVM_CONFIG)`

LDLIBS += -L $(ERL_INTERFACE_LIB_DIR) -lerl_interface -lei
LDLIBS_CP = $(LDLIBS) -lclang `$(NIFTY_LLVM_CONFIG) --ldflags`
LDFLAGS += -shared

# Verbosity.

c_verbose_0 = @echo " C     " $(?F);
c_verbose = $(c_verbose_$(V))

link_verbose_0 = @echo " LD    " $(@F);
link_verbose = $(link_verbose_$(V))

SOURCES := $(shell find $(C_SRC_DIR) -type f \( -name "*.c" -o -name "*.C" -o -name "*.cc" -o -name "*.cpp" \))
OBJECTS = $(addsuffix .o, $(basename $(SOURCES)))

COMPILE_C = $(c_verbose) $(CC) $(CFLAGS) $(CPPFLAGS) -c
COMPILE_C_CP = $(c_verbose) $(CC) $(CFLAGS_CP) $(CPPFLAGS) -c

all: $(TARGETS)

../priv/nifty.so: nifty.o
	@mkdir -p $(BASEDIR)/priv/
	$(link_verbose) $(CC) $< $(LDFLAGS) $(LDLIBS) -o $@

../priv/nifty_clangparse.so: nifty_clangparse.o
	@mkdir -p $(BASEDIR)/priv/
	$(link_verbose) $(CC) $< $(LDFLAGS) $(LDLIBS_CP) -o $@

nifty.o: nifty.c
	$(COMPILE_C) $<

nifty_clangparse.o: nifty_clangparse.c
	$(COMPILE_C_CP) $<

clean:
	@rm -f $(OBJECTS)
