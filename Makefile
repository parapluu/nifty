.PHONY: default fast all get-deps compile dialyzer tests clean

CLANG_LIBRARY = "/usr/lib/llvm-3.4/include"
CLANG_INCLUDE = "/usr/lib/llvm-3.4/lib"

ifneq (,$(findstring Windows,$(OS)))
    SEP := $(strip \)
else
    SEP := $(strip /)
endif

BEAMS = ebin$(SEP)clang_parse.beam \
	ebin$(SEP)nifty.beam \
	ebin$(SEP)nifty_compiler.beam \
	ebin$(SEP)nifty_filters.beam \
	ebin$(SEP)nifty_rebar.beam \
	ebin$(SEP)nifty_tags.beam \
	ebin$(SEP)nifty_typetable.beam \
	ebin$(SEP)nifty_utils.beam

REBAR := .$(SEP)rebar

default: fast dialyzer

fast: get-deps compile

all: default tests

get-deps:
	$(REBAR) get-deps

compile:
	CLANG_LIBRARY=$(CLANG_LIBRARY) CLANG_INCLUDE=$(CLANG_INCLUDE) $(REBAR) compile

dialyzer: compile
	dialyzer -n -nn -Wunmatched_returns $(BEAMS) $(find .  -path 'deps/*/ebin/*.beam')

tests: compile
	$(REBAR) clean compile eunit skip_deps=true

clean:
	$(REBAR) clean
