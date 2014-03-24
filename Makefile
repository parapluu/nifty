.PHONY: default fast all get-deps compile dialyzer tests clean

ifneq (,$(findstring Windows,$(OS)))
    SEP := $(strip \)
else
    SEP := $(strip /)
endif

REBAR := .$(SEP)rebar

default: fast dialyzer

fast: get-deps compile

all: default #tests

get-deps:
	$(REBAR) get-deps

compile:
	$(REBAR) compile

dialyzer: compile
	dialyzer -n -nn -Wunmatched_returns ebin $(find .  -path 'deps/*/ebin/*.beam')

#tests: compile
#	$(REBAR) eunit

clean:
	$(REBAR) clean
