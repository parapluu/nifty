.PHONY: default fast all get-deps compile dialyzer tests clean mrproper

ERL_INCLUDE = $(PWD):$(ERL_LIBS)

ifneq (,$(findstring Windows,$(OS)))
    SEP := $(strip \)
else
    SEP := $(strip /)
endif

BEAMS = ebin$(SEP)nifty_clangparse.beam \
	ebin$(SEP)nifty.beam \
	ebin$(SEP)nifty_compiler.beam \
	ebin$(SEP)nifty_filters.beam \
	ebin$(SEP)nifty_rebar.beam \
	ebin$(SEP)nifty_tags.beam \
	ebin$(SEP)nifty_types.beam \
	ebin$(SEP)nifty_utils.beam

REBAR := .$(SEP)rebar

default: fast

fast: get-deps compile

all: default dialyze tests

get-deps:
	$(REBAR) get-deps

compile:
	$(REBAR) compile

dialyze: compile
	dialyzer -n -nn -Wunmatched_returns ebin $(find .  -path 'deps/*/ebin/*.beam')

fdialyze: compile
	dialyzer -n -nn -Wunmatched_returns $(BEAMS)

tests: compile
	ERL_LIBS=$(ERL_INCLUDE) $(REBAR) clean compile eunit skip_deps=true

doc:
	$(REBAR) doc skip_deps=true

clean:
	$(REBAR) clean

mrproper: clean
	rm -rf deps/
