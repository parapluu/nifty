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

DIALYZER_APPS = erts kernel stdlib compiler crypto tools
DIALYZER_FLAGS = -Wunmatched_returns -Wunderspecs

default: fast

fast: get-deps compile

all: default tests dialyze

get-deps:
	$(REBAR) get-deps

compile:
	$(REBAR) compile

dialyze: compile .nifty_plt
	dialyzer --plt .nifty_plt $(DIALYZER_FLAGS) ebin

fdialyze: compile .nifty_plt
	dialyzer -n -nn --plt .nifty_plt $(DIALYZER_FLAGS) $(BEAMS)

.nifty_plt:
	-dialyzer --build_plt --output_plt $@ --apps $(DIALYZER_APPS) deps/*/ebin

tests: compile
	ERL_LIBS=$(ERL_INCLUDE) $(REBAR) clean compile eunit skip_deps=true

rebar_regression: compile
	erl -noshell -pa `pwd`/ebin -pa `pwd`/deps/*/ebin \
	-eval 'erlang:halt(try nifty:compile("test/cfiles/types.h",foo,[]),0 catch _:_ -> io:format("~p~n",[erlang:get_stacktrace()]),1 end)'

doc:
	$(REBAR) doc skip_deps=true

clean:
	$(REBAR) clean

mrproper: clean
	rm -rf deps/
