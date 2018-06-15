.PHONY: default fast all get-deps compile dialyzer tests clean mrproper

ERL_INCLUDE = $(PWD):$(ERL_LIBS)

ifneq (,$(findstring Windows,$(OS)))
    SEP := $(strip \)
else
    SEP := $(strip /)
endif

BEAMS = ebin$(SEP)nifty_clangparse.beam \
	ebin$(SEP)nifty.beam \
	ebin$(SEP)nifty_filters.beam \
	ebin$(SEP)nifty_rebar.beam \
	ebin$(SEP)nifty_tags.beam \
	ebin$(SEP)nifty_types.beam \
	ebin$(SEP)nifty_utils.beam

REBAR := .$(SEP)rebar3

# nifty_root
ifndef NIFTY_ROOT_CONFIG
	NIFTY_ROOT_CONFIG := NIFTY_ROOT=$(PWD)
else
	NIFTY_ROOT_CONFIG :=
endif

# LLVM config
ifdef NIFTY_LLVM_VERSION
	LLVM_CONFIG := NIFTY_LLVM_CONFIG=llvm-config-$(NIFTY_LLVM_VERSION)
	NIFTY_LLVM_CONFIG := llvm-config-$(NIFTY_LLVM_VERSION)
	CLANG := clang-$(NIFTY_LLVM_VERSION)
else
	LLVM_CONFIG := NIFTY_LLVM_CONFIG=llvm-config
	NIFTY_LLVM_CONFIG := llvm-config
	CLANG := clang
endif

NIFTY_CPATH := `$(NIFTY_LLVM_CONFIG) --libdir`/clang/`$(NIFTY_LLVM_CONFIG) --version`/include/:$(CPATH)

CONFIG := $(NIFTY_ROOT_CONFIG) $(LLVM_CONFIG)

DIALYZER_APPS = erts kernel stdlib compiler crypto syntax_tools tools
DIALYZER_FLAGS = -Wunmatched_returns -Wunderspecs

default: fast

fast: compile

all: default tests dialyze doc

get-deps: rebar3
	$(REBAR) get-deps
	travis/fix_exports.sh

compile: get-deps rebar3
	$(CONFIG) $(REBAR) compile

dialyze: rebar3 compile
	# sed -i "s/^{provider/%% {provider/g" rebar.config
	# -rm _build/default/lib/nifty/ebin/*template.beam
	$(REBAR) dialyzer
	# sed -i "s/^%% {provider/{provider/g" rebar.config

tests: compile rebar3
	CC=$(CLANG) \
	CPATH=$(NIFTY_CPATH) \
	$(CONFIG) \
	ERL_LIBS=$(ERL_INCLUDE) \
	$(REBAR) eunit

doc: rebar3
	$(REBAR) edoc

clean: rebar3
	$(REBAR) clean

mrproper: clean
	$(RM) -r _build/
	$(RM) -rf nt_* dereference_regression/
	$(RM) rebar3
	$(RM) rebar.lock
	$(RM) doc

shell: rebar3
	$(REBAR) shell

rebar3:
	wget https://s3.amazonaws.com/rebar3/rebar3
	chmod +x rebar3
