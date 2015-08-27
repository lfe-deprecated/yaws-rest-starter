ifeq ($(shell which erl),)
$(error Can't find Erlang executable 'erl')
exit 1
endif

LIB = $(PROJECT)
DEPS = ./deps
BIN_DIR = ./bin
SOURCE_DIR = ./src
OUT_DIR = ./ebin
TEST_DIR = ./test
TEST_OUT_DIR = ./.eunit
SCRIPT_PATH=$(DEPS)/lfe/bin:.:./bin:"$(PATH)":/usr/local/bin
ifeq ($(shell which lfetool),)
LFETOOL=$(BIN_DIR)/lfetool
else
LFETOOL=lfetool
endif
ERL_LIBS=$(shell pwd):$(shell $(LFETOOL) info erllibs)
OS := $(shell uname -s)
ifeq ($(OS),Linux)
		HOST=$(HOSTNAME)
endif
ifeq ($(OS),Darwin)
		HOST = $(shell scutil --get ComputerName)
endif

$(BIN_DIR):
	mkdir -p $(BIN_DIR)

get-lfetool: $(BIN_DIR)
	curl -L -o ./lfetool https://raw.github.com/lfe/lfetool/dev-v1/lfetool && \
	chmod 755 ./lfetool && \
	mv ./lfetool $(BIN_DIR)

get-version:
	@PATH=$(SCRIPT_PATH) $(LFETOOL) info version
	@echo "Erlang/OTP, LFE, & library versions:"
	@ERL_LIBS=$(ERL_LIBS) PATH=$(SCRIPT_PATH) erl \
	-eval "lfe_io:format(\"~p~n\",['$(PROJECT)':'get-versions'()])." \
	-noshell -s erlang halt

get-deps:
	@echo "Getting dependencies ..."
	@PATH=$(SCRIPT_PATH) ERL_LIBS=$(ERL_LIBS) $(LFETOOL) download deps

clean-ebin:
	@echo "Cleaning ebin dir ..."
	@rm -f $(OUT_DIR)/*.beam

clean-eunit:
	-@PATH=$(SCRIPT_PATH) $(LFETOOL) tests clean

compile: get-deps clean-ebin
	@echo "Compiling project code and dependencies ..."
	@which rebar.cmd >/dev/null 2>&1 && \
	PATH=$(SCRIPT_PATH) ERL_LIBS=$(ERL_LIBS) rebar.cmd compile || \
	PATH=$(SCRIPT_PATH) ERL_LIBS=$(ERL_LIBS) rebar compile

compile-no-deps: clean-ebin
	@echo "Compiling only project code ..."
	@which rebar.cmd >/dev/null 2>&1 && \
	PATH=$(SCRIPT_PATH) ERL_LIBS=$(ERL_LIBS) \
	rebar.cmd compile skip_deps=true || \
	PATH=$(SCRIPT_PATH) ERL_LIBS=$(ERL_LIBS) rebar compile skip_deps=true

compile-tests: clean-eunit
	@PATH=$(SCRIPT_PATH) ERL_LIBS=$(ERL_LIBS) $(LFETOOL) tests build

repl: compile repl-raw

repl-no-deps: compile-no-deps repl-raw

repl-raw:
	@which clear >/dev/null 2>&1 && clear || printf "\033c"
	@echo "Starting an LFE REPL ..."
	@PATH=$(SCRIPT_PATH) ERL_LIBS=$(ERL_LIBS) $(LFETOOL) repl lfe \
	+pc unicode -pa $(TEST_OUT_DIR)

shell: compile
	@which clear >/dev/null 2>&1 && clear || printf "\033c"
	@echo "Starting an Erlang shell ..."
	@PATH=$(SCRIPT_PATH) ERL_LIBS=$(ERL_LIBS) erl + pc unicode

shell-no-deps: compile-no-deps
	@which clear >/dev/null 2>&1 && clear || printf "\033c"
	@echo "Starting an Erlang shell ..."
	@PATH=$(SCRIPT_PATH) ERL_LIBS=$(ERL_LIBS) erl + pc unicode

clean: clean-ebin clean-eunit
	@which rebar.cmd >/dev/null 2>&1 && rebar.cmd clean || rebar clean

check-unit-only:
	@PATH=$(SCRIPT_PATH) ERL_LIBS=$(ERL_LIBS) $(LFETOOL) tests unit

check-integration-only:
	@PATH=$(SCRIPT_PATH) ERL_LIBS=$(ERL_LIBS) $(LFETOOL) tests integration

check-system-only:
	@PATH=$(SCRIPT_PATH) ERL_LIBS=$(ERL_LIBS) $(LFETOOL) tests system

check-unit-with-deps: get-deps compile compile-tests check-unit-only
check-unit: clean-eunit compile-no-deps check-unit-only
check-integration: clean-eunit compile check-integration-only
check-system: clean-eunit compile check-system-only
check-all-with-deps: clean-eunit compile check-unit-only \
	check-integration-only check-system-only clean-eunit
check-all: get-deps clean-eunit compile-no-deps
	@PATH=$(SCRIPT_PATH) ERL_LIBS=$(ERL_LIBS) $(LFETOOL) tests all

check: check-unit-with-deps

check-travis: $(LFETOOL) check

push-all:
	@echo "Pusing code to github ..."
	git push --all
	git push upstream --all
	git push --tags
	git push upstream --tags
