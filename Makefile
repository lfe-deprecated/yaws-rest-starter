PROJECT = yaws-rest-starter
LIB = yaws-rest-starter
DEPS = ./deps
BIN_DIR = ./bin
ETC_DIR = ./etc
EXPM = $(BIN_DIR)/expm
LFE_DIR = $(DEPS)/lfe
LFE_EBIN = $(LFE_DIR)/ebin
LFE = $(LFE_DIR)/bin/lfe
LFEC = $(LFE_DIR)/bin/lfec
LFE_UTILS_DIR = $(DEPS)/lfe-utils
LFEUNIT_DIR = $(DEPS)/lfeunit
YAWS_DIR = $(DEPS)/yaws
YAWS = $(YAWS_DIR)/bin/yaws
YAWS_CONF = $(ETC_DIR)/yaws.conf
# Note that ERL_LIBS is for running this project in development and that
# ERL_LIB is for installation.
ERL_LIBS = $(LFE_DIR):$(LFE_UTILS_DIR):$(LFEUNIT_DIR):$(YAWS_DIR):./
SOURCE_DIR = ./src
OUT_DIR = ./ebin
TEST_DIR = ./test
TEST_OUT_DIR = ./.eunit
FINISH = -run init stop -noshell

dev: compile-only
	@ERL_LIBS=$(ERL_LIBS) $(YAWS) -i --conf $(YAWS_CONF)

run: compile
	@ERL_LIBS=$(ERL_LIBS) $(YAWS) -D --heart --conf $(YAWS_CONF)

update-conf:
	@ERL_LIBS=$(ERL_LIBS) $(YAWS) -h --conf $(YAWS_CONF)

stats:
	@ERL_LIBS=$(ERL_LIBS) $(YAWS) -S

stop:
	@ERL_LIBS=$(ERL_LIBS) $(YAWS) --stop

get-version:
	@echo
	@echo "Getting version info ..."
	@echo
	@echo -n app.src: ''
	@erl -eval 'io:format("~p~n", [ \
		proplists:get_value(vsn,element(3,element(2,hd(element(3, \
		erl_eval:exprs(element(2, erl_parse:parse_exprs(element(2, \
		erl_scan:string("Data = " ++ binary_to_list(element(2, \
		file:read_file("src/$(LIB).app.src"))))))), []))))))])' \
		$(FINISH)
	@echo -n package.exs: ''
	@grep version package.exs |awk '{print $$2}'|sed -e 's/,//g'

# Note that this make target expects to be used like so:
#>--$ ERL_LIB=some/path make get-install-dir
#
# Which would give the following result:
#>--some/path/yaws-rest-starter-1.0.0
#
get-install-dir:
	@echo $(ERL_LIB)/$(PROJECT)-$(shell make get-version)

$(BIN_DIR):
	mkdir -p $(BIN_DIR)

$(EXPM): $(BIN_DIR)
	curl -o $(EXPM) http://expm.co/__download__/expm
	chmod +x $(EXPM)

get-deps: $(EXPM)
	rebar get-deps
	for DIR in $(wildcard $(DEPS)/*); \
	do cd $$DIR; echo "Updating $$DIR ..."; \
	git pull; cd - > /dev/null; done

clean-ebin:
	rm -f $(OUT_DIR)/*.beam

clean-eunit:
	rm -rf $(TEST_OUT_DIR)

compile: get-deps clean-ebin
	rebar compile

compile-only: clean-ebin
	rebar compile skip_deps=true

compile-tests: clean-eunit
	mkdir -p $(TEST_OUT_DIR)
	ERL_LIBS=$(ERL_LIBS) $(LFEC) -o $(TEST_OUT_DIR) $(TEST_DIR)/*_tests.lfe

shell: compile
	clear
	ERL_LIBS=$(ERL_LIBS) $(LFE) -pa $(TEST_OUT_DIR)

clean: clean-ebin clean-eunit
	rebar clean

check: compile compile-tests
	@clear;
	@rebar eunit verbose=1 skip_deps=true

check-only: compile-only compile-tests
	@clear;
	@rebar eunit verbose=1 skip_deps=true

push-all:
	git push --all
	git push upstream --all
	git push --tags
	git push upstream --tags

# Note that this make target expects to be used like so:
#>--$ ERL_LIB=some/path make install
#
install: INSTALLDIR=$(shell make get-install-dir)
install: compile
	if [ "$$ERL_LIB" != "" ]; \
	then mkdir -p $(INSTALLDIR)/$(EBIN); \
		mkdir -p $(INSTALLDIR)/$(SRC); \
		cp -pPR $(EBIN) $(INSTALLDIR); \
		cp -pPR $(SRC) $(INSTALLDIR); \
	else \
		echo "ERROR: No 'ERL_LIB' value is set in the env." \
		&& exit 1; \
	fi

upload: get-version
	@echo "Package file:"
	@echo
	@cat package.exs
	@echo
	@echo "Continue with upload? "
	@read
	$(EXPM) publish
