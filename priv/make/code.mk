compile:
	@mkdir -p $(LOG_DIR)
	@rebar3 compile

check:
	@rebar3 as test eunit

repl: compile
	@ERL_LIBS=$(EBIN_DIRS) $(LFE)

shell:
	@rebar3 shell

clean:
	@rebar3 clean
	@rm -rf ebin/* _build/default/lib/$(PROJECT)

clean-all: clean
	@rebar3 as dev lfe clean
