YAWS_DIR = _build/default/lib/yaws
APP_DIR = $(ROOT_DIR)

run: compile
	@$(YAWS_DIR)/bin/yaws -i \
	-pa `rebar3 path -s" -pa "` \
	--conf $(APP_DIR)/priv/etc/yaws.conf \
	--id $(PROJECT)

daemon: compile
	@$(YAWS_DIR)/bin/yaws \
	-pa `rebar3 path -s" -pa "` \
	-D --heart \
	--conf $(APP_DIR)/priv/etc/yaws.conf \
	--id $(PROJECT)

stop:
	@$(YAWS_DIR)/bin/yaws \
	-pa `rebar3 path -s" -pa "` \
	--stop --id $(PROJECT)
