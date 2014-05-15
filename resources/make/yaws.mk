ETC_DIR = ./etc
YAWS_DIR = $(DEPS)/yaws
YAWS = $(YAWS_DIR)/bin/yaws
YAWS_CONF = $(ETC_DIR)/yaws.conf
YAWS_SERVER_ID = yawsreststarter

app-deps:
	@mkdir -p logs

dev: app-deps compile-no-deps
	@ERL_LIBS=$(shell lfetool info erllibs) \
	$(YAWS) -i --conf $(YAWS_CONF) --id $(YAWS_SERVER_ID) \
	--runmod inets --runmod ssl

run: app-deps compile
	@ERL_LIBS=$(shell lfetool info erllibs) \
	$(YAWS) -D --heart --conf $(YAWS_CONF) --id $(YAWS_SERVER_ID) \
	--runmod inets --runmod ssl

update-conf:
	@ERL_LIBS=$(ERL_LIBS) $(YAWS) -h --conf $(YAWS_CONF) --id $(YAWS_SERVER_ID)

stats:
	@ERL_LIBS=$(ERL_LIBS) $(YAWS) -S --id $(YAWS_SERVER_ID)

stop:
	@ERL_LIBS=$(ERL_LIBS) $(YAWS) --stop --id $(YAWS_SERVER_ID)
