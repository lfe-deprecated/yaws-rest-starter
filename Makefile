PROJECT = yrests
ROOT_DIR = $(shell pwd)
REPO = $(shell git config --get remote.origin.url)
LOG_DIR = $(ROOT_DIR)/log
LFE_DIR = _build/default/lib/lfe
LFE = $(LFE_DIR)/bin/lfe
EBIN_DIRS = $(ERL_LIBS):$(shell rebar3 path -s:)

include priv/make/code.mk
include priv/make/yaws.mk
include priv/make/docs.mk

