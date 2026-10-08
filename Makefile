export HOST_UID := $(shell id -u)
export HOST_GID := $(shell id -g)
export HOST_HOME := $(HOME)
export XAUTHORITY_FILE := $(or $(XAUTHORITY),$(HOME)/.Xauthority)

COMPOSE = docker compose run --rm

.PHONY: setup run test shell clean

setup:
	mkdir -p "$(HOST_HOME)/.cache/megastrike/m2"
	mkdir -p "$(HOST_HOME)/.cache/megastrike/gitlibs"
	docker compose build

run:
	$(COMPOSE) megastrike clojure -M:run

test:
	$(COMPOSE) megastrike clojure -X:test

shell:
	$(COMPOSE) megastrike bash

clean:
	docker compose down --remove-orphans
