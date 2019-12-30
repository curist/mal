.DEFAULT_GOAL := step
SHELL=/bin/bash

T ?= 0
test:
	@cd .. && make "test^janet^step$(T)"

step: test
	make step T=$$(( $(T) + 1 ))

dev:
	ls **/* | entr -c make
