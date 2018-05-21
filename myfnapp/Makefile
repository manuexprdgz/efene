BASEDIR = $(shell pwd)
REBAR = rebar3

.PHONY: compile clean test shell

compile:
	$(REBAR) compile

clean:
	$(REBAR) clean

test:
	$(REBAR) ct

shell:
	$(REBAR) efene shell
