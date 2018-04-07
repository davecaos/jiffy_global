REBAR   = rebar3
PROJECT = jiffy_global
VERSION = 1.0.0

all: compile

compile:
		@$(REBAR) compile
clean:
		@$(REBAR) clean
			rm -f $(PROJECT).tar.gz

distclean:
		@$(REBAR) clean -a
			rm -f $(PROJECT).tar.gz

dialyzer:
		@$(REBAR) dialyzer

dev-rel: compile
		@$(REBAR) as dev release

test:
		@$(REBAR) eunit

travis-test:
		@$(REBAR) ct

update:
		@$(REBAR) update


.PHONY: compile rel clean dialyzer test update
