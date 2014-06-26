REBAR=/bin/env rebar
REPO=erlang-srv-stub
all: get-deps compile

get-deps:
	@$(REBAR) get-deps

compile:
	@$(REBAR) compile

clean:
	@$(REBAR) clean
