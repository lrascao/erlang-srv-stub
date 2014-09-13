REBAR=`which rebar || printf ./rebar`

all: get-deps compile

get-deps:
	@$(REBAR) get-deps

compile:
	@$(REBAR) compile

FORCE:

test: FORCE
	@$(REBAR) compile eunit

clean:
	@$(REBAR) clean
