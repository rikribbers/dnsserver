.PHONY:	eqc eunit dialyzer ct

all: eqc eunit dialyzer ct

eunit:
	rebar3 eunit

dialyzer:
	rebar3 dialyzer

eqc:
	rebar3 eqc

ct:
	rebar3 ct