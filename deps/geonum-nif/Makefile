REBAR=rebar

all:
	$(REBAR) compile

clean:
	$(REBAR) clean

eunit:
	@$(REBAR) eunit

tests: clean deps/proper eunit

