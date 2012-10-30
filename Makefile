REBAR=rebar

all: compile

compile:
	$(REBAR) compile

clean:
	$(REBAR) clean

test: clean compile
	$(REBAR) eunit skip_deps=true

docs:
	$(REBAR) doc skip_deps=true

