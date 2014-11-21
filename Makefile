all: deps compile

compile: deps
	./rebar compile

deps:
	./rebar get-deps

clean:
	./rebar clean
	rm -rf rel/slimrt

dist:
	cd rel && ../rebar generate -f
