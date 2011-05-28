all: compile deps

deps: deps/gen_nb_server

deps/gen_nb_server:
	@./rebar get-deps

compile:
	@./rebar compile

clean:
	@./rebar clean