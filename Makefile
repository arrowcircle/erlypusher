all:
	./rebar compile skip_deps=true

full:
	./rebar get-deps compile

clean:
	./rebar clean

test:
	./rebar eunit skip_deps=true

dev:
	./rebar compile && erl -sname erlypusher@localhost -pa ebin deps/*/ebin -gproc gproc_dist all -boot start_sasl -s erlypusher -port 8081

run:
	./rebar compile && ERL_LIBS=apps:deps erl +K true -sname erlypusher@localhost -pa ebin deps/*/ebin -gproc gproc_dist all -boot start_sasl -s erlypusher -sasl errlog_type error -port 8081

run2:
	./rebar compile && ERL_LIBS=apps:deps erl +K true -sname erlypusher2@localhost -pa ebin deps/*/ebin -gproc gproc_dist all -boot start_sasl -s erlypusher -sasl errlog_type error -port 8082