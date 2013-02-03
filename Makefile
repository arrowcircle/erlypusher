all:
	./rebar compile skip_deps=true

full:
	./rebar get-deps compile

clean:
	./rebar clean

test:
	./rebar eunit skip_deps=true

run:
	./rebar compile && ERL_LIBS=apps:deps erl +K true -sname erlypusher -pa ebin deps/*/ebin -gproc gproc_dist all -boot start_sasl -s erlypusher -sasl errlog_type error