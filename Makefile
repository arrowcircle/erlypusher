all:
	./rebar compile skip_deps=true

full:
	GPROC_DIST=true ./rebar get-deps compile

clean:
	./rebar clean

test:
	rm -Rf .eunit rm -Rf ebin; ./rebar eunit skip_deps=true

dev:
	./rebar compile && erl -sname erlypusher@localhost -pa ebin deps/*/ebin -gproc gproc_dist all -boot start_sasl -s erlypusher

run:
	./rebar compile && ERL_LIBS=apps:deps erl +K true -sname erlypusher@`hostname -s` -pa ebin deps/*/ebin -gproc gproc_dist all -boot start_sasl -s erlypusher -sasl errlog_type error

run2:
	./rebar compile && ERL_LIBS=apps:deps erl +K true -sname erlypusher2@`hostname -s` -pa ebin deps/*/ebin -gproc gproc_dist all -boot start_sasl -s erlypusher -sasl errlog_type error

start_daemon:
	./rebar compile && ERL_LIBS=apps:deps run_erl -daemon  ./tmp/ ./log/ "erl +K true -sname erlypusher@`hostname -s` -pa ebin deps/*/ebin -gproc gproc_dist all -boot start_sasl -s erlypusher -sasl errlog_type error"

stop_daemon:
	echo "init:stop()." | to_erl ./tmp/

install:
	cp erlypusher.init.d /etc/init.d/erlypusher; chmod 755 /etc/init.d/erlypusher; update-rc.d erlypusher defaults