./rebar compile && erl -sname erlypusher -pa ebin deps/*/ebin -gproc gproc_dist all -boot start_sasl -s erlypusher
