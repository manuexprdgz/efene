PROJECT = efene
include erlang.mk

fnshell:
	erl -run fn_repl main -run init stop -noshell -pa ebin
