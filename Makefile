PROJECT = efene
DEPS = aleppo
dep_aleppo = git https://github.com/marianoguerra/aleppo expose-macro-info
include erlang.mk

fnshell:
	erl -run fn_repl main -run init stop -noshell -pa ebin
