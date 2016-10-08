PROJECT = efene
DEPS = aleppo ast_walk
dep_aleppo = git https://github.com/ErlyORM/aleppo master
dep_ast_walk = git https://github.com/marianoguerra/ast_walk 0.1.0

include erlang.mk

fnshell:
	erl -run fn_repl main -run init stop -noshell -pa ebin
