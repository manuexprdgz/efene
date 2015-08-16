-module(fn_ext_val_atom_i).
-export([handle/3]).

-behaviour(fn_exts).

-include("include/efene.hrl").

handle(_Path, ?Atom(Line, Name), State) ->
    info_to_ast(Line, Name, State);
handle(_Path, _Ast, _State) ->
    next.

info_to_ast(Line, line, State) ->
    {{integer, Line, Line}, State};
info_to_ast(Line, module, #{module := Module}=State) ->
    {{atom, Line, Module}, State};
info_to_ast(Line, Name, State) ->
    State1 = fn_to_erl:add_error(State, unknown_compiler_info, Line,
                       fn_to_erl:expected_got("\"line\" or \"module\"", Name)),
    {{atom, Line, error}, State1}.

