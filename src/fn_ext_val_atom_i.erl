-module(fn_ext_val_atom_i).
-export([handle/3]).

-behaviour(fn_exts).

-include("efene.hrl").

handle(_Path, ?Atom(Line, Name), State) ->
    info_to_ast(Line, Name, State);
handle(_Path, _Ast, _State) ->
    next.

info_to_ast(Line, line, State) ->
    {{integer, Line, Line}, State};
info_to_ast(Line, module, #{module := Module}=State) ->
    {{atom, Line, Module}, State};
info_to_ast(Line, module_string, #{module := Module}=State) ->
    {{string, Line, atom_to_list(Module)}, State};
info_to_ast(Line, function_name, #{function_name := Name}=State) ->
    {{atom, Line, Name}, State};
info_to_ast(Line, function_arity, #{function_arity := Arity}=State) ->
    {{integer, Line, Arity}, State};
info_to_ast(Line, Name, State) ->
    State1 = fn_to_erl:add_error(State, unknown_compiler_info, Line,
                       fn_to_erl:expected_got("\"line\", \"function_name\", \"function_arity\", \"module_string\" or \"module\"", Name)),
    {{atom, Line, error}, State1}.

