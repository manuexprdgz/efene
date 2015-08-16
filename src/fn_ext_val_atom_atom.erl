-module(fn_ext_val_atom_atom).
-export([handle/3]).

-behaviour(fn_exts).

-include("include/efene.hrl").

handle(_Path, ?V(Line, string, AtomStr), State) ->
    {{atom, Line, list_to_atom(AtomStr)}, State};
handle(_Path, _Ast, _State) ->
    next.
