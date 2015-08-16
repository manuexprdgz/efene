-module(fn_ext_val_atom_atom).
-export([handle/2]).

-behaviour(fn_exts).

-include("include/efene.hrl").

handle(?V(Line, string, AtomStr), State) ->
    {{atom, Line, list_to_atom(AtomStr)}, State};
handle(_Ast, _State) ->
    next.
