-module(fn_ext_val_atom_c).
-export([handle/2]).

-behaviour(fn_exts).

-include("include/efene.hrl").

handle(?V(Line, string, [Char]), State) ->
    {{char, Line, Char}, State};
handle(_Ast, _State) ->
    next.
