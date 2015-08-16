-module(fn_ext_val_atom_c).
-export([handle/3]).

-behaviour(fn_exts).

-include("include/efene.hrl").

handle(_Path, ?V(Line, string, [Char]), State) ->
    {{char, Line, Char}, State};
handle(_Path, _Ast, _State) ->
    next.
