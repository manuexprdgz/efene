-module(fn_ext_val_var__).
-export([handle/3]).

-behaviour(fn_exts).

handle(_Path, _Ast, State) ->
    {'fn compiler ignore', State}.
