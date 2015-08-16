-module(fn_ext_val_var__).
-export([handle/2]).

-behaviour(fn_exts).

handle(_Ast, State) ->
    {'fn compiler ignore', State}.
