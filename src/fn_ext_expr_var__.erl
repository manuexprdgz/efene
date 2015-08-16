-module(fn_ext_expr_var__).
-export([handle/2]).

-behaviour(fn_exts).

handle(_Ast, State) ->
    {'fn compiler ignore', State}.
