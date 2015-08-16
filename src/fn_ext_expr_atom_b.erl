-module(fn_ext_expr_atom_b).
-export([handle/2]).

-behaviour(fn_exts).

-include("include/efene.hrl").

% binary comprehension
handle(?E(Line, 'for', {Qualifiers, Body}), State) ->
    {Items, EBody, State1} = fn_to_erl:lc_to_ast(Line, Qualifiers, Body, State),
    R = {bc, Line, EBody, Items},
    {R, State1};
handle(_, _State) ->
    next.

