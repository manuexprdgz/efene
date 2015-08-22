-module(fn_ext_expr_atom_b).
-export([handle/3]).

-behaviour(fn_exts).

-include("efene.hrl").

% binary comprehension
handle(_Path, ?E(Line, 'for', {Qualifiers, Body}), State) ->
    {Items, EBody, State1} = fn_to_erl:lc_to_ast(Line, Qualifiers, Body, State),
    R = {bc, Line, EBody, lists:map(fun generate_to_b_generate/1, Items)},
    {R, State1};
handle(_Path, _, _State) ->
    next.

generate_to_b_generate({generate, Line, Left, Right}) ->
    {b_generate, Line, Left, Right};
generate_to_b_generate(Other) ->
    Other.
