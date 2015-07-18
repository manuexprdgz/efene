-module(fn_SUITE).
-compile(export_all).

all() -> [eval_macro, can_expand_var_with_ref, can_expand_macro_str].

init_per_suite(Config) ->
    Config.

print(Thing) ->
    ct:print("~p~n", [Thing]).

exp(Macros, Key, Args) ->
    fn_erl_macro:expand_macro(Macros, Key, Args).

eval_macro(_) ->
    {ok, Macros} = fn_erl_macro:macro_defs("../../examples/ms.hrl"),
    print(dict:to_list(Macros)),
    {ok, [{string, _, "bob"}]} = exp(Macros, 'AUTHOR', #{}),
    {ok, [{op, _, '*', {integer, _, 2}, {integer, _, 3}}]} = exp(Macros, 'Const', {}),

    {ok, [{op, _, '+', {integer, _, 2}, {integer, _, 1}}]} = exp(Macros, {'Inc', 1}, #{'A' => {integer, 1, 2}}),

    {ok, [{tuple, _, [{string, _, "bob"}, {integer, _, 42}]}]} = exp(Macros, {'AUTHOR', 1}, #{'A' => {integer, 1, 42}}),

    {ok, [{tuple, _, [{atom, _, val}, {integer, 1, 1}, {integer, 2, 2}, {integer, 3, 3}]}]} = exp(Macros, {'V', 3}, #{'Line' => {integer, 1, 1}, 'Type' => {integer, 2, 2}, 'Val' => {integer, 3, 3}}),

    R1 = exp(Macros, {'IncConst', 1}, #{'A' => {integer, 1, 42}}),
    print(R1),
    {ok, [{op, _, '+',
           {integer, _, 42},
           {op, _, '*',
            {integer, _, 2},
            {integer, _, 3}}}]} = R1.

can_expand_var_with_ref(_) ->
    {ok, Macros} = fn_erl_macro:macro_defs("../../examples/ms.hrl"),
    Ref = make_ref(),
    {ok, [{op, _, '+', {var, 2, Ref}, {integer, _, 1}}]} = exp(Macros, {'Inc', 1}, #{'A' => {var, 2, Ref}}),
    AstNode = {op, 1, '-', {integer, 1, 42}, {integer, 1, 43}},
    {ok, [{op, _, '+', AstNode, {integer, _, 1}}]} = exp(Macros, {'Inc', 1}, #{'A' => AstNode}).

can_expand_macro_str(_) ->
    {ok, Macros} = fn_erl_macro:macro_defs("../../examples/ms.hrl"),
    AstNode = {op, 1, '-', {integer, 1, 42}, {integer, 1, 43}},
    {ok, [{string, _, "42 - 43"}]} = exp(Macros, {'Text', 1}, #{'Val' => AstNode}),

    {ok, [Ast]} = exp(Macros, {'TESTCALL', 1}, #{'Call' => AstNode}),
    "io:format(\"Call ~s: ~w~n\", [\"42 - 43\",42 - 43])" = lists:flatten(erl_pp:expr(Ast)).



