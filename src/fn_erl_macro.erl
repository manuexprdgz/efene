-module(fn_erl_macro).
-export([macro_defs/1, filter_macros/1, split_arities/1, parse_macro_defs/1]).

macro_defs(Path) ->
    {ok, Epp} = epp:open(Path, []),
    epp:parse_file(Epp),
    Macros = epp:macro_defs(Epp),
    epp:close(Epp),
    Macros.

filter_macros(Macros) -> lists:filter(fun is_ignorable_macro/1, Macros).

is_ignorable_macro({{{atom, 'MACHINE'}, none}, _}) -> false;
is_ignorable_macro({{{atom, 'BASE_MODULE'}, none}, _}) -> false;
is_ignorable_macro({{{atom, 'MODULE'}, none}, _}) -> false;
is_ignorable_macro({{{atom, 'LINE'}, none}, _}) -> false;
is_ignorable_macro({{{atom, 'FILE'}, none}, _}) -> false;
is_ignorable_macro({{{atom, 'BASE_MODULE_STRING'}, none}, _}) -> false;
is_ignorable_macro({{{atom, 'MODULE_STRING'}, none}, _}) -> false;
is_ignorable_macro({{{atom, 'BEAM'}, none}, _}) -> false;
is_ignorable_macro(_) -> true.

split_arity({_Name, undefined}) -> [];
split_arity({Name, {Arity, Rest}}) -> [{{Name, Arity}, Rest}];
split_arity({Name, Arities}) ->
    lists:map(fun ({Arity, Rest}) -> {{Name, Arity}, Rest} end, Arities).

split_arities(Macros) ->
    lists:flatmap(fun split_arity/1, Macros).

parse_macro_def({Key, {Args, Tokens}}=Macro) ->
    TokensDot = Tokens ++ [{dot, 1}],

    case aleppo:process_tokens(TokensDot) of
        {ok, [{macro, _MacroName, _MacroDefAst}=MacroAst, {dot, _}, {eof, _}]} ->
            {ok, {Key, {Args, MacroAst}}};
        {ok, ErlMacroAst} ->
            case erl_parse:parse_exprs(remove_eof(ErlMacroAst)) of
                {ok, ErlAst} ->
                    {ok, {Key, {Args, ErlAst}}};
                {error, Reason} ->
                    io:format("!~p~n~p~n", [ErlMacroAst, aleppo:process_tokens(ErlMacroAst)]),
                    {error, {Macro, Reason}}
            end;
        {error, ErlParseError} ->
            io:format("ERROR: ~p~n", [TokensDot]),
            {error, {Macro, ErlParseError}}
    end.

parse_macro_defs(Macros) ->
    Results = lists:map(fun parse_macro_def/1, Macros),
    {Os, Es} = lists:foldl(fun ({ok, Result}, {Oks, Errs}) ->
                                 {[Result|Oks], Errs};
                             ({error, Result}, {Oks, Errs}) ->
                                 {Oks, [Result|Errs]}
                         end, {[], []}, Results),
    {lists:reverse(Os), lists:reverse(Es)}.

remove_eof(Tokens) -> remove_eof(Tokens, []).

remove_eof([], Accum) -> lists:reverse(Accum);
remove_eof([{eof, _}|T], Accum) -> remove_eof(T, Accum);
remove_eof([H|T], Accum) -> remove_eof(T, [H|Accum]).
