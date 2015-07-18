-module(fn_erl_macro).
-export([macro_defs/1, expand_macro/2, expand_macro/3]).

macro_defs(Path) ->
    {ok, _Tokens, Macros} = aleppo:process_file(Path, [{return_macros, true}]),
    {ok, Macros}.

expand_macro(Macros, Name) ->
    expand_macro(Macros, Name, #{}).

get_macro(Macros, Name) ->
    case dict:find(Name, Macros) of
        {ok, Macro} ->
            {MacroArgs, Tokens} = case Macro of
                                      {_MArgs, _MTokens} = R -> R;
                                      MTokens when is_list(MTokens) -> {[], MTokens}
                                  end,
            {ok, MacroArgs, Tokens};
        error -> {error, {macro_not_found, Name}}
    end.

expand_macro(Macros, Name, Args) ->
    case get_macro(Macros, Name) of
        {ok, MacroArgs, Tokens} ->
            ct:pal("args: ~p~ntokens: ~p~n", [MacroArgs, Tokens]),
            case expand_macros(Macros, Tokens, Args) of
                {ok, ETokens, Refs} -> parse_tokens(ETokens, Refs);
                {error, _Reason} = Error -> Error
            end;
        {error, _Reason} = Error -> Error
    end.

expand_macros(Macros, Macro, Args) ->
    expand_macros(Macros, Macro, Args, [], #{}).

expand_macros(_Macros, [], _Args, Accum, Refs) ->
    {ok, lists:reverse(Accum), Refs};
expand_macros(Macros, [{var, _, VarName}|T], Args, Accum, Refs) ->
    case maps:get(VarName, Args, undefined) of
        undefined -> {error, {var_not_defined, VarName}};
        Value ->
            {RefVar, NewRefs} = add_ref_var(Refs, Value),
            expand_macros(Macros, T, Args, [RefVar|Accum], NewRefs)
    end;
expand_macros(Macros, [{macro, {var, _, MacroName}}|T], Args, Accum, Refs) ->
    case get_macro(Macros, MacroName) of
        {ok, [], Tokens} ->
            expand_macros(Macros, T, Args, lists:reverse(Tokens) ++ Accum, Refs);
        {error, _Reason} = Error -> Error
    end;

%expand_macros(Macros, [{macro, {var, _, MacroName}, MacroArgs}|T], Args, Accum, Refs) ->

expand_macros(Macros, [H|T], Args, Accum, Refs) ->
    expand_macros(Macros, T, Args, [H|Accum], Refs).

add_ref_var(Refs, Value) ->
    Ref = make_ref(),
    NewRefs = maps:put(Ref, Value, Refs),
    {{var, 1, Ref}, NewRefs}.

parse_tokens(Tokens, Refs) ->
    {ok, Tokens1} = aleppo:process_tokens(Tokens ++ [{dot, 1}]),
    case erl_parse:parse_exprs(remove_eof(Tokens1, [])) of
        {ok, AstWithRefs} ->
            AstReplaced = replace_ast_refs(AstWithRefs, Refs),
            {ok, AstReplaced};
        {error, _Reason} = Error -> Error
    end.

replace_ast_refs(Ast, Refs) ->
    Walker = fun ({var, _, Val}) when is_reference(Val) ->
                     maps:get(Val, Refs);
                 (Other) -> Other
             end,
    erl_ast_walk:exprs(Ast, Walker).

remove_eof([], Accum) -> lists:reverse(Accum);
remove_eof([{eof, _}|T], Accum) -> remove_eof(T, Accum);
remove_eof([H|T], Accum) -> remove_eof(T, [H|Accum]).
