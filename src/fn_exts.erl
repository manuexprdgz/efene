-module(fn_exts).
-export([get_extensions/0, match_extensions/2, handle/4, behaviour_info/1]).

behaviour_info(callbacks) ->
        [{handle, 2}];
behaviour_info(_) ->
        undefined.

% returns the same format as code:all_loaded/0
all_modules() ->
    [{list_to_atom(filename:rootname(filename:basename(F))), filename:dirname(F)} ||
         P <- code:get_path(),
         F <- filelib:wildcard(P ++ "/*.beam")].

get_extensions() ->
    lists:filtermap(fun parse_and_load_extension/1, all_modules()).

match_extensions(Name, Extensions) ->
    Exts = lists:filter(fun (Info) -> is_extension_handler(Info, Name) end,
                 Extensions),
    lists:sort(Exts).

parse_and_load_extension({Name, Path}) ->
    StrName = atom_to_list(Name),
    case is_extension(StrName) of
        true ->
            case code:ensure_loaded(Name) of
                {module, _} -> ok;
                {error, Reason} ->
                    io:format("error loading extension module ~p: ~p~n",
                              [Name, Reason])
            end,
            {true, {Name, StrName, Path}};
        false -> false
    end.

handle(Name, Ast, State, Extensions) ->
    case match_extensions(Name, Extensions) of
        [] -> {error, notfound};
        Exts -> handle(Ast, State, Exts)
    end.

handle(Ast, State, []) ->
    {{error, {nomatch, Ast}}, State};
handle(Ast, State, [{Module, _ModStr, _ModPath}|Exts]) ->
    %io:format("looking for handler ~p:handle/2~n", [Module]),
    try erlang:function_exported(Module, handle, 2) of
        true -> 
            %io:format("handler ~p:handle/2 found~n", [Module]),
            case Module:handle(Ast, State) of
                next ->
                    %io:format("handler ~p:handle/2 returned next~n", [Module]),
                    handle(Ast, State, Exts);
                Other ->
                    %io:format("handler ~p:handle/2 returned ~p~n", [Module, Other]),
                    Other
            end;
        false->
            %io:format("handler ~p:handle/2 not found~n", [Module]),
            % TODO: return warning
            handle(Ast, State, Exts)
    catch T:E ->
        % TODO: return warning
        %{error, {ext_handler_error, T, E}}
        io:format("error calling handler ~p:handle/2 ~p: ~p~n", [Module, T, E]),
        handle(Ast, State, Exts)
    end.


is_extension_handler({_AtomName, ModName, _Path}, Name) ->
    ModPrefix = "fn_ext_" ++ Name,
    lists:suffix(ModPrefix, ModName).

is_extension("fn_ext_" ++ _) -> true;
is_extension(_) -> false.
