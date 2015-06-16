-module(fn_repl).
-export([main/0]).

loop(Bindings) ->
  case io:get_line(">>> ") of
      eof ->
          io:format("~nBye!~n");
      Input ->
          NewBindings = handle_input(Input, Bindings),
          loop(NewBindings)
  end.

handle_input(Input, Bindings) ->
    case str_to_erl_ast(Input, "repl") of
        {ok, {[Ast], _State}} ->
            try
                case erl_eval:expr(Ast, Bindings,
                                   {value, fun lfun_value_handler/2},
                                   {value, fun nlfun_value_handler/2}) of
                    {value, Value, B1} ->
                        io:format("~p~n", [Value]),
                        B1;
                    Other ->
                        io:format("error: ~p~n", [Other]),
                        Bindings
                end
            catch
                T:E ->
                    io:format("~p: ~p~n~n", [T, E]),
                    pprint_strace(erlang:get_stacktrace()),
                    Bindings
            end;
        Other ->
            print(Other),
            Bindings
    end.

lfun_value_handler(Name, Arguments) ->
    throw({not_defined, {Name, Arguments}}).

nlfun_value_handler({Mod, Fun}, Arguments) ->
    erlang:apply(Mod, Fun, Arguments).

main() ->
  Bindings = erl_eval:new_bindings(),
  loop(Bindings).


print({error, _}=Error) ->
    Reason = fn_error:normalize(Error),
    io:format("error:~s~n", [Reason]);
print(Data) ->
    try io:format("~s~n", [Data]) catch
        _:_ -> io:format("~p~n", [Data])
    end.

str_to_erl_ast(String, Module) ->
    State0 = fn_to_erl:new_state(Module),
    State = State0#{level => 1},

    case efene:str_to_ast(String) of
        {ok, Ast} -> {ok, fn_to_erl:ast_to_ast(Ast, State)};
        Other -> Other
    end.

pprint_strace(Strace) ->
    Lines = lists:map(fun ({ModName, FunName, Arity, Props}) ->
                              File = proplists:get_value(file, Props, "?"),
                              Line = proplists:get_value(line, Props, 0),
                              Args = [ModName, FunName, Arity, File, Line],
                              io_lib:format("~p.~p:~p ~s:~p", Args)
              end, Strace),
    Trace = string:join(Lines, "\n"),
    io:format("~s~n", [Trace]).
