-module(fn_repl).
-export([main/0, start/0]).

start() ->
 spawn(fun main/0).

loop(Bindings) ->
  case io:get_line(">>> ") of
      eof ->
          io:format("~nBye!~n");
      Input ->
          case handle_input(Input, Bindings) of
              {ok, NewBindings} ->
                  loop(NewBindings);
              {halt, NewBindings} ->
                  c:q(),
                  io:format("~nBye!~n"),
                  timer:sleep(5000), % to avoid printing the next ">>>"
                  loop(NewBindings)
          end
  end.

handle_input("exit\n", Bindings) ->
    {halt, Bindings};
handle_input(Input, Bindings) ->
    case str_to_erl_ast(Input, "repl") of
        {ok, {[Ast], _State}} ->
            try
                case erl_eval:expr(Ast, Bindings,
                                   {value, fun lfun_value_handler/2},
                                   {value, fun nlfun_value_handler/2}) of
                    {value, Value, B1} ->
                        print(Value),
                        {ok, B1};
                    Other ->
                        io:format("error: ~p~n", [Other]),
                        {ok, Bindings}
                end
            catch
                T:E ->
                    io:format("~p: ~p~n~n", [T, E]),
                    pprint_strace(erlang:get_stacktrace()),
                    {ok, Bindings}
            end;
        Other ->
            print(Other),
            {ok, Bindings}
    end.

lfun_value_handler(Name, Arguments) ->
    throw({not_defined, {Name, Arguments}}).

nlfun_value_handler({Mod, Fun}, Arguments) ->
    erlang:apply(Mod, Fun, Arguments).

main() ->
    io:format("efene shell (write exit to quit, Ctrl+g for Job Control Mode)~n~n"),
    Bindings = erl_eval:new_bindings(),
    loop(Bindings).

print({error, _}=Error) ->
    Reason = fn_error:normalize(Error),
    io:format("error:~s~n", [Reason]);
print(Data) ->
    io:format("~s~n", [pprint_data(Data)]).

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

pprint_data(V) when is_binary(V) ->
    io_lib:format("'~s'", [V]);
pprint_data({V}) ->
    io_lib:format("(~s,)", [pprint_data(V)]);
pprint_data(V) when is_tuple(V) ->
    Items = lists:map(fun pprint_data/1, tuple_to_list(V)),
    CsItems = string:join(Items, ", "),
    io_lib:format("(~s)", [CsItems]);
pprint_data(V) when is_list(V) ->
    IsPrintable = io_lib:printable_unicode_list(V),
    if IsPrintable ->
           % TODO: escape quotes
           io_lib:format("\"~s\"", [V]);
       true ->
           Items = lists:map(fun pprint_data/1, V),
           CsItems = string:join(Items, ", "),
           io_lib:format("[~s]", [CsItems])
    end;
pprint_data(M) when is_map(M) ->
    Items = lists:map(fun ({K, V}) ->
                             io_lib:format("~s: ~s", [pprint_data(K), pprint_data(V)])
                     end, maps:to_list(M)),
    CsItems = string:join(Items, ", "),
    io_lib:format("{~s}", [CsItems]);
pprint_data(V) ->
    io_lib:format("~p", [V]).
