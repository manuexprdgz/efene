-module(fn_ext_val_atom_b).
-export([handle/3]).

-behaviour(fn_exts).

-include("efene.hrl").

handle(_Path, ?S(Line, map, TSList), State) ->
    type_specifiers_to_ast(Line, TSList, State);
handle(_Path, _Ast, _State) ->
    next.

type_specifiers_to_ast(Line, TSList, State) ->
    {RFields, State1} = lists:mapfoldl(fun to_bin_element/2, State, TSList),
    R = {bin, Line, RFields},
    {R, State1}.

var_to_erl_var(?Var(Line, Name)) -> {var, Line, Name}.

% if value is the _ var assume defaults
to_bin_element({kv, Line, Name=?Var(_), ?Var('_')}, State) ->
    BinElement =  {bin_element, Line, var_to_erl_var(Name), default, default},
    {BinElement, State};
to_bin_element({kv, Line, Name=?Var(_), ?Atom(ALine, AName)}, State) ->
    % reuse param logic by building a map {type: AName}
    to_bin_element({kv, Line, Name, ?S(ALine, map,
                                       [{kv, ALine, ?Atom(ALine, type),
                                         ?Atom(ALine, AName)}])}, State);
% if value is an integer assume it's the size
to_bin_element({kv, Line, Name=?Var(_), ?Int(SLine, SVal)}, State) ->
    BinElement =  {bin_element, Line, var_to_erl_var(Name),
                   {integer, SLine, SVal}, default},
    {BinElement, State};
to_bin_element({kv, Line, Name=?Var(_), ?S(_MapLine, map, Fields)}, State) ->
    InitialState =  {bin_element, Line, var_to_erl_var(Name), default, default},
    parse_bin_element_fields(Line, Fields, State, InitialState);

to_bin_element(Other, State) ->
    Line = element(2, Other),
    State1 = fn_to_erl:add_error(State, invalid_bin_type_specifier, Line,
                       fn_to_erl:expected_got("\"line\" or \"module\"", Other)),
    {{atom, Line, error}, State1}.

parse_bin_element_fields(_Line, [], State, BinElement) ->
    {BinElement, State};

parse_bin_element_fields(Line, [{kv, _Line, ?Atom(size), ?V(_, integer, _Size)=NewSize}|T],
                         State, {BeType, BeLine, BeName, _OldSize, Params}) ->
    {ENewSize, State1} = fn_to_erl:ast_to_ast(NewSize, State),
    NewBinElement = {BeType, BeLine, BeName, ENewSize, Params},
    parse_bin_element_fields(Line, T, State1, NewBinElement);

parse_bin_element_fields(Line, [{kv, _Line, ?Atom(unit), ?V(_, integer, Unit)}|T],
                         State, {BeType, BeLine, BeName, BeSize, Params})
                        when Unit >= 1 andalso Unit =< 256 ->
    {NewParams, State1} = add_bin_element_param(Params, {unit, Unit}, State),
    NewBinElement = {BeType, BeLine, BeName, BeSize, NewParams},
    parse_bin_element_fields(Line, T, State1, NewBinElement);

parse_bin_element_fields(Line, [{kv, KvLine, ?Atom(type), ?Atom(Type)}|T],
                         State, {BeType, BeLine, BeName, BeSize, Params}) ->
    ValidValues = [integer, float, binary, bytes, bitstring, bits, utf8, utf16, utf32],
    {NewParams, State1} = add_bin_element_param(KvLine, Params, Type, ValidValues, State),
    NewBinElement = {BeType, BeLine, BeName, BeSize, NewParams},
    parse_bin_element_fields(Line, T, State1, NewBinElement);

parse_bin_element_fields(Line, [{kv, KvLine, ?Atom(endianness), ?Atom(Endianness)}|T],
                         State, {BeType, BeLine, BeName, BeSize, Params}) ->
    ValidValues = [big, little, native],
    {NewParams, State1} = add_bin_element_param(KvLine, Params, Endianness, ValidValues, State),
    NewBinElement = {BeType, BeLine, BeName, BeSize, NewParams},
    parse_bin_element_fields(Line, T, State1, NewBinElement);

parse_bin_element_fields(Line, [{kv, KvLine, ?Atom(sign), ?Atom(Sign)}|T],
                         State, {BeType, BeLine, BeName, BeSize, Params}) ->
    ValidValues = [signed, unsigned],
    {NewParams, State1} = add_bin_element_param(KvLine, Params, Sign, ValidValues, State),
    NewBinElement = {BeType, BeLine, BeName, BeSize, NewParams},
    parse_bin_element_fields(Line, T, State1, NewBinElement);

parse_bin_element_fields(Line, [Other|T], State, BinElement) ->
    Msg = "one of val (var), size (integer), type (atom), sign (atom), endianness (atom), unit (1..256)",
    OtherLine = element(2, Other),
    State1 = fn_to_erl:add_error(State, invalid_bin_type_specifier_field, OtherLine,
                       fn_to_erl:expected_got(Msg, {ast, Other})),
    parse_bin_element_fields(Line, T, State1, BinElement).

add_bin_element_param(default, Param, State) ->
    add_bin_element_param([], Param, State);
add_bin_element_param(L, Param, State) ->
    {[Param|L], State}.

add_bin_element_param(Line, Params, Param, ValidValues, State) ->
    IsInValues = lists:member(Param, ValidValues),
    if IsInValues -> add_bin_element_param(Params, Param, State);
       true ->
           Msg = io_lib:format("one of ~p", [ValidValues]),
           State1 = fn_to_erl:add_error(State, invalid_bin_type_specifier_value, Line,
                              fn_to_erl:expected_got(Msg, Param)),
           {Params, State1}
    end.


