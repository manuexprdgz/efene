%% Copyright 2015 Mariano Guerra
%% 
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%%     http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(fn_error).
-export([to_string/2, normalize/1]).

to_string(Module, Errors) when is_list(Errors) ->
    lists:map(fun (Error) -> to_string(Module, Error) end, Errors);
to_string(Module, {Type, Line, Details}) ->
    TypeStr = type_to_string(Type),
    DetailsStr = details_to_string(Details),
    io_lib:format("~p:~p:~p: ~s at line ~p: ~s~n", [Module, Line, Type, TypeStr, Line, DetailsStr]).

type_to_string(invalid_fn_ref) -> <<"Invalid Function Reference">>;
type_to_string(invalid_bin_type_specifier_field) -> <<"Invalid Type Specifier Field">>;
type_to_string(invalid_bin_type_specifier_value) -> <<"Invalid Type Specifier Value">>;
type_to_string(unknown_compiler_info) -> <<"Unknown Compiler Info Name">>;
type_to_string(case_mismatch) -> <<"Case Mismatch">>;
type_to_string(bad_record_field_init) -> <<"Bad Record Field Initialization">>;
type_to_string(bad_record_field_decl) -> <<"Bad Record Field Declaration">>;
type_to_string(invalid_export) -> <<"Invalid Export">>;
type_to_string(invalid_expression) -> <<"Invalid Expression">>;
type_to_string(invalid_top_level_expression) -> <<"Invalid Top Level Expression">>;
type_to_string(invalid_type_declaration) -> <<"Invalid Type Declaration">>;
type_to_string(invalid_type_value) -> <<"Invalid Type Value">>;
type_to_string(invalid_type_argument) -> <<"Invalid Type Argument">>;
type_to_string(invalid_catch) -> <<"Invalid Catch">>;
type_to_string(duplicated_function_spec) -> <<"Duplicated Function Spec">>;
type_to_string(Other) -> atom_to_list(Other).

format_maybe_ast({ast, Ast}) -> fn_pp:print(Ast);
format_maybe_ast(Other) -> io_lib:format("~p", [Other]).

details_to_string({expected, Expected, got, Got}) when is_list(Expected) ->
    io_lib:format("Expected ~s got ~s", [Expected, format_maybe_ast(Got)]);
details_to_string({expected, Expected, got, Got}) ->
    io_lib:format("Expected ~p got ~s", [Expected, format_maybe_ast(Got)]);
details_to_string(Other) -> format_maybe_ast(Other).

normalize({error, {Line, fn_parser, Reason}}) ->
    io_lib:format("~p: parse error: '~s'", [Line, Reason]);
normalize({error, {Line, fn_lexer, {illegal, Reason}}}) ->
    io_lib:format("~p: illegal char ~p", [Line, Reason]);
normalize({error, {Line, fn_lexer, {eof, _}}}) ->
    io_lib:format("~p: end of file", [Line]);
normalize({error, {efene, _Module, Reason}}) ->
    io_lib:format("~s", [Reason]);
normalize({error, Other}) ->
    io_lib:format("~p", [Other]);
normalize({Line, erl_lint, Reason}) ->
    normalize_erl_lint(Line, Reason);
normalize({Line, sys_core_fold, Reason}) ->
    io_lib:format("line ~p: ~s", [Line, normalize_sys_core_fold(Reason)]);
normalize({_Path, [{_, erl_lint, _Reason}|_]=Errors}) ->
    ErrorsStrs = [normalize(Error) || Error <- Errors],
    [string:join(ErrorsStrs, "\n"), "\n"];
normalize(Other) ->
    io_lib:format("~p", [Other]).

normalize_sys_core_fold(no_clause_match) ->
    "no clause will match";
normalize_sys_core_fold(nomatch_guard) ->
    "no guard will match";
normalize_sys_core_fold(useless_building) ->
    "A term is constructed, but never used";
normalize_sys_core_fold(Other) ->
    atom_to_list(Other).

normalize_erl_lint(Line, Reason) ->
    io_lib:format("line ~p: ~s", [Line, normalize_erl_lint(Reason)]).

normalize_erl_lint({attribute, Attr}) ->
    io_lib:format("bad attribute: ~p", [Attr]);
normalize_erl_lint({bad_export_type, ETs}) ->
    io_lib:format("bad export type: ~p", [ETs]);
normalize_erl_lint({bad_on_load_arity, Fa}) ->
    io_lib:format("bad on load arity: ~s", [format_mfa_or_na(Fa)]);
normalize_erl_lint({bad_on_load, Val}) ->
    io_lib:format("bad on load: ~p", [Val]);
normalize_erl_lint({behaviour_info, Fa}) ->
    io_lib:format("bad behaviour info: ~p", [Fa]);
normalize_erl_lint({builtin_type, TypePair}) ->
    io_lib:format("bad type pair: ~s", [format_mfa_or_na(TypePair)]);
normalize_erl_lint({define_import, NA}) ->
    io_lib:format("bad define import: ~s", [format_mfa_or_na(NA)]);
normalize_erl_lint({field_name_is_variable, Name, V}) ->
    io_lib:format("field name is variable: ~p ~p", [Name, V]);
normalize_erl_lint(illegal_bin_pattern) ->
    "illegal bin pattern";
normalize_erl_lint(illegal_bitsize) ->
    "illegal bit size";
normalize_erl_lint(illegal_expr) ->
    "illegal expression";
normalize_erl_lint(illegal_guard_expr) ->
    "illegal guard expression";
normalize_erl_lint({illegal_guard_local_call, {is_record, _WasALiteral3}}) ->
    "illegal guard expression, local call, is record";
normalize_erl_lint({illegal_guard_local_call, {F, A}}) ->
    io_lib:format("illegal guard expression, locall cal: ~p:~p", [F, A]);
normalize_erl_lint(illegal_map_construction) ->
    "illegal map construction";
normalize_erl_lint(illegal_map_key) ->
    "illegal map key";
normalize_erl_lint({illegal_map_key_variable,Var}) ->
    io_lib:format("illegal map key, variable: ~p", [Var]);
normalize_erl_lint(illegal_pattern) ->
    "illegal pattern";
normalize_erl_lint(illegal_record_info) ->
    "illegal record_info usage";
normalize_erl_lint({mnemosyne, "rule"}) ->
    "mnemosyne (?)";
normalize_erl_lint(multiple_on_loads) ->
    "multiple on_loads";
normalize_erl_lint(pmod_unsupported) ->
    "pmod unsuported";
normalize_erl_lint({redefine_callback, MFA}) ->
    io_lib:format("redefine callback: ~s", [format_mfa_or_na(MFA)]);
normalize_erl_lint({redefine_field, Name, F}) ->
    io_lib:format("redefine field: ~p ~p", [Name, F]);
normalize_erl_lint({redefine_function, NA}) ->
    io_lib:format("redefine function: ~s", [format_mfa_or_na(NA)]);
normalize_erl_lint(redefine_module) ->
    "redefine module";
normalize_erl_lint({redefine_record, Name}) ->
    io_lib:format("redefine record: ~p", [Name]);
normalize_erl_lint({redefine_spec, MFA}) ->
    io_lib:format("redefine spec: ~s", [format_mfa_or_na(MFA)]);
normalize_erl_lint({redefine_type, TypePair}) ->
    io_lib:format("redefine type: ~s", [format_mfa_or_na(TypePair)]);
normalize_erl_lint({singleton_typevar, Var}) ->
    io_lib:format("singleton typevar: ~p", [Var]);
normalize_erl_lint({spec_fun_undefined, MFA}) ->
    io_lib:format("spec function undefined: ~s", [format_mfa_or_na(MFA)]);
normalize_erl_lint(spec_wrong_arity) ->
    "spec with wrong arity";
normalize_erl_lint({too_many_arguments, Arity}) ->
    io_lib:format("to many arguments: ~p", [Arity]);
normalize_erl_lint(typed_literal_string) ->
    "typed literal string";
normalize_erl_lint({type_syntax, Type}) ->
    io_lib:format("type syntax: ~p", [Type]);
normalize_erl_lint({unbound_var, V}) ->
    io_lib:format("unbound variable: ~p", [V]);
normalize_erl_lint({unused_var, V}) ->
    io_lib:format("unused variable: ~p", [V]);
normalize_erl_lint({unused_record, V}) ->
    io_lib:format("unused record: ~p", [V]);
normalize_erl_lint({unused_type, NA}) ->
    io_lib:format("unused type: ~s", [format_mfa_or_na(NA)]);
normalize_erl_lint({unused_function, MFA}) ->
    io_lib:format("unused type: ~s", [format_mfa_or_na(MFA)]);
normalize_erl_lint({undefined_field, Name, F}) ->
    io_lib:format("undefined field: ~p ~p", [Name, F]);
normalize_erl_lint({undefined_function, NA}) ->
    io_lib:format("undefined function: ~s", [format_mfa_or_na(NA)]);
normalize_erl_lint(undefined_module) ->
    "undefined module";
normalize_erl_lint({undefined_on_load, Fa}) ->
    io_lib:format("undefined on_load: ~p", [Fa]);
normalize_erl_lint({undefined_record, Name}) ->
    io_lib:format("undefined record: ~p", [Name]);
normalize_erl_lint({undefined_type, {Type, Arity}}) ->
    io_lib:format("undefined type: ~p:~p", [Type, Arity]);
normalize_erl_lint({unsafe_var, V, In}) ->
    io_lib:format("unsafe variable: ~p ~p", [V, In]);
normalize_erl_lint(unsized_binary_in_bin_gen_pattern) ->
    "unsized binary in binary generator pattern";
normalize_erl_lint(unsized_binary_not_at_end) ->
    "unsized binary not at end";
normalize_erl_lint({variable_in_record_def, V}) ->
    io_lib:format("variable in record definition: ~p", [V]);
normalize_erl_lint({wildcard_in_update, Name}) ->
    io_lib:format("wildcard in update: ~p", [Name]);
normalize_erl_lint({call_to_redefined_old_bif, NA}) ->
    io_lib:format("call to redefined old bif: ~s", [format_mfa_or_na(NA)]);
normalize_erl_lint({duplicated_export, NA}) ->
    io_lib:format("duplicated export: ~s", [format_mfa_or_na(NA)]);
normalize_erl_lint({not_exported_opaque, NA}) ->
    io_lib:format("not exported opaque: ~s", [format_mfa_or_na(NA)]);
normalize_erl_lint({deprecated, NA, Desc}) ->
    io_lib:format("deprecated: ~s ~s", [format_mfa_or_na(NA), Desc]);
normalize_erl_lint({bad_bitsize, Val}) ->
    io_lib:format("bad bitsize: ~s", [Val]);
normalize_erl_lint(Other) ->
    io_lib:format("other: ~p", [Other]).

format_mfa_or_na({Name, Arity}) -> io_lib:format("~s:~p", [Name, Arity]);
format_mfa_or_na({Module, Name, Arity}) -> io_lib:format("~s.~s:~p", [Module, Name, Arity]);
format_mfa_or_na(Other) -> io_lib:format("~p", [Other]).
