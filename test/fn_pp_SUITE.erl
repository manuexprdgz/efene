%%
%%MODULO DE PRUEBAS ASOCIADO A fn_pp.erl
%%Autor1: Manuel Exposito Rodriguez
%%Autor2: Javier Penas Noce
%%

-module(fn_pp_SUITE).
-compile(export_all).
-include_lib("proper/include/proper.hrl").

-include("efene.hrl").

-define(operadores, ['~', 'not', 'andd', '&', '*', '/', '//', '%','orr', '+', '-', '|', '^', 'exor', '>>', '<<','++', '--','<', '<=', '>', '>=', '==', 'is', '!=', 'isnt','and','or','!', '=']).
-define(list300, ['~', 'not']).
-define(list400, ['andd', '&', '*', '/', '//', '%']).
-define(list500, ['orr', '+', '-', '|', '^', 'exor', '>>', '<<']).
-define(list600, ['++', '--']).
-define(list700, ['<', '<=', '>', '>=', '==', 'is', '!=', 'isnt']).
-define(list800, ['and']).
-define(list900, ['or']).
-define(list1000, ['!', '=']).


all() -> [ind, fmt, print_kvs, print_seq, print_path, print, precedence, print_catch, print_when, print_after].

init_per_suite(Config) ->
     Config.

end_per_suite(Config) ->
     Config.

list_ind(_Config) -> elements([0,1,2,3,4]).

check(Prop) -> 
	Result = proper:quickcheck(?MODULE:Prop()),
	if
	 	Result == false -> ct:fail({Prop, proper:counterexample()});
	 	Result == true -> true
	end .

match_A(A) -> 
	if		
		A == 0 -> "";
		A == 1 -> "  ";
		A == 2 -> "    ";
		A == 3 -> "      ";
		A == 4 -> "        "
	end.


ind(_Config) -> proper:quickcheck(?FORALL(A,list_ind(), match_A(A)==fn_pp:ind(A))).

fmt(_Config) -> proper:quickcheck(fmt2()).

type_gen() -> ?LET({T},{elements(['s'])},{T}).

select_gen(Type) -> 
	if Type =:= 's' -> "A";
	   Type =:= 'c' -> $a;
	   Type =:= 'p' -> [{atributes, [[{elemento1},{nombre, "JUAN"}], [{nombre, 'JAVIER'}]]}]
	end.

 

args_gen() -> 
		?LET({Str, T}, {string(), elements(['s', 'c', 'p'])},
		{Str,select_gen(T), "~10.5"++T}).

fmt_gen() -> ?LET({Format},{type_gen()},{Format, io:format(Format, ["A"], "A")}).

fmt2() ->
	?FORALL({Str,Args,Fmt}, args_gen(), [Str, "",io:format(Fmt, [Args]), "\n"] == fn_pp:fmt(Str, Fmt, true, 0, Args)).


print_kvs(_Config) -> proper:quickcheck(print_kvs_test()).

print_kvs_test() -> ?FORALL({Items}, list(any()), print_kvs_result(Items) == fn_pp:print_kvs(Items)).

print_kvs_result([])  -> "";
print_kvs_result([H]) -> fn_pp:print(H, "", false, 0);
print_kvs_result([H|T]) -> fn_pp:print([H|T], "", false, 0, [], "", "").


print_seq(_Config) -> proper:quickcheck(print_seq_test()).

print_seq_test() -> ?FORALL({Items}, list(any()), print_seq_result(Items) == fn_pp:print_seq(Items)).

print_seq_result([])  -> "";
print_seq_result([H]) -> fn_pp:print(H, "", false, 0);
print_seq_result([H|T]) -> fn_pp:print([H|T], "", false, 0, [], "", "").


print_path(_Config) -> proper:quickcheck(print_path_test()).

print_path_test() -> ?FORALL({Items}, list(any()), print_path_result(Items) == fn_pp:print_path(Items)).

print_path_result([])  -> "";
print_path_result([H]) -> fn_pp:print(H, "", false, 0);
print_path_result([H|T]) -> fn_pp:print([H|T], "", false, 0, [], "", "").


gen_list(Type) ->
	if Type =:= integer -> {pos_integer(), 'p'};
	   Type =:= boolean -> {bool(), 's'};
	   Type =:= string -> {string(), 's'};
	   Type =:= float -> {float(), 'p'}
	end.
gen_Args(Type,Lista) -> 
	if Type =:= integer -> [Lista];
	   Type =:= boolean -> [atom_to_list(Lista)];
	   Type =:= string -> [io:write_string(Lista, $")];
	   Type =:= float -> [Lista]
	end.

gen_printArgs() -> ?LET(
	{Tipo},
	{elements([integer, boolean, string, float])},
	gen_list(Tipo)).

print(_Config) -> proper:quickcheck(print_list_test()).

print_list_test() -> ?FORALL({Lista,Tipo}, gen_printArgs(), [Lista, "",io:format("~"++Tipo, gen_Args(Tipo,Lista)), "\n"] == fn_pp:print(Lista)).


precedence(_Config) -> proper:quickcheck(precedence_test()).

precedence_test() -> ?FORALL({Operador, Salida}, gen_precedence(), Salida == fn_pp:precedence(Operador)).

gen_precedence() -> ?LET({Operador}, {elements(?operadores)}, {Operador, select_output(Operador)}).


select_output(Operador) ->
	case lists:member(Operador, ['~', 'not', 'andd', '&', '*', '/', '//', '%','orr', '+', '-', '|', '^', 'exor', '>>', '<<','<', '<=', '>', '>=', '==', 'is', '!=', 'isnt','and','or']) of
		true -> {left, select_output_aux({300, ?list300}, [{400, ?list400}, {500, ?list500}, {700, ?list700}, {800, ?list800}, {900, ?list900}], Operador)};
		false -> {right, select_output_aux({600, ?list600}, [{1000, ?list1000}], Operador)}
	end.
select_output_aux({Num_lista, Lista}, [H|T], Operador) ->
	case lists:member(Operador, Lista) of 
		true -> Num_lista;
		false -> select_output_aux(H, T, Operador)
	end;
select_output_aux({Num_lista, Lista}, [], Operador) -> 
	case lists:member(Operador, Lista) of
		true -> Num_lista;
		false -> throw("error seleccionando el operador")
	end.


print_catch(_Config) -> proper:quickcheck(print_catch_test()).

print_catch_test() -> ?FORALL({Nl, Indent}, {any(), pos_integer()}, "" == fn_pp:print_catch(nocatch, Nl, Indent)).


print_after(_Config) -> proper:quickcheck(print_after_test()).

print_after_test() -> ?FORALL({Indent}, {pos_integer()}, "" == fn_pp:print_after(noafter, Indent)).


print_when(_Config) -> proper:quickcheck(print_when_test()).

print_when_test() -> ?FORALL({Arg}, {nowhen}, "" == fn_pp:print_when(nowhen)).