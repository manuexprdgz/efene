%%
%%MODULO DE PRUEBAS ASOCIADO A fn_pp.erl
%%Autor1: Manuel Exposito Rodriguez
%%Autor2: Javier Penas Noce
%%

-module(fn_pp_SUITE).
-compile(export_all).
-include_lib("proper/include/proper.hrl").

all() -> [ind, fmt, print_kvs].

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
	   Type =:= 'c' -> $a
	end.

 

args_gen() -> 
		?LET({Str, T}, {string(), elements(['s', 'c'])},
		{Str,select_gen(T), "~10.5"++T}).

fmt_gen() -> ?LET({Format},{type_gen()},{Format, io:format(Format, ["A"], "A")}).

fmt2() ->
	?FORALL({Str,Args,Fmt}, args_gen(), [Str, "",io:format(Fmt, [Args]), "\n"] == fn_pp:fmt(Str, Fmt, true, 0, Args)).


print_kvs(_Config) -> proper:quickcheck(print_kvs_test()).

print_kvs_test() -> ?FORALL({Items}, list(any()), print_kvs_result(Items) == fn_pp:print_kvs(Items)).

print_kvs_result([])  -> "";
print_kvs_result([H]) -> fn_pp:print(H, "", false, 0);
print_kvs_result([H|T]) -> fn_pp:print([H|T], "", false, 0, [], "", "").