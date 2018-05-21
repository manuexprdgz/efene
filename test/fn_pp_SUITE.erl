-module(fn_pp_SUITE).
-compile(export_all).
-include_lib("proper/include/proper.hrl").

all() -> [ind].

% init_per_suite(Config) ->
%     Config.

% end_per_suite(Config) ->
%     Config.

list_ind(_Config) -> 0.

match_A(A) -> 
	if		
		A == 0 -> "";
		A == 1 -> "  ";
		A == 2 -> "    ";
		A == 3 -> "      ";
		A == 4 -> "        "
	end.


ind(_Config) -> proper:quickcheck(?FORALL(A,list_ind(), match_A(A)==fn_pp:ind(A))).

fmt(_Config) -> ["a", "", [104, "b"], "\n"] == fn_pp:fmt("a", "h", true, 0, "b").