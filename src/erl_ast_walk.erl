%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 
%%     $Id$
%%
-module(erl_ast_walk).

%% A module to walk and maybe modify ast nodes

%% This module only traverses legal Erlang code. This is most noticeable
%% in guards where only a limited number of expressions are allowed.
%% N.B. if this module is to be used as a basis for tranforms then
%% all the error cases must be handled otherwise this module just crashes!

-export([walk/2, expr/2, exprs/2]).

walk(Forms, Fun) ->
    forms(Forms, Fun).

%% forms(Fs) -> lists:map(fun (F) -> form(F) end, Fs).

forms([F0|Fs0], Fun) ->
    F1 = form(F0, Fun),
    Fs1 = forms(Fs0, Fun),
    [F1|Fs1];
forms([], _Fun) -> [].

%% -type form(Form) -> Form.
%%  Here we show every known form and valid internal structure. We do not
%%  that the ordering is correct!

%% First the various attributes.
form({attribute,Line,module,Mod}, Fun) ->
    Fun({attribute,Line,module,Mod});
form({attribute,Line,file,{File,Line}}, Fun) ->	%This is valid anywhere.
    Fun({attribute,Line,file,{File,Line}});
form({attribute,Line,export,Es0}, Fun) ->
    Es1 = farity_list(Es0, Fun),
    Fun({attribute,Line,export,Es1});
form({attribute,Line,import,{Mod,Is0}}, Fun) ->
    Is1 = farity_list(Is0, Fun),
    Fun({attribute,Line,import,{Mod,Is1}});
form({attribute,Line,compile,C}, Fun) ->
    Fun({attribute,Line,compile,C});
form({attribute,Line,record,{Name,Defs0}}, Fun) ->
    Defs1 = record_defs(Defs0, Fun),
    Fun({attribute,Line,record,{Name,Defs1}});
form({attribute,Line,asm,{function,N,A,Code}}, Fun) ->
    Fun({attribute,Line,asm,{function,N,A,Code}});
form({attribute,Line,Attr,Val}, Fun) ->		%The general attribute.
    Fun({attribute,Line,Attr,Val});
form({function,Line,Name0,Arity0,Clauses0}, Fun) ->
    {Name,Arity,Clauses} = function(Name0, Arity0, Clauses0, Fun),
    Fun({function,Line,Name,Arity,Clauses});
% Mnemosyne, ignore...
form({rule,Line,Name,Arity,Body}, _Fun) ->
    {rule,Line,Name,Arity,Body}; % Dont dig into this
%% Extra forms from the parser.
form({error,E}, Fun) -> Fun({error,E});
form({warning,W}, Fun) -> Fun({warning,W});
form({eof,Line}, Fun) -> Fun({eof,Line}).

%% -type farity_list([Farity]) -> [Farity] when Farity <= {atom(),integer()}.

farity_list([{Name,Arity}|Fas], Fun) ->
    [Fun({Name,Arity})|farity_list(Fas, Fun)];
farity_list([], _Fun) -> [].

%% -type record_defs([RecDef]) -> [RecDef].
%%  N.B. Field names are full expressions here but only atoms are allowed
%%  by the *parser*!

record_defs([{record_field,Line,{atom,La,A},Val0}|Is], Fun) ->
    Val1 = expr(Val0, Fun),
    [Fun({record_field,Line,{atom,La,A},Val1})|record_defs(Is, Fun)];
record_defs([{record_field,Line,{atom,La,A}}|Is], Fun) ->
    [Fun({record_field,Line,{atom,La,A}})|record_defs(Is, Fun)];
record_defs([], _Fun) -> [].

%% -type function(atom(), integer(), [Clause]) -> {atom(),integer(),[Clause]}.

function(Name, Arity, Clauses0, Fun) ->
    Clauses1 = clauses(Clauses0, Fun),
    Fun({Name,Arity,Clauses1}).

%% -type clauses([Clause]) -> [Clause].

clauses([C0|Cs], Fun) ->
    C1 = clause(C0, Fun),
    [C1|clauses(Cs, Fun)];
clauses([], _Fun) -> [].

%% -type clause(Clause) -> Clause.

clause({clause,Line,H0,G0,B0}, Fun) ->
    H1 = head(H0, Fun),
    G1 = guard(G0, Fun),
    B1 = exprs(B0, Fun),
    Fun({clause,Line,H1,G1,B1}).

%% -type head([Pattern]) -> [Pattern].

head(Ps, Fun) -> patterns(Ps, Fun).

%% -type patterns([Pattern]) -> [Pattern].
%%  These patterns are processed "sequentially" for purposes of variable
%%  definition etc.

patterns([P0|Ps], Fun) ->
    P1 = pattern(P0, Fun),
    [P1|patterns(Ps, Fun)];
patterns([], _Fun) -> [].

%% -type pattern(Pattern) -> Pattern.
%%  N.B. Only valid patterns are included here.

pattern({var,Line,V}, Fun) -> Fun({var,Line,V});
pattern({match,Line,L0,R0}, Fun) ->
    L1 = pattern(L0, Fun),
    R1 = pattern(R0, Fun),
    Fun({match,Line,L1,R1});
pattern({integer,Line,I}, Fun) -> Fun({integer,Line,I});
pattern({char,Line,C}, Fun) -> Fun({char,Line,C});
pattern({float,Line,F}, Fun) -> Fun({float,Line,F});
pattern({atom,Line,A}, Fun) -> Fun({atom,Line,A});
pattern({string,Line,S}, Fun) -> Fun({string,Line,S});
pattern({nil,Line}, Fun) -> Fun({nil,Line});
pattern({cons,Line,H0,T0}, Fun) ->
    H1 = pattern(H0, Fun),
    T1 = pattern(T0, Fun),
    Fun({cons,Line,H1,T1});
pattern({tuple,Line,Ps0}, Fun) ->
    Ps1 = pattern_list(Ps0, Fun),
    Fun({tuple,Line,Ps1});
pattern({map,Line,Ps0}, Fun) ->
    Ps1 = pattern_list(Ps0, Fun),
    Fun({map,Line,Ps1});
pattern({map_field_exact,Line,K,V}, Fun) ->
    Ke = expr(K, Fun),
    Ve = pattern(V, Fun),
    Fun({map_field_exact,Line,Ke,Ve});
%%pattern({struct,Line,Tag,Ps0}) ->
%%    Ps1 = pattern_list(Ps0),
%%    {struct,Line,Tag,Ps1};
pattern({record,Line,Name,Pfs0}, Fun) ->
    Pfs1 = pattern_fields(Pfs0, Fun),
    Fun({record,Line,Name,Pfs1});
pattern({record_index,Line,Name,Field0}, Fun) ->
    Field1 = pattern(Field0, Fun),
    Fun({record_index,Line,Name,Field1});
pattern({record_field,Line,Rec0,Name,Field0}, Fun) ->
    Rec1 = expr(Rec0, Fun),
    Field1 = expr(Field0, Fun),
    Fun({record_field,Line,Rec1,Name,Field1});
pattern({record_field,Line,Rec0,Field0}, Fun) ->
    Rec1 = expr(Rec0, Fun),
    Field1 = expr(Field0, Fun),
    Fun({record_field,Line,Rec1,Field1});
pattern({bin,Line,Fs}, Fun) ->
    Fs2 = pattern_grp(Fs, Fun),
    Fun({bin,Line,Fs2});
pattern({op,Line,Op,A}, Fun) ->
    Fun({op,Line,Op,A});
pattern({op,Line,Op,L,R}, Fun) ->
    Fun({op,Line,Op,L,R}).

pattern_grp([{bin_element,L1,E1,S1,T1} | Fs], Fun) ->
    S2 = case S1 of
	     default ->
		 default;
	     _ ->
		 expr(S1, Fun)
	 end,
    T2 = case T1 of
	     default ->
		 default;
	     _ ->
		 bit_types(T1, Fun)
	 end,
    [Fun({bin_element,L1,expr(E1, Fun),S2,T2}) | pattern_grp(Fs, Fun)];
pattern_grp([], _Fun) ->
    [].

bit_types([], _Fun) ->
    [];
bit_types([Atom | Rest], Fun) when is_atom(Atom) ->
    [Atom | bit_types(Rest, Fun)];
bit_types([{Atom, Integer} | Rest], Fun) when is_atom(Atom), is_integer(Integer) ->
    [{Atom, Integer} | bit_types(Rest, Fun)].



%% -type pattern_list([Pattern]) -> [Pattern].
%%  These patterns are processed "in parallel" for purposes of variable
%%  definition etc.

pattern_list([P0|Ps], Fun) ->
    P1 = pattern(P0, Fun),
    [P1|pattern_list(Ps, Fun)];
pattern_list([], _Fun) -> [].

%% -type pattern_fields([Field]) -> [Field].
%%  N.B. Field names are full expressions here but only atoms are allowed
%%  by the *linter*!.

pattern_fields([{record_field,Lf,{atom,La,F},P0}|Pfs], Fun) ->
    P1 = pattern(P0, Fun),
    [Fun({record_field,Lf,{atom,La,F},P1})|pattern_fields(Pfs, Fun)];
pattern_fields([{record_field,Lf,{var,La,'_'},P0}|Pfs], Fun) ->
    P1 = pattern(P0, Fun),
    [Fun({record_field,Lf,{var,La,'_'},P1})|pattern_fields(Pfs, Fun)];
pattern_fields([], _Fun) -> [].

%% -type guard([GuardTest]) -> [GuardTest].

guard([G0|Gs], Fun) when is_list(G0) ->
    [guard0(G0, Fun) | guard(Gs, Fun)];
guard(L, Fun) ->
    guard0(L, Fun).

guard0([G0|Gs], Fun) ->
    G1 = guard_test(G0, Fun),
    [G1|guard0(Gs, Fun)];
guard0([], _Fun) -> [].

guard_test(Expr={call,Line,{atom,La,F},As0}, Fun) ->
    case erl_internal:type_test(F, length(As0)) of
	true -> 
	    As1 = gexpr_list(As0, Fun),
	    Fun({call,Line,{atom,La,F},As1});
	_ ->
	    gexpr(Expr, Fun)
    end;
guard_test(Any, Fun) ->
    gexpr(Any, Fun).

%% Before R9, there were special rules regarding the expressions on
%% top level in guards. Those limitations are now lifted - therefore
%% there is no need for a special clause for the toplevel expressions.
%% -type gexpr(GuardExpr) -> GuardExpr.

gexpr({var,Line,V}, Fun) -> Fun({var,Line,V});
gexpr({integer,Line,I}, Fun) -> Fun({integer,Line,I});
gexpr({char,Line,C}, Fun) -> Fun({char,Line,C});
gexpr({float,Line,F}, Fun) -> Fun({float,Line,F});
gexpr({atom,Line,A}, Fun) -> Fun({atom,Line,A});
gexpr({string,Line,S}, Fun) -> Fun({string,Line,S});
gexpr({nil,Line}, Fun) -> Fun({nil,Line});
gexpr({map,Line,Map0,Es0}, Fun) ->
    [Map1|Es1] = gexpr_list([Map0|Es0], Fun),
    Fun({map,Line,Map1,Es1});
gexpr({map,Line,Es0}, Fun) ->
    Es1 = gexpr_list(Es0, Fun),
    Fun({map,Line,Es1});
gexpr({map_field_assoc,Line,K,V}, Fun) ->
    Ke = gexpr(K, Fun),
    Ve = gexpr(V, Fun),
    Fun({map_field_assoc,Line,Ke,Ve});
gexpr({map_field_exact,Line,K,V}, Fun) ->
    Ke = gexpr(K, Fun),
    Ve = gexpr(V, Fun),
    Fun({map_field_exact,Line,Ke,Ve});
gexpr({cons,Line,H0,T0}, Fun) ->
    H1 = gexpr(H0, Fun),
    T1 = gexpr(T0, Fun),				%They see the same variables
    Fun({cons,Line,H1,T1});
gexpr({tuple,Line,Es0}, Fun) ->
    Es1 = gexpr_list(Es0, Fun),
    Fun({tuple,Line,Es1});
gexpr({record_index,Line,Name,Field0}, Fun) ->
    Field1 = gexpr(Field0, Fun),
    Fun({record_index,Line,Name,Field1});
gexpr({record_field,Line,Rec0,Name,Field0}, Fun) ->
    Rec1 = gexpr(Rec0, Fun),
    Field1 = gexpr(Field0, Fun),
    Fun({record_field,Line,Rec1,Name,Field1});
gexpr({record,Line,Name,Inits0}, Fun) ->
    Inits1 = grecord_inits(Inits0, Fun),
    Fun({record,Line,Name,Inits1});
gexpr({call,Line,{atom,La,F},As0}, Fun) ->
    case erl_internal:guard_bif(F, length(As0)) of
	true -> As1 = gexpr_list(As0, Fun),
		Fun({call,Line,{atom,La,F},As1})
    end;
% Guard bif's can be remote, but only in the module erlang...
gexpr({call,Line,{remote,La,{atom,Lb,erlang},{atom,Lc,F}},As0}, Fun) ->
    case erl_internal:guard_bif(F, length(As0)) or
	 erl_internal:arith_op(F, length(As0)) or 
	 erl_internal:comp_op(F, length(As0)) or
	 erl_internal:bool_op(F, length(As0)) of
	true -> As1 = gexpr_list(As0, Fun),
		Fun({call,Line,{remote,La,{atom,Lb,erlang},{atom,Lc,F}},As1})
    end;
gexpr({bin,Line,Fs}, Fun) ->
    Fs2 = pattern_grp(Fs, Fun),
    Fun({bin,Line,Fs2});
gexpr({op,Line,Op,A0}, Fun) ->
    case erl_internal:arith_op(Op, 1) or 
	 erl_internal:bool_op(Op, 1) of
	true -> A1 = gexpr(A0, Fun),
		Fun({op,Line,Op,A1})
    end;
gexpr({op,Line,Op,L0,R0}, Fun) when Op =:= 'andalso'; Op =:= 'orelse' ->
    %% R11B: andalso/orelse are now allowed in guards.
    L1 = gexpr(L0, Fun),
    R1 = gexpr(R0, Fun),			%They see the same variables
    Fun({op,Line,Op,L1,R1});
gexpr({op,Line,Op,L0,R0}, Fun) ->
    case erl_internal:arith_op(Op, 2) or
	  erl_internal:bool_op(Op, 2) or 
	  erl_internal:comp_op(Op, 2) of
	true ->
	    L1 = gexpr(L0, Fun),
	    R1 = gexpr(R0, Fun),			%They see the same variables
	    Fun({op,Line,Op,L1,R1})
    end.

%% -type gexpr_list([GuardExpr]) -> [GuardExpr].
%%  These expressions are processed "in parallel" for purposes of variable
%%  definition etc.

gexpr_list([E0|Es], Fun) ->
    E1 = gexpr(E0, Fun),
    [E1|gexpr_list(Es, Fun)];
gexpr_list([], _Fun) -> [].

grecord_inits([{record_field,Lf,{atom,La,F},Val0}|Is], Fun) ->
    Val1 = gexpr(Val0, Fun),
    [Fun({record_field,Lf,{atom,La,F},Val1})|grecord_inits(Is, Fun)];
grecord_inits([{record_field,Lf,{var,La,'_'},Val0}|Is], Fun) ->
    Val1 = gexpr(Val0, Fun),
    [Fun({record_field,Lf,{var,La,'_'},Val1})|grecord_inits(Is, Fun)];
grecord_inits([], _Fun) -> [].

%% -type exprs([Expression]) -> [Expression].
%%  These expressions are processed "sequentially" for purposes of variable
%%  definition etc.

exprs([E0|Es], Fun) ->
    E1 = expr(E0, Fun),
    [E1|exprs(Es, Fun)];
exprs([], _Fun) -> [].

%% -type expr(Expression) -> Expression.

expr({var,Line,V}, Fun) -> Fun({var,Line,V});
expr({integer,Line,I}, Fun) -> Fun({integer,Line,I});
expr({float,Line,F}, Fun) -> Fun({float,Line,F});
expr({atom,Line,A}, Fun) -> Fun({atom,Line,A});
expr({string,Line,S}, Fun) -> Fun({string,Line,S});
expr({char,Line,C}, Fun) -> Fun({char,Line,C});
expr({nil,Line}, Fun) -> Fun({nil,Line});
expr({cons,Line,H0,T0}, Fun) ->
    H1 = expr(H0, Fun),
    T1 = expr(T0, Fun),				%They see the same variables
    Fun({cons,Line,H1,T1});
expr({lc,Line,E0,Qs0}, Fun) ->
    Qs1 = lc_bc_quals(Qs0, Fun),
    E1 = expr(E0, Fun),
    Fun({lc,Line,E1,Qs1});
expr({bc,Line,E0,Qs0}, Fun) ->
    Qs1 = lc_bc_quals(Qs0, Fun),
    E1 = expr(E0, Fun),
    Fun({bc,Line,E1,Qs1});
expr({tuple,Line,Es0}, Fun) ->
    Es1 = expr_list(Es0, Fun),
    Fun({tuple,Line,Es1});
expr({map,Line,Map0,Es0}, Fun) ->
    [Map1|Es1] = exprs([Map0|Es0], Fun),
    Fun({map,Line,Map1,Es1});
expr({map,Line,Es0}, Fun) ->
    Es1 = exprs(Es0, Fun),
    Fun({map,Line,Es1});
expr({map_field_assoc,Line,K,V}, Fun) ->
    Ke = expr(K, Fun),
    Ve = expr(V, Fun),
    Fun({map_field_assoc,Line,Ke,Ve});
expr({map_field_exact,Line,K,V}, Fun) ->
    Ke = expr(K, Fun),
    Ve = expr(V, Fun),
    Fun({map_field_exact,Line,Ke,Ve});
%%expr({struct,Line,Tag,Es0}) ->
%%    Es1 = pattern_list(Es0),
%%    {struct,Line,Tag,Es1};
expr({record_index,Line,Name,Field0}, Fun) ->
    Field1 = expr(Field0, Fun),
    Fun({record_index,Line,Name,Field1});
expr({record,Line,Name,Inits0}, Fun) ->
    Inits1 = record_inits(Inits0, Fun),
    Fun({record,Line,Name,Inits1});
expr({record_field,Line,Rec0,Name,Field0}, Fun) ->
    Rec1 = expr(Rec0, Fun),
    Field1 = expr(Field0, Fun),
    Fun({record_field,Line,Rec1,Name,Field1});
expr({record,Line,Rec0,Name,Upds0}, Fun) ->
    Rec1 = expr(Rec0, Fun),
    Upds1 = record_updates(Upds0, Fun),
    Fun({record,Line,Rec1,Name,Upds1});
expr({record_field,Line,Rec0,Field0}, Fun) ->
    Rec1 = expr(Rec0, Fun),
    Field1 = expr(Field0, Fun),
    Fun({record_field,Line,Rec1,Field1});
expr({block,Line,Es0}, Fun) ->
    %% Unfold block into a sequence.
    Es1 = exprs(Es0, Fun),
    Fun({block,Line,Es1});
expr({'if',Line,Cs0}, Fun) ->
    Cs1 = icr_clauses(Cs0, Fun),
    Fun({'if',Line,Cs1});
expr({'case',Line,E0,Cs0}, Fun) ->
    E1 = expr(E0, Fun),
    Cs1 = icr_clauses(Cs0, Fun),
    Fun({'case',Line,E1,Cs1});
expr({'receive',Line,Cs0}, Fun) ->
    Cs1 = icr_clauses(Cs0, Fun),
    Fun({'receive',Line,Cs1});
expr({'receive',Line,Cs0,To0,ToEs0}, Fun) ->
    To1 = expr(To0, Fun),
    ToEs1 = exprs(ToEs0, Fun),
    Cs1 = icr_clauses(Cs0, Fun),
    Fun({'receive',Line,Cs1,To1,ToEs1});
expr({'try',Line,Es0,Scs0,Ccs0,As0}, Fun) ->
    Es1 = exprs(Es0, Fun),
    Scs1 = icr_clauses(Scs0, Fun),
    Ccs1 = icr_clauses(Ccs0, Fun),
    As1 = exprs(As0, Fun),
    Fun({'try',Line,Es1,Scs1,Ccs1,As1});
expr({'fun',Line,Body}, Fun) ->
    case Body of
	{clauses,Cs0} ->
	    Cs1 = fun_clauses(Cs0, Fun),
	    Fun({'fun',Line,{clauses,Cs1}});
	{function,F,A} ->
	    Fun({'fun',Line,{function,F,A}});
	{function,M,F,A} when is_atom(M), is_atom(F), is_integer(A) ->
	    %% R10B-6: fun M:F/A. (Backward compatibility)
	    Fun({'fun',Line,{function,M,F,A}});
	{function,M0,F0,A0} ->
	    %% R15: fun M:F/A with variables.
	    M = expr(M0, Fun),
	    F = expr(F0, Fun),
	    A = expr(A0, Fun),
	    Fun({'fun',Line,{function,M,F,A}})
    end;
expr({named_fun,Loc,Name,Cs}, Fun) ->
    Fun({named_fun,Loc,Name,fun_clauses(Cs, Fun)});
expr({call,Line,F0,As0}, Fun) ->
    %% N.B. If F an atom then call to local function or BIF, if F a
    %% remote structure (see below) then call to other module,
    %% otherwise apply to "function".
    F1 = expr(F0, Fun),
    As1 = expr_list(As0, Fun),
    Fun({call,Line,F1,As1});
expr({'catch',Line,E0}, Fun) ->
    %% No new variables added.
    E1 = expr(E0, Fun),
    Fun({'catch',Line,E1});
expr({match,Line,P0,E0}, Fun) ->
    E1 = expr(E0, Fun),
    P1 = pattern(P0, Fun),
    Fun({match,Line,P1,E1});
expr({bin,Line,Fs}, Fun) ->
    Fs2 = pattern_grp(Fs, Fun),
    Fun({bin,Line,Fs2});
expr({op,Line,Op,A0}, Fun) ->
    A1 = expr(A0, Fun),
    Fun({op,Line,Op,A1});
expr({op,Line,Op,L0,R0}, Fun) ->
    L1 = expr(L0, Fun),
    R1 = expr(R0, Fun),				%They see the same variables
    Fun({op,Line,Op,L1,R1});
%% The following are not allowed to occur anywhere!
expr({remote,Line,M0,F0}, Fun) ->
    M1 = expr(M0, Fun),
    F1 = expr(F0, Fun),
    Fun({remote,Line,M1,F1}).

%% -type expr_list([Expression]) -> [Expression].
%%  These expressions are processed "in parallel" for purposes of variable
%%  definition etc.

expr_list([E0|Es], Fun) ->
    E1 = expr(E0, Fun),
    [E1|expr_list(Es, Fun)];
expr_list([], _Fun) -> [].

%% -type record_inits([RecordInit]) -> [RecordInit].
%%  N.B. Field names are full expressions here but only atoms are allowed
%%  by the *linter*!.

record_inits([{record_field,Lf,{atom,La,F},Val0}|Is], Fun) ->
    Val1 = expr(Val0, Fun),
    [Fun({record_field,Lf,{atom,La,F},Val1})|record_inits(Is, Fun)];
record_inits([{record_field,Lf,{var,La,'_'},Val0}|Is], Fun) ->
    Val1 = expr(Val0, Fun),
    [Fun({record_field,Lf,{var,La,'_'},Val1})|record_inits(Is, Fun)];
record_inits([], _Fun) -> [].

%% -type record_updates([RecordUpd]) -> [RecordUpd].
%%  N.B. Field names are full expressions here but only atoms are allowed
%%  by the *linter*!.

record_updates([{record_field,Lf,{atom,La,F},Val0}|Us], Fun) ->
    Val1 = expr(Val0, Fun),
    [Fun({record_field,Lf,{atom,La,F},Val1})|record_updates(Us, Fun)];
record_updates([], _Fun) -> [].

%% -type icr_clauses([Clause]) -> [Clause].

icr_clauses([C0|Cs], Fun) ->
    C1 = clause(C0, Fun),
    [C1|icr_clauses(Cs, Fun)];
icr_clauses([], _Fun) -> [].

%% -type lc_bc_quals([Qualifier]) -> [Qualifier].
%%  Allow filters to be both guard tests and general expressions.

lc_bc_quals([{generate,Line,P0,E0}|Qs], Fun) ->
    E1 = expr(E0, Fun),
    P1 = pattern(P0, Fun),
    [Fun({generate,Line,P1,E1})|lc_bc_quals(Qs, Fun)];
lc_bc_quals([{b_generate,Line,P0,E0}|Qs], Fun) ->
    E1 = expr(E0, Fun),
    P1 = pattern(P0, Fun),
    [Fun({b_generate,Line,P1,E1})|lc_bc_quals(Qs, Fun)];
lc_bc_quals([E0|Qs], Fun) ->
    E1 = expr(E0, Fun),
    [E1|lc_bc_quals(Qs, Fun)];
lc_bc_quals([], _Fun) -> [].

%% -type fun_clauses([Clause]) -> [Clause].

fun_clauses([C0|Cs], Fun) ->
    C1 = clause(C0, Fun),
    [C1|fun_clauses(Cs, Fun)];
fun_clauses([], _Fun) -> [].
