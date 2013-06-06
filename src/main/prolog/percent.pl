% -------------------------------------------------------------------------------
% Copyright 2013 DevClear
% 
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
% 
%   http://www.apache.org/licenses/LICENSE-2.0
% 
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.
%-------------------------------------------------------------------------------
:- module(percent,[percent/2, prepare_percent/2]).

:- use_module(library(random)).
:- use_module(library(lists)).
:- use_module(library(between)).
:- use_module(utils).
:- load_files(library(plunit),[if(changed),load_type(source)]).

:- op(520,yfx,'..').  % a little higher than '-'
:- op(200,fy,':').  % same as '+'
%:- op(950,fy,pct).
%:- op(950,xfy,pct).
:- op(1150,fy,pct).
:- op(1150,xfy,pct).

% File: percent.pl
% Author: Brendan McCarhty
%
% This file contains utilities for random value generation based on percentage expressions.
% This includes the percent/2 and prepare_percent/2 predicates as well as term expansion for terms 
% using the 'pct' operator in the clause head, e.g.:
%
%  20 pct foo(a).
%  30 pct foo(b)
%  pct foo(c)
%
% Predicates defined this way are rewritten such that any invocation will attempt to unify
% with one and only one of the clauses, where the chosen clause is weighted toward the
% provided percentages -- these should not exceed 100, and clauses with no value are assigned
% equal probability of the remaining total from the sum of all explicitly valued clauses (the
% third clause above, for example, is assigned a percentage probability of 50%). Note that if
% the randomly-chosen clause fails, then another clause is not attempted for a given call.

:- meta_predicate percent(+,2).
:- meta_predicate prepare_percent(2,+).

% percent(?Value,:Expr)
% Value is randomly consistent with Expr. Expr can be:
%
% (1) A simple value.
% (2) A range (Min..Max) optionally with a qualification function e.g. (Min..Fn..Max), where Fn may be:
%     * avg(X)
%     * step(X)
%     The range is inclusive. Min and Max may be numbers or dates as in '1-jan-2012', which simply
%     provides a readable format that is immediately converted into epoch seconds. When Min is
%     a date, Max may also be a relative offset such as days(100), hours(16), etc.
% (3) A list of values to draw from, where each element of the list is an (Expr) or (Expr:P),
%     where P is the probability of selecting that entry. Remaining elements with no explicit P are assigned
%     remaining probability equally thus in [a:50,b,c] both 'b' and 'c' will have a 25% chance of matching.
% (4) A merge of lists (Expr1 + Expr2), where the merged result is then treated equivalently to the previous case.
%     For any pair of matching entries between the two lists, if one has a P supplied (Expr:P), then that P will 
%     be included in the merged result. For example ([a,b:40,c:20] + [b,d]) merges to [a,b:40,c:20,d].
% (5) A reference to a user-defined predicate (:pred) whose first argument will provide a domain of values.
%     Syntax note: insert a space when combined with '+', e.g (a + : b), not (a +: b).
% (6) A list-generator of the form F(LenExpr,Expr) that will produce a list of the supplied LenExpr.
%     The 'F' may be one of the following:
%     * bag - may contain dups
%     * set - will not contain dups
%     * kit - like 'set', except that it will reduce the length if it cannot possibly be satisfied
%     In all cases, a result that might otherwise evaluate to the empty set will fail.
%
%     Note 'set' may safely be applied to large domain expressions such as in (set(2,1..9999999)) since it
%     does not (necessarily) pre-generate all elements of the domain. To do this however it must generate
%     then retry duplicate entries. Since this is a potentially intractable operation it meters itself
%     and throws an exception after a 'reasonable' number of tries. 'Kit' on the other hand always
%     pre-generates if possible and thus simply fails if there is no possible answer.
%
percent(V,Mod:Expr) :- once(peval(Expr,Mod,V)).

peval(C,_,C) :- simple(C).
peval(Min..Max,_,V) :- eval_range(Min,Max,V).
peval([X|Z],Mod,V) :- 	random(0,101,R), walk([X|Z],Mod,R,[],V).
peval(:Pred,Mod,V) :-
	arg(1,Pred,X),
	findall(X,Mod:Pred,Xs), peval(Xs,Mod,V).
peval(F1+F2,Mod,V) :-
	gen_list(F1+F2,Mod,Lm),
	peval(Lm,Mod,V).
peval(bag(LenExpr,ListExpr),Mod,V) :-
	eval_length(LenExpr,Mod,Len),
	(expand_expr(set,Len,ListExpr,Mod,List)->
	    random_bounded_subset(Len,Mod,List,false,V);
	    eval_bag(Len,ListExpr,Mod,V)).
peval(set(LenExpr,ListExpr),Mod,V) :-
	eval_length(LenExpr,Mod,Len),
	(expand_expr(bag,Len,ListExpr,Mod,List)->
	    random_bounded_subset(Len,Mod,List,true,V);
	    eval_set(Len,ListExpr,Mod,V)).
peval(kit(LenExpr,ListExpr),Mod,V) :-
	eval_length(LenExpr,Mod,Len),
	(expand_expr(kit,Len,ListExpr,Mod,List)->
	    true;
	    throw(impractical_expr(expr_expansgion_too_large_use_set_instead))),
	length(List,DomainLen),
	KitLen is min(Len,DomainLen),
	KitLen > 0,
	random_bounded_subset(KitLen,Mod,List,true,V).
peval(arg(S),Mod,V) :-
	functor(S,_,A),
	A1 is A + 1,
	random(1,A1,R),
	arg(R,S,V1),
	peval(V1,Mod,V).

% prepare_percent(+Expr,?PreparedExpr)
% PreparedExpr is Expr or an optimized form of it that may
% later be passed to percent/2, e.g.:
%
%   prepare_percent([a:20,b],Prep), percent(V,Prep).
%
% This is only for effeciency, as compared to just calling percent/2 directly.
% The above example is only for illustration and itself would not be more efficient
% than just calling percent/2 directly. Only where Prep may be reused across multiple
% calls to percent, this *may* be more efficient.
%
prepare_percent(Mod:Expr,V) :- once(prep(Expr,Mod,V)).

prep(:Pred,Mod,V) :-
	arg(1,Pred,X),
	findall(X,Mod:Pred,V).
prep(F1+F2,Mod,V) :-
	gen_list(F1+F2,Mod,V).
prep([X|R],_Mod,arg(V)) :-
	balance([X|R],L),
	V =.. [x|L].
prep(bag(Lx,Ex),Mod,bag(Ly,Ey)) :-
    prep(Lx,Mod,Ly),
    prep(Ex,Mod,Ey).
prep(set(Lx,Ex),Mod,set(Ly,Ey)) :-
    prep(Lx,Mod,Ly),
    prep(Ex,Mod,Ey).
prep(kit(Lx,Ex),Mod,kit(Ly,Ey)) :-
    prep(Lx,Mod,Ly),
    prep(Ex,Mod,Ey).
prep(T,_,T).



% balance(+WeightedList,?BalancedList)
% WeightedList contains elements of the form X:P,
% and BalancedList is a list of all the Xs,
% each duplicated by P. The final result is
% minimized by the GCD of all the counts.
balance(L,B) :-
	enumerate(L,L1),
	(gcd(L1,_,L2)->
	    true;
	    L1 = L2),
	expand(L2,B).


enumerate(X,Y) :- enumerate(X,0,_,0,Y).
enumerate([],Total,N,ImplicitCount,[]) :-
	(ImplicitCount =:= 0->
	    N=0;
	    N is (100-Total) // ImplicitCount).
enumerate([X|R],Total,N,ImplicitCount,[Y:P|Z]) :-
	(X = Y:P->
	    (T1 is Total+P,
		I1 = ImplicitCount);
	    (T1 = Total,
		I1 is ImplicitCount + 1,
		X = Y,
		P = N)),
	enumerate(R,T1,N,I1,Z).

gcd([],_,[]).
gcd([X:Px|R],GCD,[X:Dx|Z]) :-
	(R = [_:Py|_]->
	    (Px<Py->(Lesser=Px,Greater=Py);(Lesser=Py,Greater=Px));
	    (Lesser=Px,Greater=Px)),
	(nonvar(GCD)->
	    true;
	    (between(2,Lesser,N),
		GCD is (Lesser+2-N))),
	Lesser rem GCD =:= 0,
	Greater rem GCD =:= 0,
	gcd(R,GCD,Z),
	Dx is Px // GCD.

expand([],[]).
expand([X:P|R],Rez) :-
	(P > 0 ->
	    (Rez = [X|Z],
		P1 is P -1,
		expand([X:P1|R],Z));
	    (Rez = Z,
		expand(R,Z))).


% eval_range(+Min,+Max,?V)
% Generate a random value between Min and Max inclusive.
% Min may also include intermediate functions (e.g. avg/1)
% and the values may be dates.
eval_range(Min,Max,V) :-
	redate_range_pair(Min,Max,Min1,Max1),
	simple(Min1),
	prandom(Min1,Max1,V).
eval_range(Min..avg(Avg),Max,V) :-
	redate_range_pair(Min,Max,Min1,Max1),
	redate_intermediate(Avg,Avg1),
	random_mean(Min1,Avg1,Max1,V).
eval_range(Min..step(Step),Max,V) :-
	redate_range_pair(Min,Max,Min1,Max1),
	redate_intermediate(Step,Step1),	
	prandom(Min1,Max1,V1),
	V is (V1 - ((Min+V1) mod Step1)).

redate(Date1,Date2) :- (idate(Date1,Date2)->true; Date1=Date2).

redate_range_pair(Min,Max,Min1,Max1) :-
	redate(Min,Min1),
	(date_plus(Min1,Max,Max1)->
	    true;
	    redate(Max,Max1)).

redate_intermediate(V,R) :-
	(utils:date_operand_to_seconds(V,R)->
	    true;
	    V=R).


eval_length(Expr,Mod,Len) :-
	peval(Expr,Mod,Len),
	(integer(Len)->true;throw(expr_error(list_length_not_integer,expr=Expr,evaluated_as=Len))).

% expand_expr(+Kind,+Length,+Expr,+Mod,?Expanded)
% Expanded is an enumeration of all items represented by Expr. Succeeds only
% in cases where it is estimated to be a performance benefit, and the
% result is not too large.
%
expand_expr(_,_,:Pred,Mod,L) :-
	arg(1,Pred,X),
	findall(X,Mod:Pred,L).
expand_expr(Kind,Len,WeightedList,_Mod,L) :-
	(member(Kind,[kit,set])->
	    true;
	    Len > 5),
	balance(WeightedList,L).
expand_expr(set,Len,Min..Max,_,L) :-
	((Max - Min) - Len) < 5,
	(for(X,Min,Max), foreach(X,L) do true).

random_bounded_subset(0,_Mod,_List,_Reduce,[]).
random_bounded_subset(N,Mod,List,Reduce,[X|Z]) :-
	N > 0,
	(Reduce->
	    random_select(X,List,Rem);
	    (random_member(X,List), Rem=List)),
	N1 is N - 1,
	random_bounded_subset(N1,Mod,Rem,Reduce,Z).

eval_bag(0,_Expr,_Mod,[]).
eval_bag(N,Expr,Mod,[X|Z]) :-
	peval(Expr,Mod,X),
	N1 is N-1,
	eval_bag(N1,Expr,Mod,Z).

eval_set(0,_Expr,_Mod,[]).
eval_set(N,Expr,Mod,[X|Z]) :-
	N > 0,
	N1 is N-1,
	eval_set(N1,Expr,Mod,Z),
	MaxAttempts = 100,
	between(1,100,Try),
	(Try>=MaxAttempts->throw(impractical_expr(set(N,Expr)));true),
	peval(Expr,Mod,X),
	\+member(X,Z),
	!.

% walk(+WeightedList,+Module,+RemainingPercent,+AccumulatedUnweighted,?Result)
% Result is randomly generated from WeightedList or AccumulatedUnweighted.
% The latter as well as RemainingPercent are calculated as the list is
% processed.
%
walk([],Mod,_,Implicit,V) :-
	random_member(Vexpr,Implicit),
	peval(Vexpr,Mod,V).
walk([X|Z],Mod,R,Implicit,V) :-
	(X=X1:P->
	    (R =< P ->
		V = X1;
		(R1 is R - P),
		walk(Z,Mod,R1,Implicit,V));
	    walk(Z,Mod,R,[X|Implicit],V)).

gen_list(L,_Mod,L) :- is_list(L).
gen_list(ListExpr,Mod,L) :-
	list_expr(ListExpr),
	peval(ListExpr,Mod,L).
gen_list(F1+F2,Mod,Lm) :-
	gen_list(F1,Mod,L1),
	gen_list(F2,Mod,L2),
	merge_qualified_lists(L1,L2,Lm).
gen_list(:Pred,Mod,L) :-
	arg(1,Pred,X),
	findall(X,Mod:Pred,L).

list_expr(set(_,_)).
list_expr(bag(_,_)).
list_expr(kit(_,_)).

merge_qualified_lists([],L,L).
merge_qualified_lists([H1|Hz],L,Z) :-
	(merge_entry(L,H1,L1)->
	    true;
	    L1 = [H1|L]),
	merge_qualified_lists(Hz,L1,Z).

merge_entry([X|Z],X,[X|Z]).
merge_entry([X:P|Z],X,[X:P|Z]).
merge_entry([X|Z],X:P,[X:P|Z]).
merge_entry([X:P1|_],X:P2,_) :-
	P1 \== P2,
	throw(expr_error(percentage_merge_conflict(P1,P2))).



%step_random_mean(Low,Mean,High,Step,R) :-
%	random_mean(Low,Mean,High,V1),
%	step(V1,Step,R).

random_mean(Low,Mean,High,R) :-
	Split is 1 - (Mean - Low) / (High - Low),
	(maybe(Split)->prandom(Low,Mean,R);prandom(Mean,High,R)).

%random_pct(Pct,R) :-
%	Split is 1 - Pct / 100,
%	(maybe(Split)->random(0,Pct,R);random(Pct,101,R)).

%step_prandom(Min,Max,Step,Var) :-
%	prandom(Min,Max,V1),
%	step(V1,Step,Var).

step(V,Step,V1) :- V1 is Step * (V // Step).

prandom(Low,High,V) :- H1 is High+1, random(Low,H1,V).


%%%
%%% Term expansion
%%%

% TBD should probably still findall & merge nested probabilities. Otherwise this only applies to the
% top-level goal, which can still succeed on invocations rather than merge them together. Thus below
% the probability of 'a' is just 4% : (.2 * .2) = .04.
%
%   20 foo(a).
%   80 foo(b).
%   20 bar(X) :- foo(X).
%   80 bar(c).


expand_generator_term(((Pct pct Term) :- Body), M, [TrackTerm,(ExpandedTerm :- Body)]) :- !, expand_pct_term(Pct,Term,M,TrackTerm,ExpandedTerm).
expand_generator_term((Pct pct Term), M, [TrackTerm,ExpandedTerm]) :-
	expand_pct_term(Pct,Term,M,TrackTerm,ExpandedTerm).
expand_generator_term(end_of_file, M, Rez) :-
	findall(Pred,expand_generator_at_eof(M,Pred),Preds),
	append(Preds,[end_of_file],Rez).

expand_generator_at_eof(M,(Fh :- (percent(ActualIndex,Pct_f), Fb))) :-
	current_predicate(M:'track_pct_expand$$'/6),
	bagof((Index:Pct),FirstArg^(M:'track_pct_expand$$'(F,A,FirstArg,Fr,Index,Pct)),Ixs),
	prepare_percent(Ixs,Pct_f),
	length(HeadArgs,A),
	Fh =.. [F|HeadArgs],
	% Special case for ::/2 rules, which require at least one side to be bound
	% in order to know what the target predicate is. Since it would be problematic
	% for other reasons to allow head variations for clauses of the same predicate,
	% we'll assume that they all match and therefore just work with the first one.
	(F = '::' ->
	    (once(M:'track_pct_expand$$'(F,A,FirstArg,_Fr,_Index,_Pct)),
		arg(1,Fh,FirstArg)) ;
	    true),
	Fb =.. [Fr,ActualIndex|HeadArgs].

expand_pct_term(Pct,Term,M,TrackTerm,ExpandedTerm) :-
	functor(Term,Term_functor,Term_arity),
	((current_predicate(M:'track_pct_expand$$'/6),
	    findall(Index,M:'track_pct_expand$$'(Term_functor,Term_arity,_FirstArg,_,Index,_),Indexes))->
	    length(Indexes,Len);
	    Len = 0),
	Index is Len + 1,
	atom_concat('$_pct_',Term_functor,Fr),
	Term =.. [_|Args],
	ExpandedTerm =.. [Fr,Index|Args],
	(Term_arity=0->FirstArg=[];arg(1,Term,FirstArg)),
	TrackTerm =.. ['track_pct_expand$$',Term_functor,Term_arity,FirstArg,Fr,Index,Pct].

:- multifile user:term_expansion/6.
user:term_expansion(Term1, Layout, Ids, Term2, Layout, [pct_token|Ids]) :-
	nonmember(pct_token, Ids),
	(prolog_load_context(module,Module)->true;Module=user),
	(current_predicate(expand_generator_term/3)-> % avoid strange error msg when loading this module
	    expand_generator_term(Term1,Module,Term2),
	    true).


%%%
%%% Unit test
%%%
:- begin_tests(percent).

rg(From,To,V) :- V >= From, V =< To.

% ?? Spider generates an error for this next line, so commented out.
% The second arg is meta-predicate, but if (X=5, percent(A,X)) works why shouldn't this?
% Anyway, it's not really a useful case so it doesn't much matter -bpm2Jun13
%test(basic,[true(Rez=5)]) :- percent(Rez,5).
test(basic,[true(Rez=a)]) :- percent(Rez,a).
test(basic,[true(Rez=a)]) :- percent(Rez,[a]).

test(list,[true(member(Rez,[a]))]) :- percent(Rez,[a]).
test(list,[true(member(Rez,[a,b]))]) :- percent(Rez,[a,b]).
test(list,[true(member(Rez,[a,b,c,d,e,f]))]) :- percent(Rez,[a,b,c,d,e,f]).
test(list,[true(member(Rez,[a]))]) :- percent(Rez,[a:100]).
test(list,[true(member(Rez,[a,b]))]) :- percent(Rez,[a:50,b:50]).
test(list,[true(member(Rez,[a,b,c,d,e,f]))]) :- percent(Rez,[a:40,b:20,c:30,d,e,f]).
test(list,[true((Rez1=a;Rez2=b))]) :- percent(Rez1,[a:99,z]), percent(Rez2,[b:99,z]).
test(basic,[true(Rez=a)]) :- percent(Rez,[a:100]).

test(range,[true(rg(1,5,Rez))]) :- percent(Rez,1..4).
test(range,[true(rg(1,5,Rez))]) :- percent(Rez,[1..4]).
test(range,[true(rg(0,9,Rez))]) :- percent(Rez,1..avg(8)..9).
test(range,[true(member(Rez,[1,3,5]))]) :- percent(Rez,1..step(2)..5).
test(range,[true(member(Rez,[1,3,4,5]))]) :- percent(Rez,[1,3..5]).

test(date_range,true(member(Rez,[1-feb-2011,2-feb-2011]))) :-
	percent(Rez1,[1-feb-2011..2-feb-2011]),
	idate(Rez,Rez1).
test(date_range,true(member(Rez,[1-feb-2011,2-feb-2011]))) :-
	percent(Rez1,[1-feb-2011..days(2)]),
	idate(Rez,Rez1).

test(list_fn,[true(maplist(rg(1,5),Rez)),true(length(Rez,3))]) :- percent(Rez,bag(3,1..5)).
test(list_fn,[true(maplist(rg(1,5),Rez))]) :- percent(Rez,bag(3,[4,3,2])).
test(list_fn,[exception(expr_error(list_length_not_integer,_,_))]) :- percent(_,bag(a,1..5)).

test(bag,true(X=[a,a])) :- percent(X,bag(2,[a])).
test(set,[exception(impractical_expr(_))]) :- percent(_,set(2,[a])).
test(kit,true(X=[a])) :- percent(X,kit(1,[a])).
test(kit,true(X=[a])) :- percent(X,kit(2,[a])).

test(pred,true(member(Rez,[a,b,c]))) :- percent(Rez,:foo(_)).
test(pred,true(member(Rez,[[a],[b],[c],[a,b],[a,c],[b,c],[a,a],[b,b],[c,c],[b,a],[c,a],[c,b]]))) :- percent(Rez,bag([1,2],:foo(_))).
test(pred,true(member(Rez,[[a],[b],[c],[a,b],[a,c],[b,a],[b,c],[c,a],[c,b]]))) :- percent(Rez,set([1,2],:foo(_))).

test(merge,true(member(Rez,[a,b,c,d]))) :- percent(Rez,[d]+ :foo(_)).
test(merge,true(member(Rez,[a,b,c,d]))) :- percent(Rez,[a,d]+ :foo(_)).
test(merge,true(member(Rez,[a,b,c,d]))) :- percent(Rez,[a:80,d]+ :foo(_)).

test(arg,true(member(Rez,[a,b,c,d]))) :- percent(Rez,arg(x(a,b,c,d))).

test(prepare,true(member(Rez,[a,b,c]))) :-
	prepare_percent([a:80,b,c],Prep),
	percent(Rez,Prep).
test(prepare,true((length(Rez,Len),Len > 0, Len =< 4))) :-
	prepare_percent(set([1:85,2:10,3,4],0..999),Prep),
	percent(Rez,Prep).

foo(a).
foo(b).
foo(c).


:- end_tests(percent).

