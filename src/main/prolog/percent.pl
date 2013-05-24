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
:- module(percent,[percent/2, percent/3]).

:- use_module(library(random)).
:- use_module(library(lists)).
:- use_module(library(between)).
:- use_module(utils).
:- load_files(library(plunit),[if(changed),load_type(source)]).

:- op(520,yfx,'..').  % a little higher than '-'


% TBD nesting user/1, sys/1 constructs to handle nested structs
% TBD percent(Accounts,fn(length([1:85,2:10,3,4]),+percent(_,[AccountStart..AccountEnd])),Accounts_f),
% TBD provide option for nested tracked
% TBD fast non-compiled
% TBD comma-separate predicates



% percent(?VarToBind,+BindingExpr).
% percent(?VarToBind,+BindingExpr,-Generator).
%
percent(Var,Expr) :-
	percent(Var,Expr,Gen),
	Gen.
percent(Var,Expr,percent:Gen) :-
	(var(Expr)->throw('IllegalUnboundArgumentException'(arg=2,percent(Var,Expr,Gen)));true),
	default_context(Context),
	rewrite(Expr,Context,Var,Gen).

default_context(context(Min,linear,1,Max)) :-
	current_prolog_flag(min_tagged_integer,Min),
	current_prolog_flag(max_tagged_integer,Max).

rewrite(Expr,exec(Var,Gen)) :- rewrite(Expr,Var,Gen).
rewrite(Expr,Var,Gen) :-
	default_context(Context),
	rewrite(Expr,Context,Var,Gen).
rewrite(A,_,Var,Var=A) :- atomic(A).
rewrite(Expr,Context,Var,Gen) :-
	refn(Expr,Context,C1),
	arith(C1,Var,Gen).
rewrite(fn(Fn,Sub),Context,Var,Gen) :- fn_rewrite(Fn,Sub,Context,Var,Gen).
rewrite((X+D):P,Context,Var,Gen) :- rewrite(X+(D:P),Context,Var,Gen).
rewrite((+D):P,Context,Var,Gen) :- rewrite(+(D:P),Context,Var,Gen).
rewrite(+Pred,Context,Var,Gen) :- rewrite([]+Pred,Context,Var,Gen).
rewrite([D|Ds],Context,Var,Gen) :- rewrite([D|Ds]+true,Context,Var,Gen).
rewrite(Dist+Pred,Context,Var,eval(Gen,Var)) :-
	once(bind_pred(Pred,Pred1,Var)),
	(Pred=true->Vs=[];(findall(Var,Pred1,Vs))),
	(Pred=percent(Var,Spec)->    % Make a nested percent a lazy eval
	    percent(Var,Spec,percent:eval(Gen,Var));
	    rewire(Dist,Vs,Context,Var,Gen)
	).

% TBD +(x,y) does not work; maybe drop '+' prefix?


bind_pred(M:Pred,M:Pred1,Var) :-  bind_pred(Pred,Pred1,Var).
bind_pred(Pred,Pred1,Var) :- atomic(Pred), Pred1 =.. [Pred,Var].
bind_pred(Pred,Pred,Var) :- arg(1,Pred,Var).

refn(min(Min),context(_Min,Dist,Step,Max),context(Min1,Dist,Step,Max)) :- redate(Min,Min1).
refn(max(Max),context(Min,Dist,Step,_Max),context(Min,Dist,Step,Max1)) :- redate(Max,Max1).
refn(avg(Avg),context(Min,_Dist,Step,Max),context(Min,avg(Avg1),Step,Max)) :- redate(Avg,Avg1).
refn(step(Step),context(Min,Dist,_Step,Max),context(Min,Dist,Step,Max)).
refn(Min..Max,context(_Min,Dist,Step,_Max),context(Min1,Dist,Step,Max1)) :-
	redate(Min,Min1),
	(Max=days(Days)->  % TBD support other time units besides days
	    date_plus(Min1,days(Days),Max1);
	    redate(Max,Max1)).

redate(Date1,Date2) :- (idate(Date1,Date2)->true; Date1=Date2).


fn_rewrite(Fn,Sub,Context,Var,Gen) :-
	refn(Fn,Context,C1),
	rewrite(Sub,C1,Var,Gen).
fn_rewrite(length(Len),Sub,Context,Var,multi(Rlen,OneVar,Var,Gen)) :-
	cxwrite(Len,Rlen),
	rewrite(Sub,Context,OneVar,Gen).


% catch context-embedded terms with arity>0 here
%
cxwrite(avg(A),Rez) :- !, rewrite(A,Var,Gen), once(ctxr(Var,avg(Var),Gen,Rez)).
cxwrite(X,Y) :- rewrite(X,Y).

ctxr(N,WrapperOut,N=N,exec(X,X=WrapperOut)).
ctxr(_Var,WrapperOut,Gen,exec(Var,(Gen,Var=WrapperOut))).

simple(exec(X,X=Y),Y).


arith(context(Min,Dist,Step,Max),Var,Gen) :-
	cxwrite(Min,Rmin),
	cxwrite(Dist,Rdist),
	cxwrite(Step,Rstep),
	cxwrite(Max,Rmax),
	((simple(Rmin,Min1), simple(Rmax,Max1), simple(Rstep,Step1), simple(Rdist,Dist1))->
	    dexpr(Min1,Dist1,Step1,Max1,Var,Gen);
	    Gen=eval(Rmin,Rdist,Rstep,Rmax,Var)).

dexpr(Min,linear,1,Max,Var,prandom(Min,Max,Var)).
dexpr(Min,avg(Avg),1,Max,Var,random_mean(Min,Avg,Max,Var)).
dexpr(Min,linear,Step,Max,Var,step_prandom(Min,Max,Step,Var)) :- Step \== 1.
dexpr(Min,avg(Avg),Step,Max,Var,step_random_mean(Min,Avg,Max,Step,Var)) :- Step \== 1.

eval(exec(V,P),V) :- P.
eval(dist(Type,Len,Pct,Index),V) :- eval_dist(Type,Len,Pct,Index,V).
eval(Min,Dist,Step,Max,Var) :-
	eval(Min,VMin),
	eval(Dist,VDist),
	eval(Step,VStep),
	eval(Max,VMax),
	dexpr(VMin,VDist,VStep,VMax,Var,Gen),
	Gen.

% multi(+LenGenerator,-Var,?Values,+ValueGenerator)
% Values is generated by calling ValuGenerator Len times, where
% Len is generated from LenGenerator. Only unique values are
% generated, based on the presumption that the desired Values
% are a subset of all possible values that can be generated and
% regenerating a value is not desirable.
%
multi(Len,Var,Vs,Gen) :-
	eval(Len,VLen),
	find_this_many(VLen,Var,Gen,[],Vs).

find_this_many(N,_Var,_Gen,_,[]) :- N =< 0.
find_this_many(Len,Var,Gen,SoFar,[Var1|Vs]) :-
	Len > 0,
	copy_term((Var,Gen),(Var1,Gen1)),
	(repeat(1000),
	    Gen1,
	    nonmember(Var1,SoFar),
	    !
	    ),
	Len1 is Len - 1,
	find_this_many(Len1,Var,Gen,[Var1|SoFar],Vs).

step_random_mean(Low,Mean,High,Step,R) :-
	random_mean(Low,Mean,High,V1),
	step(V1,Step,R).

random_mean(Low,Mean,High,R) :-
	Split is 1 - (Mean - Low) / (High - Low),
	(maybe(Split)->prandom(Low,Mean,R);prandom(Mean,High,R)).

random_pct(Pct,R) :-
	Split is 1 - Pct / 100,
	(maybe(Split)->random(0,Pct,R);random(Pct,101,R)).

step_prandom(Min,Max,Step,Var) :-
	prandom(Min,Max,V1),
	step(V1,Step,Var).

step(V,Step,V1) :- V1 is Step * (V // Step).

prandom(Low,High,V) :- H1 is High+1, random(Low,H1,V).

r(Call,Times,R) :-
	between(1,Times,_),
	Call,
	write(R), nl.

x(Call,R,Times,Rez) :- findall(R,r(Call,Times,R),Rs), sumlist(Rs,Sum), Rez is Sum / Times.



rewire(Spec,Base,Context,Var,Dist) :-
	splitwrap(Spec,ExplicitSpec,ExplicitTotal,Context,Var,ImplicitSpec),  % wraps ExplicitSpec

	shrink(Base,ImplicitSpec,Base1),
	shrink(Base1,ExplicitSpec,Base2),
	append(ImplicitSpec,Base2,CombinedBase),

	ImplicitSlots is 100-ExplicitTotal,

	(ExplicitSpec=[]->
	    struct(CombinedBase,Dist1);
	    (CombinedBase=[]->
		(eq_dist(ExplicitSpec,_,EqSpec)->
		    struct(EqSpec,Dist1);
		    (scale(ExplicitSpec,Scaled), struct(Scaled,Dist1)));
		emerge(ExplicitSpec,CombinedBase,ExplicitTotal,ImplicitSlots,Context,Var,Dist1))),
	once(simplify(Dist1,Dist)).

simplify(listmixed,2,Total,[X:Xp,Y:_],exec(V,(maybe(Prob)->eval(X,V);eval(Y,V)))) :- Prob is Xp/Total.
simplify(X,X).

emerge(Explicit,Base,ExplicitTotal,ImplicitSlots,Context,Var,Dist) :-
	length(Base,BaseLen),
	Per is ImplicitSlots // BaseLen,
	((Per > 1,ImplicitSlots rem BaseLen =:= 0)->Strategy=merge;Strategy=split),
	emerge1(Strategy,Explicit,Base,Per,ExplicitTotal,ImplicitSlots,Context,Var,Dist).

emerge1(merge,Explicit,Base,Per,_ExplicitTotal,_ImplicitSlots,Context,Var,Dist) :-
	pwrap(Base,Per,PerBase),
	append(Explicit,PerBase,All),
	rewire(All,[],Context,Var,Dist).
emerge1(split,Explicit,Base,_Per,ExplicitTotal,ImplicitSlots,_Context,_Var,dist(listmixed,2,Total,[Dist1:ExplicitTotal,Dist2:ImplicitSlots])) :-
	Total is ExplicitTotal + ImplicitSlots,
	struct(Explicit,Dist1),
	struct(Base,Dist2).
		
pwrap([],_,[]).
pwrap([X|R],P,[X:P|Z]) :- pwrap(R,P,Z).
	

splitwrap([],[],0,_,_,[]).
splitwrap([X:P|R],[Xw:P|Y],Total,Context,Var,Z) :-
	wrap(X,Context,Var,Xw),
	splitwrap(R,Y,T1,Context,Var,Z),
	Total is T1 + P.
splitwrap([X|R],Y,Total,Context,Var,[Xw|Z]) :-
	\+ functor(X,':',2),
	wrap(X,Context,Var,Xw),
	splitwrap(R,Y,Total,Context,Var,Z).

wrap_NOPE(X,Context,Var,Y) :-
	(functor(X,'-',2)->  % TBD
	    idate(X,Y);
	    (is_list(X)->
		(rewrite(X,Context,Var,Gen),
		    Y=pct(Var,Gen));
		X=Y)).

wrap_ORG(X,Context,Var,Y) :-
	((atomic(X);functor(X,pct,_))->
	    X=Y;
	    ((functor(X,'-',2)->
	      idate(X,Y);
	      (rewrite(X,Context,Var,Gen),
		  Y=pct(Var,Gen))))).

wrap(X,Context,Var,Y) :-
	((atomic(X);functor(X,pct,_))->
	    X=Y;
	    ((functor(X,'-',2)->
	      idate(X,Y);
	      % TBD, percent built-in forms conflict with user-structured
	      % elements -- didn't consider that when I wrote it! Here,
	      % try the built-in forms first (if there is a conflict then
	      % the unexpected might happen -bpm 28-Mar-13
	      ((rewrite(X,Context,Var,Gen)->
		Y=pct(Var,Gen);
		X=Y))))).


shrink([],_List,[]).
shrink([X|R],List,Z) :-
	(memberchk(X,List);memberchk(X:_,List)->Z=S1;Z=[X|S1]),
	shrink(R,List,S1).

struct(List,dist(Type,Len,Total,Z)) :-
	enumerate(List,Expanded),
	length(Expanded,Len),
	once(xform(Expanded,Len,Type,Total,Z)).

sum_dist([],0).
sum_dist([X|Y],Z) :- (X=_:P->(sum_dist(Y,Z1),Z is Z1+P);Z=100).

scale(List,Scaled) :- eq_dist(List,_,Scaled), !.
scale(List,Scaled) :- gcd(List,_,Scaled), !.
scale(List,List).

eq_dist([],_,[]).
eq_dist([X:P|R],P,[X|Z]) :- eq_dist(R,P,Z).

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

enumerate(X,Y) :- enumerate(X,1,Y).
enumerate([],_,[]).
enumerate([X:P|Y],M,[X|Z]) :- P > 0, P1 is P-1, enumerate([X:P1|Y],M,Z).
enumerate([_:P|Y],M,Z) :- P =< 0, enumerate(Y,M,Z).
enumerate([X|Y],M,Z) :- \+functor(X,':',2), enumerate([X:M|Y],M,Z).


xform(List,Len,Type,100,X) :-
	current_prolog_flag(max_arity,MaxArity),
	(Len =< MaxArity->
	    (X =.. [x|List], Type = structfixed);
	    (X = List, Type = listmixed)).
xform(List,_Len,mixu,Total,List) :-   %TBD
	sum_dist(List,Total).

% eval_dist(Type,Len,Pct,Index,V).
eval_dist(structfixed,Len,_Pct,Index,V) :-
	prandom(1,Len,ArgIndex),
	arg(ArgIndex,Index,V1),
	eval_dist_element(V1,V).
eval_dist(listfixed,Len,_Pct,Index,V) :-
	prandom(1,Len,ArgIndex),
	nth1(ArgIndex,Index,V1),
	eval_dist_element(V1,V).
eval_dist(listmixed,_Len,Pct,Index,V) :-
	random(0,Pct,ArgIndex),
	eval_fit(Index,ArgIndex,V1),
	eval_dist_element(V1,V).

eval_dist_element(pct(V,Gen),V) :- !, Gen.
eval_dist_element(dist(Type,Len,Pct,Index),V) :- !, eval_dist(Type,Len,Pct,Index,V).
eval_dist_element(X,X).

eval_fit([X:P|R],N,V) :- (N =< P -> X=V ; (N1 is N-P, eval_fit(R,N1,V))).

tp([a],[],dist(structfixed,1,100,x(a))).
tp([],[a],dist(structfixed,1,100,x(a))).
tp([a],[a],dist(structfixed,1,100,x(a))).
tp([a:100],[],dist(structfixed,1,100,x(a))).
tp([a:100],[a],dist(structfixed,1,100,x(a))).

tp([a,b],[],dist(structfixed,2,100,x(a,b))).
tp([],[a,b],dist(structfixed,2,100,x(a,b))).
tp([a,b],[a,b],dist(structfixed,2,100,x(a,b))).
tp([a:50,b],[],dist(structfixed,2,100,x(a,b))).
tp([a,b:50],[],dist(structfixed,2,100,x(b,a))).
tp([a:50,b],[a,b],dist(structfixed,2,100,x(a,b))).
tp([a,b:50],[a,b],dist(structfixed,2,100,x(b,a))).
tp([a:50,b:50],[a,b],dist(structfixed,2,100,x(a,b))).

tp([a:50,b:40],[],dist(structfixed,9,100,x(a,a,a,a,a,b,b,b,b))).
tp([a:50,b:40],[a,b],dist(structfixed,9,100,x(a,a,a,a,a,b,b,b,b))).

tp([a:80],[a,b],dist(structfixed,5,100,x(a,a,a,a,b))).
tp([a:80],[b,a],dist(structfixed,5,100,x(a,a,a,a,b))).
tp([a:80,b],[a,b],dist(structfixed,5,100,x(a,a,a,a,b))).

tp([a:40,c:40],[a,b,c],dist(structfixed,5,100,x(a,a,c,c,b))).
tp([a:25,b:25],[a,b,c,d],dist(structfixed,4,100,x(a,b,c,d))).

tp([a:13,b:7,c:11],[a,b,c],dist(structfixed,31,100,x(a,a,a,a,a,a,a,a,a,a,a,a,a,b,b,b,b,b,b,b,c,c,c,c,c,c,c,c,c,c,c))).
tp([a:51,b],[],dist(structfixed,100,100,
		    x(a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,
		      b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b))).

tp([5..6],[],dist(structfixed,1,100,x(pct(_,_)))).
tp([5..6:80,d],[],dist(structfixed,5,100,x(pct(V,prandom(5,6,V)),pct(V,prandom(5,6,V)),pct(V,prandom(5,6,V)),pct(V,prandom(5,6,V)),d))).

test_rewire(Spec,Dist,Rez) :-
	default_context(Context),
	rewire(Spec,Dist,Context,_,Rez).

go :- tp(Spec,Dist,Exp),
	(test_rewire(Spec,Dist,Got)->true;(format("FAILURE, (~p,~p) FAILED~n",[Spec,Dist]),!,fail)),
	(Exp=Got->format("SUCCESS: (~p,~p) ==> ~p~n",[Spec,Dist,Got]);(format("FAILURE, (~p,~p)~n expected: ~p~n      got: ~p~n",[Spec,Dist,Exp,Got]),!,fail)),
	fail.
go.




:- begin_tests(percent).

pev(Spec,Spec,F) :- percent(_,Spec,Fx), Fx = percent:eval(F,_).
pem(Spec,Spec,F) :- percent(_,Spec,Fx), Fx = percent:F.


test(L,[true(Rez=dist(structfixed,1,100,x(a)))]) :- pev([a],L,Rez).
test(L,[true(Rez=dist(structfixed,1,100,x(a)))]) :- pev([a:100],L,Rez).
test(L,[true(Rez=dist(structfixed,2,100,x(a,b)))]) :- pev([a,b],L,Rez).
test(L,[true(Rez=dist(structfixed,2,100,x(a,b)))]) :- pev([a:50,b],L,Rez).
test(L,[true(Rez=dist(structfixed,2,100,x(b,a)))]) :- pev([a,b:50],L,Rez).
test(L,[true(Rez=dist(structfixed,2,100,x(a,b)))]) :- pev([a:50,b:50],L,Rez).
test(L,[true(Rez=dist(structfixed,9,100,x(a,a,a,a,a,b,b,b,b)))]) :- pev([a:50,b:40],L,Rez).
test(L,[true(Rez=dist(structfixed,5,100,x(a,a,a,a,b)))]) :- pev([a:80,b],L,Rez).
test(L,[true(Rez=dist(structfixed,5,100,x(a,a,b,b,c)))]) :- pev([a:40,b:40,c],L,Rez).
test(L,[true(Rez=dist(structfixed,31,100,x(a,a,a,a,a,a,a,a,a,a,a,a,a,b,b,b,b,b,b,b,c,c,c,c,c,c,c,c,c,c,c)))]) :- pev([a:13,b:7,c:11],L,Rez).
test(L,[true(Rez=
          dist(structfixed,100,100,
		    x(a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,
		      b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b)))]) :- pev([a:51,b],L,Rez).

test(L,[true(Rez=dist(structfixed,1,100,x(pct(_,_))))]) :- pev([5..6],L,Rez).
test(L,[true(Rez=dist(structfixed,5,100,x(pct(V,prandom(5,6,V)),pct(V,prandom(5,6,V)),pct(V,prandom(5,6,V)),pct(V,prandom(5,6,V)),d)))]) :- pev([5..6:80,d],L,Rez).


test(L,[true(Rez=Exp)]) :-
	Exp=multi(exec(_A,eval(dist(structfixed,2,100,x(1,2)),_B)),_C,_X,eval(dist(structfixed,2,100,x(a,b)),_D)),
	pem(fn(length([1,2]),+(member(_,[a,b]))),L,Rez).
test(L,[true(Rez=Exp)]) :-
	Exp=multi(exec(_A,eval(dist(structfixed,2,100,x(1,2)),_B)),_C,_X,eval(dist(structfixed,2,100,x(a,b)),_D)),
	pem(fn(length([1,2]),+percent(_,[a,b])),L,Rez).
%test(L,[true(Rez=Exp)]) :-
%	Exp=multi(exec(_A,eval(dist(structfixed,2,100,x(1,2)),_A)),_B,X,eval(dist(structfixed,2,100,x(a,b)),_B)),
%	pev(fn(length([1,2]),+percent(_,[500..600])),L,Rez).
%test(L,[true(Rez=blue)]) :- pev(fn(length([1:80,2:15,3,4,5]),+percent(_,[500..600])),L,Rez).


:- end_tests(percent).

