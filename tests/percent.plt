
:- use_module(percent).
:- use_module(utils).
:- use_module(library(plunit)).

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
