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
:- module(evgen,[gen/1,store/2,buffer/1,csort/2,follow/3,seq/1,seq/2,seq/3,stabilize/3,filter/4,first/2,
		 findone/1, findone/2,tracker/3,track/2,tracked/2]).
:- use_module(library(between)).
:- use_module(library(lists)).
:- use_module(library(samsort)).
:- use_module(library(random)).
:- use_module(library(file_systems)).
:- use_module(library(xml)).
:- load_files(library(plunit),[if(changed),load_type(source)]).
:- use_module(percent).
:- use_module(typer).
:- use_module(utils).

% File: evgen.pl
% Author: Brendan McCarhty


:- op(700,xfx,as).
:- op(1050,xfy,'::').
:- op(400,xf,k). % thousand
:- op(400,xf,m). % million
:- op(400,xf,b). % billion


:- meta_predicate findone(2).
:- meta_predicate findone(2,+).

info(S,Args) :- write('[RUGE INFO] '), format(S,Args).

findone(T) :- findone(T,1000).
findone(Mod:T,X) :- between(0,X,I), (I=:=X->(format("Warning: 'findone' attempts exceeded ~p without success: ~p~n",[X,T]), fail);true), Mod:T, !.


:- meta_predicate tracker(+,2,-).
tracker(Max,Mod:Id,tracker(Id,Mod,Max)) :- (Mod:bb_delete(Id,_)->true;true), Mod:bb_put(Id,[]).
tracked(V,tracker(Id,Mod,_Max)) :- Mod:bb_get(Id,L), member(V,L).
track(V,tracker(Id,Mod,Max)) :-
	Mod:bb_get(Id,Current),
	L1=[V|Current],
	shorten(Max,L1,L2),
	Mod:bb_update(Id,_,L2).

shorten(Max,Longer,Shorter) :-
	((Max =< 0;Longer=[])->
	    Shorter=[];
	    (Longer=[L|Ls],
		Shorter=[L|Ss],
		M1 is Max-1,
		shorten(M1,Ls,Ss))).


%head_member(V,L,Max) :- head_member(V,L,0,Max).
%head_member(V,[Head|Tail],Pos,Max) :-
%	Pos < Max,
%	(V=Head->
%	    true;
%	    (P1 is Pos+1, head_member(V,Tail,P1,Max))).

%first_n_members([],_,[],0).
%first_n_members([X|Xs],Max,Y,Len) :-
%	(Max =< 0->
%	    (Y=[],Len=0);
%	    (Y=[X|Ys],M1 is Max-1,first_n_members(Xs,M1,Ys,Lz),Len is Lz+1)).




%%%
%%% Top-leval predicates
%%%
:- meta_predicate gen(2).
:- meta_predicate store(?,2).
:- meta_predicate buffer(2).
:- meta_predicate csort(+,2).
:- meta_predicate follow(+,2,+).
:- meta_predicate filter(+,2,+,2).
:- meta_predicate seq(2).
:- meta_predicate seq(2,+).
:- meta_predicate seq(2,+,+).
:- meta_predicate stabilize(+,+,2).
:- meta_predicate first(+,2).

tell_starting_from(Op) :- current_directory(D), info('"~p" invoked from directory ~p~n',[Op,D]).

run_top(Mod,Expr) :-
	functor(Expr,F,_),
	tell_starting_from(F),
	expr_generator(Expr,Mod,Name,Gen,T,Down),
	current_output(Stream),
	output(Stream,Down,Mod,Name,Gen,T).

gen(Mod:Src) :- run_top(Mod,gen(Src)).

first(N,Mod:Src) :- run_top(Mod,first(N,Src)).

buffer(Mod:Src) :- run_top(Mod,buffer(Src)).

seq(Mod:Src) :- run_top(Mod,seq(Src)).
seq(Mod:Src1,Src2) :- run_top(Mod,seq(Src1,Src2)).
seq(Mod:Src1,Src2,Src3) :- run_top(Mod,seq(Src1,Src2,Src3)).

stabilize(Count,ConflictId,Mod:Src) :- run_top(Mod,stabilize(Count,ConflictId,Src)).

csort(Column,Mod:Src) :- run_top(Mod,csort(Column,Src)).

follow(LookAhead,Mod:Src,Follow) :- run_top(Mod,follow(LookAhead,Src,Follow)).

filter(Kind,Mod:Pred,Len,Mod:Src) :- run_top(Mod,filter(Kind,Mod:Pred,Len,Src)).

store(Dest,Mod:Src) :-
	tell_starting_from('store'),	
	expr_generator(Src,Mod,Name,Gen,T,Down),
	output(Dest,Down,Mod,Name,Gen,T).



% expr_generator(+Expr,+Module,?Name,?Generator,?GeneratedTerm,?ControlArgs)
% Generator can be called repeatedly, binding GeneratedTerm,
% reflecting the intent of Expr. Name is the best guess for
% a name that might be used as file name or module name for
% buffering.
%
expr_generator(Expr,Mod,Name,Gen,T,Down) :-
	catch(expr(Expr,down(pl,-1,[]),Mod,Name,Gen,T,Down),
	      Code,
	      throw('ExprSyntaxError'(Code))).

down(format,down(Fmt,_,_),Fmt).
down(limit,down(_,Limit,_),Limit).
down(peek,down(_,_,Peek),Peek).

expr(Expr,Down,Mod,F,Gen,T,Down2) :-
	(expr1(Expr,Down,Mod,F,Gen,T,Down2)->
	    true;
	    throw('SubExprFailed'(Expr))).

expr1(gen(Rule),Down,Mod,Pred,xgen(Args,Pred,Arity,Down,Mod,T),T,Down) :-
	functor(Rule,Pred,Arity),
	Rule =.. [Pred|Args],
	(defined_rule(Mod,Pred,Arity,_,_,_,_,_,_)->   	% Validate that a rule exists here, before actual execution
	    true;
	    throw('UnknownGeneratorRuleException'(Pred/Arity))).
expr1(seq(S1,S2),Down,Mod,Name,Gen,T,Down2) :- expr1(seq([S1,S2]),Down,Mod,Name,Gen,T,Down2).
expr1(seq(S1,S2,S3),Down,Mod,Name,Gen,T,Down2) :- expr1(seq([S1,S2,S3]),Down,Mod,Name,Gen,T,Down2).
expr1(seq(Seq),Down,Mod,Name,(member(G,Gs),G),T,Down) :-
	(is_list(Seq)->Ss=Seq;Ss=[Seq]),
	plist(Ss,Down,Mod,Name,Gs,T).
expr1(follow(LookAhead,SrcMain,Follow),Down,Mod,Name,xfollow(LookAhead,Mod,Peek1,Peek2,Gm,Tm,Gf,Tf,T),T,Down2) :-
	expr(SrcMain,Down,Mod,Name,Gm,Tm,_),
	Down = down(Fmt,Limit,Peek1),
	Down2 = down(Fmt,Limit,Peek2),
	plist(Follow,Down2,Mod,_Name,Gf,Tf).
expr1(stabilize(Count,ConflictId,Src),Down,Mod,Name,stabilize(Count,Mod,ConflictId,BaseKey,PendingKey,Gen,St,T),T,Down) :-
	expr(Src,Down,Mod,Name,Gen,St,_),
	atom_concat(ConflictId,'_base',BaseKey),
	atom_concat(ConflictId,'_pending',PendingKey).
expr1(first(Limit,Expr),down(Fmt,_,Peek),Mod,Name,Gen,T,Down2) :-
	Down2 = down(Fmt,Limit,Peek),
	expr(Expr,Down2,Mod,Name,Gen,T,Down2).
expr1(buffer(Src),Down,Mod,Name,evgen:xbuffer(Mod,Name,Gen,St,T),T,Down) :- expr(Src,Down,Mod,Name,Gen,St,_).
%expr1(load(Src),Down,Mod,Src,evgen:load_thru(Stream,Mod,Type,T),T,Down) :-
%	open_dest(Src,'should_not_happen',Type,Stream).
expr1(store(Dest,Src),Down,Mod,Name,store_thru(Type,Stream,Mod,Gen,St,T),T,Down2) :-
	expr(Src,Down,Mod,_,Gen,St,_),
	open_dest(Dest,Name,Type,Stream),
	(set_format(Dest,Down,Down2)->
	    true;
	    throw(invalid_format(Dest))).
expr1(filter(Kind,Pred,Len,Src),Down,Mod,Name,xfilter(Kind,Pred,Len,Mod,Gen,St,T),T,Down) :-
	expr(Src,Down,Mod,Name,Gen,St,_).
%expr1(csort_tbd_needed(Column,Src),Down,Mod,Name,ld(F2,T),T,Down) :-
%	expr(Src,Down,Mod,Name,Gen,St,_),
%	open(temp('devc_sort'),write,Stream,[if_exists(generate_unique_name)]),
%	stream_property(Stream, file_name(TempFileName)),
%	format('Logging to ~a~n', [TempFileName]),
%	output(Stream,Down,Mod,Name,Gen,St),
%	exec_sort(F2,Column,F2).
expr1(csort(Column,Src),Down,Mod,Name,xsort(Column,Gen,St,Down,T),T,Down) :-
	expr(Src,Down,Mod,Name,Gen,St,_).

set_format(file(_,Fmt),down(_,L,P),down(Fmt,L,P)) :- valid_format(Fmt).
set_format(Fmt,down(_,L,P),down(Fmt,L,P)) :- valid_format(Fmt).


xsort(Column,Gen,St,_Down,T) :-
	findall(Key-St,(Gen,arg(Column,St,Key)),S1),
	samkeysort(S1,Sorted),
	member(_-T,Sorted).

xfilter(after,Pred,PeekLen,Mod,Gen,St,Head) :-
	gen_head(PeekLen,Mod,Gen,St,[Head|Tail]),
	\+ (member(X,Tail),
	       call(Mod:Pred,Head,X)).

gen_head(TargetLen,Mod,Gen,T,Peek) :-
	Key = head_key,	% TBD recursive
	gen_head_use_key(Key,TargetLen,Mod,Gen,T,Peek).

gen_head_use_key(Key,TargetLen,Mod,Gen,T,FullPeek) :-
	Mod:bb_put(Key,[]),
	Gen,
	Mod:bb_get(Key,Peek),
	append(Peek,[T],FullPeek),
	length(FullPeek,PeekLen),
	(PeekLen < TargetLen ->
	    (Mod:bb_put(Key,FullPeek),fail);
	    true),
	FullPeek = [_|NewPeek],
	Mod:bb_put(Key,NewPeek).
gen_head_use_key(Key,_,Mod,_,_,Peek) :-
	Mod:bb_get(Key,Rem),
	Mod:bb_put(Key,[]),
	suffix(Rem,Peek),
	Peek \== [].
	

xfollow(LookAhead,Mod,Peek1,Peek2,Gm,Tm,Gf,Tf,T) :-
%	percent(Alt,[fail|Gf],AltGen),		
	prepare_percent([fail|Gf],AltGen),	
	Key = follow_key, % TBD recursive
	Mod:bb_put(Key,[]),
	Gm,
	Mod:bb_get(Key,Peek),
	length(Peek,PeekLen),
	(PeekLen < LookAhead ->
	    (Mod:bb_put(Key,[Tm|Peek]),fail);
	    true),
	last(H,NextUp,Peek),
	Mod:bb_put(Key,[Tm|H]),
	append(Peek1,Peek,Peek2),
	(NextUp=T;
	    (percent(Alt,AltGen),
		Alt \== fail,
		once(Alt),
		Tf=T)).

plist([],_,_,_,[],_).
plist([X|Xs],Down,Mod,Name,[G|Gs],T) :-
	(X=X1:P->G=G1:P;(X=X1,G=G1)),
	expr(X1,Down,Mod,Name,G1,T,_),
	plist(Xs,Down,Mod,_,Gs,T).


prepare_bases(N,L) :-
	(N =< 0->
	    L=[];
	    (L=[[]|Z],
		N1 is N-1,
		prepare_bases(N1,Z))).

in_conflict(Mod,BaseKey,ConflictValues) :-
	Mod:bb_get(BaseKey,Bases),
	format("in_conflict(~p,~p,~p,bases=~p)~n",[Mod,BaseKey,ConflictValues,Bases]),
	member(Cv,ConflictValues),
	member(Base,Bases),
	member(Cv,Base),
	format("Conflict found! ~p in ~p~n",[Cv,ConflictValues]).

stabilize(Count,Mod,ConflictId,BaseKey,PendingKey,Gen,Next,T) :-
	prepare_bases(Count,Bases),
	Mod:bb_put(BaseKey,Bases),
	Mod:bb_put(PendingKey,[]),	
	Gen,
	stabilize_next(Mod,ConflictId,BaseKey,PendingKey,Next,T).

stabilize_next(Mod,ConflictId,BaseKey,PendingKey,Next,Term) :-
	(Mod:unstable(ConflictId,base,Next,Base)->
	    (format("Got ~p~n",[Base]),
	      Mod:bb_get(BaseKey,[Ph|Pt]),
		append(Base,Ph,P1),
		Mod:bb_put(BaseKey,[P1|Pt]),
		Next=Term);
	    (Mod:unstable(ConflictId,push,Next,ConflictValues)->
		stabilize_push(Mod,ConflictId,BaseKey,PendingKey,ConflictValues,Next,Term);
		Next=Term)).

stabilize_push(Mod,ConflictId,BaseKey,PendingKey,ConflictValues,Next,Term) :-
	(in_conflict(Mod,BaseKey,ConflictValues) ->
	    (Mod:bb_get(PendingKey,Pending),
		append(Pending,[Next],NextPending),
		Mod:bb_put(PendingKey,NextPending),
		fail);
	    (Mod:bb_get(BaseKey,Bases),
		last(B1,_,Bases),
		NewBases = [[]|B1],
		Mod:bb_update(BaseKey,_,NewBases),
		format(">>> Now(~a) ~p~n",[BaseKey,NewBases]),
		Mod:bb_update(PendingKey,Pending,[]),
		(Next=Term;
		    (member(Pg,Pending),
			format("RETRY: ~p~n",[Pg]),
			stabilize_next(Mod,ConflictId,BaseKey,PendingKey,Pg,Term))))).

:- dynamic gen_counter/2.

xgen(InArgs,Pred,Arity,Down,Mod,Term) :-
	down(limit,Down,Limit),
	down(peek,Down,Peek),
	defined_rule(Mod,Pred,Arity,InArgs,Limit,Index,Peek,Term,Invocation),
	statistics(walltime,[UniqueValue,_]),
	retractall(gen_counter(UniqueValue,_)),
	assert(gen_counter(UniqueValue,0)),  % TDB max/limit broken
	Mod:Invocation,
	retract(gen_counter(UniqueValue,Index)),
	NextIndex is Index + 1,
	assert(gen_counter(UniqueValue,NextIndex)).


defined_rule(Mod,Pred,Arity,InArgs,Limit,Index,Peek,Term,Invocation) :-
	Rx = Mod:'$grule'(Pred,Arity,InArgs,Limit,Index,Peek,Term,Invocation),
      	current_predicate('$grule',Rx),
	Rx.
defined_rule(Mod,Pred,0,[],_,_,_,T,Mod:T) :-
	current_predicate(Mod:Pred/Arity),
	length(Args,Arity),
	T =.. [Pred|Args].


xbuffer(Mod,_Name,Gen,T,T) :-
	Gen,
	AssertMod = Mod,  % TBD
	xassert(Mod,AssertMod,T).

store_thru(Type,Stream,Mod,Gen,T,T) :-
	Gen,
	format_type(Type,T,Mod,Stream).
store_thru(_,Stream,_,_,_) :- close(Stream).

%load_thru(Stream,_Mod,Type,T) :- read_next(Type,Stream,T).
%load_thru(Stream,_,_,_) :- close(Stream), fail.


open_dest(Fmt,_,Fmt,Stream) :- valid_format(Fmt), current_output(Stream).
open_dest(file(Type),Name,Type,Stream) :- open_name_type(Name,Type,Stream).
open_dest(file(Name,Type),_Name,Type,Stream) :- open_name_type(Name,Type,Stream).

open_name_type(Name,Type,Stream) :-
	Dir = 'generated',
	(directory_exists(Dir)->
	    true;
	    make_directory(Dir)),
	atom_concat(Dir,'/',F1),
	atom_concat(F1,Name,F2),
	atom_concat(F2,'.',F3),
	atom_concat(F3,Type,Fn),
	open(Fn,write,Stream).

output(S,Down,Mod,_,Gen,T) :- functor(S,'$stream',_), down(format,Down,Fmt), wgen_and_close(Gen,T,Mod,Fmt,S,Down).
output(D,Down,Mod,Name,Gen,T) :- open_dest(D,Name,Type,Stream), wgen_and_close(Gen,T,Mod,Type,Stream,Down).

wgen_and_close(Gen,T,Mod,Type,Stream,Down) :-
	(stream_property(Stream, file_name(Fn))->true;Fn=''),
	statistics(walltime,[T1,_]),
	Key = '$LimitCounter',
	call_cleanup(wgen(Gen,T,Key,Mod,Type,Stream,Down),
		     (stream_property(Stream,interactive)->true;close(Stream))),
	statistics(walltime,[T2,_]),
	WallTimeSeconds is (T2 - T1) / 1000,
	format_time(WallTimeSeconds,Sf),
	bb_get(Key,NumberOutput),
	(Fn=''->Where=stdout;Where=Fn),
	(NumberOutput=1->What=line;What=lines),
	info("Execution summary: ~p ~a in ~s written to ~p",[NumberOutput,What,Sf,Where]).


wgen(Gen,T,Key,Mod,Type,Stream,down(_,Limit,_)) :-
	bb_put(Key,0),
	Gen,
	bb_get(Key,N),
	((Limit < 0 ; N < Limit)->
	    (format_type(Type,T,Mod,Stream),
		N1 is N+1,
		bb_put(Key,N1),
		fail);
	    !).
wgen(_,_,_,_,_,_,_).



%%%
%%% FORMATTING
%%%


valid_format(Fmt) :- functor(Fmt,F,_), valid_functor_format(F).

valid_functor_format(pl).
valid_functor_format(csv).
valid_functor_format(xml).

format_type(pl,T,_Mod,Stream) :- writeq(Stream,T), write(Stream,'.'), nl.
format_type(csv,T,Mod,Stream) :- format_csv(',',T,Mod,Stream).
format_type(csv(Sep),T,Mod,Stream) :- format_csv(Sep,T,Mod,Stream).
format_type(xml,T,Mod,Stream) :- format_xml(T,Mod,2,Stream).
format_type(xml(Tab),T,Mod,Stream) :- format_xml(T,Mod,Tab,Stream).

format_csv(Sep,T,Mod,Stream) :-	
	functor(T,F,Arity),
	format_simple(F,Sep,Stream),
	(format_csv_items(1,Sep,F,Arity,T,Mod,Stream,Spill)->
	    true;
	    throw('FormatFailure'(csv,T))),
	nl(Stream),
	(foreach(Tn,Spill), param([Sep,Mod,Stream]) do (foreach(Tx,Tn), param([Sep,Mod,Stream]) do format_csv(Sep,Tx,Mod,Stream))).


format_csv_items(Pos,Sep,F,Arity,T,Mod,Stream,Spill) :-
	(Pos > Arity->
	    Spill=[];
	    (write(Stream,Sep),
		arg(Pos,T,Arg),
		format_csv_item(F,Pos,Arg,Mod,Stream,Sep,Sz,Spill),
		Pos1 is Pos+1,
		format_csv_items(Pos1,Sep,F,Arity,T,Mod,Stream,Sz))).
	
format_csv_item(Enc,Ord,Value,Mod,Stream,Sep,S1,S2) :-
	((current_predicate(Mod:'$gfd'/12),
	  Mod:'$gfd'(_,Enc,_Arity,Ord,Type,_Inverse,_Min,Max,_Kx,_,_,_))->
	    true;
	    (Max=1,Type=term)),
	((is_list(Value), Type \== string) ->
	    format_csv_item_list(Value,true,Type,Mod,Stream,Sep,S1,S2);
	    (format_single_csv_item(Type,Mod,Stream,Sep,Value)->
		S1=S2;
		(write(Stream,1),S2=[Value|S1]))).

format_single_csv_item(date,_,Stream,_,V) :- write(Stream,V).
format_single_csv_item(string,_,Stream,Sep,V) :- format_simple(V,Sep,Stream).
format_single_csv_item(int,_,Stream,_,V) :- write(Stream,V).
format_single_csv_item(term,Mod,Stream,Sep,V) :-
	((current_predicate(Mod:expose_term/3),
	  Mod:expose_term(V,Format,Args))->
	    format(Stream,Format,Args);
	    format_simple(V,Sep,Stream)).

format_csv_item_list(Vs,First,Type,Mod,Stream,Sep,S1,S2) :-
	(member(Type,[date,string,int,term])->
	    (format_csvs(Vs,First,Type,Mod,Stream,Sep),S1=S2);
	    (length(Vs,Len), write(Stream,Len), S2 = [Vs|S1])).


format_csvs([],_,_,_,_,_).
format_csvs([V|Vs],First,Type,Mod,Stream,Sep) :-
	(First->true;write(Stream,'|')),
	format_single_csv_item(Type,Mod,Stream,Sep,V),
	format_csvs(Vs,false,Type,Mod,Stream,Sep).


format_simple(X,Sep,Stream) :-
	(atom(X)->atom_chars(X,Xc);Xc=X),
	format_string(Xc,Sep,Stream).

format_string([],_,_).
format_string([C|R],Sep,Stream) :-
	(C==Sep->write('\\');true),
	write(Stream,C),
	format_string(R,Sep,Stream).


format_xml(T,Mod,Tab,Stream) :-	format_xml(T,Mod,Tab,0,Stream).
format_xml(T,Mod,Tab,Indent,Stream) :-
	indent(Stream,Tab,Indent,NestedIndent),
	T =.. [P|Args],
	format_xml_term(Args,P,Mod,Tab,Indent,NestedIndent,Stream),
	nl(Stream).

format_xml_term([],P,_Mod,_Tab,_,_,Stream) :-
	format(Stream,"<~a/>",[P]).
format_xml_term([Arg],P,_Mod,_Tab,_,_,Stream) :-
	simple(Arg),
	!,
	format(Stream,"<~a>~a<~a>",[P,Arg,P]).
format_xml_term([A|Z],P,Mod,Tab,Indent,NestedIndent,Stream) :-
	format(Stream,"<~a>~n",[P]),
	format_xml_args([A|Z],Mod,Tab,NestedIndent,Stream),
	indent(Stream,Tab,Indent,_),
	format(Stream,"</~a>",[P]).

format_xml_args([],_,_,_,_).
format_xml_args([X|Z],Mod,Tab,Indent,Stream) :-
	format_xml(X,Mod,Tab,Indent,Stream),
	format_xml_args(Z,Mod,Tab,Indent,Stream).

indent(Stream,Tab,Indent,NestedIndent) :-
	(for(_,1,Indent), param(Stream) do write(Stream,' ')),
	NestedIndent is Indent + Tab.

:-set_prolog_flag(discontiguous_warnings,off).

:- multifile user:term_expansion/6.
:- discontiguous user:term_expansion/6.

% Formats that must be handled:
%
%  * bar::foo(X,Y)
%  * N::foo(X,Y)
%  * bar(N)::foo(X,Y)
%  * bar(N), pos=P::foo(X,Y) :- get_mutble(V,P).
%
user:term_expansion(Rule,Layout,Ids,[Access,Rule],Layout,[Token|Ids]) :-
	Token=gen_token,
	nonmember(Token,Ids),
	(Rule=(Invocation:-_)->true;Rule=Invocation),
	Invocation = (Call :: Term),
	expand_rule(Call,Term,Access).


rule_format(V,As,Az,Ks,Ks,_) :- var(V), !, append(As,[V],Az).
rule_format(X=Y,As,As,Ks,[X=Y|Ks],_) :- !.
rule_format((X,Y),As,Az,Ks,Kz,F) :-
	!,
	rule_format(X,As,A1,Ks,K1,F),
	rule_format(Y,A1,Az,K1,Kz,F).
rule_format(T,_,Az,K,K,F) :-	T =.. [F|Az].

expand_rule(Call,GenTerm,Access) :-
	once(rule_format(Call,[],Args,[],Kws,Pred)),
	length(Args,Arity),
	(var(Pred)->
	    (var(GenTerm)->
		throw(invalid_rule_syntax('one side of :: rule must be nonvar'));
		functor(GenTerm,Pred,_Arity));
	    true),
	bind_gen_args(Kws,_,Limit,Index,Peek),
	Access = '$grule'(Pred,Arity,Args,Limit,Index,Peek,GenTerm,(Call::GenTerm)).

% bind_gen_args(+ParametersFromRule,+InArgsFromFormula,?Limit,?Index,?Peek)
% ParametersFromRule list is bound with corresponding InArgsFromFormula,
% excluding keyword args.
%
bind_gen_args([],[],_,_,_).
bind_gen_args([P|Ps],InArgs,Limit,Index,Peek) :-
	((nonvar(P),P=(X=Y))->
	    (keyword_arg(X,Limit,Index,Peek,Y), Iz=InArgs);
	    InArgs = [P|Iz]),
	bind_gen_args(Ps,Iz,Limit,Index,Peek).

keyword_arg(max,V,_,_,V).
keyword_arg(index,_,V,_,V).
keyword_arg(peek,_,_,V,V).

listify(X,Y) :-
	(is_list(X)->
	    X=Y;
	    ((nonvar(X),X=(H,T))->
		(Y=[H|Z], listify(T,Z));
		Y = [X])).


%%%
%%% UNIT TEST
%%%

:- begin_tests(evgen).

test(xyz,[true(Rez=[a,b,c]),nondet]) :- gen_head(3,evgen,member(T,[a,b,c,d,e]),T,Rez).
test(xyz,[true(Rez=[a,b,c]),nondet]) :- gen_head(5,evgen,member(T,[a,b,c]),T,Rez).
test(xyz,true(Rez=[[a,b],[b,c],[c]])) :- findall(L,gen_head(2,evgen,member(T,[a,b,c]),T,L),Rez).
test(xyz,true(Rez=[[a,b,c],[b,c],[c]])) :- findall(L,gen_head(8,evgen,member(T,[a,b,c]),T,L),Rez).


test(xrule1,true(Rez=Exp)) :-
	Term = bar(_,_),
	Rule = (foo::Term),
	Exp = '$grule'(foo,0,[],_Limit,_Index,_Peek,Term,Rule),
	expand_term(Rule,[Rez|_]).
test(xrule2,true(Rez=Exp)) :-
	Term = bar(_,_),
	Rule = (foo(x)::Term),
	Exp = '$grule'(foo,1,[x],_Limit,_Index,_Peek,Term,Rule),
	expand_term(Rule,[Rez|_]).
test(xrule3,true(Rez=Exp)) :-
	Term = bar(_,_),
	Peek = y,
	Rule = (foo(x),peek=Peek::Term),
	Exp = '$grule'(foo,1,[x],_Limit,_Index,Peek,Term,Rule),
	expand_term(Rule,[Rez|_]).
test(xrule4,true(Rez=Exp)) :-
	Term = bar(_,_),
	Rule = (Var::Term),
	Exp = '$grule'(bar,1,[Var],_Limit,_Index,_Peek,Term,Rule),
	expand_term(Rule,[Rez|_]).
test(xrule5,true(Rez=Exp)) :-
	Term = bar(_,_),
	Rule = ((Var1,Var2)::Term),
	Exp = '$grule'(bar,2,[Var1,Var2],_Limit,_Index,_Peek,Term,Rule),
	expand_term(Rule,[Rez|_]).
test(xrule6,true(Rez=Exp)) :-
	Term = bar(_,_),
	Peek = y,
	Rule = (peek=Peek::Term),
	Exp = '$grule'(bar,0,[],_Limit,_Index,Peek,Term,Rule),
	expand_term(Rule,[Rez|_]).
test(xrule7,true(Rez=Exp)) :-
	Term = bar(_,_),
	Peek = y,
	Index = z,
	Rule = (peek=Peek,index=Index::Term),
	Exp = '$grule'(bar,0,[],_Limit,Index,Peek,Term,Rule),
	expand_term(Rule,[Rez|_]).

% Need another way to test
%test(rule_pct,true(Rez=[Exp|_])) :-
%	Rule = (10 pct foo::bar),
%	Exp = '$grule'(foo,0,[],_Limit,_Index,_Peek,bar,Rule),
%	expand_term(Rule,Rez).





:- end_tests(evgen).
