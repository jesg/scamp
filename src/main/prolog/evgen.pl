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
:- module(evgen,[gen/1,gen/2,gen/3,gen/4,gen2/2,store/2,buffer/1,csort/2,follow/3,seq/1,stabilize/3,filter/4,
		 findone/1, findone/2,tracker/3,track/2,tracked/2]).
:- use_module(library(between)).
:- use_module(library(lists)).
:- use_module(library(samsort)).
:- use_module(library(random)).
:- use_module(library(file_systems)).
:- load_files(library(plunit),[if(changed),load_type(source)]).
:- use_module(percent).
:- use_module(typer).
:- use_module(utils).

%:- op(520,yfx,'..').  % a little higher than '-'
:- op(700,xfx,as).
:- op(950,xfy,pct).
%:- op(1150,xfy,pct).
%:- op(1000,fy,pct).
:- op(950,fy,pct).
:- op(500,xfy,generate).
:- op(1150,fx,generate).
:- op(400,xf,k). % thousand
:- op(400,xf,m). % million
:- op(400,xf,b). % billion

% Notes
% 1: Simulate entire state
% 2: Simulate isolated sequences over time, sort, phase 2 to mix in verification queries


:- meta_predicate findone(2).
:- meta_predicate findone(2,+).

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


:- meta_predicate tracker2(+,2,-).
tracker2(Max,Mod:Id,tracker(Id,Mod,Max)) :- (Mod:bb_delete(Id,_)->true;true), Mod:bb_put(Id,tk(0,[])).
tracked2(V,tracker(Id,Mod,Max)) :- Mod:bb_get(Id,tk(_Len,L)), head_member(V,L,0,Max).
track2(V,tracker(Id,Mod,Max)) :-
	Mod:bb_get(Id,tk(Len,Current)),
	L1=[V|Current],
	(Len > 100 ->
	    first_n_members(L1,Max,L2,Len2);
	    (L1=L2,Len2 is Len+1)),
	Mod:bb_update(Id,_,tk(Len2,L2)).

head_member(V,L,Max) :- head_member(V,L,0,Max).
head_member(V,[Head|Tail],Pos,Max) :-
	Pos < Max,
	(V=Head->
	    true;
	    (P1 is Pos+1, head_member(V,Tail,P1,Max))).

first_n_members([],_,[],0).
first_n_members([X|Xs],Max,Y,Len) :-
	(Max =< 0->
	    (Y=[],Len=0);
	    (Y=[X|Ys],M1 is Max-1,first_n_members(Xs,M1,Ys,Lz),Len is Lz+1)).


t1(Count) :- for(I,1,Count) do (tracker(2,foo,T), track(I,T), (tracked(q,T)->true;true)).
t2(Count) :- for(I,1,Count) do (tracker2(2,foo,T), track2(I,T), (tracked2(q,T)->true;true)).

gg(Count) :- utils:saytime(evgen:t1(Count)), utils:saytime(evgen:t2(Count)).


% sort name vs. ord
% sort ext vs. int
% format
% format customization
% gen operator
% composite keys
% atom lhs (no first arg)
% nox/index/ordex (investigate direct clause ref)
% assertx that keeps those in sync
% 'any' op; uses random then ordex then index
% add total=X, in addition to index=X, that counts total output
% report on total at end
% limits / head



%%%
%%% Top-leval predicates
%%%
:- meta_predicate gen(2).
:- meta_predicate gen(?,2).
:- meta_predicate gen(?,?,2).
:- meta_predicate gen(?,?,?,2).
:- meta_predicate gen2(?,2).
:- meta_predicate store(?,2).
:- meta_predicate buffer(2).
:- meta_predicate csort(+,2).
:- meta_predicate follow(+,2,+).
:- meta_predicate filter(+,2,+,2).
:- meta_predicate seq(2).
:- meta_predicate stabilize(+,+,2).

tell_starting_from(Op) :- current_directory(D), format('[ruge INFO] : "~p" invoked from directory ~p~n',[Op,D]).


gen(Src) :- gen([],Src).
gen(A1,Src) :-
	(is_list(A1)->
	    L=A1;
	    L=[A1]),
	gen2(L,Src).
gen(A1,A2,Src) :- gen2([A1,A2],Src).
gen(A1,A2,A3,Src) :- gen2([A1,A2,A3],Src).

gen2(Args,Mod:Src) :-
	tell_starting_from('gen'),
	append(Args,[Src],Z),
	Expr =.. [gen|Z],
	expr_generator(Expr,Mod,Name,Gen,T),
	current_output(Stream),
	output(Stream,Mod,Name,Gen,T).

store(Dest,Mod:Src) :-
	tell_starting_from('store'),	
	expr_generator(Src,Mod,Name,Gen,T),
	output(Dest,Mod,Name,Gen,T).

buffer(Mod:Src) :-
	tell_starting_from('buffer'),	
	expr_generator(buffer(Src),Mod,Name,Gen,T),
	current_output(Stream),
	output(Stream,Mod,Name,Gen,T).
%	format("Output buffer to module ~a~n",[Name]).

seq(Mod:Src) :-
	tell_starting_from('seq'),
	expr_generator(seq(Src),Mod,Name,Gen,T),
	current_output(Stream),
	output(Stream,Mod,Name,Gen,T).

stabilize(Count,ConflictId,Mod:Src) :-
	tell_starting_from('stabliize'),
	expr_generator(stabilize(Count,ConflictId,Src),Mod,Name,Gen,T),
	current_output(Stream),
	output(Stream,Mod,Name,Gen,T).

csort(Column,Mod:Src) :- 
	tell_starting_from('csort'),
	expr_generator(csort(Column,Src),Mod,Name,Gen,T),
	current_output(Stream),
	output(Stream,Mod,Name,Gen,T).

%pct(Mod:Src) :-
%	expr_generator(pct(Src),Mod,Name,Gen,T),
%	current_output(Stream),
%	output(Stream,Mod,Name,Gen,T).

follow(LookAhead,Mod:Src,Follow) :-
	tell_starting_from('follow'),
	expr_generator(follow(LookAhead,Src,Follow),Mod,Name,Gen,T),
	current_output(Stream),
	output(Stream,Mod,Name,Gen,T).

filter(Kind,Mod:Pred,Len,Mod:Src) :-
	tell_starting_from('filter'),
	expr_generator(filter(Kind,Mod:Pred,Len,Src),Mod,Name,Gen,T),
	current_output(Stream),
	output(Stream,Mod,Name,Gen,T).


% expr_generator(+Expr,+Module,?Name,?Generator,?GeneratedTerm)
% Generator can be called repeatedly, binding GeneratedTerm,
% reflecting the intent of Expr. Name is the best guess for
% a name that might be used as file name or module name for
% buffering.
%
expr_generator(Expr,Mod,Name,Gen,T) :-
	catch(expr(Expr,down(-1,[]),Mod,Name,Gen,T),
	      Code,
	      throw('ExprSyntaxError'(Code))).

down(limit,down(Limit,_),Limit).
down(peek,down(_,Peek),Peek).

expr(Expr,Down,Mod,F,Gen,T) :-
	(expr1(Expr,Down,Mod,F,Gen,T)->
	    true;
	    throw('SubExprFailed'(Expr))).

expr1(Gp,Down,Mod,Pred,xgen(Args,Pred,Arity,Down,Mod,T),T) :-
	Gp =.. [gen|Z],
	append(Args,[PredSpec],Z),
	(PredSpec=Pred/Arity->true;PredSpec=Pred),
	% Validate that a rule exists
	Rx = Mod:'$grule'(Pred,Arity,_,_,_,_,_,false),  % TBD check for arity too
      	current_predicate('$grule',Rx),
	(Rx->true;throw('UnknownGeneratorRuleException'(PredSpec))).
expr1(seq(Seq),Down,Mod,Name,(member(G,Gs),G),T) :-
	plist(Seq,Down,Mod,Name,Gs,T).
expr1(follow(LookAhead,SrcMain,Follow),Down,Mod,Name,xfollow(LookAhead,Mod,Peek1,Peek2,Gm,Tm,Gf,Tf,T),T) :-
	expr(SrcMain,Down,Mod,Name,Gm,Tm),
	Down = down(Limit,Peek1),
	Down2 = down(Limit,Peek2),
	plist(Follow,Down2,Mod,_Name,Gf,Tf).
expr1(stabilize(Count,ConflictId,Src),Down,Mod,Name,stabilize(Count,Mod,ConflictId,BaseKey,PendingKey,Gen,St,T),T) :-
	expr(Src,Down,Mod,Name,Gen,St),
	atom_concat(ConflictId,'_base',BaseKey),
	atom_concat(ConflictId,'_pending',PendingKey).
expr1(head(Limit,Expr),down(_,Peek),Mod,Name,Gen,T) :- expr(Expr,down(Limit,Peek),Mod,Name,Gen,T).
expr1(buffer(Src),Down,Mod,Name,evgen:xbuffer(Mod,Name,Gen,St,T),T) :- expr(Src,Down,Mod,Name,Gen,St).
expr1(load(Src),_,Mod,Src,evgen:load_thru(Stream,Mod,Type,T),T) :-
	open_dest(Src,'should_not_happen',Type,Stream).
expr1(store(Dest,Src),Down,Mod,Name,store_thru(Type,Stream,Mod,Gen,St,T),T) :-
	expr(Src,Down,Mod,_,Gen,St),
	open_dest(Dest,Name,Type,Stream).
expr1(filter(Kind,Pred,Len,Src),Down,Mod,Name,xfilter(Kind,Pred,Len,Mod,Gen,St,T),T) :-
	expr(Src,Down,Mod,Name,Gen,St).
expr1(csort_tbd_needed(Column,Src),Down,Mod,Name,ld(F2,T),T) :-
	expr(Src,Down,Mod,Name,Gen,St),
	open(temp('devc_sort'),write,Stream,[if_exists(generate_unique_name)]),
	stream_property(Stream, file_name(TempFileName)),
	format('Logging to ~a~n', [TempFileName]),
	output(Stream,Mod,Name,Gen,St),
	exec_sort(F2,Column,F2).
expr1(csort(Column,Src),Down,Mod,Name,xsort(Column,Gen,St,Down,T),T) :-
	expr(Src,Down,Mod,Name,Gen,St).


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
	percent(Alt,[fail|Gf],AltGen),	
	Key = follow_key, % TBD recursive
	Mod:bb_put(Key,[]),
	Gm,
	%(Tm=update_event(9342,_Src,_Channel,_Sender,created,_Date,_Template,_Accounts,_2140001274,_Payload)->format("follow1.YES~n",[]);true),
	Mod:bb_get(Key,Peek),
	length(Peek,PeekLen),
	(PeekLen < LookAhead ->
	    (Mod:bb_put(Key,[Tm|Peek]),fail);
	    true),
	last(H,NextUp,Peek),
	Mod:bb_put(Key,[Tm|H]),
	append(Peek1,Peek,Peek2),
	(NextUp=T;
	    (AltGen,
		Alt \== fail,
		once(Alt),
		Tf=T)).
	%(T=update_event(9342,_Src,_Channel,_Sender,created,_Date,_Template,_Accounts,_2140001274,_Payload)->format("follow2.YES~n",[]);true).


plist([],_,_,_,[],_).
plist([X|Xs],Down,Mod,Name,[G|Gs],T) :-
	(X=X1:P->G=G1:P;(X=X1,G=G1)),
	expr(X1,Down,Mod,Name,G1,T),
	plist(Xs,Down,Mod,_,Gs,T).


prepare_bases(N,L) :-
	(N =< 0->
	    L=[];
	    (L=[[]|Z],
		N1 is N-1,
		prepare_bases(N1,Z))).

in_conflict(Mod,BaseKey,ConflictValues) :-
%	Mod:unstable(ConflictId,push,Next,ConflictValues),
	Mod:bb_get(BaseKey,Bases),
	format("in_conflict(~p,~p,~p,bases=~p)~n",[Mod,BaseKey,ConflictValues,Bases]),
	member(Cv,ConflictValues),
	member(Base,Bases),
	member(Cv,Base),
	format("Conflict found! ~p in ~p~n",[Cv,ConflictValues]).

%stabilize(_Count,_Mod,_ConflictId,_BaseKey,_PendingKey,Gen,T,T) :-
%	Gen.
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
		    

xgen(InArgs,Pred,Arity,Down,Mod,Term) :-
	down(limit,Down,Limit),
	down(peek,Down,Peek),
	Rx = Mod:'$grule'(Pred,Arity,InArgs,Limit,Index,Peek,Term,true),
      	current_predicate('$grule',Rx),
	statistics(walltime,[UniqueValue,_]),
	retractall(gen_counter(UniqueValue,_)),
	assert(gen_counter(UniqueValue,0)),  % TDB max/limit broken
	Rx,
	retract(gen_counter(UniqueValue,Index)),
	NextIndex is Index + 1,
	assert(gen_counter(UniqueValue,NextIndex)).


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

xbuffer(Mod,_Name,Gen,T,T) :-
%	(current_predicate(N,Mod:P)->  % TBD arity
%	    retractall(Mod:P);
%	    true),
	Gen,
	%(T=update_event(9342,_Src,_Channel,_Sender,created,_Date,_Template,_Accounts,_2140001274,_Payload)->(format("buffer1.YES~n",[]),blue_doggie);true),	
	AssertMod = Mod,  % TBD
	xassert(Mod,AssertMod,T).
	%(T=update_event(9342,_Src,_Channel,_Sender,created,_Date,_Template,_Accounts,_2140001274,_Payload)->format("buffer2.YES~n",[]);true).

blue_doggie.

store_thru(Type,Stream,_Mod,Gen,T,T) :-
	Gen,
	format_type(Type,T,Stream).
store_thru(_,Stream,_,_,_) :- close(Stream).

load_thru(Stream,_Mod,Type,T) :- read_next(Type,Stream,T).
load_thru(Stream,_,_,_) :- close(Stream), fail.


open_dest(file(Type),Name,Type,Stream) :- open_name_type(Name,Type,Stream).
open_dest(file(Type,Name),_Name,Type,Stream) :- open_name_type(Name,Type,Stream).

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

output(S,Mod,_,Gen,T) :- functor(S,'$stream',_), wgen_and_close(Gen,T,Mod,csv,S).
output(D,Mod,Name,Gen,T) :- open_dest(D,Name,Type,Stream), wgen_and_close(Gen,T,Mod,Type,Stream).

wgen_and_close(Gen,T,Mod,Type,Stream) :-
	(stream_property(Stream, file_name(Fn))->true;Fn=''),
	statistics(walltime,[T1,_]),
	call_cleanup(wgen(Gen,T,Mod,Type,Stream),
		     (stream_property(Stream,interactive)->true;close(Stream))),
	statistics(walltime,[T2,_]),
	WallTimeSeconds is (T2 - T1) / 1000,
	format_time(WallTimeSeconds,Sf),
	(Fn=''->
	    format("Time ",[]);
	    format("Output to ~a ",[Fn])),
	format("~s~n",[Sf]).	


wgen(Gen,T,Mod,Type,Stream) :-
	Gen,
	format_type(Type,T,Mod,Stream),
	fail.
wgen(_,_,_,_,_).


format_type(csv,T,Mod,Stream) :- format_csv(',',T,Mod,Stream).
format_type(csv(Sep),T,Mod,Stream) :- format_csv(Sep,T,Mod,Stream).

%format_csv(Sep,T_hack,Mod,Stream) :-
format_csv(Sep,T,Mod,Stream) :-	
%	(functor(T_hack,action,1)->
%	    arg(1,T_hack,T);
%	    T_hack=T),
	functor(T,F,Arity),
	write(Stream,F),
	(format_csv_items(1,Sep,F,Arity,T,Mod,Stream,Spill)->
	    true;
	    throw('FormatFailure'(csv,T))),
	nl(Stream),
	%format("Spill ~p~n",[Spill]),
	(foreach(Tn,Spill), param([Sep,Mod,Stream]) do (foreach(Tx,Tn), param([Sep,Mod,Stream]) do format_csv(Sep,Tx,Mod,Stream))).


format_csv_items(Pos,Sep,F,Arity,T,Mod,Stream,Spill) :-
	(Pos > Arity->
	    Spill=[];
	    (write(Stream,Sep),
		arg(Pos,T,Arg),
		format_csv_item(F,Pos,Arg,Mod,Stream,Sz,Spill),
		Pos1 is Pos+1,
		format_csv_items(Pos1,Sep,F,Arity,T,Mod,Stream,Sz))).
	
%format_csv_items(Sep,F,Arity,T,Mod,Stream) :-
%	(for(X,1,Arity) do (write(Stream,Sep), arg(X,T,Arg), format_csv_item(F,X,Arg,Mod,Stream))).

format_csv_item(Enc,Ord,Value,Mod,Stream,S1,S2) :-
	(Mod:'$gfd'(_,Enc,_Arity,Ord,Type,_Inverse,_Min,Max,_Kx,_,_,_)->
	    true;
	    (Max=1,Type=term)),
	((is_list(Value), Max \== 1) ->
	    format_csv_item_list(Value,true,Type,Mod,Stream,S1,S2);
	    (format_single_csv_item(Type,Mod,Stream,Value)->
		S1=S2;
		(write(Stream,1),S2=[Value|S1]))).

format_single_csv_item(date,_,Stream,V) :- write(Stream,V).
format_single_csv_item(string,_,Stream,V) :- is_list(V)->format(Stream,"~s",[V]);write(Stream,V).
format_single_csv_item(int,_,Stream,V) :- write(Stream,V).
format_single_csv_item(term,Mod,Stream,V) :-
	(Mod:expose_term(V,Format,Args)->
	    format(Stream,Format,Args);
	    write(Stream,V)).
%format_single_csv_item(_UserDefined,_Mod,Stream,V,S,[V|S]) :- write(Stream,1).

format_csv_item_list(Vs,First,Type,Mod,Stream,S1,S2) :-
	(member(Type,[date,string,int,term])->
	    (format_csvs(Vs,First,Type,Mod,Stream),S1=S2);
	    (length(Vs,Len), write(Stream,Len), S2 = [Vs|S1])).
%	    (length(Vs,Len), format("HIT! ~p~n",[Vs]), abort, write(Stream,Len), S2 = [Vs|S1])).


format_csvs([],_,_,_,_).
format_csvs([V|Vs],First,Type,Mod,Stream) :-
	(First->true;write(Stream,'|')),
	format_single_csv_item(Type,Mod,Stream,V),
	format_csvs(Vs,false,Type,Mod,Stream).


generation_rule_head((N generate P::G),N,P,1,G).
generation_rule_head((N generate P),N,P,A,G) :- functor(P,G,A).
generation_rule_head((generate P::G),[],P,1,G).
generation_rule_head((generate P),[],P,A,G) :- functor(P,G,A).

:-set_prolog_flag(discontiguous_warnings,off).

:- multifile user:term_expansion/6.
:- discontiguous user:term_expansion/6.

user:term_expansion(Term,Layout,Ids,[Access,Term],Layout,[Token|Ids]) :-
	Token=gen_token,
	nonmember(Token, Ids),
	(Term=(Th:-_)->true;Term=Th),
%        (generation_rule_head(Th,Params,CalledByFunctor,Rule)->true;fail),
        generation_rule_head(Th,Params,F,A,Rule),
	!,
        listify(Params,Ps),
%        functor(Rule,F,A),
%	(Th=(Parameters generate Rule)->listify(Parameters,Ps);Th=(generate Rule)),
%	(Rule='::'(CalledByFunctor,GenTerm)->
%         functor(GenTerm,F,A);
%	 (functor(Rule,F,A), CalledByFunctor=F)),     
	bind_gen_args(Ps,Args,Limit,Index,Peek),
	Access = ('$grule'(F,A,Args,Limit,Index,Peek,Rule,Exec) :- (Exec->Th;true)).


listify(X,Y) :-
	(is_list(X)->
	    X=Y;
	    ((nonvar(X),X=(H,T))->
		(Y=[H|Z], listify(T,Z));
		Y = [X])).

%C generate update_event(X) :- between(1,C,Z), (Z=3->X=0;X=Z).
%generate query_by_account(x).
%generate query_by_recipient(x).




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


expand_generator_term(((Pct pct Term) :- Body), M, (ExpandedTerm :- Body)) :- !, expand_pct_term(Pct,Term,M,ExpandedTerm).
expand_generator_term((Pct pct Term), M, ExpandedTerm) :- expand_pct_term(Pct,Term,M,ExpandedTerm).
expand_generator_term(end_of_file, M, end_of_file) :- expand_generator_eof(M).


expand_generator_eof(M) :-
	current_predicate(M:'track_pct_expand$$'/5),
	bagof((Index:Pct),(M:'track_pct_expand$$'(F,A,Fr,Pct,Index)),Ixs),
	%format("G_EOF ~p/~p ~p~n",[F,A,Ixs]),
	percent(ActualIndex,Ixs,Gen),
	length(HeadArgs,A),
	Fh =.. [F|HeadArgs],
	BodyArgs = [ActualIndex|HeadArgs],
	Fb =.. [Fr|BodyArgs],
	%assert((M:Fh :- Gen, Fb)),
	M:assert((pct(Fh) :- Gen, Fb)),
	M:assert((Fh :- Fb)),
	fail.
expand_generator_eof(_).
	       

expand_pct_term(Pct,Term,M,ExpandedTerm) :-
	functor(Term,F,A),
	%format("--XPAND ~a:~a/~p~n",[M,F,A]),
	((current_predicate(M:'track_pct_expand$$'/5), findall(tag,M:'track_pct_expand$$'(F,A,_,_,_),Tags))->
	    length(Tags,Index);
	    Index=0),
	Term =.. [F|Args],
	atom_concat(F,'$$',Fr),
	assert(M:'track_pct_expand$$'(F,A,Fr,Pct,Index)),
	ExpandedTerm =.. [Fr,Index|Args].

user:term_expansion(Term1, Layout, Ids, Term2, Layout, [pct_token|Ids]) :-
	nonmember(pct_token, Ids),
	prolog_load_context(module,Module),
	(current_predicate(expand_generator_term/3)-> % avoid strange error msg when loading this module
	    expand_generator_term(Term1,Module,Term2),
	    true).



:- begin_tests(evgen).

test(xyz,[true(Rez=[a,b,c]),nondet]) :- gen_head(3,evgen,member(T,[a,b,c,d,e]),T,Rez).
test(xyz,[true(Rez=[a,b,c]),nondet]) :- gen_head(5,evgen,member(T,[a,b,c]),T,Rez).
test(xyz,true(Rez=[[a,b],[b,c],[c]])) :- findall(L,gen_head(2,evgen,member(T,[a,b,c]),T,L),Rez).
test(xyz,true(Rez=[[a,b,c],[b,c],[c]])) :- findall(L,gen_head(8,evgen,member(T,[a,b,c]),T,L),Rez).

:- end_tests(evgen).