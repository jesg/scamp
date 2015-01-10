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
:- module(evgen,[
              gen/1,
              gen/2,
              gen/3,
              take/2,
              take/3]
         ).

:- expects_dialect(sicstus).
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(random)).
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).

% File: evgen.pl
% Author: Brendan McCarhty


:- op(700,xfx,as).
:- op(400,xf,k). % thousand
:- op(400,xf,m). % million
:- op(400,xf,b). % billion

%%%
%%% Top-leval predicates
%%%
:- meta_predicate gen(2).
:- meta_predicate gen(2,+).
:- meta_predicate take(+,2,+).

run_top(Mod,Expr) :-
    current_output(Stream),
    run_top(Mod,Expr,[format(pl),output(Stream)]).

run_top(Mod,Expr,Options) :-
	functor(Expr,F,_),
	expr_generator(Expr,Mod,Name,Gen,T,Down),
  (member(output(Stream),Options)->true;current_output(Stream)),
  (member(format(Fmt),Options)->true;Fmt=pl),
	output(dest(Stream,Fmt),Down,Mod,Name,Gen,T).

gen(Mod:Src) :- run_top(Mod,gen(Src,unbounded)).
gen(Mod:Src,Count) :- number(Count),run_top(Mod,gen(Src,Count)).
gen(Mod:Src,Options) :- run_top(Mod,gen(Src,unbounded),Options).
gen(Mod:Src,Count,Options) :- run_top(Mod,gen(Src,Count),Options).

take(N,Mod:Src) :- gen(Mod:Src,N).
take(N,Mod:Src,Options) :- gen(Mod:Src,N,Options).

% expr_generator(+Expr,+Module,?Name,?Generator,?GeneratedTerm,?ControlArgs)
% Generator can be called repeatedly, binding GeneratedTerm,
% reflecting the intent of Expr. Name is the best guess for
% a name that might be used as file name or module name for
% buffering.
%
expr_generator(Expr,Mod,Name,Gen,T,Down) :-
	init_down(InitDown),
	catch(expr(Expr,InitDown,Mod,Name,Gen,T,Down),
	      Code,
	      throw('ExprSyntaxError'(Code))).

init_down(down(pl,0,unbounded,-1,[])).

down(format,down(Fmt,_,_,_,_),Fmt).    % output format, e.g. pl)
down(pos,down(_,Pos,_,_,_),Pos).       % position of next event in output stream
down(count,down(_,_,Count,_,_),Count). % Number of output lines requested
down(limit,down(_,_,_,Limit,_),Limit). % max events in output stream
down(peek,down(_,_,_,_,Peek),Peek).    % list of preceding values

% swap_down(?Key,?DownBefore,?BeforeVal,?DownAfter,?AfterVal)
%
swap_down(format,down(Fmt,Pos,Count,Limit,Peek),Fmt,down(New,Pos,Count,Limit,Peek),New).
swap_down(pos,down(Fmt,Pos,Count,Limit,Peek),Pos,down(Fmt,New,Count,Limit,Peek),New).
swap_down(count,down(Fmt,Pos,Count,Limit,Peek),Count,down(Fmt,Pos,New,Limit,Peek),New).
swap_down(limit,down(Fmt,Pos,Count,Limit,Peek),Limit,down(Fmt,Pos,Count,New,Peek),New).
swap_down(peek,down(Fmt,Pos,Count,Limit,Peek),Peek,down(Fmt,Pos,Count,Limit,New),New).


expr(Expr,Down,Mod,F,Gen,T,Down2) :-
	(expr1(Expr,Down,Mod,F,Gen,T,Down2)->
	    true;
	    throw('SubExprFailed'(Expr))).

% it does match but an exception is thrown
expr1(gen(Rule,Count),Down,Mod,Pred,
      xgen(Args,Pred,Arity,Down1,Mod,T),T,Down1) :-
	swap_down(count,Down,_,Down1,Count),
	functor(Rule,Pred,Arity),
	Rule =.. [Pred|Args],
	(defined_rule(Mod,Pred,Arity,_,_,_,_,_,_)->   	% Validate that a rule exists here, before actual execution
	    true;
	    throw('UnknownGeneratorRuleException'(Pred/Arity))).

:- dynamic gen_counter/2.

xgen(InArgs,Pred,Arity,Down,Mod,Term) :-
	down(limit,Down,Limit),
	down(peek,Down,Peek),
	defined_rule(Mod,Pred,Arity,InArgs,Limit,Index,Peek,Term,Invocation),
	statistics(walltime,[UniqueValue,_]),
	retractall(gen_counter(UniqueValue,_)),
	assert(gen_counter(UniqueValue,0)),  % TBD max/limit broken
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

output(dest(S,Fmt),Down,Mod,_,Gen,T) :- wgen_and_close(Gen,T,Mod,Fmt,S,Down).

wgen_and_close(Gen,T,Mod,Type,Stream,Down) :-
	(stream_property(Stream, file_name(Fn))->true;Fn=''),
	Key = '$LimitCounter',
	call_cleanup(wgen_repeat(Gen,T,Key,Mod,Type,Stream,Down),
		     (stream_property(Stream,interactive)->true;close(Stream))).

wgen_repeat(Gen,T,Key,Mod,Type,Stream,Down) :-
	bb_put(Key,0),
	down(count,Down,Count),
	repeat,
	(wgen(Gen,T,Key,Mod,Type,Stream,Down)->true;fail), % local cut just to be safe
	bb_get(Key,N),
	(number(Count)->N >= Count;true),
	!.

wgen(Gen,T,Key,Mod,Type,Stream,Down) :-
	down(limit,Down,Limit),
	down(count,Down,Count),
	(\+number(Count)->Bound=Limit;(Count>Limit->Bound=Count;Bound=Limit)),
	Gen,
	bb_get(Key,N),
	((Bound < 0 ; N < Bound)->
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

valid_functor_format(json).
valid_functor_format(pl).

format_type(pl,T,_Mod,Stream) :- writeq(Stream,T), write(Stream,'.'), nl.
format_type(json,T,_Mod,Stream) :- prolog_to_json(T,Json),json_write(Stream,Json,[width(0)]),nl.
