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
:- module(utils,[time/2, ftime/2, ftime/3, saytime/1, say_time_briefly/1, format_time/2, days_between_dates/3, idate/2, month/3, date_plus/3, repeat_char/2, working_sub_term/2]).
:- use_module(library(codesio)).
:- use_module(library(between)).
:- use_module(library(system)).
:- use_module(library(lists)).

:- meta_predicate time(2,?).
:- meta_predicate ftime(2,?).
:- meta_predicate ftime(2,?,?).


time(Mod:Term,WallTimeSeconds) :-
	statistics(walltime,[T1,_]),
	Mod:Term,
	statistics(walltime,[T2,_]),
	WallTimeSeconds is (T2 - T1) / 1000.

:- meta_predicate saytime(2).
:- meta_predicate say_time_briefly(2).

saytime(Term) :-
	ftime(Term,WallTime),
	format("Term ~p executed in ~s (walltime)~n",[Term,WallTime]).

say_time_briefly(Term) :-
	ftime(Term,WallTime),
	Term = Mod:T,
	functor(T,F,A),
	format("Term ~a:~a/~d executed in ~s (walltime)~n",[Mod,F,A,WallTime]).

ftime(Term,WallTime) :- ftime(Term,WallTime,_).
ftime(Term,WallTime,Sec) :-
	time(Term,Sec),
	format_time(Sec,WallTime),
	!.  % efficiency

% format_time(+Seconds,?FullFormatString)
format_time(Sec,WallTime) :-
	Seci is integer(Sec),
	Min is Seci // 60,
	MinSec is Seci rem 60,
	Hrs is Min // 60,
	HrsMin is Min rem 60,
	plural(Hrs,"hour",HrsTerm),
	plural(HrsMin,"minute",MinTerm),
	plural(MinSec,"second",SecTerm),	
	once(fmt_mtime(Hrs,HrsMin,MinSec,Sec,HrsTerm,MinTerm,SecTerm,WallTime)).

fmt_mtime(0,0,_Sec,Original,_,_,_SecTerm,Time) :- format_to_codes("~h seconds",[Original],Time).
fmt_mtime(0,Min,Sec,_,_,MinTerm,SecTerm,Time) :- format_to_codes("~d ~s and ~d ~s",[Min,MinTerm,Sec,SecTerm],Time).
fmt_mtime(Hrs,Min,Sec,_,HrsTerm,MinTerm,SecTerm,Time) :- format_to_codes("~d ~s, ~d ~s, and ~d ~s",[Hrs,HrsTerm,Min,MinTerm,Sec,SecTerm],Time).

plural(1,X,X).
plural(N,X,Y) :- N =\= 1, append(X,"s",Y).
	

%repeat_char(N,Char) :- (for(_,1,N) do put_code(Char)).
repeat_char(N,Char) :- (N=<0->true;(put_code(Char),N1 is N-1,repeat_char(N1,Char))).

%gen_directory("generated/").
%data_module("data:").
%
%:- meta_predicate gen_file(2,+,+).


%date_within(Date,Start,End) :-
%	% ensure year is bound
%	Start = _-_-MinYear,
%	End = _-_-MaxYear,
%	Date = _-_-Year,
%	between(MinYear,MaxYear,Year),
%	idate(Date,Du),
%	idate(Start,Su),
%	Du >= Su,
%	idate(End,Eu),
%	Du =< Eu.

%date_plus_days(Date,Days,Date1) :-
%	Date1 is Date + Days * 86400. % 60 * 60 * 24.

date_plus(Date,Plus,Date1) :-
	date_operand_to_seconds(Plus,Ps),
	Date1 is Date + Ps.

date_operand_to_seconds(hour(Hour),S) :- date_operand_to_seconds(hours(Hour),S).
date_operand_to_seconds(hours(Hours),S) :- S is Hours * 3600.
date_operand_to_seconds(day(Day),S) :- date_operand_to_seconds(days(Day),S).
date_operand_to_seconds(days(Days),S) :- S is Days * 86400. % 60 * 60 * 24.
date_operand_to_seconds(week(Week),S) :- date_operand_to_seconds(weeks(Week),S).
date_operand_to_seconds(weeks(Weeks),S) :- S is Weeks * 604800.  % 7 * 60 * 60 * 24.
	
%date_plus_days(Day-Month-Year,Days,PlusDay-PlusMonth-PlusYear) :-
%	month(Month,Mx,Max),
%	between(1,Max,Day),
%	datime(Time,datime(Year,Mx,Day,1,1,1)),
%	Plus is Time + Days * 60 * 60 * 24,
%	datime(Plus,datime(PlusYear,Pm,PlusDay,_,_,_)),
%	month(PlusMonth,Pm,_).

% TBD leap years!
%date_to_days(Day-Month-Year,Days) :-
%	integer(Days),
%	!,
%	Year is Days // 365,
%	Left is Days rem 365,
%	Mn is Left // 12,
%	month(Month,Mn,_),
%	Day is Mn rem 12.

% TBD go thru datime
date_to_days(Day-Month-Year,Days) :-
	DaysFromYear is Year * 365,
	index_days_accumulating_in_year(Month,_,DaysFromMonth),
	Days is DaysFromYear + DaysFromMonth + Day.

%date_to_days(Day-Month-Year,Days) :-
%	month(Month,Mx,Max),
%	between(1,Max,Day),
%	datime(Time,datime(Year,Month,Day,1,1,1)),
%	Time is Days * 60 * 60 * 24,

days_between_dates(Date1,Date2,DiffInDays) :-
	date_to_days(Date1,Days1),
	date_to_days(Date2,Days2),
	DiffInDays is Days1 - Days2.
	
idate(Day-Month-Yr,Date) :-
	(var(Date)->(D1=1,D2=1,D3=1);(D1=_,D2=_,D3=_)),
	(integer(Month)->Month=Mx;month(Month,Mx,_)),
	datime(Date,datime(Yr,Mx,Day,D1,D2,D3)).


month(jan,1,31).
month(feb,2,28).
month(mar,3,31).
month(apr,4,30).
month(may,5,31).
month(jun,6,30).
month(jul,7,31).
month(aug,8,31).
month(sep,9,30).
month(oct,10,31).
month(nov,11,30).
month(dec,12,31).

index_date_days :-
	month(Month,Mx,Total),
	Mp is Mx-1,
	(Mp<1->T=0;index_days_accumulating_in_year(_Prev,Mp,T)),
	T1 is Total + T,
	assert(index_days_accumulating_in_year(Month,Mx,T1)),
	fail.
index_date_days.

:- index_date_days.

user:portray(ufs(T)) :- is_list(T) -> format("\"~s\"",[T]) ; write(T).


% Standard library(terms) sub_term barfs on things like gen(1k)
%
working_sub_term(T,T).
working_sub_term(Sub,T) :-
	T =.. [_|Args],
	member(Arg,Args),
	working_sub_term(Sub,Arg).
	

