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
:- module(typer,['=>'/2,xassert/3]).
:- use_module(library(between)).
:- use_module(library(random)).

%:- op(750,xfy,'::').
:- op(1050,xfy,'::').
:- op(500,xfy,'=>').

:-set_prolog_flag(discontiguous_warnings,off).


interface_expansion(Term,Meta) :- 
	Term = (Ifc :: P),
	nonvar(Ifc),
	functor(Ifc,interface,_),  % Future: other interface attributes
	functor(P,F,Arity),
	findall(M,(between(1,Arity,Pos),expand_interface(Arity,Pos,P,F,M)),FieldMeta),
	(member('$gfd'(_,_,Arity,KeyPos,_,_,_,_,0,_,_,_),FieldMeta)->true;KeyPos=1),
	findall(Acc,(member(Fm,FieldMeta),expand_accessor(Fm,Arity,KeyPos,Acc)),As),
	findall(D,(member(Fm,FieldMeta),expand_dex(Fm,D)),Ds),	
	append(FieldMeta,As,M1),
	append(M1,Ds,Meta).

expand_interface(Arity,Pos,P,EncType,Term) :-
	Term = '$gfd'(Name,EncType,Arity,Pos,FdType,Inverse,Min,Max,KeyIndex,Countx,Index,Ordex),
	arg(Pos,P,Fd),
	(Fd = (Head:Tail)->
	    true;
	    (Fd=Head,Tail=string)),
	Head =.. [Name|Qualifiers],
	(member(optional,Qualifiers)->Min=0;Min=1),
	(member(many,Qualifiers)->Max=(-1);Max=1),
	(member(key,Qualifiers)->KeyIndex=0;KeyIndex=(-1)),
	name(Pos,Pc),
	atom_codes(PosAtom,Pc),
	atom_concat(EncType,PosAtom,Sfx),
	((member(index,Qualifiers);member(ordex,Qualifiers))->atom_concat('$gindex_',Sfx,Index);Index=nox),
	(member(ordex,Qualifiers)->atom_concat('$gordex_',Sfx,Ordex);Ordex=nox),
	((Index==nox;Ordex==nox)->Countx=nox;atom_concat('$gcount_',Sfx,Countx)),
	((Tail = (FdType=>Inverse))->
	    true;
	    (Tail=FdType,Inverse='')).

expand_accessor(Term,Arity,KeyPos,Acc) :-
	Term = '$gfd'(Name,EncType,Arity,Pos,_FdType,_Inverse,_Min,_Max,_KeyIndex,_Countx,Index,_Ordex),
	Acc = ('$gax'(Name,KeyShort,Value):-AccBody),
	KeyShort =.. [EncType,Key],
	length(Args,Arity),
	Target =.. [EncType|Args],
	RefLookup =.. [Index,Value,Ref],
	(Index==nox->
	    AccBody = Target;
	    AccBody = ((var(Key),nonvar(Value))->(clause(RefLookup,true),clause(Target,_,Ref)); Target)),
	arg(KeyPos,Target,Key),
	arg(Pos,Target,Value).

% mod accessor
% add rand accessor
% add counter if needed
% fixed assertx

expand_dex(_,[]).
%expand_dex(Term,[(:-dynamic Countx/1),(:- dynamic Index/2),(:- dynamic Ordex/2)]) :-
%expand_dex(Term,[(:- clause(Ix,true))]) :-
%	Term = '$gfd'(_Name,_EncType,_Arity,_Pos,_FdType,_Inverse,_Min,_Max,_KeyIndex,Countx,Index,Ordex),
%	Cx =.. [Countx,_], clause(Cx,true),
%	Ix =.. [Index,_,_], clause(Ix,true),
%	Ox =.. [Ordex,_,_], clause(Ox,true).

% TBD, can expand => inline, in addition to having as pred

(K => F) :-
	current_predicate('$gax',Mod:_),  % TBD better/simpler way than subverting module system?
	(var(F)->
	    (Mod:'$gax'(Fn,K,V), F=..[Fn,V]);
	    (functor(F,Fn,_),
		arg(1,F,V),
		(arg(2,F,any)->
		    random_select(Mod,Fn,K,V);
		    true),
		Mod:'$gax'(Fn,K,V))).
(K => (F1,F2)) :-  % TBD if var(F) then forever, but does it matter?
	(K => F1),
	(K => F2).

random_select(Mod,Fn,K,V) :-
	(var(K)->true;functor(K,EncType,_)),
	Mod:'$gfd'(Fn,EncType,_Arity,_Pos,_FdType,_Inverse,_Min,_Max,_KeyIndex,Countx,_Index,Ordex),
	(Mod:bb_get(Countx,RowCount)->true;RowCount=0),
%	Cp =.. [Countx,RowCount],
%	Mod:Cp,
	random(0,RowCount,Rx),
	Ox =.. [Ordex,Rx,V],
	Mod:Ox.

% cidb:'$gax'(sor, event(A), B) :-  % for indexed fields not not pos 1
%        ((var(A),nonvar(B))->
%          ('$gindex_event2'(B,Ref),
%           clause(cidb:event(A, B, _, _, _, _, _, _, _, _),_,Ref));
%        cidb:event(A, B, _, _, _, _, _, _, _, _).


% '$gcount_foo3'(287).   % only one
% '$gindex_foo3'(joe,'$ref'(98765)). % index: one per event
% '$gordex_foo3'(117,joe).  % ordex: one per unique value

%bump(Mod,F,Count) :- 
%	(Countp =.. [F,Count],
%	    (Mod:retract(Countp)->
%		true;
%		Count=0),
%	    NextCount is Count+1,
%	    NextCountp =.. [F,NextCount],
%	    Mod:assert(NextCountp)).
bump(Mod,F,Count) :-
	(Mod:bb_get(F,Count)->true;Count=0),
	NextCount is Count+1,
	Mod:bb_put(F,NextCount).


:- meta_predicate xassert(2).	
xassert(DefMod,AssertMod,Term) :-
	assert(AssertMod:Term,Ref),
	functor(Term,F,A),
	((for(Pos,1,A), param([DefMod,AssertMod,Term,F,Ref]) do update_indexes(DefMod,AssertMod,Term,Pos,F,Ref))->
	    true;
	    % An unexpect failure of udpate_indexes would silently propagate
	    % a failure to the caller. Make sure that doesn't happen.
	    throw(xassert_failed(Term))).

% TBD dynamic

update_indexes(DefMod,AssertMod,Term,Pos,EncType,Ref) :-
	(DefMod:'$gfd'(_FdName,EncType,_Arity,Pos,_FdType,_Inverse,_Min,_Max,_KeyIndex,Countx,Index,Ordex)->
	    updx(AssertMod,Term,Pos,Countx,Index,Ordex,Ref);
	    true).

updx(Mod,Term,Pos,Countx,Index,Ordex,Ref) :-
	arg(Pos,Term,V),
	(V==''->   % TBD/TBR (a) '' always = null? (b) should include here anyway?
	    true;  
	    (Ordex=nox->  
		true;
		% If any index exists already, then a unique ordex would already 
		% have been created.
		(Xe =.. [Index,V,_],
		    (Mod:clause(Xe,true)->
			true;
			(bump(Mod,Countx,NumberOfOrdex),
			    (Oh =.. [Ordex,NumberOfOrdex,V],
				Mod:assert(Oh)))))),
	    (Index=nox->
		true;
		(Ih =.. [Index,V,Ref],
		    Mod:assert(Ih)))).

%	    (bump(Mod,Countx,NumberOfOrdex),
%		(Oh =.. [Ordex,NumberOfOrdex,V],
%		    Mod:assert(Oh)))).


	    
		
:- multifile user:term_expansion/6.
user:term_expansion(Term,Layout,Ids,Meta,Layout,[Token|Ids]) :-
	Token=gen_interface,
	nonmember(Token,Ids),
	interface_expansion(Term,Meta).


