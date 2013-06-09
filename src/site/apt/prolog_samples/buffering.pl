
user:file_search_path(ruge,'$RUGE_HOME').

:- include(ruge(common)).

:- dynamic message/2.

say::message(X,Count1) :-
	between(1,9,_),
	percent(X,[blue,green,red]),
	findall(X,message(X,_),Cs),
	length(Cs,Count),
	Count1 is Count+1.




