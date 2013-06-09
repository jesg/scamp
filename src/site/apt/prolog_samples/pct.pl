user:file_search_path(ruge,'$RUGE_HOME').

:- include(ruge(common)).

gen1 :-
	message(X),
	write(X),
	write(' '),
	fail.
gen1 :- nl.

75 pct message(hello).
25 pct message(world).


