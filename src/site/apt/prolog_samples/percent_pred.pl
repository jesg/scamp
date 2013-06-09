user:file_search_path(ruge,'$RUGE_HOME').

:- include(ruge(common)).

gen1 :-
	percent(X,:message(_)),
	write(X),
	write(' '),
	fail.
gen1 :- nl.

message(hello).
message(world).

