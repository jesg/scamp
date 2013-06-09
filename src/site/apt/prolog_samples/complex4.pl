
user:file_search_path(ruge,'$RUGE_HOME').

:- include(ruge(common)).

say(N)::R :-
	between(1,N,_),
	valid_say(R).

valid_say(message(Word,email(User,Domain))) :-
	percent(Word,[hello,world,out,there,you]),
	percent(User,[groucho,harpo,chico,zeppo]),
	percent(Domain,[yahoo,gmail,hotmail,aol]).

expose_term(email(N,Domain),"~a@~a.com",[N,Domain]).

