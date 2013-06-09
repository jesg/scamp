
user:file_search_path(ruge,'$RUGE_HOME').

:- include(ruge(common)).

say::message(hello,email(groucho,yahoo)).
say::update(world,email(harpo,gmail)).
say::post(out,email(chico,hotmail)).
say::communique(there,email(zeppo,aol)).

expose_term(email(N,Domain),"~a@~a.com",[N,Domain]).

