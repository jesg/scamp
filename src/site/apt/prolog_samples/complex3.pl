
user:file_search_path(ruge,'$RUGE_HOME').

:- include(ruge(common)).

10 pct say::message(hello,email(groucho,yahoo)).
10 pct say::update(world,email(harpo,gmail)).
70 pct say::post(out,email(chico,hotmail)).
10 pct say::communique(there,email(zeppo,aol)).

expose_term(email(N,Domain),"~a@~a.com",[N,Domain]).

