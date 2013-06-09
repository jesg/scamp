user:file_search_path(ruge,'$RUGE_HOME').

:- include(ruge(common)).

message(hello,email(groucho,yahoo)).
message(world,email(harpo,gmail)).
message(out,email(chico,hotmail)).
message(there,email(zeppo,aol)).

expose_term(email(N,Domain),"~a@~a.com",[N,Domain]).

