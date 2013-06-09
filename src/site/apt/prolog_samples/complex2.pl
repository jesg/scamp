
user:file_search_path(ruge,'$RUGE_HOME').

:- include(ruge(common)).

say::R :- valid_say(R).

10 pct valid_say(message(hello,email(groucho,yahoo))).
10 pct valid_say(update(world,email(harpo,gmail))).
70 pct valid_say(post(out,email(chico,hotmail))).
10 pct valid_say(communique(there,email(zeppo,aol))).

expose_term(email(N,Domain),"~a@~a.com",[N,Domain]).

