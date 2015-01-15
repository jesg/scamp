
% examples/complex_random_cases.pl

:- load_files('../prolog/percent.pl',[expand(true)]).
:- load_files('../prolog/evgen.pl',[expand(true)]).
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).

:- json_object
       valid_user(name:text, id:integer, email).

:- json_object
       email(user:text, domain:text).

valid_user(Name,Id,email(User,Domain)) :-
    percent(Name,[jason,bill,jill,dill]),
    percent(Id,1..100),
    percent(User,[naru:70,foobar,dar]),
    percent(Domain,[google,yahoo]).

dump_5_users :-
    take(5,valid_user,[format(json)]),
    halt.
