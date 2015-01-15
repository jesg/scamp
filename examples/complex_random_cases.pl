
% examples/complex_random_cases.pl

:- load_files('../prolog/percent.pl',[expand(true)]).
:- load_files('../prolog/evgen.pl',[expand(true)]).
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).

:- json_object
       valid_user(name:text, email).

:- json_object
       email(user:text, domain:text).

valid_user(Name,email(User,Domain)) :-
    percent(Name,[jason,bill,jill,dill]),
    percent(User,[naru,foobar,dar]),
    percent(Domain,[google,yahoo]).

dump_5_users :-
    take(5,valid_user,[format(json)]),
    halt.
