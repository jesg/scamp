
% examples/generate_messages.pl

:- load_files('../prolog/evgen.pl',[expand(true)]).
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).

:- json_object
       message(message:text).

message(hello).
message(world).

dump_messages :-         gen(message),halt.
dump_first_message :-    take(1,message),halt.
dump_messages_in_json :- gen(message,[format(json)]),halt.
