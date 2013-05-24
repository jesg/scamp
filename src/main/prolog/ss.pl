
file_search_path(ruge,'.').
:- include(ruge(common)).

:- save_program('ruge',
		(prolog_flag(argv,V),
		  format("Loaded ~a~n",[V]))).

:- halt.

%:- use_module(library(system)).

%:- save_program('ruge',
%		(E = 'RUGEPG',
%		    (environ(E,V) ->
%			consult(V);
%			format("No '~a' set, try this:~n  'sictus -r ruge -D~a=myPrologFile'~n~n",[E,E])))).

