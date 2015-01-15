##Scamp

Scamp is a fork of [Ruge](http://bmccarthy.bitbucket.org/ruge/).  Ruge is written by Brendon McCarthy.  Scamp is an effort make Ruge compatible with SWI-Prolog an open source prolog.

Scamp is a WIP.

###Install
Install SWI-Prolog 6 or 7.  Information about packages for Linux can be found [here](http://www.swi-prolog.org/build/LinuxDistro.html).

###Examples
Examples can be found in the examples directory.

```prolog

% examples/generate_messages.pl

:- load_files('../prolog/evgen.pl',[expand(true)]).
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).

:- json_object
       message(message:text).

message(hello).
message(world).

dump_messages :- gen(message),halt.
dump_first_message :- take(1,message),halt.
dump_messages_in_json :- gen(message,[format(json)]),halt.
```

To dump all messages run:
`swipl -f generate_messages -g dump_messages`
```
message(hello).
message(world).
```

To dump the first message run:
`swipl -f generate_messages -g dump_first_message`
```
message(hello).
```

To dump messages in json run:
`swipl -f generate_messages -g dump_messages_in_json`
```
{"message":"hello"}
{"message":"world"}
```
Scamp will output one json object per line.

###Percent
`percent/2` provides randomization at the goal level.  Valid expressions for `percent`.

| Expression | Result
|------------|-------
| [a,b,c] | Random a, b, or c
| [a:80,b:10,c:10] | 80% a else b or c
| [a:80,b,c] | Same as previous, the remaining 20% automatically allocated over b and c
| 8..70 | An integer between 8 and 70
| 8..avg(16)..70 | 	Same as above, but the average will be 16
| 1-jan-2013..3-feb-2013 | A date range
| 1-jan-2013..days(100) | A date range with relative upper bound
| :my_pred | A random value from user-defined my_pred/1
| set(2,[a,b,c]) | A list of length two, with unique elements a, b, or c
| set(1..3,[a,b,c]) | A random permutation of [a,b,c]

Generate some users:

```prolog

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
```

Run: `swipl -f complex_random_cases.pl -g dump_5_users`
```
{"name":"jill", "id":10, "email": {"user":"naru", "domain":"google"}}
{"name":"jason", "id":56, "email": {"user":"naru", "domain":"google"}}
{"name":"bill", "id":70, "email": {"user":"foobar", "domain":"yahoo"}}
{"name":"jason", "id":69, "email": {"user":"naru", "domain":"google"}}
{"name":"jill", "id":65, "email": {"user":"dar", "domain":"google"}}
```

###Major Differences with Ruge
Scamp seeks to be a library not a framework so it does not include the `::` operator or the `pct` operator.  Many of the predicates in Ruge's evgen module where removed due to a lack of unit tests.

Scamp is a prolog library so all the java source files where removed.

###License
Copyright 2013 DevClear

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
