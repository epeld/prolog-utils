:- module(bar, []).
:- use_module(example).

bar(111, 333).

example:example_usage :-
  bar(112, X),
  X = 333.
