%
% This module tries to interpret pdf ps code
%
:- module(interpreter, []).

interpret([C | Rest]) :-
  interpret_command(C),
  !,
  interpret(Rest).

interpret([]).

interpret_command(tf(Name, Size)) :-
  format("~n").
interpret_command(td(X, Y)) :-
  Y =\= 0 *->
    format(" ")
  ;  format(" ").
interpret_command(tj(Args)) :-
  print(Args).


maybe_space(X) :-
  X < -100 *-> format(" ") ; true.

print_item(string(Codes)) :-
  format(Codes).

print_item(N) :-
  number(N),
  maybe_space(N).

print([A | Args]) :-
  print_item(A),
  print(Args).

print([]).
