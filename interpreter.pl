%
% This module tries to interpret pdf ps code
%
:- module(interpreter, []).

interpret(Commands) :-
  initial_state(State0),
  interpret_(Commands, State0, State1),
  reverse(State1, State),
  format("~w~n", [State]).



initial_state([]).

interpret_([C | Rest], State0, State) :-
  interpret_command(C, State0, State1),
  !,
  interpret_(Rest, State1, State).

interpret_([], State, State).


interpret_command(tf(Name, Size), S0, [paragraph(Name, Size) | S0]).
interpret_command(td(X, Y), S0, [space(X, Y) | S0]).

interpret_command(tj(Args), S0, S) :-
  print(Args, S0, S).


maybe_space(X, S0, [space | S0]) :-
  X < -100.

maybe_space(X, S0, S0) :-
  X >= -100.

print_item(string(Codes), S0, [String | S0]) :-
  format(string(String), Codes, []).

print_item(N, S0, S) :-
  number(N),
  maybe_space(N, S0, S).

print([A | Args], S0, S) :-
  print_item(A, S0, S1),
  print(Args, S1, S).

print([], S, S).
