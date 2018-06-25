%
% This module tries to interpret pdf ps code
%
:- module(interpreter, []).
:- use_module(transform, [transform/2]).

interpret(Commands, Stream) :-
  initial_state(State0),
  !,
  interpret_(Commands, State0, State1),
  !,
  format("Done interpreting~n"),
  transform(State1, State),
  format("Writing to file~n"),
  format(Stream, "~w~n", [State]),
  format("Done~n").



initial_state([]).

interpret_([C | Rest], State0, State) :-
  interpret_command(C, State0, State1),
  !,
  interpret_(Rest, State1, State).

interpret_([], State, State).


interpret_command(tf(Name, Size), S0, [font(Name, Size, default) | S0]).

interpret_command(td(X, Y), [ Item | S0], [move(X, Y), Item | S0]) :- 
  \+ Item = font(_A, _B, _C).

interpret_command(td(X, Y), [font(A, B, _) | S0], [font(A, B, topleft(X, Y)) | S0]).

interpret_command(tj(Args), S0, S) :-
  print(Args, S0, S).


maybe_space(X, S0, S) :-
  X < -100,
  print_item(string([32]), S0, S).

maybe_space(X, S0, S0) :-
  X >= -100.


print_item(String, [Item | S0], [String, Item | S0]) :-
  Item \= string(_),
  String = string(_Codes).

print_item(string(Codes), [string(Other) | S0], [string(Joined) | S0]) :-
  append(Other, Codes, Joined).

print_item(N, S0, S) :-
  number(N),
  maybe_space(N, S0, S).


print([A | Args], S0, S) :-
  print_item(A, S0, S1),
  print(Args, S1, S).

print([], S, S).
