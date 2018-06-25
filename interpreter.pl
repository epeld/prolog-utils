%
% This module tries to interpret pdf ps code
%
:- module(interpreter, []).

interpret(Commands, Stream) :-
  initial_state(State0),
  !,
  interpret_(Commands, State0, State1),
  !,
  format("Done interpreting~n"),
  reverse(State1, State2),
  remove_header(State2, State3),
  detect_new_paragaph(State3, State4_0),
  join_strings(State4_0, State4),
  maplist(atomify, State4, State),
  format(Stream, "~w~n", [State]).

join_strings([string(A), string(B) | Rest0], Rest) :-
  !,
  append(A,B,AB),
  join_strings([string(AB) | Rest0], Rest).

join_strings([A | Rest0], [A | Rest]) :-
  join_strings(Rest0, Rest).

join_strings([], []).

remove_header([font(_A, Size, _C), string(_Blabla) | State], State) :-
  very_close(Size, 6.974).

detect_new_paragaph([move(X, Y) | Rest0], Rest) :-
  very_close(X, 11.956),
  very_close(Y, -11.956),
  !,
  detect_new_paragaph([new_paragraph | Rest0], Rest).

detect_new_paragaph([new_paragraph, string(Something), move(X, Y) | Rest0],
                    [new_paragraph, string(Something) | Rest]) :-
  very_close(X, -11.956),
  very_close(Y, -11.956),
  !,
  detect_new_paragaph(Rest0, Rest).

detect_new_paragaph([move(0, Y) | Rest0],
                    Rest) :-
  very_close(Y, -11.956),
  !,
  detect_new_paragaph(Rest0, Rest).


detect_new_paragaph([A | Rest0], [A | Rest]) :-
  detect_new_paragaph(Rest0, Rest).

detect_new_paragaph([], []).


very_close(A, B) :-
  Diff is A - B,
  Diff >= 0,
  Diff < 0.1.

very_close_abs(A, B) :-
  very_close(A, B) ; very_close(B, A).


atomify(move(A,B), move(A,B)) :- !.

atomify(string(Codes), string(String)) :- !, format(string(String), Codes, []).

atomify(font(key(Codes), S, _1), font(String, S)) :- !, format(string(String), Codes, []).


atomify(A, A) :- !.
atomify(A, Name) :-
  functor(A, Name, _).


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
