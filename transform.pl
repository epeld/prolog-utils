:- module(
     transform,
     [
       transform/2
     ]
   ).


transform(Commands, Commands) :-
  false, 
  !.


transform(Commands, CommandsOut) :-
  State1 = Commands,
  reverse(State1, State2),
  remove_header(State2, State3),
  detect_new_paragaph(State3, State4_0),
  join_strings(State4_0, State4),
  maplist(atomify, State4, State),
  State = CommandsOut.



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


atomify(A, A) :- !.
atomify(A, Name) :-
  functor(A, Name, _).
