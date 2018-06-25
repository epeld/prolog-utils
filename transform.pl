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
  reverse(State1, State2_0),

  State2_0 = State3,

  % only body needs paragraph detection
  detect_new_paragaph(State3, State4_0),
  join_strings(State4_0, State4),

  % only body really
  maplist(atomify, State4, State),

  group_by_font(State, CommandsOut0),
  maplist(identify_by_font, CommandsOut0, CommandsOut).


identify_by_font(font(_Name, 9.963, _Pos)-Elements,
                 body(Elements)).

identify_by_font(font(_Name, 6.974, _Pos)-Elements,
                 trailer_footer(Elements)).


group_by_font([], []).
group_by_font([font(A,B,C) | Rest], [font(A,B,C)-Items | Out]) :-
  non_font_items(Rest, Items, Tail),
  group_by_font(Tail, Out).


non_font_items([], [], []).
non_font_items([Item | Items], [Item | NonFontItems], Rest) :-
  not_font(Item),
  !,
  non_font_items(Items, NonFontItems, Rest).

non_font_items([font(A, B,C) | Items], [], [font(A, B,C) | Items]).

not_font(Item) :-
  Item \= font(_A, _B, _C).


join_strings([string(A), string(B) | Rest0], Rest) :-
  !,
  append(A,B,AB),
  join_strings([string(AB) | Rest0], Rest).

join_strings([A | Rest0], [A | Rest]) :-
  join_strings(Rest0, Rest).

join_strings([], []).


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
