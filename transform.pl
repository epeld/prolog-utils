:- module(
     transform,
     [
       transform/2
     ]
   ).


transform(Commands, CommandsOut) :-
  reverse(Commands, Commands0),

  group_by_font(Commands0, CommandsOut0),
  maplist(identify_by_font, CommandsOut0, CommandsOut1),
  maplist(transform_element_once, CommandsOut1, CommandsOut).


%
% Transformation Rules
%
transform_element_once(A, B) :- once(transform_element(A,B)).

transform_element(trailer_footer(_Elements), removed).

transform_element(body(Elements), body(Out)) :-
  detect_new_paragaph(Elements, Elements1),
  join_strings(Elements1, Elements2),
  maplist(prettify, Elements2, Out).

transform_element(El, El).


%
% Identification Rules
%
identify_by_font(font(_Name, 9.963, _Pos)-Elements,
                 body(Elements)).

identify_by_font(font(_Name, 6.974, _Pos)-Elements,
                 trailer_footer(Elements)).


%
%
%
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


prettify(move(A,B), move(A,B)) :- !.

prettify(string(Codes), string(String)) :- !, format(string(String), Codes, []).


prettify(A, A) :- !.
prettify(A, Name) :-
  functor(A, Name, _).
