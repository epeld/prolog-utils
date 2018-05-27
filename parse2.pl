:- module(parse2).
:- use_module(library(clpfd)).

:- set_prolog_flag(double_quotes, codes).


as(X) --> "a", (as_(X) ; []).

as_(X) -->
  spaces(X),
  as(X).


spaces(X, A, B) :-
  space_length(N0, X),
  length(Spaces, N0),
  append(Spaces, B, A),
  maplist(=(32), Spaces).


space_length(N, variable) :-
  space_length(N).

space_length(N, n(N)) :-
  space_length(N).

space_length(N) :-
  between(1, 10, N).


:- begin_tests(parse2).

:- set_prolog_flag(double_quotes, codes).

test(variable_spacing, nondet) :-
  phrase(as(variable), "a   a a      a").


test(as_1) :-
  phrase(as(variable), "a").

test(as_2, nondet) :-
  phrase(as(n(1)), "a a").

test(as_3, nondet) :-
  phrase(as(variable), "a a a").


test(spaces) :-
  phrase(spaces(n(1)), " ").

test(spaces_2) :-
  phrase(spaces(n(2)), "  ").

test(spaces_unknown, nondet) :-
  phrase(spaces(_N), "  ").


test(space_length, nondet) :-
  space_length(_N, variable).

test(space_length_1) :-
  space_length(_N, n(1)).

test(space_length_2, set(N = [variable, n(1)])) :-
  space_length(1, N).


:- end_tests(parse2).
