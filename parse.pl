
:- module(pparse, []).
:- use_module(example).
:- use_module(library(dcg/basics)).

:- set_prolog_flag(double_quotes, codes).

%
% Tests
%
example:example_usage :-
  phrase(prolog_atom("foobar"), "foobar").

example:example_usage :-
  phrase((
	    prolog_atom("foo"),
	    char(32, _T),
	    prolog_atom("bar")
	  ),
	 "foo bar").

example:example_usage :-
  [Char] = "f",
  phrase(char(Char, csymf), "f").

example:example_usage :-
  phrase(
    (
      prolog_atom("foo"),
      "()"
    ), "foo()").

example:example_usage :-
  \+ phrase(
    (
      prolog_atom("foo()")
    ), "foo()").

example:example_usage :-
  phrase(
    (
      functor("bar",[15,33])
    ), "bar(15,33)").

example:example_usage :-
  phrase(
    (
      functor("foo",[prolog_atom("a"),3])
    ), "foo(a, 3)").

example:example_usage :-
  phrase(
    clause(functor("foo", [prolog_atom("a"), 3]), functor("bar", [15, 33])),
    "foo(a, 3) :- bar(15, 33)."
  ).

%
% Code
%
clause(Head, Body) -->
  functor(Head),
  optional_whitespace,
  ":-",
  optional_whitespace,
  functor(Body),
  optional_whitespace,
  ".".

functor(Name,[]) -->
  prolog_atom(Name).

functor(Name,Args) -->
  prolog_atom(Name),
  "(",
  functor_args(Args),
  ")".


functor_args([]) -->
  optional_whitespace.

functor_args([Arg | Args]) -->
  optional_whitespace,
  prolog_term(Arg),
  functor_args1(Args).

functor_args1([]) --> [].

functor_args1([Arg | Args]) -->
  ",",
  optional_whitespace,
  prolog_term(Arg),
  optional_whitespace,
  functor_args1(Args).


optional_whitespace -->
  char(_C, space),
  optional_whitespace.

optional_whitespace -->
  [].


prolog_term(prolog_atom(Term)) -->
  prolog_atom(Term).

prolog_term(Integer) -->
  {
    number(Integer) ; var(Integer)
  },
  integer(Integer).


prolog_atom([C]) -->
  char(C, csymf).

prolog_atom([C | Rest]) -->
  char(C, csymf),
  prolog_atom(Rest).


char(C, Type, [C | Rest], Rest) :-
  char_type(C, Type).


