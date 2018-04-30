
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
      functor_parts("bar",[15,33])
    ), "bar(15,33)").

example:example_usage :-
  phrase(
    (
      functor_parts("foo",[prolog_atom("a"),3])
    ), "foo(a, 3)").

example:example_usage :-
  Foo = functor("foo",[prolog_atom("a"),3]),
  Bar = functor("bar", [15, 33]),
  phrase(
    clause(Foo, Bar),
    "foo(a, 3) :- bar(15, 33)."
  ).

example:example_usage :-
  Foo = functor("foo",[prolog_atom("a"),3]),
  Bar = functor("bar", [15, 33]),
  Baz = functor("baz", [prolog_atom("a"), prolog_atom("b")]),
  phrase(
    clause(Foo, (Bar, Baz)),
    "foo(a, 3) :- bar(15, 33), baz(a,b)."
  ).

example:example_usage :-
  Foo = functor("foo",[prolog_atom("a"),3]),
  Bar = functor("bar", [15, 33]),
  Baz = functor("baz", [prolog_atom("a"), prolog_atom("b")]),
  phrase(
    clause(Foo, (Bar; Baz)),
    "foo(a, 3) :- bar(15, 33); baz(a,b)."
  ).

example:example_usage :-
  Foo = functor("foo",[prolog_atom("a"),3]),
  Bar = functor("bar", [15, 33]),
  Baz = functor("baz", [prolog_atom("a"), prolog_atom("b")]),
  phrase(
    clause(Foo, (Bar, Baz, Baz)),
    "foo(a, 3) :- bar(15, 33), baz(a,b), baz(a,b)."
  ).

example:example_usage :-
  Foo = functor("foo",[prolog_atom("a"),3]),
  Baz = functor("baz", [prolog_atom("a"), prolog_atom("b")]),
  phrase(
    clause(Foo, ((3 = 3), Baz)),
    "foo(a, 3) :- 3 = 3, baz(a,b)."
  ).

%
% Code
%
clause(Head, Body) -->
  functor_whole(Head),
  optional_whitespace,
  ":-",
  optional_whitespace,
  clause_body(Body, 1200).

clause_body(Body, _Precedence) -->
  functor_whole(Body),
  optional_whitespace,
  ".".

clause_body(Compound) -->
  {
    functor(Compound, Op, 2),
    arg(1, Compound, F1),
    arg(2, Compound, F2)
  },
  functor_whole(F1),
  optional_whitespace,
  operator(Op),
  optional_whitespace,
  clause_body(F2).

operator(Op) -->
  {
    operator(OpC),
    atom_codes(Op, OpC)
  },
  OpC.

operator(",").
operator(";").
operator("|").
% operator("*->").
% operator("-").
% operator("=").

functor_whole(functor(Name, Args)) -->
  functor_parts(Name, Args).

functor_parts(Name,[]) -->
  prolog_atom(Name).

functor_parts(Name,Args) -->
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


