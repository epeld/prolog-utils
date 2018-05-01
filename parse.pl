:- module(pparse, []).
:- use_module(example).
:- use_module(library(dcg/basics)).

:- set_prolog_flag(double_quotes, codes).

%
% Tests
%
example:example_usage :-
  phrase(prolog_atom("foobar"), "foobar").

example:example_usage(first_test) :-
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
      functor_parts("foo",[functor("a", []),3])
    ), "foo(a, 3)").

example:example_usage :-
  Foo = functor("foo",[functor("a", []),3]),
  Bar = functor("bar", [15, 33]),
  phrase(
    clause(Foo, Bar),
    "foo(a, 3) :- bar(15, 33)."
  ).

example:example_usage :-
  Foo = functor("foo",[functor("a", []),3]),
  %% Bar = functor("bar", [15, 33]),
  %% Baz = functor("baz", [functor("a", []), functor("b", [])]),
  phrase(
    clause(Foo, _Body),
    "foo(a, 3) :- bar(15, 33), baz(a,b)."
  ).

example:example_usage :-
  % Foo = functor("foo",[prolog_atom("a"),3]),
  phrase(
    clause(_Foo, _Body),
    "foo(a, 3) :- bar(15, 33); baz(a,b)."
  ).

example:example_usage(foobar, [exclusive(false)]) :-
  % Foo = functor("foo",[prolog_atom("a"),3]),
  phrase(
    clause(_Head, _Body),
    "foo(a, 3) :- bar(15, 33), baz(a,b), baz(a,b)."
  ).

example:example_usage(eq_body, [exclusive(false)]) :-
  Foo = functor("foo",[functor("a", []),3]),
  phrase(
    clause(Foo, _Body),
    "foo(a, 3) :- 3 = 3, baz(a,b)."
  ).

example:example_usage(eq, [exclusive(false)]) :-
  phrase(
    operator_expression(_Exp, _P),
    "3 = 3"
  ).

example:example_usage(eq2, [exclusive(false)]) :-
  phrase(
    operator_expression(_Exp, _P),
    "3 = 3, 4 = 3"
  ).

example:example_usage(comma_eq_body, [exclusive(false)]) :-
  phrase(
    operator_expression(_Exp, _P),
    "3 = 3, baz(2, 5)"
  ).

example:example_usage(foo, [exclusive(false)]) :-
  phrase(
    clause_body(_Body),
    "a, b"
  ).

example:example_usage(unary_op, [exclusive(false)]) :-
  phrase(
    clause_body(_Body),
    ":- do_something"
  ).

%
% Code
%
clause(Head, Body) -->
  clause_body(functor(":-", Head, Body)),
  ".".

clause_body(Body) --> clause_body(Body, _P).

clause_body(Body, 0) -->
  functor_whole(Body),
  optional_whitespace.

clause_body(Body, Precedence) -->
  operator_expression(Body, Precedence).

operator_expression(functor(Op, F1), Precedence) -->
  unary_operator(Op, _Type, Precedence, RightPrecedence),
  optional_whitespace,
  operand(F1, Precedence0),
  {
    Precedence0 =< RightPrecedence
  }.

operator_expression(functor(Op, F1, F2), Precedence) -->
  prolog_term(Term1),
  optional_whitespace,
  operator_lhs(Term1, F1, 0, LeftPrecedence),
  operator_rhs(Op, F2, LeftPrecedence, Precedence).

operator_lhs(Term, Term, Precedence, Precedence) --> [].
operator_lhs(Term, functor(Op, Term, Rhs), LeftPrecedence, Precedence) -->
  operator_rhs(Op, Rhs, LeftPrecedence, Precedence).

operator_rhs(Op, Right, LeftPrecedence, Precedence) -->
  {
    binary_operator(Op, Type, Precedence),
    left_precedence(Type, Precedence, LeftPrecedence0),
    LeftPrecedence =< LeftPrecedence0,
    right_precedence(Type, Precedence, RightPrecedence)
  },
  Op,
  optional_whitespace,
  operand(Right, P),
  {
    P =< RightPrecedence
  }.

operand(Operand, Precedence) -->
  clause_body(Operand, Precedence).

operand(Operand, 0) -->
  prolog_term(Operand).


unary_operator(Op, fx, Precedence, RightPrecedence) :-
  operator(Op, fx, Precedence),
  succ(RightPrecedence, Precedence).

unary_operator(Op, fy, Precedence, Precedence) :-
  operator(Op, fy, Precedence).

unary_operator(Op, Type, P, RP) -->
  { unary_operator(Op, Type, P, RP) }, Op.


binary_operator(Op, Type, Precedence) -->
  {
    binary_operator(Op, Type, Precedence)
  }, Op.

binary_operator(Op, Type, Precedence) :-
  operator(Op, Type, Precedence),
  binary_operator_type(Type).

binary_operator_type(xfx).
binary_operator_type(xfy).
binary_operator_type(yfx).


left_precedence(xfx, Precedence, LeftPrecedence) :-
  succ(LeftPrecedence, Precedence).

left_precedence(xfy, Precedence, LeftPrecedence) :-
  succ(LeftPrecedence, Precedence).

left_precedence(yfx, Precedence, Precedence).


right_precedence(xfx, Precedence, RightPrecedence) :-
  succ(RightPrecedence, Precedence).

right_precedence(yfx, Precedence, RightPrecedence) :-
  succ(RightPrecedence, Precedence).

right_precedence(xfy, Precedence, Precedence).


operator("-->", xfx, 1200).
operator(":-", xfx, 1200).
operator(":-", fx, 1200).
operator("?-", fx, 1200).
% operator("1150", fx, 1200).
operator(";", xfy, 1100).
operator("|", xfy, 1100).
operator("*->", xfy, 1050).
operator("->", xfy, 1050).
operator(",", xfy, 1000).
operator(":=", xfx, 990).
operator("\\+", fy, 900).
operator(Op, xfx, 700) :-
  member(Op, ["<", "=", "=..", "=@=", "\\=@=", "=:=", "=<", "==", "=\\=", ">",
	      ">=", "@<", "@=<", "@>", "@>=", "\\=", "\\==", ">:<", ":<"]).
operator(":", xfy, 600).
operator("+", yxf, 500).
operator("-", yxf, 500).
operator("/\\", yxf, 500).
operator("\\/", yxf, 500).
%operator("xor", yxf, 500).
operator("?", fx, 500).
operator("*", yfx, 400).
operator("/", yfx, 400).
operator("//", yfx, 400).
operator("<<", yfx, 400).
operator(">>", yfx, 400).
operator("**", xfx, 200).
operator("^", xfy, 200).
operator("+", fy, 200).
operator("-", fy, 200).
operator("\\", fy, 200).
operator(".", yfx, 100).
operator("$", fx, 1).


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


prolog_term(Functor) -->
  functor_whole(Functor).

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


