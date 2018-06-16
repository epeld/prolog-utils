%
% This module provides predicates for parsing pdf ps code
%
:- module(code,
          []).
:- use_module(library(clpfd)).
:- use_module(pdf).


:- set_prolog_flag(double_quotes, codes).

code_block(Commands) -->
  "BT",
  code_commands(Commands),
  "ET".

code_command(Command) -->
  arguments(Args),
  pdf:optional_space,
  command_name(Name),
  {
    functor_args(Command, Name, Args)
  }.

functor_args(F, Name, Args) :-
  F=..[Name | Args].

command_name(AName) -->
  {
    %between(1,2, Length),
    Length = 2,
    length(Name, Length)
  },
  Name,
  {
    maplist(pdf:alpha, Name),
    atom_codes(AName0, Name),
    downcase_atom(AName0, AName)
  }.

argument(Arg) -->
  pdf:value(Arg).

arguments([Arg | Args]) -->
  argument(Arg),
  arguments_(Args, Arg).

arguments_([Arg | Args], Prev) -->
  special_whitespace(Prev),
  argument(Arg),
  arguments_(Args, Arg).

arguments_([], _) --> [].

special_whitespace(N), [Digit] -->
  {
    number(N) ; key(N)
  },
  pdf:whitespace,
  pdf:digit(Digit).

special_whitespace(Other) -->
  {
    not_number(Other)
  },
  pdf:optional_space.

key(key(_Codes)).

not_number(Other) :-
  \+ var(Other),
  \+ number(Other),
  \+ key(Other).

:- begin_tests(code).

:- set_prolog_flag(double_quotes, codes).

test(command_name) :-
  phrase(command_name(C), "Tf"),
  C = tf.

test(command_name2) :-
  phrase(command_name(C), "TJ"),
  C = tj.

test(command_name3) :-
  phrase(command_name(C), "Td"),
  C = td.

test(arguments0, all(C = [[211.283]])) :-
  phrase(arguments(C), "211.283").


test(arguments, nondet) :-
  phrase(arguments(C), "211.283 683.997"),
  C = [211.283, 683.997].

test(command, nondet) :-
  phrase(code_command(C), "211.283 683.997 Td"),
  C = td(211.283, 683.997).

test(command2, all(C = [tf(key("F30"), 9.963)])) :-
  phrase(code_command(C), "/F30 9.963 Tf").

:- end_tests(code).
