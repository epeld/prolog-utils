%
% This module provides predicates for parsing pdf ps code
%
:- module(code,
          []).
:- use_module(library(clpfd)).
:- use_module(pdf).


:- set_prolog_flag(double_quotes, codes).

code_block(Commands) -->
  "BT", pdf:whitespace,
  code_commands(Commands),
  pdf:whitespace,
  "ET".

code_commands([ C | Commands]) -->
  code_command(C),
  code_commands_1(Commands).

code_commands([]) --> [].

code_commands_1([ C | Commands]) -->
  pdf:optional_whitespace,
  code_command(C),
  code_commands_1(Commands).

code_commands_1([]) --> [].


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
  (
    pdf:digit(Digit)
  ; ("-", { [Digit] = "-" }
  )).

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

test(arguments2, all(C = [[[string("HO"), -498]]])) :-
  phrase(arguments(C),
         "[(HO)-498]"). % TODO whitespace fixes the problem..

test(arguments3, all(C = [[[string("HO")]]])) :-
  phrase(arguments(C),
         "[(HO)]").

test(arguments4, all(C = [[string("HO")]])) :-
  phrase(arguments(C),
         "(HO)").

test(arguments5, all(C = [[3]])) :-
  phrase(arguments(C),
         "3").

test(command, nondet) :-
  phrase(code_command(C), "211.283 683.997 Td"),
  C = td(211.283, 683.997).

test(command2, all(C = [tf(key("F30"), 9.963)])) :-
  phrase(code_command(C), "/F30 9.963 Tf").

test(command3, all(C = [tf(key("F30"), -9.963)])) :-
  phrase(code_command(C), "/F30 -9.963 Tf").

test(command4, all(C = [tf(key("F30"))])) :-
  phrase(code_command(C), "/F30 Tf").

test(command5, all(C = [td(key("F30"), -498)])) :-
  phrase(code_command(C), "/F30 -498 Td").

test(command6, all(C = [tj([string("HO"), 32, string("W"), -498, string("TO")])])) :-
  phrase(code_command(C),
         "[(HO)32(W)-498(TO)]TJ").

test(code_ccommands, all(C = [[td(key("F30"), -498), td(key("F30"), -498)]])) :-
  phrase(code_commands(C), "/F30 -498 Td /F30 -498 Td").

test(code_block, all(C = [[td(key("F30"), -498), td(key("F30"), -498)]])) :-
  phrase(code_block(C), "BT /F30 -498 Td /F30 -498 Td ET").

:- end_tests(code).
