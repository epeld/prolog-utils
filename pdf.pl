:- module(pdf).
:- use_module(library(clpfd)).

:- set_prolog_flag(double_quotes, codes).

trailer(trailer(Meta, Offset)) -->
  "trailer", whitespace,
  dictionary(Meta), whitespace,
  "startxref", whitespace,
  integer(Offset), whitespace,
  "%%EOF".

stream(Contents, Length) -->
  {
    length(Contents, Length)
  },
  "stream",
  ( "\n" ; "\r\n" ),
  Contents,
  ("\n" ; []),
  "endstream".

array(Array) --> "[", optional_whitespace, array_(Array), "]".

array_([]) --> [].
array_([X | Rest]) -->
  value(X),
  array_1(Rest).

array_1([]) --> optional_whitespace.
array_1([X | Rest]) -->
  whitespace,
  value(X),
  array_1(Rest).


number(X) --> integer(X).
number(X) --> float(X).

float(F) -->
  integer(A),
  ".",
  integer(B),
  {
    format(codes(N), "~w.~w", [A,B]),
    read_from_codes(N, F)
  }.

reference(reference(X, Y)) -->
  integer(X), space, integer(Y), space, "R".

object(object(R, D, none)) -->
  object_definition(R), whitespace, dictionary(D), whitespace, "endobj".

object(object(R, D, stream(StreamContents))) -->
  object_definition(R), whitespace, dictionary(D), whitespace,
  {
    member(key("Length")-StreamLength, D)
  },
  stream(StreamContents, StreamLength),
  whitespace, "endobj".

object_definition(reference(X, Y)) -->
  integer(X),
  whitespace,
  integer(Y),
  whitespace,
  "obj".

dictionary(D) -->
  "<<",
  whitespace,
  key_value_pairs(D),
  ">>".

key_value_pairs([Key - Value | Rest]) -->
  key(Key), whitespace, value(Value), whitespace,
  ( key_value_pairs(Rest) ; empty(Rest) ).


key(key(Key)) -->
  "/",
  key_(Key).

key_(Key, Before, After) :-
  append(Key, After, Before),
  maplist(keychar, Key).

keychar(C) :-
  digit(C) ; alpha(C).

value(Value) -->
  number(Value) ;
  key(Value) ;
  reference(Value) ;
  array(Value) ;
  dictionary(Value) ;
  hex_string(Value).

integer(Integer) -->
  digits(Digits),
  {
    number_codes(Integer, Digits)
  }.

digits(Digits) -->
  digits_(Digits),
  { Digits = [_X | _] }.

digits_([]) --> [].
digits_([D | Rest]) -->
  digit(D), digits_(Rest).


empty([]) --> [].

digit(D) --> [D], { digit(D) }.

digit(D) :-
  [Lower, Upper] = "09",
  Lower #=< D,
  D #=< Upper.

digits("1234567890").

alpha(C) --> { alpha(C) }, [C].

alpha(C) :-
  [Lower, Upper] = "Az",
  Lower #=< C,
  C #=< Upper.


space --> " ".

optional_whitespace --> whitespace ; [].

whitespace --> " ".
whitespace --> "\n".


hex_string(hex(Hex)) -->
  "<",
  hex(Hex),
  ">".

hex([H | Rest]) -->
  hexchar(A),
  hexchar(B),
  {
    H #= A * 16 + B
  },
  hex(Rest).

hex([]) --> [].

hexchar(C) --> { hexchar(C) }, [C].

hexchar(C) :-
  digit(C) ;
  (
    [A,F] = "AF",
    A #=< C, C #=< F
  ).

:- begin_tests(pdf).

:- set_prolog_flag(double_quotes, codes).

test(alpha) :-
  phrase(alpha(_A), "L").

test(digit) :-
  phrase(digit(_D), "3").

test(key) :-
  length(K, 6),
  phrase(key(key(K)), "/Length").

test(keynum, all(_X = [_])) :-
  length(K, 3),
  phrase(key(key(K)), "/F15").

test(dictionary, all(_X = [_])) :-
  phrase(pdf:dictionary(D),
         "<<
/Length 3121
/Filter /FlatDecode
>>"),
  length(D, 2).

test(oneline_dictionary, all(_X = [_])) :-
  phrase(pdf:dictionary(D),
         "<< /F30 6 0 R /F15 9 0 R /F8 12 0 R /F31 15 0 R /F7 18 0 R >>"),
  length(D, 5).

test(nested_dictionary, all(_X = [_])) :-
  phrase(pdf:dictionary(D),
         "<<
/Font << /F30 6 0 R /F15 9 0 R /F8 12 0 R /F31 15 0 R /F7 18 0 R >>
/ProcSet [ /PDF /Text ]
>>"),
  length(D, 2).


test(object_definition, all(_X = [_])) :-
  phrase(pdf:object_definition(_R),
         "3 0 obj").


test(object, all(_X = [_])) :-
  phrase(pdf:object(object(_R, _D, none)),
         "3 0 obj <<
/Length 3121
/Filter /FlateDecode
>> endobj").

test(reference, all(_X = [_])) :-
  phrase(pdf:reference(reference(X, Y)),
         "15 0 R"),
  X = 15, Y = 0.

test(float, all(_X = [_])) :-
  phrase(pdf:float(F),
         "15.33"),
  F = 15.33.


test(array, all(_X = [_])) :-
  phrase(pdf:array(X),
         "[1 0 3 44.5]"),
  length(X, 4).

test(whitespace_array, all(_X = [_])) :-
  phrase(pdf:array(X),
         "[ 1 0 3 44.5 ]"),
  length(X, 4).

test(empty_array, all(_X = [_])) :-
  phrase(pdf:array(X),
         "[]"),
  length(X, 0).

test(empty_array_whitespace, all(_X = [_])) :-
  phrase(pdf:array(X),
         "[ ]"),
  length(X, 0).

test(singleton_array, all(_X = [_])) :-
  phrase(pdf:array(X),
         "[123]"),
  length(X, 1).

test(key_array, all(_X = [_])) :-
  phrase(pdf:array(X),
         "[ /PDF /Text ]"),
  length(X, 2).

test(stream, all(_X = [_])) :-
  phrase(pdf:stream(X, 10),
         "stream\r\n0123456789\nendstream"),
  length(X, 10).

test(stream_obj, all(_X = [_])) :-
  phrase(pdf:object(object(_A, _B, _C)),
         "1 0 obj << /Length 10 >> stream\r\n0123456789\nendstream endobj").

test(hex_string, all(_X = [_])) :-
  phrase(pdf:hex_string(_),
         "<860E0479AA1B431901634E53BADB1169>").


test(trailer, all(_X = [_])) :-
  phrase(pdf:trailer(_T),
         "trailer
<<
/Size 134
/Root 132 0 R
/Info 133 0 R
/ID [<860E0479AA1B431901634E53BADB1169> <860E0479AA1B431901634E53BADB1169>]
>>
startxref
183475
%%EOF").

:- end_tests(pdf).
