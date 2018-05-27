:- module(pdf, []).
:- use_module(library(clpfd)).

:- set_prolog_flag(double_quotes, codes).

gibberish --> [].
gibberish --> [_C], gibberish.

xref(xref(Start, Rows)) -->
  "xref", whitespace,
  integer(Start), space, integer(Count), whitespace,
  {
    length(Rows, Count)
  },
  xref_rows(Rows).

xref_rows([]) --> [].
xref_rows([R | Rows]) -->
  xref_row(R), whitespace,
  xref_rows(Rows).

xref_row(xref_row(X, Y, State)) -->
  integer(X), space, integer(Y), space, allocation_state(State), optional_space.

allocation_state(free) --> "f".
allocation_state(in_use) --> "n".

trailer(trailer(Meta, Offset)) -->
  "trailer", whitespace,
  dictionary(Meta), whitespace,
  xref_indicator(Offset).

xref_indicator(Offset) -->
  "startxref", whitespace,
  integer(Offset), whitespace,
  "%%EOF",
  optional_whitespace.

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

optional_space --> space ; [].
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

hexchar(N) --> { hexchar(C, N) }, [C].

hexchar(C, N) :-
  (
    digit(C),
    [Zero] = "0",
    N #= C - Zero
  ) ;
  (
    [A,F] = "AF",
    A #=< C, C #=< F,
    N #= C - A
  ).


paren_string(string(S)) -->
  paren_string_(S).

paren_string_(S) -->
  "(",
  paren_content(S),
  ")".

paren_content([]) --> [].
paren_content([C | Rest]) -->
  paren_char(C),
  paren_content(Rest).

paren_content(S) -->
  paren_string_(S0),
  {
    append(["(", S0, ")"], SPrefix),
    append(SPrefix, Other, S)
  },
  paren_content(Other).

paren_char(C) -->
  [C],
  {
    [Left, Right] = "()",
    C #\= Left,
    C #\= Right
  }.

paren_char(C) -->
  "\\", [C].


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

test(reference_array, all(_X = [_])) :-
  phrase(pdf:array(X),
         "[73 0 R 77 0 R 80 0 R 83 0 R 86 0 R 89 0 R]"),
  length(X, 6).

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

test(xref, all(_X = [_])) :-
  phrase(pdf:xref(_),
         "xref
0 6
0000000000 65535 f 
0000003321 00000 n 
0000003209 00000 n 
0000000009 00000 n 
0000182298 00000 n 
0000178182 00000 n 
").

test(xref_row, all(_X = [_])) :-
  phrase(pdf:xref_row(_),
         "0000000000 65535 f").

test(xref_row_2, all(_X = [_])) :-
  phrase(pdf:xref_row(_),
         "0000178182 00000 n").

test(xref_row_3, all(_X = [_])) :-
  phrase(pdf:xref_row(_),
         "0000000009 00000 n ").

test(paren_string, all(_X = [_])) :-
  phrase(pdf:paren_string(_),
         "(This is a string)").

test(paren_string_nested, all(_X = [_])) :-
  phrase(pdf:paren_string(_),
         "(This is (a) string)").

test(paren_string_escaped, all(_X = [_])) :-
  phrase(pdf:paren_string(_),
         "(This is \\)a\\( string)").

test(gibberish, all(_X = [_])) :-
  phrase((
            pdf:gibberish, "abc"
          ),
         "asdawfwfwsdabc").

test(gibberish, all(_X = [_])) :-
  phrase((
            pdf:gibberish,
            "abc",
            pdf:gibberish
          ),
         "2232abc ").

:- end_tests(pdf).