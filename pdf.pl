:- module(pdf, []).
:- use_module(library(clpfd)).

:- set_prolog_flag(double_quotes, codes).

parse_reference(String, Reference) :-
  string_codes(String, Codes),
  phrase(reference(Reference), Codes).

codify_reference(reference(A,B), Codes) :-
  format(codes(Codes), "~w ~w R", [A,B]).

gibberish2(_A, _B).

gibberish --> [].
gibberish --> [_C], gibberish.

object_dictionary(object(_R, Dictionary, _), Dictionary).

object_type(object(_R, D, none), font_desc) :-
  member(key("FontFile")-_Path, D).

object_type(object(_R, D, none), proc_set) :-
  member(key("ProcSet")-_Array, D).

object_type(object(_R, _, stream(_)), stream).

object_type(object(_R, none, array(_)), array).

object_type(object(_R, D, _Payload), Type) :-
  dictionary_type(D, Type).

dictionary_type(D, Type) :-
  member(key("Type")-key(CType), D),
  atom_codes(Type0, CType),
  downcase_atom(Type0, Type).

remainder(_A, []).

object_references(object(_Ref, Meta, _Stream), Reference) :-
  Reference = reference(_X, _Y),
  member(key(_Key)-Reference, Meta).

all_object_references(Object, Refs) :-
  findall(Ref, object_references(Object, Ref), Refs).

xref_lookup(xref(Start, Rows), reference(X, Gen), Offset) :-
  length(Rows, Length),
  Length0 #= Length - 1,
  between(0, Length0, X0),
  X #= X0 + Start,
  %% X #= X0 + Start,
  %% 0 #=< X0, X0 < Length,
  nth0(
    X0,
    Rows,
    xref_row(Offset, Gen, in_use)
  ).

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

stream_header -->
  "stream", ( "\n" ; "\r\n" ).

stream(skipped, Length) -->
  stream_header,
  skip(Length),
  ("\n" ; []),
  "endstream".

skip(0) --> [].
skip(N) -->
  { N > 0, N0 is N - 1 },
  [_],
  skip(N0).

%% stream(Contents, Length) -->
%%   {
%%     length(Contents, Length)
%%   },
%%   "stream",
%%   ( "\n" ; "\r\n" ),
%%   Contents,
%%   ("\n" ; []),
%%   "endstream".

array(Array) -->
  "[", optional_whitespace, array_(Array), "]".

array_([]) --> [].
array_([X | Rest]) -->
  array_value(X),
  array_1(Rest, X).

array_1([], _) --> optional_whitespace.
array_1([X | Rest], Previous) -->
  optional_whitespace(Whitespace),
  array_value(X),
  {
    valid_whitespace(Previous, Whitespace, X)
  },
  array_1(Rest, X).

valid_whitespace(A, false, B) :-
  (\+ wants_whitespace(A)) ; (\+ wants_whitespace(B)).

valid_whitespace(_A, true, _B).

wants_whitespace(key(_)).
wants_whitespace(N) :- number(N).

array_value(Value) --> value(Value).
array_value(N-Symbols) -->
  integer(N),
  optional_whitespace,
  keys(Symbols).

keys_([]) -->
  [].

keys_([Symbol | Rest]) -->
  key(Symbol),
  keys_(Rest).

keys([Symbol | Rest]) -->
  key(Symbol),
  keys_(Rest).

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

%
% Used to read a stream object up until the
% stream data starts
stream_object_header(object(R, D, _)) -->
  object_definition(R), whitespace,
  dictionary(D), whitespace,
  stream_header.

object(object(R, D, Payload)) -->
  object_definition(R), whitespace,
  (
    ( dictionary(D), whitespace )
  ; { D = none }
  ),
  (
    ( object_with_payload(object(R, D, Payload)), whitespace )
  ; { Payload = none }
  ),
  "endobj".

object_with_payload(object(_R, _D, array(Array))) -->
  array(Array).

object_with_payload(object(_R, D, stream(StreamContents))) -->
  {
    dictionary_length(D, StreamLength)
  },
  stream(StreamContents, StreamLength).


dictionary_length(Dictionary, Length) :-
  member(key("Length")-Length, Dictionary).

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
  key(Key), whitespace, value(Value), whitespace, optional_spaces, optional_whitespace,
  ( key_value_pairs(Rest) ; empty(Rest) ).


key(key(Key)) -->
  "/",
  key_(Key),
  { Key \= [] }.


key_([C | Key]) -->
  keychar(C),
  key_(Key).

key_([]) --> [].

keychar(C) -->
  { keychar(C) }, [C].

keychar(C) :-
  digit(C) ; alpha(C) ; member(C, "+-.").

value(Value) -->
  number(Value) ;
  key(Value) ;
  reference(Value) ;
  array(Value) ;
  dictionary(Value) ;
  paren_string(Value) ;
  hex_string(Value).

integer(Integer) -->
  "-",
  digits(Digits),
  {
    number_codes(Integer0, Digits),
    Integer is -Integer0
  }.

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

optional_spaces --> space, optional_spaces.
optional_spaces --> [].

optional_space --> space ; [].

optional_whitespace --> whitespace ; []. %optional_whitespace(_).

optional_whitespace(true) --> whitespace.
optional_whitespace(false) --> [].

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

test(number, all(_X = [-194])) :-
  phrase(integer(_Int), "-194").

test(key, all(K = ["Length"])) :-
  length(K, 6),
  phrase(key(key(K)), "/Length").

test(key, all(K = [".notdef"])) :-
  length(K, 7),
  phrase(key(key(K)), "/.notdef").

test(key_non_alphanum, all(K = ["XFHWXJ+CMBX10"])) :-
  phrase(key(key(K)), "/XFHWXJ+CMBX10").

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

test(dictionary_2, all(_X = [_])) :-
  phrase(pdf:dictionary(D),
         "<<
/Ascent 694
/CapHeight 686
/Descent -194
/FontName /XFHWXJ+CMBX10
/ItalicAngle 0
/StemV 114
/XHeight 444
/FontBBox [-301 -250 1164 946]
/Flags 4
/CharSet (/A/C/E/H/I/M/O/R/S/T/W)
/FontFile 5 0 R
>>"),
  length(D, _L).

test(dictionary_3, all(_X = [_])) :-
  phrase(pdf:dictionary(D),
         "<<
/Length 3121      
/Filter /FlateDecode
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

test(special_array, [blocked(todo), all(_X = [_])]) :-
  phrase(pdf:dictionary(_D),
         "[ 0 /.notdef 65/A 66/.notdef 67/C 68/.notdef 69/E 70/.notdef 72/H/I 74/.notdef 77/M 78/.notdef 79/O 80/.notdef 82/
R/S/T 85/.notdef 87/W 88/.notdef]").


test(object_definition, all(_X = [_])) :-
  phrase(pdf:object_definition(_R),
         "3 0 obj").


test(object, all(_X = [_])) :-
  phrase(pdf:object(object(_R, _D, none)),
         "3 0 obj <<
/Length 3121
/Filter /FlateDecode
>> endobj").

test(object_type, all(_X = [_])) :-
  phrase(pdf:object(Object),
         "4 0 obj <<
/Ascent 694
/CapHeight 686
/Descent -194
/FontName /XFHWXJ+CMBX10
/ItalicAngle 0
/StemV 114
/XHeight 444
/FontBBox [-301 -250 1164 946]
/Flags 4
/CharSet (/A/C/E/H/I/M/O/R/S/T/W)
/FontFile 5 0 R
>> endobj"),
  object_type(Object, font_desc) .

test(object_type, all(_X = [_])) :-
  phrase(pdf:object(Object),
         "23 0 obj <<
/Font << /F7 18 0 R /F8 12 0 R /F31 15 0 R >>
/ProcSet [ /PDF /Text ]
>> endobj"),
  object_type(Object, proc_set) .

test(array_object, all(_X = [_])) :-
  phrase(pdf:object(object(_R, none, X)),
         "108 0 obj\n[ 1 2 3 ]\nendobj"),
  X = array([1,2,3]).

test(write_object, [blocked(infinite_loop), all(_X = [_])]) :-
  phrase(pdf:object(object(reference(1,2), [key("Length")-123], none)),
         S),
  S = "1 2 obj <<
/Length 123
>> endobj".

test(object_references, all(Ref = [reference(1,2), reference(4,5)])) :-
  pdf:object_references(
        object(
          reference(3,4),
          [key("Something")-"Else",
           key("Foo")-reference(1,2),
           key("Bar")-reference(4,5)
          ], none
        ),
        Ref
      ).

test(reference, all(_X = [_])) :-
  phrase(pdf:reference(reference(X, Y)),
         "15 0 R"),
  X = 15, Y = 0.

test(float, all(_X = [_])) :-
  phrase(pdf:float(F),
         "15.33"),
  F = 15.33.

test(float2, all(_X = [_])) :-
  phrase(pdf:float(F),
         "-15.33"),
  F = -15.33.


test(array, all(_X = [_])) :-
  phrase(pdf:array(X),
         "[1 0 3 44.5]"),
  length(X, 4).

test(array2, all(_X = [_])) :-
  phrase(pdf:array(X),
         "[ /PDF /Text ]"),
  length(X, 2).

test(array3, all(_X = [_])) :-
  phrase(pdf:array(X),
         "[ /Text ]"),
  length(X, 1).


test(difference_array, nondet) :-
  phrase(pdf:array(_X),
         "[ 0 /.notdef 39/quoteright 40/.notdef 46/period 47/.notdef 65/A 66/.notdef 68/D/E 70/.notdef 73/I 74/.notdef 76/L/M/N 79/.notdef 80/P 81/.notdef 82/R/S/T/U 86/.notdef 87/W 88/.notdef 97/a/b/c/d/e/f/g/h/i 106/.notdef 108/l/m/n/o/p/q/r/s/t/u 118/.notdef 119/w 120/.notdef 121/y 122/.notdef]").

test(array_2, all(_X = [_])) :-
  phrase(pdf:array(X),
         "[1 2 3 ]"),
  length(X, 3).

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
  %length(X, 10)
  X = skipped
.

test(stream_obj, all(_X = [_])) :-
  phrase(pdf:object(object(_A, _B, _C)),
         "1 0 obj <<\n/Length 10\n   \n/Filter /FlateDecode\n>>\nstream\n0123456789\nendstream\nendobj").

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

test(charset_string, all(_X = [_])) :-
  phrase(pdf:paren_string(_),
         "(/A/C/E/H/I/M/O/R/S/T/W)").

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
