:- module(pdffile, []).
:- use_module(library(clpfd)).

:- set_prolog_flag(double_quotes, codes).

locate_xref(Stream, Offset) :-
  seek(Stream, -100, eof, _Pos),
  phrase_from_stream(
    (
      pdf:gibberish,
      pdf:xref_indicator(Offset)
    ),
    Stream
  ),
  seek(Stream, Offset, bof, Offset),
  !.


parse_file(FileName, Context) :-
  setup_call_cleanup(
    open(FileName, read, Stream, [type(binary)]),
    (
      parse_stream(Stream, Context),
      do_something(Context)
    ),
    close(Stream)
  ).

parse_stream(Stream, context(Stream, XRef, Trailer)) :-
  locate_xref(Stream, _Offset),
  phrase_from_stream((
                        pdf:xref(XRef),
                        pdf:trailer(Trailer)
                      ),
                     Stream),
  !.


do_something(Context) :-
  context_reify_object(Context, reference(132, 0), Object),
  format("Found object?! ~w~n", [Object]).

context_reify_object(Context, Reference, Object) :-
  context_locate_object(Context, Reference),
  context_read_object(Context, Object).

context_read_object(Context, Object) :-
  context_stream(Context, Stream),
  phrase_from_stream(
    (
      pdf:object(Object),
      pdf:remainder
    ),
    Stream),
  !.

context_locate_object(Context, Reference) :-
  context_xref(Context, XRef),
  context_stream(Context, Stream),
  pdf:xref_lookup(XRef, Reference, Offset),
  !,
  seek(Stream, Offset, bof, Offset).

context_stream(context(S,_1,_2), S).
context_xref(context(_1,X,_2), X).
