:- module(pdffile, []).
:- use_module(library(clpfd)).

:- set_prolog_flag(double_quotes, codes).

locate_xref(Stream, Offset) :-
  seek(Stream, -100, eof, _Pos),
  phrase_from_stream((
            pdf:gibberish,
            pdf:xref_indicator(Offset)
                      ), Stream),
  seek(Stream, Offset, bof, Offset),
  !.


parse_file(FileName) :-
  setup_call_cleanup(
    open(FileName, read, Stream, [type(binary)]),
    parse_stream(Stream),
    close(Stream)
  ).

parse_stream(Stream) :-
  locate_xref(Stream, Offset),
  format("Offset ~w~n", [Offset]),
  phrase_from_stream((
                        pdf:xref(XRef),
                        pdf:trailer(Trailer)
                      ),
                     Stream),
  !,
  format("Xref ~w~n", XRef),
  format("Trailer ~w~n", Trailer).


