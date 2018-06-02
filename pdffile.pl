:- module(pdffile, []).
:- use_module(library(clpfd)).
:- use_module(pdf).

:- set_prolog_flag(double_quotes, codes).
:- set_prolog_flag(backtrace_depth, 40).
:- set_prolog_flag(backtrace_goal_depth, 5).
:- set_prolog_flag(debugger_write_options,
                   [quoted(true), portray(true), max_depth(30), priority(699)]).

test_app :-
  parse_file("/home/erik/Downloads/halmos.pdf", _Ctx).

parse_file(FileName, Context) :-
  setup_call_cleanup(
    open(FileName, read, Stream, [type(binary)]),
    (
      context_from_stream(Stream, Context),
      do_something(Context)
    ),
    close(Stream)
  ).

%
% Construct a pdf-context from the given stream.
% The context will give you access to the rest of the API
%
context_from_stream(Stream, context(Stream, XRef, Trailer)) :-
  locate_xref(Stream, _Offset),
  phrase_from_stream(
    (
      pdf:xref(XRef),
      pdf:trailer(Trailer)
    ),
    Stream
  ),
  !.


do_something(Context) :-
  forall(
    context_reify_object(Context, Ref, Object),
    (
      pdf:all_object_references(Object, Refs),
      format("Object ~w -> ~w~n", [Ref, Refs])
    )
  ).

%
% Seek to start of X-ref section
%
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

%
% Reify = locate + Read
%
context_reify_object(Context, Reference, Object) :-
  context_locate_object(Context, Reference),
  format("Located ~w", [Reference]),
  (
    context_read_object(Context, Object)
    *->
    format(" - OK~n", []) ;
    format("FAIL ~n")
  )
.

%
% Read == Parse out object from pdffile
%
context_read_object(Context, Object) :-
  context_stream(Context, Stream),
  phrase_from_stream(
    (
      pdf:object(Object),
      pdf:remainder
    ),
    Stream),
  !.

%
% Locate = Look up object in X-ref, seek to offset
%
context_locate_object(Context, Reference) :-
  context_xref(Context, XRef),
  context_stream(Context, Stream),
  !,
  pdf:xref_lookup(XRef, Reference, Offset),
  seek(Stream, Offset, bof, Offset).

%
% Context getters
%
context_stream(context(S,_1,_2), S).
context_xref(context(_1,X,_2), X).
