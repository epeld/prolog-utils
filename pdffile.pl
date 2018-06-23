:- module(pdffile,
          [
            with_code_stream/3,
            with_file_context/2,
            context_reify_object/3,
            find_object_type/2
          ]).
:- use_module(library(clpfd)).
:- use_module(pdf).

:- set_prolog_flag(double_quotes, codes).
:- set_prolog_flag(backtrace_depth, 40).
:- set_prolog_flag(backtrace_goal_depth, 5).
:- set_prolog_flag(debugger_write_options,
                   [quoted(true), portray(true), max_depth(30), priority(699)]).

%
% Calls Goal with a with a Stream argument
% for reading the PS-code inside the Referenced object
with_code_stream(Context, Reference, Goal) :-
  pdffile:context_stream(Context, Stream),
  pdffile:context_locate_object(Context, Reference),

  find_stream_data_offset(Stream, Offset),

  !,
  seek(Stream, Offset, bof, Offset),
  setup_call_cleanup(
    zopen(
      Stream, ZStream,
      [
        close_parent(false)
      ]
    ),
    call(Goal, ZStream),
    close(ZStream)
  ).


%
% Calls Goal with a Context-argument
% created from the indicated file name
with_file_context(FileName, Goal) :-
  functor(Goal, _Term, _Arity),
  setup_call_cleanup(
    open(FileName, read, Stream, [type(binary)]),
    (
      context_from_stream(Stream, Context),
      call(Goal, Context)
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


% Finds the file position (Offset) where the stream data starts,
% given a Stream positioned just before the whole stream object
find_stream_data_offset(Stream, Offset) :-
  find_stream_data_offset(Stream, _Object, Offset).

find_stream_data_offset(Stream, Object, Offset) :-
  phrase_from_stream(
    (
      pdf:stream_object_header(Object),
      lazy_list_character_count(Offset),
      pdf:gibberish2
    ),
    Stream
  ).


all_objects(Context, Objects) :-
  findall(
    Object,
    context_reify_object(Context, _Ref, Object),
    Objects
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
  (
    context_read_object(Context, Object)
    *->
    true ;
    format("~w - FAILED ~n", [Reference])
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


%
% Object Type Predicates
%
find_object_type(Object, Type) :-
  once(pdf:object_type(Object, Type)) *-> true
  ; (
    % format("Unkown type for ~w~n", [Object]),
    Type = unknown
  ).
