%
% This is a test app to try and parse som PDF-code
%
:- module(code_app, []).
:- use_module(pdffile).

:- set_prolog_flag(double_quotes, codes).
:- set_prolog_flag(backtrace_depth, 40).
:- set_prolog_flag(backtrace_goal_depth, 5).
:- set_prolog_flag(debugger_write_options,
                   [quoted(true), portray(true), max_depth(30), priority(699)]).

test_app :-
  with_file_context("/home/erik/Downloads/halmos.pdf",
                    code_app:do_something).


do_something(Context) :-
  format("Program started~n"),
  pdffile:context_reify_object(Context, Reference, Object),
  pdf:object_type(Object, stream),

  % Print info
  !,
  format("Object ~w has type ~w~n", [Reference, stream]),
  pretty_print_object(Object),

  % Locate data
  pdffile:context_stream(Context, Stream),
  pdffile:context_locate_object(Context, Reference),
  find_stream_data_offset(Stream, Object, Offset),

  % Open stream
  !,
  seek(Stream, Offset, bof, Offset),
  setup_call_cleanup(
    zopen(
      Stream, ZStream,
      [
        close_parent(false)
      ]
    ),
    phrase_from_stream(everything(Content), ZStream),
    close(ZStream)
  ),

  !,
  format("---PDF CODE---~n"),
  format(Content),
  format("--------------~n").


find_stream_data_offset(Stream, Object, Offset) :-
  phrase_from_stream(
    (
      pdf:stream_object_header(Object),
      lazy_list_character_count(Offset),
      pdf:gibberish2
    ),
    Stream
  ).

everything([A | Rest]) --> [A], everything(Rest).
everything([]) --> [].

abc(Content, Length) -->
  {
    length(Content, Length)
  },
  Content, pdf:gibberish2.

abc2(Content) -->
  lazy_list_character_count(Content),
  pdf:gibberish2.

pretty_print_object(Object) :-
  pdf:object_dictionary(Object, Dictionary),
  pretty_print_dictionary(Dictionary).

pretty_print_dictionary(D) :-
  format("<<~n"),
  forall(
    member(K-V, D),
    (
      key_string(K, KS),
      key_string(V, VS),
      format("~w = ~w~n", [KS, VS])
    )
  ),
  format(">>~n").

key_string(key(Codes), String) :-
  string_codes(String, Codes).

key_string(Other, Other) :-
  Other \= key(_).
