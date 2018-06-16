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
  stream_reference(Context, Reference),
  !,
  format("~w~n", Reference),
  pdffile:with_code_stream(Context, Reference, code_app:print_code).


stream_reference(Context, Reference) :-
  pdffile:context_reify_object(Context, Reference, Object),
  pdf:object_type(Object, stream).

print_code(ZStream) :-
  phrase_from_stream(everything(Content), ZStream),

  !,
  %format("---PDF CODE---~n"),
  format(Content)
  %format("--------------~n")
.


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
