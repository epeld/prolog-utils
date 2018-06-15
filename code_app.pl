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
  !,
  format("Object ~w has type ~w~n", [Reference, stream]).
