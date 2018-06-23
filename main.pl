:- module(main, []).
% :- use_module(main, []).

main :-
  argv(Args),
  main_with_args(Args).

main_with_args(Args) :-
  args_mode(Args, Mode, Args0),
  mode_exists(Mode),
  run(Mode, Args0).

mode_exists(Mode) :-
  clause(run(Mode, _Args), _Body).

run(print, Args) :-
  args_filename(Args, FileName, Rest0),
  args_reference(Rest0, Reference, Rest1),
  args_empty(Rest1),
  print(FileName, Reference).
run(help, _Args) :-
  print_usage.

print(FileName, Reference) :-
  catch(
    code_app:run(print, FileName, Reference),
    Error,
    (
      format_error("Error: ~w~n", [Error]),
      true
    )
  ).


args_mode([StringMode | Rest], Mode, Rest) :-
  atom_string(Mode, StringMode).

args_filename([FileName | Rest], FileName, Rest) :- !.
args_filename(_A, _B, _C) :-
  print_usage, fail.


args_reference([ReferenceString | Rest], Reference, Rest) :-
  !,
  (
    pdf:parse_reference(ReferenceString, Reference)
    *-> !
  ; ( format_error("Cannot parse ~w~n", [ReferenceString]), fail )
  ).
args_reference(_A, _B, _C) :-
  print_usage, fail.


args_empty([]) :- !.
args_empty(_A) :-
  print_usage, fail.


print_usage :-
  format_error("Usage: prologpdf <FileName> [<Reference>]~n", []).


format_error(Message, Args) :-
  stream_property(Stream, alias(user_error)),
  !,
  format(Stream, Message, Args).
