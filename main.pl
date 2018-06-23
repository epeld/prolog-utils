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

run(print, [FileName, Reference]) :-
  once(
    parse_reference(Reference, R)
  ),
  print(FileName, R).


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



print_usage :-
  format_error("Usage: ???~n", []).


format_error(Message, Args) :-
  stream_property(Stream, alias(user_error)),
  !,
  format(Stream, Message, Args).
