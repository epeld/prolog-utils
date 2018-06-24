
:- module(prologpdf, []).

prologpdf :-
  current_prolog_flag(argv, Args),
  prologpdf_with_args(Args).

prologpdf_with_args(Args) :-
  args_mode(Args, Mode, Args0),
  mode_exists(Mode),
  run(Mode, Args0).

mode_exists(Mode) :-
  clause(run(Mode, _Args), _Body).

run(print, [FileName, Reference]) :-
  once(
    pdf:parse_reference(Reference, R)
  ),
  print(FileName, R).

run(list, [FileName]) :-
  pdffile:with_file_context(FileName,
                            prologpdf:list_objects).


run(raw, [FileName, Reference]) :-
  once(
    pdf:parse_reference(Reference, R)
  ),
  pdffile:with_file_context(FileName,
                            prologpdf:print_raw_object(R)).


print_raw_object(Reference, Context) :-
  pdffile:context_locate_object(Context, Reference),
  pdffile:context_read_object(Context, _Object, Start, Stop),
  pdffile:context_locate_object(Context, Reference),
  !,
  Length is Stop - Start,
  length(Codes, Length),
  pdffile:context_stream(Context, Stream),
  phrase_from_stream(
    (Codes, pdf:gibberish2),
    Stream
  ),
  format("~s", [Codes]).

list_objects(Context) :-
  forall(
    pdffile:context_reify_object(Context, Reference, Object),
    (
      pdffile:find_object_type(Object, Type),
      pdf:codify_reference(Reference, Codes),
      string_codes(RefString, Codes),
      format("~w~|~t~w~15+~n", [RefString, Type])
    )
  ).

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
