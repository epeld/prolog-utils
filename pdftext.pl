:- module(pdftext, []).
:- use_module(interpreter, []).


% TODO remove this clause
pdftext :-
  !,
  pdftext_from_testfile.

pdftext :-
  user_input(Input),
  pdftext(Input).




pdftext(Input) :-
  code:parse_code(Input, Commands),
  interpreter:interpret(Commands, CommandsOut),
  !,
  with_open_file("/tmp/out.txt", write, [],
                write_commands(CommandsOut)),

  !.


write_commands(Commands, Stream) :-
  format(Stream, "~w", [Commands]).

pdftext_from_testfile :-
  pdftext_from_file("samples/code.txt").


pdftext_from_file(FileName) :-
  with_open_file(FileName, read, [type(binary)], pdftext:pdftext).


with_open_file(FileName, Mode, Options, Goal) :-
  setup_call_cleanup(
    open(FileName, Mode, Stream, Options),
    call(Goal, Stream),
    close(Stream)
  ).


user_input(Input) :-
  stream_property(Input, alias(user_input)).
