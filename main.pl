:- module(main, []).
% :- use_module(main, []).

main :-
  FileName = "/home/erik/Downloads/halmos.pdf",
  Reference = reference(3, 0),
  code_app:run(print, FileName, Reference).
