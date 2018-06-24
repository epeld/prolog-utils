
:- module(load, []).
:- use_module(pdf).
:- use_module(pdffile).
:- use_module(graph).
:- use_module(code).
:- use_module(interpreter).
:- use_module(main).

:- use_module(code_app).
:- use_module(graph_app).

:- example:run_tests.

%
% Produce the prologpdf command line tool.
% Note that the new "prologpdf"-process
%
% Will be invoked with the same command line
% flags as your current prolog process
%
compile_app :-
  qsave_program(
    "prologpdf",
    [
      toplevel(main:main),
      stand_alone(false)
    ]
  ).
