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


