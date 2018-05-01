%
% This module defines utilities for specifying short usage cases / tests
%
:- module(example, [example_usage/0]).
:- use_module(library(pprint)).

:- multifile example_usage/0.
:- multifile example_usage/1.
:- multifile example_usage/2.

%
% Important! Keep these, or the example-collection logic will break!
%
example_usage(this_is_an_example).
example_usage(this_is_an_example_with_options, []).

run_tests :-
  usage_example_results(S, F, E),
  print_results("Successes", S),
  print_results("Failures", F),
  print_results("Errors", E).

usage_examples(Examples) :-
  Examples = [_H | _R],
  bagof(
    (example_usage :- Example),
    (
      clause(example_usage(_Name, Options), Example),
      member(exclusive(true), Options)
    ),
    Examples
  ),
  !.

usage_examples(Examples) :-
  bagof(
    (example_usage :- Example),
    clause(example_usage, Example),
    Examples0
  ),
  bagof(
    (example_usage(Name) :- Body),
    clause(example_usage(Name), Body),
    Examples1
  ),
  bagof(
    (example_usage(Name, Options) :- Body),
    clause(example_usage(Name, Options), Body),
    Examples2
  ),
  append([Examples0, Examples1, Examples2], Examples).


collect_results(Examples, Results) :-
  maplist(run_case, Examples, Results0),
  maplist(make_result, Examples, Results0, Results).


make_result(Example, Result, result(Example, Result)).


usage_example_results(Successes, Failures, Errors) :-
  usage_examples(Examples),

  !,
  length(Examples, Length),
  format("Found ~w usage_examples~n", [Length]),

  length(Results, Length),
  collect_results(Examples, Results),

  include(is_result(ok), Results, Successes),
  include(is_result(failed), Results, Failures),
  include(is_result(error), Results, Errors).


is_result(ok, result(_, ok(_D))).
is_result(failed, result(_, failed)).
is_result(error, result(_, error(_Err, _Err2))).


run_case((Head :- Body), Result) :-
  case_options(Head, Options),
  transform_body(Body, Options, Body0),
  catch(
    (
      call(Body0) *->
      (
	!,
	Result = ok(choicepoints_discarded)
      );
      Result = failed
    ),
    Error,
    Result = Error
  ).


transform_body(Body, Options, (Body, spy)) :-
  member(spy, Options),
  throw(Options).

transform_body(Body, Options, Body) :-
  \+ member(spy, Options).

case_options(example_usage(_Name, Options), Options).
case_options(example_usage(_Name), []).
case_options(example_usage, []).


print_results(Heading,  []) :-
  !,
  format("~n~n--- NO ~w ---~n", [Heading]).

print_results(Heading,  Examples) :-
  length(Examples, Length),
  format("~n~n--- ~w ~w ---~n", [Length, Heading]),
  (
    Examples = [] *-> format("<none>~n") ;
    forall(
      member(result(Example, Result), Examples),
      (
	result_summary(Result, Summary),
	format_example(Example, Summary)
      )
    )
  ).

format_example((Head :- _Body), ok) :-
  format("~n% Example '~w': OK~n", [Head]).

format_example((Head :- (Module:Body)), Other) :-
  Other \= ok,
  ( Head = example_usage, Name = anonymous_example ;
    Head = example_usage(Name) ;
    Head = example_usage(Name, _Options)
  ),
  format("~n% Module ~w: '~w' ~n:- ~w. % --> ~w~n", [Module, Name, Body, Other]).


result_summary(error(existence_error(procedure, Clause), Impl), does_not_exist(Clause, Context)) :-
  !,
  error_summary(Impl, Context).

result_summary(error(Err, Impl), Err-Summary) :-
  error_summary(Impl, Summary).

result_summary(ok(_), ok).
result_summary(failed, failed).

error_summary(Impl, Summary) :-
  Impl = context(Summary,_) *-> true ;
  Impl = Summary.
