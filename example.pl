%
% This module defines utilities for specifying short usage cases / tests
%
:- module(example, [example_usage/0]).
:- use_module(library(pprint)).

:- multifile example_usage/0.

run_tests :-
  usage_example_results(S, F, E),
  print_results("Successes", S),
  print_results("Failures", F),
  print_results("Errors", E).


usage_examples(Examples) :-
  bagof(
    Example,
    clause(example_usage, Example),
    Examples
  ).


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


run_case(Case, Result) :-
  catch(
    (
      call(Case) *->
      (
	!,
	Result = ok(choicepoints_discarded)
      );
      Result = failed
    ),
    Error,
    Result = Error
  ).


print_results(Heading,  []) :-
  !,
  format("~n~n--- NO ~w ---~n", [Heading]).

print_results(Heading,  Examples) :-
  length(Examples, Length),
  format("~n~n--- ~w ~w ---~n", [Length, Heading]),
  (
    Examples = [] *-> format("<none>~n") ;
    forall(
      member(result(Module:Example, Result), Examples),
      (
	result_summary(Result, Summary),
	format("~n% In module '~w'~n:- ~w. % --> ~w~n", [Module, Example, Summary])
      )
    )
  ).

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
