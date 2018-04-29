:- module(printing, [suggest/2]).
:- use_module(library(listing)).

%
% List all predicates that match a partial string
all_suggestions(Partial, Suggestions) :-
  setof(S, suggest(Partial, S), Suggestions0),
  sort(Suggestions0, Suggestions).

%
% Backtracks over predicate-suggestions matching 'Partial'
suggest(Partial, Suggestion) :-
  public_predicate(Suggestion),
  Suggestion = Name / _Arity,
  sub_string(Name, _B, _L, _A, Partial).


%
% Backtrack over all modules (and their public predicates)
public_predicate(Module, Indicator) :-
  module_property(Module, exports(Exported)),
  member(Indicator, Exported).



%
% Works like member/2 but on (lists, like, these) instead.
% the "list" must be instantiated
comma_member(X, (_M : P)) :-
  comma_member(X, P).

comma_member(X, (A ; B)) :-
  comma_member(X, A) ;
  comma_member(X, B).

comma_member(X, (A, B)) :-
  !,
  (
    X = A ;
    comma_member(X, B)
  ).

comma_member(X, X) :-
  X \= (_A, _B).


functor_indicator(Functor, Name/Arity) :-
  functor(Functor, Name, Arity).
