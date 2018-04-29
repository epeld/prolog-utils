
:- module(foo, []).
:- use_module(example).

example:example_usage :-
  foobar(1,1), !.

foobar(1,2).
foobar(_X,15).
foobar(X, Y) :-
  X = Y.


baz(X) :-
  foobar(X, X).


mycall(direct_call(Term), direct_call(Term)) :-
  !.

mycall(true, true) :-
  !.

mycall((Term1, Term2), (Trace1, Trace2)) :-
  !,
  mycall(Term1, Trace1),
  mycall(Term2, Trace2).

mycall(Term, (Term -> Trace0)) :-
  catch(
    clause(Term, Body),
    error(permission_error(_Access, _Proc, _Pred), _Context),
    (
      call(Term),
      Body = direct_call(Term)
    )
  ),
  mycall(Body, Trace0).


:- forall(member(X, [1,2,3]), format("Foo ~w~n", [X])).
