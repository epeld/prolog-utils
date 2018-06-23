%
% This is a test app to try and parse som PDF-code
%
:- module(graph_app, []).
:- use_module(pdffile).
:- use_module(pdf).

:- set_prolog_flag(double_quotes, codes).
:- set_prolog_flag(backtrace_depth, 40).
:- set_prolog_flag(backtrace_goal_depth, 5).
:- set_prolog_flag(debugger_write_options,
                   [quoted(true), portray(true), max_depth(30), priority(699)]).


test_app :-
  pdffile:with_file_context("/home/erik/Downloads/halmos.pdf",
                           graph_app:do_something).


do_something(Context) :-
  format("~n// ---- Start of Graph -----~ndigraph {~n"),
  % First, write object types
  forall(
    context_reify_object(Context, Ref, Object),
    (
      pdffile:find_object_type(Object, Type),
      pretty_reference_with_label(Ref, Type, Pretty, Label),
      format("\"~w\" [ label=\"~w\" ];~n", [ Pretty, Label ])
    )
  ),
  % Write relations
  forall(
    pdffile:context_reify_object(Context, Ref, Object),
    (
      pdf:all_object_references(Object, Refs),
      maplist(pretty_reference, Refs, PrettyRefs),
      pretty_reference(Ref, PrettyRef),
      forall(
        member(R, PrettyRefs),
        (
          format("\"~w\" -> \"~w\";~n", [PrettyRef, R])
        )
      )
    )
  ),
  format("}~n// ---- End of Graph -----~n").


%
% Graph Predicates
%
graph_from_objects(Objects, Graph) :-
  findall(
    (Ref->Ref2),
    (
      member(Object, Objects),
      Object = object(Ref, _1, _2),
      pdf:object_references(Object, Ref2)
    ),
    Graph
  ).

%
% Pretty Printing Predicates
%
pretty_reference(reference(X,Y), Pretty) :-
  format(string(Pretty), "~w_~w_obj", [X,Y]).

pretty_reference_with_label(reference(X,Y), Type, Pretty, Label) :-
  format(string(Pretty), "~w_~w_obj", [X,Y]),
  format(string(Label), "~w_~w_~w", [X,Y,Type]).

