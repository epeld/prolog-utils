:- module(graph, []).

subgraph_of(Node, Graph, Subgraph) :-
  subgraphs(Graph, Subgraphs),
  member(Subgraph, Subgraphs),
  has_edge(Subgraph, Node).

subgraphs([Edge | Graph], [Subgraph | Subgraphs]) :-
  subgraph(Graph, [Edge], Subgraph),
  subtract(Graph, Subgraph, Graph2),
  subgraphs(Graph2, Subgraphs).

subgraphs([], []).

subgraph(Graph0, Subgraph0, Subgraph) :-
  partition(connected_to(Subgraph0), Graph0, Included, Rest),
  (
    (
      Included = [],
      Subgraph0 = Subgraph
    )
  ;
  (
    union(Included, Subgraph0, Subgraph1),
    subgraph(Rest, Subgraph1, Subgraph)
  )).


has_edge(G, Node) :-
  member(Node -> _Other, G).

has_edge(G, Node) :-
  member(_Other -> Node, G).

connected_to(G, (N -> _N2)) :-
  has_edge(G, N).

connected_to(G, (_N -> N2)) :-
  has_edge(G, N2).


:- begin_tests(graph).


test(subgraphs, nondet) :-
  subgraphs(
    [a->b, d->e, b->c, c->f],
    Subgraphs
  ),
  length(Subgraphs, 2),
  member(G, Subgraphs),
  member(a->b, G),
  member(b->c, G),
  member(c->f, G).


test(subgraph_of, nondet) :-
  subgraph_of(
    g,
    [a->b, d->e, b->c, c->f, e->g],
    G
  ),
  length(G, 2),
  member(d->e, G),
  member(e->g, G).

test(subgraph_of_2, nondet) :-
  subgraph_of(
    c,
    [a->b, d->e, b->c, c->f, e->g],
    G
  ),
  length(G, 3),
  member(a->b, G),
  member(b->c, G).

:- end_tests(graph).
