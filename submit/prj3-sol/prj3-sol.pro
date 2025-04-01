#!/usr/bin/env -S swipl
%-*- mode: prolog; -*-

:- module(prj3_sol,  [
     get_divisible/3, has_length_n_lists/2, poly_eval/3, plus_expr/2,
     poly_expr/3, element_at/3, dag_path/4, graph_path1/4, graph_path2/4
   ]).


/*********************** IMPORTANT RESTRICTIONS ************************/

/*

You may make use of the binary operators `=` (unify), `\=` (not
unify), `=:=` (number equality), `=\=` (number inequality), is/2,
arithmetic operators, append/3, member/2, length/2 but are *not*
allowed to use any other features or built-in Prolog procedures.
Violating this restriction will result in a zero on that exercise.

You are not allowed to use any of Prolog's higher-order features or
extra-logical control features like or, cut, if-then. Violating this
restriction will result in a *zero grade* for the *entire project*.

Unless stated otherwise, you may introduce auxiliary helper
procedures.

*/


% #1: "10-points"
% get_divisible(IntList, N, DivList): DivList is a sub-list of
% those elements of integer list IntList which are divisible by
% integer N.
% The elements in DivList must be in the same order in which they occur
% in IntList.
% Hints: A mod B results in the modulus of A wrt B when used
% in an arithmetic context.z
get_divisible([], _, []).
get_divisible([Head|RestOfIntList], N, [Head|RestOfDivList]) :-
    Head mod N =:= 0,
    get_divisible(RestOfIntList, N, RestOfDivList).
get_divisible([Head|RestOfIntList], N, RestOfDivList) :-
    Head mod N =\= 0,
    get_divisible(RestOfIntList, N, RestOfDivList).

% #2: "10-points"
% has_length_n_lists(Lists, N): Lists is a list of lists
% such that the length of the next list is N + the length
% of the previous list.
has_length_n_lists([], _).
has_length_n_lists([_], _).
has_length_n_lists([List1, List2|RestOfLists], N) :-
    length(List1, Length1),
    length(List2, Length2),
    Length1 + N =:= Length2,
    has_length_n_lists([List2|RestOfLists], N).

% #3: "10-points"
% poly_eval(Coeffs, X, Eval): Given a numeric list Coeffs of
% polynomial coefficients [ C_0, C_1, C_2, ... ], Eval
% matches the value of the polynomial at X, namely
% C_0*X^0 + C_1*X^1 + C_2*X^2 + C_3*X^3.
% *Restriction*: must be tail-recursive and cannot use exponentiation.
% Hint: use an auxiliary procedure.
poly_eval(Coeffs, X, Eval) :-
    poly_eval_aux(Coeffs, X, 0, 1, Eval).

poly_eval_aux([], _, Acc, _, Acc).
poly_eval_aux([CurrCoeff|RestOfCoeffs], X, Acc, PowerValue, Eval) :-
    UpdatedAcc is Acc + CurrCoeff * PowerValue,
    UpdatedPower is PowerValue * X,
    poly_eval_aux(RestOfCoeffs, X, UpdatedAcc, UpdatedPower, Eval).


% #4: "10-points"
% plus_expr(Terms, AddExpr): Given a non-empty list Terms of Prolog
% terms T1, T2, ..., Tn, AddExpr matches T1 + T2 + ... + Tn.
% *Hint*: + is left-associative.
plus_expr([SingleTerm], SingleTerm).
plus_expr([FirstTerm, SecondTerm|RestOfTerms], AddExpr) :-
    plus_expr([FirstTerm+SecondTerm|RestOfTerms], AddExpr).

% #5: "10-points"
% poly_expr(Coeffs, X, Poly): Given a non-empty list of coefficients
% C1, C2], ..., CN and some
% Prolog term X, Poly is the expression:
% C1*X**0 + C2*X**1 + ... + CN*X**(N - 1)
% Hint: structure your code as for plus_expr/2.
poly_expr(Coeffs, X, Poly) :-
    create_poly_terms(Coeffs, X, 0, ResultingTerms),
    add_terms_together(ResultingTerms, Poly).

create_poly_terms([], _, _, []).
create_poly_terms([CurrCoeff|RestOfCoeffs], X, PowerValue, [CurrCoeff*X**PowerValue|RestOfTerms]) :-
    NextPowerValue is PowerValue + 1,
    create_poly_terms(RestOfCoeffs, X, NextPowerValue, RestOfTerms).

add_terms_together([SingleTerm], SingleTerm).
add_terms_together([FirstTerm, SecondTerm|RestOfTerms], ResultingValue) :-
    add_terms_together([FirstTerm+SecondTerm|RestOfTerms], ResultingValue).

% #6: "15-points"
% element_at(List, Index, Element): List is a list whose Index
% (0-origin) element matches Element.
% *Must* be implemented using a single rule.
element_at([HeadElement|_], 0, HeadElement).
element_at([_|RestOfList], Index, Element) :-
    Index > 0,
    NextElementIndex is Index - 1,
    element_at(RestOfList, NextElementIndex, Element).

%% A graph is represented as a list of edges where an edge is
%% represented as a structure edge(From, To) representing an edge from
%% node From to node To.  

% #7: "10-points"
% dag_path(Dag, From, To, Path): Path is a list of edges specifying a
% path from node From to node To in DAG Dag.  It should be assumed
% that Dag, From, To are ground (no variables).
% The implementation *must* take advantage of the fact that Dag is a
% DAG.
dag_path(_, From, From, []).
dag_path(Dag, From, To, [edge(From, NextNode)|Path]) :-
    member(edge(From, NextNode), Dag),
    dag_path(Dag, NextNode, To, Path).

% #8: "15-points"
% graph_path1(Graph, From, To, Path): Path is a list of edges
% specifying an acyclic path from node From to node To in unrestricted
% graph Graph.  It should be assumed that Graph, From, To are ground
% (no variables).
% *Must* be implemented by tracking previously visited nodes to prevent
% getting caught up within cycles in Graph.
graph_path1(Graph, From, To, Path) :-
    graph_path1(Graph, From, To, [], Path).

graph_path1(_, From, From, _, []).
graph_path1(Graph, From, To, VisitedList, [edge(From, NextNode)|Path]) :-
    member(edge(From, NextNode), Graph),
    \+ member(NextNode, VisitedList),
    graph_path1(Graph, NextNode, To, [From|VisitedList], Path).

% #9: "10-points"
% graph_path2(Graph, From, To, Path): Path is a list of edges from node
% From to node To in unrestricted graph Graph.  It should be assumed
% that Graph, From, To are ground (no variables).
% *Must* be implemented without tracking previously visited nodes.
% *Hint*: a small variation of dag_path/4.
graph_path2(Graph, From, To, Path) :-
    length(Path, _),
    graph_path2_traversal(Graph, From, To, Path).

graph_path2_traversal(_, From, From, []).
graph_path2_traversal(Graph, From, To, [edge(From, NextNode)|Path]) :-
    member(edge(From, NextNode), Graph),
    graph_path2_traversal(Graph, NextNode, To, Path).