(* ::Package:: *)

BeginPackage["PeterBurbery`UndirectedGraphs`"]

PeterBurbery`UndirectedGraphs`TadpoleGraph

Begin["`Private`"]

Girth // ClearAll

Girth::usage = "Girth[g] returns the girth of the graph g, i.e. the length of the shortest cycle in g.";

(*If g is acyclic, then Girth[g] returns Infinity. caused problems*)

Girth[graph_?GraphQ] :=
    Min[Length /@ FindCycle[graph, Infinity, All]]

End[]

EndPackage[]
