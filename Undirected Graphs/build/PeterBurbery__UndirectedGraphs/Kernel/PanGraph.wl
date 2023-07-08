(* ::Package:: *)

BeginPackage["PeterBurbery`UndirectedGraphs`"]

PeterBurbery`UndirectedGraphs`PanGraph

Begin["`Private`"]

PanGraph // ClearAll

PanGraph::usage = "PanGraph[n] makes an n pan graph.";

PanGraph[n_?PositiveIntegerQ /; 3 <= n] :=
    TadpoleGraph[{n, 1}]

End[]

EndPackage[]
