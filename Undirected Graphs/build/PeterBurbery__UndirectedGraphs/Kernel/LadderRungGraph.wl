(* ::Package:: *)

BeginPackage["PeterBurbery`UndirectedGraphs`"]

PeterBurbery`UndirectedGraphs`LadderRungGraph

Begin["`Private`"]

LadderRungGraph // ClearAll

LadderRungGraph::usage = "LadderRungGraph[n] returns a ladder rung graph with n rungs.";

LadderRungGraph[n_?PositiveIntegerQ, opts : OptionsPattern[Graph]] :=
    Graph[GraphDisjointUnion @@ ConstantArray[PathGraph[{1, 2}], n], 
        opts]

End[]

EndPackage[]
