(* ::Package:: *)

BeginPackage["PeterBurbery`UndirectedGraphs`"]

PeterBurbery`UndirectedGraphs`AlternatingTreeGraph

Begin["`Private`"]

AlternatingTreeGraph // ClearAll

AlternatingTreeGraph::usage = "AlternatingTreeGraph[n] generates an alternating tree graph from a path graph with n vertices.";

AlternatingTreeGraph[n_?IntegerQ /; n > 0, options : OptionsPattern[Graph
    ]] :=
    Graph[Module[{pathgraph, top, bottom},
        pathgraph = PathGraph[Range[n]];
        top = UndirectedEdge @@@ Transpose[{Range[2, n - 1], Range[n 
            + 1, 2 n - 2]}];
        bottom = UndirectedEdge @@@ Transpose[{Range[2, n - 1], Range[
            2 n - 1, 3 n - 4]}];
        IndexGraph[EdgeAdd[pathgraph, Join[top, bottom], options, GraphLayout
             -> "SpringElectricalEmbedding"]]
    ],options]

End[]

EndPackage[]
