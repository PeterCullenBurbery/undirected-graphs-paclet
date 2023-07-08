(* ::Package:: *)

BeginPackage["PeterBurbery`UndirectedGraphs`"]

PeterBurbery`UndirectedGraphs`HelmGraph

Begin["`Private`"]

HelmGraph // ClearAll

HelmGraph::usage = "HelmGraph[n] makes an n-helm graph.";

HelmGraph[n_?PositiveIntegerQ /; 3 <= n, opts : OptionsPattern[Graph]
    ] :=
    Module[{sunletGraph, replacedGraph},
        sunletGraph = SunletGraph[n];
        replacedGraph = VertexReplace[sunletGraph, MapIndexed[#1 -> Identity
             @@ #2&, VertexList[sunletGraph]]];
        Graph[EdgeAdd[replacedGraph, Table[2 n + 1 \[UndirectedEdge] 
            vertex, {vertex, 1, 2 n, 2}]], opts, VertexCoordinates -> {2 n + 1 ->
             {0, 0}}]
    ]

End[]

EndPackage[]
