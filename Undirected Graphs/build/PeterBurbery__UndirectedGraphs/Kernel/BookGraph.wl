(* ::Package:: *)

BeginPackage["PeterBurbery`UndirectedGraphs`"]

PeterBurbery`UndirectedGraphs`BookGraph

Begin["`Private`"]

BookGraph // ClearAll

BookGraph::usage = "BookGraph[m] generates an m-book graph.";

BookGraph[m_?PositiveIntegerQ, opts : OptionsPattern[Graph]] :=
    GraphProduct[StarGraph[m + 1], PathGraph[{1, 2}], "Cartesian", opts
        ]

End[]

EndPackage[]
