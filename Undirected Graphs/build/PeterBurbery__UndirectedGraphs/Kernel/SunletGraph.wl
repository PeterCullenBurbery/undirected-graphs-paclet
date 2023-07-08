(* ::Package:: *)

BeginPackage["PeterBurbery`UndirectedGraphs`"]

PeterBurbery`UndirectedGraphs`SunletGraph

Begin["`Private`"]

SunletGraph // ClearAll

SunletGraph::usage="SunletGraph[n] makes an n-sunlet graph.";

SunletGraph[n_?PositiveIntegerQ /; 3 <= n, opts : OptionsPattern[Graph
    ]] :=
    Graph[GraphProduct[CycleGraph[n], CompleteGraph[2], "Rooted"], opts,
         VertexCoordinates -> Join[MapThread[{#1, 1} -> #2&, {Range[n], CirclePoints[
        {0, 0}, 1/2 Csc[\[Pi] / n], n]}], MapThread[{#1, 2} -> #2&, {Range[1,
         n], CirclePoints[{0, 0}, 1/2 (2 + Csc[\[Pi] / n]), n]}]]]

End[]

EndPackage[]
