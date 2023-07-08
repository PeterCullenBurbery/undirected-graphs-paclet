(* ::Package:: *)

BeginPackage["PeterBurbery`UndirectedGraphs`"]

PeterBurbery`UndirectedGraphs`ReliabilityPolynomial

Begin["`Private`"]

ReliabilityPolynomial // ClearAll

TadpoleGraph::usage = "TadpoleGraph[{m, n}] makes an m, n tadpole graph.";

TadpoleGraph[{m_?PositiveIntegerQ /; 3 <= m, n_?PositiveIntegerQ}, opts
     : OptionsPattern[Graph]] :=
    Graph[EdgeAdd[CycleGraph[m], UndirectedEdge @@@ Partition[Range[n
         + 1] + m - 1, 2, 1]], opts, VertexCoordinates -> Join[Thread[Range[m
        ] -> CirclePoints[{1, (2 \[Pi]) / m}, m]], MapThread[#1 -> {#2, 0}&, 
        {Range[m + 1, m + n], Range[2, n + 1]}]]]

End[]

EndPackage[]
