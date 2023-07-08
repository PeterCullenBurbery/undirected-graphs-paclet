(* ::Package:: *)

BeginPackage["PeterBurbery`UndirectedGraphs`"]

PeterBurbery`UndirectedGraphs`VertexCoordinateList

Begin["`Private`"]

ClearAll[VertexCoordinateList]

VertexCoordinateList::usage = "VertexCoordinateList[g] returns a list of vertex coordinates for the graph g. The coordinates are in the same order as the vertices in VertexList[g].";

VertexCoordinateList[g_] :=
    GraphEmbedding[g][[Last /@ Sort[Transpose[{VertexList[g], Range[VertexCount[
        g]]}]]]]

End[]

EndPackage[]
