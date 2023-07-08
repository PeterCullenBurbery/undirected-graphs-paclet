(* ::Package:: *)

BeginPackage["PeterBurbery`UndirectedGraphs`"]

PeterBurbery`UndirectedGraphs`BananaTreeGraph

Begin["`Private`"]

BananaTreeGraph // ClearAll

BananaTreeGraph::usage = "BananaTreeGraph[{n,k}] makes an n, k banana tree graph.";

BananaTreeGraph[{n_?PositiveIntegerQ, k_?PositiveIntegerQ}, opts : OptionsPattern[
    Graph]] :=
    With[{\[FormalN] = n, \[FormalK] = k},
        Module[{bottomvertices, bottomvertex, extraEdges, graphWithoutBottomVertex,
             bottomVertexXCoordinate, bottomVertexYCoordinate, angleList},
            angleList = Most[Range[3/2 \[Pi], 2 Pi + 3/2 \[Pi], 2 \[Pi]
                 / (\[FormalK] - 1)]];
            bottomvertex = (\[FormalN] \[FormalK] + 1);
            bottomVertexXCoordinate = (2 \[FormalN] - 1) / 2;
            bottomVertexYCoordinate = 1/2 (-1 - (\[FormalN] - 2));
            bottomvertices = Table[2 + \[FormalK] * (-1 + i), {i, \[FormalN]
                }];
            extraEdges = Table[bottomvertex \[UndirectedEdge] vertex,
                 {vertex, bottomvertices}];
            graphWithoutBottomVertex = Graph[GraphDisjointUnion @@ ConstantArray[
                StarGraph[\[FormalK]], \[FormalN]]];
            EdgeAdd[graphWithoutBottomVertex, extraEdges, opts, VertexCoordinates
                 -> Join[Join @@ Table[Thread[(Range[2, \[FormalK]] + i \[FormalK]) ->
                 (AngleVector[{1/2 + i 2, 1/2}, {1/2, #}]& /@ angleList)], {i, 0, \[FormalN]
                }], Table[(1 + i \[FormalK]) -> {1/2 + 2 i, 1/2}, {i, 0, \[FormalN] -
                 1}], {bottomvertex -> {bottomVertexXCoordinate, bottomVertexYCoordinate
                }}]]
        ]
    ]                                                         (*Module[{baseStarGraphEdgeList},\
baseStarGraphEdgeList=List@@@EdgeList[StarGraph[k]];SimpleGraph[Join[\
UndirectedEdge@@@Catenate[Table[baseStarGraphEdgeList+k \
Threaded[{i,i}],{i,0,n-1}]],Table[n k+1\[UndirectedEdge]i,{i,1,n \
k,k}]],opts,GraphLayout->{"VertexLayout"->"BalloonEmbedding"}]]*)

BananaTreeGraph[args___] :=
    Null /; CheckArguments[BananaTreeGraph, {2, 2}]

End[]

EndPackage[]
