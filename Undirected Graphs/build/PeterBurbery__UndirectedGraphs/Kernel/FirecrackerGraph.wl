(* ::Package:: *)

BeginPackage["PeterBurbery`UndirectedGraphs`"]

PeterBurbery`UndirectedGraphs`FirecrackerGraph

Begin["`Private`"]

FirecrackerGraph // ClearAll

FirecrackerGraph::usage = "FirecrackerGraph[{n,k}] makes an n, k firecracker graph";

FirecrackerGraph[{n_?PositiveIntegerQ /; 2 <= n, k_?PositiveIntegerQ 
    /; 2 <= k}, opts : OptionsPattern[Graph]] :=
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
            extraEdges = UndirectedEdge @@@ Partition[bottomvertices,
                 2, 1];
            graphWithoutBottomVertex = Graph[GraphDisjointUnion @@ ConstantArray[
                StarGraph[\[FormalK]], \[FormalN]]];
            EdgeAdd[graphWithoutBottomVertex, extraEdges, opts, VertexCoordinates
                 -> Join[Join @@ Table[Thread[(Range[2, \[FormalK]] + i \[FormalK]) ->
                 (AngleVector[{1/2 + i 2, 1/2}, {1/2, #}]& /@ angleList)], {i, 0, \[FormalN]
                }], Table[(1 + i \[FormalK]) -> {1/2 + 2 i, 1/2}, {i, 0, \[FormalN] -
                 1}]]]
        ]
    ]

End[]

EndPackage[]



