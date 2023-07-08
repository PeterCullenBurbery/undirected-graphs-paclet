(* ::Package:: *)

BeginPackage["PeterBurbery`UndirectedGraphs`"]

PeterBurbery`UndirectedGraphs`KayakPaddleGraph

Begin["`Private`"]

KayakPaddleGraph // ClearAll

KayakPaddleGraph::usage = "KayakPaddleGraph[{k,m,l}] makes a k, m, l kayak paddle graph.";

KayakPaddleGraph[{k_?PositiveIntegerQ /; 3 <= k, m_?PositiveIntegerQ 
    /; 3 <= m, l_?PositiveIntegerQ /; 2 <= l}, opts : OptionsPattern[Graph
    ]] :=
    Module[{tadpoleGraph, vertexList, numericalOrdering, unorderedVertexCoordinates,
         orderedVertexList, reorderedVertexListRules, unOrderedGraph, orderedCycleGraph,
         tadPoleGraphJoinedToCycleGraph, verticesToAdd, edgesToAdd, vertexCoordinatesRules,
         tadPoleGraphRules},
        tadpoleGraph = TadpoleGraph[{k, l}];
        vertexList = Range[k + l, k + l + m - 1];
        unorderedVertexCoordinates = CirclePoints[{1, (2 \[Pi]) / m +
             \[Pi]}, m] + Threaded[{l + 2, 0}];
        numericalOrdering = Ordering[unorderedVertexCoordinates, All,
             NumericalOrder[First[#1], First[#2]]&];
        orderedVertexList = vertexList[[numericalOrdering]];
        reorderedVertexListRules = Thread[orderedVertexList -> vertexList
            ];
        unOrderedGraph = VertexReplace[CycleGraph[m], Thread[Range[m]
             -> vertexList], VertexLabels -> Automatic, VertexCoordinates -> (Join[
            Thread[vertexList[[numericalOrdering]] -> (unorderedVertexCoordinates
            [[numericalOrdering]])]])];
        orderedCycleGraph = VertexReplace[unOrderedGraph, reorderedVertexListRules
            ];
        verticesToAdd = VertexList[orderedCycleGraph];
        edgesToAdd = EdgeList[orderedCycleGraph];
        vertexCoordinatesRules = Thread[verticesToAdd -> GraphEmbedding[
            orderedCycleGraph]];
        tadPoleGraphRules = Thread[VertexList[tadpoleGraph] -> GraphEmbedding[
            tadpoleGraph]];
        tadPoleGraphJoinedToCycleGraph = EdgeAdd[tadpoleGraph, edgesToAdd,
             VertexCoordinates -> Join[tadPoleGraphRules, vertexCoordinatesRules],
             opts]
    ]

End[]

EndPackage[]
