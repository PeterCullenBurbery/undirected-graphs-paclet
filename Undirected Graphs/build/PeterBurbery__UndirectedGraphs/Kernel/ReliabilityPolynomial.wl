(* ::Package:: *)

BeginPackage["PeterBurbery`UndirectedGraphs`"]

PeterBurbery`UndirectedGraphs`ReliabilityPolynomial

Begin["`Private`"]

ReliabilityPolynomial // ClearAll

ReliabilityPolynomial::usage="ReliabilityPolynomial[graph] gives the reliability polynomial of graph. ReliabilityPolynomial[graph,indeterminate] gives the reliability polynomial of graph with the indeterminate indeterminate.";

ReliabilityPolynomial[graph_?GraphQ, Optional[firstIndeterminate_, \[FormalP]
     ]] :=
     Module[{tuttePolynomial, vertexCount, connectedComponents, connectedComponentsCount,
           edgeCount},
          tuttePolynomial = TuttePolynomial[graph, {1, firstIndeterminate
                ^ -1}];
          connectedComponents = ConnectedGraphComponents[graph];
          (*The number of connected components is denoted by c at MathWorld
               *)
          connectedComponentsCount = Length[connectedComponents];
          (*The number of vertexes is denoted by n at MathWorld*)
          vertexCount = VertexCount[graph];
          (*The number of edges is denoted by m at MathWorld*)
          edgeCount = EdgeCount[graph];
(*This is based on C(p)=(1-p)^(n-c) p^(m-n+c) T(1, p^-1)
where T is the Tutte polynomial from MathWorld.*)
          Simplify[(1 - firstIndeterminate) ^ (vertexCount - connectedComponentsCount
               ) firstIndeterminate ^ (edgeCount - vertexCount + connectedComponentsCount
               ) tuttePolynomial]
     ]

End[]

EndPackage[]



