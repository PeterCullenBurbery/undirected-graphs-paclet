(* ::Package:: *)

BeginPackage["PeterBurbery`UndirectedGraphs`"];

(* Declare your packages public symbols here. *)

OddNodes;
Girth;
GraphPredicateData;
TakeLargestGraphComponentBy;
GraphicalDegreeSequenceQ;

GeneralizedGraphData;

GraphConvexHull;

ResistanceMatrix;

RandomCustomGraph;

Begin["`Private`"];

(* Define your public and private symbols here. *)

GraphPredicateData[graph_?GraphQ]:=<|"Acyclic"->AcyclicGraphQ[graph],"Bipartite"->BipartiteGraphQ[graph],"Complete"->CompleteGraphQ[graph],"Connected"->ConnectedGraphQ[graph],"EdgeTransitive"->EdgeTransitiveGraphQ[graph],"WeightedEdge"->EdgeWeightedGraphQ[graph],"Empty"->EmptyGraphQ[graph],"Eulerian"->EulerianGraphQ[graph],"Hamiltonian"->HamiltonianGraphQ[graph],"LoopFree"->LoopFreeGraphQ[graph],"Mixed"->MixedGraphQ[graph],"Path"->PathGraphQ[graph],"Planar"->PlanarGraphQ[graph],"Simple"->SimpleGraphQ[graph],"Tree"->TreeGraphQ[graph],"Undirected"->UndirectedGraphQ[graph],"VertexTransitive"->VertexTransitiveGraphQ[graph],"WeightedVertex"->VertexWeightedGraphQ[graph],"WeaklyConnected"->WeaklyConnectedGraphQ[graph],"Weighted"->WeightedGraphQ[graph]|>



OddNodes[graph_?(UndirectedGraphQ[#]\[And]ConnectedGraphQ[#]&)]:=VertexList[graph,u_/;OddQ[VertexDegree[graph,u]]]

Girth[graph_?GraphQ]:=Min[Length/@FindCycle[graph,Infinity,All]]

TakeLargestGraphComponentBy[graph_?GraphQ,function_:EdgeCount,length_:1]:=TakeLargestBy[ConnectedGraphComponents[graph],function,length]

GraphicalDegreeSequenceQ[sequence:{___Integer}]:=EvenQ[Total[sequence]]&&Block[{orderedSequence},orderedSequence=ReverseSort[sequence];ContainsOnly[(k|->( Sum[orderedSequence[[i]],{i,1,k}]<=k(k-1)+Sum[Min[{orderedSequence[[i]],k}],{i,k+1,Length[sequence]}]))/@Range[Length[sequence]],{True}]]

GraphConvexHull[graph_?GraphQ,vertexSet_]:=FixedPoint[Function[in,Union@Flatten@Function[{g,v},FindPath[g,First[#],Last[#],GraphDistance[g,First[#],Last[#]],All]&/@Subsets[v,{2}]][graph,in]][#]&,vertexSet]/;SubsetQ[VertexList[graph],vertexSet]

GeneralizedGraphData[graph_?GraphQ]:=AssociationThread[{"IncidenceMatrix","Order","Size","Nodes","Edges","AdjacencyMatrix","GraphComplement","GraphCenter","GraphRadius","GraphDiameter","GraphPeriphery"}->{IncidenceMatrix[graph],VertexCount[graph],EdgeCount[graph],VertexList[graph],EdgeList[graph],AdjacencyMatrix[graph],GraphComplement[graph],GraphCenter[graph],GraphRadius[graph],GraphPeriphery[graph],GraphDiameter[graph]}]

ResistanceMatrix[graph_?GraphQ]:=With[{\[CapitalGamma]=PseudoInverse[N@KirchhoffMatrix[graph]]},Outer[Plus,Diagonal[\[CapitalGamma]],Diagonal[\[CapitalGamma]]]-\[CapitalGamma]-Transpose[\[CapitalGamma]]]

RandomCustomGraph[{nodes_,edges_},quality_]:=NestWhile[RandomGraph[{nodes,edges}]&,Null,!quality,1,2980]

End[]; (* End `Private` *)

EndPackage[];
