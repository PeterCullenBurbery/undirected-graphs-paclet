(* ::Package:: *)

BeginPackage["PeterBurbery`UndirectedGraphs`"];

(* Declare your packages public symbols here. *)

OddNodes;
Girth;
Begin["`Private`"];

(* Define your public and private symbols here. *)
OddNodes[graph_?(UndirectedGraphQ[#]\[And]ConnectedGraphQ[#]&)]:=VertexList[graph,u_/;OddQ[VertexDegree[graph,u]]]
Girth[graph_?GraphQ]:=Min[Length/@FindCycle[graph,Infinity,All]]
End[]; (* End `Private` *)

EndPackage[];
