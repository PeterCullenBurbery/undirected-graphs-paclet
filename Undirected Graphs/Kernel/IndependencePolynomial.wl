(* ::Package:: *)

BeginPackage["PeterBurbery`UndirectedGraphs`"]

PeterBurbery`UndirectedGraphs`IndependencePolynomial

Begin["`Private`"]

IndependencePolynomial // ClearAll

IndependencePolynomial::usage = "IndependencePolynomial[g] computes the independence polynomial of graph g.\nIndependencePolynomial[g, indeterminate] computes the independence polynomial of graph g with indeterminate.";

IndependencePolynomial[graph_?GraphQ, Optional[indeterminate_, \[FormalX]
    ]] :=
    Total[KeyValueMap[indeterminate^#1 Length[#2]&, GroupBy[Length][Select[
        IndependentVertexSetQ[graph, #]&][Subsets[VertexList[graph]]]]]]

(*Here I use the precomputed value if there is an entity for that \
graph*)

IndependencePolynomial[graph_?GraphQ /; Quiet[MatchQ[ToEntity[graph],
     _Entity], ToEntity::noentp], Optional[indeterminate_, \[FormalX]]] :=
    GraphData[
        CanonicalName @ ToEntity[graph]
                 (*This is to get {"Barbell",
            
20} from Entity["Graph",{"Barbell",20}], for example*) ,
        "IndependencePolynomial"
    ][indeterminate]

End[]

EndPackage[]



