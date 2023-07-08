(* ::Package:: *)

BeginPackage["PeterBurbery`UndirectedGraphs`"]

PeterBurbery`UndirectedGraphs`VertexInsert

Begin["`Private`"]

VertexInsert // ClearAll

VertexInsert::usage = "VertexInsert[graph,edge,vertex] inserts vertex into edge in graph. VertexInsert[graph,edge,vertex,\"DoNotInsertIntoMultipleEdges\"] inserts a vertex into just one edge when there are multiple edges between two vertices.";

(*I'm adding GraphLayout->Automatic to avoid mishapen graphs.*)

VertexInsert[graph_?GraphQ, edge : UndirectedEdge[u_, v_], vertex_, opts
     : OptionsPattern[Graph]] /; !VertexQ[graph, vertex] && EdgeQ[graph, 
    edge] :=
    VertexInsert[graph, edge, vertex, "DoNotInsertIntoMultipleEdges",
         opts]

VertexInsert[graph_?GraphQ, edge : UndirectedEdge[u_, v_], vertex_, "DoNotInsertIntoMultipleEdges",
     opts : OptionsPattern[Graph]] /; !VertexQ[graph, vertex] && EdgeQ[graph,
     edge] :=
    EdgeDelete[
        EdgeAdd[graph, {u \[UndirectedEdge] vertex, vertex \[UndirectedEdge]
             v}], edge, opts(*,GraphLayout->
Automatic*) ]

VertexInsert[args___] :=
    Null /; CheckArguments[VertexInsert[args], {3, 4}]

End[]

EndPackage[]
