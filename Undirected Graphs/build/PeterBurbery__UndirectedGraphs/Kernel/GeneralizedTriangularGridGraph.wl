(* ::Package:: *)

BeginPackage["PeterBurbery`UndirectedGraphs`"]

PeterBurbery`UndirectedGraphs`GeneralizedTriangularGridGraph

Begin["`Private`"]

ClearAll[TriangularGridGraph]

TriangularGridGraph[{wide_Integer?Positive, high_Integer?Positive}, opts
     : OptionsPattern[Graph]] :=
    Module[{cells, edges, vertices},
        cells = Flatten[Table[CirclePoints[{Sqrt[3] (2 j + k - 2), Times[
            3, k] - 2}, {2, \[Pi] / 2}, 3], {j, wide}, {k, high}], 1];
        edges = Union[Sort /@ Flatten[Partition[#, 2, 1, 1]& /@ cells,
             1]];
        vertices = Union[Flatten[edges, 1]];
        IndexGraph[Graph[UndirectedEdge @@@ edges, opts, VertexCoordinates
             -> Thread[vertices -> vertices]]]
    ]

ClearAll[GeneralizedTriangularGridGraph]

GeneralizedTriangularGridGraph::usage = "GeneralizedTriangularGridGraph[{width, height}] generates a triangular grid graph with the specified width and height. The width and height should be positive integers.";

GeneralizedTriangularGridGraph[input : {m_?PositiveIntegerQ, n_?PositiveIntegerQ
    }, opts : OptionsPattern[Graph]] :=
    Module[{vertexCoordinates, triangularGridGraph, vertices, vertexCoordinatesToVerticesAssociation,
         topPartDeletedGraph, subGraph, verticesToKeep},
        triangularGridGraph = TriangularGridGraph[input + 1]
                                                             (*This line creates a triangular grid graph that is \
            
            
            
            
            
            
slightly larger than the desired size.*);
        vertexCoordinates = VertexCoordinateList[triangularGridGraph]
            
                            (*This line gets the coordinates of each \
            
            
            
            
            
vertex in the graph.*);
        vertices = VertexList[triangularGridGraph]
                                                   (*This line gets a list of all vertices in \
            
            
            
            
            
            
the graph.*);
        topPartDeletedGraph = VertexDelete[triangularGridGraph, Flatten[
            PositionLargest[vertexCoordinates, 1, Order[Last[#1], Last[#2]]&]]]
                        (*-This line removes the top part of the \
graph.*);
        vertexCoordinatesToVerticesAssociation = AssociationThread[vertexCoordinates
             -> vertices]
             (*This line creates an association that maps each \
            
            
            
vertex to its coordinates.*);
        verticesToKeep = Lookup[vertexCoordinatesToVerticesAssociation,
             Catenate[Values[Most /@ SortBy[First] /@ GroupBy[Last][vertexCoordinates
            ]]]]                                                                 
               (*This line determines which vertices \
            
            
            
            
to keep in the final graph.*);
        subGraph = Subgraph[topPartDeletedGraph, verticesToKeep, opts
            ]
         (*This line creates a subgraph of the original graph that \
contains only the vertices that were determined to be kept in the \
previous line.*)]

End[]

EndPackage[]
