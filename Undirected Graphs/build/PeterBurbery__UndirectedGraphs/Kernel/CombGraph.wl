(* ::Package:: *)

BeginPackage["PeterBurbery`UndirectedGraphs`"]

PeterBurbery`UndirectedGraphs`CombGraph

Begin["`Private`"]

CombGraph // ClearAll

CombGraph::usage = "CombGraph[n] makes an n-comb graph with n vertexes along the bottom.";

CombGraph[
    n_?PositiveIntegerQ(*you can't have a negative number or 0*) /; 2
         <= n
    ,
    opts : OptionsPattern[Graph]
] :=
    Module[{gridGraph},
        gridGraph = GridGraph[{2, n}];
        GraphDifference[
            gridGraph
            ,
            PathGraph[2 Array[#&, n]]
            ,
            opts                       (*I tried putting opts after VertexCoordinates, 
                
                
                
                
   but this didn't allow overriding whereas putting for example \
VertexCoordinates->Automatic then VertexCoordinates-><*custom list*> 
    
    
    
    
   did allow me to override the default*) ,
            VertexCoordinates -> ResourceFunction["IntegerChop"][ResourceFunction[
                "VertexCoordinateList"][gridGraph]]
        ]
    ]

CombGraph[args___] :=
    Null /; CheckArguments[CombGraph[args], {1, 1}]

End[]

EndPackage[]
