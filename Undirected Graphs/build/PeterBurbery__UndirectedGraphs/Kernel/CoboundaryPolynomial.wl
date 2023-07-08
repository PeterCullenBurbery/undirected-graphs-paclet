(* ::Package:: *)

BeginPackage["PeterBurbery`UndirectedGraphs`"]

PeterBurbery`UndirectedGraphs`CoboundaryPolynomial

Begin["`Private`"]

CoboundaryPolynomial // ClearAll

CoboundaryPolynomial::usage = "CoboundaryPolynomial[graph] returns the coboundary polynomial of graph.\nCoboundaryPolynomial[graph,{indeterminate1,indeterminate2}] returns the bivariate coboundary polynomial of graph with the indeterminates indeterminatei.";

CoboundaryPolynomial[graph_?GraphQ, Optional[{firstIndeterminate_, secondIndeterminate_
    }, {\[FormalQ], \[FormalT]}]] :=
    Module[{tuttePolynomial, vertexCount, connectedComponents, connectedComponentsCount
        },
        tuttePolynomial = TuttePolynomial[graph, {(firstIndeterminate
             + secondIndeterminate - 1) / (secondIndeterminate - 1), secondIndeterminate
            }];
        connectedComponents = ConnectedGraphComponents[graph];
(*The number of connected components is denoted by Subscript[c, G]
 at MathWorld*)
        connectedComponentsCount = Length[connectedComponents];
(*The number of vertexes is denoted by Subscript[n, G] at MathWorld
    *)
        vertexCount = VertexCount[graph];
(*This is based on Subscript[Overscript[\[Chi], _], G](q,t)=(t-1)^(
Subscript[n, G]-Subscript[c, G]) Subscript[T, G]((q+t-1)/(t-1),t)
 where T is the Tutte polynomial from MathWorld.*)
        Simplify[(secondIndeterminate - 1) ^ (vertexCount - connectedComponentsCount
            ) tuttePolynomial]
    ]

CoboundaryPolynomial[graph_?GraphQ /; Quiet[MatchQ[ToEntity[graph], _Entity
    ], ToEntity::noentp], Optional[{firstIndeterminate_, secondIndeterminate_
    }, {\[FormalQ], \[FormalT]}]] :=
    GraphData[
        CanonicalName @ ToEntity[graph]
                 (*This is to get {"Barbell",
            
20} from Entity["Graph",{"Barbell",20}], for example*) ,
        "CoboundaryPolynomial"
    ][firstIndeterminate, secondIndeterminate]

End[]

EndPackage[]
