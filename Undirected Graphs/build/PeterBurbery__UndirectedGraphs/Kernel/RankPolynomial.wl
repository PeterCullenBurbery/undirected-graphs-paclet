BeginPackage["PeterBurbery`UndirectedGraphs`"]

PeterBurbery`UndirectedGraphs`RankPolynomial

Begin["`Private`"]

RankPolynomial // ClearAll

RankPolynomial::usage = "RankPolynomial[graph] computes the rank polynomial of graph. RankPolynomial[graph,{indeterminate1,indeterminate2}] computes the rank polynomial of graph with the indeterminates indeterminatei.";

RankPolynomial[
        graph_?GraphQ, Optional[{firstIndeterminate_, secondIndeterminate_
            }, {\[FormalX], \[FormalY]}](*{firstIndeterminate:\[FormalX],
secondIndeterminate:\[FormalY]}*)   (*I need to add the option for \
two indeterminates and set the default to something like \[FormalX], \
    
    
    
    
    
\[FormalY]*)] /;
    firstIndeterminate =!= (*its important to use UnsameQ here*)
        secondIndeterminate(*this is because I use \[FormalY]*):=
    Module[{tuttePolynomial, vertexCount, connectedComponents, connectedComponentsCount
        },
        tuttePolynomial = TuttePolynomial[graph, {firstIndeterminate 
            ^ -1 + 1, secondIndeterminate + 1}];
        connectedComponents = ConnectedGraphComponents[graph];
        connectedComponentsCount = Length[connectedComponents];
        vertexCount = VertexCount[graph];
        Simplify[
            firstIndeterminate ^
                    (
                        vertexCount(*n at MathWorld*)- connectedComponentsCount
                            (*c at MathWorld*) ) tuttePolynomial
        ]
    ]

RankPolynomial[args___] :=
    Null /; CheckArguments[RankPolynomial[args], {1, 2}]

End[]

EndPackage[]
