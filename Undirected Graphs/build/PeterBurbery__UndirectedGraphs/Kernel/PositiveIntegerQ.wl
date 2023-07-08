BeginPackage["PeterBurbery`UndirectedGraphs`"]

PeterBurbery`UndirectedGraphs`PositiveIntegerQ

Begin["`Private`"]

ClearAll[PositiveIntegerQ]

PositiveIntegerQ::usage = "PositiveIntegerQ[n] yields True when n is a strictly positive integer. PositiveInteger[0] returns False.";

PositiveIntegerQ[n_] :=
    And @@ {IntegerQ[n], TrueQ[n \[Element] PositiveIntegers]}

End[]

EndPackage[]
