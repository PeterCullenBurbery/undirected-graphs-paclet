BeginPackage["PeterBurbery`UndirectedGraphs`"]

GraphicalDegreeSequenceQ::usage="GraphicalDegreeSequenceQ[seq] returns True when seq is a graphical degree sequence, and False otherwise.";

Begin["`Private`"]

GraphicalDegreeSequenceQ//ClearAll

GraphicalDegreeSequenceQ[sequence:{___Integer}]:=EvenQ[Total[sequence]]&&Module[{orderedSequence},orderedSequence=ReverseSort[sequence];ContainsOnly[(k|->( Sum[orderedSequence[[i]],{i,1,k}]<=k(k-1)+Sum[Min[{orderedSequence[[i]],k}],{i,k+1,Length[sequence]}]))/@Range[Length[sequence]],{True}]]

End[]

EndPackage[]