(* ::Package:: *)

BeginPackage["PeterBurbery`UndirectedGraphs`"]

PeterBurbery`UndirectedGraphs`ResistanceMatrix
Begin["`Private`"]

ResistanceMatrix // ClearAll

ResistanceMatrix::usage="ResistanceMatrix[graph] returns the resistance matrix of graph.";

ResistanceMatrix[g_?GraphQ] := 
 Module[{kirchhoffMatrix, pseudoInverse, diagonal}, 
  kirchhoffMatrix = KirchhoffMatrix[g]; 
  pseudoInverse = PseudoInverse[kirchhoffMatrix];
  diagonal = Diagonal[pseudoInverse];
  SparseArray[
   Outer[Plus, diagonal, diagonal] - pseudoInverse - 
    Transpose[pseudoInverse]]]

End[]

EndPackage[]




