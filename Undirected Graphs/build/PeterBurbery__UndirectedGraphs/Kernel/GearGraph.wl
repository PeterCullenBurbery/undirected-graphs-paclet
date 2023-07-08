(* ::Package:: *)

BeginPackage["PeterBurbery`UndirectedGraphs`"]

PeterBurbery`UndirectedGraphs`GearGraph

Begin["`Private`"]

GearGraph // ClearAll

GearGraph[3, opts : OptionsPattern[Graph]] :=
    Graph[\!\(\*
GraphicsBox[
NamespaceBox["NetworkGraphics",
DynamicModuleBox[{Typeset`graph = HoldComplete[
Graph[{1, 2, 3, 4, 5, 6, 7}, {Null, 
SparseArray[
         Automatic, {7, 7}, 
          0, {1, {{0, 3, 6, 9, 12, 14, 16, 18}, {{2}, {3}, {4}, {1}, {
            5}, {7}, {1}, {5}, {6}, {1}, {6}, {7}, {2}, {3}, {3}, {
            4}, {2}, {4}}}, Pattern}]}, {VertexCoordinates -> {{0, 
           0}, {Rational[1, 2] 3^Rational[1, 2], 
Rational[-1, 2]}, {0, 1}, {Rational[-1, 2] 3^Rational[1, 2], 
Rational[-1, 2]}, {Rational[1, 4] 3^Rational[1, 2], 
Rational[1, 4]}, {Rational[-1, 4] 3^Rational[1, 2], 
Rational[1, 4]}, {0, 
Rational[-1, 2]}}}]]}, 
TagBox[GraphicsGroupBox[
        GraphicsComplexBox[{{0., 0.}, {0.8660254037844386, -0.5}, {0.,
          1.}, {-0.8660254037844386, -0.5}, {0.4330127018922193, 
         0.25}, {-0.4330127018922193, 0.25}, {0., -0.5}}, {
{Hue[0.6, 0.7, 0.5], Opacity[0.7], Arrowheads[0.], 
           ArrowBox[{{1, 2}, {1, 3}, {1, 4}, {2, 5}, {2, 7}, {3, 5}, {
            3, 6}, {4, 6}, {4, 7}}, 0.020399597244776385`]}, 
{Hue[0.6, 0.2, 0.8], EdgeForm[{GrayLevel[0], Opacity[0.7]}], 
           DiskBox[1, 0.020399597244776385], 
           DiskBox[2, 0.020399597244776385], 
           DiskBox[3, 0.020399597244776385], 
           DiskBox[4, 0.020399597244776385], 
           DiskBox[5, 0.020399597244776385], 
           DiskBox[6, 0.020399597244776385], 
           DiskBox[7, 0.020399597244776385]}}]],
MouseAppearanceTag["NetworkGraphics"]],
AllowKernelInitialization->False]],
DefaultBaseStyle->"NetworkGraphics",
FormatType->TraditionalForm,
FrameTicks->None,
ImageSize->{172.79999999999995`, Automatic}]\),
         opts]; (*This one didn't fit in the algorithm, so I'm hardcoding \
it.*)

GearGraph::usage = "GearGraph[n] generates an n-gear graph.";

GearGraph[n_?PositiveIntegerQ /; 4 <= n, opts : OptionsPattern[Graph]
    ] :=
    Module[{addOne, wheelGraph, outerEdges, range, vertexCoordinates},
        
        addOne = n + 1;
        wheelGraph = WheelGraph[addOne, opts];
        outerEdges = UndirectedEdge @@@ Partition[GraphPeriphery[wheelGraph
            ], 2, 1, 1];
        range = Range[addOne + 1, 2 addOne - 1];
        vertexCoordinates = Join[Thread[Range[n + 2, 2 n + 1] -> (Midpoint[
            {##}]& @@@ Partition[CirclePoints[n], 2, 1, 1])], Thread[Range[2, n +
             1] -> CirclePoints[n]]];
        Graph[Fold[VertexInsert[#1, #2[[1]], #2[[2]], "DoNotInsertIntoMultipleEdges"
            ]&, wheelGraph, Transpose[{outerEdges, range}]], opts, VertexCoordinates
             -> vertexCoordinates]
    ]

End[]

EndPackage[]
