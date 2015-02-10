HFreeQ[g_,h_]:=Module[{
  subs=Subgraph[g,#]&/@Subsets[VertexList[g],{VertexCount[h]}]},
  Return[NoneTrue[IsomorphicGraphQ[#,h]&/@subs,TrueQ]];
]

FindInducedH[g_,h_]:=Module[{
  subs=Subgraph[g,#]&/@Subsets[VertexList[g],{VertexCount[h]}]},
  list=IsomorphicGraphQ[#,h]&/@subs;
  If[Length[list]==0,
    Return[list],
    Return[subs[[FirstPosition[list,True]]]]];
]

SimplicialVertexQ[g_, v_] := 
  CompleteGraphQ[VertexDelete[NeighborhoodGraph[g, v], v]];

ChordalGraphQ::usage = 
  "ChordalGraph[g] yields True if g is a chordal graph and False otherwise.";

ChordalGraphQ[g_] := 
 Module[{h = g, 
   slist = Select[VertexList[g], SimplicialVertexQ[g, #] &]},
  While[Length[slist] != 0,
   h = VertexDelete[h, slist];
   slist = Select[VertexList[h], SimplicialVertexQ[h, #] &];
   ];
  EmptyGraphQ[h]
]

SplitGraphQ::usage = 
  "SplitGraphQ[h] yields True if g is a split graph and False otherwise.";

SplitGraphQ[g_] := 
  HFreeQ[g, CycleGraph[4]] && HFreeQ[g, CycleGraph[5]] && HFreeQ[g, GraphComplement[CycleGraph[4]]];

RegularGraphQ[g_] := Module[
  {degrees = VertexDegree[g]},
  Min[degrees] == Max[degrees]
]

RegularGraphQ[g_, k_] := Module[
  {degrees = VertexDegree[g]},
  Count[degrees, k] == Length[degrees]
]

FindInducedRRegularSubgraph::usage = 
  "FindInducedRRegularSubgraph[g,r,k] finds an induced r-regular subgraph of g of size k";

FindInducedRRegularSubgraph[g_, r_, k_] := Module[
   {subs = Subgraph[g, #] & /@ Subsets[VertexList[g], {k}]},
   candidates = RegularGraphQ[#, r] & /@ subs;
   If[MemberQ[candidates, True],
    Return[subs[[FirstPosition[candidates, True]]]],
    Return[{}]]
   ];
   
FindAllInducedRRegularSubgraphs::usage = 
  "FindAllInducedRRegularSubgraphs[g,r,k] finds all induced r-regular subgraphs of g of size k";

FindAllInducedRRegularSubgraphs[g_, r_, k_] := Module[
   {subs = Subgraph[g, #] & /@ Subsets[VertexList[g], {k}]},
   candidates = RegularGraphQ[#, r] & /@ subs;
   pos = Flatten[Position[candidates, True]];
   If[MemberQ[candidates, True],
    Return[subs[[pos]]],
    Return[{}]]
   ];
