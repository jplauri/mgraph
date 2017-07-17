HFreeQ[g_, h_] := 
 Module[{subs = Subgraph[g, #] & /@ Subsets[VertexList[g], {VertexCount[h]}]}, 
  NoneTrue[IsomorphicGraphQ[#, h] & /@ subs, TrueQ]
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

ThresholdGraphQ::usage = 
  "ThresholdGraphQ[g] yields True if g is a threshold graph and False otherwise.";

ThresholdGraphQ[g_] := HFreeQ[g, CycleGraph[4]] && HFreeQ[g, PathGraph[{1, 2, 3, 4}]] && HFreeQ[g, GraphComplement[CycleGraph[4]]];

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

RandomIntervalGraph::usage = 
  "RandomIntervalGraph[n, max] gives an interval graph on n vertices, where each interval is taken from [0,max]";

RandomIntervalGraph[n_, max_] := 
 Module[{t = Table[Sort[Interval[RandomSample[Range[max], 2]]], {n}]},
  SimpleGraph[
   AdjacencyGraph[
    Boole[Reap[
       Do[Sow[UnsameQ[IntervalIntersection[t[[i]], #], Interval[]] & /@
           t], {i, n}]][[2, 1]]]]]
  ]

ChromaticNumber::usage = "ChromaticNumber[g] gives the chromatic number of the graph g";

ChromaticNumber[g_] := Module[{k = 1}, While[ChromaticPolynomial[g, k] == 0, ++k]; k]
 
CliqueCoverNumber::usage = "CliqueCoverNumber[g] gives the vertex clique covering number of the graph g";
 
CliqueCoverNumber[g_] := ChromaticNumber[GraphComplement[g]];

MinimumIndependentDominatingSet::usage = "MinimumIndependentDominatingSet[g] gives the independent domination number of the graph g";

MinimumIndependentDominatingSet[g_] := First@MinimalBy[FindClique[GraphComplement[g], Infinity, All], Length];

ForestNumber::usage = "ForestNumber[g] gives the forest number of the graph g";

ForestNumber[g_, k_] := AnyTrue[Subgraph[g, #] & /@ Subsets[VertexList[g], {k}], AcyclicGraphQ];

ForestNumber[g_] := Module[{k = 1}, While[ForestNumber[g, k] == True, ++k]; k - 1];

DominatingSetQ[g_, s_] := ! 
  MemberQ[Intersection[s, AdjacencyList[g, #]] & /@ 
    Complement[VertexList[g], s], {}];
    
ConnectedDominatingSetQ[g_, s_] := 
  ConnectedGraphQ[Subgraph[g, s]] && DominatingSetQ[g, s];
  
DominatingPairQ[g_, p_] := Module[{},
   dist = GraphDistance[g, First[p], Last[p]];
   paths = FindPath[g, First[p], Last[p], {dist}, All];
   AnyTrue[paths, DominatingSetQ[g, #] &]
   ];
   
DiametralPathGraphQ[g_] := Module[{},
  mat = UpperTriangularize[GraphDistanceMatrix[g]];
  AnyTrue[Position[mat, Max[mat]], DominatingPairQ[g, #] &]
  ]
