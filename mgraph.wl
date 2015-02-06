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
