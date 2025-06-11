(* ::Package:: *)

(* ::Text:: *)
(*V10 : There is only so much one can do, and that is to get \[Gamma] \[Del](SuperStar[\[Chi]] \[Del]\!\(\*SuperscriptBox[\(\[Phi]\), \(\(*\)\(2\)\)]\))\[Phi]. BUT I try to split the branching and the diffusion process*)


SetOptions[$FrontEndSession,NotebookAutoSave->True]
NotebookSave[]


(* ::Title:: *)
(*Graphics*)


(* ::Section:: *)
(*Notations*)


(* ::Input::Initialization:: *)
Needs["Notation`"]
Notation[NotationTemplateTag[Subscript[\[Beta], a__]] \[DoubleLongLeftRightArrow] NotationTemplateTag[\[Beta][a__]]]
Notation[NotationTemplateTag[Subscript[R, a__]] \[DoubleLongLeftRightArrow] NotationTemplateTag[R[a__]]]
Notation[NotationTemplateTag[Subscript[\[Phi], a__]] \[DoubleLongLeftRightArrow] NotationTemplateTag[\[Phi][a__]]]
Notation[NotationTemplateTag[Subscript[\[Chi], a__]] \[DoubleLongLeftRightArrow] NotationTemplateTag[\[Chi][a__]]]
Notation[NotationTemplateTag[Subscript[\[Psi], a__]] \[DoubleLongLeftRightArrow] NotationTemplateTag[\[Psi][a__]]]
Notation[NotationTemplateTag[Subscript[\[Xi], a__]] \[DoubleLongLeftRightArrow] NotationTemplateTag[\[Xi][a__]]]Notation[NotationTemplateTag[\[SelectionPlaceholder]] \[DoubleLongLeftRightArrow] NotationTemplateTag[\[Placeholder]]]
Notation[NotationTemplateTag[Subscript[SuperStar[\[Phi]], a__]] \[DoubleLongLeftRightArrow] NotationTemplateTag[\[Phi]s[a__]]]
Notation[NotationTemplateTag[Subscript[SuperStar[\[Chi]], a__]] \[DoubleLongLeftRightArrow] NotationTemplateTag[\[Chi]s[a__]]]
Notation[NotationTemplateTag[Subscript[SuperStar[\[Psi]], a__]] \[DoubleLongLeftRightArrow] NotationTemplateTag[\[Psi]s[a__]]]
Notation[NotationTemplateTag[Subscript[SuperStar[\[Xi]], a__]] \[DoubleLongLeftRightArrow] NotationTemplateTag[\[Xi]s[a__]]]
Notation[NotationTemplateTag[SuperStar[\[Phi]]] \[DoubleLongLeftRightArrow] NotationTemplateTag[\[Phi]s]]
{
 {Notation[NotationTemplateTag[SuperStar[\[Chi]]] \[DoubleLongLeftRightArrow] NotationTemplateTag[\[Chi]s]]},
 {Notation[NotationTemplateTag[SuperStar[\[Psi]]] \[DoubleLongLeftRightArrow] NotationTemplateTag[\[Psi]s]]
  Notation[NotationTemplateTag[SuperStar[\[Xi]]] \[DoubleLongLeftRightArrow] NotationTemplateTag[\[Xi]s]]}
}


(* ::Subsection::Closed:: *)
(*Test Notation*)


(* ::Input:: *)
(*V[a]*)


(* ::Input:: *)
(*\[Beta][1,2]*)


(* ::Section::Closed:: *)
(*MyGraph[]*)


(* ::Input::Initialization:: *)
Clear[MyGraph];
Options[MyGraph]={"undirected" ->True,"label" ->False};
MyGraph[edges__,root_,source_,OptionsPattern[]]:={Module[{locEdges={},i,locWeights={},locRoot=root,locSource=source,p},
Clear[\[Beta]];

For[i=1, i<=(Dimensions@edges)[[1]],i++,
If[OptionValue["undirected"] && edges[[i,2]] =!=locSource && edges[[i,2]] =!=edges[[i,1]] ,
AppendTo[locEdges,edges[[i,2]]->edges[[i,1]]];
AppendTo[locWeights,\[Beta][edges[[i,2]],edges[[i,1]]] ]
];

If[edges[[i,1]]==locSource,Continue[]];

AppendTo[locEdges,edges[[i,1]]->edges[[i,2]]];
AppendTo[locWeights,\[Beta][edges[[i,1]],edges[[i,2]]] ]
];

locEdges=Sort[locEdges,(#1[[1]]<=#2[[1]] &&#1[[2]]<=#2[[1]])||(#1[[1]]<#2[[2]] &&#1[[2]]<=#2[[2]]) &];

p[\[Beta][a_,b_],\[Beta][c_,d_]]:=1/;(a<=c&&b<=c);
p[\[Beta][a_,b_],\[Beta][c_,d_]]:=-1/;(c<=a&&d<=a);
p[\[Beta][a_,b_],\[Beta][c_,d_]]:=1/;(a<d&&b<=d);
p[\[Beta][a_,b_],\[Beta][c_,d_]]:=-1/;(c<b&&d<=b);
locWeights=Sort[locWeights,p];

(*Print[locEdges];
Print[locWeights]*);

If[ OptionValue["label"],
(*True*)Graph[locEdges,EdgeWeight->locWeights, EdgeStyle->Blue, VertexLabels->{locRoot->ToString[locRoot]<>", ROOT",locSource->ToString[locSource]<>", SOURCE","Name"}, EdgeLabels->"EdgeWeight", VertexStyle->{locRoot->Red,locSource->Green,Blue},EdgeShapeFunction->"FilledArrow"],
(*False*)Graph[locEdges,EdgeWeight->locWeights, EdgeStyle->Blue, VertexLabels->{locRoot->ToString[locRoot]<>", ROOT",locSource->ToString[locSource]<>", SOURCE","Name"}, VertexStyle->{locRoot->Red,locSource->Green,Blue},EdgeShapeFunction->"FilledArrow"]]
],
root(*Root*),source(*Source*)}


(* ::Subsection:: *)
(*Usage example of MyGraph[]*)


(* ::Text:: *)
(*"edges" must be a list of ordered pairs containing the vertices of the graph. The edge is intended from the first vertex to the second.*)
(*If "directed" is false (optional, default to true), then a undirected graph is drawn*)


(* ::Input:: *)
(*edges = {{1,2},{2,3},{3,1}};*)
(*root=2;*)
(*source=3;*)
(*g=MyGraph[edges,root,source][[1]]*)


(* ::Input:: *)
(*gLabel=MyGraph[edges,root,source,"printEdgesLabel"->True]*)


(* ::Section::Closed:: *)
(*DrawOnGraph[]*)


(* ::Text:: *)
(*"Path" should be made of triplets whose first two elements are the initial and final vertices respectively, while the last is the color of this edge*)


(* ::Input::Initialization:: *)
Clear[DrawOnGraph];
Options[DrawOnGraph]={"drawOthers"-> True};
DrawOnGraph[graph_,path__,OptionsPattern[]]:=Module[{locEdges,locPathStyle={},locRoot,locSource,i},
locEdges = EdgeList@graph;
locRoot = 
 Select[(List@@@PropertyValue[graph,VertexLabels])[[2;;]],StringMatchQ[#[[2]],___~~"ROOT"]&][[All,1]][[1]];
locSource = 
 Select[(List@@@PropertyValue[graph,VertexLabels])[[2;;]],StringMatchQ[#[[2]],___~~"SOURCE"]&][[All,1]][[1]];

For[i=1, i<=Length[path],i++,
AppendTo[locPathStyle,{path[[i,1]]\[DirectedEdge]path[[i,2]]->path[[i,3]]}];
(*Print[locPathStyle]*)
];
If[OptionValue["drawOthers"],
(*True*)Graph[locEdges,EdgeStyle->Flatten@{locPathStyle,Blue}, VertexLabels->{locRoot->ToString[locRoot]",ROOT",locSource->ToString[locSource]",SOURCE","Name"}, VertexStyle->{locRoot->Red,locSource->Green,Blue},EdgeShapeFunction->"FilledArrow"],
(*False*)Graph[locEdges,EdgeStyle->Flatten@{locPathStyle,White}, VertexLabels->{locRoot->ToString[locRoot]",ROOT",locSource->ToString[locSource]",SOURCE","Name"}, VertexStyle->{locRoot->Red,locSource->Green,Blue},EdgeShapeFunction->"FilledArrow"]
]
]


(* ::Subsection:: *)
(*Usage example of DrawOnGraph[]*)


(* ::Text:: *)
(*"path" should be an ordered list of pairs of vertices, i.e. edges, that form the desired Laplacian RW*)


(* ::Input::Initialization:: *)
path1={{2,1,Red},{1,3,Green}};
DrawOnGraph[g,path1]


(* ::Input:: *)
(*DrawOnGraph[g,path1,"drawOthers"->False]*)


(* ::Section::Closed:: *)
(*DrawFromExpression[]*)


Clear[DrawFromExpression];
Options[DrawFromExpression]={"graph"->g,"drawOthers"->False};

DrawFromExpression[exp_,OptionsPattern[]]:=Module[{terms,graphs={},i},
terms=exp//.R[a_,b_,c_]^n_->R[a,b,c]//Expand;
terms=List @@ (terms+Zeta[3]);
terms=terms//.\[Beta][__]->1;
terms=DeleteElements[terms,{Zeta[3]}];
terms=Outer[List,terms];
terms=terms//.Times->List;
terms=monoFlatten@@@terms;
terms=terms//.R[a_,b_,c_]->{a,b,c};
(*For[i=1,i<=Length[terms],i++,
terms[[i]]=DeleteElements[terms[[i]],{a_?NumericQ}];
];*)
Print[terms];

For[i=1,i<=Length[terms],i++,
AppendTo[graphs,DrawOnGraph[OptionValue["graph"],terms[[i,2;;]],"drawOthers"->OptionValue["drawOthers"]]];
Print[graphs[[i]]];
Print[Weight ==terms[[i,1]]];
];
]


(* ::Section::Closed:: *)
(*AugmentedGraph[]*)


(* ::Text:: *)
(*This is useful to compute the total incoming current from the source towards the DLA cluster/Tree. WE MUST ADD THE EDGE FROM THE OLD SOURCE TO THE NEW SOURCE, AS WELL AS ALLOW THE EDGES FROM THE OLD SOURCE TO THE NEIGHBORS*)


(* ::Input::Initialization:: *)
Clear[AugmentedGraph];
AugmentedGraph[graph_]:=Module[{locVertices,locEdges,locPathStyle={},locRoot,locSource,newSource,oldSourceNeighbors,graphPlus,i},

locVertices = VertexList@graph;
locEdges = EdgeList@graph/.DirectedEdge->List;
locRoot = 
 Select[(List@@@PropertyValue[graph,VertexLabels])[[2;;]],StringMatchQ[#[[2]],___~~"ROOT"]&][[All,1]][[1]];
locSource = 
 Select[(List@@@PropertyValue[graph,VertexLabels])[[2;;]],StringMatchQ[#[[2]],___~~"SOURCE"]&][[All,1]][[1]];

newSource=locVertices[[-1]]+1;

(*Add the edge from the oldSource to the newSource*)
AppendTo[locEdges,{locSource,newSource}];

(*Add all the edges from the oldSource to its neighbors*)
oldSourceNeighbors=Select[locEdges, #[[2]]==locSource &];

For[i=1,i<=Length[oldSourceNeighbors],i++,
AppendTo[locEdges,{locSource,oldSourceNeighbors[[i,1]]}]
];

graphPlus =MyGraph[locEdges,locRoot,newSource,"undirected"->False][[1]];

Return[{graphPlus,locSource,newSource}]
]


(* ::Subsubsection::Closed:: *)
(*Usage example of AugmentedGraph[]*)


(* ::Input:: *)
(*g=myFavoriteGraph*)
(*AugmentedGraph[g]*)


(* ::Title::Closed:: *)
(*Field Theory tools*)


(* ::Section::Closed:: *)
(*monoFlatten[]*)


(* ::Input::Initialization:: *)
Clear[monoFlatten];
(*monoFlatten[a_?NumericQ]:={};*)
monoFlatten[a_]:={a};
monoFlatten[{a__}]:={{a}};
monoFlatten[{a__,b__}]:={a,b};


(* ::Subsubsection:: *)
(*Test*)


(* ::Section::Closed:: *)
(*ExtractPaths[]*)


(* ::Input:: *)
(*(*To extract the paths and their weights from the expression of an expectation value*)*)


Clear[ExtractPaths];
ExtractPaths[exp_]:=Module[{terms,paths={},i},

terms=exp//Expand;
terms=List @@Collect[terms,R[__,Red]];
terms=Outer[List,terms];

For[i=1,i<=Length[terms],i++,
If[MatchQ[terms[[i,1]],R[__,Red]*A_],
AppendTo[paths,terms[[i]]]
]
];
terms=paths//.R[__,Blue]->1//FullSimplify;

Return[terms]
]


(* ::Subsubsection:: *)
(*Usage example of ExtractPaths[]*)


(* ::Input:: *)
(*exp=(1-(Subscript[R, 2,3,RGBColor[0, 0, 1]] Subscript[R, 3,2,RGBColor[0, 0, 1]] Subscript[\[Beta], 2,3] Subscript[\[Beta], 3,2])/((Subscript[\[Beta], 2,1]+Subscript[\[Beta], 2,3]+Subscript[\[Beta], 2,5]) (Subscript[\[Beta], 3,2]+Subscript[\[Beta], 3,4]))+(Subscript[R, 1,2,RGBColor[1, 0, 0]] Subscript[R, 2,3,RGBColor[0, 0, 1]] Subscript[R, 3,4,RGBColor[0, 0, 1]] Subscript[\[Beta], 1,2] Subscript[\[Beta], 2,3] Subscript[\[Beta], 3,4])/((Subscript[\[Beta], 1,2]+Subscript[\[Beta], 1,5]) (Subscript[\[Beta], 2,1]+Subscript[\[Beta], 2,3]+Subscript[\[Beta], 2,5]) (Subscript[\[Beta], 3,2]+Subscript[\[Beta], 3,4]))-(Subscript[R, 2,5,RGBColor[0, 0, 1]] Subscript[R, 5,2,RGBColor[0, 0, 1]] Subscript[\[Beta], 2,5] Subscript[\[Beta], 5,2])/((Subscript[\[Beta], 2,1]+Subscript[\[Beta], 2,3]+Subscript[\[Beta], 2,5]) (Subscript[\[Beta], 5,1]+Subscript[\[Beta], 5,2]+Subscript[\[Beta], 5,4]))+(Subscript[R, 1,5,RGBColor[1, 0, 0]] Subscript[R, 2,3,RGBColor[0, 0, 1]] Subscript[R, 3,4,RGBColor[0, 0, 1]] Subscript[R, 5,2,RGBColor[0, 0, 1]] Subscript[\[Beta], 1,5] Subscript[\[Beta], 2,3] Subscript[\[Beta], 3,4] Subscript[\[Beta], 5,2])/((Subscript[\[Beta], 1,2]+Subscript[\[Beta], 1,5]) (Subscript[\[Beta], 2,1]+Subscript[\[Beta], 2,3]+Subscript[\[Beta], 2,5]) (Subscript[\[Beta], 3,2]+Subscript[\[Beta], 3,4]) (Subscript[\[Beta], 5,1]+Subscript[\[Beta], 5,2]+Subscript[\[Beta], 5,4]))+(Subscript[R, 1,5,RGBColor[1, 0, 0]] Subscript[R, 5,4,RGBColor[0, 0, 1]] Subscript[\[Beta], 1,5] Subscript[\[Beta], 5,4])/((Subscript[\[Beta], 1,2]+Subscript[\[Beta], 1,5]) (Subscript[\[Beta], 5,1]+Subscript[\[Beta], 5,2]+Subscript[\[Beta], 5,4]))+(Subscript[R, 1,2,RGBColor[1, 0, 0]] Subscript[R, 2,5,RGBColor[0, 0, 1]] Subscript[R, 5,4,RGBColor[0, 0, 1]] Subscript[\[Beta], 1,2] Subscript[\[Beta], 2,5] Subscript[\[Beta], 5,4])/((Subscript[\[Beta], 1,2]+Subscript[\[Beta], 1,5]) (Subscript[\[Beta], 2,1]+Subscript[\[Beta], 2,3]+Subscript[\[Beta], 2,5]) (Subscript[\[Beta], 5,1]+Subscript[\[Beta], 5,2]+Subscript[\[Beta], 5,4]))-(Subscript[R, 1,5,RGBColor[1, 0, 0]] Subscript[R, 2,3,RGBColor[0, 0, 1]] Subscript[R, 3,2,RGBColor[0, 0, 1]] Subscript[R, 5,4,RGBColor[0, 0, 1]] Subscript[\[Beta], 1,5] Subscript[\[Beta], 2,3] Subscript[\[Beta], 3,2] Subscript[\[Beta], 5,4])/((Subscript[\[Beta], 1,2]+Subscript[\[Beta], 1,5]) (Subscript[\[Beta], 2,1]+Subscript[\[Beta], 2,3]+Subscript[\[Beta], 2,5]) (Subscript[\[Beta], 3,2]+Subscript[\[Beta], 3,4]) (Subscript[\[Beta], 5,1]+Subscript[\[Beta], 5,2]+Subscript[\[Beta], 5,4])));ExtractPaths[exp]*)


(* ::Section::Closed:: *)
(*Vertex rules and VertexAssignment[]*)


(* ::Text:: *)
(*The vertices are where the interaction takes place in the field theory. *)


(* ::Input::Initialization:: *)
Clear[V];
V[1.]:=1.;
V[1]:=1;
(*V[0]:=0;*)
V'[a_]:=1


(* ::Input::Initialization:: *)
Clear[VertexAssignment];
Options[VertexAssignment]={"system"->"LERW","print"->False,"excludedVertices"->{},"sourceBC"->1};

VertexAssignment[graph_,OptionsPattern[]]:=Module[{locVertices,locWeights,totWeights,locSource,action,x,y},
Clear[i];
locVertices=VertexList@graph;
locWeights=WeightedAdjacencyMatrix[graph]//Normal;
totWeights =Total[locWeights,{2}];
locSource = 
 Select[(List@@@PropertyValue[graph,VertexLabels])[[2;;]],StringMatchQ[#[[2]],___~~"SOURCE"]&][[All,1]][[1]];

(*LERW*)
If[OptionValue["system"]=="LERW",
If[OptionValue["print"],Print["#####   "<>OptionValue["system"]<>"   #####"]];
action = Product[ If[totWeights[[x]]===0 || MemberQ[OptionValue["excludedVertices"],x],1,V[(1+ Sum[locWeights[[x,y]]/totWeights[[x]] \[Chi]s[y,Subscript[i, x]]\[Chi][x,Subscript[i, x]],{y,Length[locVertices]}] ) ]],{x,Length[locVertices]}];
action*=V[(1+ OptionValue["sourceBC"] \[Chi][locSource,Subscript[i, s]])];(*This is the BC*)
Return[action]];

(*DLA*)
If[OptionValue["system"]=="DLA",If[OptionValue["print"],Print["#####   "<>OptionValue["system"]<>"   #####"]];
Return[
Product[ If[totWeights[[x]]===0 || MemberQ[OptionValue["excludedVertices"],x],1,V[(1+ Product[1+locWeights[[x,y]]/totWeights[[x]] \[Chi]s[y,Subscript[i, x]],{y,Length[locVertices]}] \[Chi][x,Subscript[i, x]]+ Sum[locWeights[[x,y]]/totWeights[[x]] \[Phi]s[y,Subscript[j, x]]\[Phi][x,Subscript[j, x]],{y,Length[locVertices]}])] ],{x,Length[locVertices]}]
]]
]


(* ::Subsection:: *)
(*Usage example of VertexAssignment[]*)


(* ::Input:: *)
(*Z=VertexAssignment[g[[1]],"system"->"LERW"]*)


(* ::Input:: *)
(*VertexAssignment[g[[1]],"system"->"LERW","excludedVertices"->{1}]*)


(* ::Section::Closed:: *)
(*\[Delta]rules*)


(* ::Input::Initialization:: *)
ClearAll[\[CapitalSigma]\[Delta]rule]
\[CapitalSigma]\[Delta]rule={\[Delta][a_,b_]^n_/;(!NumericQ[b]&& !MatchQ[b,Subscript[1, __]]):>\[Delta][a,b]^(n-2) \[Delta][a,a],
\[Delta][b_,a_]\[Delta][c_,b_]/;(!NumericQ[b] && !MatchQ[b,Subscript[1, __]]):>\[Delta][c,a],
\[Delta][a_,b_]\[Delta][c_,b_]/;(!NumericQ[b]&& !MatchQ[b,Subscript[1, __]]):>\[Delta][a,c],
\[Delta][a_,a_]/;(MatchQ[a,Subscript[i, __]])/;(a=!=Subscript[i, s]):>n,
\[Delta][a_,a_]/;(MatchQ[a,Subscript[k, _]]):>1,
\[Delta][a_,a_]/;(MatchQ[a,Subscript[l, _]]):>1,
\[Delta][a_,a_]/;(MatchQ[a,Subscript[jj, _]]):>1,
\[Delta][a_,a_]/;(MatchQ[a,Subscript[ll, _]]):>1};


(* ::Input::Initialization:: *)
ClearAll[\[Delta]ruleExternal]
\[Delta]ruleExternal={\[Delta][a_,a_]/;(NumericQ[a] || a==Subscript[i, s] || MatchQ[a,Subscript[j, __]]|| MatchQ[a,Subscript[jj, __]] || MatchQ[a,Subscript[1, __]]||MatchQ[a,Subscript[k, _]]):>1,
\[Delta][a_,b_]/;(NumericQ[a] ||MatchQ[a,Subscript[k, _]]|| MatchQ[a,Subscript[j, __]]|| MatchQ[a,Subscript[jj, __]] || MatchQ[a,Subscript[1, _]])/;(a=!=b):>0,
\[Delta][a_,b_]/;(NumericQ[b] ||MatchQ[b,Subscript[k, _]]|| MatchQ[b,Subscript[j, __]]|| MatchQ[a,Subscript[jj, __]] || MatchQ[b,Subscript[1, _]])/;(a=!=b):>0 };


(* ::Subsection::Closed:: *)
(*Usage examples of the \[Delta]rules*)


(* ::Input:: *)
(*\[Delta][b,a]\[Delta][c,b]/.\[CapitalSigma]\[Delta]rule*)


(* ::Input:: *)
(*\[Delta][b,a]\[Delta][a,b]//.\[CapitalSigma]\[Delta]rule*)


(* ::Input:: *)
(*\[Delta][b,a]\[Delta][c,a]\[Delta][c,b]//.\[CapitalSigma]\[Delta]rule*)


(* ::Input:: *)
(*\[Delta][1,i]\[Delta][i,1]//.\[CapitalSigma]\[Delta]rule//.\[Delta]ruleExternal*)


(* ::Input:: *)
(*\[Delta][1,i]\[Delta][i,2]//.\[CapitalSigma]\[Delta]rule//.\[Delta]ruleExternal*)


(* ::Input:: *)
(*\[Delta][1,i]\[Delta][i,1]\[Delta][a,b]\[Delta][a,b]//.\[CapitalSigma]\[Delta]rule//.\[Delta]ruleExternal*)


(* ::Input:: *)
(*\[Delta][Subscript[k, 1],Subscript[k, 1]]//.\[CapitalSigma]\[Delta]rule*)


(* ::Input:: *)
(*\[Delta][Subscript[i, 1],Subscript[i, 1]]//.\[CapitalSigma]\[Delta]rule*)


(* ::Input:: *)
(*\[Delta][Subscript[j, -1],Subscript[j, -1]]//.\[Delta]ruleExternal*)


(* ::Input:: *)
(*\[Delta][Subscript[k, -1],Subscript[k, -1]]//.\[Delta]ruleExternal*)


(* ::Input:: *)
(*\[Delta][Subscript[1, 1],Subscript[1, -1]]\[Delta][Subscript[1, 1],Subscript[1, -1]]//.\[CapitalSigma]\[Delta]rule*)


(* ::Section::Closed:: *)
(*Operator "vertices" rules*)


(* ::Input::Initialization:: *)
Clear[Op];
Op[1]:=1;
Op[0]:=0;
Op'[_]:=1


(* ::Section::Closed:: *)
(*FieldsNamesGenerator[]*)


(* ::Input::Initialization:: *)
Clear[FieldsNamesGenerator];
FieldsNamesGenerator[fields_]:=
FieldsNamesGenerator[fields]=Block[{locFields={},starFields={},deltaFields={},deltaStarFields={},l},

locFields=fields;

For[l=1,l<=Length[locFields],l++,
AppendTo[starFields, ToExpression[ToString[locFields[[l]]] <> "s"]];
AppendTo[deltaFields, ToExpression["\[Delta]"<>ToString[locFields[[l]]] ]];
AppendTo[deltaStarFields,ToExpression["\[Delta]"<> ToString[starFields[[l]]] ]];
];(*End of loop to define the fields*)

Return[{locFields,starFields,deltaFields,deltaStarFields}]
]


(* ::Subsubsection:: *)
(*Test*)


(* ::Input:: *)
(*fields={\[Phi],\[Chi]};*)
(*FieldsNamesGenerator[fields]//AbsoluteTiming*)
(*FieldsNamesGenerator[fields]//AbsoluteTiming*)
(**)


(* ::Section::Closed:: *)
(*WickContractionBlock[]*)


(* ::Input:: *)
(*(*IT SHOULD BE WORKING WITH 4 FIELDS TOO!!!!!*)*)


(* ::Input::Initialization:: *)
Clear[WickContractionBlock];
Options[WickContractionBlock]={"print"->False,"fields"->{\[Chi]}, "endTime"->1,"#vertices"->10};

WickContractionBlock[A_ +B_,OptionsPattern[]]:=WickContractionBlock[A,"print"->OptionValue["print"],"fields"->OptionValue["fields"], "endTime"->OptionValue["endTime"],"#vertices"->OptionValue["#vertices"]]+
WickContractionBlock[B,"print"->OptionValue["print"],"fields"->OptionValue["fields"], "endTime"->OptionValue["endTime"],"#vertices"->OptionValue["#vertices"]];

WickContractionBlock[Op[A_ +B_]*C_,OptionsPattern[]]:=WickContractionBlock[Op[A]C,"print"->OptionValue["print"],"fields"->OptionValue["fields"], "endTime"->OptionValue["endTime"],"#vertices"->OptionValue["#vertices"]]+
WickContractionBlock[Op[B]C,"print"->OptionValue["print"],"fields"->OptionValue["fields"], "endTime"->OptionValue["endTime"],"#vertices"->OptionValue["#vertices"]];

WickContractionBlock[A_?NumericQ,OptionsPattern[]]:=A;

(*################################################################################
									Partition Function                             
##############################################################################*)
(*There are two possible contributions for each field: 1) it is not contracted: \[Chi]->0; 2) it is contracted*)

WickContractionBlock[V[A_] *B_, OptionsPattern[]]:=Block[{locFields={}, starFields={}, deltaFields={},deltaStarFields={},contributions=0,z,zTemp1,zTemp2,\[Kappa],j,l,timeIndex},

z=V[A]B;

(*
If[OptionValue["print"],
Print["###################  Partition Function  ##################"];
];*)

(* Updated version with FieldsNamesGenarator
locFields=OptionValue["fields"];

For[l=1,l<=Length[locFields],l++,
AppendTo[starFields, ToExpression[ToString[locFields[[l]]] <> "s"]];
AppendTo[deltaFields, ToExpression["\[Delta]"<>ToString[locFields[[l]]] ]];
AppendTo[deltaStarFields,ToExpression["\[Delta]"<> ToString[starFields[[l]]] ]]
];(*End of loop to define the fields*)
*)

locFields=OptionValue["fields"][[1]];
starFields=OptionValue["fields"][[2]];
deltaFields=OptionValue["fields"][[3]];
deltaStarFields=OptionValue["fields"][[4]];


(*Start a loop over time*)
For[timeIndex=0,timeIndex<=(OptionValue["endTime"]-1),timeIndex++,

z=z//.f_[x_,Subscript[t, (timeIndex+1)],a_]->f[x,a];

(*Start of loop over fields*)
For[l=1,l<=Length[locFields],l++,

(*Start of loop over space*)
For[j=1,j<=OptionValue["#vertices"],j++,

(*First contribution*)
zTemp1=z//.locFields[[l]][j,a_]->0//.starFields[[l]][j,a_]->0;
If[zTemp1-z=!=0,
contributions=zTemp1;
(*Print["First contribution: ",zTemp1];*)
];

(*Second contribution*)
zTemp1=z/. locFields[[l]][j,a_]->locFields[[l]][j,a]+\[Kappa] *deltaFields[[l]][j,a];
If[zTemp1-z=!=0,
zTemp1=D[zTemp1,\[Kappa]]/.\[Kappa]->0;
];

zTemp2=zTemp1/. starFields[[l]][j,a_]->starFields[[l]][j,a]+\[Kappa] *deltaStarFields[[l]][j,a];
If[zTemp2-zTemp1=!=0,
zTemp1=D[zTemp2,\[Kappa]]/.\[Kappa]->0;
];

zTemp1=Expand[zTemp1];
zTemp2=zTemp1//.deltaFields[[l]][j,a_]*deltaStarFields[[l]][j,b_](*/;(!NumericQ[a*b]):>*)->\[Delta][a,b];

zTemp1=zTemp2/.deltaFields[[l]][j,a_]->0/.deltaStarFields[[l]][j,b_]->0;


If[contributions-zTemp1=!=0,
contributions+=zTemp1;
(*Print["Second contribution: ",zTemp1];*)
];


(*THIRD contribution*)
zTemp1=z/. locFields[[l]][j,a_]->locFields[[l]][j,a]+\[Kappa] *deltaFields[[l]][j,a];
If[zTemp1-z=!=0,
zTemp1=1/2 D[zTemp1,{\[Kappa],2}]/.\[Kappa]->0;
];

zTemp2=zTemp1/. starFields[[l]][j,a_]->starFields[[l]][j,a]+\[Kappa] *deltaStarFields[[l]][j,a];
If[zTemp2-zTemp1=!=0,
zTemp1=D[zTemp2,{\[Kappa],2}]/.\[Kappa]->0;
];

zTemp1=Expand[zTemp1];
zTemp2=zTemp1//.deltaFields[[l]][j,a_]*deltaStarFields[[l]][j,b_](*/;(!NumericQ[a*b]):>*)->\[Delta][a,b];
zTemp2=zTemp1//.deltaFields[[l]][j,a_]*deltaStarFields[[l]][j,b_]*deltaFields[[l]][j,a_]*deltaStarFields[[l]][j,b_](*/;(!NumericQ[a*b]):>*)->\[Delta][a,b]*\[Delta][a,b];

zTemp1=zTemp2/.deltaFields[[l]][j,a_]->0/.deltaStarFields[[l]][j,b_]->0;

If[contributions-zTemp1=!=0,
contributions+=zTemp1;
(*Print["Third contribution: ",zTemp1];*)
];

z=contributions;


(*Print["Overall contributions: ",contributions];
Print["################################"];*)

contributions=0;

];(*End of loop over space*)

z=z/.locFields[[l]][x_,a_]->0;
z=z/.starFields[[l]][x_,a_]->0;

];(*End of loop over fields*)

(*For[l=1,l<=Length[locFields],l++,
z=z/.locFields[[l]][x_,a_]->0;
z=z/.starFields[[l]][x_,a_]->0
];*)

z=z//.\[CapitalSigma]\[Delta]rule;

];(*End of loop over time*)

Return[z]
];


(* ::Subsection:: *)
(* WickContractionBlock[] quartic term test*)


(* ::Item:: *)
(*Simple*)


(* ::Input:: *)
(*edges = {{1,2},{2,3},{3,1}};*)
(*root=2;*)
(*source=3;*)
(*g=MyGraph[edges,root,source][[1]];*)
(*Z=VertexAssignment[g,"system"->"LERW"];*)
(*WickContractionBlock[Z ,"fields"->FieldsNamesGenerator[{\[Chi]}], "endTime"->1]/.n->-1*)
(*WickContractionBlock[Z Subscript[\[Chi], 1,Subscript[i, 10]] Subscript[SuperStar[\[Chi]], 1,Subscript[i, 10]],"fields"->FieldsNamesGenerator[{\[Chi]}], "endTime"->1]/.n->-1*)


(* ::Input:: *)
(*(*IT SEEMS TO BE WORKING EVEN WITH 4 FIELDS!!!!*)*)


(* ::Input:: *)
(**)
(*WickContractionBlock[V[ 1+Subscript[\[Chi], 1,Subscript[i, 10]] Subscript[SuperStar[\[Chi]], 1,Subscript[i, 10]]]Subscript[\[Chi], 1,Subscript[i, 10]] Subscript[SuperStar[\[Chi]], 1,Subscript[i, 10]],"fields"->FieldsNamesGenerator[{\[Chi]}], "endTime"->1]*)


(* ::Item:: *)
(*More complex example CORRECT!!*)


(* ::Input:: *)
(*edges={{1,3},{1,2},{2,2},{2,3}};*)
(*root=1;*)
(*source=3;*)
(*g=MyGraph[edges,root,source][[1]];*)
(*g;*)
(*Z=VertexAssignment[g]*)
(*WickContractionBlock[Z,"fields"->{\[Chi]}, "endTime"->1]/.n->-1*)


(* ::Item::Closed:: *)
(*Regular 2x2 lattice with absorption (i.e. source) at 4*)


(* ::Input:: *)
(*edges={{1,2},{1,3},{2,4},{3,4}};*)
(*root=1;*)
(*source=4;*)
(*g=MyGraph[edges,root,source];*)
(*g[[1]];*)
(*Z=VertexAssignment[g[[1]]]*)
(**)
(*WickContraction[Z]/.n->-1*)


(* ::Section::Closed:: *)
(*ExpectationValueBlock[]*)


(* ::Input::Initialization:: *)
Clear[ExpectationValueBlock];
Options[ExpectationValueBlock]={"operator"->1,"print"->False, "fields"->{\[Chi]},"endTime"->1,"draw"->False,"graph"->g, "Rrule"->{R[__]->1},"external\[Delta]"->True};

ExpectationValueBlock[Z_,OptionsPattern[]]:=Block[{Nvertices,z,terms,graphs={},i},

Nvertices=Length[VertexList@OptionValue["graph"]];

If[OptionValue["operator"]===1,
(*True*)z=WickContractionBlock[Z,
"print"->OptionValue["print"], 
"fields"->OptionValue["fields"],
"endTime"->OptionValue["endTime"],
"#vertices"->Nvertices
],
(*False*)z=WickContractionBlock[Z*Op[OptionValue["operator"]],
"print"->OptionValue["print"], 
"fields"->OptionValue["fields"],
"endTime"->OptionValue["endTime"],
"#vertices"->Nvertices
]/WickContractionBlock[Z, "fields"->OptionValue["fields"],"endTime"->OptionValue["endTime"],
"#vertices"->Nvertices]
];

If[OptionValue["external\[Delta]"],
z=z//.\[Delta]ruleExternal];
z=z/.n->-1;
z=z/.m->0;

If[OptionValue["draw"],
(*True*)
z=z//.R[a_,b_,c_]^n_->R[a,b,c];
terms=List @@ (z+Zeta[3]);
terms=terms//.\[Beta][__]->1;
terms=DeleteElements[terms,{Zeta[3]}];
terms=Outer[List,terms];
terms=terms//.Times->List;
terms=monoFlatten@@@terms;
terms=terms//.R[a_,b_,c_]->{a,b,c};
(*For[i=1,i<=Length[terms],i++,
terms[[i]]=DeleteElements[terms[[i]],{a_?NumericQ}];
];*)
Print[terms];
z=z/.R[___]->1;

For[i=1,i<=Length[terms],i++,
AppendTo[graphs,DrawOnGraph[OptionValue["graph"],terms[[i,2;;]],"drawOthers"->False]];
Print[graphs[[i]]];
Print[Weight ==terms[[i,1]]];
];,

(*False*)z=z/.OptionValue["Rrule"]];

Return[z]]


(* ::Subsection:: *)
(*ExpectationValue[] vs ExpectationValueBlock[] speed test*)


(* ::Item:: *)
(*My favorite graph*)


(* ::Input:: *)
(*myFavoriteEdges={{1,2},{1,5},{2,3},{2,5},{5,4},{3,4}};*)
(*myFavoriteRoot=1;*)
(*myFavoriteSource=4;*)
(*myFavoriteGraph=MyGraph[myFavoriteEdges,myFavoriteRoot,myFavoriteSource][[1]]*)


(* ::Input:: *)
(*g=myFavoriteGraph;*)
(*Z=VertexAssignment[g];*)
(*ExpectationValue[Z]//AbsoluteTiming*)
(*ExpectationValueBlock[Z,"graph"->g]//AbsoluteTiming*)


(* ::Text:: *)
(*It  does  seem  to  be  faster  indeed*)


(* ::Section::Closed:: *)
(*LERWtransitionProb[]*)


(* ::Text:: *)
(*This function computes the transition probability as*)


(* ::DisplayFormula:: *)
(*\!\(TraditionalForm\`\**)
(*SubsuperscriptBox[*)
(*StyleBox["P", "TI"], *)
(*StyleBox[*)
(*RowBox[{"L", "E", "R", "W"}], "TI"], *)
(*StyleBox["s", "TI"]] \((\[Gamma] -> \[Gamma] + \**)
(*StyleBox["x", "TI"])\) == \**)
(*FractionBox[*)
(*RowBox[{"\[Beta]", *)
(*StyleBox[*)
(*RowBox[{"v", "x"}], "TI"]}], *)
(*SubscriptBox[*)
(*StyleBox["r", "TI"], *)
(*StyleBox["v", "TI"]]] \**)
(*FractionBox[*)
(*RowBox[{"\[Integral]", *)
(*SuperscriptBox[*)
(*StyleBox["e", "TI"], *)
(*RowBox[{"-", *)
(*SubscriptBox[*)
(*StyleBox["S", "TI"], *)
(*RowBox[{*)
(*StyleBox["G", "TI"], "|", "\[Gamma]", "\[Union]", *)
(*RowBox[{"{", *)
(*StyleBox["s", "TI"], "}"}]}]]}]], *)
(*SubsuperscriptBox["\[Chi]", "1", "*"], *)
(*RowBox[{"(", *)
(*StyleBox["x", "TI"], ")"}], *)
(*SubscriptBox["\[Chi]", "1"], *)
(*RowBox[{"(", *)
(*StyleBox["s", "TI"], ")"}]}], *)
(*RowBox[{"\[Integral]", *)
(*SuperscriptBox[*)
(*StyleBox["e", "TI"], *)
(*RowBox[{"-", *)
(*SubscriptBox[*)
(*StyleBox["S", "TI"], *)
(*RowBox[{*)
(*StyleBox["G", "TI"], "|", "\[Gamma]", "\[Union]", *)
(*RowBox[{"{", *)
(*StyleBox["s", "TI"], "}"}], "\\", *)
(*RowBox[{"{", *)
(*StyleBox["v", "TI"], "}"}]}]]}]], *)
(*SubsuperscriptBox["\[Chi]", "1", "*"], *)
(*RowBox[{"(", *)
(*StyleBox["v", "TI"], ")"}], *)
(*SubscriptBox["\[Chi]", "1"], *)
(*RowBox[{"(", *)
(*StyleBox["s", "TI"], ")"}]}]]\)*)


(* ::Input::Initialization:: *)
Clear[LERWtransitionProb];
Options[LERWtransitionProb]={"draw"->False,"print"->False};

LERWtransitionProb[graph_,path__,OptionsPattern[]]:=Module[{locSource,locRoot,locWeights,totWeights,locVertices,denominator,prob=1, Z,numerator},
locWeights=WeightedAdjacencyMatrix[graph]//Normal;
totWeights =Total[locWeights,{2}];

locVertices=VertexList@graph;

locRoot = 
 Select[(List@@@PropertyValue[graph,VertexLabels])[[2;;]],StringMatchQ[#[[2]],___~~"ROOT"]&][[All,1]][[1]];
locSource = 
 Select[(List@@@PropertyValue[graph,VertexLabels])[[2;;]],StringMatchQ[#[[2]],___~~"SOURCE"]&][[All,1]][[1]];

If[locRoot =!= path[[1,1]], Print["####  WRONG ROOT  ####"]; Return[NULL]];
If[locSource =!= path[[-1,2]], Print["####  WRONG SOURCE  ####"];Return[NULL]];

Module[{locBC={{},{}},\[Phi],i=1,j},
locBC={{\[Phi][locSource]==1},{locSource}};

For[i=1,i<=Length[path],i++,

Z=VertexAssignment[graph, "excludedVertices"->locBC[[2]]];
Z=ExpectationValue[Z*\[Chi]s[path[[i,1]],1]];

If[OptionValue["print"],
Print["#####  Previous Partition Function  #####"];
Print[Z]
];

denominator=Z//FullSimplify;


(*Then we need to solve it again with updated BC at the current step*)

AppendTo[locBC[[1]],\[Phi][path[[i,1]]]==0];
AppendTo[locBC[[2]],path[[i,1]]];
(*Print[locBC]*);


Z=VertexAssignment[graph, "excludedVertices"->locBC[[2]],"sourceBC"->denominator^-1];
Z=ExpectationValue[Z*\[Chi]s[path[[i,2]],1] ];

If[OptionValue["print"],
Print["#####  Current Partition Function  #####"];
Print[Z]
];

numerator=locWeights[[path[[i,1]],path[[i,2]]]]/totWeights[[path[[i,1]]]]*Z//FullSimplify;

Print["#####  Transition probability "<>ToString[path[[i,1]]]<>" to "<>ToString[path[[i,2]]]<>"  #####"];
Print[numerator/.\[Beta][_,_]->1//FullSimplify];

prob*=numerator//FullSimplify;
]
];
If[OptionValue["draw"],Print[DrawOnGraph[graph,path]]];
Return[prob]
]


(* ::Subsection:: *)
(*Usage example of LaplacianRW[] &LERWtransitionProb[]*)


(* ::Text:: *)
(*"path" should be an ordered list of pairs of vertices, i.e. edges, that form the desired Laplacian RW.*)
(*If "draw" is true, then draw the path*)


(* ::Item:: *)
(*Simple cases first*)


(* ::Input::Initialization:: *)
edges = {{1,2},{2,3},{3,1}};
root=2;
source=3;
g=MyGraph[edges,root,source][[1]];

path1={{2,1},{1,3}};
LaplacianRW[g,{{2,1},{1,3}}](*/.\[Beta][_,_]->1*)


(* ::Input:: *)
(*(\[Beta][1,3] \[Beta][2,1])/((\[Beta][1,2]+\[Beta][1,3]) ((\[Beta][1,3] \[Beta][2,1])/(\[Beta][1,2]+\[Beta][1,3])+\[Beta][2,3]))//FullSimplify*)


(* ::Input:: *)
(*%//FullSimplify*)


(* ::Input:: *)
(*(*Correct!!*)*)


(* ::Input:: *)
(*VertexAssignment[g,"excludedVertices"->{2,3}]*)
(*LERWtransitionProb[g,path1]*)


(* ::Input:: *)
(*(*Correct!!*)*)


(* ::Item:: *)
(*Let's try with another example, i.e. my favorite graph*)


(* ::Input:: *)
(*myFavoriteEdges={{1,2},{1,5},{2,3},{2,5},{5,4},{3,4}};*)
(*myFavoriteRoot=1;*)
(*myFavoriteSource=4;*)
(*myFavoriteGraph=MyGraph[myFavoriteEdges,myFavoriteRoot,myFavoriteSource][[1]]*)


(* ::Input:: *)
(*PathFinder[myFavoriteGraph]*)


(* ::Input:: *)
(*path2={{1,2},{2,3},{3,4}};*)
(*LaplacianRW[myFavoriteGraph,path2,"draw"->True]/.\[Beta][_,_]->1*)


(* ::Input:: *)
(*LERWtransitionProb[myFavoriteGraph,path2]/.\[Beta][_,_]->1*)


(* ::Input:: *)
(*(*Correct!!!*)*)


(* ::Input:: *)
(**)


(* ::Title:: *)
(*Dynamics*)


(* ::Section::Closed:: *)
(*Znorm[]*)


(* ::Input::Initialization:: *)
Clear[Znorm];
Options[Znorm]={"graph"->g,"fields"->{\[Phi],\[Xi],\[Chi]},"theory"->"LERW","\[Beta]rule"->{\[Beta][__]->1.}};

Znorm[expression_,OptionsPattern[]]:=Block[{locZnorm,excludedVertices,token},

excludedVertices=(expression * 2)/.R[__]->1/.\[Gamma]->1//Expand;
excludedVertices=List @@excludedVertices;
excludedVertices=excludedVertices[[2;;]]; (*Gets rid of the numerical coefficient*)
excludedVertices=excludedVertices/.SuperStar[\[Phi]][a_,__]->a;
excludedVertices=excludedVertices/.\!\(
\*SubsuperscriptBox[\((
\*SuperscriptBox[\(\[Phi]\), \(*\)])\), \(a_, __\), \(2\)] -> a\);
excludedVertices=excludedVertices/.SuperStar[\[Psi]][a_,__]->a;
excludedVertices=excludedVertices/.\!\(
\*SubsuperscriptBox[\((
\*SuperscriptBox[\(\[Psi]\), \(*\)])\), \(a_, __\), \(n_\)] -> a\);
excludedVertices=excludedVertices/.SuperStar[\[Xi]][a_,__]->a;
excludedVertices=excludedVertices/.\!\(
\*SubsuperscriptBox[\((
\*SuperscriptBox[\(\[Xi]\), \(*\)])\), \(a_, __\), \(n_\)] -> a\);

locZnorm=VertexAssignment[OptionValue["graph"],"system"->"LERW","excludedVertices"->excludedVertices]/.OptionValue["\[Beta]rule"];

ExpectationValueBlock[locZnorm * token,"graph"->OptionValue["graph"],"fields"->OptionValue["fields"]]/.token->1
]


(* ::Subsubsection:: *)
(*Test*)


(* ::Input:: *)
(*g*)


(* ::Input:: *)
(*VertexAssignment[g]*)


(* ::Input:: *)
(*ExpectationValue[%]*)


(* ::Input:: *)
(*%/.\[Beta][__]->1*)


(* ::Input:: *)
(*Znorm[+(5/18) \[Gamma] Subscript[R, 1,2,RGBColor[1, 0, 0]] Subscript[SuperStar[\[Phi]], 2,Subscript[t, 2],Subscript[k, 1]],"graph"->g]*)


(* ::Input:: *)
(*Znorm[11/18 \[Gamma] Subscript[SuperStar[\[Phi]], 1,Subscript[t, 2],Subscript[k, 1]]]*)


(* ::Section::Closed:: *)
(*DynamicLERWaction[]*)


(* ::Text:: *)
(*This action should be:*)


(* ::Input:: *)
(*\!\(TraditionalForm\`\**)
(*SuperscriptBox[*)
(*StyleBox["e", "TI"], *)
(*RowBox[{"-", *)
(*SubscriptBox[*)
(*StyleBox["S", "TI"], *)
(*StyleBox["t", "TI"]]}]] == \**)
(*UnderscriptBox["\[Product]", *)
(*StyleBox["x", "TI"],*)
(*LimitsPositioning->True]\**)
(*SuperscriptBox[*)
(*StyleBox["e", "TI"], *)
(*RowBox[{"-", *)
(*UnderscriptBox["\[Sum]", *)
(*StyleBox["i", "TI"],*)
(*LimitsPositioning->True], *)
(*SubsuperscriptBox["\[Chi]", *)
(*StyleBox["i", "TI"], "*"], *)
(*RowBox[{"(", *)
(*RowBox[{*)
(*StyleBox["x", "TI"], ",", *)
(*StyleBox["t", "TI"]}], ")"}], *)
(*SubscriptBox["\[Chi]", *)
(*StyleBox["i", "TI"]], *)
(*RowBox[{"(", *)
(*RowBox[{*)
(*StyleBox["x", "TI"], ",", *)
(*StyleBox["t", "TI"]}], ")"}], "-", *)
(*SuperscriptBox["\[Psi]", "*"], *)
(*RowBox[{"(", *)
(*RowBox[{*)
(*StyleBox["x", "TI"], ",", *)
(*StyleBox["t", "TI"]}], ")"}], "\[Psi]", *)
(*RowBox[{"(", *)
(*RowBox[{*)
(*StyleBox["x", "TI"], ",", *)
(*StyleBox["t", "TI"]}], ")"}], "-", *)
(*SuperscriptBox["\[Phi]", "*"], *)
(*RowBox[{"(", *)
(*RowBox[{*)
(*StyleBox["x", "TI"], ",", *)
(*StyleBox["t", "TI"]}], ")"}], "\[Phi]", *)
(*RowBox[{"(", *)
(*RowBox[{*)
(*StyleBox["x", "TI"], ",", *)
(*StyleBox["t", "TI"]}], ")"}]}]] \**)
(*UnderscriptBox["\[Product]", *)
(*RowBox[{*)
(*StyleBox["x", "TI"], "\[Element]", *)
(*StyleBox["G", "TI"], "\\", *)
(*RowBox[{"{", *)
(*StyleBox["s", "TI"], "}"}]}],*)
(*LimitsPositioning->True]{1 + \**)
(*UnderscriptBox["\[Sum]", *)
(*StyleBox["y", "TI"],*)
(*LimitsPositioning->True]\**)
(*FractionBox[*)
(*SubscriptBox["\[Beta]", *)
(*StyleBox[*)
(*RowBox[{"x", "y"}], "TI"]], *)
(*SubscriptBox[*)
(*StyleBox["r", "TI"], *)
(*StyleBox["x", "TI"]]] *)
(*\*SuperscriptBox[\(\[Phi]\), \(*\)] \((\**)
(*StyleBox["y", "TI"], \**)
(*StyleBox["t", "TI"] + 1)\) \[Phi] \((\**)
(*StyleBox["x", "TI"], \**)
(*StyleBox["t", "TI"])\) *)
(*\*SuperscriptBox[\(\[Psi]\), \(*\)] \((\**)
(*StyleBox["x", "TI"], \**)
(*StyleBox["t", "TI"] + 1)\) *)
(*\*SubsuperscriptBox[\(\[Chi]\), \(1\), \(*\)] \((\**)
(*StyleBox["y", "TI"], \**)
(*StyleBox["t", "TI"])\) + \**)
(*UnderscriptBox["\[Sum]", *)
(*StyleBox["y", "TI"],*)
(*LimitsPositioning->True]\**)
(*FractionBox[*)
(*SubscriptBox["\[Beta]", *)
(*StyleBox[*)
(*RowBox[{"x", "y"}], "TI"]], *)
(*SubscriptBox[*)
(*StyleBox["r", "TI"], *)
(*StyleBox["x", "TI"]]] \**)
(*UnderoverscriptBox["\[Sum]", *)
(*StyleBox["i", "TI"], *)
(*RowBox[{*)
(*StyleBox["N", "TI"], "==", *)
(*RowBox[{"-", "1"}]}],*)
(*LimitsPositioning->True]\**)
(*SubsuperscriptBox["\[Chi]", *)
(*StyleBox["i", "TI"], "*"] \((\**)
(*StyleBox["y", "TI"], \**)
(*StyleBox["t", "TI"])\) \**)
(*SubscriptBox["\[Chi]", *)
(*StyleBox["i", "TI"]] \((\**)
(*StyleBox["x", "TI"], \**)
(*StyleBox["t", "TI"])\) + *)
(*\*SuperscriptBox[\(\[Psi]\), \(*\)] \((\**)
(*StyleBox["x", "TI"], \**)
(*StyleBox["t", "TI"] + 1)\) \[Psi] \((\**)
(*StyleBox["x", "TI"], \**)
(*StyleBox["t", "TI"])\)}\[Cross]\(\([\)\(1 + \[Psi] \((\**)
(*StyleBox["x", "TI"], \**)
(*StyleBox["T", "TI"] + 1)\)\)\(]\)\) \(\([\)\(1 + \**)
(*SubscriptBox[*)
(*StyleBox["a", "TI"], *)
(*StyleBox["s", "TI"]] \((\**)
(*StyleBox["t", "TI"])\) *)
(*\*SubscriptBox[\(\[Chi]\), \(1\)] \((\**)
(*StyleBox["s", "TI"], \**)
(*StyleBox["t", "TI"])\)\)\(]\)\)\)*)


(* ::Text:: *)
(*For fields that do not have a color index, we default to set it to Subscript[j, -1], because in the WickContraction[] it automated with color indices.*)
(*Instead, the source is set with index 1, so that it can contract with an external "probe"*)


(* ::Input:: *)
(*Clear[DynamicLERWaction];*)
(*Options[DynamicLERWaction]={"sourceBC"->1};*)
(**)
(*DynamicLERWaction[graph_,endTime_,OptionsPattern[]]:=Module[{locSource,locRoot,locWeights,totWeights,locVertices,actionTimeSlice,action},*)
(*Clear[i,k,j];*)
(**)
(*locWeights=WeightedAdjacencyMatrix[graph]//Normal;*)
(*totWeights =Total[locWeights,{2}];*)
(**)
(*locVertices=VertexList@graph;*)
(**)
(*locRoot = *)
(* Select[(List@@@PropertyValue[graph,VertexLabels])[[2;;]],StringMatchQ[#[[2]],___~~"ROOT"]&][[All,1]][[1]];*)
(*locSource = *)
(* Select[(List@@@PropertyValue[graph,VertexLabels])[[2;;]],StringMatchQ[#[[2]],___~~"SOURCE"]&][[All,1]][[1]];*)
(**)
(**)
(*actionTimeSlice [T_]:= Product[ If[totWeights[[x]]===0 ,1,(*FALSE*)*)
(*V[(1+ Sum[locWeights[[x,y]]/totWeights[[x]] R[x,y,Red]\[Phi]s[y,Subscript[t, T+1],Subscript[k, x]]\[Phi][x,Subscript[t, T],Subscript[k, x]]\[Psi]s[x,Subscript[t, T+1],Subscript[j, -1]]\[Chi]s[y,Subscript[t, T],1],{y,Length[locVertices]}]+Sum[locWeights[[x,y]]/totWeights[[x]] R[x,y,Blue]\[Chi]s[y,Subscript[t, T],Subscript[i, x]]\[Chi][x,Subscript[t, T],Subscript[i, x]],{y,Length[locVertices]}]+ \[Psi]s[x,Subscript[t, T+1],Subscript[j, -1]]\[Psi][x,Subscript[t, T],Subscript[j, -1]]) ]],{x,Length[locVertices]}]*)
(*V[(1+ OptionValue["sourceBC"] \[Chi][locSource,Subscript[t, T],1])];*)
(**)
(*action=Product[actionTimeSlice[T],{T,endTime}]Product[V[(1+\[Psi][x,Subscript[t, endTime+1],Subscript[j, -1]])(*This allows the path tracker to be absorbed at the end*) ]V[(1+\[Phi][x,Subscript[t, endTime+1],0])(*This allows the path to end after "endTime" steps*)],{x,Length[locVertices]}];*)
(**)
(*Return[action]*)
(**)
(*]*)


(* ::Subsection:: *)
(*Usage example of DynamicLERWaction[]*)


(* ::Item:: *)
(*My favorite graph*)


(* ::Input:: *)
(*myFavoriteEdges={{1,2},{1,5},{2,3},{2,5},{5,4},{3,4}};*)
(*myFavoriteRoot=1;*)
(*myFavoriteSource=4;*)
(*myFavoriteGraph=MyGraph[myFavoriteEdges,myFavoriteRoot,myFavoriteSource][[1]]*)


(* ::Input:: *)
(*endTime=1;*)
(*DynamicLERWaction[myFavoriteGraph,endTime]*)


(* ::Section::Closed:: *)
(*Evaluation of the DynamicLERWaction[]*)


(* ::Item:: *)
(*Simple case*)


(* ::Subitem:: *)
(*One time-slice*)


(* ::Input:: *)
(*fields={\[Chi],\[Phi],\[Psi]};*)
(*edges = {{1,2},{2,3},{3,1}};*)
(*root=2;*)
(*source=3;*)
(*g=MyGraph[edges,root,source][[1]];*)
(*g;*)
(**)
(*endTime=1;*)
(*Z=DynamicLERWaction[g,endTime];*)


(* ::Input:: *)
(*fields={\[Chi],\[Phi],\[Psi]};*)
(*endTime=1;*)
(*Z=DynamicLERWaction[g,endTime];*)
(*ExpectationValue[Z,"fields"->fields,"endTime"->endTime,"print"->False,"draw"->True,"graph"->g]*)


(* ::Input:: *)
(*Z//.V[A_]->A;*)
(*GeneralizedExpectationValue[%,"draw"->True,"graph"->g]*)


(* ::Subitem::Closed:: *)
(*Two time-slices*)


(* ::Input:: *)
(*endTime=2;*)
(*Z=DynamicLERWaction[g,endTime];*)
(*Z//.V[A_]->A;*)
(*GeneralizedExpectationValue[%]*)
(*%/.\[Beta][__]->1*)


(* ::Input:: *)
(*(*Wich is (Z_t)^2. Correct!!!*)*)


(* ::Input:: *)
(*WickContraction[Z,"fields"->fields,"endTime"->endTime]*)


(* ::Input:: *)
(*(*IT IS WAAAAAY QUICKER!!!!!!!*)*)


(* ::Input:: *)
(*ExpectationValue[Z,"fields"->fields,"endTime"->endTime]*)


(* ::Input:: *)
(*%/.V[_]->1*)


(* ::Subitem:: *)
(*Now let's add an external field*)


(* ::Subsubitem::Closed:: *)
(*With the generalized expectation value*)


(* ::Input:: *)
(*endTime=1;*)
(*Z=DynamicLERWaction[g,endTime];*)
(*Z//.V[A_]->A;*)
(*GeneralizedExpectationValue[%* \[Phi]s[2,Subscript[t, 1],Subscript[i, -1]]]*)


(* ::Input:: *)
(*(*Which makes sense since I am not considering any step. endTime=1 means that we consider only one time-slice, i.e. the initial position*)*)


(* ::Input:: *)
(*endTime=2;*)
(*Z=DynamicLERWaction[g,endTime];*)


(* ::Input:: *)
(*(*WARNING: VERY SLOW, DO NOT RUN THIS LINE*)GeneralizedExpectationValue[Z \[Phi]s[2,Subscript[t, 1]]]*)


(* ::Input:: *)
(*(*The previous line gives*)-((4 \[Beta][1,2] \[Beta][1,3] \[Beta][2,1]^2)/((\[Beta][1,2]+\[Beta][1,3])^2 (\[Beta][2,1]+\[Beta][2,3])^2))+(4 \[Beta][1,3] \[Beta][2,1])/((\[Beta][1,2]+\[Beta][1,3]) (\[Beta][2,1]+\[Beta][2,3]))*)


(* ::Input:: *)
(*%/.\[Beta][__]->1*)


(* ::Subsubitem:: *)
(*Done with the enhanced WickContraction*)


(* ::Input:: *)
(*fields={\[Chi],\[Phi],\[Psi]};*)
(*endTime=2;*)
(*Z=DynamicLERWaction[g,endTime];*)
(*ExpectationValue[Z ,"fields"->fields,"endTime"->endTime,"graph"->g,"draw"->True]*)
(*Z Op[\[Phi]s[2,Subscript[t, 1],0]];*)
(*ExpectationValue[Z Op[\[Phi]s[2,Subscript[t, 1],0]],"fields"->fields,"endTime"->endTime,"graph"->g,"draw"->True]*)


(* ::Input:: *)
(*1+V[1+\[Phi][1,Subscript[t, 3],0]] V[1+\[Phi][2,Subscript[t, 3],0]] V[1+\[Phi][3,Subscript[t, 3],0]] V[1+\[Psi][1,Subscript[t, 3],Subscript[i, -1]]] V[1+\[Psi][2,Subscript[t, 3],Subscript[i, -1]]] V[1+\[Psi][3,Subscript[t, 3],Subscript[i, -1]]]+(\[Beta][1,2]^2 \[Beta][2,1]^2)/((\[Beta][1,2]+\[Beta][1,3])^2 (\[Beta][2,1]+\[Beta][2,3])^2)+(V[1+\[Phi][1,Subscript[t, 3],0]] V[1+\[Phi][2,Subscript[t, 3],0]] V[1+\[Phi][3,Subscript[t, 3],0]] V[1+\[Psi][1,Subscript[t, 3],Subscript[i, -1]]] V[1+\[Psi][2,Subscript[t, 3],Subscript[i, -1]]] V[1+\[Psi][3,Subscript[t, 3],Subscript[i, -1]]] \[Beta][1,2]^2 \[Beta][2,1]^2)/((\[Beta][1,2]+\[Beta][1,3])^2 (\[Beta][2,1]+\[Beta][2,3])^2)-(2 \[Beta][1,2] \[Beta][2,1])/((\[Beta][1,2]+\[Beta][1,3]) (\[Beta][2,1]+\[Beta][2,3]))-(2 V[1+\[Phi][1,Subscript[t, 3],0]] V[1+\[Phi][2,Subscript[t, 3],0]] V[1+\[Phi][3,Subscript[t, 3],0]] V[1+\[Psi][1,Subscript[t, 3],Subscript[i, -1]]] V[1+\[Psi][2,Subscript[t, 3],Subscript[i, -1]]] V[1+\[Psi][3,Subscript[t, 3],Subscript[i, -1]]] \[Beta][1,2] \[Beta][2,1])/((\[Beta][1,2]+\[Beta][1,3]) (\[Beta][2,1]+\[Beta][2,3]))/.f_[a_,Subscript[t, 3],b_]->f[a,b];*)
(*WickContraction[V[1]*%,"fields"->fields,"endTime"->0]*)


(* ::Input:: *)
(*(*expected*)*)


(* ::Input:: *)
(*\[Phi]s[2,Subscript[t, 1],0] (\[Beta][2,1] \[Phi][2,Subscript[t, 1],Subscript[k, 2]] \[Phi]s[1,Subscript[t, 2],Subscript[k, 2]] \[Chi]s[1,Subscript[t, 1],1] \[Psi]s[2,Subscript[t, 2],Subscript[i, -1]])/(\[Beta][2,1]+\[Beta][2,3]) ((\[Beta][1,3] \[Chi][1,Subscript[t, 1],Subscript[i, 1]] \[Chi]s[3,Subscript[t, 1],Subscript[i, 1]])/(\[Beta][1,2]+\[Beta][1,3]) \[Chi][3,Subscript[t, 1],1])((\[Beta][1,3] \[Phi][1,Subscript[t, 2],Subscript[k, 1]] \[Phi]s[3,Subscript[t, 3],Subscript[k, 1]] \[Chi]s[3,Subscript[t, 2],1] \[Psi]s[1,Subscript[t, 3],Subscript[i, -1]])/(\[Beta][1,2]+\[Beta][1,3]) \[Phi][3,Subscript[t, 3],0]\[Psi][1,Subscript[t, 3],Subscript[i, -1]]\[Chi][3,Subscript[t, 2],1])\[Psi][2,Subscript[t, 2],Subscript[i, -1]]\[Psi]s[2,Subscript[t, 3],Subscript[i, -1]]\[Psi][2,Subscript[t, 3],Subscript[i, -1]]*)


(* ::Input:: *)
(*(*Weight*)*)


(* ::Input:: *)
(*\[Phi]s[2,Subscript[t, 1],0] (\[Beta][2,1] \[Phi][2,Subscript[t, 1],Subscript[k, 2]] \[Phi]s[1,Subscript[t, 2],Subscript[k, 2]] \[Chi]s[1,Subscript[t, 1],1] \[Psi]s[2,Subscript[t, 2],Subscript[i, -1]])/(\[Beta][2,1]+\[Beta][2,3]) ((\[Beta][1,3] \[Chi][1,Subscript[t, 1],Subscript[i, 1]] \[Chi]s[3,Subscript[t, 1],Subscript[i, 1]])/(\[Beta][1,2]+\[Beta][1,3]) \[Chi][3,Subscript[t, 1],1])((\[Beta][1,3] \[Phi][1,Subscript[t, 2],Subscript[k, 1]] \[Phi]s[3,Subscript[t, 3],Subscript[k, 1]] \[Chi]s[3,Subscript[t, 2],1] \[Psi]s[1,Subscript[t, 3],Subscript[i, -1]])/(\[Beta][1,2]+\[Beta][1,3]) \[Phi][3,Subscript[t, 3],0]\[Psi][1,Subscript[t, 3],Subscript[i, -1]]\[Chi][3,Subscript[t, 2],1])\[Psi][2,Subscript[t, 2],Subscript[i, -1]]\[Psi]s[2,Subscript[t, 3],Subscript[i, -1]]\[Psi][2,Subscript[t, 3],Subscript[i, -1]]//.f_[_,_,_]->1*)


(* ::Input:: *)
(*(*THEY SEEM TO MATCH!!!!!!!!!*)*)


(* ::Item:: *)
(*Regular 2x2 lattice with absorption (i.e. source) at 4*)


(* ::Input:: *)
(*edges={{1,2},{1,3},{2,4},{3,4}};*)
(*root=1;*)
(*source=4;*)
(*g=MyGraph[edges,root,source][[1]];*)
(*g;*)
(*fields={\[Chi],\[Phi],\[Psi]};*)
(*endTime=1;*)
(*Z=DynamicLERWaction[g,endTime]*)


(* ::Input:: *)
(*Z/.V[A_]->A/.R[___]->1;*)
(*GeneralizedExpectationValue[%]*)


(* ::Input:: *)
(*edges={{1,2},{1,3},{2,4},{3,4}};*)
(*root=1;*)
(*source=4;*)
(*g=MyGraph[edges,root,source][[1]];*)
(*g;*)
(*fields={\[Chi],\[Phi],\[Psi]};*)
(*endTime=1;*)
(*Z=DynamicLERWaction[g,endTime];*)
(**)
(*ExpectationValue[Z ,"fields"->fields,"endTime"->endTime,"graph"->g,"draw"->False]*)


(* ::Subitem:: *)
(*Two time-slices (it's very slow, I would need kay's trick for the vertices AND NOW I HAVE IT!!!!)*)


(* ::Input:: *)
(*edges={{1,2},{1,3},{2,4},{3,4}};*)
(*root=1;*)
(*source=4;*)
(*g=MyGraph[edges,root,source][[1]];*)
(*g;*)
(*fields={\[Chi],\[Phi],\[Psi]};*)
(*endTime=2;*)
(*Z=DynamicLERWaction[g,endTime];*)
(*ExpectationValue[Z ,"fields"->fields,"endTime"->endTime,"graph"->g,"draw"->False]*)


(* ::Item:: *)
(*My favorite graph*)


(* ::Subitem:: *)
(*Partition function*)


(* ::Input:: *)
(*myFavoriteEdges={{1,2},{1,5},{2,3},{2,5},{5,4},{3,4}};*)
(*myFavoriteRoot=1;*)
(*myFavoriteSource=4;*)
(*myFavoriteGraph=MyGraph[myFavoriteEdges,myFavoriteRoot,myFavoriteSource][[1]];*)


(* ::Input:: *)
(*fields={\[Chi],\[Phi],\[Psi]};*)
(*endTime=1;*)
(*Z=DynamicLERWaction[myFavoriteGraph,endTime];*)
(**)
(*ExpectationValue[Z ,"fields"->fields,"endTime"->endTime,"graph"->g,"draw"->False]*)


(* ::Subitem:: *)
(*With a source at 1:*)


(* ::Input:: *)
(*g=myFavoriteGraph;*)
(*fields={\[Chi],\[Phi],\[Psi]};*)
(*endTime=2;*)
(*Z=DynamicLERWaction[g,endTime];*)
(*Z Op[\[Phi]s[1,Subscript[t, 1],0]];*)
(*ExpectationValue[Z Op[\[Phi]s[1,Subscript[t, 1],0]],"fields"->fields,"endTime"->endTime,"graph"->g,"draw"->True]*)


(* ::Section::Closed:: *)
(*DynamicDLAaction[]*)


(* ::Text:: *)
(*This action should be:*)


(* ::Input:: *)
(*\!\(TraditionalForm\`\**)
(*SuperscriptBox[*)
(*StyleBox["e", "TI"], *)
(*RowBox[{"-", *)
(*SubscriptBox[*)
(*StyleBox["S", "TI"], *)
(*StyleBox["t", "TI"]]}]] == \**)
(*UnderscriptBox["\[Product]", *)
(*StyleBox["x", "TI"],*)
(*LimitsPositioning->True]\**)
(*SuperscriptBox[*)
(*StyleBox["e", "TI"], *)
(*RowBox[{"-", *)
(*UnderscriptBox["\[Sum]", *)
(*StyleBox["i", "TI"],*)
(*LimitsPositioning->True], *)
(*SubsuperscriptBox["\[Chi]", *)
(*StyleBox["i", "TI"], "*"], *)
(*RowBox[{"(", *)
(*RowBox[{*)
(*StyleBox["x", "TI"], ",", *)
(*StyleBox["t", "TI"]}], ")"}], *)
(*SubscriptBox["\[Chi]", *)
(*StyleBox["i", "TI"]], *)
(*RowBox[{"(", *)
(*RowBox[{*)
(*StyleBox["x", "TI"], ",", *)
(*StyleBox["t", "TI"]}], ")"}], "-", *)
(*SuperscriptBox["\[Phi]", "*"], *)
(*RowBox[{"(", *)
(*RowBox[{*)
(*StyleBox["x", "TI"], ",", *)
(*StyleBox["t", "TI"]}], ")"}], "\[Phi]", *)
(*RowBox[{"(", *)
(*RowBox[{*)
(*StyleBox["x", "TI"], ",", *)
(*StyleBox["t", "TI"]}], ")"}]}]] \**)
(*UnderscriptBox["\[Product]", *)
(*RowBox[{*)
(*StyleBox["x", "TI"], "\[Element]", *)
(*StyleBox["G", "TI"], "\\", *)
(*RowBox[{"{", *)
(*StyleBox["s", "TI"], "}"}]}],*)
(*LimitsPositioning->True]{1 + \**)
(*UnderscriptBox["\[Sum]", *)
(*StyleBox["y", "TI"],*)
(*LimitsPositioning->True]\**)
(*FractionBox[*)
(*SubscriptBox["\[Beta]", *)
(*StyleBox[*)
(*RowBox[{"x", "y"}], "TI"]], *)
(*SubscriptBox[*)
(*StyleBox["r", "TI"], *)
(*StyleBox["x", "TI"]]] *)
(*\*SuperscriptBox[\(\[Phi]\), \(*\)] \((\**)
(*StyleBox["y", "TI"], \**)
(*StyleBox["t", "TI"] + 1)\) \[Phi] \((\**)
(*StyleBox["x", "TI"], \**)
(*StyleBox["t", "TI"])\) *)
(*\*SuperscriptBox[\(\[Phi]\), \(*\)] \((\**)
(*StyleBox["x", "TI"], \**)
(*StyleBox["t", "TI"] + 1)\) *)
(*\*SubsuperscriptBox[\(\[Chi]\), \(1\), \(*\)] \((\**)
(*StyleBox["y", "TI"], \**)
(*StyleBox["t", "TI"])\) + \**)
(*UnderscriptBox["\[Sum]", *)
(*StyleBox["y", "TI"],*)
(*LimitsPositioning->True]\**)
(*FractionBox[*)
(*SubscriptBox["\[Beta]", *)
(*StyleBox[*)
(*RowBox[{"x", "y"}], "TI"]], *)
(*SubscriptBox[*)
(*StyleBox["r", "TI"], *)
(*StyleBox["x", "TI"]]] \**)
(*UnderoverscriptBox["\[Sum]", *)
(*StyleBox["i", "TI"], *)
(*RowBox[{*)
(*StyleBox["N", "TI"], "==", *)
(*RowBox[{"-", "1"}]}],*)
(*LimitsPositioning->True]\**)
(*SubsuperscriptBox["\[Chi]", *)
(*StyleBox["i", "TI"], "*"] \((\**)
(*StyleBox["y", "TI"], \**)
(*StyleBox["t", "TI"])\) \**)
(*SubscriptBox["\[Chi]", *)
(*StyleBox["i", "TI"]] \((\**)
(*StyleBox["x", "TI"], \**)
(*StyleBox["t", "TI"])\) + *)
(*\*SuperscriptBox[\(\[Phi]\), \(*\)] \((\**)
(*StyleBox["x", "TI"], \**)
(*StyleBox["t", "TI"] + 1)\) \[Phi] \((\**)
(*StyleBox["x", "TI"], \**)
(*StyleBox["t", "TI"])\)}\[Cross]\(\([\)\(1 + \[Phi] \((\**)
(*StyleBox["x", "TI"], \**)
(*StyleBox["T", "TI"] + 1)\)\)\(]\)\) \(\([\)\(1 + \**)
(*SubscriptBox[*)
(*StyleBox["a", "TI"], *)
(*StyleBox["s", "TI"]] \((\**)
(*StyleBox["t", "TI"])\) *)
(*\*SubscriptBox[\(\[Chi]\), \(1\)] \((\**)
(*StyleBox["s", "TI"], \**)
(*StyleBox["t", "TI"])\)\)\(]\)\)\)*)


(* ::Text:: *)
(*For fields that do not have a color index, we default to set it to Subscript[j, -1], because in the WickContraction[] it automated with color indices.*)
(*Instead, the source is set with index 1, so that it can contract with an external "probe".*)


(* ::Input:: *)
(*Clear[DynamicDLAaction];*)
(*Options[DynamicDLAaction]={"sourceBC"->1,"excludedVertices"->{}};*)
(**)
(*DynamicDLAaction[graph_,endTime_,OptionsPattern[]]:=Module[{locSource,locRoot,locWeights,totWeights,locVertices,actionTimeSlice,action},*)
(*Clear[i,k,j];*)
(**)
(*locWeights=WeightedAdjacencyMatrix[graph]//Normal;*)
(*totWeights =Total[locWeights,{2}];*)
(**)
(*locVertices=VertexList@graph;*)
(**)
(*locRoot = *)
(* Select[(List@@@PropertyValue[graph,VertexLabels])[[2;;]],StringMatchQ[#[[2]],___~~"ROOT"]&][[All,1]][[1]];*)
(*locSource = *)
(* Select[(List@@@PropertyValue[graph,VertexLabels])[[2;;]],StringMatchQ[#[[2]],___~~"SOURCE"]&][[All,1]][[1]];*)
(**)
(**)
(*actionTimeSlice [T_]:= Product[ If[totWeights[[x]]===0  || MemberQ[OptionValue["excludedVertices"],x],1,(*FALSE*)*)
(*V[(1+ Sum[locWeights[[x,y]]R[x,y,Red]\[Phi]s[y,Subscript[t, T+1],Subscript[k, x]]\[Phi][x,Subscript[t, T],Subscript[k, x]]\[Phi]s[x,Subscript[t, T+1],Subscript[k, x]]\[Chi]s[y,Subscript[t, T],1],{y,Length[locVertices]}]+Sum[locWeights[[x,y]]/totWeights[[x]] R[x,y,Blue]\[Chi]s[y,Subscript[t, T],Subscript[i, x]]\[Chi][x,Subscript[t, T],Subscript[i, x]],{y,Length[locVertices]}]+ \[Phi]s[x,Subscript[t, T+1],Subscript[k, x]]\[Phi][x,Subscript[t, T],Subscript[k, x]]) ]],{x,Length[locVertices]}]*)
(*V[(1+ OptionValue["sourceBC"] \[Chi][locSource,Subscript[t, T],1])];*)
(**)
(*action=Product[actionTimeSlice[T],{T,endTime}]Product[V[(1+\[Phi][x,Subscript[t, endTime+1],Subscript[k, x]])(*This allows the path tracker to be absorbed at the end*) ]V[(1+\[Phi][x,Subscript[t, endTime+1],0])(*This allows the path to end after "endTime" steps*)],{x,Length[locVertices]}];*)
(**)
(*Return[action]*)
(**)
(*]*)


(* ::Subsection::Closed:: *)
(*Usage example of DynamicDLAaction[]*)


(* ::Item:: *)
(*My favorite graph*)


(* ::Input:: *)
(*myFavoriteEdges={{1,2},{1,5},{2,3},{2,5},{5,4},{3,4}};*)
(*myFavoriteRoot=1;*)
(*myFavoriteSource=4;*)
(*myFavoriteGraph=MyGraph[myFavoriteEdges,myFavoriteRoot,myFavoriteSource][[1]]*)


(* ::Subitem::Closed:: *)
(*Partition Function*)


(* ::Input:: *)
(*fields={\[Phi],\[Chi]};*)
(*endTime=1;*)
(*g=myFavoriteGraph;*)
(*Z=DynamicDLAaction[myFavoriteGraph,endTime];*)
(*ExpectationValue[Z,"endTime"->endTime,"fields"->fields,"draw"->True]*)


(* ::Subitem::Closed:: *)
(*Observables: Op[\[Phi]s[1,Subscript[t, 1],0]]*)


(* ::Input:: *)
(*fields={\[Phi],\[Chi]};*)
(*endTime=1;*)
(*g=myFavoriteGraph;*)
(*Z=DynamicDLAaction[myFavoriteGraph,endTime];*)
(*ExpectationValue[Z Op[\[Phi]s[1,Subscript[t, 1],0]],"endTime"->endTime,"fields"->fields,"draw"->False]*)


(* ::Subsubitem::Closed:: *)
(*Here I would like to compare it to the result obtained by solving the laplace eq but I still need the normalization from the FT*)


(* ::Input:: *)
(*sol=LaplaceEqSolver[g];*)
(*phi2=Select[sol,#[[1]]==\[CapitalPhi][2] &][[1,2]];*)
(*phi5=Select[sol,#[[1]]==\[CapitalPhi][5] &][[1,2]];*)
(*phi2/(phi2+phi5)//FullSimplify*)
(*phi5/(phi2+phi5)//FullSimplify*)
(**)


(* ::Subitem::Closed:: *)
(*Observables: Op[\[Chi]s[1,Subscript[t, 1],1]], i.e. the solution to the Laplace eq*)


(* ::Input:: *)
(*fields={\[Phi],\[Chi]};*)
(*endTime=1;*)
(*g=myFavoriteGraph;*)
(*Z=DynamicDLAaction[g,endTime];*)
(*ExpectationValue[Z  Op[\[Chi]s[4,Subscript[t, 1],1]],"endTime"->endTime,"fields"->fields,"draw"->False]*)


(* ::Input:: *)
(*g=AugmentedGraph[myFavoriteGraph]*)
(*Z=DynamicDLAaction[g,endTime];*)
(**)
(*ExpectationValue[Z  ,"endTime"->endTime,"fields"->fields,"draw"->False]*)


(* ::Subsubitem::Closed:: *)
(*\[CapitalPhi] evaluated at the new source:*)


(* ::Input:: *)
(*Z=DynamicDLAaction[g,endTime,"excludedVertices"->{1}];*)
(*ExpectationValue[Z  Op[\[Chi]s[6,Subscript[t, 1],1]],"endTime"->endTime,"fields"->fields,"draw"->False]*)


(* ::Input:: *)
(*(*Which is the same as Z. CORRECT!*)*)


(* ::Subsubitem::Closed:: *)
(*\[CapitalPhi] evaluated at the old source			 CORRECT!!!!!*)


(* ::Input:: *)
(*Z=DynamicDLAaction[g,endTime,"excludedVertices"->{1}];*)
(*ExpectationValue[Z  Op[\[Chi]s[6,Subscript[t, 1],1]],"endTime"->endTime,"fields"->fields,"draw"->False]*)
(*ExpectationValue[Z  Op[\[Chi]s[4,Subscript[t, 1],1]],"endTime"->endTime,"fields"->fields,"draw"->False]*)


(* ::Input:: *)
(*(*Let's check that we get the same result by solving the Laplace equation. First, we need to properly normalize it, i.e. divide by Z*)*)


(* ::Input:: *)
(*Z=DynamicDLAaction[g,endTime,"excludedVertices"->{1}];*)
(*Phi4FT=ExpectationValue[Z  ,"operator"->\[Chi]s[4,Subscript[t, 1],1],"endTime"->endTime,"fields"->fields,"draw"->False]*)


(* ::Input:: *)
(*(*Now from the Laplace eq*)*)


(* ::Input:: *)
(*Phi4Laplace=LaplaceEqSolver[g,"selectSolution"->4]*)


(* ::Input:: *)
(*(*And now, let's subtract them*)*)


(* ::Input:: *)
(*Phi4Laplace-Phi4FT//FullSimplify*)


(* ::Input:: *)
(*(*CORRECT!!!!!!!!*)*)


(* ::Subsection::Closed:: *)
(*Let's check that kay's trick for the total current works: IT WORKS ONLY FOR SYMMETRICAL \[Beta] BUT WITH ANY \[Sigma]!!!!!!*)


(* ::Input:: *)
(*g=AugmentedGraph[myFavoriteGraph];*)
(*endTime=1;*)
(*fields={\[Phi],\[Chi]};*)
(*Z=DynamicDLAaction[g,endTime,"excludedVertices"->{1}];*)
(*Limit[\[Sigma](ExpectationValue[Z  Op[\[Chi]s[6,Subscript[t, 1],1]-\[Chi]s[4,Subscript[t, 1],1]],"endTime"->endTime,"fields"->fields,"draw"->False]/.Subscript[\[Beta], 4,6]->\[Sigma]),\[Sigma]->\[Infinity]]*)


(* ::Input:: *)
(*Z2=DynamicDLAaction[myFavoriteGraph,endTime,"excludedVertices"->{1}];*)
(*ExpectationValue[Z2  Op[\[Chi]s[2,Subscript[t, 1],1]],"endTime"->endTime,"fields"->fields,"draw"->False]+*)
(*ExpectationValue[Z2  Op[\[Chi]s[5,Subscript[t, 1],1]],"endTime"->endTime,"fields"->fields,"draw"->False]-*)
(**)
(*ExpectationValue[Z2  Op[\[Chi]s[2,Subscript[t, 1],1]+\[Chi]s[5,Subscript[t, 1],1]],"endTime"->endTime,"fields"->fields,"draw"->False]*)


(* ::Input:: *)
(*(*Ok, I can insert the operators directly inside Op[]*)*)


(* ::Item::Closed:: *)
(*Kay's trick works only with symmetrical weights!!!!! *)


(* ::Input:: *)
(*(*Now let's verify that kay's trick works*)*)


(* ::Input:: *)
(*Z=DynamicDLAaction[g,endTime,"excludedVertices"->{1}];*)
(*Limit[\[Sigma]*(ExpectationValue[Z  ,"operator"->(\[Chi]s[6,Subscript[t, 1],1]-\[Chi]s[4,Subscript[t, 1],1]),"endTime"->endTime,"fields"->fields,"draw"->False]/.Subscript[\[Beta], 4,6]->\[Sigma]),\[Sigma]->\[Infinity]]*)
(**)
(*Z2=DynamicDLAaction[myFavoriteGraph,endTime,"excludedVertices"->{1}];*)
(*ExpectationValue[Z2,"operator"-> (\[Beta][1,2]\[Chi]s[2,Subscript[t, 1],1]+\[Beta][1,5]\[Chi]s[5,Subscript[t, 1],1]),"endTime"->endTime,"fields"->fields,"draw"->False]*)
(*%%%-%//FullSimplify*)
(*%//.\[Beta][a_,b_]/;(a<b):>\[Beta][b,a]//FullSimplify*)


(* ::Input:: *)
(*Z=DynamicDLAaction[g,endTime,"excludedVertices"->{1}];*)
(*Limit[\[Sigma]*(ExpectationValue[Z Op[(\[Chi]s[6,Subscript[t, 1],1]-\[Chi]s[4,Subscript[t, 1],1])],"endTime"->endTime,"fields"->fields,"draw"->False]/.Subscript[\[Beta], 4,6]->\[Sigma]),\[Sigma]->\[Infinity]]*)
(**)
(*Z2=DynamicDLAaction[myFavoriteGraph,endTime,"excludedVertices"->{1}];*)
(*ExpectationValue[Z2 Op[(\[Beta][1,2]\[Chi]s[2,Subscript[t, 1],1]+\[Beta][1,5]\[Chi]s[5,Subscript[t, 1],1])],"endTime"->endTime,"fields"->fields,"draw"->False]*)
(*%%%-%//FullSimplify*)
(*%//.\[Beta][a_,b_]/;(a<b):>\[Beta][b,a]//FullSimplify*)


(* ::Item:: *)
(*Does it work for any \[Sigma]? YESSS*)


(* ::Input:: *)
(*g*)


(* ::Subitem:: *)
(*Exclude just 1: CORRECT!!!! *)


(* ::Input:: *)
(*AugmGraph=AugmentedGraph[myFavoriteGraph];*)
(*g=AugmGraph[[1]];*)
(*endTime=1;*)
(*fields={\[Phi],\[Chi]};*)
(*Z=DynamicDLAaction[g,endTime,"excludedVertices"->{1}];*)
(*exp1=(ExpectationValue[Z  Op[\[Chi]s[6,Subscript[t, 1],1]],"endTime"->endTime,"fields"->fields,"draw"->False]/.Subscript[\[Beta], 4,6]->\[Sigma])*)
(*exp2=(ExpectationValue[Z  Op[\[Chi]s[4,Subscript[t, 1],1]],"endTime"->endTime,"fields"->fields,"draw"->False]/.Subscript[\[Beta], 4,6]->\[Sigma])*)


(* ::Input:: *)
(*exp2/.\[Sigma]/(\[Sigma]+Subscript[\[Beta], 4,3]+Subscript[\[Beta], 4,5])*A_+\[Sigma]/(\[Sigma]+Subscript[\[Beta], 4,3]+Subscript[\[Beta], 4,5])*B_+\[Sigma]/(\[Sigma]+Subscript[\[Beta], 4,3]+Subscript[\[Beta], 4,5])->HoldForm[\[Sigma]/(\[Sigma]+Subscript[\[Beta], 4,3]+Subscript[\[Beta], 4,5])*(A+B+1)]*)


(* ::Input:: *)
(*\[Sigma] (exp1-exp2)*)


(* ::Input:: *)
(*Limit[\[Sigma] (exp1-exp2), \[Sigma] -> \[Infinity]]-((-((Subscript[\[Beta], 3,4] Subscript[\[Beta], 4,3])/((Subscript[\[Beta], 3,2]+Subscript[\[Beta], 3,4]) (\[Sigma]+Subscript[\[Beta], 4,3]+Subscript[\[Beta], 4,5])))+(Subscript[\[Beta], 2,5] Subscript[\[Beta], 3,4] Subscript[\[Beta], 4,3] Subscript[\[Beta], 5,2])/((Subscript[\[Beta], 2,1]+Subscript[\[Beta], 2,3]+Subscript[\[Beta], 2,5]) (Subscript[\[Beta], 3,2]+Subscript[\[Beta], 3,4]) (\[Sigma]+Subscript[\[Beta], 4,3]+Subscript[\[Beta], 4,5]) (Subscript[\[Beta], 5,1]+Subscript[\[Beta], 5,2]+Subscript[\[Beta], 5,4]))-(Subscript[\[Beta], 2,3] Subscript[\[Beta], 3,4] Subscript[\[Beta], 4,5] Subscript[\[Beta], 5,2])/((Subscript[\[Beta], 2,1]+Subscript[\[Beta], 2,3]+Subscript[\[Beta], 2,5]) (Subscript[\[Beta], 3,2]+Subscript[\[Beta], 3,4]) (\[Sigma]+Subscript[\[Beta], 4,3]+Subscript[\[Beta], 4,5]) (Subscript[\[Beta], 5,1]+Subscript[\[Beta], 5,2]+Subscript[\[Beta], 5,4]))-(Subscript[\[Beta], 2,5] Subscript[\[Beta], 3,2] Subscript[\[Beta], 4,3] Subscript[\[Beta], 5,4])/((Subscript[\[Beta], 2,1]+Subscript[\[Beta], 2,3]+Subscript[\[Beta], 2,5]) (Subscript[\[Beta], 3,2]+Subscript[\[Beta], 3,4]) (\[Sigma]+Subscript[\[Beta], 4,3]+Subscript[\[Beta], 4,5]) (Subscript[\[Beta], 5,1]+Subscript[\[Beta], 5,2]+Subscript[\[Beta], 5,4]))-(Subscript[\[Beta], 4,5] Subscript[\[Beta], 5,4])/((\[Sigma]+Subscript[\[Beta], 4,3]+Subscript[\[Beta], 4,5]) (Subscript[\[Beta], 5,1]+Subscript[\[Beta], 5,2]+Subscript[\[Beta], 5,4]))+(Subscript[\[Beta], 2,3] Subscript[\[Beta], 3,2] Subscript[\[Beta], 4,5] Subscript[\[Beta], 5,4])/((Subscript[\[Beta], 2,1]+Subscript[\[Beta], 2,3]+Subscript[\[Beta], 2,5]) (Subscript[\[Beta], 3,2]+Subscript[\[Beta], 3,4]) (\[Sigma]+Subscript[\[Beta], 4,3]+Subscript[\[Beta], 4,5]) (Subscript[\[Beta], 5,1]+Subscript[\[Beta], 5,2]+Subscript[\[Beta], 5,4])))(\[Sigma]+Subscript[\[Beta], 4,3]+Subscript[\[Beta], 4,5])+exp2 (\[Sigma]/(\[Sigma]+Subscript[\[Beta], 4,3]+Subscript[\[Beta], 4,5]))^-1 (Subscript[\[Beta], 4,3]+Subscript[\[Beta], 4,5]))//FullSimplify*)


(* ::Input:: *)
(*(*CORRECT!!! THE LIMIT \[Sigma]->\[Infinity] IS THE SAME AS THE BIG PARENTHESES. CHECK ONENOTE NOTEBOOK FOR DETAILS*)*)


(* ::Input:: *)
(*Limit[\[Sigma](1-\[Sigma]/(\[Sigma]+Subscript[\[Beta], 4,3]+Subscript[\[Beta], 4,5])),\[Sigma]->\[Infinity]]*)


(* ::Input:: *)
(*Limit[\[Sigma]((Subscript[\[Beta], 3,4] Subscript[\[Beta], 4,3])/((Subscript[\[Beta], 3,2]+Subscript[\[Beta], 3,4]) (\[Sigma]+Subscript[\[Beta], 4,3]+Subscript[\[Beta], 4,5]))),\[Sigma]->\[Infinity]]*)


(* ::Input:: *)
(**)
(*exp3=ExpectationValue[Z Op[\[Beta][1,2]\[Chi]s[2,Subscript[t, 1],1]+\[Beta][1,5]\[Chi]s[5,Subscript[t, 1],1]],"endTime"->endTime,"fields"->fields,"draw"->False]/.Subscript[\[Beta], 4,6]->\[Sigma]*)


(* ::Input:: *)
(*\[Sigma](exp1-exp2)-exp3//FullSimplify*)
(*%//.\[Beta][a_,b_]/;(a<b):>\[Beta][b,a]//FullSimplify*)


(* ::Subitem::Closed:: *)
(*Exclude 1 and 2: CORRECT!!!!*)


(* ::Input:: *)
(*AugmGraph=AugmentedGraph[myFavoriteGraph];*)
(*g=AugmGraph[[1]];*)
(*endTime=1;*)
(*fields={\[Phi],\[Chi]};*)
(*Z=DynamicDLAaction[g,endTime,"excludedVertices"->{1,2}];*)
(*exp1=\[Sigma](ExpectationValue[Z  Op[\[Chi]s[6,Subscript[t, 1],1]-\[Chi]s[4,Subscript[t, 1],1]],"endTime"->endTime,"fields"->fields,"draw"->False]/.Subscript[\[Beta], 4,6]->\[Sigma])*)


(* ::Input:: *)
(**)
(*exp2=ExpectationValue[Z Op[\[Beta][2,3]\[Chi]s[3,Subscript[t, 1],1]+(\[Beta][1,5]+\[Beta][2,5])\[Chi]s[5,Subscript[t, 1],1]],"endTime"->endTime,"fields"->fields,"draw"->False]/.Subscript[\[Beta], 4,6]->\[Sigma]*)


(* ::Input:: *)
(*exp1-exp2//FullSimplify*)
(*%//.\[Beta][a_,b_]/;(a<b):>\[Beta][b,a]//FullSimplify*)


(* ::Subsection::Closed:: *)
(*\[Beta] or \[Beta]/r? Answer: \[Beta]!!*)


(* ::Text:: *)
(*We want to check whether we need to use \[Beta] or \[Beta]/r in the DLA dynamical action*)


(* ::Item::Closed:: *)
(*My favorite graph*)


(* ::Input:: *)
(*myFavoriteEdges={{1,2},{1,5},{2,3},{2,5},{5,4},{3,4}};*)
(*myFavoriteRoot=1;*)
(*myFavoriteSource=4;*)
(*myFavoriteGraph=MyGraph[myFavoriteEdges,myFavoriteRoot,myFavoriteSource][[1]]*)


(* ::Input:: *)
(*g=AugmentedGraph[myFavoriteGraph];*)
(*endTime=1;*)
(*fields={\[Phi],\[Chi]};*)
(*Z=DynamicDLAaction[g,endTime,"excludedVertices"->{1}];*)
(*DLAnorm=Limit[\[Sigma](ExpectationValue[Z  Op[\[Chi]s[6,Subscript[t, 1],1]-\[Chi]s[4,Subscript[t, 1],1]],"endTime"->endTime,"fields"->fields,"draw"->False]/.Subscript[\[Beta], 4,6]->\[Sigma]),\[Sigma]->\[Infinity]]*)


(* ::Input:: *)
(*g=myFavoriteGraph;*)
(*endTime=1;*)
(*fields={\[Phi],\[Chi]};*)
(*Z=DynamicDLAaction[g,endTime,"excludedVertices"->{}];*)
(*exp=ExpectationValue[Z Op[\[Phi]s[1,Subscript[t, 1],0]],"endTime"->endTime,"fields"->fields,"draw"->False,"Rrule"->{}]/DLAnorm/.\[Beta][__]->1*)
(*ExtractPaths[exp](*This is nicely normalized!!!*)*)


(* ::Input:: *)
(*DrawFromExpression[exp];*)


(* ::Subitem::Closed:: *)
(*Now, if we use \[Beta]/r: NOT PROPERLY NORMALIZED*)


(* ::Input:: *)
(*g=AugmentedGraph[myFavoriteGraph];*)
(*endTime=1;*)
(*fields={\[Phi],\[Chi]};*)
(*Z=DynamicDLAaction[g,endTime,"excludedVertices"->{1}];*)
(*DLAnorm=Limit[\[Sigma](ExpectationValue[Z  Op[\[Chi]s[6,Subscript[t, 1],1]-\[Chi]s[4,Subscript[t, 1],1]],"endTime"->endTime,"fields"->fields,"draw"->False]/.Subscript[\[Beta], 4,6]->\[Sigma]),\[Sigma]->\[Infinity]]*)


(* ::Input:: *)
(*g=myFavoriteGraph;*)
(*endTime=1;*)
(*fields={\[Phi],\[Chi]};*)
(*Z=DynamicDLAaction[g,endTime,"excludedVertices"->{}];*)
(*exp=ExpectationValue[Z Op[\[Phi]s[1,Subscript[t, 1],0]],"endTime"->endTime,"fields"->fields,"draw"->False,"Rrule"->{}]/DLAnorm/.\[Beta][__]->1*)
(*ExtractPaths[exp]*)


(* ::Input:: *)
(*(*Not properly normalized!!!*)*)


(* ::Subsection::Closed:: *)
(*Can I send each field to zero immediately or do I need to do it at the end? Answer: YESSS!*)


(* ::Item:: *)
(*My favorite graph*)


(* ::Input:: *)
(*myFavoriteEdges={{1,2},{1,5},{2,3},{2,5},{5,4},{3,4}};*)
(*myFavoriteRoot=1;*)
(*myFavoriteSource=4;*)
(*myFavoriteGraph=MyGraph[myFavoriteEdges,myFavoriteRoot,myFavoriteSource][[1]]*)


(* ::Subitem::Closed:: *)
(*Partition Function right away*)


(* ::Input:: *)
(*fields={\[Phi],\[Chi]};*)
(*endTime=1;*)
(*g=myFavoriteGraph;*)
(*Z=DynamicDLAaction[myFavoriteGraph,endTime];*)
(*ExpectationValue[Z,"endTime"->endTime,"fields"->fields,"draw"->False]*)


(* ::Subsubitem:: *)
(*Partition function field by field*)


(* ::Input:: *)
(*fields={\[Phi]};*)
(*endTime=1;*)
(*g=myFavoriteGraph;*)
(*Z=DynamicDLAaction[myFavoriteGraph,endTime];*)
(*ExpectationValue[Z,"endTime"->endTime,"fields"->fields,"draw"->False]*)
(*fields={\[Chi]};*)
(*ExpectationValue[%%,"endTime"->endTime,"fields"->fields,"draw"->False]*)


(* ::Input:: *)
(*(*Ok, for the partition funciton it works. But in the partition function only \[Chi] is used*)*)


(* ::Subitem:: *)
(*Observables: Op[\[Phi]s[1,Subscript[t, 1],0]]*)


(* ::Subsubitem:: *)
(*Right away*)


(* ::Input:: *)
(*fields={\[Phi],\[Chi]};*)
(*endTime=1;*)
(*g=myFavoriteGraph;*)
(*Z=DynamicDLAaction[myFavoriteGraph,endTime];*)
(*ExpectationValue[Z Op[\[Phi]s[1,Subscript[t, 1],0]],"endTime"->endTime,"fields"->fields,"draw"->False,"Rrule"->{}]*)


(* ::Subsubitem:: *)
(*Field by field*)


(* ::Input:: *)
(*fields={\[Phi]};*)
(*endTime=1;*)
(*g=myFavoriteGraph;*)
(*Z=DynamicDLAaction[myFavoriteGraph,endTime];*)
(*ExpectationValue[Z Op[\[Phi]s[1,Subscript[t, 1],0]],"endTime"->endTime,"fields"->fields,"draw"->False,"Rrule"->{}]*)
(*fields={\[Chi]};*)
(*ExpectationValue[%%,"endTime"->endTime,"fields"->fields,"draw"->False "Rrule"->{}]*)


(* ::Input:: *)
(*(*OK!! IT SEEMS THAT I CAN DO FIRST A FIELD, SET TO ZERO, THEN THE OTHERS*)*)


(* ::Section::Closed:: *)
(*I would like to compute the denominator of the DLA transition probability. To do this, I try to use nn different fields and then send nn->-1.*)


(* ::Input:: *)
(*fields={\[Chi],\[Phi]};*)
(*edges = {{1,2},{2,3},{3,1}};*)
(*root=2;*)
(*source=3;*)
(*g=MyGraph[edges,root,source][[1]];*)
(*g=AugmentedGraph[g];*)
(**)
(*endTime=1;Z=DynamicDLAaction[g,endTime,"excludedVertices"->{}]*)


(* ::Input:: *)
(*ExpectationValue[Z,"endTime"->endTime,"fields"->fields,"draw"->False]*)


(* ::Input:: *)
(*ExpectationValue[Z Op[(\[Chi]s[4,Subscript[t, 1],1]-\[Chi]s[3,Subscript[t, 1],1])]Op[(\[Chi]s[4,Subscript[t, 1],1]-\[Chi]s[3,Subscript[t, 1],1])],"endTime"->endTime,"fields"->fields,"draw"->False]*)


(* ::Input:: *)
(*fields={\[Chi],\[Phi]};*)
(*edges = {{1,2},{2,3},{3,1}};*)
(*root=2;*)
(*source=3;*)
(*g=MyGraph[edges,root,source][[1]];*)
(*g=AugmentedGraph[g];*)
(**)
(*endTime=1;*)
(*Z=DynamicDLAaction[g,endTime,"excludedVertices"->{}];*)
(**)
(*Z1=ExpectationValue[Z,"endTime"->endTime,"fields"->fields,"draw"->False]*)


(* ::Input:: *)
(**)
(*endTime=3;*)
(*Z=DynamicDLAaction[g,endTime,"excludedVertices"->{}];*)
(*Z3=ExpectationValue[Z,"endTime"->endTime,"fields"->fields,"draw"->False]*)


(* ::Input:: *)
(*Z1^3-Z3//FullSimplify*)


(* ::Input:: *)
(*(*SO, USING nn TIME-SLICES YIELDS Z^nn!!!! THIS IS EXPECTED. DOES IT WORK FOR <\[Chi]^*\[Chi]>?? *)*)


(* ::Subitem:: *)
(*Does it work for <SuperStar[\[Chi]] \[Chi]>?*)


(* ::Input:: *)
(*fields={\[Chi],\[Phi]};*)
(*edges = {{1,2},{2,3},{3,1}};*)
(*root=2;*)
(*source=3;*)
(*g=MyGraph[edges,root,source][[1]];*)
(*g=AugmentedGraph[g];*)
(**)
(*endTime=1;*)
(*Z=DynamicDLAaction[g,endTime,"excludedVertices"->{}];*)
(**)
(*Z1=ExpectationValue[Z Op[\[Chi]s[2,Subscript[t, 1],1]],"endTime"->endTime,"fields"->fields,"draw"->True,"graph"->g]*)


(* ::Input:: *)
(*endTime=3;*)
(*Z=DynamicDLAaction[g,endTime,"excludedVertices"->{}];*)
(*Z3=ExpectationValue[Z Op[\[Chi]s[2,Subscript[t, 1],1]] Op[\[Chi]s[2,Subscript[t, 2],1]] Op[\[Chi]s[2,Subscript[t, 3],1]],"endTime"->endTime,"fields"->fields,"draw"->False]*)


(* ::Input:: *)
(*Z1^3-Z3//FullSimplify*)


(* ::Input:: *)
(*(*IT WORKS!!!!!!!!!!!*)
(*CAN I USE THIS TO COMPUTE THE DENOMINATOR?????*)*)


(* ::Item::Closed:: *)
(*Other attempt*)


(* ::Input:: *)
(*Z=V[1+(Subscript[R, 1,2,RGBColor[0, 0, 1]] Subscript[\[Beta], 1,2] Subscript[\[Chi], 1,Subscript[t, 1],Subscript[i, 1]] Subscript[SuperStar[\[Chi]], 2,Subscript[t, 1],Subscript[i, 1]])/(Subscript[\[Beta], 1,2]+Subscript[\[Beta], 1,3])+(Subscript[R, 1,3,RGBColor[0, 0, 1]] Subscript[\[Beta], 1,3] Subscript[\[Chi], 1,Subscript[t, 1],Subscript[i, 1]] Subscript[SuperStar[\[Chi]], 3,Subscript[t, 1],Subscript[i, 1]])/(Subscript[\[Beta], 1,2]+Subscript[\[Beta], 1,3])] V[1+(Subscript[R, 2,1,RGBColor[0, 0, 1]] Subscript[\[Beta], 2,1] Subscript[\[Chi], 2,Subscript[t, 1],Subscript[i, 2]] Subscript[SuperStar[\[Chi]], 1,Subscript[t, 1],Subscript[i, 2]])/(Subscript[\[Beta], 2,1]+Subscript[\[Beta], 2,3])+(Subscript[R, 2,3,RGBColor[0, 0, 1]] Subscript[\[Beta], 2,3] Subscript[\[Chi], 2,Subscript[t, 1],Subscript[i, 2]] Subscript[SuperStar[\[Chi]], 3,Subscript[t, 1],Subscript[i, 2]])/(Subscript[\[Beta], 2,1]+Subscript[\[Beta], 2,3])] V[1+(Subscript[R, 3,1,RGBColor[0, 0, 1]] Subscript[\[Beta], 3,1] Subscript[\[Chi], 3,Subscript[t, 1],Subscript[i, 3]] Subscript[SuperStar[\[Chi]], 1,Subscript[t, 1],Subscript[i, 3]])/(Subscript[\[Beta], 3,1]+Subscript[\[Beta], 3,2]+Subscript[\[Beta], 3,4])+(Subscript[R, 3,2,RGBColor[0, 0, 1]] Subscript[\[Beta], 3,2] Subscript[\[Chi], 3,Subscript[t, 1],Subscript[i, 3]] Subscript[SuperStar[\[Chi]], 2,Subscript[t, 1],Subscript[i, 3]])/(Subscript[\[Beta], 3,1]+Subscript[\[Beta], 3,2]+Subscript[\[Beta], 3,4])+(Subscript[R, 3,4,RGBColor[0, 0, 1]] Subscript[\[Beta], 3,4] Subscript[\[Chi], 3,Subscript[t, 1],Subscript[i, 3]] Subscript[SuperStar[\[Chi]], 4,Subscript[t, 1],Subscript[i, 3]])/(Subscript[\[Beta], 3,1]+Subscript[\[Beta], 3,2]+Subscript[\[Beta], 3,4])];Z=Z/.\[Chi][a_,b_,c_]->(\[Phi][a,b,c])/.\[Chi]s[a_,b_,c_]->\[Phi]s[a,b,c]*)


(* ::Input:: *)
(*fields={\[Chi],\[Phi]};*)
(*ExpectationValue[Z,"endTime"->endTime,"fields"->fields,"draw"->False]-(1-(Subscript[\[Beta], 1,2] Subscript[\[Beta], 2,1])/((Subscript[\[Beta], 1,2]+Subscript[\[Beta], 1,3]) (Subscript[\[Beta], 2,1]+Subscript[\[Beta], 2,3]))-(Subscript[\[Beta], 1,3] Subscript[\[Beta], 3,1])/((Subscript[\[Beta], 1,2]+Subscript[\[Beta], 1,3]) (Subscript[\[Beta], 3,1]+Subscript[\[Beta], 3,2]+Subscript[\[Beta], 3,4]))-(Subscript[\[Beta], 1,2] Subscript[\[Beta], 2,3] Subscript[\[Beta], 3,1])/((Subscript[\[Beta], 1,2]+Subscript[\[Beta], 1,3]) (Subscript[\[Beta], 2,1]+Subscript[\[Beta], 2,3]) (Subscript[\[Beta], 3,1]+Subscript[\[Beta], 3,2]+Subscript[\[Beta], 3,4]))-(Subscript[\[Beta], 1,3] Subscript[\[Beta], 2,1] Subscript[\[Beta], 3,2])/((Subscript[\[Beta], 1,2]+Subscript[\[Beta], 1,3]) (Subscript[\[Beta], 2,1]+Subscript[\[Beta], 2,3]) (Subscript[\[Beta], 3,1]+Subscript[\[Beta], 3,2]+Subscript[\[Beta], 3,4]))-(Subscript[\[Beta], 2,3] Subscript[\[Beta], 3,2])/((Subscript[\[Beta], 2,1]+Subscript[\[Beta], 2,3]) (Subscript[\[Beta], 3,1]+Subscript[\[Beta], 3,2]+Subscript[\[Beta], 3,4])))^2//FullSimplify*)


(* ::Title:: *)
(*Final Dynamical theories (?)*)


(* ::Section::Closed:: *)
(*FinalDynamicLERWaction[]*)


(* ::Text:: *)
(*This action should be:*)


(* ::Input:: *)
(*\!\(TraditionalForm\`\**)
(*SuperscriptBox[*)
(*StyleBox["e", "TI"], *)
(*RowBox[{"-", *)
(*SubscriptBox[*)
(*StyleBox["S", "TI"], *)
(*StyleBox["t", "TI"]]}]] == \**)
(*UnderscriptBox["\[Product]", *)
(*StyleBox["x", "TI"],*)
(*LimitsPositioning->True]\**)
(*SuperscriptBox[*)
(*StyleBox["e", "TI"], *)
(*RowBox[{"-", *)
(*UnderscriptBox["\[Sum]", *)
(*StyleBox["i", "TI"],*)
(*LimitsPositioning->True], *)
(*SubsuperscriptBox["\[Chi]", *)
(*StyleBox["i", "TI"], "*"], *)
(*RowBox[{"(", *)
(*RowBox[{*)
(*StyleBox["x", "TI"], ",", *)
(*StyleBox["t", "TI"]}], ")"}], *)
(*SubscriptBox["\[Chi]", *)
(*StyleBox["i", "TI"]], *)
(*RowBox[{"(", *)
(*RowBox[{*)
(*StyleBox["x", "TI"], ",", *)
(*StyleBox["t", "TI"]}], ")"}], "-", *)
(*SuperscriptBox["\[Psi]", "*"], *)
(*RowBox[{"(", *)
(*RowBox[{*)
(*StyleBox["x", "TI"], ",", *)
(*StyleBox["t", "TI"]}], ")"}], "\[Psi]", *)
(*RowBox[{"(", *)
(*RowBox[{*)
(*StyleBox["x", "TI"], ",", *)
(*StyleBox["t", "TI"]}], ")"}], "-", *)
(*SuperscriptBox["\[Phi]", "*"], *)
(*RowBox[{"(", *)
(*RowBox[{*)
(*StyleBox["x", "TI"], ",", *)
(*StyleBox["t", "TI"]}], ")"}], "\[Phi]", *)
(*RowBox[{"(", *)
(*RowBox[{*)
(*StyleBox["x", "TI"], ",", *)
(*StyleBox["t", "TI"]}], ")"}]}]] \**)
(*UnderscriptBox["\[Product]", *)
(*RowBox[{*)
(*StyleBox["x", "TI"], "\[Element]", *)
(*StyleBox["G", "TI"], "\\", *)
(*RowBox[{"{", *)
(*StyleBox["s", "TI"], "}"}]}],*)
(*LimitsPositioning->True]{1 + \**)
(*UnderscriptBox["\[Sum]", *)
(*StyleBox["y", "TI"],*)
(*LimitsPositioning->True]\**)
(*FractionBox[*)
(*SubscriptBox["\[Beta]", *)
(*StyleBox[*)
(*RowBox[{"x", "y"}], "TI"]], *)
(*SubscriptBox[*)
(*StyleBox["r", "TI"], *)
(*StyleBox["x", "TI"]]] *)
(*\*SuperscriptBox[\(\[Phi]\), \(*\)] \((\**)
(*StyleBox["y", "TI"], \**)
(*StyleBox["t", "TI"] + 1)\) \[Phi] \((\**)
(*StyleBox["x", "TI"], \**)
(*StyleBox["t", "TI"])\) *)
(*\*SuperscriptBox[\(\[Psi]\), \(*\)] \((\**)
(*StyleBox["x", "TI"], \**)
(*StyleBox["t", "TI"] + 1)\) *)
(*\*SubsuperscriptBox[\(\[Chi]\), \(1\), \(*\)] \((\**)
(*StyleBox["y", "TI"], \**)
(*StyleBox["t", "TI"])\) + \**)
(*UnderscriptBox["\[Sum]", *)
(*StyleBox["y", "TI"],*)
(*LimitsPositioning->True]\**)
(*FractionBox[*)
(*SubscriptBox["\[Beta]", *)
(*StyleBox[*)
(*RowBox[{"x", "y"}], "TI"]], *)
(*SubscriptBox[*)
(*StyleBox["r", "TI"], *)
(*StyleBox["x", "TI"]]] \**)
(*UnderoverscriptBox["\[Sum]", *)
(*StyleBox["i", "TI"], *)
(*RowBox[{*)
(*StyleBox["N", "TI"], "==", *)
(*RowBox[{"-", "1"}]}],*)
(*LimitsPositioning->True]\**)
(*SubsuperscriptBox["\[Chi]", *)
(*StyleBox["i", "TI"], "*"] \((\**)
(*StyleBox["y", "TI"], \**)
(*StyleBox["t", "TI"])\) \**)
(*SubscriptBox["\[Chi]", *)
(*StyleBox["i", "TI"]] \((\**)
(*StyleBox["x", "TI"], \**)
(*StyleBox["t", "TI"])\) + *)
(*\*SuperscriptBox[\(\[Psi]\), \(*\)] \((\**)
(*StyleBox["x", "TI"], \**)
(*StyleBox["t", "TI"] + 1)\) \[Psi] \((\**)
(*StyleBox["x", "TI"], \**)
(*StyleBox["t", "TI"])\)}\[Cross]\(\([\)\(1 + \[Psi] \((\**)
(*StyleBox["x", "TI"], \**)
(*StyleBox["T", "TI"] + 1)\)\)\(]\)\) \(\([\)\(1 + \**)
(*SubscriptBox[*)
(*StyleBox["a", "TI"], *)
(*StyleBox["s", "TI"]] \((\**)
(*StyleBox["t", "TI"])\) *)
(*\*SubscriptBox[\(\[Chi]\), \(1\)] \((\**)
(*StyleBox["s", "TI"], \**)
(*StyleBox["t", "TI"])\)\)\(]\)\)\)*)


(* ::Text:: *)
(*For fields that do not have a color index, we default to set it to Subscript[j, -1], because in the WickContraction[] it automated with color indices.*)
(*Instead, the source is set with index 1, so that it can contract with an external "probe"*)


(* ::Input:: *)
(*Clear[FinalDynamicLERWaction];*)
(*Options[FinalDynamicLERWaction]={"sourceBC"->1,"excludedVertices"->{},"graph"->g,"startingTime"->1,"endTime"->10};*)
(**)
(*FinalDynamicLERWaction[OptionsPattern[]]:=Module[{locGraph,locStartingTime,locEndTime,locSource,locRoot,locWeights,totWeights,locVertices,actionTimeSlice,action},*)
(*Clear[i,k,j,\[Gamma]];*)
(**)
(**)
(*locGraph=OptionValue["graph"];*)
(*locStartingTime =OptionValue["startingTime"];*)
(*locEndTime =OptionValue["endTime"];*)
(**)
(*locWeights=WeightedAdjacencyMatrix[locGraph]//Normal;*)
(*totWeights =Total[locWeights,{2}];*)
(**)
(*locVertices=VertexList@locGraph;*)
(**)
(*locRoot = *)
(* Select[(List@@@PropertyValue[locGraph,VertexLabels])[[2;;]],StringMatchQ[#[[2]],___~~"ROOT"]&][[All,1]][[1]];*)
(*locSource = *)
(* Select[(List@@@PropertyValue[locGraph,VertexLabels])[[2;;]],StringMatchQ[#[[2]],___~~"SOURCE"]&][[All,1]][[1]];*)
(**)
(**)
(*actionTimeSlice [T_]:= Product[ *)
(*V[(1+\[Gamma] Sum[If[totWeights[[x]]===0 ,0,(*FALSE*)locWeights[[x,y]]/totWeights[[x]] (R[x,y,Red]\[Phi]s[y,Subscript[t, T+1],Subscript[k, x]]\[Psi]s[x,Subscript[t, T+1],Subscript[j, -1]]- \[Phi]s[x,Subscript[t, T+1],Subscript[k, x]])\[Phi][x,Subscript[t, T],Subscript[k, x]]\[Chi]s[y,Subscript[t, T],1]] ,{y,Length[locVertices]}]+Sum[If[totWeights[[x]]===0 ,0,(*FALSE*)locWeights[[x,y]]/totWeights[[x]] R[x,y,Blue]\[Chi]s[y,Subscript[t, T],Subscript[i, x]]\[Chi][x,Subscript[t, T],Subscript[i, x]]],{y,Length[locVertices]}]+ \[Psi]s[x,Subscript[t, T+1],Subscript[j, -1]]\[Psi][x,Subscript[t, T],Subscript[j, -1]]+\[Phi]s[x,Subscript[t, T+1],Subscript[k, x]]\[Phi][x,Subscript[t, T],Subscript[k, x]]) ],{x,Length[locVertices]}]*)
(*V[(1+ OptionValue["sourceBC"] \[Chi][locSource,Subscript[t, T],1])];*)
(**)
(*action=Product[actionTimeSlice[T],{T,locStartingTime,locEndTime}]Product[V[(1+\[Psi][x,Subscript[t, locEndTime+1],Subscript[j, -1]])(*This allows the PASSIVE path tracker to be absorbed at the end*) ]V[(1+\[Phi][x,Subscript[t, locEndTime+1],Subscript[k, x]])(*This allows the ACTIVE path tracker to be absorbed at the end*) ]V[(1+\[Phi][x,Subscript[t, locEndTime+1],0])(*This allows the path to end after "endTime" steps*)],{x,Length[locVertices]}];*)
(**)
(*Return[action]*)
(**)
(*]*)


(* ::Input:: *)
(*fields={\[Phi],\[Psi],\[Chi]};*)
(*startingTime=1;*)
(*endTime=1;*)
(*g=myFavoriteGraph;*)
(*Z=FinalDynamicLERWaction["graph"->g,"startingTime"->startingTime,"endTime"->endTime];*)
(*ExpectationValue[Z Op[Subscript[SuperStar[\[Phi]], 2,Subscript[t, 1],0] Subscript[SuperStar[\[Psi]], 1,Subscript[t, 1],Subscript[j, -1]]],"endTime"->endTime,"fields"->fields,"draw"->False,"Rrule"->{}]*)


(* ::Input:: *)
(*%/.\[Beta][__]->1/.R[__,Blue]->1*)


(* ::Subsection:: *)
(*Usage example of DynamicLERWaction[]*)


(* ::Item:: *)
(*My favorite graph*)


(* ::Input:: *)
(*myFavoriteEdges={{1,2},{1,5},{2,3},{2,5},{5,4},{3,4}};*)
(*myFavoriteRoot=1;*)
(*myFavoriteSource=4;*)
(*myFavoriteGraph=MyGraph[myFavoriteEdges,myFavoriteRoot,myFavoriteSource][[1]]*)


(* ::Subitem::Closed:: *)
(*Partition Function*)


(* ::Input:: *)
(*g=myFavoriteGraph;*)
(*endTime=1;*)
(*Z=FinalDynamicLERWaction["graph"->g,"endTime"->endTime]*)


(* ::Subitem:: *)
(*Observable Op[\[Phi]s[1,Subscript[t, 1],0]]*)


(* ::Input:: *)
(*fields={\[Phi],\[Psi],\[Chi]};*)
(*startingTime=1;*)
(*endTime=1;*)
(*g=myFavoriteGraph;*)
(*Z=FinalDynamicLERWaction["graph"->g,"startingTime"->startingTime,"endTime"->endTime];*)
(*exp=ExpectationValue[Z Op[Subscript[SuperStar[\[Phi]], 1,Subscript[t, 1],0]],"endTime"->endTime+1,"fields"->fields,"draw"->False,"Rrule"->{}]*)
(**)
(*(*Dexp=D[exp,\[Gamma]]/.\[Gamma]->0*)*)


(* ::Input:: *)
(*exp*18/13/.\[Beta][__]->1/.R[__,Blue]->1/.\[Gamma]->2 \[Gamma]//Expand*)


(* ::Input:: *)
(*%/.R[__,Red]->1*)


(* ::Input:: *)
(*(*Dexp=D[exp,\[Gamma]]/.\[Gamma]->0;*)*)
(*exp2=exp/.R[a_,b_,Red]->R[a,b,Red]Op[\[Phi]s[b,Subscript[t, 1],0]\[Psi]s[a,Subscript[t, 1],Subscript[j, -1]]]*)


(* ::Input:: *)
(*exp=ExpectationValue[Z exp2,"endTime"->endTime,"fields"->fields,"draw"->False,"Rrule"->{}]*)


(* ::Input:: *)
(*comp=exp*18/13/.\[Beta][__]->1/.R[__,Blue]->1/.\[Gamma]->2 \[Gamma]//Expand*)


(* ::Input:: *)
(*comp/.\[Gamma]^n_->\[Gamma]*)


(* ::Input:: *)
(*Dexp=D[exp,\[Gamma]]/.\[Gamma]->0;*)
(*exp2=Dexp/.R[a_,b_,Red]R[b_,c_,Red]->R[a,b,Red]R[b,c,Red]Op[\[Phi]s[c,Subscript[t, 1],0]\[Psi]s[b,Subscript[t, 1],Subscript[j, -1]]\[Psi]s[a,Subscript[t, 1],Subscript[j, -1]]]*)


(* ::Input:: *)
(*exp=ExpectationValue[Z exp2,"endTime"->endTime,"fields"->fields,"draw"->False,"Rrule"->{}]*)


(* ::Input:: *)
(*Dexp=D[exp,\[Gamma]]/.\[Gamma]->0;*)
(*exp2=Dexp/.R[a_,b_,Red]R[b_,c_,Red]R[c_,d_,Red]->R[a,b,Red]R[b,c,Red]R[c,d,Red]Op[\[Phi]s[c,Subscript[t, 1],0]\[Psi]s[c,Subscript[t, 1],Subscript[j, -1]]\[Psi]s[b,Subscript[t, 1],Subscript[j, -1]]\[Psi]s[a,Subscript[t, 1],Subscript[j, -1]]]*)


(* ::Input:: *)
(*ExpectationValue[Z Op[Subscript[SuperStar[\[Phi]], 2,Subscript[t, 1],0] Subscript[SuperStar[\[Psi]], 1,Subscript[t, 1],Subscript[j, -1]]],"endTime"->endTime,"fields"->fields,"draw"->False,"Rrule"->{}]*)


(* ::Input:: *)
(*exp/.R[__,Blue]->1/.\[Beta][__]->1*)


(* ::Input:: *)
(*%/.\[Gamma]->1/.R[__]->1*)


(* ::Section::Closed:: *)
(*FinalDynamicDLAaction[]	THIS IS OUR BENCHMARK*)


(* ::Text:: *)
(*This action should be:*)


(* ::Input:: *)
(*\!\(TraditionalForm\`\**)
(*SuperscriptBox[*)
(*StyleBox["e", "TI"], *)
(*RowBox[{"-", *)
(*SubscriptBox[*)
(*StyleBox["S", "TI"], *)
(*StyleBox["t", "TI"]]}]] == \**)
(*UnderscriptBox["\[Product]", *)
(*StyleBox["x", "TI"],*)
(*LimitsPositioning->True]\**)
(*SuperscriptBox[*)
(*StyleBox["e", "TI"], *)
(*RowBox[{"-", *)
(*UnderscriptBox["\[Sum]", *)
(*StyleBox["i", "TI"],*)
(*LimitsPositioning->True], *)
(*SubsuperscriptBox["\[Chi]", *)
(*StyleBox["i", "TI"], "*"], *)
(*RowBox[{"(", *)
(*RowBox[{*)
(*StyleBox["x", "TI"], ",", *)
(*StyleBox["t", "TI"]}], ")"}], *)
(*SubscriptBox["\[Chi]", *)
(*StyleBox["i", "TI"]], *)
(*RowBox[{"(", *)
(*RowBox[{*)
(*StyleBox["x", "TI"], ",", *)
(*StyleBox["t", "TI"]}], ")"}], "-", *)
(*SuperscriptBox["\[Phi]", "*"], *)
(*RowBox[{"(", *)
(*RowBox[{*)
(*StyleBox["x", "TI"], ",", *)
(*StyleBox["t", "TI"]}], ")"}], "\[Phi]", *)
(*RowBox[{"(", *)
(*RowBox[{*)
(*StyleBox["x", "TI"], ",", *)
(*StyleBox["t", "TI"]}], ")"}]}]]\[Cross]\**)
(*UnderscriptBox["\[Product]", *)
(*RowBox[{*)
(*StyleBox["x", "TI"], "\[Element]", *)
(*StyleBox["G", "TI"], "\\", *)
(*RowBox[{"{", *)
(*StyleBox["s", "TI"], "}"}]}],*)
(*LimitsPositioning->True]{1 + \**)
(*UnderscriptBox["\[Sum]", *)
(*StyleBox["y", "TI"],*)
(*LimitsPositioning->True]\**)
(*FractionBox[*)
(*SubscriptBox["\[Beta]", *)
(*StyleBox[*)
(*RowBox[{"x", "y"}], "TI"]], *)
(*SubscriptBox[*)
(*StyleBox["r", "TI"], *)
(*StyleBox["x", "TI"]]] \**)
(*UnderoverscriptBox["\[Sum]", *)
(*StyleBox["i", "TI"], *)
(*RowBox[{*)
(*StyleBox["N", "TI"], "==", *)
(*RowBox[{"-", "1"}]}],*)
(*LimitsPositioning->True]\**)
(*SubsuperscriptBox["\[Chi]", *)
(*StyleBox["i", "TI"], "*"] \((\**)
(*StyleBox["y", "TI"], \**)
(*StyleBox["t", "TI"])\) \**)
(*SubscriptBox["\[Chi]", *)
(*StyleBox["i", "TI"]] \((\**)
(*StyleBox["x", "TI"], \**)
(*StyleBox["t", "TI"])\) + *)
(*\*SuperscriptBox[\(\[Phi]\), \(*\)] \((\**)
(*StyleBox["x", "TI"], \**)
(*StyleBox["t", "TI"] + 1)\) \[Phi] \((\**)
(*StyleBox["x", "TI"], \**)
(*StyleBox["t", "TI"])\) + \n+\[Gamma] \**)
(*UnderscriptBox["\[Sum]", *)
(*StyleBox["y", "TI"],*)
(*LimitsPositioning->True]\**)
(*SubscriptBox["\[Beta]", *)
(*StyleBox[*)
(*RowBox[{"x", "y"}], "TI"]] \(( *)
(*\*SuperscriptBox[\(\[Phi]\), \(*\)] \((\**)
(*StyleBox["y", "TI"], \**)
(*StyleBox["t", "TI"] + 1)\) *)
(*\*SuperscriptBox[\(\[Phi]\), \(*\)] \((\**)
(*StyleBox["x", "TI"], \**)
(*StyleBox["t", "TI"] + 1)\) - *)
(*\*SuperscriptBox[\(\[Phi]\), \(*\)] \((\**)
(*StyleBox["x", "TI"], \**)
(*StyleBox["t", "TI"] + 1)\))\) \[Phi] \((\**)
(*StyleBox["x", "TI"], \**)
(*StyleBox["t", "TI"])\) *)
(*\*SubsuperscriptBox[\(\[Chi]\), \(1\), \(*\)] \((\**)
(*StyleBox["y", "TI"], \**)
(*StyleBox["t", "TI"])\) *)
(*\*SubscriptBox[\(\[Chi]\), \(1\)] \((\**)
(*StyleBox["s", "TI"], \**)
(*StyleBox["t", "TI"])\)} . \)*)


(* ::Text:: *)
(*For fields that do not have a color index, we default to set it to Subscript[j, -1], because in the WickContraction[] it automated with color indices.*)
(*Instead, the source is set with index 1, so that it can contract with an external "probe".*)


(* ::Input::Initialization:: *)
Clear[FinalDynamicDLAaction];
Options[FinalDynamicDLAaction]={"sourceBC"->1,"excludedVertices"->{},"graph"->g,"endTime"->10};

FinalDynamicDLAaction[OptionsPattern[]]:=Module[{locGraph,locEndTime,locSource,locRoot,locWeights,totWeights,locVertices,actionTimeSlice,action},
Clear[i,k,j,\[Gamma]];

locGraph=OptionValue["graph"];
locEndTime =OptionValue["endTime"];

locWeights=WeightedAdjacencyMatrix[locGraph]//Normal;
totWeights =Total[locWeights,{2}];

locVertices=VertexList@locGraph;

locRoot = 
 Select[(List@@@PropertyValue[locGraph,VertexLabels])[[2;;]],StringMatchQ[#[[2]],___~~"ROOT"]&][[All,1]][[1]];
locSource = 
 Select[(List@@@PropertyValue[locGraph,VertexLabels])[[2;;]],StringMatchQ[#[[2]],___~~"SOURCE"]&][[All,1]][[1]];


actionTimeSlice [T_]:= Product[
If[totWeights[[x]]===0  || MemberQ[OptionValue["excludedVertices"],x],1,
(*FALSE*)V[(1+\[Gamma] Sum[locWeights[[x,y]](R[x,y,Red]\[Phi]s[y,Subscript[t, T+1],Subscript[k, x]]-1)\[Phi]s[x,Subscript[t, T+1],Subscript[k, x]]\[Phi][x,Subscript[t, T],Subscript[k, x]]\[Chi]s[y,Subscript[t, T],1],{y,Length[locVertices]}]+
Sum[locWeights[[x,y]]/totWeights[[x]] R[x,y,Blue]\[Chi]s[y,Subscript[t, T],Subscript[i, x]]\[Chi][x,Subscript[t, T],Subscript[i, x]],{y,Length[locVertices]}]+ \[Phi]s[x,Subscript[t, T+1],Subscript[k, x]]\[Phi][x,Subscript[t, T],Subscript[k, x]]) ]
],{x,Length[locVertices]}]
V[(1+ \[Phi]s[locSource,Subscript[t, T+1],Subscript[k, locSource]]\[Phi][locSource,Subscript[t, T],Subscript[k, locSource]]+OptionValue["sourceBC"] \[Chi][locSource,Subscript[t, T],1])]
(*V[(1+ OptionValue["sourceBC"] \[Chi][locSource,Subscript[t, T],1])]*);

action=Product[actionTimeSlice[T],{T,locEndTime}](*Product[V[(1+\[Phi][x,Subscript[t, locEndTime+1],Subscript[k, x]])(*This allows the path tracker to be absorbed at the end*) ]V[(1+\[Phi][x,Subscript[t, locEndTime+1],0])(*This allows the path to end after "endTime" steps*)],{x,Length[locVertices]}]*);

Return[action]

]


(* ::Subsection:: *)
(*Usage example of DynamicDLAaction[]*)


(* ::Item:: *)
(*My favorite graph*)


(* ::Input::Initialization:: *)
myFavoriteEdges={{1,2},{1,5},{2,3},{2,5},{5,4},{3,4}};
myFavoriteRoot=1;
myFavoriteSource=4;
myFavoriteGraph=MyGraph[myFavoriteEdges,myFavoriteRoot,myFavoriteSource][[1]]


(* ::Subitem::Closed:: *)
(*Partition Function*)


(* ::Input:: *)
(*Z=FinalDynamicDLAaction["graph"->g,"endTime"->endTime]*)


(* ::Input:: *)
(*fields={\[Phi],\[Chi]};*)
(*endTime=1;*)
(*g=myFavoriteGraph;*)
(*Z=FinalDynamicDLAaction["graph"->g,"endTime"->endTime];*)
(*ExpectationValue[Z,"endTime"->endTime,"fields"->fields,"draw"->False]*)


(* ::Subitem::Closed:: *)
(*Observables: Op[\[Phi]s[1,Subscript[t, 1],0]]*)


(* ::Input:: *)
(*fields=FieldsNamesGenerator[{\[Phi],\[Chi]}];*)
(*endTime=1;*)
(*g=myFavoriteGraph;*)
(*Z=FinalDynamicDLAaction["graph"->g,"endTime"->endTime]/.\[Beta][__]->1;*)
(*expDLAcorrect=ExpectationValueBlock[Z Op[\[Phi]s[1,Subscript[t, 1],Subscript[k, 1]]],"endTime"->endTime,"fields"->fields,"draw"->False,"Rrule"->{R[__,Blue]->1},"external\[Delta]"->True,"graph"->g]*18/13/.\[Gamma]->"\!\(\*SubscriptBox[\(\[Gamma]\), \(1\)]\)"//Expand*)


(* ::Input:: *)
(*expDLAcorrect/.\[Phi]s[a_,Subscript[t, b_],c_]->\[Phi]s[a,Subscript[t, b-1],Subscript[k, a]];*)
(*%/.\[Psi]s[a_,Subscript[t, b_],c_]->\[Psi]s[a,Subscript[t, b-1],Subscript[l, a]];*)
(*expDLAcorrect2=ExpectationValueBlock[Z %,"endTime"->endTime,"fields"->fields,"draw"->False,"Rrule"->{R[__,Blue]->1},"graph"->g]/.\[Gamma]->"\!\(\*SubscriptBox[\(\[Gamma]\), \(2\)]\)"*)


(* ::Input:: *)
(*expDLAcorrect2/.\[Phi]s[a_,Subscript[t, b_],c_]->\[Phi]s[a,Subscript[t, b-1],Subscript[k, a]];*)
(*%/.\[Psi]s[a_,Subscript[t, b_],c_]->\[Psi]s[a,Subscript[t, b-1],Subscript[l, a]];*)
(*expDLAcorrect3=ExpectationValueBlock[Z %,"endTime"->endTime,"fields"->fields,"draw"->False,"Rrule"->{R[__,Blue]->1},"graph"->g]/.\[Gamma]->"\!\(\*SubscriptBox[\(\[Gamma]\), \(3\)]\)"/.\[Phi]s[a_,Subscript[t, b_],c_]->\[Phi]s[a,Subscript[t, b],Subscript[k, a]]*)


(* ::Input:: *)
(*expDLAcorrect3-expDLA3*)


(* ::Input:: *)
(*comp=exp*18/13/.\[Beta][__]->1/.R[__,Blue]->1//Expand*)


(* ::Input:: *)
(*exp2=comp/.SuperStar[\[Phi]][a_,b_,c_]->Op[SuperStar[\[Phi]][a,b,c]]/.Subscript[t, a_]->Subscript[t, a-1]*)


(* ::Input:: *)
(*fields={\[Phi],\[Chi]};*)
(*endTime=1;*)
(*g=myFavoriteGraph;*)
(*Z=FinalDynamicDLAaction["graph"->g,"endTime"->endTime];*)
(*exp=ExpectationValue[Z exp2,"endTime"->endTime,"fields"->fields,"draw"->False,"Rrule"->{},"external\[Delta]"->True]*)


(* ::Input:: *)
(*comp2=exp/.\[Beta][__]->1/.R[__,Blue]->1//Expand*)


(* ::Input:: *)
(*exp3=comp2/.SuperStar[\[Phi]][a_,b_,c_]->Op[SuperStar[\[Phi]][a,b,c]]/.Subscript[t, a_]->Subscript[t, a-1]*)


(* ::Input:: *)
(*fields={\[Phi],\[Chi]};*)
(*endTime=1;*)
(*g=myFavoriteGraph;*)
(*Z=FinalDynamicDLAaction["graph"->g,"endTime"->endTime];*)
(*exp=ExpectationValue[Z exp3,"endTime"->endTime,"fields"->fields,"draw"->False,"Rrule"->{},"external\[Delta]"->True];*)
(*comp3=exp/.\[Beta][__]->1/.R[__,Blue]->1//Expand*)


(* ::Input:: *)
(*DrawFromExpression[comp2,"graph"->g]*)


(* ::Input:: *)
(**)
(*ExtractPaths[Dexp]/.\[Beta][__]->1*)


(* ::Input:: *)
(*exp/.\[Gamma]->0/.\[Beta][__]->1/.R[__]->1*)


(* ::Subsubitem:: *)
(*Here I would like to compare it to the result obtained by solving the laplace eq but I still need the normalization from the FT*)


(* ::Input:: *)
(*sol=LaplaceEqSolver[g];*)
(*phi2=Select[sol,#[[1]]==\[CapitalPhi][2] &][[1,2]];*)
(*phi5=Select[sol,#[[1]]==\[CapitalPhi][5] &][[1,2]];*)
(*phi2/(phi2+phi5)//FullSimplify*)
(*phi5/(phi2+phi5)//FullSimplify*)
(**)


(* ::Subitem:: *)
(*Normalized*)


(* ::Input::Initialization:: *)
fields=FieldsNamesGenerator[{\[Phi],\[Chi]}];
endTime=1;
g=myFavoriteGraph;
Z=FinalDynamicDLAaction["graph"->g,"endTime"->endTime]/.\[Beta][__]->1;
\[Phi]s[1,Subscript[t, 1],Subscript[k, 1]]/Znorm[\[Phi]s[1,Subscript[t, 1],Subscript[k, 1]],"graph"->g,"fields"->fields,"\[Beta]rule"->{\[Beta][__]->1}];
expDLAcorrectNorm=ExpectationValueBlock[Z %,"endTime"->endTime,"fields"->fields,"draw"->False,"Rrule"->{R[__,Blue]->1},"external\[Delta]"->True,"graph"->g]/.\[Gamma]->"\!\(\*SubscriptBox[\(\[Gamma]\), \(1\)]\)"//Expand


(* ::Input:: *)
(*expDLAcorrectNorm*)


(* ::Input::Initialization:: *)
expDLAcorrectNorm/.\[Phi]s[b_,Subscript[t, a_],c_]->\[Phi]s[b,Subscript[t, a-1],Subscript[k, b]];
Replace[%,a_/;(!NumericQ[a]):>a/(Znorm[a,"graph"->g,"fields"->fields,"\[Beta]rule"->{\[Beta][__]->1}]),1];
expDLAcorrectNorm2=ExpectationValueBlock[Z %,"endTime"->endTime,"fields"->fields,"draw"->False,"Rrule"->{R[__,Blue]->1},"graph"->g]/.\[Gamma]->"\!\(\*SubscriptBox[\(\[Gamma]\), \(2\)]\)"/.\[Phi]s[b_,Subscript[t, a_],c_]->\[Phi]s[b,Subscript[t, a-1],Subscript[k, b]];


(* ::Input::Initialization:: *)
fields=FieldsNamesGenerator[{\[Phi],\[Chi]}];
endTime=1;
g=myFavoriteGraph;
Z=FinalDynamicDLAaction["graph"->g,"endTime"->endTime]/.\[Beta][__]->1;
Replace[expDLAcorrectNorm2,a_/;(!NumericQ[a]):>a/(Znorm[a,"graph"->g,"fields"->fields,"\[Beta]rule"->{\[Beta][__]->1}]),1];
expDLAcorrectNorm3=ExpectationValueBlock[Z %,"endTime"->endTime,"fields"->fields,"draw"->False,"Rrule"->{R[__,Blue]->1},"graph"->g]/.\[Gamma]->"\!\(\*SubscriptBox[\(\[Gamma]\), \(3\)]\)"/.\[Phi]s[b_,Subscript[t, a_],c_]->\[Phi]s[b,Subscript[t, a],Subscript[k, b]]/.Subscript[t, a_]->Subscript[t, a-1];


(* ::Input:: *)
(*(*SO IT SEEMS THAT MY NEW ACTION YIELDS THE SAME RESULT AS THIS ONE. WHY IT DOES NOT CONVERGE THEN??????*)*)


(* ::Subitem::Closed:: *)
(*Observables: Op[\[Chi]s[1,Subscript[t, 1],1]], i.e. the solution to the Laplace eq*)


(* ::Input:: *)
(*fields={\[Phi],\[Chi]};*)
(*endTime=1;*)
(*g=myFavoriteGraph;*)
(*Z=DynamicDLAaction[g,endTime];*)
(*ExpectationValue[Z  Op[\[Chi]s[4,Subscript[t, 1],1]],"endTime"->endTime,"fields"->fields,"draw"->False]*)


(* ::Input:: *)
(*g=AugmentedGraph[myFavoriteGraph]*)
(*Z=DynamicDLAaction[g,endTime];*)
(**)
(*ExpectationValue[Z  ,"endTime"->endTime,"fields"->fields,"draw"->False]*)


(* ::Subsubitem::Closed:: *)
(*\[CapitalPhi] evaluated at the new source:*)


(* ::Input:: *)
(*Z=DynamicDLAaction[g,endTime,"excludedVertices"->{1}];*)
(*ExpectationValue[Z  Op[\[Chi]s[6,Subscript[t, 1],1]],"endTime"->endTime,"fields"->fields,"draw"->False]*)


(* ::Input:: *)
(*(*Which is the same as Z. CORRECT!*)*)


(* ::Subsubitem::Closed:: *)
(*\[CapitalPhi] evaluated at the old source			 CORRECT!!!!!*)


(* ::Input:: *)
(*Z=DynamicDLAaction[g,endTime,"excludedVertices"->{1}];*)
(*ExpectationValue[Z  Op[\[Chi]s[6,Subscript[t, 1],1]],"endTime"->endTime,"fields"->fields,"draw"->False]*)
(*ExpectationValue[Z  Op[\[Chi]s[4,Subscript[t, 1],1]],"endTime"->endTime,"fields"->fields,"draw"->False]*)


(* ::Input:: *)
(*(*Let's check that we get the same result by solving the Laplace equation. First, we need to properly normalize it, i.e. divide by Z*)*)


(* ::Input:: *)
(*Z=DynamicDLAaction[g,endTime,"excludedVertices"->{1}];*)
(*Phi4FT=ExpectationValue[Z  ,"operator"->\[Chi]s[4,Subscript[t, 1],1],"endTime"->endTime,"fields"->fields,"draw"->False]*)


(* ::Input:: *)
(*(*Now from the Laplace eq*)*)


(* ::Input:: *)
(*Phi4Laplace=LaplaceEqSolver[g,"selectSolution"->4]*)


(* ::Input:: *)
(*(*And now, let's subtract them*)*)


(* ::Input:: *)
(*Phi4Laplace-Phi4FT//FullSimplify*)


(* ::Input:: *)
(*(*CORRECT!!!!!!!!*)*)


(* ::Section::Closed:: *)
(*ConvergenceManual[]*)


(* ::Input:: *)
(*Clear[ConvergenceManual];*)
(*Options[ConvergenceManual]={"graph"->g,"fields"->FieldsNamesGenerator[{\[Phi],\[Chi]}],"steps"->10,"precision"->10^-9,"theory"->"DLA","gammaValue"->0.1};*)
(**)
(*ConvergenceManual[operator_,OptionsPattern[]]:=Module[{Z,locOperator,locPrecision,loc\[Gamma],exp,i,temp,token},*)
(**)
(*i=1;*)
(**)
(*If[OptionValue["theory"]=="DLA",*)
(*Z=FinalDynamicDLAaction["graph"->OptionValue[ "graph"],"endTime"->1]/.\[Beta][__]->1.,*)
(*Z=FinalDynamicLERWaction["graph"->OptionValue[ "graph"],"endTime"->1]/.\[Beta][__]->1.*)
(*];*)
(**)
(*locOperator= operator + token;*)
(*locPrecision=OptionValue["precision"];*)
(*loc\[Gamma]=OptionValue["gammaValue"];*)
(**)
(*exp=Replace[locOperator,a_/;(!NumericQ[a]):>a/(Znorm[a,"graph"->OptionValue["graph"],"fields"->OptionValue["fields"]]),1];*)
(**)
(*exp=ExpectationValueBlock[Z exp,"endTime"->1,"fields"->OptionValue["fields"],"Rrule"->{R[__,Blue]->1},"graph"->OptionValue[ "graph"]];*)
(*exp=exp/.\[Phi]s[a_,Subscript[t, b_],c_]->\[Phi]s[a,Subscript[t, b-1],Subscript[k, a]];*)
(*exp=exp/.\[Psi]s[a_,Subscript[t, b_],c_]->\[Psi]s[a,Subscript[t, b-1],Subscript[l, a]];*)
(*exp=exp/.\[Gamma]->loc\[Gamma];*)
(**)
(*Print["########## i="<>ToString[i]<>", "<>ToString[(Coefficient[exp, \[Phi]s[1,Subscript[t, 1],Subscript[k, 1]]]/.R[__]->0)]<>" ############"];*)
(**)
(*exp=exp/.token->0;*)
(**)
(*For[i=2,i<=OptionValue["steps"] && (Coefficient[exp,  \[Phi]s[1,Subscript[t, 1],Subscript[k, 1]]]/.R[__]->0/.\[Gamma]->loc\[Gamma])>=locPrecision,i++,*)
(**)
(*exp=exp+token;*)
(*exp=Replace[exp,a_/;(!NumericQ[a]):>a/(Znorm[a,"graph"->OptionValue["graph"],"fields"->OptionValue["fields"]]),1];*)
(**)
(*exp=ExpectationValueBlock[Z exp,"endTime"->1,"fields"->OptionValue["fields"],"Rrule"->{R[__,Blue]->1},"graph"->OptionValue[ "graph"]];*)
(*exp=exp/.\[Phi]s[a_,Subscript[t, b_],c_]->\[Phi]s[a,Subscript[t, b-1],Subscript[k, a]];*)
(*exp=exp/.\[Psi]s[a_,Subscript[t, b_],c_]->\[Psi]s[a,Subscript[t, b-1],Subscript[l, a]];*)
(*exp=exp/.\[Gamma]->loc\[Gamma]/.token->0;*)
(**)
(*Print["########## i="<>ToString[i]<>", "<>ToString[(Coefficient[exp,  \[Phi]s[1,Subscript[t, 1],Subscript[k, 1]]]/.R[__]->0)]<>" ############"];*)
(*];*)
(**)
(*(*If[(Coefficient[exp, \[Phi]s[1,Subscript[t, 1],Subscript[k, 1]]]/.R[__]->0/.\[Gamma]->loc\[Gamma])<=locPrecision, *)
(*exp=exp/.\[Gamma]->loc\[Gamma]*)
(*];*)*)
(**)
(*Return[exp];*)
(*]*)


(* ::Subsubsection::Closed:: *)
(*Test ConvergenceManual[]*)


(* ::Item:: *)
(*IT WORKS*)


(* ::Input:: *)
(*myFavoriteEdges={{1,2},{1,5},{2,3},{2,5},{5,4},{3,4}};*)
(*myFavoriteRoot=1;*)
(*myFavoriteSource=4;*)
(*g=myFavoriteGraph=MyGraph[myFavoriteEdges,myFavoriteRoot,myFavoriteSource][[1]]*)


(* ::Input:: *)
(*g=myFavoriteGraph;*)
(*precision=10^-8;*)
(*expManual=ConvergenceManual[\[Phi]s[1,Subscript[t, 1],Subscript[k, 1]],"graph"->g,"steps"->100,"precision"->precision,"gammaValue"->0.3]*)


(* ::Input:: *)
(*expmanualTo4=expManual/.a_/;Abs[a]<precision:>0/.SuperStar[\[Phi]][__]->1*)


(* ::Text:: *)
(*GRAND TOTAL CHECK*)


(* ::Input:: *)
(*expmanualTo4-(30./77 Subscript[R, 1,5,RGBColor[1, 0, 0]] Subscript[R, 5,4,RGBColor[1, 0, 0]]);*)
(*%-(20./231 Subscript[R, 1,2,RGBColor[1, 0, 0]] Subscript[R, 2,5,RGBColor[1, 0, 0]] Subscript[R, 5,4,RGBColor[1, 0, 0]]+32./231 Subscript[R, 1,2,RGBColor[1, 0, 0]] Subscript[R, 1,5,RGBColor[1, 0, 0]] Subscript[R, 5,4,RGBColor[1, 0, 0]]+12./231 Subscript[R, 1,5,RGBColor[1, 0, 0]] Subscript[R, 5,2,RGBColor[1, 0, 0]] Subscript[R, 5,4,RGBColor[1, 0, 0]]);*)
(*%-(19./462 Subscript[R, 1,2,RGBColor[1, 0, 0]] Subscript[R, 2,3,RGBColor[1, 0, 0]] Subscript[R, 2,5,RGBColor[1, 0, 0]] Subscript[R, 5,4,RGBColor[1, 0, 0]]+19./462 Subscript[R, 1,2,RGBColor[1, 0, 0]] Subscript[R, 2,3,RGBColor[1, 0, 0]] Subscript[R, 2,5,RGBColor[1, 0, 0]] Subscript[R, 3,4,RGBColor[1, 0, 0]]+25./462 Subscript[R, 1,2,RGBColor[1, 0, 0]] Subscript[R, 1,5,RGBColor[1, 0, 0]] Subscript[R, 2,3,RGBColor[1, 0, 0]] Subscript[R, 5,4,RGBColor[1, 0, 0]]+25./462 Subscript[R, 1,2,RGBColor[1, 0, 0]] Subscript[R, 1,5,RGBColor[1, 0, 0]] Subscript[R, 2,3,RGBColor[1, 0, 0]] Subscript[R, 3,4,RGBColor[1, 0, 0]]+1./77 Subscript[R, 1,5,RGBColor[1, 0, 0]] Subscript[R, 2,3,RGBColor[1, 0, 0]] Subscript[R, 5,2,RGBColor[1, 0, 0]] Subscript[R, 5,4,RGBColor[1, 0, 0]]+1./77 Subscript[R, 1,5,RGBColor[1, 0, 0]] Subscript[R, 2,3,RGBColor[1, 0, 0]] Subscript[R, 3,4,RGBColor[1, 0, 0]] Subscript[R, 5,2,RGBColor[1, 0, 0]]);*)
(*%-(9./77 Subscript[R, 1,2,RGBColor[1, 0, 0]] Subscript[R, 2,3,RGBColor[1, 0, 0]] Subscript[R, 3,4,RGBColor[1, 0, 0]])*)
(*%/.a_/;Abs[a]<10^-7:>0*)
(*%/.R[__]->1*)


(* ::Item::Closed:: *)
(**)


(* ::Input:: *)
(*g=myFavoriteGraph;*)
(*Convergence[\[Phi]s[1,Subscript[t, 1],Subscript[k, 1]],"graph"->g,"fields"->{\[Phi],\[Xi],\[Chi]},"steps"->6,"precision"->10^-7]*)


(* ::Input:: *)
(*%/.f_[__]->1*)


(* ::Input:: *)
(*exp=%%*)


(* ::Input:: *)
(*g=myFavoriteGraph;*)
(*Convergence[exp,"graph"->g,"fields"->{\[Phi],\[Xi],\[Chi]},"steps"->6,"precision"->10^-7]*)


(* ::Input:: *)
(*fields={\[Phi],\[Xi],\[Chi]};*)
(*endTime=1;*)
(*g=myFavoriteGraph;*)
(*Z=Final2DynamicDLAaction["graph"->g,"endTime"->endTime]/.\[Beta][__]->1;*)
(*exp2=ExpectationValue[Z exp,"endTime"->endTime,"fields"->fields,"draw"->False,"Rrule"->{},"external\[Delta]"->True]*)


(* ::Input:: *)
(**)
(*exp2=exp2/.R[__,Blue]->1/.\[Gamma]->0.01//Expand;*)
(*exp2=exp2/.Subscript[t, a_]->Subscript[t, a-1];*)


(* ::Item::Closed:: *)
(**)


(* ::Input:: *)
(*g=myFavoriteGraph;*)
(*exp3=Convergence[\[Phi]s[1,Subscript[t, 1],Subscript[k, 1]],"graph"->g,"fields"->{\[Phi],\[Xi],\[Chi]},"steps"->4,"precision"->10^-7]*)


(* ::Input:: *)
(*g=myFavoriteGraph;*)
(*exp4=Convergence[exp3,"graph"->g,"fields"->{\[Phi],\[Xi],\[Chi]},"steps"->4,"precision"->10^-7]*)


(* ::Input:: *)
(*g=myFavoriteGraph;*)
(*exp5=Convergence[exp4,"graph"->g,"fields"->{\[Phi],\[Xi],\[Chi]},"steps"->5,"precision"->10^-7]*)


(* ::Input:: *)
(*g=myFavoriteGraph;*)
(*exp6=Convergence[exp5,"graph"->g,"fields"->{\[Phi],\[Xi],\[Chi]},"steps"->20,"precision"->10^-5]*)


(* ::Section:: *)
(*DynamicDLAactionInitial\[Pi][] WITH (SuperStar[\[Phi]]-1) SuperStar[\[Phi]], BUT with \[Pi] (here \[Psi] for convenience)*)


(* ::Text:: *)
(*In this version I use the initial vertex with (SuperStar[\[Phi]]-1) but I use an auxiliary field in the \[Gamma] term. This is arguably better(?)*)


(* ::Input:: *)
(*(*IT WORKS??*)*)


(* ::Input:: *)
(*Clear[DynamicDLAactionInitial\[Pi]];*)
(*Options[DynamicDLAactionInitial\[Pi]]={"sourceBC"->1,"excludedVertices"->{},"graph"->g,"endTime"->10};*)
(**)
(*DynamicDLAactionInitial\[Pi][OptionsPattern[]]:=Module[{locGraph,locEndTime,locSource,locRoot,locWeights,totWeights,locVertices,actionTimeSlice,action},*)
(*Clear[i,k,j,l,\[Gamma]];*)
(**)
(*locGraph=OptionValue["graph"];*)
(*locEndTime =OptionValue["endTime"];*)
(**)
(*locWeights=WeightedAdjacencyMatrix[locGraph]//Normal;*)
(*totWeights =Total[locWeights,{2}];*)
(**)
(*locVertices=VertexList@locGraph;*)
(**)
(*locRoot = *)
(* Select[(List@@@PropertyValue[locGraph,VertexLabels])[[2;;]],StringMatchQ[#[[2]],___~~"ROOT"]&][[All,1]][[1]];*)
(*locSource = *)
(* Select[(List@@@PropertyValue[locGraph,VertexLabels])[[2;;]],StringMatchQ[#[[2]],___~~"SOURCE"]&][[All,1]][[1]];*)
(**)
(*actionTimeSlice [T_]:= Product[*)
(*If[totWeights[[x]]===0  || MemberQ[OptionValue["excludedVertices"],x],1,*)
(*(*FALSE*)V[(1+\[Gamma] Sum[locWeights[[x,y]](R[x,y,Red]\[Psi]s[y,Subscript[t, T+1],Subscript[l, y]]\[Phi]s[x,Subscript[t, T+1],Subscript[k, x]]-\[Psi]s[x,Subscript[t, T+1],Subscript[l, x]])\[Chi]s[y,Subscript[t, T],1],{y,Length[locVertices]}]\[Psi][x,Subscript[t, T],Subscript[l, x]])]*)
(*V[(1+Sum[locWeights[[x,y]]/totWeights[[x]] R[x,y,Blue]\[Chi]s[y,Subscript[t, T],Subscript[i, x]]\[Chi][x,Subscript[t, T],Subscript[i, x]],{y,Length[locVertices]}]*)
(*+(*Convert*)\[Phi]s[x,Subscript[t, T+1],Subscript[k, x]]*\[Psi][x,Subscript[t, T],Subscript[l, x]]*)
(*+(*SPLIT*)\[Psi]s[x,Subscript[t, T],Subscript[l, x]]*\[Phi][x,Subscript[t, T],Subscript[k, x]]*)
(*+(*JUST COPY \[Phi]*)\[Phi]s[x,Subscript[t, T+1],Subscript[k, x]]*\[Phi][x,Subscript[t, T],Subscript[k, x]]) ]*)
(*],{x,Length[locVertices]}]*)
(*V[(1+ \[Phi]s[locSource,Subscript[t, T+1],Subscript[k, locSource]]\[Psi][locSource,Subscript[t, T],Subscript[l, locSource]]+ \[Phi]s[locSource,Subscript[t, T+1],Subscript[k, locSource]]\[Phi][locSource,Subscript[t, T],Subscript[k, locSource]]+\[Chi][locSource,Subscript[t, T],1])]*)
(*(*V[(1+ OptionValue["sourceBC"] \[Chi][locSource,Subscript[t, T],1])]*);*)
(**)
(*action=Product[actionTimeSlice[T],{T,1,locEndTime}](*Product[V[(1+\[Phi][x,Subscript[t, locEndTime+1],Subscript[k, x]])(*This allows the path tracker to be absorbed at the end*) ]V[(1+\[Phi][x,Subscript[t, locEndTime+1],0])(*This allows the path to end after "endTime" steps*)],{x,Length[locVertices]}]*);*)
(**)
(*Return[action]*)
(**)
(*]*)


(* ::Subsection:: *)
(*Test*)


(* ::Item:: *)
(*My favorite graph*)


(* ::Input:: *)
(*myFavoriteEdges={{1,2},{1,5},{2,3},{2,5},{5,4},{3,4}};*)
(*myFavoriteRoot=1;*)
(*myFavoriteSource=4;*)
(*g=myFavoriteGraph=MyGraph[myFavoriteEdges,myFavoriteRoot,myFavoriteSource][[1]]*)


(* ::Input:: *)
(*fields=FieldsNamesGenerator[{\[Phi],\[Psi],\[Chi]}];*)
(*endTime=1;*)
(*Z=DynamicDLAactionInitial\[Pi]["graph"->g,"endTime"->endTime]/.\[Beta][__]->1;*)
(*Z0=Z/.\[Gamma]->0;*)


(* ::Subitem:: *)
(*Normalized with purification*)


(* ::Input:: *)
(*\[Phi]s[1,Subscript[t, 1],Subscript[k, 1]]/Znorm[\[Phi]s[1,Subscript[t, 1],Subscript[k, 1]],"graph"->g,"fields"->fields,"\[Beta]rule"->{\[Beta][__]->1}];*)
(**)
(*ExpectationValueBlock[Z %,"endTime"->endTime,"fields"->fields,"draw"->False,"Rrule"->{R[__,Blue]->1},"graph"->g]/.\[Gamma]->"\!\(\*SubscriptBox[\(\[Gamma]\), \(1\)]\)"/.Subscript[t, a_]->Subscript[t, a-1]//Expand*)
(**)
(*Replace[%,a_/;(!NumericQ[a]):>a/(Znorm[a,"graph"->g,"fields"->fields,"\[Beta]rule"->{\[Beta][__]->1}]),1];*)
(*expDLANormInitial\[Pi]=ExpectationValueBlock[Z0 %,"endTime"->endTime,"fields"->fields,"draw"->False,"Rrule"->{R[__,Blue]->1},"graph"->g]/.\[Gamma]->"\!\(\*SubscriptBox[\(\[Gamma]\), \(1\)]\)"/.Subscript[t, a_]->Subscript[t, a-1]//Expand*)


(* ::Input:: *)
(*Replace[expDLANormInitial\[Pi],a_/;(!NumericQ[a]):>a/(Znorm[a,"graph"->g,"fields"->fields,"\[Beta]rule"->{\[Beta][__]->1}]),1];*)
(*ExpectationValueBlock[Z %,"endTime"->endTime,"fields"->fields,"draw"->False,"Rrule"->{R[__,Blue]->1},"graph"->g]/.\[Gamma]->"\!\(\*SubscriptBox[\(\[Gamma]\), \(2\)]\)"/.Subscript[t, a_]->Subscript[t, a-1]//Expand;*)
(**)
(*Replace[%,a_/;(!NumericQ[a]):>a/(Znorm[a,"graph"->g,"fields"->fields,"\[Beta]rule"->{\[Beta][__]->1}]),1];*)
(*expDLANormInitial\[Pi]2=ExpectationValueBlock[Z0 %,"endTime"->endTime,"fields"->fields,"draw"->False,"Rrule"->{R[__,Blue]->1},"graph"->g]/.\[Gamma]->"\!\(\*SubscriptBox[\(\[Gamma]\), \(2\)]\)"/.Subscript[t, a_]->Subscript[t, a-1]//Expand*)
(**)
(*expDLANormInitial\[Pi]2-expDLAcorrectNorm2*)


(* ::Input:: *)
(*expDLANormInitial\[Pi]2;*)
(*expDLANormInitial\[Pi]2-expDLAcorrectNorm2*)


(* ::Input:: *)
(*(*YESSS*)*)


(* ::Input:: *)
(*Replace[expDLANormInitial\[Pi]2,a_/;(!NumericQ[a]):>a/(Znorm[a,"graph"->g,"fields"->fields,"\[Beta]rule"->{\[Beta][__]->1}]),1];*)
(*ExpectationValueBlock[Z %,"endTime"->endTime,"fields"->fields,"draw"->False,"Rrule"->{R[__,Blue]->1},"graph"->g]/.\[Gamma]->"\!\(\*SubscriptBox[\(\[Gamma]\), \(3\)]\)"/.\[Gamma]->"\!\(\*SubscriptBox[\(\[Gamma]\), \(3\)]\)"/.Subscript[t, a_]->Subscript[t, a-1]//Expand;*)
(**)
(*Replace[%,a_/;(!NumericQ[a]):>a/(Znorm[a,"graph"->g,"fields"->fields,"\[Beta]rule"->{\[Beta][__]->1}]),1];*)
(*expDLANormInitial\[Pi]3=ExpectationValueBlock[Z0 %,"endTime"->endTime,"fields"->fields,"draw"->False,"Rrule"->{R[__,Blue]->1},"graph"->g]/.\[Gamma]->"\!\(\*SubscriptBox[\(\[Gamma]\), \(3\)]\)"/.Subscript[t, a_]->Subscript[t, a-1]//Expand*)
(**)
(*expDLANormInitial\[Pi]3-expDLAcorrectNorm3*)


(* ::Input:: *)
(*expDLANormKay\[Gamma]3-expDLAcorrectNorm3*)


(* ::Input:: *)
(*(*IT WORKS!!!!*)*)


(* ::Section:: *)
(*DynamicDLAactionKay\[Gamma][] without  (SuperStar[\[Phi]]-1) SuperStar[\[Phi]], with \[Pi] (here \[Psi] for convenience)  THE BEST?*)


(* ::Text:: *)
(*In this version I copy a double field and then annihilate it in the next step. So, I am effectively copying a single field. However, I do this so that I can simplify the \[Gamma] vertex. The goal is to have \[Gamma](\[Phi][y,t+1]-\[Phi][x,t+1])\[Chi] \[Phi] with possibly the \[Chi] that can enter the brackets. To do this I need a "purification" layer in which the fields are reset to order 1. I am also using mediators \[Psi]: at each time step the \[Phi]s become produce a \[Psi] and branch*)


(* ::Input:: *)
(*(*IT WORKS!!!!!!!*)*)


(* ::Input:: *)
(*Clear[DynamicDLAactionKay\[Gamma]];*)
(*Options[DynamicDLAactionKay\[Gamma]]={"sourceBC"->1,"excludedVertices"->{},"graph"->g,"endTime"->10};*)
(**)
(*DynamicDLAactionKay\[Gamma][OptionsPattern[]]:=Module[{locGraph,locEndTime,locSource,locRoot,locWeights,totWeights,locVertices,actionTimeSlice,action},*)
(*Clear[i,k,j,l,\[Gamma]];*)
(**)
(*locGraph=OptionValue["graph"];*)
(*locEndTime =OptionValue["endTime"];*)
(**)
(*locWeights=WeightedAdjacencyMatrix[locGraph]//Normal;*)
(*totWeights =Total[locWeights,{2}];*)
(**)
(*locVertices=VertexList@locGraph;*)
(**)
(*locRoot = *)
(* Select[(List@@@PropertyValue[locGraph,VertexLabels])[[2;;]],StringMatchQ[#[[2]],___~~"ROOT"]&][[All,1]][[1]];*)
(*locSource = *)
(* Select[(List@@@PropertyValue[locGraph,VertexLabels])[[2;;]],StringMatchQ[#[[2]],___~~"SOURCE"]&][[All,1]][[1]];*)
(**)
(*actionTimeSlice [T_]:= Product[*)
(*If[totWeights[[x]]===0  || MemberQ[OptionValue["excludedVertices"],x],1,*)
(*(*FALSE*)V[(1+\[Gamma] Sum[locWeights[[x,y]](R[x,y,Red]\[Psi]s[y,Subscript[t, T+1],Subscript[l, y]]-\[Psi]s[x,Subscript[t, T+1],Subscript[l, x]])\[Chi]s[y,Subscript[t, T],1],{y,Length[locVertices]}]\[Psi][x,Subscript[t, T],Subscript[l, x]])]*)
(*V[(1+Sum[locWeights[[x,y]]/totWeights[[x]] R[x,y,Blue]\[Chi]s[y,Subscript[t, T],Subscript[i, x]]\[Chi][x,Subscript[t, T],Subscript[i, x]],{y,Length[locVertices]}]*)
(*+(*COPY and ANNIHILATE*)\[Phi]s[x,Subscript[t, T+1],Subscript[k, x]]*\[Phi][x,Subscript[t, T],Subscript[k, x]]\[Psi][x,Subscript[t, T],Subscript[l, x]]*)
(*+(*COPY and SPLIT*)\[Phi]s[x,Subscript[t, T+1],Subscript[k, x]]\[Psi]s[x,Subscript[t, T],Subscript[l, x]]*\[Phi][x,Subscript[t, T],Subscript[k, x]]*)
(*+(*JUST COPY \[Phi]*)\[Phi]s[x,Subscript[t, T+1],Subscript[k, x]]*\[Phi][x,Subscript[t, T],Subscript[k, x]]*)
(*+(* COPY \[Psi] into \[Phi]*)\[Phi]s[x,Subscript[t, T+1],Subscript[k, x]]*\[Psi][x,Subscript[t, T],Subscript[l, x]]) ]*)
(*],{x,Length[locVertices]}]*)
(*V[(1+ \[Phi]s[locSource,Subscript[t, T+1],Subscript[k, locSource]]\[Psi][locSource,Subscript[t, T],Subscript[l, locSource]]+ \[Phi]s[locSource,Subscript[t, T+1],Subscript[k, locSource]]\[Phi][locSource,Subscript[t, T],Subscript[k, locSource]]+\[Chi][locSource,Subscript[t, T],1])]*)
(*(*V[(1+ OptionValue["sourceBC"] \[Chi][locSource,Subscript[t, T],1])]*);*)
(**)
(*action=Product[actionTimeSlice[T],{T,1,locEndTime}](*Product[V[(1+\[Phi][x,Subscript[t, locEndTime+1],Subscript[k, x]])(*This allows the path tracker to be absorbed at the end*) ]V[(1+\[Phi][x,Subscript[t, locEndTime+1],0])(*This allows the path to end after "endTime" steps*)],{x,Length[locVertices]}]*);*)
(**)
(*Return[action]*)
(**)
(*]*)


(* ::Subsection:: *)
(*Test*)


(* ::Item:: *)
(*My favorite graph*)


(* ::Input:: *)
(*myFavoriteEdges={{1,2},{1,5},{2,3},{2,5},{5,4},{3,4}};*)
(*myFavoriteRoot=1;*)
(*myFavoriteSource=4;*)
(*g=myFavoriteGraph=MyGraph[myFavoriteEdges,myFavoriteRoot,myFavoriteSource][[1]]*)


(* ::Input:: *)
(*fields=FieldsNamesGenerator[{\[Phi],\[Psi],\[Chi]}];*)
(*endTime=1;*)
(*Z=DynamicDLAactionKay\[Gamma]["graph"->g,"endTime"->endTime]/.\[Beta][__]->1;*)
(*Z0=Z/.\[Gamma]->0;*)


(* ::Subitem:: *)
(*Normalized with purification*)


(* ::Input:: *)
(*\[Phi]s[1,Subscript[t, 1],Subscript[k, 1]]/Znorm[\[Phi]s[1,Subscript[t, 1],Subscript[k, 1]],"graph"->g,"fields"->fields,"\[Beta]rule"->{\[Beta][__]->1}];*)
(**)
(*ExpectationValueBlock[Z %,"endTime"->endTime,"fields"->fields,"draw"->False,"Rrule"->{R[__,Blue]->1},"graph"->g]/.\[Gamma]->"\!\(\*SubscriptBox[\(\[Gamma]\), \(1\)]\)"/.Subscript[t, a_]->Subscript[t, a-1]//Expand*)
(**)
(*Replace[%,a_/;(!NumericQ[a]):>a/(Znorm[a,"graph"->g,"fields"->fields,"\[Beta]rule"->{\[Beta][__]->1}]),1];*)
(*expDLANormKay\[Gamma]=ExpectationValueBlock[Z0 %,"endTime"->endTime,"fields"->fields,"draw"->False,"Rrule"->{R[__,Blue]->1},"graph"->g]/.\[Gamma]->"\!\(\*SubscriptBox[\(\[Gamma]\), \(1\)]\)"/.Subscript[t, a_]->Subscript[t, a-1]//Expand*)


(* ::Input:: *)
(*Replace[expDLANormKay\[Gamma],a_/;(!NumericQ[a]):>a/(Znorm[a,"graph"->g,"fields"->fields,"\[Beta]rule"->{\[Beta][__]->1}]),1];*)
(*ExpectationValueBlock[Z %,"endTime"->endTime,"fields"->fields,"draw"->False,"Rrule"->{R[__,Blue]->1},"graph"->g]/.\[Gamma]->"\!\(\*SubscriptBox[\(\[Gamma]\), \(2\)]\)"/.Subscript[t, a_]->Subscript[t, a-1]//Expand;*)
(**)
(*Replace[%,a_/;(!NumericQ[a]):>a/(Znorm[a,"graph"->g,"fields"->fields,"\[Beta]rule"->{\[Beta][__]->1}]),1];*)
(*expDLANormKay\[Gamma]2=ExpectationValueBlock[Z0 %,"endTime"->endTime,"fields"->fields,"draw"->False,"Rrule"->{R[__,Blue]->1},"graph"->g]/.\[Gamma]->"\!\(\*SubscriptBox[\(\[Gamma]\), \(2\)]\)"/.Subscript[t, a_]->Subscript[t, a-1]//Expand*)


(* ::Input:: *)
(*expDLANormKay\[Gamma]2;*)
(*expDLANormKay\[Gamma]2-expDLAcorrectNorm2*)


(* ::Input:: *)
(*(*YESSS*)*)


(* ::Input:: *)
(*Replace[expDLANormKay\[Gamma]2,a_/;(!NumericQ[a]):>a/(Znorm[a,"graph"->g,"fields"->fields,"\[Beta]rule"->{\[Beta][__]->1}]),1];*)
(*ExpectationValueBlock[Z %,"endTime"->endTime,"fields"->fields,"draw"->False,"Rrule"->{R[__,Blue]->1},"graph"->g]/.\[Gamma]->"\!\(\*SubscriptBox[\(\[Gamma]\), \(3\)]\)"/.\[Gamma]->"\!\(\*SubscriptBox[\(\[Gamma]\), \(3\)]\)"/.Subscript[t, a_]->Subscript[t, a-1]//Expand;*)
(**)
(*Replace[%,a_/;(!NumericQ[a]):>a/(Znorm[a,"graph"->g,"fields"->fields,"\[Beta]rule"->{\[Beta][__]->1}]),1];*)
(*expDLANormKay\[Gamma]3=ExpectationValueBlock[Z0 %,"endTime"->endTime,"fields"->fields,"draw"->False,"Rrule"->{R[__,Blue]->1},"graph"->g]/.\[Gamma]->"\!\(\*SubscriptBox[\(\[Gamma]\), \(3\)]\)"/.Subscript[t, a_]->Subscript[t, a-1]//Expand*)


(* ::Input:: *)
(*expDLANormKay\[Gamma]3-expDLAcorrectNorm3*)


(* ::Input:: *)
(*(*IT WORKS!!!!*)*)


(* ::Subsubsection::Closed:: *)
(*ConvergenceKay\[Gamma][]*)


(* ::Input::Initialization:: *)
Clear[ConvergenceKay\[Gamma]];
Options[ConvergenceKay\[Gamma]]={"graph"->g,"fields"->FieldsNamesGenerator[{\[Phi],\[Psi],\[Chi]}],"steps"->10,"precision"->10^-9,"theory"->"DLA","gammaValue"->0.1};

ConvergenceKay\[Gamma][operator_,OptionsPattern[]]:=Module[{Z,Z0,locOperator,locPrecision,loc\[Gamma],exp,i=1,temp},

(*If[OptionValue["theory"]=="DLA",
Z=DynamicDLAactionKay\[Gamma]["graph"->OptionValue[ "graph"],"endTime"->1]/.\[Beta][__]->1.
];*)


Z=DynamicDLAactionKay\[Gamma]["graph"->OptionValue[ "graph"],"endTime"->1]/.\[Beta][__]->1.;
Z0=Z/.\[Gamma]->0;

locOperator= operator;
locPrecision=OptionValue["precision"];
loc\[Gamma]=OptionValue["gammaValue"];

exp=locOperator/(Znorm[locOperator,"graph"->OptionValue["graph"],"fields"->OptionValue["fields"]]);

exp=ExpectationValueBlock[Z exp,"endTime"->1,"fields"->OptionValue["fields"],"Rrule"->{R[__,Blue]->1.},"graph"->OptionValue[ "graph"]];
exp=exp/.Subscript[t, a_]->Subscript[t, a-1]/.\[Gamma]->loc\[Gamma];

(*Purifying step*)
exp=Replace[exp,a_/;(!NumericQ[a]):>a/(Znorm[a,"graph"->OptionValue["graph"],"fields"->OptionValue["fields"]]),1];

exp=ExpectationValueBlock[Z0 exp,"endTime"->1,"fields"->OptionValue["fields"],"Rrule"->{R[__,Blue]->1.},"graph"->OptionValue[ "graph"]];
exp=exp/.Subscript[t, a_]->Subscript[t, a-1]/.\[Gamma]->loc\[Gamma];

Print["########## i=",i,", ",(Coefficient[exp, \[Phi]s[1,Subscript[t, 1],Subscript[k, 1]]]/.R[__]->0)," ############"];

For[i=2,i<=OptionValue["steps"] && (Coefficient[exp, \[Phi]s[1,Subscript[t, 1],Subscript[k, 1]]]/.R[__]->0)>=locPrecision,i++,


exp=Replace[exp,a_/;(!NumericQ[a]):>a/(Znorm[a,"graph"->OptionValue["graph"],"fields"->OptionValue["fields"]]),1];

exp=ExpectationValueBlock[Z exp,"endTime"->1,"fields"->OptionValue["fields"],"Rrule"->{R[__,Blue]->1.},"graph"->OptionValue[ "graph"]];
exp=exp/.Subscript[t, a_]->Subscript[t, a-1]/.\[Gamma]->loc\[Gamma];

(*Purifying step*)

exp=Replace[exp,a_/;(!NumericQ[a]):>a/(Znorm[a,"graph"->OptionValue["graph"],"fields"->OptionValue["fields"]]),1];

exp=ExpectationValueBlock[Z0 exp,"endTime"->1,"fields"->OptionValue["fields"],"Rrule"->{R[__,Blue]->1.},"graph"->OptionValue[ "graph"]];
exp=exp/.Subscript[t, a_]->Subscript[t, a-1]/.\[Gamma]->loc\[Gamma];

Print["########## i=",i,", ",(Coefficient[exp, \[Phi]s[1,Subscript[t, 1],Subscript[k, 1]]]/.R[__]->0)," ############"];
];

Return[exp];
]


(* ::Subsubsection::Closed:: *)
(*Test ConvergenceKay\[Gamma][]*)


(* ::Input:: *)
(*g=myFavoriteGraph;*)
(*precision=10^-9;*)
(*expKay\[Gamma]=ConvergenceKay\[Gamma][\[Phi]s[1,Subscript[t, 1],Subscript[k, 1]],"graph"->g,"steps"->120,"precision"->precision,"gammaValue"->0.3]*)


(* ::Input:: *)
(*expKay\[Gamma]To4=expKay\[Gamma]/.a_/;Abs[a]<2*precision:>0/.SuperStar[\[Phi]][__]->1*)


(* ::Text:: *)
(*GRAND TOTAL CHECK*)


(* ::Input:: *)
(*expKay\[Gamma]To4-(30./77 Subscript[R, 1,5,RGBColor[1, 0, 0]] Subscript[R, 5,4,RGBColor[1, 0, 0]]);*)
(*%-(20./231 Subscript[R, 1,2,RGBColor[1, 0, 0]] Subscript[R, 2,5,RGBColor[1, 0, 0]] Subscript[R, 5,4,RGBColor[1, 0, 0]]+32./231 Subscript[R, 1,2,RGBColor[1, 0, 0]] Subscript[R, 1,5,RGBColor[1, 0, 0]] Subscript[R, 5,4,RGBColor[1, 0, 0]]+12./231 Subscript[R, 1,5,RGBColor[1, 0, 0]] Subscript[R, 5,2,RGBColor[1, 0, 0]] Subscript[R, 5,4,RGBColor[1, 0, 0]]);*)
(*%-(19./462 Subscript[R, 1,2,RGBColor[1, 0, 0]] Subscript[R, 2,3,RGBColor[1, 0, 0]] Subscript[R, 2,5,RGBColor[1, 0, 0]] Subscript[R, 5,4,RGBColor[1, 0, 0]]+19./462 Subscript[R, 1,2,RGBColor[1, 0, 0]] Subscript[R, 2,3,RGBColor[1, 0, 0]] Subscript[R, 2,5,RGBColor[1, 0, 0]] Subscript[R, 3,4,RGBColor[1, 0, 0]]+25./462 Subscript[R, 1,2,RGBColor[1, 0, 0]] Subscript[R, 1,5,RGBColor[1, 0, 0]] Subscript[R, 2,3,RGBColor[1, 0, 0]] Subscript[R, 5,4,RGBColor[1, 0, 0]]+25./462 Subscript[R, 1,2,RGBColor[1, 0, 0]] Subscript[R, 1,5,RGBColor[1, 0, 0]] Subscript[R, 2,3,RGBColor[1, 0, 0]] Subscript[R, 3,4,RGBColor[1, 0, 0]]+1./77 Subscript[R, 1,5,RGBColor[1, 0, 0]] Subscript[R, 2,3,RGBColor[1, 0, 0]] Subscript[R, 5,2,RGBColor[1, 0, 0]] Subscript[R, 5,4,RGBColor[1, 0, 0]]+1./77 Subscript[R, 1,5,RGBColor[1, 0, 0]] Subscript[R, 2,3,RGBColor[1, 0, 0]] Subscript[R, 3,4,RGBColor[1, 0, 0]] Subscript[R, 5,2,RGBColor[1, 0, 0]]);*)
(*%-(9./77 Subscript[R, 1,2,RGBColor[1, 0, 0]] Subscript[R, 2,3,RGBColor[1, 0, 0]] Subscript[R, 3,4,RGBColor[1, 0, 0]])*)
(*%/.a_/;Abs[a]<precision:>0*)
(*If[(%/.R[__]->1)==0, Print["!!!!!IT WORKS!!!!!!"], Print["!!!!!NOOOOOO!!!!!!"]]*)


(* ::Section::Closed:: *)
(*DynamicDLAactionKay\[Gamma]2[] without (SuperStar[\[Phi]]-1) SuperStar[\[Phi]], less \[Psi] (i.e. \[Pi])*)


(* ::Text:: *)
(*In this version I copy a double field and then annihilate it in the next step. So, I am effectively copying a single field. However, I do this so that I can simplify the \[Gamma] vertex. The goal is to have \[Gamma](\[Phi][y,t+1]-\[Phi][x,t+1])\[Chi] \[Phi] with possibly the \[Chi] that can enter the brackets. To do this I need a "purification" layer in which the fields are reset to order 1. I am also using mediators \[Psi]: at each time step the \[Phi]s become produce a \[Psi] and branch*)


(* ::Input:: *)
(*(*IT WORKS!!!!!!!*)*)


(* ::Input:: *)
(*Clear[DynamicDLAactionKay\[Gamma]2];*)
(*Options[DynamicDLAactionKay\[Gamma]2]={"sourceBC"->1,"excludedVertices"->{},"graph"->g,"endTime"->10};*)
(**)
(*DynamicDLAactionKay\[Gamma]2[OptionsPattern[]]:=Module[{locGraph,locEndTime,locSource,locRoot,locWeights,totWeights,locVertices,actionTimeSlice,action},*)
(*Clear[i,k,j,l,\[Gamma]];*)
(**)
(*locGraph=OptionValue["graph"];*)
(*locEndTime =OptionValue["endTime"];*)
(**)
(*locWeights=WeightedAdjacencyMatrix[locGraph]//Normal;*)
(*totWeights =Total[locWeights,{2}];*)
(**)
(*locVertices=VertexList@locGraph;*)
(**)
(*locRoot = *)
(* Select[(List@@@PropertyValue[locGraph,VertexLabels])[[2;;]],StringMatchQ[#[[2]],___~~"ROOT"]&][[All,1]][[1]];*)
(*locSource = *)
(* Select[(List@@@PropertyValue[locGraph,VertexLabels])[[2;;]],StringMatchQ[#[[2]],___~~"SOURCE"]&][[All,1]][[1]];*)
(**)
(*actionTimeSlice [T_]:= Product[*)
(*If[totWeights[[x]]===0  || MemberQ[OptionValue["excludedVertices"],x],1,*)
(*(*FALSE*)V[(1+\[Gamma] Sum[locWeights[[x,y]](R[x,y,Red]\[Phi]s[y,Subscript[t, T+1],Subscript[k, y]]-\[Phi]s[x,Subscript[t, T+1],Subscript[k, x]])\[Chi]s[y,Subscript[t, T],1],{y,Length[locVertices]}]\[Psi][x,Subscript[t, T],Subscript[l, x]])]*)
(*V[(1+Sum[locWeights[[x,y]]/totWeights[[x]] R[x,y,Blue]\[Chi]s[y,Subscript[t, T],Subscript[i, x]]\[Chi][x,Subscript[t, T],Subscript[i, x]],{y,Length[locVertices]}]*)
(*+(*COPY and ANNIHILATE*)\[Phi]s[x,Subscript[t, T+1],Subscript[k, x]]*(\[Phi][x,Subscript[t, T],Subscript[k, x]]\[Phi][x,Subscript[t, T],Subscript[k, x]])/2*)
(*+(*COPY and BRANCH*)\[Phi]s[x,Subscript[t, T+1],Subscript[k, x]]\[Psi]s[x,Subscript[t, T],Subscript[l, x]]*\[Phi][x,Subscript[t, T],Subscript[k, x]]*)
(*+(*JUST COPY \[Phi]*)\[Phi]s[x,Subscript[t, T+1],Subscript[k, x]]*\[Phi][x,Subscript[t, T],Subscript[k, x]]) ]*)
(*],{x,Length[locVertices]}]*)
(*V[(1+ \[Phi]s[locSource,Subscript[t, T+1],Subscript[k, locSource]]\[Phi][locSource,Subscript[t, T],Subscript[k, locSource]]+\[Chi][locSource,Subscript[t, T],1])]*)
(*(*V[(1+ OptionValue["sourceBC"] \[Chi][locSource,Subscript[t, T],1])]*);*)
(**)
(*action=Product[actionTimeSlice[T],{T,1,locEndTime}](*Product[V[(1+\[Phi][x,Subscript[t, locEndTime+1],Subscript[k, x]])(*This allows the path tracker to be absorbed at the end*) ]V[(1+\[Phi][x,Subscript[t, locEndTime+1],0])(*This allows the path to end after "endTime" steps*)],{x,Length[locVertices]}]*);*)
(**)
(*Return[action]*)
(**)
(*]*)


(* ::Subsection:: *)
(*Test*)


(* ::Item::Closed:: *)
(*My favorite graph*)


(* ::Input:: *)
(*myFavoriteEdges={{1,2},{1,5},{2,3},{2,5},{5,4},{3,4}};*)
(*myFavoriteRoot=1;*)
(*myFavoriteSource=4;*)
(*g=myFavoriteGraph=MyGraph[myFavoriteEdges,myFavoriteRoot,myFavoriteSource][[1]]*)


(* ::Input:: *)
(*fields=FieldsNamesGenerator[{\[Phi],\[Psi],\[Chi]}];*)
(*endTime=1;*)
(*Z=DynamicDLAactionKay\[Gamma]2["graph"->g,"endTime"->endTime]/.\[Beta][__]->1;*)
(*Z0=Z/.\[Gamma]->0;*)


(* ::Subitem::Closed:: *)
(*Normalized with purification*)


(* ::Input:: *)
(*\[Phi]s[1,Subscript[t, 1],Subscript[k, 1]]/Znorm[\[Phi]s[1,Subscript[t, 1],Subscript[k, 1]],"graph"->g,"fields"->fields,"\[Beta]rule"->{\[Beta][__]->1}];*)
(**)
(*ExpectationValueBlock[Z %,"endTime"->endTime,"fields"->fields,"draw"->False,"Rrule"->{R[__,Blue]->1},"graph"->g]/.\[Gamma]->"\!\(\*SubscriptBox[\(\[Gamma]\), \(1\)]\)"/.Subscript[t, a_]->Subscript[t, a-1]//Expand*)
(**)
(*Replace[%,a_/;(!NumericQ[a]):>a/(Znorm[a,"graph"->g,"fields"->fields,"\[Beta]rule"->{\[Beta][__]->1}]),1];*)
(*expDLANormKay\[Gamma]bis=ExpectationValueBlock[Z0 %,"endTime"->endTime,"fields"->fields,"draw"->False,"Rrule"->{R[__,Blue]->1},"graph"->g]/.\[Gamma]->"\!\(\*SubscriptBox[\(\[Gamma]\), \(1\)]\)"/.Subscript[t, a_]->Subscript[t, a-1]//Expand*)


(* ::Input:: *)
(*Replace[expDLANormKay\[Gamma]bis,a_/;(!NumericQ[a]):>a/(Znorm[a,"graph"->g,"fields"->fields,"\[Beta]rule"->{\[Beta][__]->1}]),1];*)
(*ExpectationValueBlock[Z %,"endTime"->endTime,"fields"->fields,"draw"->False,"Rrule"->{R[__,Blue]->1},"graph"->g]/.\[Gamma]->"\!\(\*SubscriptBox[\(\[Gamma]\), \(2\)]\)"/.Subscript[t, a_]->Subscript[t, a-1]//Expand*)
(**)
(*ExpectationValueBlock[Z0 %,"endTime"->endTime,"fields"->fields,"draw"->False,"Rrule"->{R[__,Blue]->1},"graph"->g]/.\[Gamma]->"\!\(\*SubscriptBox[\(\[Gamma]\), \(2\)]\)"/.Subscript[t, a_]->Subscript[t, a-1]//Expand;*)
(*expDLANormKay\[Gamma]2bis=Replace[%,a_/;(!NumericQ[a]):>a/(Znorm[a,"graph"->g,"fields"->fields,"\[Beta]rule"->{\[Beta][__]->1}]),1]*)


(* ::Input:: *)
(*expDLANormKay\[Gamma]2bis;*)
(*%-expDLAcorrectNorm2*)


(* ::Input:: *)
(*(*YESSS*)*)


(* ::Input:: *)
(*Replace[expDLANormKay\[Gamma]2bis,a_/;(!NumericQ[a]):>a/(Znorm[a,"graph"->g,"fields"->fields,"\[Beta]rule"->{\[Beta][__]->1}]),1];*)
(*ExpectationValueBlock[Z %,"endTime"->endTime,"fields"->fields,"draw"->False,"Rrule"->{R[__,Blue]->1},"graph"->g]/.\[Gamma]->"\!\(\*SubscriptBox[\(\[Gamma]\), \(3\)]\)"/.\[Gamma]->"\!\(\*SubscriptBox[\(\[Gamma]\), \(3\)]\)"/.Subscript[t, a_]->Subscript[t, a-1]//Expand;*)
(**)
(*ExpectationValueBlock[Z0 %,"endTime"->endTime,"fields"->fields,"draw"->False,"Rrule"->{R[__,Blue]->1},"graph"->g]/.\[Gamma]->"\!\(\*SubscriptBox[\(\[Gamma]\), \(3\)]\)"/.Subscript[t, a_]->Subscript[t, a-1]//Expand;*)
(*expDLANormKay\[Gamma]3bis=Replace[%,a_/;(!NumericQ[a]):>a/(Znorm[a,"graph"->g,"fields"->fields,"\[Beta]rule"->{\[Beta][__]->1}]),1]*)


(* ::Input:: *)
(*expDLANormKay\[Gamma]3-expDLAcorrectNorm3*)


(* ::Input:: *)
(*(*IT WORKS!!!!*)*)


(* ::Subsection::Closed:: *)
(*ConvergenceKay\[Gamma]2[]*)


(* ::Input::Initialization:: *)
Clear[ConvergenceKay\[Gamma]2];
Options[ConvergenceKay\[Gamma]2]={"graph"->g,"fields"->FieldsNamesGenerator[{\[Phi],\[Psi],\[Chi]}],"steps"->10,"precision"->10^-9,"theory"->"DLA","gammaValue"->0.1};

ConvergenceKay\[Gamma]2[operator_,OptionsPattern[]]:=Module[{Z,Z0,locOperator,locPrecision,loc\[Gamma],exp,i=1,temp},

(*If[OptionValue["theory"]=="DLA",
Z=DynamicDLAactionKay\[Gamma]["graph"->OptionValue[ "graph"],"endTime"->1]/.\[Beta][__]->1.
];*)


Z=DynamicDLAactionKay\[Gamma]2["graph"->OptionValue[ "graph"],"endTime"->1]/.\[Beta][__]->1.;
Z0=Z/.\[Gamma]->0;

locOperator= operator;
locPrecision=OptionValue["precision"];
loc\[Gamma]=OptionValue["gammaValue"];

exp=locOperator/(Znorm[locOperator,"graph"->OptionValue["graph"],"fields"->OptionValue["fields"]]);

exp=ExpectationValueBlock[Z exp,"endTime"->1,"fields"->OptionValue["fields"],"Rrule"->{R[__,Blue]->1.},"graph"->OptionValue[ "graph"]];
exp=exp/.Subscript[t, a_]->Subscript[t, a-1]/.\[Gamma]->loc\[Gamma];

(*Purifying step*)

exp=ExpectationValueBlock[Z0 exp,"endTime"->1,"fields"->OptionValue["fields"],"Rrule"->{R[__,Blue]->1.},"graph"->OptionValue[ "graph"]];
exp=exp/.Subscript[t, a_]->Subscript[t, a-1]/.\[Gamma]->loc\[Gamma];

exp=Replace[exp,a_/;(!NumericQ[a]):>a/(Znorm[a,"graph"->OptionValue["graph"],"fields"->OptionValue["fields"]]),1];
Print["########## i=",i,", ",(Coefficient[exp, \[Phi]s[1,Subscript[t, 1],Subscript[k, 1]]]/.R[__]->0)," ############"];

(*Now iterates untill it converges*)
For[i=2,i<=OptionValue["steps"] && (Coefficient[exp, \[Phi]s[1,Subscript[t, 1],Subscript[k, 1]]]/.R[__]->0)>=locPrecision,i++,


exp=Replace[exp,a_/;(!NumericQ[a]):>a/(Znorm[a,"graph"->OptionValue["graph"],"fields"->OptionValue["fields"]]),1];

exp=ExpectationValueBlock[Z exp,"endTime"->1,"fields"->OptionValue["fields"],"Rrule"->{R[__,Blue]->1.},"graph"->OptionValue[ "graph"]];
exp=exp/.Subscript[t, a_]->Subscript[t, a-1]/.\[Gamma]->loc\[Gamma];

(*Purifying step*)

exp=ExpectationValueBlock[Z0 exp,"endTime"->1,"fields"->OptionValue["fields"],"Rrule"->{R[__,Blue]->1.},"graph"->OptionValue[ "graph"]];
exp=exp/.Subscript[t, a_]->Subscript[t, a-1]/.\[Gamma]->loc\[Gamma];

exp=Replace[exp,a_/;(!NumericQ[a]):>a/(Znorm[a,"graph"->OptionValue["graph"],"fields"->OptionValue["fields"]]),1];


Print["########## i=",i,", ",(Coefficient[exp, \[Phi]s[1,Subscript[t, 1],Subscript[k, 1]]]/.R[__]->0)," ############"];
];

Return[exp];
]


(* ::Subsubsection::Closed:: *)
(*Test ConvergenceKay\[Gamma]2[]*)


(* ::Input:: *)
(*g=myFavoriteGraph;*)
(*precision=10^-7;*)
(*expKay\[Gamma]2=ConvergenceKay\[Gamma][\[Phi]s[1,Subscript[t, 1],Subscript[k, 1]],"graph"->g,"steps"->100,"precision"->precision,"gammaValue"->0.3]*)


(* ::Input:: *)
(*expKay\[Gamma]2To4=expKay\[Gamma]/.a_/;Abs[a]<precision:>0/.SuperStar[\[Phi]][__]->1*)


(* ::Text:: *)
(*GRAND TOTAL CHECK*)


(* ::Input:: *)
(*expKay\[Gamma]2To4-(30./77 Subscript[R, 1,5,RGBColor[1, 0, 0]] Subscript[R, 5,4,RGBColor[1, 0, 0]]);*)
(*%-(20./231 Subscript[R, 1,2,RGBColor[1, 0, 0]] Subscript[R, 2,5,RGBColor[1, 0, 0]] Subscript[R, 5,4,RGBColor[1, 0, 0]]+32./231 Subscript[R, 1,2,RGBColor[1, 0, 0]] Subscript[R, 1,5,RGBColor[1, 0, 0]] Subscript[R, 5,4,RGBColor[1, 0, 0]]+12./231 Subscript[R, 1,5,RGBColor[1, 0, 0]] Subscript[R, 5,2,RGBColor[1, 0, 0]] Subscript[R, 5,4,RGBColor[1, 0, 0]]);*)
(*%-(19./462 Subscript[R, 1,2,RGBColor[1, 0, 0]] Subscript[R, 2,3,RGBColor[1, 0, 0]] Subscript[R, 2,5,RGBColor[1, 0, 0]] Subscript[R, 5,4,RGBColor[1, 0, 0]]+19./462 Subscript[R, 1,2,RGBColor[1, 0, 0]] Subscript[R, 2,3,RGBColor[1, 0, 0]] Subscript[R, 2,5,RGBColor[1, 0, 0]] Subscript[R, 3,4,RGBColor[1, 0, 0]]+25./462 Subscript[R, 1,2,RGBColor[1, 0, 0]] Subscript[R, 1,5,RGBColor[1, 0, 0]] Subscript[R, 2,3,RGBColor[1, 0, 0]] Subscript[R, 5,4,RGBColor[1, 0, 0]]+25./462 Subscript[R, 1,2,RGBColor[1, 0, 0]] Subscript[R, 1,5,RGBColor[1, 0, 0]] Subscript[R, 2,3,RGBColor[1, 0, 0]] Subscript[R, 3,4,RGBColor[1, 0, 0]]+1./77 Subscript[R, 1,5,RGBColor[1, 0, 0]] Subscript[R, 2,3,RGBColor[1, 0, 0]] Subscript[R, 5,2,RGBColor[1, 0, 0]] Subscript[R, 5,4,RGBColor[1, 0, 0]]+1./77 Subscript[R, 1,5,RGBColor[1, 0, 0]] Subscript[R, 2,3,RGBColor[1, 0, 0]] Subscript[R, 3,4,RGBColor[1, 0, 0]] Subscript[R, 5,2,RGBColor[1, 0, 0]]);*)
(*%-(9./77 Subscript[R, 1,2,RGBColor[1, 0, 0]] Subscript[R, 2,3,RGBColor[1, 0, 0]] Subscript[R, 3,4,RGBColor[1, 0, 0]])*)
(*%/.a_/;Abs[a]<precision:>0*)
(*If[(%/.R[__]->1)==0, Print["!!!!!IT WORKS!!!!!!"], Print["!!!!!NOOOOOO!!!!!!"]]*)


(* ::Section::Closed:: *)
(*DynamicDLAactionKay\[Gamma]3[] without (SuperStar[\[Phi]]-1) SuperStar[\[Phi]], less \[Psi] (i.e. \[Pi]), moved \[Gamma]*)


(* ::Text:: *)
(*Same as previous one but with the \[Gamma] moved to the branching, i.e. creation of \[Psi]*)


(* ::Input:: *)
(*(*IT WORKS of course!!!!!!!!!!!!!!!!*)*)


(* ::Input:: *)
(*Clear[DynamicDLAactionKay\[Gamma]3];*)
(*Options[DynamicDLAactionKay\[Gamma]3]={"sourceBC"->1,"excludedVertices"->{},"graph"->g,"endTime"->10};*)
(**)
(*DynamicDLAactionKay\[Gamma]3[OptionsPattern[]]:=Module[{locGraph,locEndTime,locSource,locRoot,locWeights,totWeights,locVertices,actionTimeSlice,action},*)
(*Clear[i,k,j,l,\[Gamma]];*)
(**)
(*locGraph=OptionValue["graph"];*)
(*locEndTime =OptionValue["endTime"];*)
(**)
(*locWeights=WeightedAdjacencyMatrix[locGraph]//Normal;*)
(*totWeights =Total[locWeights,{2}];*)
(**)
(*locVertices=VertexList@locGraph;*)
(**)
(*locRoot = *)
(* Select[(List@@@PropertyValue[locGraph,VertexLabels])[[2;;]],StringMatchQ[#[[2]],___~~"ROOT"]&][[All,1]][[1]];*)
(*locSource = *)
(* Select[(List@@@PropertyValue[locGraph,VertexLabels])[[2;;]],StringMatchQ[#[[2]],___~~"SOURCE"]&][[All,1]][[1]];*)
(**)
(*actionTimeSlice [T_]:= Product[*)
(*If[totWeights[[x]]===0  || MemberQ[OptionValue["excludedVertices"],x],1,*)
(*(*FALSE*)V[(1+ Sum[locWeights[[x,y]](R[x,y,Red]\[Phi]s[y,Subscript[t, T+1],Subscript[k, y]]-\[Phi]s[x,Subscript[t, T+1],Subscript[k, x]])\[Chi]s[y,Subscript[t, T],1],{y,Length[locVertices]}]\[Psi][x,Subscript[t, T],Subscript[l, x]])]*)
(*V[(1+Sum[locWeights[[x,y]]/totWeights[[x]] R[x,y,Blue]\[Chi]s[y,Subscript[t, T],Subscript[i, x]]\[Chi][x,Subscript[t, T],Subscript[i, x]],{y,Length[locVertices]}]*)
(*+(*COPY and ANNIHILATE*)\[Phi]s[x,Subscript[t, T+1],Subscript[k, x]]*(\[Phi][x,Subscript[t, T],Subscript[k, x]]\[Phi][x,Subscript[t, T],Subscript[k, x]])/2*)
(*+(*COPY and BRANCH*)\[Gamma] \[Phi]s[x,Subscript[t, T+1],Subscript[k, x]]\[Psi]s[x,Subscript[t, T],Subscript[l, x]]*\[Phi][x,Subscript[t, T],Subscript[k, x]]*)
(*+(*JUST COPY \[Phi]*)\[Phi]s[x,Subscript[t, T+1],Subscript[k, x]]*\[Phi][x,Subscript[t, T],Subscript[k, x]]) ]*)
(*],{x,Length[locVertices]}]*)
(*V[(1+ \[Phi]s[locSource,Subscript[t, T+1],Subscript[k, locSource]]\[Phi][locSource,Subscript[t, T],Subscript[k, locSource]]+\[Chi][locSource,Subscript[t, T],1])]*)
(*(*V[(1+ OptionValue["sourceBC"] \[Chi][locSource,Subscript[t, T],1])]*);*)
(**)
(*action=Product[actionTimeSlice[T],{T,1,locEndTime}](*Product[V[(1+\[Phi][x,Subscript[t, locEndTime+1],Subscript[k, x]])(*This allows the path tracker to be absorbed at the end*) ]V[(1+\[Phi][x,Subscript[t, locEndTime+1],0])(*This allows the path to end after "endTime" steps*)],{x,Length[locVertices]}]*);*)
(**)
(*Return[action]*)
(**)
(*]*)


(* ::Subsection::Closed:: *)
(*Test*)


(* ::Item:: *)
(*My favorite graph*)


(* ::Input:: *)
(*myFavoriteEdges={{1,2},{1,5},{2,3},{2,5},{5,4},{3,4}};*)
(*myFavoriteRoot=1;*)
(*myFavoriteSource=4;*)
(*g=myFavoriteGraph=MyGraph[myFavoriteEdges,myFavoriteRoot,myFavoriteSource][[1]]*)


(* ::Input:: *)
(*fields=FieldsNamesGenerator[{\[Phi],\[Psi],\[Chi]}];*)
(*endTime=1;*)
(*Z=DynamicDLAactionKay\[Gamma]3["graph"->g,"endTime"->endTime]/.\[Beta][__]->1;*)
(*Z0=Z/.\[Gamma]->0;*)


(* ::Subitem:: *)
(*Normalized with purification*)


(* ::Input:: *)
(*\[Phi]s[1,Subscript[t, 1],Subscript[k, 1]]/Znorm[\[Phi]s[1,Subscript[t, 1],Subscript[k, 1]],"graph"->g,"fields"->fields,"\[Beta]rule"->{\[Beta][__]->1}];*)
(**)
(*ExpectationValueBlock[Z %,"endTime"->endTime,"fields"->fields,"draw"->False,"Rrule"->{R[__,Blue]->1},"graph"->g]/.\[Gamma]->"\!\(\*SubscriptBox[\(\[Gamma]\), \(1\)]\)"/.Subscript[t, a_]->Subscript[t, a-1]//Expand*)
(**)
(*Replace[%,a_/;(!NumericQ[a]):>a/(Znorm[a,"graph"->g,"fields"->fields,"\[Beta]rule"->{\[Beta][__]->1}]),1];*)
(*expDLANormKay\[Gamma]bis=ExpectationValueBlock[Z0 %,"endTime"->endTime,"fields"->fields,"draw"->False,"Rrule"->{R[__,Blue]->1},"graph"->g]/.\[Gamma]->"\!\(\*SubscriptBox[\(\[Gamma]\), \(1\)]\)"/.Subscript[t, a_]->Subscript[t, a-1]//Expand*)


(* ::Input:: *)
(*Replace[expDLANormKay\[Gamma]bis,a_/;(!NumericQ[a]):>a/(Znorm[a,"graph"->g,"fields"->fields,"\[Beta]rule"->{\[Beta][__]->1}]),1];*)
(*ExpectationValueBlock[Z %,"endTime"->endTime,"fields"->fields,"draw"->False,"Rrule"->{R[__,Blue]->1},"graph"->g]/.\[Gamma]->"\!\(\*SubscriptBox[\(\[Gamma]\), \(2\)]\)"/.Subscript[t, a_]->Subscript[t, a-1]//Expand*)
(**)
(*ExpectationValueBlock[Z0 %,"endTime"->endTime,"fields"->fields,"draw"->False,"Rrule"->{R[__,Blue]->1},"graph"->g]/.\[Gamma]->"\!\(\*SubscriptBox[\(\[Gamma]\), \(2\)]\)"/.Subscript[t, a_]->Subscript[t, a-1]//Expand;*)
(*expDLANormKay\[Gamma]2bis=Replace[%,a_/;(!NumericQ[a]):>a/(Znorm[a,"graph"->g,"fields"->fields,"\[Beta]rule"->{\[Beta][__]->1}]),1]*)


(* ::Input:: *)
(*expDLANormKay\[Gamma]2bis;*)
(*%-expDLAcorrectNorm2*)


(* ::Input:: *)
(*(*YESSS*)*)


(* ::Input:: *)
(*Replace[expDLANormKay\[Gamma]2bis,a_/;(!NumericQ[a]):>a/(Znorm[a,"graph"->g,"fields"->fields,"\[Beta]rule"->{\[Beta][__]->1}]),1];*)
(*ExpectationValueBlock[Z %,"endTime"->endTime,"fields"->fields,"draw"->False,"Rrule"->{R[__,Blue]->1},"graph"->g]/.\[Gamma]->"\!\(\*SubscriptBox[\(\[Gamma]\), \(3\)]\)"/.\[Gamma]->"\!\(\*SubscriptBox[\(\[Gamma]\), \(3\)]\)"/.Subscript[t, a_]->Subscript[t, a-1]//Expand;*)
(**)
(*ExpectationValueBlock[Z0 %,"endTime"->endTime,"fields"->fields,"draw"->False,"Rrule"->{R[__,Blue]->1},"graph"->g]/.\[Gamma]->"\!\(\*SubscriptBox[\(\[Gamma]\), \(3\)]\)"/.Subscript[t, a_]->Subscript[t, a-1]//Expand;*)
(*expDLANormKay\[Gamma]3bis=Replace[%,a_/;(!NumericQ[a]):>a/(Znorm[a,"graph"->g,"fields"->fields,"\[Beta]rule"->{\[Beta][__]->1}]),1]*)


(* ::Input:: *)
(*expDLANormKay\[Gamma]3bis-expDLAcorrectNorm3*)


(* ::Input:: *)
(*(*IT WORKS!!!!*)*)


(* ::Subsection::Closed:: *)
(*ConvergenceKay\[Gamma]2[]*)


(* ::Input::Initialization:: *)
Clear[ConvergenceKay\[Gamma]2];
Options[ConvergenceKay\[Gamma]2]={"graph"->g,"fields"->FieldsNamesGenerator[{\[Phi],\[Psi],\[Chi]}],"steps"->10,"precision"->10^-9,"theory"->"DLA","gammaValue"->0.1};

ConvergenceKay\[Gamma]2[operator_,OptionsPattern[]]:=Module[{Z,Z0,locOperator,locPrecision,loc\[Gamma],exp,i=1,temp},

(*If[OptionValue["theory"]=="DLA",
Z=DynamicDLAactionKay\[Gamma]["graph"->OptionValue[ "graph"],"endTime"->1]/.\[Beta][__]->1.
];*)


Z=DynamicDLAactionKay\[Gamma]2["graph"->OptionValue[ "graph"],"endTime"->1]/.\[Beta][__]->1.;
Z0=Z/.\[Gamma]->0;

locOperator= operator;
locPrecision=OptionValue["precision"];
loc\[Gamma]=OptionValue["gammaValue"];

exp=locOperator/(Znorm[locOperator,"graph"->OptionValue["graph"],"fields"->OptionValue["fields"]]);

exp=ExpectationValueBlock[Z exp,"endTime"->1,"fields"->OptionValue["fields"],"Rrule"->{R[__,Blue]->1.},"graph"->OptionValue[ "graph"]];
exp=exp/.Subscript[t, a_]->Subscript[t, a-1]/.\[Gamma]->loc\[Gamma];

(*Purifying step*)

exp=ExpectationValueBlock[Z0 exp,"endTime"->1,"fields"->OptionValue["fields"],"Rrule"->{R[__,Blue]->1.},"graph"->OptionValue[ "graph"]];
exp=exp/.Subscript[t, a_]->Subscript[t, a-1]/.\[Gamma]->loc\[Gamma];

exp=Replace[exp,a_/;(!NumericQ[a]):>a/(Znorm[a,"graph"->OptionValue["graph"],"fields"->OptionValue["fields"]]),1];
Print["########## i=",i,", ",(Coefficient[exp, \[Phi]s[1,Subscript[t, 1],Subscript[k, 1]]]/.R[__]->0)," ############"];

(*Now iterates untill it converges*)
For[i=2,i<=OptionValue["steps"] && (Coefficient[exp, \[Phi]s[1,Subscript[t, 1],Subscript[k, 1]]]/.R[__]->0)>=locPrecision,i++,


exp=Replace[exp,a_/;(!NumericQ[a]):>a/(Znorm[a,"graph"->OptionValue["graph"],"fields"->OptionValue["fields"]]),1];

exp=ExpectationValueBlock[Z exp,"endTime"->1,"fields"->OptionValue["fields"],"Rrule"->{R[__,Blue]->1.},"graph"->OptionValue[ "graph"]];
exp=exp/.Subscript[t, a_]->Subscript[t, a-1]/.\[Gamma]->loc\[Gamma];

(*Purifying step*)

exp=ExpectationValueBlock[Z0 exp,"endTime"->1,"fields"->OptionValue["fields"],"Rrule"->{R[__,Blue]->1.},"graph"->OptionValue[ "graph"]];
exp=exp/.Subscript[t, a_]->Subscript[t, a-1]/.\[Gamma]->loc\[Gamma];

exp=Replace[exp,a_/;(!NumericQ[a]):>a/(Znorm[a,"graph"->OptionValue["graph"],"fields"->OptionValue["fields"]]),1];


Print["########## i=",i,", ",(Coefficient[exp, \[Phi]s[1,Subscript[t, 1],Subscript[k, 1]]]/.R[__]->0)," ############"];
];

Return[exp];
]


(* ::Subsubsection::Closed:: *)
(*Test ConvergenceKay\[Gamma]2[]*)


(* ::Input:: *)
(*g=myFavoriteGraph;*)
(*precision=10^-7;*)
(*expKay\[Gamma]2=ConvergenceKay\[Gamma][\[Phi]s[1,Subscript[t, 1],Subscript[k, 1]],"graph"->g,"steps"->100,"precision"->precision,"gammaValue"->0.3]*)


(* ::Input:: *)
(*expKay\[Gamma]2To4=expKay\[Gamma]/.a_/;Abs[a]<precision:>0/.SuperStar[\[Phi]][__]->1*)


(* ::Text:: *)
(*GRAND TOTAL CHECK*)


(* ::Input:: *)
(*expKay\[Gamma]2To4-(30./77 Subscript[R, 1,5,RGBColor[1, 0, 0]] Subscript[R, 5,4,RGBColor[1, 0, 0]]);*)
(*%-(20./231 Subscript[R, 1,2,RGBColor[1, 0, 0]] Subscript[R, 2,5,RGBColor[1, 0, 0]] Subscript[R, 5,4,RGBColor[1, 0, 0]]+32./231 Subscript[R, 1,2,RGBColor[1, 0, 0]] Subscript[R, 1,5,RGBColor[1, 0, 0]] Subscript[R, 5,4,RGBColor[1, 0, 0]]+12./231 Subscript[R, 1,5,RGBColor[1, 0, 0]] Subscript[R, 5,2,RGBColor[1, 0, 0]] Subscript[R, 5,4,RGBColor[1, 0, 0]]);*)
(*%-(19./462 Subscript[R, 1,2,RGBColor[1, 0, 0]] Subscript[R, 2,3,RGBColor[1, 0, 0]] Subscript[R, 2,5,RGBColor[1, 0, 0]] Subscript[R, 5,4,RGBColor[1, 0, 0]]+19./462 Subscript[R, 1,2,RGBColor[1, 0, 0]] Subscript[R, 2,3,RGBColor[1, 0, 0]] Subscript[R, 2,5,RGBColor[1, 0, 0]] Subscript[R, 3,4,RGBColor[1, 0, 0]]+25./462 Subscript[R, 1,2,RGBColor[1, 0, 0]] Subscript[R, 1,5,RGBColor[1, 0, 0]] Subscript[R, 2,3,RGBColor[1, 0, 0]] Subscript[R, 5,4,RGBColor[1, 0, 0]]+25./462 Subscript[R, 1,2,RGBColor[1, 0, 0]] Subscript[R, 1,5,RGBColor[1, 0, 0]] Subscript[R, 2,3,RGBColor[1, 0, 0]] Subscript[R, 3,4,RGBColor[1, 0, 0]]+1./77 Subscript[R, 1,5,RGBColor[1, 0, 0]] Subscript[R, 2,3,RGBColor[1, 0, 0]] Subscript[R, 5,2,RGBColor[1, 0, 0]] Subscript[R, 5,4,RGBColor[1, 0, 0]]+1./77 Subscript[R, 1,5,RGBColor[1, 0, 0]] Subscript[R, 2,3,RGBColor[1, 0, 0]] Subscript[R, 3,4,RGBColor[1, 0, 0]] Subscript[R, 5,2,RGBColor[1, 0, 0]]);*)
(*%-(9./77 Subscript[R, 1,2,RGBColor[1, 0, 0]] Subscript[R, 2,3,RGBColor[1, 0, 0]] Subscript[R, 3,4,RGBColor[1, 0, 0]])*)
(*%/.a_/;Abs[a]<precision:>0*)
(*If[(%/.R[__]->1)==0, Print["!!!!!IT WORKS!!!!!!"], Print["!!!!!NOOOOOO!!!!!!"]]*)


(* ::Section::Closed:: *)
(*DynamicDLAactionBEST[] without (SuperStar[\[Phi]]-1) SuperStar[\[Phi]], NO \[Psi]: BEST!!!*)


(* ::Text:: *)
(*In this version I copy a double field and then annihilate it in the next step. So, I am effectively copying a single field. However, I do this so that I can simplify the \[Gamma] vertex. The goal is to have \[Gamma] \[Del](SuperStar[\[Chi]] \[Del]\!\(\*SuperscriptBox[\(\[Phi]\), \(\(*\)\(2\)\)]\))\[Phi], i.e. \[Gamma](SuperStar[\[Phi]][y,t+1]-SuperStar[\[Phi]][x,t+1])SuperStar[\[Phi]][x,t]SuperStar[\[Chi]][y,t]\[Phi]. *)
(*To do this I need a "purification" layer in which the fields are reset to order 1. *)
(*WARNING: we are using a \[Psi] just for technical reasons.*)


(* ::Input:: *)
(*(*IT WORKS!!!!!!!*)*)


(* ::Input:: *)
(*Clear[DynamicDLAactionBEST];*)
(*Options[DynamicDLAactionBEST]={"sourceBC"->1,"excludedVertices"->{},"graph"->g,"endTime"->1};*)
(**)
(*DynamicDLAactionBEST[OptionsPattern[]]:=Module[{locGraph,locEndTime,locSource,locRoot,locWeights,totWeights,locVertices,actionTimeSlice,action},*)
(*Clear[i,k,j,l,\[Gamma]];*)
(**)
(*locGraph=OptionValue["graph"];*)
(*locEndTime =OptionValue["endTime"];*)
(**)
(*locWeights=WeightedAdjacencyMatrix[locGraph]//Normal;*)
(*totWeights =Total[locWeights,{2}];*)
(**)
(*locVertices=VertexList@locGraph;*)
(**)
(*locRoot = *)
(* Select[(List@@@PropertyValue[locGraph,VertexLabels])[[2;;]],StringMatchQ[#[[2]],___~~"ROOT"]&][[All,1]][[1]];*)
(*locSource = *)
(* Select[(List@@@PropertyValue[locGraph,VertexLabels])[[2;;]],StringMatchQ[#[[2]],___~~"SOURCE"]&][[All,1]][[1]];*)
(**)
(*actionTimeSlice [T_]:= Product[*)
(*If[totWeights[[x]]===0  || MemberQ[OptionValue["excludedVertices"],x],1,*)
(*(*FALSE*)V[(1+\[Gamma] Sum[locWeights[[x,y]](R[x,y,Red]\[Phi]s[y,Subscript[t, T+1],Subscript[k, y]]-\[Phi]s[x,Subscript[t, T+1],Subscript[k, x]])\[Psi]s[x,Subscript[t, T],Subscript[jj, x]]\[Chi]s[y,Subscript[t, T],1],{y,Length[locVertices]}]*\[Phi][x,Subscript[t, T],Subscript[k, x]])]*)
(*V[(1+Sum[locWeights[[x,y]]/totWeights[[x]] R[x,y,Blue]\[Chi]s[y,Subscript[t, T],Subscript[i, x]]\[Chi][x,Subscript[t, T],Subscript[i, x]],{y,Length[locVertices]}]*)
(*+(*COPY and ANNIHILATE*)\[Phi]s[x,Subscript[t, T+1],Subscript[k, x]]*(\[Phi][x,Subscript[t, T],Subscript[k, x]]\[Phi][x,Subscript[t, T],Subscript[k, x]])/2*)
(*+(*JUST COPY \[Phi]*)\[Phi]s[x,Subscript[t, T+1],Subscript[k, x]]*\[Phi][x,Subscript[t, T],Subscript[k, x]]*)
(*+(*JUST COPY \[Phi] from the interaction (just a technicality, not really needed)*)\[Phi]s[x,Subscript[t, T+1],Subscript[k, x]]*\[Psi][x,Subscript[t, T],Subscript[jj, x]]) ]*)
(*],{x,Length[locVertices]}]*)
(*V[(1+ \[Phi]s[locSource,Subscript[t, T+1],Subscript[k, locSource]]*\[Phi][locSource,Subscript[t, T],Subscript[k, locSource]]+\[Chi][locSource,Subscript[t, T],1])]*)
(*(*V[(1+ OptionValue["sourceBC"] \[Chi][locSource,Subscript[t, T],1])]*);*)
(**)
(*action=Product[actionTimeSlice[T],{T,1,locEndTime}](*Product[V[(1+\[Phi][x,Subscript[t, locEndTime+1],Subscript[k, x]])(*This allows the path tracker to be absorbed at the end*) ]V[(1+\[Phi][x,Subscript[t, locEndTime+1],0])(*This allows the path to end after "endTime" steps*)],{x,Length[locVertices]}]*);*)
(**)
(*Return[action]*)
(**)
(*]*)


(* ::Subsection::Closed:: *)
(*Test*)


(* ::Item:: *)
(*My favorite graph*)


(* ::Input:: *)
(*myFavoriteEdges={{1,2},{1,5},{2,3},{2,5},{5,4},{3,4}};*)
(*myFavoriteRoot=1;*)
(*myFavoriteSource=4;*)
(*g=myFavoriteGraph=MyGraph[myFavoriteEdges,myFavoriteRoot,myFavoriteSource][[1]]*)


(* ::Input:: *)
(*fields=FieldsNamesGenerator[{\[Phi],\[Psi],\[Chi]}];*)
(*endTime=1;*)
(*Z=DynamicDLAactionBEST["graph"->g,"endTime"->endTime]/.\[Beta][__]->1;*)
(*Z0=Z/.\[Gamma]->0;*)


(* ::Subitem::Closed:: *)
(*Normalized with purification*)


(* ::Input:: *)
(*\[Phi]s[1,Subscript[t, 1],Subscript[k, 1]]/Znorm[\[Phi]s[1,Subscript[t, 1],Subscript[k, 1]],"graph"->g,"fields"->fields,"\[Beta]rule"->{\[Beta][__]->1}];*)
(**)
(*ExpectationValueBlock[Z %,"endTime"->endTime,"fields"->fields,"draw"->False,"Rrule"->{R[__,Blue]->1},"graph"->g]/.\[Gamma]->"\!\(\*SubscriptBox[\(\[Gamma]\), \(1\)]\)"/.Subscript[t, a_]->Subscript[t, a-1]//Expand*)
(**)
(*Replace[%,a_/;(!NumericQ[a]):>a/(Znorm[a,"graph"->g,"fields"->fields,"\[Beta]rule"->{\[Beta][__]->1}]),1];*)
(*expDLANormBEST=ExpectationValueBlock[Z0 %,"endTime"->endTime,"fields"->fields,"draw"->False,"Rrule"->{R[__,Blue]->1},"graph"->g]/.\[Gamma]->"\!\(\*SubscriptBox[\(\[Gamma]\), \(1\)]\)"/.Subscript[t, a_]->Subscript[t, a-1]//Expand*)


(* ::Input:: *)
(*Replace[expDLANormBEST,a_/;(!NumericQ[a]):>a/(Znorm[a,"graph"->g,"fields"->fields,"\[Beta]rule"->{\[Beta][__]->1}]),1];*)
(*ExpectationValueBlock[Z %,"endTime"->endTime,"fields"->fields,"draw"->False,"Rrule"->{R[__,Blue]->1},"graph"->g]/.\[Gamma]->"\!\(\*SubscriptBox[\(\[Gamma]\), \(2\)]\)"/.Subscript[t, a_]->Subscript[t, a-1]//Expand*)
(**)
(*ExpectationValueBlock[Z0 %,"endTime"->endTime,"fields"->fields,"draw"->False,"Rrule"->{R[__,Blue]->1},"graph"->g]/.\[Gamma]->"\!\(\*SubscriptBox[\(\[Gamma]\), \(2\)]\)"/.Subscript[t, a_]->Subscript[t, a-1]//Expand;*)
(*expDLANormBEST2=Replace[%,a_/;(!NumericQ[a]):>a/(Znorm[a,"graph"->g,"fields"->fields,"\[Beta]rule"->{\[Beta][__]->1}]),1]*)


(* ::Input:: *)
(*expDLANormBEST2;*)
(*%-expDLAcorrectNorm2*)


(* ::Input:: *)
(*(*YESSS*)*)


(* ::Input:: *)
(*Replace[expDLANormBEST2,a_/;(!NumericQ[a]):>a/(Znorm[a,"graph"->g,"fields"->fields,"\[Beta]rule"->{\[Beta][__]->1}]),1];*)
(*ExpectationValueBlock[Z %,"endTime"->endTime,"fields"->fields,"draw"->False,"Rrule"->{R[__,Blue]->1},"graph"->g]/.\[Gamma]->"\!\(\*SubscriptBox[\(\[Gamma]\), \(3\)]\)"/.\[Gamma]->"\!\(\*SubscriptBox[\(\[Gamma]\), \(3\)]\)"/.Subscript[t, a_]->Subscript[t, a-1]//Expand;*)
(**)
(*ExpectationValueBlock[Z0 %,"endTime"->endTime,"fields"->fields,"draw"->False,"Rrule"->{R[__,Blue]->1},"graph"->g]/.\[Gamma]->"\!\(\*SubscriptBox[\(\[Gamma]\), \(3\)]\)"/.Subscript[t, a_]->Subscript[t, a-1]//Expand;*)
(*expDLANormBEST3=Replace[%,a_/;(!NumericQ[a]):>a/(Znorm[a,"graph"->g,"fields"->fields,"\[Beta]rule"->{\[Beta][__]->1}]),1]*)


(* ::Input:: *)
(*expDLANormBEST3-expDLAcorrectNorm3*)


(* ::Input:: *)
(*(*IT WORKS!!!!*)*)


(* ::Section::Closed:: *)
(*DynamicDLAactionBEST[] without (SuperStar[\[Phi]]-1) SuperStar[\[Phi]], NO \[Psi], no annihilation*)


(* ::Text:: *)
(*Same as previous one, but no annihilation term. The idea is that we do not need it since we are not able to propagate multiple copies in time. So, after getting double fields, they will naturally die in the purification step. (At least I hope so)*)
(*NO THIS DOES NOT WORK BECAUSE I NEED TO PROPAGATE DOUBLE FIELDS!!!!*)


(* ::Input:: *)
(*(*IT WORKS?*)*)


(* ::Input::Initialization:: *)
Clear[DynamicDLAactionBEST];
Options[DynamicDLAactionBEST]={"sourceBC"->1,"excludedVertices"->{},"graph"->g,"endTime"->1};

DynamicDLAactionBEST[OptionsPattern[]]:=Module[{locGraph,locEndTime,locSource,locRoot,locWeights,totWeights,locVertices,actionTimeSlice,action},
Clear[i,k,j,l,\[Gamma]];

locGraph=OptionValue["graph"];
locEndTime =OptionValue["endTime"];

locWeights=WeightedAdjacencyMatrix[locGraph]//Normal;
totWeights =Total[locWeights,{2}];

locVertices=VertexList@locGraph;

locRoot = 
 Select[(List@@@PropertyValue[locGraph,VertexLabels])[[2;;]],StringMatchQ[#[[2]],___~~"ROOT"]&][[All,1]][[1]];
locSource = 
 Select[(List@@@PropertyValue[locGraph,VertexLabels])[[2;;]],StringMatchQ[#[[2]],___~~"SOURCE"]&][[All,1]][[1]];

actionTimeSlice [T_]:= Product[
If[totWeights[[x]]===0  || MemberQ[OptionValue["excludedVertices"],x],1,
(*FALSE*)V[(1+\[Gamma] Sum[locWeights[[x,y]](R[x,y,Red]\[Phi]s[y,Subscript[t, T+1],Subscript[k, y]]-\[Phi]s[x,Subscript[t, T+1],Subscript[k, x]])\[Psi]s[x,Subscript[t, T],Subscript[jj, x]]\[Chi]s[y,Subscript[t, T],1],{y,Length[locVertices]}]*\[Phi][x,Subscript[t, T],Subscript[k, x]])]
V[(1+Sum[locWeights[[x,y]]/totWeights[[x]] R[x,y,Blue]\[Chi]s[y,Subscript[t, T],Subscript[i, x]]\[Chi][x,Subscript[t, T],Subscript[i, x]],{y,Length[locVertices]}]
(*+COPY and ANNIHILATE\[Phi]s[x,Subscript[t, T+1],Subscript[k, x]]*(\[Phi][x,Subscript[t, T],Subscript[k, x]]\[Phi][x,Subscript[t, T],Subscript[k, x]])/2 NO ANNIHILATION*)
+(*JUST COPY \[Phi]*)\[Phi]s[x,Subscript[t, T+1],Subscript[k, x]]*\[Phi][x,Subscript[t, T],Subscript[k, x]]
+(*JUST COPY \[Phi] from the interaction (just a technicality, not really needed)*)\[Phi]s[x,Subscript[t, T+1],Subscript[k, x]]*\[Psi][x,Subscript[t, T],Subscript[jj, x]]) ]
],{x,Length[locVertices]}]
V[(1+ \[Phi]s[locSource,Subscript[t, T+1],Subscript[k, locSource]]*\[Phi][locSource,Subscript[t, T],Subscript[k, locSource]]+\[Chi][locSource,Subscript[t, T],1])]
(*V[(1+ OptionValue["sourceBC"] \[Chi][locSource,Subscript[t, T],1])]*);

action=Product[actionTimeSlice[T],{T,1,locEndTime}](*Product[V[(1+\[Phi][x,Subscript[t, locEndTime+1],Subscript[k, x]])(*This allows the path tracker to be absorbed at the end*) ]V[(1+\[Phi][x,Subscript[t, locEndTime+1],0])(*This allows the path to end after "endTime" steps*)],{x,Length[locVertices]}]*);

Return[action]

]


(* ::Subsection:: *)
(*Test*)


(* ::Item:: *)
(*My favorite graph*)


(* ::Input:: *)
(*myFavoriteEdges={{1,2},{1,5},{2,3},{2,5},{5,4},{3,4}};*)
(*myFavoriteRoot=1;*)
(*myFavoriteSource=4;*)
(*g=myFavoriteGraph=MyGraph[myFavoriteEdges,myFavoriteRoot,myFavoriteSource][[1]]*)


(* ::Input:: *)
(*fields=FieldsNamesGenerator[{\[Phi],\[Psi],\[Chi]}];*)
(*endTime=1;*)
(*Z=DynamicDLAactionBEST["graph"->g,"endTime"->endTime]/.\[Beta][__]->1;*)
(*Z0=Z/.\[Gamma]->0;*)


(* ::Subitem:: *)
(*Normalized with purification*)


(* ::Input:: *)
(*\[Phi]s[1,Subscript[t, 1],Subscript[k, 1]]/Znorm[\[Phi]s[1,Subscript[t, 1],Subscript[k, 1]],"graph"->g,"fields"->fields,"\[Beta]rule"->{\[Beta][__]->1}];*)
(**)
(*ExpectationValueBlock[Z %,"endTime"->endTime,"fields"->fields,"draw"->False,"Rrule"->{R[__,Blue]->1},"graph"->g]/.\[Gamma]->"\!\(\*SubscriptBox[\(\[Gamma]\), \(1\)]\)"/.Subscript[t, a_]->Subscript[t, a-1]//Expand*)
(**)
(*Replace[%,a_/;(!NumericQ[a]):>a/(Znorm[a,"graph"->g,"fields"->fields,"\[Beta]rule"->{\[Beta][__]->1}]),1];*)
(*expDLANormBEST=ExpectationValueBlock[Z0 %,"endTime"->endTime,"fields"->fields,"draw"->False,"Rrule"->{R[__,Blue]->1},"graph"->g]/.\[Gamma]->"\!\(\*SubscriptBox[\(\[Gamma]\), \(1\)]\)"/.Subscript[t, a_]->Subscript[t, a-1]//Expand*)


(* ::Input:: *)
(*Replace[expDLANormBEST,a_/;(!NumericQ[a]):>a/(Znorm[a,"graph"->g,"fields"->fields,"\[Beta]rule"->{\[Beta][__]->1}]),1];*)
(*ExpectationValueBlock[Z %,"endTime"->endTime,"fields"->fields,"draw"->False,"Rrule"->{R[__,Blue]->1},"graph"->g]/.\[Gamma]->"\!\(\*SubscriptBox[\(\[Gamma]\), \(2\)]\)"/.Subscript[t, a_]->Subscript[t, a-1]//Expand*)
(**)
(*ExpectationValueBlock[Z0 %,"endTime"->endTime,"fields"->fields,"draw"->False,"Rrule"->{R[__,Blue]->1},"graph"->g]/.\[Gamma]->"\!\(\*SubscriptBox[\(\[Gamma]\), \(2\)]\)"/.Subscript[t, a_]->Subscript[t, a-1]//Expand;*)
(*expDLANormBEST2=Replace[%,a_/;(!NumericQ[a]):>a/(Znorm[a,"graph"->g,"fields"->fields,"\[Beta]rule"->{\[Beta][__]->1}]),1]*)


(* ::Input:: *)
(*expDLANormBEST2;*)
(*%-expDLAcorrectNorm2*)


(* ::Input:: *)
(*(*YESSS*)*)


(* ::Input:: *)
(*Replace[expDLANormBEST2,a_/;(!NumericQ[a]):>a/(Znorm[a,"graph"->g,"fields"->fields,"\[Beta]rule"->{\[Beta][__]->1}]),1];*)
(*ExpectationValueBlock[Z %,"endTime"->endTime,"fields"->fields,"draw"->False,"Rrule"->{R[__,Blue]->1},"graph"->g]/.\[Gamma]->"\!\(\*SubscriptBox[\(\[Gamma]\), \(3\)]\)"/.\[Gamma]->"\!\(\*SubscriptBox[\(\[Gamma]\), \(3\)]\)"/.Subscript[t, a_]->Subscript[t, a-1]//Expand;*)
(**)
(*ExpectationValueBlock[Z0 %,"endTime"->endTime,"fields"->fields,"draw"->False,"Rrule"->{R[__,Blue]->1},"graph"->g]/.\[Gamma]->"\!\(\*SubscriptBox[\(\[Gamma]\), \(3\)]\)"/.Subscript[t, a_]->Subscript[t, a-1]//Expand;*)
(*expDLANormBEST3=Replace[%,a_/;(!NumericQ[a]):>a/(Znorm[a,"graph"->g,"fields"->fields,"\[Beta]rule"->{\[Beta][__]->1}]),1]*)


(* ::Input:: *)
(*expDLANormBEST3-expDLAcorrectNorm3*)


(* ::Input:: *)
(*(*IT WORKS!!!!*)*)


(* ::Section::Closed:: *)
(*DynamicDLAactionBEST[] without (SuperStar[\[Phi]]-1) SuperStar[\[Phi]], \[Psi] in another position*)


(* ::Text:: *)
(*Same but with \[Psi] in another position*)


(* ::Input:: *)
(*(*IT WORKS ??*)*)


(* ::Input:: *)
(*Clear[DynamicDLAactionBEST];*)
(*Options[DynamicDLAactionBEST]={"sourceBC"->1,"excludedVertices"->{},"graph"->g,"endTime"->1};*)
(**)
(*DynamicDLAactionBEST[OptionsPattern[]]:=Module[{locGraph,locEndTime,locSource,locRoot,locWeights,totWeights,locVertices,actionTimeSlice,action},*)
(*Clear[i,k,j,l,\[Gamma]];*)
(**)
(*locGraph=OptionValue["graph"];*)
(*locEndTime =OptionValue["endTime"];*)
(**)
(*locWeights=WeightedAdjacencyMatrix[locGraph]//Normal;*)
(*totWeights =Total[locWeights,{2}];*)
(**)
(*locVertices=VertexList@locGraph;*)
(**)
(*locRoot = *)
(* Select[(List@@@PropertyValue[locGraph,VertexLabels])[[2;;]],StringMatchQ[#[[2]],___~~"ROOT"]&][[All,1]][[1]];*)
(*locSource = *)
(* Select[(List@@@PropertyValue[locGraph,VertexLabels])[[2;;]],StringMatchQ[#[[2]],___~~"SOURCE"]&][[All,1]][[1]];*)
(**)
(*actionTimeSlice [T_]:= Product[*)
(*If[totWeights[[x]]===0  || MemberQ[OptionValue["excludedVertices"],x],1,*)
(*(*FALSE*)V[(1+\[Gamma] Sum[locWeights[[x,y]](R[x,y,Red]\[Phi]s[y,Subscript[t, T+1],Subscript[k, y]]-\[Phi]s[x,Subscript[t, T+1],Subscript[k, x]])\[Psi]s[x,Subscript[t, T],Subscript[jj, x]]\[Chi]s[y,Subscript[t, T],1],{y,Length[locVertices]}]*\[Phi][x,Subscript[t, T],Subscript[k, x]])]*)
(*V[(1+Sum[locWeights[[x,y]]/totWeights[[x]] R[x,y,Blue]\[Chi]s[y,Subscript[t, T],Subscript[i, x]]\[Chi][x,Subscript[t, T],Subscript[i, x]],{y,Length[locVertices]}]*)
(*(*+(*COPY and ANNIHILATE*)\[Phi]s[x,Subscript[t, T+1],Subscript[k, x]]*(\[Phi][x,Subscript[t, T],Subscript[k, x]]\[Phi][x,Subscript[t, T],Subscript[k, x]])/2*)*)
(*+(*JUST COPY \[Phi]*)\[Phi]s[x,Subscript[t, T+1],Subscript[k, x]]*\[Phi][x,Subscript[t, T],Subscript[k, x]]*)
(*+(**) \[Psi][x,Subscript[t, T],Subscript[jj, x]]) ]*)
(*],{x,Length[locVertices]}]*)
(*V[(1+ \[Phi]s[locSource,Subscript[t, T+1],Subscript[k, locSource]]*\[Phi][locSource,Subscript[t, T],Subscript[k, locSource]]+\[Chi][locSource,Subscript[t, T],1])]*)
(*(*V[(1+ OptionValue["sourceBC"] \[Chi][locSource,Subscript[t, T],1])]*);*)
(**)
(*action=Product[actionTimeSlice[T],{T,1,locEndTime}](*Product[V[(1+\[Phi][x,Subscript[t, locEndTime+1],Subscript[k, x]])(*This allows the path tracker to be absorbed at the end*) ]V[(1+\[Phi][x,Subscript[t, locEndTime+1],0])(*This allows the path to end after "endTime" steps*)],{x,Length[locVertices]}]*);*)
(**)
(*Return[action]*)
(**)
(*]*)


(* ::Subsection::Closed:: *)
(*Test*)


(* ::Item:: *)
(*My favorite graph*)


(* ::Input:: *)
(*myFavoriteEdges={{1,2},{1,5},{2,3},{2,5},{5,4},{3,4}};*)
(*myFavoriteRoot=1;*)
(*myFavoriteSource=4;*)
(*g=myFavoriteGraph=MyGraph[myFavoriteEdges,myFavoriteRoot,myFavoriteSource][[1]]*)


(* ::Input:: *)
(*fields=FieldsNamesGenerator[{\[Phi],\[Psi],\[Chi]}];*)
(*endTime=1;*)
(*Z=DynamicDLAactionBEST["graph"->g,"endTime"->endTime]/.\[Beta][__]->1;*)
(*Z0=Z/.\[Gamma]->0;*)


(* ::Subitem::Closed:: *)
(*Normalized with purification*)


(* ::Input:: *)
(*\[Phi]s[1,Subscript[t, 1],Subscript[k, 1]]/Znorm[\[Phi]s[1,Subscript[t, 1],Subscript[k, 1]],"graph"->g,"fields"->fields,"\[Beta]rule"->{\[Beta][__]->1}];*)
(**)
(*ExpectationValueBlock[Z %,"endTime"->endTime,"fields"->fields,"draw"->False,"Rrule"->{R[__,Blue]->1},"graph"->g]/.\[Gamma]->"\!\(\*SubscriptBox[\(\[Gamma]\), \(1\)]\)"/.Subscript[t, a_]->Subscript[t, a-1]//Expand*)
(**)
(*Replace[%,a_/;(!NumericQ[a]):>a/(Znorm[a,"graph"->g,"fields"->fields,"\[Beta]rule"->{\[Beta][__]->1}]),1];*)
(*expDLANormBEST=ExpectationValueBlock[Z0 %,"endTime"->endTime,"fields"->fields,"draw"->False,"Rrule"->{R[__,Blue]->1},"graph"->g]/.\[Gamma]->"\!\(\*SubscriptBox[\(\[Gamma]\), \(1\)]\)"/.Subscript[t, a_]->Subscript[t, a-1]//Expand*)


(* ::Input:: *)
(*Replace[expDLANormBEST,a_/;(!NumericQ[a]):>a/(Znorm[a,"graph"->g,"fields"->fields,"\[Beta]rule"->{\[Beta][__]->1}]),1];*)
(*ExpectationValueBlock[Z %,"endTime"->endTime,"fields"->fields,"draw"->False,"Rrule"->{R[__,Blue]->1},"graph"->g]/.\[Gamma]->"\!\(\*SubscriptBox[\(\[Gamma]\), \(2\)]\)"/.Subscript[t, a_]->Subscript[t, a-1]//Expand*)
(**)
(*ExpectationValueBlock[Z0 %,"endTime"->endTime,"fields"->fields,"draw"->False,"Rrule"->{R[__,Blue]->1},"graph"->g]/.\[Gamma]->"\!\(\*SubscriptBox[\(\[Gamma]\), \(2\)]\)"/.Subscript[t, a_]->Subscript[t, a-1]//Expand;*)
(*expDLANormBEST2=Replace[%,a_/;(!NumericQ[a]):>a/(Znorm[a,"graph"->g,"fields"->fields,"\[Beta]rule"->{\[Beta][__]->1}]),1]*)


(* ::Input:: *)
(*expDLANormBEST2;*)
(*%-expDLAcorrectNorm2*)


(* ::Input:: *)
(*(*YESSS*)*)


(* ::Input:: *)
(*Replace[expDLANormBEST2,a_/;(!NumericQ[a]):>a/(Znorm[a,"graph"->g,"fields"->fields,"\[Beta]rule"->{\[Beta][__]->1}]),1];*)
(*ExpectationValueBlock[Z %,"endTime"->endTime,"fields"->fields,"draw"->False,"Rrule"->{R[__,Blue]->1},"graph"->g]/.\[Gamma]->"\!\(\*SubscriptBox[\(\[Gamma]\), \(3\)]\)"/.\[Gamma]->"\!\(\*SubscriptBox[\(\[Gamma]\), \(3\)]\)"/.Subscript[t, a_]->Subscript[t, a-1]//Expand;*)
(**)
(*ExpectationValueBlock[Z0 %,"endTime"->endTime,"fields"->fields,"draw"->False,"Rrule"->{R[__,Blue]->1},"graph"->g]/.\[Gamma]->"\!\(\*SubscriptBox[\(\[Gamma]\), \(3\)]\)"/.Subscript[t, a_]->Subscript[t, a-1]//Expand;*)
(*expDLANormBEST3=Replace[%,a_/;(!NumericQ[a]):>a/(Znorm[a,"graph"->g,"fields"->fields,"\[Beta]rule"->{\[Beta][__]->1}]),1]*)


(* ::Input:: *)
(*expDLANormBEST3-expDLAcorrectNorm3*)


(* ::Input:: *)
(*(*IT WORKS!!!!*)*)


(* ::Section:: *)
(*DynamicDLAactionBEST[] without (SuperStar[\[Phi]]-1) SuperStar[\[Phi]], NO \[Psi], propagation of (SuperStar[\[Phi]])^2 ...WORK IN PROGRESS...*)


(* ::Text:: *)
(*I had the intuition that we needed to propagate squared fields. Lets follow this intuition*)


(* ::Input:: *)
(*Clear[DynamicDLAactionBEST];*)
(*Options[DynamicDLAactionBEST]={"sourceBC"->1,"excludedVertices"->{},"graph"->g,"endTime"->1};*)
(**)
(*DynamicDLAactionBEST[OptionsPattern[]]:=Module[{locGraph,locEndTime,locSource,locRoot,locWeights,totWeights,locVertices,actionTimeSlice,action},*)
(*Clear[i,k,j,l,\[Gamma]];*)
(**)
(*locGraph=OptionValue["graph"];*)
(*locEndTime =OptionValue["endTime"];*)
(**)
(*locWeights=WeightedAdjacencyMatrix[locGraph]//Normal;*)
(*totWeights =Total[locWeights,{2}];*)
(**)
(*locVertices=VertexList@locGraph;*)
(**)
(*locRoot = *)
(* Select[(List@@@PropertyValue[locGraph,VertexLabels])[[2;;]],StringMatchQ[#[[2]],___~~"ROOT"]&][[All,1]][[1]];*)
(*locSource = *)
(* Select[(List@@@PropertyValue[locGraph,VertexLabels])[[2;;]],StringMatchQ[#[[2]],___~~"SOURCE"]&][[All,1]][[1]];*)
(**)
(*actionTimeSlice [T_]:= Product[*)
(*If[totWeights[[x]]===0  || MemberQ[OptionValue["excludedVertices"],x],1,*)
(*(*FALSE*)V[(1+ \[Gamma] Sum[locWeights[[x,y]](R[x,y,Red]\[Phi]s[y,Subscript[t, T+1],Subscript[k, y]]-\[Phi]s[x,Subscript[t, T+1],Subscript[k, x]])\[Chi]s[y,Subscript[t, T],1],{y,Length[locVertices]}]*\[Psi][x,Subscript[t, T],Subscript[jj, x]])]*)
(*V[(1+Sum[locWeights[[x,y]]/totWeights[[x]] R[x,y,Blue]\[Chi]s[y,Subscript[t, T],Subscript[i, x]]\[Chi][x,Subscript[t, T],Subscript[i, x]],{y,Length[locVertices]}]*)
(*(*+(*COPY and ANNIHILATE*)\[Phi]s[x,Subscript[t, T+1],Subscript[k, x]]*(\[Phi][x,Subscript[t, T],Subscript[k, x]]\[Phi][x,Subscript[t, T],Subscript[k, x]])/2*)*)
(*+ (* COPY \[Phi]^2*)\[Phi]s[x,Subscript[t, T+1],Subscript[k, x]]\[Phi]s[x,Subscript[t, T+1],Subscript[k, x]]*(\[Phi][x,Subscript[t, T],Subscript[k, x]]\[Phi][x,Subscript[t, T],Subscript[k, x]])/2*)
(*+ (* COPY \[Phi] to \[Phi]^2*)\[Phi]s[x,Subscript[t, T+1],Subscript[k, x]]\[Phi]s[x,Subscript[t, T+1],Subscript[k, x]]*\[Phi][x,Subscript[t, T],Subscript[k, x]]*)
(*+(*COPY and BRANCH*)\[Phi]s[x,Subscript[t, T+1],Subscript[k, x]]\[Psi]s[x,Subscript[t, T],Subscript[jj, x]]*\[Phi][x,Subscript[t, T],Subscript[k, x]]*)
(*+(*COPY and BRANCH*)\[Phi]s[x,Subscript[t, T+1],Subscript[k, x]]\[Psi]s[x,Subscript[t, T],Subscript[jj, x]]*(\[Phi][x,Subscript[t, T],Subscript[k, x]]\[Phi][x,Subscript[t, T],Subscript[k, x]])/2) ]*)
(*],{x,Length[locVertices]}]*)
(*V[(1+ \[Phi]s[locSource,Subscript[t, T+1],Subscript[k, locSource]]*\[Phi][locSource,Subscript[t, T],Subscript[k, locSource]]+\[Chi][locSource,Subscript[t, T],1])]*)
(*(*V[(1+ OptionValue["sourceBC"] \[Chi][locSource,Subscript[t, T],1])]*);*)
(**)
(*action=Product[actionTimeSlice[T],{T,1,locEndTime}](*Product[V[(1+\[Phi][x,Subscript[t, locEndTime+1],Subscript[k, x]])(*This allows the path tracker to be absorbed at the end*) ]V[(1+\[Phi][x,Subscript[t, locEndTime+1],0])(*This allows the path to end after "endTime" steps*)],{x,Length[locVertices]}]*);*)
(**)
(*Return[action]*)
(**)
(*]*)


(* ::Subsection:: *)
(*Test*)


(* ::Item:: *)
(*My favorite graph*)


(* ::Input:: *)
(*myFavoriteEdges={{1,2},{1,5},{2,3},{2,5},{5,4},{3,4}};*)
(*myFavoriteRoot=1;*)
(*myFavoriteSource=4;*)
(*g=myFavoriteGraph=MyGraph[myFavoriteEdges,myFavoriteRoot,myFavoriteSource][[1]]*)


(* ::Input:: *)
(*fields=FieldsNamesGenerator[{\[Phi],\[Psi],\[Chi]}];*)
(*endTime=1;*)
(*Z=DynamicDLAactionBEST["graph"->g,"endTime"->endTime]/.\[Beta][__]->1;*)
(*Z0=Z/.\[Gamma]->0;*)


(* ::Subitem:: *)
(*Normalized*)


(* ::Input:: *)
(*\[Phi]s[1,Subscript[t, 1],Subscript[k, 1]]/Znorm[\[Phi]s[1,Subscript[t, 1],Subscript[k, 1]],"graph"->g,"fields"->fields,"\[Beta]rule"->{\[Beta][__]->1}];*)
(**)
(*expDLANormBEST=ExpectationValueBlock[Z %,"endTime"->endTime,"fields"->fields,"draw"->False,"Rrule"->{R[__,Blue]->1},"graph"->g]/.\[Gamma]->"\!\(\*SubscriptBox[\(\[Gamma]\), \(1\)]\)"/.Subscript[t, a_]->Subscript[t, a-1]//Expand*)


(* ::Input:: *)
(*Replace[expDLANormBEST,a_/;(!NumericQ[a]):>a/(Znorm[a,"graph"->g,"fields"->fields,"\[Beta]rule"->{\[Beta][__]->1}]),1];*)
(*expDLANormBEST2=ExpectationValueBlock[Z %,"endTime"->endTime,"fields"->fields,"draw"->False,"Rrule"->{R[__,Blue]->1},"graph"->g]/.\[Gamma]->"\!\(\*SubscriptBox[\(\[Gamma]\), \(2\)]\)"/.Subscript[t, a_]->Subscript[t, a-1]//Expand*)


(* ::Input:: *)
(*expDLANormBEST2/.Subscript[SuperStar[\[Phi]], a_,b_,c_] *Subscript[SuperStar[\[Phi]], a_,b_,c_]->Subscript[SuperStar[\[Phi]], a,b,c];*)
(*%-expDLAcorrectNorm2*)


(* ::Input:: *)
(*(*YESSS*)*)


(* ::Input:: *)
(*Replace[expDLANormBEST2, a_/;FreeQ[a, R[1,2,Red]]:>0,1]*)


(* ::Input:: *)
(*Replace[expDLANormBEST2,a_/;(!NumericQ[a]):>a/(Znorm[a,"graph"->g,"fields"->fields,"\[Beta]rule"->{\[Beta][__]->1}]),1];*)
(*ExpectationValueBlock[Z %,"endTime"->endTime,"fields"->fields,"draw"->False,"Rrule"->{R[__,Blue]->1},"graph"->g]/.\[Gamma]->"\!\(\*SubscriptBox[\(\[Gamma]\), \(3\)]\)"/.Subscript[t, a_]->Subscript[t, a-1]//Expand;*)
(**)
(*Replace[%,a_/;(!NumericQ[a]):>a/(Znorm[a,"graph"->g,"fields"->fields,"\[Beta]rule"->{\[Beta][__]->1}]),1];*)
(*expDLANormBEST3=ExpectationValueBlock[Z0 %,"endTime"->endTime,"fields"->fields,"draw"->False,"Rrule"->{R[__,Blue]->1},"graph"->g]/.\[Gamma]->"\!\(\*SubscriptBox[\(\[Gamma]\), \(3\)]\)"/.Subscript[t, a_]->Subscript[t, a-1]//Expand*)
(**)


(* ::Input:: *)
(*expDLANormBEST3/.Subscript[SuperStar[\[Phi]], a_,b_,c_] *Subscript[SuperStar[\[Phi]], a_,b_,c_]->Subscript[SuperStar[\[Phi]], a,b,c];*)
(*%-expDLAcorrectNorm3*)


(* ::Input:: *)
(*(*IT WORKS!!!!*)*)
