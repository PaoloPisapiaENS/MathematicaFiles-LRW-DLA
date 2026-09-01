(* ::Package:: *)

(* ::Title::Closed:: *)
(*Initialization*)


(* ::Input::Initialization:: *)
(*SetOptions[$FrontEndSession,NotebookAutoSave->True]*)
(*With[{nb=EvaluationNotebook[]},RunScheduledTask[If["ModifiedInMemory"/. NotebookInformation[nb],NotebookSave[nb]],300]]
NotebookSave[]*)


(* ::Input::Initialization:: *)
<<PaoloInitialization`
??PaoloInitialization`*


(* ::Input:: *)
(*$Paolofontsize=15*)
(*$Paolofont*)


(* ::Input:: *)
(*(*Quit*)*)


(* ::Input:: *)
(*FrontEndTokenExecute["SelectAll"]*)
(*FrontEndTokenExecute["SelectionCloseAllGroups"]*)


(* ::Input::Initialization:: *)
$Assumptions=b>0


(* ::Title:: *)
(*\[Beta]Function[] and \[Gamma]Function[]*)


(* ::Section::Closed:: *)
(*\[Beta]Function[] Definitions*)


(* ::Subsection::Closed:: *)
(*For the RG with effective finite quantities (i.e. renormalization without CTs)*)


(* ::Input:: *)
(*(*This is the old version*)*)
(*ClearAll[\[Beta]Function];*)
(**)
(*Options[\[Beta]Function]={"print"->False,"g0Order"->0};*)
(**)
(*\[Beta]Function[coupling_,OptionsPattern[]]:=Module[{gr,\[Beta]f,nLoop,i},*)
(*Clear[g,g0,\[Mu],\[Epsilon]];*)
(**)
(*nLoop=OptionValue["g0Order"];*)
(*If[nLoop==0,nLoop=Exponent[coupling,g0]];*)
(**)
(*gr=Normal@Series[coupling,{g0,0,nLoop}];*)
(**)
(*If[OptionValue["print"],*)
(*Print["Initial effective couling:\n ",gr,"\n"];];*)
(**)
(*\[Beta]f=-\[Mu] D[gr,\[Mu]]//Expand;*)
(*If[OptionValue["print"],*)
(*Print["\n\[Beta]-function with bare coupling: ", \[Beta]f,"\n"];];*)
(**)
(*gr=g-coupling+g0 \[Mu]^-\[Epsilon];*)
(**)
(*If[OptionValue["print"],*)
(*Print[" Bare coupling= \n ",gr,"\n"];];*)
(**)
(*(* Invert g(g0) *)*)
(*Do[\[Beta]f=\[Beta]f/.g0^n_ \[Mu]^(-n_ \[Epsilon]):>(gr)^n \[Mu]^(n \[Epsilon])//Expand;*)
(*\[Beta]f=\[Beta]f/.(g0 ):>(gr)\[Mu]^ \[Epsilon]//Expand;*)
(*(*\[Beta]f=Series[\[Beta]f,{g0,0,nLoop}]//Expand;*)*)
(*\[Beta]f=\[Beta]f/.g0^n_/;n>nLoop:>0;*)
(*\[Beta]f=\[Beta]f/.g0^n_/;n==nLoop:>(g \[Mu]^\[Epsilon])^n//Expand;*)
(*If[OptionValue["print"],*)
(*Print[\[Beta]f//FullSimplify,"\n"];];*)
(*,{i,1,nLoop}];*)
(**)
(*(*For[i=1,i<=nLoop,i++,*)
(*\[Beta]f=\[Beta]f/.g0^n_ \[Mu]^(n_ \[Epsilon]):>(gr)^n//Expand;*)
(*\[Beta]f=\[Beta]f/.(g0 \[Mu]^\[Epsilon]):>(gr)//Expand;*)
(*];*)*)
(**)
(*\[Beta]f=Normal[\[Beta]f]/.g0^n_ :>(g \[Mu]^\[Epsilon])^n//Expand;*)
(*\[Beta]f=\[Beta]f/.(g0 ):>(g \[Mu]^\[Epsilon])//Expand;*)
(*\[Beta]f=Series[\[Beta]f,{g,0,nLoop}]//Map[Expand,#]&;*)
(*(*Print[\[Beta]f];*)*)
(*(*\[Beta]f=Normal[\[Beta]f];*)*)
(*Return[\[Beta]f//FullSimplify]]*)


(* ::Input::Initialization:: *)
ClearAll[\[Beta]Function];

Options[\[Beta]Function]={"print"->False,"g0Order"->0};

\[Beta]Function[coupling_,OptionsPattern[]]:=Module[{gr,gB,\[Gamma],\[Beta]f,nLoop,i},
Clear[g,g0,\[Mu],\[Epsilon]];

nLoop=OptionValue["g0Order"];
If[nLoop==0,nLoop=Exponent[coupling,g0]];

gr=Normal@Series[coupling,{g0,0,nLoop}];

If[OptionValue["print"],
Print["Initial effective couling:\n ",gr,"\n"];];

\[Beta]f=-\[Mu] D[gr,\[Mu]]//Expand;
\[Beta]f=Series[\[Beta]f,{g0,0,nLoop}];
If[OptionValue["print"],
Print[" \[Beta]-function with bare coupling:\n\t", \[Beta]f,"\n"];];

(* Invert g(g0) *)
(*gr=g-coupling+g0 \[Mu]^-\[Epsilon]+O[g0]^nLoop;*)
gB=(g0+(g-gr)*\[Mu]^\[Epsilon]//Expand)+O[\[Gamma]]^(nLoop+1);

gB=(gB/.{g->g \[Gamma],g0->g0 \[Gamma]});

If[OptionValue["print"],
Print[" Initial bare coupling: \n\t g0(g)=",gB,"\n"];];
gB=(gB//.g0->gB/\[Gamma])//Expand;
gB=Normal[gB]/.\[Gamma]->1;

If[OptionValue["print"],
Print[" Bare coupling: \n\t g0(g)=",gB,"\n"];];

(*
Do[\[Beta]f=\[Beta]f/.g0^n_ \[Mu]^(-n_ \[Epsilon]):>(gr)^n\[Mu]^(n \[Epsilon])//Expand;
(*\[Beta]f=\[Beta]f/.(g0 ):>(gr)\[Mu]^ \[Epsilon]//Expand;
(*\[Beta]f=Series[\[Beta]f,{g0,0,nLoop}]//Expand;*)
\[Beta]f=\[Beta]f/.g0^n_/;n>nLoop:>0;
\[Beta]f=\[Beta]f/.g0^n_/;n==nLoop:>(g \[Mu]^\[Epsilon])^n//Expand;*)
If[OptionValue["print"],
Print[" Substition #",i,":\n\t",\[Beta]f//FullSimplify,"\n"];];
,{i,1,nLoop}];

(*For[i=1,i<=nLoop,i++,
\[Beta]f=\[Beta]f/.g0^n_ \[Mu]^(n_ \[Epsilon]):>(gr)^n//Expand;
\[Beta]f=\[Beta]f/.(g0 \[Mu]^\[Epsilon]):>(gr)//Expand;
];*)

\[Beta]f=Normal[\[Beta]f]/.g0^n_ :>(g \[Mu]^\[Epsilon])^n//Expand;*)
\[Beta]f=Normal[\[Beta]f]/.(g0 ):>(gB)//Expand;
\[Beta]f=Series[\[Beta]f,{g,0,nLoop}]//Map[Expand,#]&;
(*Print[\[Beta]f];*)
(*\[Beta]f=Normal[\[Beta]f];*)

Return[\[Beta]f//FS]
]


(* ::Item::Closed:: *)
(*Test on inverting g(g0)*)


(* ::Input:: *)
(*(*\[Beta]as function of g0*)*)
(*\[Epsilon] \[Mu]^-\[Epsilon] g0-2 ((1+2 b) banana \[Epsilon] \[Mu]^(-2 \[Epsilon])) g0^2+SeriesData[g0, 0, {}, 1, 3, 1] ;*)
(*(*g as function of g0*)*)
(*g0 \[Mu]^-\[Epsilon]-(a g0^2 \[Mu]^(-2 \[Epsilon]))+b g0^3 \[Mu]^(-3\[Epsilon])-c g0^4 \[Mu]^(-4\[Epsilon]);*)
(*(*Inversion to get g0 as a function of g*)*)
(*gg0=(g0+(g-%)*\[Mu]^\[Epsilon]//Expand)+O[\[Gamma]]^5*)
(*gg0=(gg0/.{g->g \[Gamma],g0->g0 \[Gamma]})*)
(*gg0=(gg0//.g0->gg0/\[Gamma])//Expand*)
(**)
(*Clear[gg0]*)


(* ::Input:: *)
(*(*Check: yep!*)*)


(* ::Input:: *)
(*(Normal[SeriesData[\[Gamma], 0, {g \[Mu]^\[Epsilon], a g^2 \[Mu]^\[Epsilon], (2 a^2 - b) g^3 \[Mu]^\[Epsilon], (5 a^3 - 5 a b + c) g^4 \[Mu]^\[Epsilon]}, 1, 5, 1]]/.\[Gamma]->1)/.g->g0 \[Mu]^-\[Epsilon]-(a g0^2 \[Mu]^(-2 \[Epsilon]))+b g0^3 \[Mu]^(-3\[Epsilon])-c g0^4 \[Mu]^(-4\[Epsilon])*)
(*Series[%,{g0,0,6}]*)


(* ::Subsection::Closed:: *)
(*I can write \[Beta] as  *)
(*\!\(TraditionalForm\`\[Beta] == \[Epsilon] \**)
(*SubscriptBox[*)
(*StyleBox["g", "TI"], *)
(*StyleBox["R", "TI"]] \**)
(*FractionBox["1", *)
(*RowBox[{"1", "+", *)
(*SubscriptBox[*)
(*StyleBox["g", "TI"], *)
(*StyleBox["R", "TI"]], *)
(*SubscriptBox["\[PartialD]", *)
(*SubscriptBox[*)
(*StyleBox["g", "TI"], *)
(*StyleBox["R", "TI"]]], "log", *)
(*StyleBox["Z", "TI"]}]]\) with \!\(TraditionalForm\`\**)
(*SubscriptBox[*)
(*StyleBox["g", "TI"], *)
(*StyleBox["B", "TI"]] == \**)
(*SubscriptBox[*)
(*StyleBox["g", "TI"], *)
(*StyleBox["R", "TI"]] \**)
(*StyleBox["Z", "TI"] *)
(*\*SuperscriptBox[\(\[Mu]\), \(\[Epsilon]\)]\)*)


(* ::Text:: *)
(*But then I am not sure I can generalize it...*)


(* ::Input::Initialization:: *)
ClearAll[\[Beta]FunctionFromZ];

Options[\[Beta]FunctionFromZ]={"print"->False};

\[Beta]FunctionFromZ[Zg_,LoopOrder_:0,gg_:{g},OptionsPattern[]]:=Module[{z,\[Beta]f,nLoop,i},
Clear[g,g0,\[Mu],\[Epsilon]];

z=Expand[Zg];

nLoop=LoopOrder;
If[nLoop==0,nLoop=Exponent[z,gg[[1]]]+1];

z=Normal@Series[Zg,Sequence@@({#,0,nLoop}&/@gg)];

If[OptionValue["print"],
Print["RG factor:\n ",z,"\n"];];

\[Beta]f=\[Epsilon] gg[[1]] 1/(1+gg[[1]] D[Log[z],gg[[1]]]);

\[Beta]f=Series[\[Beta]f,Sequence@@({#,0,nLoop}&/@gg)]//Map[Expand,#]&;

If[OptionValue["print"],
Print["\n\[Beta]-function: ", \[Beta]f,"\n"];];

Return[Map[Expand,\[Beta]f]]]


(* ::Subsection::Closed:: *)
(*I can also write it as (this is a mess to implement with the derivative wrt \[Mu]) NOT IMPLEMENTED*)
(*\!\(TraditionalForm\`\[Beta] == \[Epsilon] \**)
(*SubscriptBox[*)
(*StyleBox["g", "TI"], *)
(*StyleBox["R", "TI"]] + \**)
(*SubscriptBox[*)
(*StyleBox["g", "TI"], *)
(*StyleBox["R", "TI"]] \[Mu] *)
(*\*SubscriptBox[\(\[PartialD]\), \(\[Mu]\)]log \**)
(*StyleBox["Z", "TI"]\) with \!\(TraditionalForm\`\**)
(*SubscriptBox[*)
(*StyleBox["g", "TI"], *)
(*StyleBox["B", "TI"]] == \**)
(*SubscriptBox[*)
(*StyleBox["g", "TI"], *)
(*StyleBox["R", "TI"]] \**)
(*StyleBox["Z", "TI"] *)
(*\*SuperscriptBox[\(\[Mu]\), \(\[Epsilon]\)]\) *)


(* ::Input:: *)
(*ClearAll[\[Beta]FunctionFromZ2];*)
(**)
(*Options[\[Beta]FunctionFromZ2]={"print"->False};*)
(**)
(*\[Beta]FunctionFromZ2[Zg_,LoopOrder_:0,OptionsPattern[]]:=Module[{z,\[Beta]f,nLoop,i},*)
(*Clear[g,g0,\[Mu],\[Epsilon]];*)
(**)
(*z=Expand[Zg];*)
(**)
(*nLoop=LoopOrder;*)
(*If[nLoop==0,nLoop=Exponent[z,g]+1];*)
(**)
(*z=Normal@Series[Zg,{g,0,nLoop}];*)
(**)
(*If[OptionValue["print"],*)
(*Print["RG factor:\n ",z,"\n"];];*)
(**)
(*\[Beta]f=\[Epsilon] g 1/(1+g D[Log[z],g]);*)
(**)
(*\[Beta]f=Series[\[Beta]f,{g,0,nLoop}]//Map[Expand,#]&;*)
(**)
(*If[OptionValue["print"],*)
(*Print["\n\[Beta]-function: ", \[Beta]f,"\n"];];*)
(**)
(*Return[Map[Expand,\[Beta]f]]]*)


(* ::Subsection:: *)
(*Tests and Results*)


(* ::Subsection::Closed:: *)
(*\[Section] b-LRW 2-loop: *)


(* ::Subsubsection::Closed:: *)
(*Using my result*)


(* ::Input:: *)
(*g=g0 \[Mu]^-\[Epsilon]-(b+2)banana (g0 \[Mu]^-\[Epsilon])^2+(g0 \[Mu]^-\[Epsilon])^3 (b+2)( doubleBanana + 2(b+1) hat);*)
(*\[Beta]Function[g,"print"->False]*)
(*%/.banana ->1/\[Epsilon]/.doubleBanana ->1/\[Epsilon]^2/.hat ->1/(2\[Epsilon]^2)+1/(4\[Epsilon])//FullSimplify*)
(*RGeq2=Normal[%]==0;*)
(**)


(* ::Input:: *)
(*(*Nice, this is finite*)*)


(* ::Subitem::Closed:: *)
(*Let's check Kay's ansatz for g (from his email "picture"): OK, IT IS FINITE TOO*)


(* ::Input:: *)
(*g=g0 \[Mu]^-\[Epsilon]-(b+2)banana (g0 \[Mu]^-\[Epsilon])^2+(g0 \[Mu]^-\[Epsilon])^3 ( (b^2+2)doubleBanana + 4(2b+1) hat );*)
(*\[Beta]Function[g,"print"->False]*)
(*%/.banana ->1/\[Epsilon]/.doubleBanana ->1/\[Epsilon]^2/.hat ->1/(2\[Epsilon]^2)+1/(4\[Epsilon])//FullSimplify*)
(*RGeq2=Normal[%]==0;*)


(* ::Item::Closed:: *)
(*Let's get the 2-Loop critical g *)


(* ::Input:: *)
(*gc1=\[Epsilon]/(b+2);*)
(*gc2=gc1+A \[Epsilon]^2*)
(*RGeq2/.g->gc2;*)
(*Expand[%];*)
(*%/.\[Epsilon]^n_/;n>3:>0;*)
(*gc2=Collect[gc2/.Flatten@Solve[%,A]//FullSimplify,{\[Epsilon],\[Epsilon]^2},FullSimplify]*)


(* ::Input:: *)
(*(*OK*)*)


(* ::Subsubsection::Closed:: *)
(*Using my result EXTENDED WITH WAVE-FUNCTION RENORMALIZATION*)


(* ::Input:: *)
(*g=g0 \[Mu]^-\[Epsilon]-(b+2)banana (g0 \[Mu]^-\[Epsilon])^2+(g0 \[Mu]^-\[Epsilon])^3 ( (b+2)( doubleBanana + 2(b+1) hat)-a b(b-1) 1/2 sunset);*)
(*\[Beta]Function[g,"print"->False]*)
(*%/.banana ->1/\[Epsilon]/.doubleBanana ->1/\[Epsilon]^2/.hat ->1/(2\[Epsilon]^2)+1/(4\[Epsilon])/.sunset->-1/(8\[Epsilon])//FullSimplify//Factor*)
(*RGeq2=Normal[%]==0;*)
(**)


(* ::Input:: *)
(*(*Nice, this is finite*)*)


(* ::Section::Closed:: *)
(*\[Gamma]Function[] Definitions*)


(* ::Subsection:: *)
(*\[Gamma]Function[]*)


(* ::Input:: *)
(*(*Old code*)*)


(* ::Input:: *)
(*ClearAll[\[Gamma]Function];*)
(**)
(**)
(*Options[\[Gamma]Function]={"print"->False,"g0Order"->0};*)
(**)
(**)
(*\[Gamma]Function[observable_,bareCoupling_, OptionsPattern[]]:=Module[{U,gr,\[Gamma]f,nLoop,i},*)
(*Clear[g,g0,\[Mu],\[Epsilon]];*)
(**)
(**)
(*nLoop=OptionValue["g0Order"];*)
(*If[nLoop==0,nLoop=Exponent[bareCoupling,g0]-1];*)
(*(*Print[nLoop]*);*)
(**)
(*gr=Normal@Series[bareCoupling,{g0,0,nLoop}];*)
(**)
(*U=Normal@Series[observable,{g0,0,nLoop}];*)
(**)
(*\[Gamma]f=-\[Mu] D[Log[U],\[Mu]]//Expand;*)
(*If[OptionValue["print"],*)
(*Print[" \[Gamma]f(\!\(\*SubscriptBox[*)
(*StyleBox[\"g\",\nBackground->RGBColor[0.9, 1, 1]], \(0\)]\))= \n ",\[Gamma]f];];*)
(**)
(**)
(*gr=g-gr+g0 \[Mu]^-\[Epsilon];*)
(**)
(*If[OptionValue["print"],*)
(*Print[" Bare coupling= \n ",gr];];*)
(**)
(*Do[\[Gamma]f=\[Gamma]f/.g0^n_ :>(gr)^n \[Mu]^(n \[Epsilon])//Expand;*)
(*\[Gamma]f=\[Gamma]f/.(g0 ):>(gr)\[Mu]^\[Epsilon]//Expand;*)
(*\[Gamma]f=\[Gamma]f/.g0^n_/;n>nLoop:>0;*)
(*\[Gamma]f=\[Gamma]f/.g0^n_/;n==nLoop:>(g \[Mu]^\[Epsilon])^n//Expand;*)
(*,{i,1,nLoop}];*)
(**)
(*(*For[i=1,i<=nLoop,i++,*)
(*\[Gamma]f=\[Gamma]f/.g0^n_ \[Mu]^(n_ \[Epsilon]):>(gr)^n//Expand;*)
(*\[Gamma]f=\[Gamma]f/.(g0 \[Mu]^\[Epsilon]):>(gr)//Expand;*)
(*];*)*)
(**)
(*\[Gamma]f=\[Gamma]f/.g0^n_ :>(g)^n \[Mu]^(n \[Epsilon])//Expand;*)
(*\[Gamma]f=\[Gamma]f/.(g0 ):>(g)\[Mu]^\[Epsilon]//Expand;*)
(*(**)
(*If[OptionValue["print"],*)
(*Print[" \[Gamma]f(g)= \n ",\[Gamma]f];];*)*)
(**)
(*\[Gamma]f=Series[\[Gamma]f,{g,0,nLoop}]//Expand;*)
(*\[Gamma]f=Factor@Simplify/@\[Gamma]f;*)
(*(*Print[\[Gamma]f];*)*)
(*(*\[Gamma]f=Normal[\[Gamma]f];*)*)
(*Return[\[Gamma]f]]*)


(* ::Input::Initialization:: *)
ClearAll[\[Gamma]Function];


Options[\[Gamma]Function]={"print"->False,"g0Order"->0};


\[Gamma]Function[observable_,bareCoupling_, OptionsPattern[]]:=Module[{U,gB,gr,\[Gamma],\[Gamma]f,nLoop,i},
Clear[g,g0,\[Mu],\[Epsilon]];


nLoop=OptionValue["g0Order"];
If[nLoop==0,nLoop=Exponent[bareCoupling,g0]-1];
(*Print[nLoop]*);

gr=Normal@Series[bareCoupling,{g0,0,nLoop}];

U=Normal@Series[observable,{g0,0,nLoop}];

\[Gamma]f=-\[Mu] D[Log[U],\[Mu]]//Expand;
If[OptionValue["print"],
Print[" \[Gamma]f(\!\(\*SubscriptBox[
StyleBox[\"g\",\nBackground->RGBColor[0.9, 1, 1]], \(0\)]\))=-\[Mu] D[Log[U],\[Mu]]= "(*,\[Gamma]f*)];];

\[Gamma]f=Series[\[Gamma]f,{g0,0,nLoop}];
If[OptionValue["print"],
Print["\t\t= ",\[Gamma]f];];

(*Invert g(g0)*)

(*gr=g-gr+g0 \[Mu]^-\[Epsilon];

If[OptionValue["print"],
Print[" Bare coupling= \n ",gr];];*)

gB=(g0+(g-gr)*\[Mu]^\[Epsilon]//Expand)+O[\[Gamma]]^(nLoop+1);

gB=(gB/. {g->g  \[Gamma],g0->g0  \[Gamma]});

If[OptionValue["print"],Print[" Initial bare coupling: \n\t g0(g)=",gB,"\n"];];

gB=(gB//.g0->gB/\[Gamma])//Expand;
gB=Normal[gB]/. \[Gamma]->1;

If[OptionValue["print"],Print[" Bare coupling: \n\t g0(g)=",gB,"\n"];];


\[Gamma]f=Normal[\[Gamma]f]/.(g0 ):>(gB)//Expand;
\[Gamma]f=Series[\[Gamma]f,{g,0,nLoop}]//Map[Expand,#]&;

Return[\[Gamma]f//FS]]


(* ::Subsection::Closed:: *)
(*\[Gamma]FunctionFromZ[]*)


(* ::Input:: *)
(*Times@@{1,2,a^-1,c^2}*)


(* ::Input:: *)
(*\[Gamma]FunctionFromZ[{1},1,0]*)


(* ::Input::Initialization:: *)
(*Don't use the List feature, it is not the correct operation!*)
ClearAll[\[Gamma]FunctionFromZ];

Options[\[Gamma]FunctionFromZ]={"print"->False,"gstar"->True};

\[Gamma]FunctionFromZ[Zobservable_List,ZCoupling_,options:OptionsPattern[]]:=
\[Gamma]FunctionFromZ[Zobservable,ZCoupling,options,0]

\[Gamma]FunctionFromZ[Zobservable_List,ZCoupling_,OptionsPattern[],LoopOrder_:0]:=Module[{U,gr,\[Gamma]f,\[Beta],factor,eq,gstar,nLoop,i},
Clear[g,g0,\[Mu],\[Epsilon]];

U=Zobservable;
factor=Length[U];
U=Times@@U;

nLoop=LoopOrder;
If[nLoop==0,nLoop=Exponent[U,g]];
(*Print[nLoop]*);

gr=Normal@Series[ZCoupling,{g,0,nLoop}];

U=Series[U,{g,0,nLoop}];

\[Gamma]f=- D[Log[U],g]//Expand;
If[OptionValue["print"],
Print[Style[" - dLogZ/dg= ",{RGBColor[0, 0, 1],Bold}],\[Gamma]f];];

\[Beta]=\[Beta]FunctionFromZ[gr];
If[OptionValue["print"],
Print[Style[" \[Beta]= ",{RGBColor[0, 0, 1],Bold}],\[Beta]];];
If[OptionValue["gstar"],
gstar=Select[Flatten@SolveValues[Simplify[Normal[\[Beta]]]==0,g],#=!=0&];

If[OptionValue["print"],
Print[Style[" Possible \!\(\*SuperscriptBox[\(g\), \(*\)]\)s= ",{RGBColor[0, 0, 1],Bold}],gstar];];

gstar=Series[gstar,{\[Epsilon],0,nLoop},Assumptions->b>0]//Expand;
gstar=Select[Normal@gstar,(#/.\[Epsilon]->0)==0&];


If[OptionValue["print"],
Print[Style[" Selected \!\(\*SuperscriptBox[\(g\), \(*\)]\)s= ",{RGBColor[0, 0, 1],Bold}],gstar];];
];

\[Gamma]f=\[Gamma]f*\[Beta]/factor;

If[OptionValue["print"],
Print[Style[Row[{" \[Gamma]f= - \[Beta]/",factor ,"* dLogZ/dg= "}],{RGBColor[0, 0, 1],Bold}],FS[\[Gamma]f]];];

If[OptionValue["gstar"],
\[Gamma]f=Normal[\[Gamma]f]/.g->gstar[[1]];
];

\[Gamma]f=Series[\[Gamma]f,{\[Epsilon],0,nLoop}]//Expand;

\[Gamma]f=Factor@(FullSimplify/@\[Gamma]f);
(*Print[\[Gamma]f];*)
(*\[Gamma]f=Normal[\[Gamma]f];*)
Return[Normal[\[Gamma]f]]
]


\[Gamma]FunctionFromZ[Zobservable_,ZCoupling_,options:OptionsPattern[]]:=
\[Gamma]FunctionFromZ[{Zobservable},ZCoupling,options,0]

\[Gamma]FunctionFromZ[Zobservable_,ZCoupling_,options:OptionsPattern[],LoopOrder_]:=
\[Gamma]FunctionFromZ[{Zobservable},ZCoupling,options,LoopOrder]



(* ::Title::Closed:: *)
(*\[Section] 1-Loop after MY simplification*)


(* ::Subsection::Closed:: *)
(*\[Section]\[Section] \[Beta] function*)


(* ::Subsubsection::Closed:: *)
(*\[Section]\[Section]\[Section] Splitting contributions: g, emitter Subscript[\[Gamma], 1] and absorber "Subscript[\[Gamma], 2]"*)


(* ::Item::Closed:: *)
(*with \[Beta]Function[] and Kay's approach*)


(* ::Input:: *)
(*Zgt=(Zg Z\[Gamma]1 Z\[Gamma]2)/Z\[Gamma]^2*)


(* ::Input:: *)
(*Zg=1+2 g0 \[Mu]^-\[Epsilon] banana;*)
(*Z\[Gamma]1=1+b g0 \[Mu]^-\[Epsilon] banana;*)
(*Z\[Gamma]2=1+(b-1) g0 \[Mu]^-\[Epsilon] banana;*)
(*Z\[Gamma]=1;*)
(**)
(*Zgt=(Zg Z\[Gamma]1 Z\[Gamma]2)/Z\[Gamma]^2;*)
(*Series[%,{g0,0,1}]*)


(* ::Input:: *)
(*(Zgt^(-1)/.g0->g0 Zgt^(-1))g0 \[Mu]^-\[Epsilon];*)
(*g=Series[%,{g0,0,2}]//Normal*)
(*(*g=Series[g0 \[Mu]^-\[Epsilon] Zgt^(-1),{g0,0,2}]//Normal*)*)
(*\[Beta]Function[g,"print"->True];*)
(*%/.banana ->1/\[Epsilon]/.doubleBanana ->1/\[Epsilon]^2/.hat ->1/(2\[Epsilon]^2)+1/(4\[Epsilon])/.sunset->-1/(8\[Epsilon])//FullSimplify//Factor*)
(*RGeq2=Simplify[Normal[%]]==0;*)


(* ::Item::Closed:: *)
(*Let's get the 1-Loop critical g**)


(* ::Input:: *)
(*Select[Flatten@Solve[RGeq2/.g^2->0,g],#[[2]]=!=0&]*)


(* ::Input:: *)
(*SolveValues[RGeq2/.g^2->0,g]*)


(* ::Input:: *)
(*gc1=SolveValues[RGeq2/.g^2->0,g][[2]]*)


(* ::Input:: *)
(*(* OK *)*)


(* ::Item::Closed:: *)
(*with \[Beta]FunctionFromZ*)


(* ::Input:: *)
(*Zg=1+2 g0 \[Mu]^-\[Epsilon] banana;*)
(*Z\[Gamma]1=1+b g0 \[Mu]^-\[Epsilon] banana;*)
(*Z\[Gamma]2=1+(b-1) g0 \[Mu]^-\[Epsilon] banana;*)
(*Z\[Gamma]=1;*)
(**)
(*Zgt=(Zg Z\[Gamma]1 Z\[Gamma]2)/Z\[Gamma]^2/.g0->g \[Mu]^\[Epsilon];*)
(*Simplify/@(Series[%/.banana ->1/\[Epsilon],{g,0,1}])*)
(**)
(*\[Beta]FunctionFromZ[Zgt,2]*)
(*%/.banana ->1/\[Epsilon]/.doubleBanana ->1/\[Epsilon]^2/.hat ->1/(2\[Epsilon]^2)+1/(4\[Epsilon])/.sunset->-1/(8\[Epsilon])//FullSimplify//Factor*)


(* ::Input:: *)
(*(*It works!!*)*)


(* ::Subsection::Closed:: *)
(*\[Section]\[Section] \[CapitalGamma]_1 observable *)


(* ::Subsubsection::Closed:: *)
(*\[Section]\[Section]\[Section] As it is*)


(* ::Input:: *)
(*gg=g0 \[Mu]^-\[Epsilon]-(2b+1)banana (g0 \[Mu]^-\[Epsilon])^2;*)
(**)
(*\[CapitalGamma]1=1-b g0 \[Mu]^-\[Epsilon] banana ;*)
(**)
(*\[Gamma]Function[\[CapitalGamma]1,gg,"print"->True]*)
(*%/.banana ->1/\[Epsilon]/.doubleBanana ->1/\[Epsilon]^2/.hat ->1/(2\[Epsilon]^2)+1/(4\[Epsilon])/.sunset->-1/(8\[Epsilon])//FullSimplify//Factor*)
(**)
(*Normal@%/.g->gc1//FullSimplify;*)
(*df=2+Collect[%,{\[Epsilon],\[Epsilon]^2},FullSimplify]/.\[Epsilon]^n_/;n>2:>0*)


(* ::Subsubsection::Closed:: *)
(*\[Section]\[Section]\[Section] After splitting the contributions*)


(* ::Item::Closed:: *)
(*with \[Gamma]Function[]*)


(* ::Input:: *)
(*gg=Series[g0 \[Mu]^-\[Epsilon] Zgt^(-1),{g0,0,2}]//Normal;*)
(**)
(*Z\[Gamma]1=1+b g0 \[Mu]^-\[Epsilon] banana;*)
(*\[CapitalGamma]1=1-b g0 \[Mu]^-\[Epsilon] banana ;*)
(**)
(*\[Gamma]Function[Z\[Gamma]1^-1,gg,"print"->False]*)
(*%/.banana ->1/\[Epsilon]/.doubleBanana ->1/\[Epsilon]^2/.hat ->1/(2\[Epsilon]^2)+1/(4\[Epsilon])/.sunset->-1/(8\[Epsilon])//FullSimplify//Factor*)
(**)
(*Normal@%/.g->gc1//FullSimplify;*)
(*df=2+Collect[%,{\[Epsilon],\[Epsilon]^2},FullSimplify]/.\[Epsilon]^n_/;n>2:>0*)


(* ::Item::Closed:: *)
(*Test with \[Gamma]FunctionFromZ*)


(* ::Input:: *)
(*Zg=1+2 g 1/\[Epsilon];*)
(*Z\[Gamma]1=1+b g 1/\[Epsilon];*)
(*Z\[Gamma]2=1+(b-1) g 1/\[Epsilon];*)
(**)
(*\[Gamma]FunctionFromZ[Z\[Gamma]1,Zg*Z\[Gamma]1*Z\[Gamma]2,0,"print"->True]*)


(* ::Input:: *)
(*(*Works!!*)*)


(* ::Subsection::Closed:: *)
(*\[Section]\[Section] \[CapitalGamma]_2 observable *)


(* ::Subsubsection::Closed:: *)
(*\[Section]\[Section]\[Section] After splitting the contributions*)


(* ::Input:: *)
(*gg=Series[g0 \[Mu]^-\[Epsilon] Zgt^(-1),{g0,0,2}]//Normal;*)
(**)
(*Z\[Gamma]2=1+(b-1) g0 \[Mu]^-\[Epsilon] banana;*)
(**)
(*\[Gamma]Function[Z\[Gamma]2^-1,gg,"print"->False]*)
(*%/.banana ->1/\[Epsilon]/.doubleBanana ->1/\[Epsilon]^2/.hat ->1/(2\[Epsilon]^2)+1/(4\[Epsilon])/.sunset->-1/(8\[Epsilon])//FullSimplify//Factor*)
(**)
(*Normal@%/.g->gc1//FullSimplify;*)
(*absorber=Collect[%,{\[Epsilon],\[Epsilon]^2},FullSimplify]/.\[Epsilon]^n_/;n>2:>0*)


(* ::Title:: *)
(*\[Section] 2-Loop after Simplification (partial: just the 1Loop has been done, but I want to see what happens if I update just the 1Loop term in g)*)
(*I CANNOT! IT IS NOT FINITE, I MUST GET THE 2LOOP TO CHECK!*)
(**)
(*IT SEEMS GOOD FOR*)
(*{GradImmediateIntNotAllowed :> 0, h -> 1, h2 -> 1, H -> 0, H2 -> 0, a2 -> 1 - a - 3/b, a -> 0, A2 -> 2 + 3/b - A, A -> 1}, 	WHY??*)


(* ::Input::Initialization:: *)
replaceDiagrams={banana ->1/\[Epsilon],doubleBanana ->1/\[Epsilon]^2,hat ->1/(2\[Epsilon]^2)+1/(4\[Epsilon]),sunset->-1/(8\[Epsilon])}

hideSubDivs={bananag->banana,banana\[Gamma]1->banana,banana\[Gamma]2->banana, banana\[Gamma]Paolo->banana,banana\[Gamma]Grad->banana,bananaMultigCT->banana,banana\[Gamma]PlusCT->banana,banana\[Gamma]PaoloCT->banana,banana\[Gamma]GradCT->banana,banana\[Gamma]MinusCT->banana,banana\[Gamma]2CT->banana,
doubleBananag->doubleBanana,doubleBanana\[Gamma]1g->doubleBanana,doubleBanana\[Gamma]Grad->doubleBanana,doubleBanana\[Gamma]Grad\[Gamma]2->doubleBanana,doubleBanana\[Gamma]Paolog-> doubleBanana,doubleBanana\[Gamma]2g-> doubleBanana,doubleBananaExtraGrad->doubleBanana,doubleBananaGradMultig->doubleBanana,doubleBananaGrad\[Gamma]Plus->doubleBanana,doubleBananaGrad\[Gamma]PlusNOsub->doubleBanana,doubleBananaGrad\[Gamma]2->doubleBanana,doubleBananaGrad\[Gamma]2NOsub->doubleBanana,
hatg->hat,hat\[Gamma]1->hat,hat\[Gamma]2->hat,hatg\[Gamma]1->hat, hat\[Gamma]1g->hat,hatg\[Gamma]2\[Gamma]1->hat,hat\[Gamma]Paolo->hat,hat\[Gamma]Grad->hat,hat\[Gamma]2g ->hat,hatg\[Gamma]2 ->hat,hat\[Gamma]1\[Gamma]2 ->hat,hat\[Gamma]Paolo\[Gamma]1 ->hat,hat\[Gamma]Paolo\[Gamma]2->hat,hat\[Gamma]Paolog ->hat,hat\[Gamma]Paolo\[Gamma]2g->hat,hatExtraGrad->hat,hatGradMultig->hat,hatMultigGrad->hat,hatGrad\[Gamma]Plus->hat,hatGrad\[Gamma]PlusNOsub->hat,hatMultig\[Gamma]Paolo->hat,hatGrad\[Gamma]2NOsub->hat}


(* ::Chapter::Closed:: *)
(*\[Section]\[Section] 2loop b=1*)


(* ::Section::Closed:: *)
(*\[Section]\[Section]\[Section] \[Beta]-function After splitting the contributions: b=1*)


(* ::Subsection::Closed:: *)
(*Here I just replace Z_gt by Z_g*Z_\[Gamma]1, but this should be the wrong way to compute \[Beta]... However the result is correct*)
(*I FINALLY MADE UP MY MIND AND CONVINCED MYSELF THAT THIS IS CORRECT*)


(* ::Input:: *)
(*Zg=1+2 g 1/\[Epsilon]-g^2 (-7/\[Epsilon]^2 5/7+5/(2\[Epsilon]));(*a=5/7*)*)
(**)
(*Z\[Gamma]1=1+ g 1/\[Epsilon]-g^2 (-2/\[Epsilon]^2+1/(2\[Epsilon]));*)
(**)
(*loopOrder=2;*)
(**)
(*Zgt=Zg Z\[Gamma]1 /.z[_]->1;*)
(*Series[Zgt,{g,0,loopOrder}]//FS//Normal*)
(*Series[%,{\[Epsilon],0,0}]//FS//Normal;*)
(**)
(*\[Beta]FunctionFromZ[Series[Zgt,{g,0,loopOrder}]//FS//Normal,loopOrder+1]*)
(*RGeq2=Simplify[Normal[%]]==0;*)


(* ::Subsection::Closed:: *)
(*Using Kay's approach*)


(* ::Item::Closed:: *)
(*No contribution splitting*)


(* ::Input:: *)
(*Zg=g0 \[Mu]^-\[Epsilon]-(b+2)banana (g0 \[Mu]^-\[Epsilon])^2+(g0 \[Mu]^-\[Epsilon])^3 ( (b+2)( doubleBanana + 2(b+1) hat))/.b->1;*)
(**)
(*Series[%,{g0,0,3}]*)


(* ::Input:: *)
(*g=Series[Zg,{g0,0,3}]//Normal*)
(*(*g=Series[g0 \[Mu]^-\[Epsilon] Zgt^(-1),{g0,0,2}]//Normal*)*)
(*\[Beta]Function[g,"print"->tTrue]*)
(*%/.banana ->1/\[Epsilon]/.doubleBanana ->1/\[Epsilon]^2/.hat ->1/(2\[Epsilon]^2)+1/(4\[Epsilon])/.sunset->-1/(8\[Epsilon])//FullSimplify//Factor*)
(*RGeq2=Simplify[Normal[%]]==0;*)


(* ::Input:: *)
(*(*Correct!*)*)


(* ::Item::Closed:: *)
(*Splitting the contributions*)


(* ::Input:: *)
(*Zg=g0 \[Mu]^-\[Epsilon]-(g0 \[Mu]^-\[Epsilon])^2 2*(bananag)+(g0 \[Mu]^-\[Epsilon])^3 ( 2 doubleBananag + 4 hatg+4 hat\[Gamma]1g +2 hatg\[Gamma]1 );(*a=5/7*)*)
(**)
(*Z\[Gamma]1=- (g0 \[Mu]^-\[Epsilon]) banana\[Gamma]1+(g0 \[Mu]^-\[Epsilon])^2 ( doubleBanana\[Gamma]1g + hatg\[Gamma]1+hat\[Gamma]1);*)
(**)
(*loopOrder=2;*)
(**)
(*Zg+g0 \[Mu]^-\[Epsilon] Z\[Gamma]1 ;*)
(*Series[%,{g0,0,loopOrder+1}]*)


(* ::Input:: *)
(*g=Series[Zg+g0 \[Mu]^-\[Epsilon] Z\[Gamma]1 ,{g0,0,loopOrder+1}]//Normal*)
(*(*g=Series[g0 \[Mu]^-\[Epsilon] Zgt^(-1),{g0,0,2}]//Normal*)*)
(*\[Beta]Function[g,"print"->tTrue]*)
(**)
(*%/.hideSubDivs *)
(*%/.replaceDiagrams//FullSimplify//Factor*)
(*RGeq2=Simplify[Normal[%]]==0;*)


(* ::Input:: *)
(*(*Correct!*)*)


(* ::Subsection::Closed:: *)
(*Let's get the 2-Loop critical g*	*)


(* ::Input:: *)
(*gc2=gc1+B \[Epsilon]^2/.b->1;*)
(*RGeq2/.g->gc2;*)
(*Series[%,{\[Epsilon],0,3}];*)
(*Flatten@Solve[Normal[%],B]//FS*)
(*gc2=(gc2/.%)//FullSimplify;*)
(*gc2=Collect[Expand@gc2,\[Epsilon],Simplify]*)


(* ::Input:: *)
(*(* CORRECT !!!*)*)


(* ::Section::Closed:: *)
(*\[Section]\[Section]\[Section] \[Gamma]-functions After splitting the contributions: b=1*)


(* ::Subsection::Closed:: *)
(*\[CapitalGamma]\[Gamma]1*)


(* ::Input:: *)
(*loopOrder=2;*)
(**)
(*g=Normal[Series[Zg+g0 \[Mu]^-\[Epsilon] Z\[Gamma]1,{g0,0,loopOrder+1}]]/. bananag->banana/. banana\[Gamma]1->banana/. doubleBananag->doubleBanana/. doubleBanana\[Gamma]1g->doubleBanana/. hatg->hat/. hat\[Gamma]1->hat/. hatg\[Gamma]1->hat/. hat\[Gamma]1g->hat*)
(**)
(*(1+Z\[Gamma]1)/(\[CapitalGamma]\[Gamma])^0/.z[_]->1/. bananag->banana/. banana\[Gamma]1->banana/. doubleBananag->doubleBanana/. doubleBanana\[Gamma]1g->doubleBanana/. hatg->hat/. hat\[Gamma]1->hat/. hatg\[Gamma]1->hat/. hat\[Gamma]1g->hat;*)
(**)
(*\[Gamma]Function[%,g,"print"->True]*)
(*%/.banana ->1/\[Epsilon]/.doubleBanana ->1/\[Epsilon]^2/.hat ->1/(2\[Epsilon]^2)+1/(4\[Epsilon])/.sunset->-1/(8\[Epsilon])//FullSimplify//Factor*)
(*%/.g->gc2+O[\[Epsilon]]^3*)


(* ::Input:: *)
(*(*Correct!*)*)


(* ::Chapter:: *)
(*\[Section]\[Section] 2loop b>1*)


(* ::Section:: *)
(*Rewritten to split into Zg, Z\[Gamma]1, Z\[Gamma]2*)


(* ::Input::Initialization:: *)
(* REFERENCE, DO NOT TOUCH *)
goodGuys=-b^3 (2 doubleBanana + 4 hat +4 hat + 2 hat)-b^2(doubleBanana +2 b hat)*2-b^3(6 hat + doubleBanana);

realNasties=b(b-1)(4 doubleBanana + 8 hat )+b(b-1)2 hat - b(b-1)(2  doubleBanana +4 hat )- b(b-1)( 4 doubleBanana);

betterNasties=b^2(b-1)6 hat+b^2(b-1)(2 doubleBanana +4 hat)+b^2(b-1)(2 doubleBanana)+b^2(b-1)(2 hat);

gammagGuys=b(2 hat + 2 hat) + b^2 doubleBanana +4 b^2 hat + b^2 doubleBanana;


(* ::Input:: *)
(*ClearAll[h,GradImmediateIntNotAllowed]*)


(* ::Input:: *)
(*GradImmediateIntNotAllowed/:(GradImmediateIntNotAllowed->0):={GradImmediateIntNotAllowed:>0,h->1,h2->1}*)


(* ::Input::Initialization:: *)
(* IN WHAT FOLLOWS, I SUB doubleBanana-> MINUS 1/\[Epsilon]^2. SO HERE I NEED TO SUM THE BANANA SQUARED. Actually, the replacement ALREADY implements the partial subtraction of subdivergencies *)

(* GradImmediateIntNotAllowed=0 then it is not allowed. To implement it also for \[Gamma]1 and \[Gamma]2, one should set h,h2->1*)
GradImmediateIntNotAllowed/:(GradImmediateIntNotAllowed->0):={GradImmediateIntNotAllowed:>0,h->1,h2->1,H->0}

twoLoopZ\[Gamma]1=1/b (-b^2 doubleBanana-2 b^3 hat+(1/2 b^2 (b-1)(banana)^2(* From \[CapitalGamma]Grad counterterm*))- b^2 (b-1)(a doubleBanana+h hat) (*If not all the \[CapitalGamma]grad can be used*));/.h->-1;

twoLoopZ\[Gamma]2=1/b (-b^2 doubleBanana-2 b^3 hat +(1/2 b^2 (b-1)(banana)^2(* From \[CapitalGamma]Grad counterterm*)) +b^2 (doubleBanana+1/2 (banana)^2(* From \[CapitalGamma]paoloG counterterm*))+2 b (hat +1/2 (banana)^2(* From \[CapitalGamma]paoloG counterterm*))- b^2 (b-1)(a2 doubleBanana+h2 hat) (*If not all the \[CapitalGamma]grad can be used*));/.h2->-1(*(2 b hat-2 b^3 hat)/b*)(*/.hat->(hat+1/4(banana)^2(* From \[CapitalGamma]Grad counterterm*))*)

twoLoopZg=(-b^3 (2 doubleBananag + 4 hatg +4 hat\[Gamma]1g+2 hatg\[Gamma]1 + 6 hat\[Gamma]2)(*-b^3 (doubleBanana+6 hat)-b^3 (2 doubleBanana+10 hat)-2 b^2 (doubleBanana+2 b hat)(*goodGuys*)+2b^2(doubleBanana +2 b hat)(*Moved to Z\[Gamma]1 and Z\[Gamma]2 (thus subtracted here) *)+b^3( doubleBanana)(*Should arise from the 1loops of Z\[Gamma]1*Z\[Gamma]2 (thus subtracted here) *)*)
+(*realNasties modified	.*)
(* ONLY \[Gamma]Grad 1) in my notes*)b(b-1)(4 (doubleBanana+(banana)^2(* From \[CapitalGamma]Grad counterterm*)) + 8 (hat+1/2 (banana)^2(* From \[CapitalGamma]Grad counterterm*)) )
+(* ONLY \[Gamma]Grad 3) in my notes*)
b(b-1)2 (hat+1/2 (banana)^2(* From \[CapitalGamma]Grad counterterm*)) GradImmediateIntNotAllowed
-  (* \[Gamma]Grad + \[Gamma]Plus 1) in my notes *)
b(b-1)(2  (doubleBanana+(banana)^2(* From \[CapitalGamma]Grad counterterm*)) +4 (hat+1/2 (banana)^2(* From \[CapitalGamma]Grad counterterm*)) )
-(* \[Gamma]Grad + \[Gamma]Plus 2) in my notes *)
 b(b-1)( 4 (doubleBanana+(banana)^2(* From \[CapitalGamma]Grad counterterm*)))
+(*betterNasties modified*)
(* ONLY \[Gamma]Grad 2) in my notes*)
b^2(b-1)6 (hat+1/2 (banana)^2(* From \[CapitalGamma]Grad counterterm*))
+(* \[Gamma]Grad + \[Gamma]Minus 1) in my notes *)
b^2(b-1)(2 (doubleBanana+(banana)^2(* From \[CapitalGamma]Grad counterterm*)) +4 (hat+1/2 (banana)^2(* From \[CapitalGamma]Grad counterterm*)))
+(* \[Gamma]Grad + \[Gamma]Minus 2) in my notes *)
b^2(b-1)(2 (doubleBanana+(banana)^2(* From \[CapitalGamma]Grad counterterm*)))
+(* \[Gamma]Grad + \[Gamma]Minus 2) in my notes (continues) *)
b^2(b-1)(2 (hat+1/2 (banana)^2(* From \[CapitalGamma]Grad counterterm*))) GradImmediateIntNotAllowed
+(*gammagGuys modified*)
(gammagGuys -(b(2 hat )+ b^2 doubleBanana (*Should arise from the 1loops of Z\[Gamma]1*Z\[Gamma]2 *))- b^2 doubleBanana (*Moved to Z\[Gamma]2 *))(*b(2 hat )  +4 b^2 hat *))/b; 
(*Here I'm missing the subdiv from the grad vertex. Try to remove them by hand see if the rest is finite*)

twoLoopZ\[Gamma]=(b(b-1))/2 ( sunset + (hat+1/2 (banana)^2(* From \[CapitalGamma]Grad counterterm*)))GradImmediateIntNotAllowed/b;


(* ::Input::Initialization:: *)
(*This is Probably necessarely *)
twoLoopZ\[Gamma]1=twoLoopZ\[Gamma]1/.(banana)^2->0(banana)^2/2;
twoLoopZg=twoLoopZg/.(banana)^2->0(banana)^2/2;
twoLoopZ\[Gamma]2=twoLoopZ\[Gamma]2/.(banana)^2->0(banana)^2/2;
twoLoopZ\[Gamma]=twoLoopZ\[Gamma]/.(banana)^2->2(banana)^2/2;


(* ::Subitem::Closed:: *)
(*Check:*)


(* ::Input:: *)
(*twoLoopZg+twoLoopZ\[Gamma]1+twoLoopZ\[Gamma]2;*)
(*FS[%*b+(- goodGuys- gammagGuys-betterNasties - realNasties)]/.banana->0*)


(* ::Section:: *)
(*\[Section]\[Section]\[Section] After splitting the contributions: b>1  THE Z HERE COULD ACTUALLY BE Z^-1*)


(* ::Item::Closed:: *)
(*Using \[Beta]FunctionFromZ[]*)


(* ::Input:: *)
(*Zg=1+2 g0 \[Mu]^-\[Epsilon] banana+z[g](g0 \[Mu]^-\[Epsilon])^2 twoLoopZg;*)
(*%/.b->1;*)
(*Z\[Gamma]1=Simplify/@(1+b g0 \[Mu]^-\[Epsilon] banana +z[\[Gamma]1](g0 \[Mu]^-\[Epsilon])^2(twoLoopZ\[Gamma]1(*+(b+b^2)(banana)^2*)(*TO REMOVE THE SUB.DIVS*)));*)
(*%/.b->1;*)
(*Z\[Gamma]2=1+(b-1) g0 \[Mu]^-\[Epsilon] banana-z[\[Gamma]2](g0 \[Mu]^-\[Epsilon])^2 twoLoopZ\[Gamma]2;*)
(**)
(*Z\[Gamma]=1+z[\[Gamma]](g0 \[Mu]^-\[Epsilon])^2/b (b(b-1))/2 ( sunset + (hat-1/2 (banana)^2(* From \[CapitalGamma]Grad counterterm*)));*)
(**)
(*Zgt=(Zg Z\[Gamma]1 Z\[Gamma]2)/Z\[Gamma]^2/.z[_]->1;*)
(*Series[Zgt,{g0,0,2}];*)
(*%/.b->1//FS;*)
(**)
(*Series[(Zgt/.g0->g0 Zgt)^(-1),{g0,0,2}];*)
(*%/.b->1//FS;*)
(**)
(*Zgt/.g0-> g \[Mu]^\[Epsilon];*)
(*%/.g->g %;*)
(*Series[%,{g,0,2}]//Normal*)


(* ::Input:: *)
(*Zgt/.g0-> g \[Mu]^\[Epsilon];*)
(*%/.g->g %;*)
(*Series[%,{g,0,2}]//Normal;*)
(*\[Beta]FunctionFromZ[%,3]*)
(*%/.banana ->1/\[Epsilon]/.doubleBanana ->1/\[Epsilon]^2/.hat ->1/(2\[Epsilon]^2)+1/(4\[Epsilon])/.sunset->-1/(8\[Epsilon])//FullSimplify//Factor*)


(* ::Subsection::Closed:: *)
(*Using \[Beta]FunctionFromZ[] 	WITH CT-IN-CT TERMS SUBTRACTED*)


(* ::Subsubsection::Closed:: *)
(*b=1 for comparison(without CT-in-CT subtraction)*)


(* ::Input:: *)
(*Z\[Gamma]1=1+ g 1/\[Epsilon]-g^2 (-2/\[Epsilon]^2+1/(2\[Epsilon]));*)
(**)
(*Zg=1+2 g 1/\[Epsilon]-g^2 (-7/\[Epsilon]^2(*5/7*)+5/(2\[Epsilon]));(*a=5/7*)*)
(**)
(**)
(*Zgt=Zg Z\[Gamma]1 /.z[_]->1;*)
(*Series[Zgt,{g,0,2}]//FS//Normal*)


(* ::Subsubsection::Closed:: *)
(*b>1*)


(* ::Input:: *)
(*(*This is a guess, to check if \[CapitalGamma]grad is needed *)*)
(*twoLoopZ\[Gamma]1=twoLoopZ\[Gamma]1/.(b-1)->0;*)
(*twoLoopZg=twoLoopZg/.(b-1)->0;*)
(*twoLoopZ\[Gamma]2=twoLoopZ\[Gamma]2/.(b-1)->0;*)
(*twoLoopZ\[Gamma]=twoLoopZ\[Gamma]/.(b-1)->0;*)


(* ::Input:: *)
(*Z\[Gamma]1=Simplify/@(1-b g0 \[Mu]^-\[Epsilon] banana -z["\[Gamma]1"](g0 \[Mu]^-\[Epsilon])^2(twoLoopZ\[Gamma]1(*+(b+b^2)(banana)^2*)(*TO REMOVE THE SUB.DIVS*)));*)
(*%/.g0-> g \[Mu]^\[Epsilon];*)
(*%/.g->g %^(1);*)
(*Series[%^(-1),{g,0,2}];*)
(*%/.banana ->1/\[Epsilon]/.doubleBanana ->-1/\[Epsilon]^2/.hat ->-1/(2\[Epsilon]^2)+1/(4\[Epsilon])/.sunset->-1/(8\[Epsilon]);*)
(*%/.b->1/.z[_]->1//Map[Expand,#]&*)
(*Z\[Gamma]1Inv=Expand/@%%;*)
(*Print["Actual Z\[Gamma]1 = ",1+ g 1/\[Epsilon],"-",g^2 (-2/\[Epsilon]^2+1/(2\[Epsilon])) ];*)
(**)
(**)
(*Zg=1-2 g0 \[Mu]^-\[Epsilon] banana-z["g"](g0 \[Mu]^-\[Epsilon])^2 twoLoopZg;*)
(*%/.g0-> g \[Mu]^\[Epsilon];*)
(*%/.g->g %^(1);*)
(*Series[%^(-1),{g,0,2}];*)
(*%/.banana ->1/\[Epsilon]/.doubleBanana ->-1/\[Epsilon]^2/.hat ->-1/(2\[Epsilon]^2)+1/(4\[Epsilon])/.sunset->-1/(8\[Epsilon]);*)
(*%/.b->1/.z[_]->1//Map[Expand,#]&*)
(*ZgInv=Expand/@%%;*)
(*Print["Actual Zg without subtraction = ",1+2 g 1/\[Epsilon],"-",g^2 (-7/\[Epsilon]^2(*5/7*)+5/(2\[Epsilon])) ];*)
(**)
(**)
(**)
(*Z\[Gamma]2=1-(b-1) g0 \[Mu]^-\[Epsilon] banana-z["\[Gamma]2"](g0 \[Mu]^-\[Epsilon])^2 twoLoopZ\[Gamma]2;*)
(*%/.g0-> g \[Mu]^\[Epsilon];*)
(*%/.g->g %^(1);*)
(*Series[%^(-1),{g,0,2}];*)
(*%/.banana ->1/\[Epsilon]/.doubleBanana ->-1/\[Epsilon]^2/.hat ->-1/(2\[Epsilon]^2)+1/(4\[Epsilon])/.sunset->-1/(8\[Epsilon]);*)
(*%/.b->1/.z[_]->1//Map[Expand,#]&*)
(*Z\[Gamma]2Inv=Expand/@%%;*)
(*Print["Actual Z\[Gamma]2  = ",1];*)
(**)
(*Z\[Gamma]=1-z["\[Gamma]"](g0 \[Mu]^-\[Epsilon])^2 twoLoopZ\[Gamma];*)
(*%/.g0-> g \[Mu]^\[Epsilon];*)
(*%/.g->g %^(1);*)
(*Series[%^(-1),{g,0,2}];*)
(*%/.banana ->1/\[Epsilon]/.doubleBanana ->-1/\[Epsilon]^2/.hat ->-1/(2\[Epsilon]^2)+1/(4\[Epsilon])/.sunset->-1/(8\[Epsilon]);*)
(*%/. b->1/. z[_]->1//Map[Expand,#]&*)
(*Z\[Gamma]Inv=Expand/@%%;*)
(*Print["Actual Z\[Gamma]  = ",1];*)
(**)
(*Zgt=((Zg Z\[Gamma]1 Z\[Gamma]2)/Z\[Gamma]^2(*-(g0 \[Mu]^-\[Epsilon])^2 ((2 b)/\[Epsilon]^2+(2 b(b-1))/\[Epsilon]^2)*))/.z[_]->1;*)
(*Series[Zgt,{g0,0,2}];*)
(**)
(*Zgt/.g0-> g \[Mu]^\[Epsilon];*)
(*%/.g->g %^(1);*)
(*Series[%^(-1),{g,0,2}];*)
(*%/.banana ->1/\[Epsilon]/.doubleBanana ->-1/\[Epsilon]^2/.hat ->-1/(2\[Epsilon]^2)+1/(4\[Epsilon])/.sunset->-1/(8\[Epsilon])//FullSimplify//Factor;*)
(*%/.b->1//FS;*)
(**)
(**)
(*ZgtInv=FS/@(((Z\[Gamma]1Inv *ZgInv *Z\[Gamma]2Inv)/Z\[Gamma]Inv^2/.z[_]->1)-g^2 ((2 b)/\[Epsilon]^2(*b*)+(2 (b-1))/\[Epsilon]^2(*b*)+(b(**b*)(b-1))/\[Epsilon]^2)(*The CT-in-CT terms. The extra b in the first two terms comes from splitting Subscript[c, g] into contributions from \[Gamma]Grad or not (only not=extra b)*)-(g^2(-1+b) b (-1/\[Epsilon]^2))(*Terms I left out but that I could need to count instead*));*)
(*(*ZgtInv=ZgtInv/.g->g ZgtInv^(1);*)*)
(**)
(*FS/@(ZgtInv/.b->1)*)
(*Print["Actual b=1 without CT-CT subtraction: ", 1+(3 g)/\[Epsilon],"+",(g^2 (11-3 \[Epsilon]))/\[Epsilon]^2]*)
(*Print["Actual b=1 with: ", 1+(3 g)/\[Epsilon],"+",(g^2 (9-3 \[Epsilon]))/\[Epsilon]^2]*)
(**)


(* ::Input:: *)
(*replaceRule={a->0,h->1,h2->1,a2->-(1/b),l->3/2}*)
(*replaceRule={a2->-3/(2b),a->-3/(2b),GradImmediateIntNotAllowed->0, h->1,h2->1}*)


(* ::Input:: *)
(*Series[ZgtInv,{g,0,2}]//Normal*)
(*\[Beta]FunctionFromZ[ZgtInv/.{a->a,h->1h},3]*)
(*(*%/.banana ->1/\[Epsilon]/.doubleBanana ->1/\[Epsilon]^2/.hat ->1/(2\[Epsilon]^2)+1/(4\[Epsilon])/.sunset->-1/(8\[Epsilon])//FullSimplify//Factor;*)*)
(*%/.replaceRule//FS*)
(*%/.b->1*)


(* ::Input:: *)
(*(-(1/4)+(9 b)/4+4 b^2+GradImmediateIntNotAllowed-b GradImmediateIntNotAllowed-(b h)/2+(b^2 h)/2-(b h2)/2+(b^2 h2)/2+6/\[Epsilon]-(8 b)/\[Epsilon]+(2 a b)/\[Epsilon]+(2 a2 b)/\[Epsilon]+(2 b^2)/\[Epsilon]-(2 a b^2)/\[Epsilon]-(2 a2 b^2)/\[Epsilon]-(2 GradImmediateIntNotAllowed)/\[Epsilon]+(2 b GradImmediateIntNotAllowed)/\[Epsilon]+(b h)/\[Epsilon]-(b^2 h)/\[Epsilon]+(b h2)/\[Epsilon]-(b^2 h2)/\[Epsilon])/.{GradImmediateIntNotAllowed->0, h->1,h2->1}//FS*)


(* ::Input:: *)
(*(3+(a+a2) b)/.a2->-3/(2b)/.a->-3/(2b)//FS*)


(* ::Item::Closed:: *)
(*Various attempts to make it finite*)


(* ::Input:: *)
(*Collect[Expand@(-2+4 \[Epsilon]+b (10+b (-8+\[Epsilon])+\[Epsilon])),{\[Epsilon]},FS];*)
(*Collect[Expand@(8 (-8+b) (-1+b)),{\[Epsilon]},FS];*)
(*Collect[Expand@(4/\[Epsilon]-(6 b)/\[Epsilon]-(4 a b)/\[Epsilon]+(2 b^2)/\[Epsilon]+(4 a b^2)/\[Epsilon]-(2 b h)/\[Epsilon]+(2 b^2 h)/\[Epsilon]),{\[Epsilon]},FS];*)
(*Collect[Expand@(-((b h)/2)+(b^2 h)/2-(b h2)/2+(b^2 h2)/2+8/\[Epsilon]-(10 b)/\[Epsilon]+(2 a b)/\[Epsilon]+(2 a2 b)/\[Epsilon]+(2 b^2)/\[Epsilon]-(2 a b^2)/\[Epsilon]-(2 a2 b^2)/\[Epsilon]+(b h)/\[Epsilon]-(b^2 h)/\[Epsilon]+(b h2)/\[Epsilon]-(b^2 h2)/\[Epsilon]-(4 l)/\[Epsilon]+(4 b l)/\[Epsilon]),{\[Epsilon]},FS];*)
(*Collect[Expand@(8/\[Epsilon]-(18 b)/\[Epsilon]+(2 a b)/\[Epsilon]+(2 a2 b)/\[Epsilon]+(10 b^2)/\[Epsilon]-(2 a b^2)/\[Epsilon]-(2 a2 b^2)/\[Epsilon]+(b h)/\[Epsilon]-(b^2 h)/\[Epsilon]+(b h2)/\[Epsilon]-(b^2 h2)/\[Epsilon]),{\[Epsilon]},FS]*)
(*Solve[%==0,l]*)
(*%/.a->0*)
(*%/.h->1*)


(* ::Item::Closed:: *)
(*Closer inspection of the Zinv*)


(* ::Input:: *)
(*Z\[Gamma]1Inv /.z[_]->1//Collect[#,{g,\[Epsilon]},FS]&*)
(*ZgInv/.z[_]->1//Collect[#,{g,\[Epsilon]},FS]&*)
(*Z\[Gamma]2Inv/.z[_]->1//Collect[#,{g,\[Epsilon]},FS]&*)
(*Z\[Gamma]Inv/.z[_]->1//Collect[#,{g,\[Epsilon]},FS]&*)
(**)


(* ::Subsubsection::Closed:: *)
(*Some RG functions*)


(* ::Subitem::Closed:: *)
(*Z\[Gamma]1Inv*)


(* ::Input:: *)
(*replaceRule={a->0,h->1,h2->1 ,a2->-(1/b),l->3/2};*)
(*replaceRule={a2->-(3/(2 b)),a->-(3/(2 b)),GradImmediateIntNotAllowed->0,h->1,h2->1};*)
(*Z\[Gamma]1Inv/Z\[Gamma]Inv^0/.z[_]->1//FS;*)
(*obsWithoutZ=\[Gamma]FunctionFromZ[%/.replaceRule,ZgtInv/.replaceRule,"print"->True,"gstar"->True]*)
(**)
(*0*)
(*{Z\[Gamma]1Inv*Z\[Gamma]Inv^-1}/.z[_]->1//FS;*)
(*obsWithZ=\[Gamma]FunctionFromZ[%/.replaceRule,ZgtInv/.replaceRule,"print"->True,"gstar"->True]*)
(**)


(* ::Input:: *)
(*(24+\[Epsilon]+3 b (-8+\[Epsilon]-4 b \[Epsilon]))//Collect[#,\[Epsilon]]&*)


(* ::Subitem::Closed:: *)
(*Z\[Gamma]2Inv*)


(* ::Input:: *)
(*Z\[Gamma]2Inv/Z\[Gamma]Inv^0/.z[_]->1//FS*)
(*\[Gamma]FunctionFromZ[%,ZgtInv/.{a->0,h->1(*,l\[Rule]1/8 (16-2 b+4 a2 b+2 b h2-b \[Epsilon]-b h2 \[Epsilon])*)},"print"->True,"gstar"->tTrue]*)
(**)


(* ::Input:: *)
(*(-1+b)(2 (-2+\[Epsilon])+b (2-4 a2-2 h2+2 \[Epsilon]+h2 \[Epsilon])) /.\[Epsilon]->0*)
(*+(8/\[Epsilon])-(9 b)/\[Epsilon]+(2 a2 b)/\[Epsilon]+b^2/\[Epsilon]-(2 a2 b^2)/\[Epsilon]+(b h2)/\[Epsilon]-(b^2 h2)/\[Epsilon]-(4 l)/\[Epsilon]+(4 b l)/\[Epsilon]*)
(*Solve[{%%==0,%==0},{a2,l}]//FS*)
(*%/.h2->1*)


(* ::Input:: *)
(*replaceRule ={a->0,h->1,h2->1 ,a2->-(1/b),l->3/2};*)
(*Z\[Gamma]2Inv/Z\[Gamma]Inv^0/. z[_]->1//FS*)
(*\[Gamma]FunctionFromZ[%/.replaceRule,ZgtInv/. replaceRule,"print"->True,"gstar"->True]*)
(**)


(* ::Subitem::Closed:: *)
(*Z\[Gamma]Inv*)


(* ::Input:: *)
(*replaceRule={a->0,h->1,h2->1 ,a2->-(1/b),l->3/2};*)
(*Z\[Gamma]Inv/.z[_]->1//FS*)
(*\[Eta]=\[Gamma]FunctionFromZ[%/.replaceRule,ZgtInv/.replaceRule,"print"->True,"gstar"->True]*)
(**)


(* ::Subitem::Closed:: *)
(*a,h and l to make them finite*)


(* ::Input:: *)
(*\[Beta]FunctionFromZ[ZgtInv/. {a->a,h->1 h,z[_]->1},3]*)
(*\[Gamma]FunctionFromZ[Z\[Gamma]1Inv/. {a->a,h->1 h,z[_]->1},ZgtInv/. {a->a,h->1 h},"print"->tTrue,"gstar"->tTrue]*)
(*\[Gamma]FunctionFromZ[Z\[Gamma]2Inv/. {a->a,h->1 h,z[_]->1},ZgtInv/. {a->a,h->1 h},"print"->tTrue,"gstar"->tTrue]*)


(* ::Input:: *)
(*Solve[{8/\[Epsilon]-(10 b)/\[Epsilon]+(4 a b)/\[Epsilon]+(2 b^2)/\[Epsilon]-(4 a b^2)/\[Epsilon]+(2 b h)/\[Epsilon]-(2 b^2 h)/\[Epsilon]-(4 l)/\[Epsilon]+(4 b l)/\[Epsilon]==0,(-1+b) b (-1+2 a+h)==0,(-1+b) (2+b (-1+2 a+h))==0},{a,h,l}]*)
(*{8/\[Epsilon]-(10 b)/\[Epsilon]+(4 a b)/\[Epsilon]+(2 b^2)/\[Epsilon]-(4 a b^2)/\[Epsilon]+(2 b h)/\[Epsilon]-(2 b^2 h)/\[Epsilon]-(4 l)/\[Epsilon]+(4 b l)/\[Epsilon]==0,(-1+b) b (-1+2 a+h)==0,(-1+b) (2+b (-1+2 a+h))==0}/.{a->0,h->1 ,l->2}*)


(* ::Subsubsection::Closed:: *)
(*df ???*)


(* ::Input:: *)
(*fractalDim=Collect[2+(obsWithZ//FS)-\[Eta],\[Epsilon],FS]*)
(*{%/.b->0/.\[Epsilon]->2,Limit[%,b->\[Infinity]]/.\[Epsilon]->2}*)
(**)
(*Collect[2+(obsWithZ//FS)+\[Eta],\[Epsilon],FS]*)
(*{%/.b->0/.\[Epsilon]->2,Limit[%,b->\[Infinity]]/.\[Epsilon]->2}*)
(**)
(*fractalDim=Collect[2+(obsWithZ//FS),\[Epsilon],FS]*)
(*{%/.b->0/.\[Epsilon]->2,Limit[%,b->\[Infinity]]/.\[Epsilon]->2}*)
(**)
(**)
(*Collect[2+(obsWithoutZ//FS),\[Epsilon],FS]*)
(*{%/.b->0/.\[Epsilon]->2,Limit[%,b->\[Infinity]]/.\[Epsilon]->2}*)
(**)


(* ::Subsection:: *)
(*using \[Beta]Function[] and Kay's approach*)


(* ::Subsubsection:: *)
(*Splitting the contributions*)


(* ::Text:: *)
(*To make sense of this we differentiate the diagrams according to the subdivergences*)


(* ::Text:: *)
(*1) I AM STILL MISSING THE CT FOR THE DELAYED RED-GREEN*)
(*2) THE "ABSORBER" IS NOT A REAL OBS OF THE THEORY: IT DISAPPEARS AFTER INTEGATING \[Psi] OUT (nor is the emitter for the interaction, for that matter. The remaining emitter is useful only for the pass-through-a-point obs) CORRIGE: IT IS, AND ITS RG FUNCTION IS INDEED FINITE: THE PROBLEM CAME FROM SOME EXTRA banana\[Gamma]Paolo^2 THAT ARE NOT SUPPOSED TO BE THERE*)


(* ::Input::Initialization:: *)
replaceRule={GradImmediateIntNotAllowed:>0,h->1,h2->1,H->0,H2->0,a2->1-a-3/b,a->0,A2->1(*2+3/b-A*),A->1(*,K->1+3b/2*)};


(* ::Input:: *)
(*(*Logic change: I write A and H in front of the diagrams we obtain from the Grad term. Before, we used a and h to subtract these terms from the complete expression.*)*)
(*\[CapitalGamma]\[Gamma]1small =(-b banana g0 \[Mu]^-\[Epsilon]+b g0^2 (b doubleBanana\[Gamma]1g-(b-1) doubleBanana\[Gamma]Grad+a Hold[b-1] doubleBanana\[Gamma]Grad-h hat+b (2+h) hat) \[Mu]^(-2 \[Epsilon]) z["\[Gamma]1"]);*)
(**)
(*\[CapitalGamma]\[Gamma]1 =(-b banana\[Gamma]1 g0 \[Mu]^-\[Epsilon]+ g0^2 \[Mu]^(-2 \[Epsilon]) b(b doubleBanana\[Gamma]1g- A Hold[b-1]doubleBanana\[Gamma]Grad +(b hatg\[Gamma]1+b hat\[Gamma]1+b hatg\[Gamma]2\[Gamma]1-hat\[Gamma]Paolo\[Gamma]1)-H Hold[b-1]hat\[Gamma]Grad)  z["\[Gamma]1"]);*)
(*PPrint[{%," = "},%,"\n"]*)
(**)
(*(*Logic change: I write A2 and H2 in front of the diagrams we obtain from the Grad term. Before,we used a2 and h2 to subtract these terms from the complete expression.*)*)
(*\[CapitalGamma]\[Gamma]2small = - g0 \[Mu]^-\[Epsilon] (b-1)banana + g0^2 \[Mu]^(-2 \[Epsilon]) (2 b^2 hat - 2 hat + b(b-1)(a2 doubleBanana+h2 hat))z["\[Gamma]2"];*)
(**)
(*\[CapitalGamma]\[Gamma]2 =(-(b banana\[Gamma]2 - banana\[Gamma]Paolo)  g0 \[Mu]^-\[Epsilon]+g0^2 \[Mu]^(-2 \[Epsilon]) (b^2 doubleBanana\[Gamma]2g -b doubleBanana\[Gamma]Paolog -b Hold[b-1] A2 doubleBanana\[Gamma]Grad\[Gamma]2 +b^2 hatg\[Gamma]2+b^2 hat\[Gamma]1\[Gamma]2+b^2 hat\[Gamma]2*)
(*- b hat\[Gamma]Paolo\[Gamma]2-2 hat\[Gamma]Paolo+K banana\[Gamma]PaoloCT^2-b Hold[b-1]H2 hat\[Gamma]Grad) z["\[Gamma]2"]);(*K should be K=1+b: half SUBDIV b*doubleBanana\[Gamma]Paolog + SUBDIV IN b*hat\[Gamma]Paolo\[Gamma]2 + SUBDIV IN hat\[Gamma]Paolo	.*)*)
(*(*HOWEVER, IF ONE DOES NOT ALLOW FOR banana*banana\[Gamma]Paolo, THEN THE VALUE OF K MUST BE K=1+3b/2! BECAUSE NOW ITS:*)
(* FULL SUBDIV b*doubleBanana\[Gamma]Paolog + SUBDIV IN b*hat\[Gamma]Paolo\[Gamma]2 + SUBDIV IN hat\[Gamma]Paolo	.*)*)
(*PPrint[{%," = "},%,{"\n=",\[CapitalGamma]\[Gamma]2//.replaceRule,"\n"}]*)
(*(*\[CapitalGamma]g =g0 \[Mu]^-\[Epsilon](1-(2 b bananag-2 (b-1)banana\[Gamma]Grad )g0 \[Mu]^-\[Epsilon]-g0^2\[Mu]^(-2 \[Epsilon]) (-4 (-1+b) doubleBanana+2 (-1+b) b doubleBanana+b^2 doubleBanana+2  hat+4 b hat+8 (-1+b) b hat+2 (-1+b)  GradImmediateIntNotAllowed hat-(-1+b) (2 doubleBanana+4 hat)+(-1+b) b (2 doubleBanana+4 hat)-b^2 (doubleBanana+6 hat)+(-1+b)(4 doubleBanana+8 hat)-b^2 (2 doubleBanana+10 hat))  z["g"]);*)*)
(**)
(*\[CapitalGamma]g =(1-(2 b bananag-2 Hold[b-1]banana\[Gamma]Grad )g0 \[Mu]^-\[Epsilon]-g0^2 \[Mu]^(-2 \[Epsilon]) (1/b**)
(*(-b^3 (2 doubleBananag+4 hatg+2 hatg\[Gamma]1+4 hat\[Gamma]1g+2 hatg\[Gamma]2+4 hat\[Gamma]2g)*)
(*+(*ONLY \[Gamma]Grad 1) in my notes	.*)Hold[(b-1)]b(4 (doubleBananaGradMultig-1/2 bananaMultigCT^2-1/2 bananaMultigCT*banana\[Gamma]GradCT)+4 (hatMultigGrad-1/2 bananaMultigCT^2)+4 (hatGradMultig-1/2 bananaMultigCT^2))*)
(*+(*ONLY \[Gamma]Grad 2) in my notes	.*)*)
(*6 Hold[(b-1)] b^2 hatExtraGrad*)
(*+(*ONLY \[Gamma]Grad 3) in my notes	.*)*)
(*2 Hold[(b-1)] b GradImmediateIntNotAllowed hatExtraGrad*)
(*-(* \[Gamma]Grad \[Gamma]Plus 1) in my notes	.*)*)
(*Hold[(b-1)]b (4 (doubleBananaGrad\[Gamma]Plus-banana\[Gamma]PlusCT*banana\[Gamma]GradCT(*These are actually 2 different diagrams with 1/2 in front*))-2doubleBananaGrad\[Gamma]PlusNOsub+4 hatGrad\[Gamma]PlusNOsub)*)
(*-(* \[Gamma]Grad \[Gamma]Plus 2) in my notes	.*)*)
(*4 Hold[(b-1)] b (doubleBananaGrad\[Gamma]Plus-banana\[Gamma]PlusCT*banana\[Gamma]GradCT)*)
(*+(* \[Gamma]Grad \[Gamma]Minus 1) in my notes	.*)*)
(*Hold[(b-1)] b^2 (4 (doubleBananaGrad\[Gamma]2-banana\[Gamma]2CT*banana\[Gamma]GradCT(*These are actually 2 different diagrams with 1/2 in front*))-2doubleBananaGrad\[Gamma]2NOsub+4 hatGrad\[Gamma]2NOsub)*)
(*+(* \[Gamma]Grad \[Gamma]Minus 2) in my notes	.*)*)
(*2 Hold[(b-1)] b^2 (doubleBananaExtraGrad-banana\[Gamma]MinusCT*banana\[Gamma]GradCT)*)
(*+(* \[Gamma]Grad \[Gamma]Minus 2bis) in my notes	.*)*)
(*2  Hold[(b-1)]b^2 hatExtraGrad GradImmediateIntNotAllowed*)
(**)
(*+(* ONLY \[Gamma]Paolo 1) in my notes	.*)*)
(*2 b (hatMultig\[Gamma]Paolo-1/2 bananaMultigCT^2)*)
(*+(* \[Gamma]Paolo \[Gamma]Minus 1) in my notes	.*)*)
(*4 b^2 hat\[Gamma]Paolo\[Gamma]2g*)
(*- J*b* banana^2 )));*)
(**)
(*PPrint[{%," = "},%,"\n"]*)
(**)
(*\[CapitalGamma]\[Gamma] =1-1/2 (-1+b) g0^2 \[Mu]^(-2 \[Epsilon]) GradImmediateIntNotAllowed (banana^2/2+hat+sunset) z["\[Gamma]"];*)
(*PPrint[{%," = "},%,"\n"]*)


(* ::Input:: *)
(*loopOrder=2;*)
(**)
(*\[CapitalGamma]gtsmall=(g0 \[Mu]^-\[Epsilon] \[CapitalGamma]\[Gamma]1small +g0 \[Mu]^-\[Epsilon] \[CapitalGamma]\[Gamma]2small+g0 \[Mu]^-\[Epsilon] \[CapitalGamma]g )/\[CapitalGamma]\[Gamma] ^2/.z[_]->1(*//.replaceRule*);*)
(**)
(*\[CapitalGamma]gt=g0 \[Mu]^-\[Epsilon] (\[CapitalGamma]\[Gamma]1 + \[CapitalGamma]\[Gamma]2+\[CapitalGamma]g +\[CapitalGamma]\[Gamma]1*\[CapitalGamma]\[Gamma]2)/\[CapitalGamma]\[Gamma] ^2/.z[_]->1(*//.replaceRule*);*)
(*\[CapitalGamma]gtProduct=g0 \[Mu]^-\[Epsilon] ((1+\[CapitalGamma]\[Gamma]1) (1+ \[CapitalGamma]\[Gamma]2)+\[CapitalGamma]g-1)/\[CapitalGamma]\[Gamma] ^2/.z[_]->1(*//.replaceRule*); (*I should also multiply \[CapitalGamma]g, right? Maybe after having removed some cross terms	.*)*)
(**)
(*FS/@(Series[%,{g0,0,loopOrder+1}]);*)
(*Normal[%]/.hideSubDivs ;*)


(* ::Input:: *)
(*\[CapitalGamma]gtProduct//.replaceRule//Series[#,{g0,0,3}]&*)
(*\[CapitalGamma]gtsmall//.replaceRule//Series[#,{g0,0,3}]&*)
(*%%-%//FS*)
(*ReleaseHold[%]/.hideSubDivs//FS*)
(**)


(* ::Input:: *)
(*(*replaceRule={GradImmediateIntNotAllowed\[RuleDelayed]0,h\[Rule]1,h2\[Rule]1,H\[Rule]0,H2\[Rule]0,a2\[Rule]1-a-3/b,a\[Rule]0,A2->1(*A2*)(*2+3/b-A*),A\[Rule]1(*A*)};*)*)
(*{GradImmediateIntNotAllowed:>0,h->1,h2->1,H->0,H2->0,a2->1-a-3/b,a->0,A2->1,A->1}*)


(* ::Input:: *)
(*g=Collect[(Series[\[CapitalGamma]gtProduct//.replaceRule,{g0,0,loopOrder+1}]//Normal),{g0},FS];*)
(*PPrint[%,%]*)
(*(*g=Series[g0 \[Mu]^-\[Epsilon] Zgt^(-1),{g0,0,2}]//Normal*)*)
(*\[Beta]Function[g(*/.hideSubDivs*)  ,"print"->tTrue](*It's slow if the subDivs are not hidden directly in g, I think it's just because it's a long expression*)*)
(**)
(*Replace[Normal[%],a_/;!(FreeQ[a,g^3]):>(a/.(*banana\[Gamma]Grad*) banana\[Gamma]Paolo->0),{1}];*)
(*%/.banana\[Gamma]Grad^2->0*)
(*%/.K->1+3b/2*)
(**)
(*%//.replaceRule;*)
(**)
(*%/.hideSubDivs *)
(*%/.replaceDiagrams//FullSimplify//Factor*)
(*ReleaseHold[%]//FS*)
(**)
(*RGeq2=Simplify[Normal[%]]==0;*)
(*%%/.{J->-(1/2) (-16+11 b)}//Collect[#,g,FS]&*)
(*%/.b->1//Collect[#,g,FS]&(*This is if some explicit bananaCT are used*)*)
(**)


(* ::Input:: *)
(*g (-((1+2 b) g)+\[Epsilon]+(g^2 (2 J+b (-27+b (22+6 \[Epsilon]))))/\[Epsilon])/.J->1/2 (27-22 b) b//Collect[#,g,FS]&*)


(* ::Item:: *)
(*Nice result I got some time ago (could want to reproduce it)*)


(* ::Input:: *)
(*(-1-2 b) g^2+b (1+5 b) g^3+g \[Epsilon]*)


(* ::Item:: *)
(*Get the value of J*)


(* ::Input:: *)
(*(*This is with some explicit bananaCT^2*)*)
(*(2 J+b (-16+b (11+\[Epsilon]+5 b \[Epsilon])))//Collect[#,\[Epsilon],FS]&*)
(*%/. {\[Epsilon]->0,H2->0}*)
(*Solve[%==0,J]*)


(* ::Input:: *)
(*(*This is with more correct elimination of Gradients*)*)
(*(2 J+b (-27+b (22+6 \[Epsilon])))//Collect[#,\[Epsilon],FS]&*)
(*%/. {\[Epsilon]->0,H2->0}*)
(*Solve[%==0,J]//FS*)


(* ::Input:: *)
(*(*This is WITHOUT any explicit bananaCT^2*)*)
(*(2 J+b (-4+b (9+\[Epsilon]+5 b \[Epsilon])))//Collect[#,\[Epsilon],FS]&*)
(*%/. {\[Epsilon]->0,H2->0}*)
(*Solve[%==0,J]*)


(* ::Item:: *)
(*Check the subdivs*)


(* ::Input:: *)
(*(*HERE I "FORGOT" ABOUT THE DIAGRAM WITH BOTH A BANANA FOR THE EMITTER AND FOR THE ABSOBER*)*)
(*g \[Epsilon]+g^2 \[Epsilon] (-b (2 bananag+banana\[Gamma]1+banana\[Gamma]2)+banana\[Gamma]Paolo+2 banana\[Gamma]Grad Hold[b-1])-2 g^3 \[Epsilon] (banana\[Gamma]Paolo^2+b^2 ((2 bananag+banana\[Gamma]1+banana\[Gamma]2)^2-2 doubleBananag-doubleBanana\[Gamma]1g-doubleBanana\[Gamma]2g-4 hatg-3 hatg\[Gamma]1-3 hatg\[Gamma]2-hatg\[Gamma]2\[Gamma]1-hat\[Gamma]1-4 hat\[Gamma]1g-hat\[Gamma]1\[Gamma]2-hat\[Gamma]2-4 hat\[Gamma]2g)+2 (hat+hat\[Gamma]Paolo)+b (-2 (2 bananag+banana\[Gamma]1+banana\[Gamma]2) banana\[Gamma]Paolo+doubleBanana\[Gamma]Paolog+4 hat+hat\[Gamma]Paolo\[Gamma]1+hat\[Gamma]Paolo\[Gamma]2)+Hold[b-1] (4 banana\[Gamma]Grad banana\[Gamma]Paolo-2 doubleBanana+3 doubleBanana\[Gamma]Grad+4 hat+b (-4 (2 bananag+banana\[Gamma]1+banana\[Gamma]2) banana\[Gamma]Grad+4 doubleBanana+doubleBanana\[Gamma]Grad+12 hat)+4 banana\[Gamma]Grad^2 Hold[b-1]))//.replaceRule;*)
(*Collect[%,{\[Epsilon] ,g,b},FS];*)
(*PPrint["\[Beta]",%,"style"->{FontSize->17}]*)


(* ::Input:: *)
(*(*Here I added it with -\[CapitalGamma]\[Gamma]1*\[CapitalGamma]\[Gamma]2  WRONG *)*)
(*g \[Epsilon]+g^2 \[Epsilon] (-b (2 bananag+banana\[Gamma]1+banana\[Gamma]2)+banana\[Gamma]Paolo+2 banana\[Gamma]Grad Hold[b-1])-2 g^3 \[Epsilon] (banana\[Gamma]Paolo^2+b^2 ((2 bananag+banana\[Gamma]1)^2+(4 bananag+3 banana\[Gamma]1) banana\[Gamma]2+banana\[Gamma]2^2-2 doubleBananag-doubleBanana\[Gamma]1g-doubleBanana\[Gamma]2g-4 hatg-3 hatg\[Gamma]1-3 hatg\[Gamma]2-hatg\[Gamma]2\[Gamma]1-hat\[Gamma]1-4 hat\[Gamma]1g-hat\[Gamma]1\[Gamma]2-hat\[Gamma]2-4 hat\[Gamma]2g)+2 (hat\[Gamma]Paolog+hat\[Gamma]Paolo)+b (-((4 bananag+3 banana\[Gamma]1+2 banana\[Gamma]2) banana\[Gamma]Paolo)+doubleBanana\[Gamma]Paolog+4 hat\[Gamma]Paolo\[Gamma]2g+hat\[Gamma]Paolo\[Gamma]1+hat\[Gamma]Paolo\[Gamma]2)+Hold[b-1] (4 banana\[Gamma]Grad banana\[Gamma]Paolo-2 doubleBanana+4 hat+b (-4 (2 bananag+banana\[Gamma]1+banana\[Gamma]2) banana\[Gamma]Grad+4 doubleBanana+(A+A2) doubleBanana\[Gamma]Grad+12 hat)+4 banana\[Gamma]Grad^2 Hold[b-1]))//.replaceRule;*)
(*%/.Hold[b-1]->0;*)
(*Collect[%,{\[Epsilon] ,g,b},FS];*)
(*PPrint["\[Beta]",%,"style"->{FontSize->17}]*)


(* ::Input:: *)
(*(*Here I added it with (1+\[CapitalGamma]\[Gamma]1*(1+\[CapitalGamma]\[Gamma]2)  *)g \[Epsilon]+g^2 \[Epsilon] (-b (2 bananag+banana\[Gamma]1+banana\[Gamma]2)+banana\[Gamma]Paolo+2 banana\[Gamma]Grad Hold[b-1])-g^3 \[Epsilon] (2 banana\[Gamma]Paolo^2+2 b^2 ((2 bananag+banana\[Gamma]1)^2+(4 bananag+banana\[Gamma]1) banana\[Gamma]2+banana\[Gamma]2^2-2 doubleBananag-doubleBanana\[Gamma]1g-doubleBanana\[Gamma]2g-4 hatg-3 hatg\[Gamma]1-3 hatg\[Gamma]2-hatg\[Gamma]2\[Gamma]1-hat\[Gamma]1-4 hat\[Gamma]1g-hat\[Gamma]1\[Gamma]2-hat\[Gamma]2-4 hat\[Gamma]2g)+4 (hat\[Gamma]Paolo+hat\[Gamma]Paolog)+GradImmediateIntNotAllowed (banana^2+2 (hat+sunset))-b (2 (4 bananag+banana\[Gamma]1+2 banana\[Gamma]2) banana\[Gamma]Paolo-2 doubleBanana\[Gamma]Paolog-2 (hat\[Gamma]Paolo\[Gamma]1+hat\[Gamma]Paolo\[Gamma]2+4 hat\[Gamma]Paolo\[Gamma]2g)+GradImmediateIntNotAllowed (banana^2+2 (hat+sunset)))+2 Hold[b-1] (2 (2 banana\[Gamma]Grad banana\[Gamma]Paolo+2 doubleBananaGradMultig-4 doubleBananaGrad\[Gamma]Plus+doubleBananaGrad\[Gamma]PlusNOsub+GradImmediateIntNotAllowed hatExtraGrad+4 hatGradMultig-2 hatGrad\[Gamma]PlusNOsub)+b (-4 (2 bananag+banana\[Gamma]1+banana\[Gamma]2) banana\[Gamma]Grad+4 doubleBananaExtraGrad+A doubleBanana\[Gamma]Grad+A2 doubleBanana\[Gamma]Grad\[Gamma]2+12 hatExtraGrad+(H+H2) hat\[Gamma]Grad)+4 banana\[Gamma]Grad^2 Hold[b-1]))(*//.replaceRule*);*)
(*(*%/.Hold[b-1]->0;*)*)
(*Collect[%,{\[Epsilon] ,g,b},FS];*)
(*PPrint["\[Beta]",%,"style"->{FontSize->16}];*)
(*ReleaseHold[%%];*)
(*Series[%,{\[Epsilon] ,0,1},{g,0,3},{b,0,2}];*)
(*PPrint["\[Beta]",%,"style"->{FontSize->16}];*)
(*%%/.hideSubDivs //FS*)
(*%/.replaceDiagrams//FullSimplify//Factor*)
(*%//.replaceRule*)


(* ::Input:: *)
(*(*Here I use K and J, and explicit bananaCT	.*)*)
(*g \[Epsilon]+g^2 \[Epsilon] (-b (2 bananag+banana\[Gamma]1+banana\[Gamma]2)+banana\[Gamma]Paolo+2 banana\[Gamma]Grad Hold[b-1])-2 g^3 \[Epsilon] (-bananaMultigCT^2-(1+(3 b)/2) banana\[Gamma]PaoloCT^2+2 hatMultig\[Gamma]Paolo+b^2 ((2 bananag+banana\[Gamma]1)^2+(4 bananag+banana\[Gamma]1) banana\[Gamma]2+banana\[Gamma]2^2-2 doubleBananag-doubleBanana\[Gamma]1g-doubleBanana\[Gamma]2g-4 hatg-3 hatg\[Gamma]1-3 hatg\[Gamma]2-hatg\[Gamma]2\[Gamma]1-hat\[Gamma]1-4 hat\[Gamma]1g-hat\[Gamma]1\[Gamma]2-hat\[Gamma]2-4 hat\[Gamma]2g)+2 hat\[Gamma]Paolo+b (doubleBanana\[Gamma]Paolog+hat\[Gamma]Paolo\[Gamma]1+hat\[Gamma]Paolo\[Gamma]2+4 hat\[Gamma]Paolo\[Gamma]2g)-banana^2 J+Hold[b-1] (-6 bananaMultigCT^2-2 bananaMultigCT banana\[Gamma]GradCT+b (-4 (2 bananag+banana\[Gamma]1+banana\[Gamma]2) banana\[Gamma]Grad-2 banana\[Gamma]GradCT banana\[Gamma]MinusCT-4 banana\[Gamma]PlusCT^2+4 doubleBananaExtraGrad+doubleBanana\[Gamma]Grad+doubleBanana\[Gamma]Grad\[Gamma]2+10 hatExtraGrad)+2 (4 banana\[Gamma]GradCT banana\[Gamma]PlusCT+2 doubleBananaGradMultig-4 doubleBananaGrad\[Gamma]Plus+doubleBananaGrad\[Gamma]PlusNOsub+2 (hatGradMultig-hatGrad\[Gamma]PlusNOsub+hatMultigGrad))+4 banana\[Gamma]Grad^2 Hold[b-1]))/.banana\[Gamma]Grad^2->0;*)
(**)
(*Collect[%,{\[Epsilon] ,g,b},FS];*)
(*PPrint["\[Beta]",%,"style"->{FontSize->16}];*)
(**)
(*(*ReleaseHold[%%];*)*)
(*Series[%%,{\[Epsilon] ,0,1},{g,0,3}(*,{b,0,2}*)];*)
(*PPrint["\[Beta]",%,"style"->{FontSize->16}];*)
(*%%/.hideSubDivs //FS*)
(*%/.replaceDiagrams//FullSimplify//Factor*)
(*%//.replaceRule*)


(* ::Subitem::Closed:: *)
(*Order b^2*)


(* ::Input:: *)
(*2 b^2 (-(2 bananag+banana\[Gamma]1)^2-(4 bananag+banana\[Gamma]1) banana\[Gamma]2-banana\[Gamma]2^2+2 doubleBananag+doubleBanana\[Gamma]1g+doubleBanana\[Gamma]2g+4 hatg+3 hatg\[Gamma]1+3 hatg\[Gamma]2+hatg\[Gamma]2\[Gamma]1+hat\[Gamma]1+4 hat\[Gamma]1g+hat\[Gamma]1\[Gamma]2+hat\[Gamma]2+4 hat\[Gamma]2g) /. hideSubDivs//FS*)
(*%/. replaceDiagrams//FullSimplify//Factor*)
(*%//.replaceRule*)


(* ::Subitem:: *)
(*Order b^1*)


(* ::Input:: *)
(*b (3 banana\[Gamma]PaoloCT^2-2 (doubleBanana\[Gamma]Paolog+hat\[Gamma]Paolo\[Gamma]1+hat\[Gamma]Paolo\[Gamma]2+4 hat\[Gamma]Paolo\[Gamma]2g)-2 (-4 (2 bananag+banana\[Gamma]1+banana\[Gamma]2) banana\[Gamma]Grad-2 banana\[Gamma]GradCT banana\[Gamma]MinusCT-4 banana\[Gamma]PlusCT^2+4 doubleBananaExtraGrad+doubleBanana\[Gamma]Grad+doubleBanana\[Gamma]Grad\[Gamma]2+10 hatExtraGrad) Hold[b-1]) /. hideSubDivs//FS*)
(*%/. replaceDiagrams//FullSimplify//Factor*)
(*%//.replaceRule*)


(* ::Item:: *)
(* banana\[Gamma]Paolo^2- banana\[Gamma]Grad^2-banana\[Gamma]Grad*banana\[Gamma]Paolo ARE ORDER \[Lambda]^4, I.E. THEY CONTAIN 4 GREEN LINES. CAN WE USE THEM????*)
(*NO, I THINK THAT banana\[Gamma]Paolo SHOULD BE USED TO MULTIPLY OTHER bananas IN CT*)


(* ::Subsubsection::Closed:: *)
(*g* at 2loop*)


(* ::Input:: *)
(*gstar2=Select[Flatten@SolveValues[RGeq2,g],#=!=0&];*)
(**)
(*gstar2=Series[gstar2,{\[Epsilon],0,loopOrder},Assumptions->b>0]//Expand*)
(*%/.\[Epsilon]->0*)
(*gstar2=Select[Normal@gstar2,(#/.\[Epsilon]->0)==0&][[1]]*)


(* ::Input:: *)
(*\[Epsilon]/(1+2 b)+(b (1+5 b) \[Epsilon]^2)/(1+2 b)^3*)


(* ::Input:: *)
(**)
(*RGeq2/.{J->-(1/2) b (-4+9 b)}//Collect[#,g,FS]&*)
(*gstar2=Select[Flatten@SolveValues[%,g],#=!=0&];*)
(**)
(*gstar2=Series[gstar2,{\[Epsilon],0,loopOrder},Assumptions->b>0]//Expand*)
(*%/.\[Epsilon]->0*)
(*gstar2=Select[Normal@gstar2,(#/.\[Epsilon]->0)==0&][[1]]*)


(* ::Input:: *)
(*(*THIS IS EXACTLY THE RESULT I WAS GETTING EARLIER!!!!	!*)*)


(* ::Input:: *)
(*(*Other option*)*)
(*RGeq2/.{J->4+(11 b)/2-7 b^2,banana\[Gamma]MinusCT->1/\[Epsilon]}//Collect[#,g,FS]&*)
(*gstar2=Select[Flatten@SolveValues[%,g],#=!=0&];*)
(**)
(*gstar2=Series[gstar2,{\[Epsilon],0,loopOrder},Assumptions->b>0]//Expand*)
(*%/.\[Epsilon]->0*)
(*gstar2=Select[Normal@gstar2,(#/.\[Epsilon]->0)==0&][[1]]*)


(* ::Subsubsection::Closed:: *)
(*RG functions: \[CapitalGamma]\[Gamma]1 & Subscript[d, f]*)


(* ::Input:: *)
(*loopOrder=2;*)
(**)
(*g=Normal[Series[\[CapitalGamma]gt,{g0,0,loopOrder+1}]]/.replaceRule;*)
(**)
(*(1+\[CapitalGamma]\[Gamma]1)/(\[CapitalGamma]\[Gamma])^0/.z[_]->1;*)
(*FS/@(%/.replaceRule);*)
(*PPrint[{\[CapitalGamma]\[Gamma]1,"="},\[CapitalGamma]\[Gamma]1]*)
(**)
(*obsWithoutZ=\[Gamma]Function[%%,g,"print"->True]*)
(*(*%/.b->1*)*)
(*%/. hideSubDivs*)
(*%/. replaceDiagrams//FullSimplify//Factor*)
(*ReleaseHold[%]*)
(**)
(*%/.g->gstar2+O[\[Epsilon]]^3//FS*)
(**)
(*2+Normal@%*)


(* ::Input:: *)
(*df=2-(b \[Epsilon])/(1+2 b)-(b (1+b+4 b^2) \[Epsilon]^2)/(2 (1+2 b)^3)*)


(* ::Input:: *)
(*(*If GradImmediateIntNotAllowed\[RuleDelayed]0, then following is the same as above*)*)
(*g=Normal[Series[\[CapitalGamma]gt,{g0,0,loopOrder+1}]]/.replaceRule;*)
(*(1+\[CapitalGamma]\[Gamma]1)*(\[CapitalGamma]\[Gamma])^-1/.z[_]->1;*)
(*FS/@(%/.replaceRule)*)
(**)
(*obsWithZ=\[Gamma]Function[%,g,"print"->tTrue]*)
(*(*%/.b->1*)*)
(*%/. hideSubDivs*)
(*%/. replaceDiagrams//FullSimplify//Factor*)
(*%/.g->gstar2+O[\[Epsilon]]^3//FS*)


(* ::Subsubsection::Closed:: *)
(*RG functions: \[CapitalGamma]\[Gamma]2		not finite (is this an observable of the theory?) THIS CANNOT WORK: I THINK I HAVE TO SPLIT THE PURE \[Gamma]L AND THE \[Gamma]PAOLO. THE REASON IS THAT I GET EXTRA, UNNECESSARY SUBTRACTION OF SUBDIVS, SUCH AS banana\[Gamma]Grad banana\[Gamma]Paolo		THIS WAY IT IS FINITE!!!!!*)


(* ::Input:: *)
(*loopOrder=2;*)
(**)
(*(*replaceRule=Flatten@{GradImmediateIntNotAllowed->0,a2->-3/(b)+1-a,a->0,h->h,h2->h2};*)*)
(*g=Normal[Series[\[CapitalGamma]gt,{g0,0,loopOrder+1}]]/.replaceRule;*)
(**)
(*(1+\[CapitalGamma]\[Gamma]2)/(\[CapitalGamma]\[Gamma])^0/.z[_]->1;*)
(*FS/@(%/.replaceRule)*)
(**)
(*\[Gamma]Function[%,g,"print"->tTrue]*)
(*Replace[Normal[%],a_/;!(FreeQ[a,g^2]):>(a/.(*banana\[Gamma]Grad*) banana\[Gamma]Paolo->0),{1}]*)
(*%/.K->1+3b/2//FS (*THIS IS CORRECT!!! IF ONE DOES NOT ALLOW FOR banana*banana\[Gamma]Paolo, THEN THE VALUE OF K MUST BE K=1+3b/2	.*)*)
(*(*%/.b->1*)%/.hideSubDivs //FS*)
(*%/.replaceDiagrams//FullSimplify//Factor*)
(*%//.replaceRule//FS*)
(*ReleaseHold[%]//Collect[#,g,FS]&*)
(*%/.b->1*)


(* ::Input:: *)
(*%/.g->gstar2+O[\[Epsilon]]^3//FS;*)
(**)


(* ::Item:: *)
(*Try to separate \[Gamma]2 and \[Gamma]Paolo: IT KINDA WORKS!!!!!*)


(* ::Input:: *)
(*\[CapitalGamma]\[Gamma]2 =1+(-(b banana\[Gamma]2)  g0 \[Mu]^-\[Epsilon]+g0^2 \[Mu]^(-2 \[Epsilon]) (b^2 doubleBanana\[Gamma]2g  +b^2 hatg\[Gamma]2+b^2 hat\[Gamma]1\[Gamma]2+b^2 hat\[Gamma]2-b Hold[b-1] doubleBanana\[Gamma]Grad\[Gamma]2 ) );(*K should be K=1+b: half SUBDIV b*doubleBanana\[Gamma]Paolog + SUBDIV IN b*hat\[Gamma]Paolo\[Gamma]2 + SUBDIV IN hat\[Gamma]Paolo	.*)*)
(*(*HOWEVER, IF ONE DOES NOT ALLOW FOR banana*banana\[Gamma]Paolo, THEN THE VALUE OF K MUST BE K=1+3b/2! BECAUSE NOW ITS:*)
(* FULL SUBDIV b*doubleBanana\[Gamma]Paolog + SUBDIV IN b*hat\[Gamma]Paolo\[Gamma]2 + SUBDIV IN hat\[Gamma]Paolo	.*)*)
(*\[CapitalGamma]\[Gamma]Paolo=1+(-( - banana\[Gamma]Paolo)  g0 \[Mu]^-\[Epsilon]+g0^2 \[Mu]^(-2 \[Epsilon]) (- b hat\[Gamma]Paolo\[Gamma]2-2 hat\[Gamma]Paolo-b doubleBanana\[Gamma]Paolog +K banana^2) )/.K->(1+2*b/2);(*K should be K=1+b: half SUBDIV b*doubleBanana\[Gamma]Paolog + SUBDIV IN b*hat\[Gamma]Paolo\[Gamma]2 + SUBDIV IN hat\[Gamma]Paolo	.*)*)
(*(*HOWEVER, IF ONE DOES NOT ALLOW FOR banana*banana\[Gamma]Paolo, THEN THE VALUE OF K MUST BE K=1+3b/2! BECAUSE NOW ITS:*)
(* FULL SUBDIV b*doubleBanana\[Gamma]Paolog + SUBDIV IN b*hat\[Gamma]Paolo\[Gamma]2 + SUBDIV IN hat\[Gamma]Paolo	.*)*)


(* ::Input:: *)
(*loopOrder=2;*)
(**)
(*(*replaceRule=Flatten@{GradImmediateIntNotAllowed->0,a2->-3/(b)+1-a,a->0,h->h,h2->h2};*)*)
(*g=Normal[Series[\[CapitalGamma]gt,{g0,0,loopOrder+1}]]/.replaceRule*)
(**)
(*(\[CapitalGamma]\[Gamma]2)/(\[CapitalGamma]\[Gamma])^0/.z[_]->1;*)
(*FS/@(%/.replaceRule)*)
(**)
(*\[Gamma]Function[%,g,"print"->tTrue]*)
(*(*Replace[Normal[%],a_/;!(FreeQ[a,g^2]):>(a/.(*banana\[Gamma]Grad*) banana\[Gamma]Paolo->0),{1}]*)*)
(*%/.K->1+3b/2//FS (*THIS IS CORRECT!!! IF ONE DOES NOT ALLOW FOR banana*banana\[Gamma]Paolo, THEN THE VALUE OF K MUST BE K=1+3b/2	.*)*)
(*(*%/.b->1*)%/.hideSubDivs //FS*)
(*%/.replaceDiagrams//FullSimplify//Factor*)
(*%//.replaceRule//FS*)
(*ReleaseHold[%]//FS*)
(*%/.b->1*)


(* ::Input:: *)
(*loopOrder=2;*)
(**)
(*(*replaceRule=Flatten@{GradImmediateIntNotAllowed->0,a2->-3/(b)+1-a,a->0,h->h,h2->h2};*)*)
(*g=g0 \[Mu]^-\[Epsilon](*Normal[Series[\[CapitalGamma]gt,{g0,0,loopOrder+1}]]/.replaceRule;*)*)
(**)
(*(\[CapitalGamma]\[Gamma]Paolo)/(\[CapitalGamma]\[Gamma])^0/.z[_]->1;*)
(*FS/@(%/.replaceRule)*)
(**)
(*\[Gamma]Function[%,g,"print"->True,"g0Order"->loopOrder]*)
(*Replace[Normal[%],a_/;!(FreeQ[a,g^2]):>(a/.(*banana\[Gamma]Grad*) banana\[Gamma]Paolo->0),{1}]*)
(*%/.K->1+3b/2//FS (*THIS IS CORRECT!!! IF ONE DOES NOT ALLOW FOR banana*banana\[Gamma]Paolo, THEN THE VALUE OF K MUST BE K=1+3b/2	.*)*)
(*(*%/.b->1*)%/.hideSubDivs //FS*)
(*%/.replaceDiagrams//FullSimplify//Factor*)
(*%//.replaceRule//FS*)
(*ReleaseHold[%]//Collect[#,g,FS]&*)
(*%/.b->1*)


(* ::Subitem:: *)
(*Compare with previous result*)


(* ::Input:: *)
(*(*Previously*)*)
(*(1-b) g+1/2 (-1+b) (2+3 b) g^2*)


(* ::Input:: *)
(*(*Now*)*)
(*Normal[SeriesData[g, 0, {-b, Rational[3, 2] b^2 + b/\[Epsilon]}, 1, 3, 1]+(g+g^2 (-1-(b (2+\[Epsilon]))/(2 \[Epsilon])))]//Collect[#,g,FS]&*)


(* ::Subsubsection::Closed:: *)
(*RG functions: \[CapitalGamma]g		I GUESS THAT THIS DOES NOT HAVE TO BE FINITE, IT IS NOT AN OBSERVABLE OF THE THEORY, THE FULL OBSERVABLE IS THE BETA FUNCTION*)


(* ::Item::Closed:: *)
(*b=1*)


(* ::Input:: *)
(*loopOrder=2;*)
(**)
(*(*replaceRule=Flatten@{GradImmediateIntNotAllowed->0,a2->-3/(b)+1-a,a->0,h->h,h2->h2};*)*)
(*g=Normal[Series[\[CapitalGamma]gt,{g0,0,loopOrder+1}]]/.replaceRule/.K->1+3b/2/.{b->1,J->0}/.{hatg\[Gamma]2->hatMultig\[Gamma]Paolo,hat\[Gamma]2g->hat\[Gamma]Paolo\[Gamma]2g}//ReleaseHold*)
(**)
(*(\[CapitalGamma]g)/(\[CapitalGamma]\[Gamma])^0/.z[_]->1;*)
(*FS/@(%/.replaceRule);*)
(*Collect[ReleaseHold[%],{g0, \[Mu] },Expand]/.{b->1,J->0};*)
(*%/.{hatg\[Gamma]2->hatMultig\[Gamma]Paolo,hat\[Gamma]2g->hat\[Gamma]Paolo\[Gamma]2g}*)
(**)
(*\[Gamma]Function[%,g,"print"->True]*)
(*Replace[Normal[%],a_/;!(FreeQ[a,g^2]):>(a/.(*banana\[Gamma]Grad*){ banana\[Gamma]Paolo->banana\[Gamma]2}),{1}]*)
(*%/.K->1+3b/2//FS (*THIS IS CORRECT!!! IF ONE DOES NOT ALLOW FOR banana*banana\[Gamma]Paolo, THEN THE VALUE OF K MUST BE K=1+3b/2	.*);*)
(*(*%/.b->1*)%/.hideSubDivs //Collect[#,g,FS]&*)
(*%/.replaceDiagrams//Collect[#,g,FS]&*)
(*%//.replaceRule//FS;*)
(*ReleaseHold[%]//FS;*)
(*%/.b->1;*)


(* ::Item::Closed:: *)
(*b>1*)


(* ::Input:: *)
(*loopOrder=2;*)
(**)
(*(*replaceRule=Flatten@{GradImmediateIntNotAllowed->0,a2->-3/(b)+1-a,a->0,h->h,h2->h2};*)*)
(*g=Normal[Series[\[CapitalGamma]gt,{g0,0,loopOrder+1}]]/.replaceRule;*)
(**)
(*(\[CapitalGamma]g)/(\[CapitalGamma]\[Gamma])^0/.z[_]->1;*)
(*FS/@(%/.replaceRule)*)
(**)
(*\[Gamma]Function[%,g,"print"->tTrue]*)
(*Replace[Normal[%],a_/;!(FreeQ[a,g^2]):>(a/.(*banana\[Gamma]Grad*) banana\[Gamma]Paolo->0),{1}]*)
(*%/.K->1+3b/2//FS (*THIS IS CORRECT!!! IF ONE DOES NOT ALLOW FOR banana*banana\[Gamma]Paolo, THEN THE VALUE OF K MUST BE K=1+3b/2	.*)*)
(*(*%/.b->1*)%/.hideSubDivs //FS*)
(*%/.replaceDiagrams//FullSimplify//Factor*)
(*%//.replaceRule//FS*)
(*ReleaseHold[%]//FS*)
(*%/.b->1*)


(* ::Input:: *)
(*(8-4 K+2 \[Epsilon]+b (-8+\[Epsilon]-3 b \[Epsilon]))//Collect[#,\[Epsilon],FS]&*)
(*%/.{\[Epsilon]->0,H2->0}*)
(*Solve[%==0,K]*)


(* ::Input:: *)
(*%/.g->gstar2+O[\[Epsilon]]^3//FS;*)
(**)


(* ::Input:: *)
(*-8+4 b+2 \[Epsilon]+3 b \[Epsilon]//Collect[#,\[Epsilon]]&*)


(* ::Subsubsection::Closed:: *)
(*Z\[Gamma]Inv		TBD*)


(* ::Input:: *)
(*replaceRule={a->0,h->1,h2->1 ,a2->-(1/b),l->3/2};*)
(*Z\[Gamma]Inv/.z[_]->1//FS*)
(*\[Eta]=\[Gamma]FunctionFromZ[%/.replaceRule,ZgtInv/.replaceRule,"print"->True,"gstar"->True]*)
(**)


(* ::Chapter::Closed:: *)
(*\[Section]\[Section] \[CapitalGamma]1 observable 		BAD AND OLD	*)


(* ::Input:: *)
(*\[CapitalGamma]2=1-c b(b-1) g0 \[Mu]^-\[Epsilon] banana ;*)
(**)
(*gg=g0 \[Mu]^-\[Epsilon]-(2b+1)banana (g0 \[Mu]^-\[Epsilon])^2/\[CapitalGamma]2+(g0 \[Mu]^-\[Epsilon])^3 (- goodGuys- gammagGuys-betterNasties - realNasties)/.{c->(1+6 b+3 b^2)/(b (1+2 b))};*)
(*gg*)
(**)
(*\[CapitalGamma]1=1-b g0 \[Mu]^-\[Epsilon] banana /\[CapitalGamma]2+(g0 \[Mu]^-\[Epsilon])^2 b( doubleBanana + 2b hat );*)
(**)
(*\[Gamma]Function[\[CapitalGamma]1,gg,"print"->False]*)
(*%/.banana ->1/\[Epsilon]/.doubleBanana ->1/\[Epsilon]^2/.hat ->1/(2\[Epsilon]^2)+1/(4\[Epsilon])/.sunset->-1/(8\[Epsilon])//FullSimplify//Factor*)
(**)
(*Normal@%/.g->gc2//FullSimplify;*)
(*df=2+Normal[Series[%,{\[Epsilon],0,2}]]*)


(* ::Input:: *)
(*Normal[Series[b (1+b (-1-2 (-1+b) c+\[Epsilon])),{\[Epsilon],0,0}]]//FS*)
(*Flatten@Solve[%==0,c]//FS*)


(* ::Input:: *)
(*Normal[Series[df/.{c->-(1/(2 b))},{\[Epsilon],0,2}]]*)


(* ::Chapter::Closed:: *)
(*\[Section]\[Section] \[CapitalGamma]1 observable NEW AFTER SIMPLIFICATION AND SPLIT OF CONTRIBUTIONS*)


(* ::Subsubsection::Closed:: *)
(*\[Section]\[Section]\[Section] b=1 *)
(*IT WORKS FOR Zg=1+2 g 1/\[Epsilon]-g^2 (-7/\[Epsilon]^2 5/7+5/(2\[Epsilon]));*)
(* IT COMES FROM THE FACT THAT THE CT CAN BE PUT ONE INSIDE THE OTHER!!!!*)


(* ::Input:: *)
(*Zg=1+2 g 1/\[Epsilon]-g^2 (-7/\[Epsilon]^2 5/7+5/(2\[Epsilon]));(*a=5/7*)*)
(**)
(*Z\[Gamma]1=1+ g 1/\[Epsilon]-g^2 (-2/\[Epsilon]^2+1/(2\[Epsilon]));*)
(**)
(*Zgt=Zg Z\[Gamma]1 /.z[_]->1;*)
(*Series[Zgt,{g,0,2}];*)
(*Expand/@%;*)
(*%//Normal*)
(*\[Beta]FunctionFromZ[%]*)


(* ::Item::Closed:: *)
(*Small check that it gives the same result and that it is the inverse of the effective coupling*)


(* ::Input:: *)
(*Series[Zgt,{g,0,2}];*)
(*Expand/@%;*)
(*%*)
(*(1-3g/\[Epsilon]+3g^2(1/\[Epsilon]^2+4(1/(2\[Epsilon]^2)+1/(4\[Epsilon]))))*)
(**)
(*(%/.g->g*%^(-1))*%%//FS*)


(* ::Input:: *)
(*(1-3g/\[Epsilon]+3g^2(1/\[Epsilon]^2+4(1/(2\[Epsilon]^2)+1/(4\[Epsilon]))))^(-1);*)
(*Series[%/.g->g*%,{g,0,2}];*)
(*Expand/@%;*)
(*%//Normal*)


(* ::Input:: *)
(*\[Beta]Function[g0 \[Mu]^(-\[Epsilon])*((1-3g/\[Epsilon]+3g^2(1/\[Epsilon]^2+4(1/(2\[Epsilon]^2)+1/(4\[Epsilon]))))/.g->g0 \[Mu]^(-\[Epsilon]))]*)


(* ::Item::Closed:: *)
(**)


(* ::Input:: *)
(*gc2*)


(* ::Input:: *)
(*\[Gamma]FunctionFromZ[Z\[Gamma]1,Zg*Z\[Gamma]1,0,"print"->True]*)


(* ::Input:: *)
(*Normal[SeriesData[g, 0, {-1, 1}, 1, 3, 1]]/.g->\[Epsilon]/3+(2 \[Epsilon]^2)/9*)
(*Series[%,{\[Epsilon],0,2}]*)


(* ::Title:: *)
(*Plots*)


(* ::Subsection:: *)
(*\[Section] New (after my simpl)*)


(* ::Input::Initialization:: *)
dfRG1L:=2-b \[Epsilon]/(2+b)
dfRG1Lsimp:=2-(b \[Epsilon])/(1+2 b)

dfRG2Lsimp:=2-(b \[Epsilon])/(1+2 b)-(b (1+b+4 b^2) \[Epsilon]^2)/(2 (1+2 b)^3)
dfRG2Lsimp2:=2-(b \[Epsilon])/(1+2 b)+b ((-1+b-6 b^2) \[Epsilon]^2)/(2 (1+2 b)^3)

dfRG2Lwf:=dfWF
dfRG2L:=2-b \[Epsilon]/(2+b)-b (\[Epsilon]/(2+b))^2

(*dfRG2Lsimp:=2-(b \[Epsilon])/(1+2 b)-(b ^2 \[Epsilon]^2)/(1+2 b)^2*)(*BAD*)

dfSLE=1+3/(4(2b+1));


(* ::Input:: *)
(*{dfRG1L,dfRG1Lsimp,dfRG2Lsimp,dfRG2Lwf,dfRG2L,dfRG2Lsimp2,dfSLE};*)
(*PPrint[{#,"->"},#/.b->0/.\[Epsilon]->2]&/@{dfRG1L,dfRG1Lsimp,dfRG2Lsimp,dfRG2Lwf,dfRG2L,dfRG2Lsimp2,dfSLE};*)
(**)
(*Limit[{dfRG1L,dfRG1Lsimp,dfRG2Lsimp,dfRG2Lwf,dfRG2L,dfRG2Lsimp2,dfSLE},b->\[Infinity]]//Quiet*)
(*%/.\[Epsilon]->2*)


(* ::Subsection:: *)
(*\[Section]\[Section] 2d*)


(* ::Input:: *)
(*dfRG2Lwf*)


(* ::Item:: *)
(*Limits*)


(* ::Subitem::Closed:: *)
(*b->\[Infinity]*)


(* ::Input:: *)
(*Limit[dfRG2Lwf,b->\[Infinity]]*)
(*%/.\[Epsilon]->2*)
(*%/.a->-2*)
(*Limit[dfRG2L,b->\[Infinity]]*)
(*%/.\[Epsilon]->2*)


(* ::Subitem::Closed:: *)
(*b->0*)


(* ::Input:: *)
(*Limit[dfRG2Lwf,b->0]*)
(*%/.\[Epsilon]->2*)
(*%/.a->-2*)
(*Limit[dfRG2L,b->0]*)
(*%/.\[Epsilon]->2*)


(* ::Subsubsection:: *)
(*Plots*)


(* ::Input:: *)
(*endRange=5;*)
(*dfRG2Lwf:=dfWF;*)
(**)
(**)
(*Simulation2d=ListPlot[{{1,Around[1.2486744695483691`, 0.023270605268075166`]},{2,Around[1.1151146520584079`, 0.0134148268356009]},{3,Around[1.0768665526174213`, 0.014642777375504247`]},{4,Around[1.0474461998303197`, 0.008312476568391155]},{5,Around[1.0454880607320536`, 0.0064093876238445445`]}},PlotStyle->{RGBColor[0, 1, 0],PointSize[0.005]},PlotLegends->Placed[{"Simulation Data (old)"},{Right,Top}]];*)
(**)
(**)
(*Simulation2dGemini=ListPlot[{{0,Around[1.7534581201029278`, 0.0060679884624822]},{1,Around[1.274522584835579, 0.008333817846449225]},{2,Around[1.1658669951861733`, 0.001939947142635663]},{3,Around[1.1073336136072602`, 0.002384187792543366]},{4,Around[1.0737383484918805`, 0.0017665587042004246`]},{5,Around[1.0670481729478147`, 0.001216345239817617]}(*,{10,1.0251\[PlusMinus]0.0012}*)},PlotStyle->{RGBColor[0, 0.66, 0],PointSize[0.1]},PlotMarkers->X,PlotLegends->Placed[{"Simulation Data (Gemini-opt1)"},{Right,Top}]];*)
(**)
(*plotSLE=Plot[dfSLE,{b,0,endRange},PlotStyle->RGBColor[1, 0, 0],PlotRange->All, PlotLegends->Placed[{Row[{"SLE: ",TraditionalForm[#]}]&@dfSLE},{Right,Top}]];*)
(**)
(*plotRG1L=Plot[#/.\[Epsilon]->2,{b,0,endRange},PlotStyle->RGBColor[0.64, 0, 1],PlotRange->All,PlotLegends->Placed[{Row[{"OLD 1-Loop: ",TraditionalForm[#]}]},{Right,Top}]]&@dfRG1L;*)
(**)
(*plotRG1Lsimp=Plot[#/.\[Epsilon]->2,{b,0,endRange},PlotStyle->RGBColor[0, 0, 1],PlotRange->All,PlotLegends->Placed[{Row[{"NEW 1-Loop: ",TraditionalForm[#]}]},{Right,Top}]]&@dfRG1Lsimp;*)
(**)
(*plotRG2Lsimp=Plot[#/.\[Epsilon]->2,{b,0,endRange},PlotStyle->RGBColor[0, 1, 1],PlotRange->All,PlotLegends->Placed[{Row[{"NEW 2-Loop: ",TraditionalForm[#]}]},{Right,Top}]]&@dfRG2Lsimp;*)
(**)
(**)
(*(*plotRG2Lwf=Plot[dfRG2Lwf/.\[Epsilon]->2/.a->+3,{b,0,endRange},PlotStyle->,PlotRange->All];*)*)
(*(*plotRG2L=Plot[dfRG2L/.\[Epsilon]->2,{b,0,endRange},PlotStyle->,PlotRange->All];*)*)
(**)
(*Show[{plotSLE*)
(*,plotRG1L*)
(*,plotRG1Lsimp*)
(*,plotRG2Lsimp*)
(*,Simulation2d*)
(*,Simulation2dGemini*)
(*(*,plotRG2Lwf*)
(*,plotRG2L*)}*)
(*,PlotRange->All,AxesLabel->{b,Subscript[d, f]},AxesOrigin->{0,1},PlotLabel->Row[{"d = 2"}](*,PlotLegends->Placed["AllExpressions", {Right,Top}]*),ImageSize->700, AspectRatio->0.7*)
(*]*)


(* ::Input:: *)
(*(* The errors are under hestimated *)*)


(* ::Item::Closed:: *)
(*Plot for Kay*)


(* ::Input:: *)
(*endRange=5;*)
(*dfRG2Lwf:=dfWF;*)
(**)
(**)
(*Simulation2d=ListPlot[{{1,Around[1.2486744695483691`, 0.023270605268075166`]},{2,Around[1.1151146520584079`, 0.0134148268356009]},{3,Around[1.0768665526174213`, 0.014642777375504247`]},{4,Around[1.0474461998303197`, 0.008312476568391155]},{5,Around[1.0454880607320536`, 0.0064093876238445445`]}},PlotStyle->{RGBColor[0, 1, 0],PointSize[0.005]},PlotLegends->Placed[{"Simulation Data (old)"},{Right,Top}]];*)
(**)
(**)
(*Simulation2dGemini=ListPlot[{{0, Around[1.7534581201029278`,0.019063147975801702`](*1.753\[PlusMinus]0.006*)}*)
(*,{1,Around[1.25127,0.0214579](*1.275\[PlusMinus]0.008*)}*)
(*,{2,Around[1.148,0.014](*1.1659\[PlusMinus]0.0019*)}*)
(*,{3,Around[1.1072,0.0120271](*1.1073\[PlusMinus]0.0024*)}*)
(*,{4,Around[1.08667,0.01](*1.0737\[PlusMinus]0.0018*)}*)
(*,{5,Around[1.06705,0.006](*1.0670\[PlusMinus]0.0012*)}(*,{10,1.0251\[PlusMinus]0.0012}*)}*)
(*,PlotStyle->{RGBColor[0, 0.66, 0],PointSize[0.005]},(*PlotMarkers->x,*)PlotLegends->Placed[{Style["Simulated Data \!\(\**)
(*StyleBox[\"d\",\nFontSlant->\"Italic\"]\)=2"(* (Gemini-opt1)"*),FontFamily->"Times"]},{Right,Top}]];*)
(**)
(*plotSLE=Plot[dfSLE,{b,0,endRange},PlotStyle->RGBColor[1, 0, 0],PlotRange->All, PlotLegends->Placed[{Style[Row[{"SLE: \!\(\*SubscriptBox[\(d\), \(f\)]\) = ",TraditionalForm[#]}]&@dfSLE,FontFamily->"Times"]},{Right,Top}]];*)
(**)
(*plotRG1L=Plot[#/.\[Epsilon]->2,{b,0,endRange},PlotStyle->RGBColor[0.64, 0, 1],PlotRange->All,PlotLegends->Placed[{Row[{"OLD 1-Loop: ",TraditionalForm[#]}]},{Right,Top}]]&@dfRG1L;*)
(**)
(*plotRG1Lsimp=Plot[#/.\[Epsilon]->2,{b,0,endRange},PlotStyle->RGBColor[0, 0, 1],PlotRange->All,PlotLegends->Placed[{Style[Row[{"FT@1-Loop: \!\(\*SubscriptBox[\(d\), \(f\)]\) = ",TraditionalForm[#],"\!\(\*SubscriptBox[\(|\), \(\[Epsilon] = 2\)]\)"}],FontFamily->"Times"]},{Right,Top}]]&@dfRG1Lsimp;*)
(**)
(*plotRG2Lsimp=Plot[#/.\[Epsilon]->2,{b,0,endRange},PlotStyle->RGBColor[0, 1, 1],PlotRange->All,PlotLegends->Placed[{Style[Row[{"FT@2-Loop: \!\(\*SubscriptBox[\(d\), \(f\)]\) = ",TraditionalForm[#],"\!\(\*SubscriptBox[\(|\), \(\[Epsilon] = 2\)]\)"}],FontFamily->"Times"]},{Right,Top}]]&@dfRG2Lsimp;*)
(**)
(**)
(*(*plotRG2Lwf=Plot[dfRG2Lwf/.\[Epsilon]->2/.a->+3,{b,0,endRange},PlotStyle->,PlotRange->All];*)*)
(*(*plotRG2L=Plot[dfRG2L/.\[Epsilon]->2,{b,0,endRange},PlotStyle->,PlotRange->All];*)*)
(**)
(*Show[{plotSLE*)
(*(*,plotRG1L*)*)
(*,plotRG1Lsimp*)
(*,plotRG2Lsimp*)
(*(*,Simulation2d*)*)
(*,Simulation2dGemini*)
(*(*,plotRG2Lwf*)
(*,plotRG2L*)}*)
(*,PlotRange->{0,2},AxesLabel->{b,Subscript[d, f]},AxesOrigin->{0,0}(*,PlotLabel->Row[{"d = 2"}]*)(*PlotLegends->Placed["AllExpressions", {Right,Top}]*)(*, ImageSize->100*)*)
(*]*)


(* ::Subsection:: *)
(*\[Section]\[Section] 3d*)


(* ::Input::Initialization:: *)
dfRG1L:=2-b \[Epsilon]/(2+b)
dfRG1Lsimp:=2-(b \[Epsilon])/(1+2 b)

dfRG2Lsimp:=2-(b \[Epsilon])/(1+2 b)-(b (1+b+4 b^2) \[Epsilon]^2)/(2 (1+2 b)^3)
dfRG2Lsimp2:=2-(b \[Epsilon])/(1+2 b)+(b (-1+b-8 b^2) \[Epsilon]^2)/(2 (1+2 b)^3)

dfRG2Lwf:=dfWF
dfRG2L:=2-b \[Epsilon]/(2+b)-b (\[Epsilon]/(2+b))^2

(*dfRG2Lsimp:=2-(b \[Epsilon])/(1+2 b)-(b ^2 \[Epsilon]^2)/(1+2 b)^2*)(*BAD*)

dfSLE=1+3/(4(2b+1));


(* ::Input:: *)
(*PPrint[{#,"->"},#/.b->0/.\[Epsilon]->2]&/@{dfRG1L,dfRG1Lsimp,dfRG2Lsimp,dfRG2Lwf,dfRG2L,dfRG2Lsimp2};*)
(**)
(*Limit[{dfRG1L,dfRG1Lsimp,dfRG2Lsimp,dfRG2Lwf,dfRG2L,dfRG2Lsimp2},b->\[Infinity]]//Quiet*)
(*%/.\[Epsilon]->1*)


(* ::Subsubsection:: *)
(*Extra data from my simulations*)


(* ::Input:: *)
(*(* It actually seems the corect functional form *)*)


(* ::Input:: *)
(*(* I use a big number instead of \[Infinity] as it cannot handle it*)*)


(* ::Input:: *)
(*model=2-d b/(a+2b);*)
(*nlm=NonlinearModelFit[{{0,2},{1,1.624}(*,{2,1.47}*),{100000,1.5}},{model},{a,d},b]*)


(* ::Input:: *)
(*fitFunc=model/.nlm["BestFitParameters"];*)


(* ::Input:: *)
(*(* OR *)*)


(* ::Input:: *)
(*fitFunc=Fit[{{0,2},{1,1.624}(*,{2,1.47}*),{100000,1.5}},{2,b/(2+b),(b/(2+b))^2},{b}]*)


(* ::Input:: *)
(*dfRG1Lsimp*)


(* ::Input:: *)
(*Limit[dfRG1Lsimp/.\[Epsilon]->1,b->\[Infinity]]*)


(* ::Input:: *)
(*inRange=0;*)
(*endRange=5;*)
(**)
(**)
(*Simulation3d=ListPlot[{{1,1.624}(*{0,2},{1,1.624},{2,Around[1.511,0.039]},{3,Around[1.483,0.028]},{4,Around[1.431,0.016]},{5,Around[1.436,0.016]}*)(*,{10,}*)},PlotStyle->{RGBColor[1, 0, 0],PointSize[0.015]},PlotLegends->Placed[{Style["Result by David Wilson"(* (Gemini-opt1)"*),FontFamily->"Times"]},{Right,Top}]];*)
(**)
(*Simulation3dGemini=ListPlot[{{0,Around[2,0.02]},{1,Around[1.61133,0.03]},{2,Around[1.511,0.039]},{3,Around[1.483,0.028]},{4,Around[1.431,0.036]},{5,Around[1.436,0.036]}(*,{10,}*)}(*{(*{0,1.753\[PlusMinus]0.006},*){2,Around[1.51,0.01]}(*,{3,1.1073\[PlusMinus]0.0024},{4,1.0737\[PlusMinus]0.0018},{5,1.0670\[PlusMinus]0.0012},{10,1.0251\[PlusMinus]0.0012}*)}*),PlotStyle->{RGBColor[0, 0.66, 0],PointSize[0.01]},PlotLegends->Placed[{Style["Simulated Data \!\(\**)
(*StyleBox[\"d\",\nFontSlant->\"Italic\"]\)=3"(* (Gemini-opt1)"*),FontFamily->"Times"]},{Right,Top}]];*)
(**)
(*plotRG1L=Plot[#/.\[Epsilon]->1,{b,inRange,endRange},PlotStyle->RGBColor[0.64, 0, 1],PlotRange->All,PlotLegends->Placed[{Row[{"OLD 1-Loop: ",TraditionalForm[#]}]},{Right,Top}]]&@dfRG1L;*)
(**)
(*plotRG1Lsimp=Plot[#/.\[Epsilon]->1,{b,inRange,endRange},PlotStyle->RGBColor[0, 0, 1],PlotRange->All,PlotLegends->Placed[{Style[Row[{"FT@1-Loop: \!\(\*SubscriptBox[\(d\), \(f\)]\) = ",TraditionalForm[#],"\!\(\*SubscriptBox[\(|\), \(\[Epsilon] = 3\)]\)"}],FontFamily->"Times"]},{Right,Top}]]&@dfRG1Lsimp;*)
(**)
(*plotRG2Lsimp=Plot[#/.\[Epsilon]->1,{b,0,endRange},PlotStyle->RGBColor[0, 1, 1],PlotRange->All,PlotLegends->Placed[{Style[Row[{"FT@2-Loop: \!\(\*SubscriptBox[\(d\), \(f\)]\) = ",TraditionalForm[#],"\!\(\*SubscriptBox[\(|\), \(\[Epsilon] = 3\)]\)"}],FontFamily->"Times"]},{Right,Top}]]&@dfRG2Lsimp;*)
(**)
(*plotRG2Lsimp2=Plot[#/.\[Epsilon]->1,{b,0,endRange},PlotStyle->RGBColor[0, 0.8, 0.25],PlotRange->All,PlotLegends->Placed[{Style[Row[{"FT@2-Loop: \!\(\*SubscriptBox[\(d\), \(f\)]\) = ",TraditionalForm[#],"\!\(\*SubscriptBox[\(|\), \(\[Epsilon] = 3\)]\)"}],FontFamily->"Times"]},{Right,Top}]]&@dfRG2Lsimp2;*)
(**)
(*(*plotRG2Lwf=Plot[dfRG2Lwf/.\[Epsilon]->1/.a->+3,{b,inRange,endRange},PlotStyle->,PlotRange->All];*)
(*plotRG2L=Plot[dfRG2L/.\[Epsilon]->1,{b,inRange,endRange},PlotStyle->,PlotRange->All];*)*)
(**)
(**)
(*(*fitPlot=Plot[fitFunc,{b,inRange,endRange},PlotStyle->Red,PlotRange->All];*)*)
(**)
(**)
(*Show[{(*plotRG1L*)
(*,*)plotRG1Lsimp*)
(*,plotRG2Lsimp*)
(*,plotRG2Lsimp2(*,plotRG2Lwf,plotRG2L*)(*,fitPlot*)*)
(*,Simulation3d*)
(*,Simulation3dGemini(*,Graphics[{Red,Text[Style["Result \nby David Wilson"(* (Gemini-opt1)"*),FontFamily->"Times"],{1,1.45}]}]*)*)
(*},PlotRange->{{0,5},{1,2}},AxesLabel->{b,Subscript[d, f]},AxesOrigin->{0,1.3},ImageSize->Large,PlotLabel->Row[{"d = 3"}]]*)


(* ::Text:: *)
(*THIS LOOKS VERY PROMISING!!! I'M USING THE REPLACEMENT rule {GradImmediateIntNotAllowed:>0,h->1,h2->1,a2->1-a-3/b,a->0,h->h,h2->h2}*)


(* ::Input:: *)
(*fitFunc/.b->3*)
