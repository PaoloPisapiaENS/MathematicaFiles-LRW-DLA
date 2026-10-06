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
(*FS*)


(* ::Input:: *)
(*$Paolofontsize=15*)
(*$Paolofont*)


(* ::Input:: *)
(*(*Quit*)*)


(* ::Input:: *)
(*(*FrontEndTokenExecute["SelectAll"]*)
(*FrontEndTokenExecute["SelectionCloseAllGroups"]*)*)


(* ::Input::Initialization:: *)
$Assumptions=b>0


(* ::Title:: *)
(*\[Beta]Function[] and \[Gamma]Function[]*)


(* ::Section:: *)
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


(* ::Subsection:: *)
(*\[Beta]FunctionFromZ: I can write \[Beta] as  *)
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


(* ::Item:: *)
(*Verification for R' prime operation: g=g0-Sqrt[(A+B/2)] g0^2 banana +g0^3(A doubleBanana+B (1/(2\[Epsilon]^2)+1/(4\[Epsilon]))) < => g0=g+Sqrt[(A+B/2)] g^2 banana -g0^3(- A doubleBanana+B (-(1/(2\[Epsilon]^2))+1/(4\[Epsilon])))  ????  {*)
(* {YES, !!!}*)
(*}*)
(**)
(*This amounts to  g=g0-Sqrt[(A+B/2)] g0^2 banana +(g0^3) (A doubleBanana+B  hat) /.{banana ->-1/\[Epsilon],doubleBanana ->1/\[Epsilon]^2,hat ->1/(2\[Epsilon]^2)-1/(4\[Epsilon]),sunset->-1/(8\[Epsilon])}*)


(* ::Input:: *)
(*gr=g0-Sqrt[(A+B/2)] g0^2 1/\[Epsilon] +g0^3 (A 1/\[Epsilon]^2+B (1/(2\[Epsilon]^2)+1/(4\[Epsilon])));*)
(**)
(*gB=(g0+(g-gr)//Expand)+O[\[Gamma]]^(3+1);*)
(**)
(*gB=(gB/.{g->g \[Gamma],g0->g0 \[Gamma]});*)
(**)
(*gB=(gB//.g0->gB/\[Gamma])//Expand;*)
(*gB=Normal[gB]/.\[Gamma]->1//Collect[#,{g,B},Expand]&*)
(**)
(*g0-Sqrt[(A+B/2)] g0^2 banana +g0^3 (A doubleBanana+B hat) /.{banana ->-1/\[Epsilon],doubleBanana ->1/\[Epsilon]^2,hat ->1/(2\[Epsilon]^2)-1/(4\[Epsilon]),sunset->-1/(8\[Epsilon])}*)
(**)
(*Clear[g,g0,gr,gB];*)
(**)


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


(* ::Subsection::Closed:: *)
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

gB=(gB/. {g->g  \[Gamma],g0->g0  \[Gamma],a_g:>a \[Gamma]});

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



(* ::Title:: *)
(*\[Section] 2-Loop after Simplification *)


(* ::Input::Initialization:: *)
introduce\[Lambda]={bananag->bananag \[Lambda],banana\[Gamma]1->banana\[Gamma]1 \[Lambda],banana\[Gamma]2->banana\[Gamma]2 \[Lambda],banana\[Gamma]Paolo->banana\[Gamma]Paolo \[Lambda]^2,banana\[Gamma]Grad->banana\[Gamma]Grad \[Lambda]^2,bananaMultigCT->bananaMultigCT \[Lambda],bananaMultigCTGrad->bananaMultigCTGrad \[Lambda],banana\[Gamma]PlusCT->banana\[Gamma]PlusCT \[Lambda],banana\[Gamma]PaoloCT->banana\[Gamma]PaoloCT \[Lambda],banana\[Gamma]GradCT->banana\[Gamma]GradCT \[Lambda]^2,banana\[Gamma]MinusCT->banana\[Gamma]MinusCT \[Lambda],banana\[Gamma]2CT->banana\[Gamma]2CT \[Lambda]};

hideSubDivs={bananag->banana,banana\[Gamma]1->banana,banana\[Gamma]2->banana, banana\[Gamma]Paolo->banana,banana\[Gamma]Grad->banana,banana\[Gamma]GradProp->banana,bananaMultigCT->banana ,bananaMultigCTGrad->banana,banana\[Gamma]PlusCT->banana,banana\[Gamma]PaoloCT->banana,banana\[Gamma]GradCT->banana,banana\[Gamma]MinusCT->banana,banana\[Gamma]2CT->banana,bananaJ->banana,bananaMultigPaoloCT->banana,banana\[Gamma]GradPropCT->banana,

doubleBananag->doubleBanana,doubleBanana\[Gamma]1g->doubleBanana,doubleBanana\[Gamma]Grad->doubleBanana,doubleBanana\[Gamma]Grad\[Gamma]2->doubleBanana,doubleBanana\[Gamma]Paolog-> doubleBanana,doubleBanana\[Gamma]2g-> doubleBanana,doubleBananaExtraGrad->doubleBanana,doubleBananaGradMultig->doubleBanana,doubleBananaGrad\[Gamma]Plus->doubleBanana,doubleBananaGrad\[Gamma]PlusNOsub->doubleBanana,doubleBananaGrad\[Gamma]2->doubleBanana,doubleBananaGrad\[Gamma]2NOsub->doubleBanana,doubleBananaGrad\[Gamma]MinusNOsub->doubleBanana,doubleBanana\[Gamma]1\[Gamma]2->doubleBanana,doubleBanana\[Gamma]1\[Gamma]Paolo->doubleBanana,doubleBananaMissing->doubleBanana,doubleBananaMissingNOsub->doubleBanana,

hatg->hat,hat\[Gamma]1->hat,hat\[Gamma]2->hat,hat\[Gamma]2\[Gamma]2->hat,hatg\[Gamma]1->hat, hat\[Gamma]1g->hat,hatg\[Gamma]2\[Gamma]1->hat,hat\[Gamma]Paolo->hat,hat\[Gamma]Grad->hat,hat\[Gamma]2g ->hat,hatg\[Gamma]2 ->hat,hat\[Gamma]1\[Gamma]2 ->hat,hat\[Gamma]Paolo\[Gamma]1 ->hat,hat\[Gamma]Paolo\[Gamma]2->hat,hat\[Gamma]Paolog ->hat,hat\[Gamma]Paolo\[Gamma]2g->hat,hatExtraGrad->hat,hatGradMultig->hat,hatMultigGrad->hat,hatGrad\[Gamma]Plus->hat,hatGrad\[Gamma]PlusNOsub->hat,hatMultig\[Gamma]Paolo->hat,hatGrad\[Gamma]2NOsub->hat,hatGrad\[Gamma]MinusNOsub->hat,hatProp->hat, hatMissingNOsub ->hat, hatMissing->hat,

sunsetPaolo->sunset};


(* ::Input::Initialization:: *)
replaceDiagrams={banana ->1/\[Epsilon],doubleBanana ->1/\[Epsilon]^2,hat ->1/(2\[Epsilon]^2)+1/(4\[Epsilon]),sunset->-1/(8\[Epsilon])};
replaceDiagramsRPrime={banana ->-1/\[Epsilon],doubleBanana ->1/\[Epsilon]^2,hat ->1/(2\[Epsilon]^2)-1/(4\[Epsilon]),sunset->+1/(8\[Epsilon])};


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


(* ::Subsection:: *)
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


(* ::Item:: *)
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


(* ::Section:: *)
(*\[Section]\[Section]\[Section] \[Gamma]-functions After splitting the contributions: b=1*)


(* ::Subsection:: *)
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


(* ::Section::Closed:: *)
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
(*GradImmediateIntNotAllowed/:(GradImmediateIntNotAllowed->1):={GradImmediateIntNotAllowed:>1,h->0,h2->0}*)


(* ::Input::Initialization:: *)
(* IN WHAT FOLLOWS, I SUB doubleBanana-> MINUS 1/\[Epsilon]^2. SO HERE I NEED TO SUM THE BANANA SQUARED. Actually, the replacement ALREADY implements the partial subtraction of subdivergencies *)

(* GradImmediateIntNotAllowed=0 then it is not allowed. To implement it also for \[Gamma]1 and \[Gamma]2, one should set h,h2->1*)
GradImmediateIntNotAllowed/:(GradImmediateIntNotAllowed->0):={GradImmediateIntNotAllowed:>0,h->1,h2->1,H->0}
GradImmediateIntNotAllowed/:(GradImmediateIntNotAllowed->1):={GradImmediateIntNotAllowed:>1,h->0,h2->0,H->1}

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


(* ::Subsection:: *)
(*Using \[Beta]FunctionFromZ[] 	USING R' OPERATION, I.E. O(1/\[Epsilon])-> -O(1/\[Epsilon])*)


(* ::Subsubsection:: *)
(*Splitting the contributions*)


(* ::Text:: *)
(*To make sense of this we differentiate the diagrams according to the subdivergences*)


(* ::Text:: *)
(*TURNS OUT I COULD NEED THE GradImmediateIntNotAllowed!!!*)
(**)
(*!!!!!!! AND I WAS MISSING 4 DIAGRAMSSSSSSS !!!!!!!*)


(* ::Text:: *)
(*1) I AM STILL MISSING THE CT FOR THE DELAYED RED-GREEN*)
(*2) THE "ABSORBER" IS NOT A REAL OBS OF THE THEORY: IT DISAPPEARS AFTER INTEGATING \[Psi] OUT (nor is the emitter for the interaction, for that matter. The remaining emitter is useful only for the pass-through-a-point obs) CORRIGE: IT IS, AND ITS RG FUNCTION IS INDEED FINITE: THE PROBLEM CAME FROM SOME EXTRA banana\[Gamma]Paolo^2 THAT ARE NOT SUPPOSED TO BE THERE*)


(* ::Input::Initialization:: *)
replaceRule={GradImmediateIntNotAllowed:>0,h->1,h2->1,H->0,H2->0,a2->1-a-3/b,a->0,A2->1(*2+3/b-A*),A->1(*,K->1+3b/2*)};
(*J\[Rule]5/2+(5/2-11 b)(b-1)(*1/2 (27-22 b) b*)*)

replaceRule={GradImmediateIntNotAllowed:>1,h->0,h2->0,H->1,H2->1,(*a2\[Rule]1-a-3/b,a\[Rule]0,*)A2->1(*2+3/b-A*),A->1(*,K->1+3b/2*)};


(* ::Input:: *)
(*(*Logic change: I write A and H in front of the diagrams we obtain from the Grad term. Before, we used a and h to subtract these terms from the complete expression.*)*)
(*\[CapitalGamma]\[Gamma]1small =(-b banana g0 \[Mu]^-\[Epsilon]+b g0^2 (b doubleBanana\[Gamma]1g-(b-1) doubleBanana\[Gamma]Grad+a Hold[b-1] doubleBanana\[Gamma]Grad-h hat+b (2+h) hat) \[Mu]^(-2 \[Epsilon]) z["\[Gamma]1"]);*)
(**)
(*\[CapitalGamma]\[Gamma]1 =(-b banana\[Gamma]1 g0 \[Mu]^-\[Epsilon]+ g0^2 \[Mu]^(-2 \[Epsilon]) b(b doubleBanana\[Gamma]1g- A Hold[b-1]doubleBanana\[Gamma]Grad +(b hatg\[Gamma]1+b hat\[Gamma]1+b hatg\[Gamma]2\[Gamma]1-hat\[Gamma]Paolo\[Gamma]1)-H Hold[b-1](hat\[Gamma]Grad-banana\[Gamma]GradProp^2/2))  z["\[Gamma]1"]);*)
(*PPrint[{%," = "},%,"\n"]*)
(**)
(*(*Logic change: I write A2 and H2 in front of the diagrams we obtain from the Grad term. Before,we used a2 and h2 to subtract these terms from the complete expression.*)*)
(*\[CapitalGamma]\[Gamma]2small = - g0 \[Mu]^-\[Epsilon] (b-1)banana + g0^2 \[Mu]^(-2 \[Epsilon]) (2 b^2 hat - 2 hat + b(b-1)(a2 doubleBanana+h2 hat))z["\[Gamma]2"];*)
(**)
(*\[CapitalGamma]\[Gamma]2 =(-(b banana\[Gamma]2 - banana\[Gamma]Paolo)  g0 \[Mu]^-\[Epsilon]*)
(*+g0^2 \[Mu]^(-2 \[Epsilon]) (b^2 doubleBanana\[Gamma]2g -b doubleBanana\[Gamma]Paolog -b Hold[b-1] A2 doubleBanana\[Gamma]Grad\[Gamma]2 +b^2 hatg\[Gamma]2+b^2 hat\[Gamma]1\[Gamma]2+b^2 hat\[Gamma]2\[Gamma]2*)
(*- b hat\[Gamma]Paolo\[Gamma]2-2 hat\[Gamma]Paolo+K banana\[Gamma]PaoloCT^2-b Hold[b-1]H2 (hat\[Gamma]Grad-banana\[Gamma]GradProp^2/2)*)
(*-(* MISSING INT ABOVE 2) in my notes	. THESE SHOULD PROBABLY GO IN \[CapitalGamma]2 ACTUALLY*)*)
(* Hold[b-1]( hatMissing GradImmediateIntNotAllowed + (-doubleBananaMissingNOsub + 2 hatMissingNOsub))(*2 independent diagams, verified *)) z["\[Gamma]2"]);(*K should be K=1+b: half SUBDIV b*doubleBanana\[Gamma]Paolog + SUBDIV IN b*hat\[Gamma]Paolo\[Gamma]2 + SUBDIV IN hat\[Gamma]Paolo	.*)*)
(*(*HOWEVER, IF ONE DOES NOT ALLOW FOR banana*banana\[Gamma]Paolo, THEN THE VALUE OF K MUST BE K=1+3b/2! BECAUSE NOW ITS:*)
(* FULL SUBDIV b*doubleBanana\[Gamma]Paolog + SUBDIV IN b*hat\[Gamma]Paolo\[Gamma]2 + SUBDIV IN hat\[Gamma]Paolo	.*)*)
(*PPrint[{%," = "},%,{"\n=",\[CapitalGamma]\[Gamma]2//.replaceRule,"\n"}]*)
(*(*\[CapitalGamma]g =g0 \[Mu]^-\[Epsilon](1-(2 b bananag-2 (b-1)banana\[Gamma]Grad )g0 \[Mu]^-\[Epsilon]-g0^2\[Mu]^(-2 \[Epsilon]) (-4 (-1+b) doubleBanana+2 (-1+b) b doubleBanana+b^2 doubleBanana+2  hat+4 b hat+8 (-1+b) b hat+2 (-1+b)  GradImmediateIntNotAllowed hat-(-1+b) (2 doubleBanana+4 hat)+(-1+b) b (2 doubleBanana+4 hat)-b^2 (doubleBanana+6 hat)+(-1+b)(4 doubleBanana+8 hat)-b^2 (2 doubleBanana+10 hat))  z["g"]);*)*)
(**)
(*\[CapitalGamma]g =(1-(2 b bananag-2 Hold[b-1]banana\[Gamma]Grad )g0 \[Mu]^-\[Epsilon]*)
(*-g0^2 \[Mu]^(-2 \[Epsilon]) (1/b**)
(*(-b^3 (2 doubleBananag+4 hatg+2 hatg\[Gamma]1+4 hat\[Gamma]1g+2 hatg\[Gamma]2+4 hat\[Gamma]2g)(*Total of 18, verified*)*)
(*+(*ONLY \[Gamma]Grad 1) in my notes	.*)Hold[(b-1)]b(4 (doubleBananaGradMultig-1/2 bananaMultigCTGrad^2-1/2 bananaMultigCT*banana\[Gamma]GradCT)+4 (hatMultigGrad-1/2 bananaMultigCTGrad^2)+4 (hatGradMultig-1/2 bananaMultigCTGrad^2))(*6 independent diagams, verified*)*)
(*+(*ONLY \[Gamma]Grad 2) in my notes	.*)*)
(*6 Hold[(b-1)] b^2 hatExtraGrad  (*3 independent diagams, verified*)*)
(*+(*ONLY \[Gamma]Grad 3) in my notes	.*)*)
(*2 Hold[(b-1)] b GradImmediateIntNotAllowed hatExtraGrad (*2 independent diagams, verified*)*)
(*-(* \[Gamma]Grad \[Gamma]Plus 1) in my notes	.*)*)
(*Hold[(b-1)]b (4 (doubleBananaGrad\[Gamma]Plus-banana\[Gamma]PlusCT*banana\[Gamma]GradCT(*These originate from 2 different diagrams*) )*)
(*-2doubleBananaGrad\[Gamma]PlusNOsub+4 hatGrad\[Gamma]PlusNOsub) (*3 independent diagams, verified*)*)
(*-(* \[Gamma]Grad \[Gamma]Plus 2) in my notes	.*)*)
(*4 Hold[(b-1)] b (doubleBananaGrad\[Gamma]Plus-banana\[Gamma]PlusCT*banana\[Gamma]GradCT) (*3 independent diagams, OF WHICH 2 ARE 0, verified*)*)
(*+(* \[Gamma]Grad \[Gamma]Minus 1) in my notes	.*)*)
(*Hold[(b-1)] b^2 (4 (doubleBananaGrad\[Gamma]2-banana\[Gamma]2CT*banana\[Gamma]GradCT(*These originate from 4 different diagrams*))*)
(*-2doubleBananaGrad\[Gamma]2NOsub+4 hatGrad\[Gamma]2NOsub) (* 6 independent diagams, verified*)*)
(*+(* \[Gamma]Grad \[Gamma]Minus 2) in my notes	.*)*)
(*2 Hold[(b-1)] b^2 (doubleBananaExtraGrad-banana\[Gamma]MinusCT*banana\[Gamma]GradCT) (*3 independent diagams, OF WHICH 2 ARE 0, verified *)*)
(*+(* \[Gamma]Grad \[Gamma]Minus "MISSING:" in my notes	. *)*)
(*Hold[(b-1)] b^2 (4 hatGrad\[Gamma]MinusNOsub-2 doubleBananaGrad\[Gamma]MinusNOsub) (*2 independent diagams, verified *)*)
(*+(* \[Gamma]Grad \[Gamma]Minus 2bis) in my notes	.*)*)
(*2  Hold[(b-1)]b^2 hatExtraGrad GradImmediateIntNotAllowed (*2 independent diagams, verified *)*)
(*+(* ONLY \[Gamma]Paolo 1) in my notes	.*)*)
(*2 b (hatMultig\[Gamma]Paolo-1/2 bananaMultigPaoloCT^2) (*1 independent diagam, verified *)*)
(*+(* \[Gamma]Paolo \[Gamma]Minus 1) in my notes	.*)*)
(*4 b^2 hat\[Gamma]Paolo\[Gamma]2g (*4 independent diagams, verified *)*)
(*+(* MISSING INT ABOVE 1) in my notes	.*)*)
(*2b Hold[b-1] doubleBananaMissing (*2 independent diagams, verified *)*)
(*- J*b* bananaJ^2 ))); (* TOTAL OF 53+4 THAT WERE MISSING, VERIFIED *)*)
(**)
(*PPrint[{%," = "},%,"\n"]*)
(**)
(*\[CapitalGamma]\[Gamma] =1- g0^2 \[Mu]^(-2 \[Epsilon]) (b^2 sunset - b sunsetPaolo (*THESE ARE ACTUALLY ALWAYS PRESENT	!!!*)*)
(*+ 1/2 b Hold[b-1] GradImmediateIntNotAllowed ((hatProp-banana\[Gamma]GradPropCT^2/2)-sunset) )z["\[Gamma]"];*)
(*PPrint[{%," = "},%,"\n"]*)
(**)
(**)
(*\[CapitalGamma]\[Gamma]P =1- g0 \[Mu]^-\[Epsilon] (b+2)banana z["\[Gamma]P"];*)
(*PPrint[{%," = "},%,"\n"]*)


(* ::Input:: *)
(*loopOrder=2;*)
(**)
(*\[CapitalGamma]gtProduct=g0 \[Mu]^-\[Epsilon] ((1+\[CapitalGamma]\[Gamma]1) (1+ \[CapitalGamma]\[Gamma]2)\[CapitalGamma]g)/(\[CapitalGamma]\[Gamma]^2)/.z[_]->1/.\[Epsilon]->0(*//.replaceRule*);*)
(**)
(*\[CapitalGamma]gtPartialProd=g0 \[Mu]^-\[Epsilon] (((1+\[CapitalGamma]\[Gamma]1) (1+ \[CapitalGamma]\[Gamma]2)+\[CapitalGamma]g-1))/(\[CapitalGamma]\[Gamma]^2)/.z[_]->1(*//.replaceRule*)/.\[Epsilon]->0; *)
(**)
(*(*I should also multiply \[CapitalGamma]g, right? Maybe after having removed some cross terms	.*)*)
(*(*Or should I maybe not multiply anything at all and add the bananaEmit*bananaAbsorb into \[CapitalGamma]g, since it's not decomposable	??*)*)
(**)
(*FS/@(Series[%,{g0,0,loopOrder+1}]);*)
(*Normal[%]/.hideSubDivs ;*)


(* ::Input:: *)
(*replaceRule*)


(* ::Input:: *)
(*Zg=Collect[(Series[\[CapitalGamma]gtPartialProd//.replaceRule,{g0,0,loopOrder+1}]//Normal),{g0},FS];*)
(*PPrint[%,%]*)
(*(*g=Series[g0 \[Mu]^-\[Epsilon] Zgt^(-1),{g0,0,2}]//Normal*)*)
(**)
(*\[Beta]FunctionFromZ[Zg/.hideSubDivs ,3,{g0},"print"->tTrue](*It's slow if the subDivs are not hidden directly in g, I think it's just because it's a long expression*)*)
(*(**)
(*Replace[Normal[%],a_/;!(FreeQ[a,g^3]):>(a/.(*banana\[Gamma]Grad*) banana\[Gamma]Paolo->0),{1}];*)
(*%/.banana\[Gamma]Grad^2->0*)
(*%/.K->1+3b/2*)*)
(**)
(*%//.replaceRule;*)
(**)
(*%/.hideSubDivs *)
(*%/.replaceDiagramsRPrime//FullSimplify//Factor*)
(*ReleaseHold[%]//FS*)
(**)
(*RGeq2=Simplify[Normal[%]]==0;*)
(**)
(*Expand[%%]/.{J->1/2 (-1+b^2)}//Collect[#,g,FS]&*)
(*%/.b->1//Collect[#,g,FS]&(*This is if some explicit bananaCT are used*)*)
(**)


(* ::Item::Closed:: *)
(*Get the value of J*)


(* ::Input:: *)
(*-1+b^2-2 J//Collect[#,\[Epsilon],FS]&*)
(*%/. {\[Epsilon]->0,H2->0}*)
(*Solve[%==0,J]*)


(* ::Item::Closed:: *)
(*Check the subdivs		TBD*)


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
(*(*Here I use K and J, and explicit bananaCT	AND ADDED THE MISSING TERM.*)*)
(*\[Epsilon] g+\[Epsilon] (-b (2 bananag+banana\[Gamma]1+banana\[Gamma]2)+banana\[Gamma]Paolo+2 banana\[Gamma]Grad Hold[b-1]) g^2-2 (\[Epsilon] (-bananaMultigCTGrad^2+banana\[Gamma]Paolo^2+2 hatMultig\[Gamma]Paolo+b^2 ((2 bananag+banana\[Gamma]1)^2+(4 bananag+banana\[Gamma]1) banana\[Gamma]2+banana\[Gamma]2^2-2 doubleBananag-doubleBanana\[Gamma]1g-doubleBanana\[Gamma]2g-4 hatg-3 hatg\[Gamma]1-3 hatg\[Gamma]2-hatg\[Gamma]2\[Gamma]1-hat\[Gamma]1-4 hat\[Gamma]1g-hat\[Gamma]1\[Gamma]2-hat\[Gamma]2-4 hat\[Gamma]2g)+2 hat\[Gamma]Paolo+b (-((4 bananag+banana\[Gamma]1+2 banana\[Gamma]2) banana\[Gamma]Paolo)+doubleBanana\[Gamma]Paolog+hat\[Gamma]Paolo\[Gamma]1+hat\[Gamma]Paolo\[Gamma]2+4 hat\[Gamma]Paolo\[Gamma]2g)-banana^2 J-banana\[Gamma]PaoloCT^2 K+Hold[b-1] (-6 bananaMultigCTGrad^2-2 bananaMultigCT banana\[Gamma]GradCT+b (-4 (2 bananag+banana\[Gamma]1+banana\[Gamma]2) banana\[Gamma]Grad-2 banana\[Gamma]GradCT (2 banana\[Gamma]2CT+banana\[Gamma]MinusCT)+2 doubleBananaExtraGrad+4 doubleBananaGrad\[Gamma]2-2 doubleBananaGrad\[Gamma]2NOsub-2 doubleBananaGrad\[Gamma]MinusNOsub+doubleBanana\[Gamma]Grad+doubleBanana\[Gamma]Grad\[Gamma]2+6 hatExtraGrad+4 (hatGrad\[Gamma]2NOsub+hatGrad\[Gamma]MinusNOsub))+2 (2 banana\[Gamma]Grad banana\[Gamma]Paolo+4 banana\[Gamma]GradCT banana\[Gamma]PlusCT+2 doubleBananaGradMultig-4 doubleBananaGrad\[Gamma]Plus+doubleBananaGrad\[Gamma]PlusNOsub+2 (hatGradMultig-hatGrad\[Gamma]PlusNOsub+hatMultigGrad))+4 banana\[Gamma]Grad^2 Hold[b-1]))) g^3+SeriesData[g, 0, {}, 1, 4, 1];*)
(**)
(*Collect[%,{\[Epsilon] ,g,b},FS];*)
(*(*PPrint["\[Beta]",%,"style"->{FontSize\[Rule]16}];*)
(**)*)
(*(*ReleaseHold[%%];*)*)
(*Series[%%,{\[Epsilon] ,0,1},{g,0,3}(*,{b,0,2}*)];*)
(*%/.K->1+3b/2;*)
(*Collect[#,{b},FS]&/@%;*)
(*PPrint["\[Beta]",%,"style"->{FontSize->16}];*)
(**)
(*Replace[Normal[%%],a_/;!(FreeQ[a,g^3]):>(a/.(*banana\[Gamma]Grad*) banana\[Gamma]Paolo->0),{1}];*)
(*%/.banana\[Gamma]Grad^2->0*)
(**)
(*%/.hideSubDivs //FS*)
(*%/.replaceDiagrams//FullSimplify//Factor*)
(*%//.replaceRule*)


(* ::Subitem::Closed:: *)
(*Order b^2: Finite*)


(* ::Input:: *)
(*2 b^2 (-(2 bananag+banana\[Gamma]1)^2-(4 bananag+banana\[Gamma]1) banana\[Gamma]2-banana\[Gamma]2^2+2 doubleBananag+doubleBanana\[Gamma]1g+doubleBanana\[Gamma]2g+4 hatg+3 hatg\[Gamma]1+3 hatg\[Gamma]2+hatg\[Gamma]2\[Gamma]1+hat\[Gamma]1+4 hat\[Gamma]1g+hat\[Gamma]1\[Gamma]2+hat\[Gamma]2+4 hat\[Gamma]2g) /. hideSubDivs//FS*)
(*%/. replaceDiagrams//FullSimplify//Factor*)
(*%//.replaceRule*)


(* ::Input:: *)
(*(*FINITE!*)*)


(* ::Subitem::Closed:: *)
(*Order b^1: Not finite*)


(* ::Subsubitem::Closed:: *)
(*No b-1*)


(* ::Input:: *)
(*2 b \[Epsilon] (-((4 bananag+banana\[Gamma]1+2 banana\[Gamma]2) banana\[Gamma]Paolo)-(3 banana\[Gamma]PaoloCT^2)/2+doubleBanana\[Gamma]Paolog+hat\[Gamma]Paolo\[Gamma]1+hat\[Gamma]Paolo\[Gamma]2+4 hat\[Gamma]Paolo\[Gamma]2g+(-4 (2 bananag+banana\[Gamma]1+banana\[Gamma]2) banana\[Gamma]Grad-2 banana\[Gamma]GradCT (2 banana\[Gamma]2CT+banana\[Gamma]MinusCT)+2 doubleBananaExtraGrad+4 doubleBananaGrad\[Gamma]2-2 doubleBananaGrad\[Gamma]2NOsub-2 doubleBananaGrad\[Gamma]MinusNOsub+doubleBanana\[Gamma]Grad+doubleBanana\[Gamma]Grad\[Gamma]2+6 hatExtraGrad+4 (hatGrad\[Gamma]2NOsub+hatGrad\[Gamma]MinusNOsub)) Hold[b-1])/. Hold[b-1]->0//Expand//Collect[#,{b,\[Epsilon]}]&*)
(*%/. hideSubDivs//FS*)
(*%/. replaceDiagrams//FullSimplify//Factor*)
(*%//.replaceRule//Expand*)


(* ::Subsubitem::Closed:: *)
(*With b-1*)


(* ::Input:: *)
(*2 b \[Epsilon] (-((4 bananag+banana\[Gamma]1+2 banana\[Gamma]2) banana\[Gamma]Paolo)-(3 banana\[Gamma]PaoloCT^2)/2+doubleBanana\[Gamma]Paolog+hat\[Gamma]Paolo\[Gamma]1+hat\[Gamma]Paolo\[Gamma]2+4 hat\[Gamma]Paolo\[Gamma]2g+(-4 (2 bananag+banana\[Gamma]1+banana\[Gamma]2) banana\[Gamma]Grad-2 banana\[Gamma]GradCT (2 banana\[Gamma]2CT+banana\[Gamma]MinusCT)+2 doubleBananaExtraGrad+4 doubleBananaGrad\[Gamma]2-2 doubleBananaGrad\[Gamma]2NOsub-2 doubleBananaGrad\[Gamma]MinusNOsub+doubleBanana\[Gamma]Grad+doubleBanana\[Gamma]Grad\[Gamma]2+6 hatExtraGrad+4 (hatGrad\[Gamma]2NOsub+hatGrad\[Gamma]MinusNOsub)) Hold[b-1]);*)
(*(%/2-(%/2/. Hold[b-1]->0)//Expand//Collect[#,{b,\[Epsilon],Hold[b-1]}]&)*2*)
(*%/. hideSubDivs//FS*)
(*%/. replaceDiagrams//FullSimplify//Factor*)
(*%//.replaceRule//Expand*)


(* ::Subitem::Closed:: *)
(*Order b^0: Finite (the trick with \[Lambda] is not super accurate, it only works with \[Gamma]Grad I guess. \[Gamma]P has to be dealt with differently)*)


(* ::Input:: *)
(*-2 \[Epsilon] (-bananaMultigCTGrad^2+banana\[Gamma]Paolo^2-banana\[Gamma]PaoloCT^2+2 (hatMultig\[Gamma]Paolo+hat\[Gamma]Paolo)-banana^2 J+2 Hold[b-1] (-3 bananaMultigCTGrad^2-bananaMultigCT banana\[Gamma]GradCT+2 banana\[Gamma]Grad banana\[Gamma]Paolo+4 banana\[Gamma]GradCT banana\[Gamma]PlusCT+2 doubleBananaGradMultig-4 doubleBananaGrad\[Gamma]Plus+doubleBananaGrad\[Gamma]PlusNOsub+2 (hatGradMultig-hatGrad\[Gamma]PlusNOsub+hatMultigGrad)+2 banana\[Gamma]Grad^2 Hold[b-1])) /.J->0//FS;*)
(*Series[%/. introduce\[Lambda],{\[Lambda],0,4}]*)
(*Normal@Series[%,{\[Lambda],0,3}]/.\[Lambda]->1//Expand//Collect[#,{b,\[Epsilon],Hold[b-1]}]&*)
(*%/.hideSubDivs//FS*)
(*%/. replaceDiagrams//FullSimplify//Factor*)
(*%//.replaceRule//FS*)


(* ::Item:: *)
(* banana\[Gamma]Paolo^2- banana\[Gamma]Grad^2-banana\[Gamma]Grad*banana\[Gamma]Paolo ARE ORDER \[Lambda]^4, I.E. THEY CONTAIN 4 GREEN LINES. CAN WE USE THEM????*)
(*NO, I THINK THAT banana\[Gamma]Paolo SHOULD BE USED TO MULTIPLY OTHER bananas IN CT*)


(* ::Subsubsection::Closed:: *)
(*g* at 2loop*)


(* ::Input:: *)
(*(* WITH GRAD AND THE 4 DIAGS I WAS MISSING	!!!*)*)
(*RGeq2/. J->1/2 (-5+10 b-12 b^2)(*banana\[Gamma]MinusCT->1/\[Epsilon]}*)//Collect[#,g,FS]&*)
(*gstar2=Select[Flatten@SolveValues[%,g],#=!=0&];*)
(**)
(*gstar2=Series[gstar2,{\[Epsilon],0,loopOrder},Assumptions->b>0]//Expand*)
(*%/.\[Epsilon]->0*)
(*gstar2=Select[Normal@gstar2,(#/.\[Epsilon]->0)==0&][[1]]*)
(*gstar2=Factor/@gstar2*)
(*%/.b->1*)


(* ::Input:: *)
(*(* NO GRAD AND THE 4 DIAGS I WAS MISSING	!!!*)*)
(*RGeq2/. J->1/2 (-2+9 b-14 b^2)(*banana\[Gamma]MinusCT->1/\[Epsilon]}*)//Collect[#,g,FS]&*)
(*gstar2=Select[Flatten@SolveValues[%,g],#=!=0&];*)
(**)
(*gstar2=Series[gstar2,{\[Epsilon],0,loopOrder},Assumptions->b>0]//Expand*)
(*%/.\[Epsilon]->0*)
(*gstar2=Select[Normal@gstar2,(#/.\[Epsilon]->0)==0&][[1]]*)
(*gstar2=Factor/@gstar2*)
(*%/.b->1*)


(* ::Input:: *)
(*(* With Rprime	! SAME AS BEFORE! CORRECT IMPLEMENTATION AT LEAST *)*)
(*RGeq2/. J->1/2 (-1+b^2)(*banana\[Gamma]MinusCT->1/\[Epsilon]}*)//Collect[#,g,FS]&*)
(*gstar2=Select[Flatten@SolveValues[%,g0],#=!=0&];*)
(**)
(*gstar2=Series[gstar2,{\[Epsilon],0,loopOrder},Assumptions->b>0]//Expand*)
(*%/.\[Epsilon]->0*)
(*gstar2=Select[Normal@gstar2,(#/.\[Epsilon]->0)==0&][[1]]*)
(*gstar2=Factor/@gstar2*)
(*%/.b->1*)


(* ::Input:: *)
(*(*b=1	:*)*)
(*\[Epsilon]/3+(2 \[Epsilon]^2)/9*)


(* ::Subsection::Closed:: *)
(*RG functions: \[CapitalGamma]\[Gamma]1 & Subscript[d, f]*)


(* ::Input:: *)
(*(* WITHOUT WF	! *)*)
(**)
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
(*Print[Style[Row[{"Replace with \!\(\*SuperscriptBox[\(g\), \(*\)]\)= ",gstar2}],RGBColor[0, 0, Rational[2, 3]]]]*)
(*%%/.g->gstar2+O[\[Epsilon]]^3*)
(*%//FS*)
(**)
(*2+Normal@%*)


(* ::Input:: *)
(*df=2-(b \[Epsilon])/(1+2 b)-(b (1+b+4 b^2) \[Epsilon]^2)/(2 (1+2 b)^3)*)


(* ::Input:: *)
(*(* WITH WF	! *)*)
(**)
(*(*If GradImmediateIntNotAllowed\[RuleDelayed]0, then following is the same as above*)*)
(*g=Collect[(Series[\[CapitalGamma]gtPartialProd//.replaceRule,{g0,0,loopOrder+1}]//Normal),{g0},FS];*)
(*(1+\[CapitalGamma]\[Gamma]1)/(\[CapitalGamma]\[Gamma])/.z[_]->1;*)
(*FS/@(%/.replaceRule)*)
(**)
(*obsWithZ=\[Gamma]Function[%,g,"print"->tTrue]*)
(*(*%/.b->1*)*)
(*%/. hideSubDivs*)
(*%/. replaceDiagrams//FullSimplify//Factor*)
(*ReleaseHold[%]//FS*)
(**)
(*Print[Style[Row[{"Replace with \!\(\*SuperscriptBox[\(g\), \(*\)]\)= ",gstar2}],RGBColor[0, 0, Rational[2, 3]]]]*)
(*%%/.g->gstar2+O[\[Epsilon]]^3*)
(*%//FS*)
(**)
(*2+Normal@%*)
(*Print["df = ",%]*)
(*%%/.b->1*)


(* ::Input:: *)
(*(*b=1 :*)*)
(*2-\[Epsilon]/3-\[Epsilon]^2/9*)


(* ::Subsection:: *)
(*RG functions: \[CapitalGamma]\[Gamma]2		not finite (is this an observable of the theory?) THIS CANNOT WORK: I THINK I HAVE TO SPLIT THE PURE \[Gamma]L AND THE \[Gamma]PAOLO. THE REASON IS THAT I GET EXTRA, UNNECESSARY SUBTRACTION OF SUBDIVS, SUCH AS banana\[Gamma]Grad banana\[Gamma]Paolo		THIS WAY IT IS FINITE!!!!!*)


(* ::Subsubsection::Closed:: *)
(*WITHOUT WF REN*)


(* ::Item::Closed:: *)
(*Together with manual simplification*)


(* ::Input:: *)
(*loopOrder=2;*)
(**)
(*\[CapitalGamma]\[Gamma]2 =(-(b banana\[Gamma]2 - banana\[Gamma]Paolo)  g0 \[Mu]^-\[Epsilon]+g0^2 \[Mu]^(-2 \[Epsilon]) (b^2 doubleBanana\[Gamma]2g -b doubleBanana\[Gamma]Paolog -b Hold[b-1] A2 doubleBanana\[Gamma]Grad\[Gamma]2 +b^2 hatg\[Gamma]2+b^2 hat\[Gamma]1\[Gamma]2+b^2 hat\[Gamma]2\[Gamma]2*)
(*- b hat\[Gamma]Paolo\[Gamma]2-2 hat\[Gamma]Paolo+K banana\[Gamma]PaoloCT^2-b Hold[b-1]H2 hat\[Gamma]Grad) z["\[Gamma]2"]);*)
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


(* ::Item::Closed:: *)
(*Without removal by hand: using \[Lambda] (a bit messy, better think it through)*)


(* ::Input:: *)
(*loopOrder=2;*)
(**)
(*(*replaceRule=Flatten@{GradImmediateIntNotAllowed->0,a2->-3/(b)+1-a,a->0,h->h,h2->h2};*)*)
(*g=Normal[Series[\[CapitalGamma]gt,{g0,0,loopOrder+1}]]/.replaceRule;*)
(**)
(*(1+\[CapitalGamma]\[Gamma]2)/(\[CapitalGamma]\[Gamma])^0/.z[_]->1;*)
(*FS/@(%/.replaceRule);*)
(**)
(*\[Gamma]Function[%,g,"print"->tTrue];*)
(**)
(*Replace[Normal[%],a_/;!(FreeQ[a,g^2]):>(a/.(*banana\[Gamma]Grad*) banana\[Gamma]Paolo->0),{1}]*)
(*Normal@%%/. introduce\[Lambda]//Expand//Collect[#,{g,\[Epsilon],\[Lambda],Hold[b-1]},FS]&*)
(*%/.\[Lambda]^n_/;n>3->0(*/.\[Lambda]->1*)*)
(**)
(*%%%-%//FS*)
(*%/.\[Lambda]->1*)


(* ::Input:: *)
(*introduce\[Lambda]={bananag->bananag \[Lambda]^(3/2),banana\[Gamma]1->banana\[Gamma]1 \[Lambda]^(3/2),banana\[Gamma]2->banana\[Gamma]2 \[Lambda],banana\[Gamma]Paolo->banana\[Gamma]Paolo \[Lambda]^2,banana\[Gamma]Grad->banana\[Gamma]Grad \[Lambda]^2,bananaMultigCT->bananaMultigCT \[Lambda],bananaMultigCTGrad->bananaMultigCTGrad \[Lambda],banana\[Gamma]PlusCT->banana\[Gamma]PlusCT \[Lambda],banana\[Gamma]PaoloCT->banana\[Gamma]PaoloCT \[Lambda],banana\[Gamma]GradCT->banana\[Gamma]GradCT \[Lambda]^2,banana\[Gamma]MinusCT->banana\[Gamma]MinusCT \[Lambda],banana\[Gamma]2CT->banana\[Gamma]2CT \[Lambda]}*)


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
(**)
(*Normal@%/. introduce\[Lambda]//Expand//Collect[#,{g,\[Epsilon],\[Lambda],Hold[b-1]},FS]&*)
(*%/.\[Lambda]^n_/;n>3->0(*/.\[Lambda]->1*)*)
(*%/.\[Lambda]->1*)
(*(*%/.K->1+3b/2//FS *)*)
(*(*%/.b->1*)%/.hideSubDivs //FS*)
(*%/.replaceDiagrams//FullSimplify//Factor*)
(*%//.replaceRule//FS*)
(*ReleaseHold[%]//Collect[#,g,FS]&*)
(*%/.b->1*)


(* ::Item::Closed:: *)
(*Try to separate \[Gamma]2 and \[Gamma]Paolo: IT  WORKS PERFECTLY!!!!! HOWEVER, NOTICE THAT \[Gamma]Paolo USES A DIFFERENT COUPLING!! (as it is expected, since it heavily contains green-blue interactions, and not just red-red)*)


(* ::Input:: *)
(*\[CapitalGamma]\[Gamma]2 =1+(-(b banana\[Gamma]2)  g0 \[Mu]^-\[Epsilon]+g0^2 \[Mu]^(-2 \[Epsilon]) (b^2 doubleBanana\[Gamma]2g  +b^2 hatg\[Gamma]2+b^2 hat\[Gamma]1\[Gamma]2+b^2 hat\[Gamma]2\[Gamma]2- b hat\[Gamma]Paolo\[Gamma]2-b Hold[b-1] doubleBanana\[Gamma]Grad\[Gamma]2 ) );(*K should be K=1+b: half SUBDIV b*doubleBanana\[Gamma]Paolog + SUBDIV IN b*hat\[Gamma]Paolo\[Gamma]2 + SUBDIV IN hat\[Gamma]Paolo	.*)*)
(*(*HOWEVER, IF ONE DOES NOT ALLOW FOR banana*banana\[Gamma]Paolo, THEN THE VALUE OF K MUST BE K=1+3b/2! BECAUSE NOW ITS:*)
(* FULL SUBDIV b*doubleBanana\[Gamma]Paolog + SUBDIV IN b*hat\[Gamma]Paolo\[Gamma]2 + SUBDIV IN hat\[Gamma]Paolo	.*)*)
(**)
(*(*This is the contribution to Subscript[g, \[Phi]\[Psi]], but actually, the observable for \[Gamma]Paolo tout court is the one below*)*)
(*\[CapitalGamma]\[Gamma]Paolo=1+(-( - banana\[Gamma]Paolo)  g0 \[Mu]^-\[Epsilon]+g0^2 \[Mu]^(-2 \[Epsilon]) (-2 hat\[Gamma]Paolo-b doubleBanana\[Gamma]Paolog +K banana^2) )(*/.K->(1+2*b/2)*);(*K should be K=1+b: half SUBDIV b*doubleBanana\[Gamma]Paolog + SUBDIV IN b*hat\[Gamma]Paolo\[Gamma]2 + SUBDIV IN hat\[Gamma]Paolo	.*)*)
(*(*HOWEVER, IF ONE DOES NOT ALLOW FOR banana*banana\[Gamma]Paolo, THEN THE VALUE OF K MUST BE K=1+3b/2! BECAUSE NOW ITS:*)
(* FULL SUBDIV b*doubleBanana\[Gamma]Paolog + SUBDIV IN b*hat\[Gamma]Paolo\[Gamma]2 + SUBDIV IN hat\[Gamma]Paolo	.*)*)
(**)
(*(*This is the observable for \[Gamma]Paolo tout court. It's n-Loop correction will produce a n+1-Loop correction to the rest. THIS IS LIKE A COULING ITSELF, WHICH ACTUALLY IS: RED-GREEN COUPLING!!	!*)*)
(*gPaolo=g0 \[Mu]^-\[Epsilon] (1+(-( - banana\[Gamma]Paolo)(b+2)  g0 \[Mu]^-\[Epsilon](*+g0^2\[Mu]^(-2 \[Epsilon]) (-2 hat\[Gamma]Paolo-b doubleBanana\[Gamma]Paolog +K banana^2) *)))(*/.K->(1+2*b/2)*);*)


(* ::Input:: *)
(**)
(**)
(**)
(*(*RG\[Gamma]2 only: FINITE	!!!!*)*)
(*loopOrder=2;*)
(**)
(*(*replaceRule=Flatten@{GradImmediateIntNotAllowed->0,a2->-3/(b)+1-a,a->0,h->h,h2->h2};*)*)
(*g=Normal[Series[\[CapitalGamma]gt,{g0,0,loopOrder+1}]]/.replaceRule;*)
(**)
(*(\[CapitalGamma]\[Gamma]2)/(\[CapitalGamma]\[Gamma])^0/.z[_]->1;*)
(*FS/@(%/.replaceRule)*)
(**)
(*\[Gamma]Function[%,g,"print"->True]*)
(*(*Replace[Normal[%],a_/;!(FreeQ[a,g^2]):>(a/.(*banana\[Gamma]Grad*) banana\[Gamma]Paolo->0),{1}]*)*)
(*(*%/.K->1+3b/2//FS*) (*THIS IS CORRECT!!! IF ONE DOES NOT ALLOW FOR banana*banana\[Gamma]Paolo, THEN THE VALUE OF K MUST BE K=1+3b/2	.*)*)
(*(*%/.b->1*)%/.hideSubDivs //FS*)
(*%/.replaceDiagrams//FullSimplify//Factor*)
(*%//.replaceRule//FS*)
(*RG\[Gamma]2=ReleaseHold[%]//FS*)
(*%/.b->1*)


(* ::Input:: *)
(*(*RG\[Gamma]Paolo only: FINITE!!!	!*)*)
(**)
(*loopOrder=2;*)
(**)
(**)
(*\[CapitalGamma]\[Gamma]Paolo=1+(-( - banana\[Gamma]Paolo)  g0 \[Mu]^-\[Epsilon]+g0^2 \[Mu]^(-2 \[Epsilon]) (-2 hat\[Gamma]Paolo-b doubleBanana\[Gamma]Paolog +K banana^2) );*)
(**)
(*(*replaceRule=Flatten@{GradImmediateIntNotAllowed->0,a2->-3/(b)+1-a,a->0,h->h,h2->h2};*)*)
(*g=g0 \[Mu]^-\[Epsilon](*Normal[Series[\[CapitalGamma]gt,{g0,0,loopOrder+1}]]/.replaceRule;*)*)
(**)
(*(\[CapitalGamma]\[Gamma]Paolo)/(\[CapitalGamma]\[Gamma])^0/.z[_]->1;*)
(*FS/@(%/.replaceRule)*)
(**)
(*\[Gamma]Function[%,g,"print"->True,"g0Order"->loopOrder]*)
(*Replace[Normal[%],a_/;!(FreeQ[a,g^2]):>(a/.(*banana\[Gamma]Grad*) banana\[Gamma]Paolo->0),{1}]*)
(*%/.K->1+2*b/2//FS (*THIS IS CORRECT!!! IF ONE DOES not ALLOW FOR banana*banana\[Gamma]Paolo, THEN THE VALUE OF K MUST BE K=1+b	.*)*)
(*(*%/.b->1*)%/.hideSubDivs //FS*)
(*%/.replaceDiagrams//FullSimplify//Factor*)
(*%//.replaceRule//FS*)
(*RG\[Gamma]Paolo=ReleaseHold[%]//Collect[#,g,FS]&*)
(*%/.b->1*)


(* ::Input:: *)
(*(*\[Gamma]Paolo as a coupling	!*)*)
(*loopOrder=1;*)
(**)
(*gPaolo=g0 \[Mu]^-\[Epsilon] (1+(-( - banana\[Gamma]Paolo)(b+2)  g0 \[Mu]^-\[Epsilon](*+g0^2\[Mu]^(-2 \[Epsilon]) (-2 hat\[Gamma]Paolo-b doubleBanana\[Gamma]Paolog +K banana^2) *)))(*/.K->(1+2*b/2)*);*)
(*g=Collect[(Series[gPaolo//.replaceRule,{g0,0,loopOrder+1}]//Normal),{g0},FS];*)
(*PPrint[%,%]*)
(*(*g=Series[g0 \[Mu]^-\[Epsilon] Zgt^(-1),{g0,0,2}]//Normal*)*)
(*\[Beta]Function[g(*/.hideSubDivs*) ,"print"->tTrue](*It's slow if the subDivs are not hidden directly in g, I think it's just because it's a long expression*)*)
(**)
(*(*Replace[Normal[%],a_/;!(FreeQ[a,g^3]):>(a/.(*banana\[Gamma]Grad*) banana\[Gamma]Paolo->0),{1}];*)
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
(*%%/.{J\[Rule]1/2 (27-22 b) b(*,J\[Rule]-(1/2) (-16+11 b)*)}//Collect[#,g,FS]&*)
(*%/.b->1//Collect[#,g,FS]&(*This is if some explicit bananaCT are used*)*)*)


(* ::Input:: *)
(*(*RG\[Gamma]Paolo with gPaolo as coupling: FINITE!!!	!*)*)
(**)
(*loopOrder=2;*)
(**)
(*(*HOWEVER, NOTICE THAT THESE ARE NOT ALL THE SAME g0!! SOME OF THEM ARE Subscript[g\[Gamma], g], other Overscript[g, ~], other Subscript[g, \[Phi]\[Chi]]	!!*)*)
(**)
(*gPaolo=g0 \[Mu]^-\[Epsilon](1 - banana\[Gamma]Paolo(b+ 2) g0 \[Mu]^-\[Epsilon](*+g0^2\[Mu]^(-2 \[Epsilon]) (-2 hat\[Gamma]Paolo-b doubleBanana\[Gamma]Paolog +K banana^2) *));*)
(**)
(*gPaolo=g0 \[Mu]^-\[Epsilon](1 - banana\[Gamma]Paolo(b g[t]+2 g[\[Phi]\[Chi]])*\[Mu]^(-\[Epsilon]*0)(*+g0^2\[Mu]^(-2 \[Epsilon]) (-2 hat\[Gamma]Paolo-b doubleBanana\[Gamma]Paolog +K banana^2) *));*)
(**)
(*\[CapitalGamma]\[Gamma]Paolo=1-b( Hold[banana\[Gamma]Paolo]  g0 \[Mu]^-\[Epsilon]+g0^2 \[Mu]^(-2 \[Epsilon])  (-2 hat\[Gamma]Paolo-b doubleBanana\[Gamma]Paolog +K banana^2) )/.K->0;*)
(**)
(*\[CapitalGamma]\[Gamma]Paolo=1-b (g0 \[Mu]^-\[Epsilon] Hold[banana\[Gamma]Paolo]+ g0 \[Mu]^(-2 \[Epsilon]) (-b doubleBanana\[Gamma]Paolog g[t]-2 hat\[Gamma]Paolo g[\[Phi]\[Chi]]));*)
(**)
(*((\[CapitalGamma]\[Gamma]Paolo-1)+1)/(\[CapitalGamma]\[Gamma])^0/.z[_]->1;*)
(*FS/@(%/.replaceRule)*)
(**)
(*\[Gamma]Function[%,gPaolo,"print"->True,"g0Order"->loopOrder]*)
(*%/.g[_]:>g*)
(*(Expand/@%)/.Hold[banana\[Gamma]Paolo]^2->Hold[banana\[Gamma]Paolo]^2//ReleaseHold*)
(*(*Replace[Normal[%],a_/;!(FreeQ[a,g^2]):>(a/.(*banana\[Gamma]Grad*) banana\[Gamma]Paolo->0),{1}]*)
(*%/.K->1+2*b/2//FS (*THIS IS CORRECT!!! IF ONE DOES not ALLOW FOR banana*banana\[Gamma]Paolo, THEN THE VALUE OF K MUST BE K=1+b	.*)*)
(**)(*%/.b->1*)%/.hideSubDivs //FS*)
(*%/.replaceDiagrams//FullSimplify//Factor*)
(*%//.replaceRule//FS*)
(*Series[%,{\[Epsilon],0,loopOrder}]*)
(*RG\[Gamma]PaoloAsCoupling=ReleaseHold[%]//Collect[#,g,FS]&*)
(*%/.\[Mu]->1*)
(*%/.b->1*)


(* ::Input:: *)
(*(*\[CapitalGamma]\[Gamma]2\[Phi]\[Psi] COMPLETELY as coupling: FINITE??	!*)*)
(**)
(*loopOrder=2;*)
(**)
(*(*HOWEVER, NOTICE THAT THESE ARE NOT ALL THE SAME g0!! SOME OF THEM ARE Subscript[g\[Gamma], g], other Overscript[g, ~], other Subscript[g, \[Phi]\[Chi]]	!!*)*)
(**)
(*gPaolo=g0 \[Mu]^-\[Epsilon](1 - banana\[Gamma]Paolo(b+ 2) g0 \[Mu]^-\[Epsilon](*+g0^2\[Mu]^(-2 \[Epsilon]) (-2 hat\[Gamma]Paolo-b doubleBanana\[Gamma]Paolog +K banana^2) *));*)
(**)
(*gPaolo=g0 \[Mu]^-\[Epsilon](1 - banana\[Gamma]Paolo(b g[t]+2 g[\[Phi]\[Chi]])*\[Mu]^(-\[Epsilon]*0)(*+g0^2\[Mu]^(-2 \[Epsilon]) (-2 hat\[Gamma]Paolo-b doubleBanana\[Gamma]Paolog +K banana^2) *));*)
(**)
(*\[CapitalGamma]\[Gamma]Paolo=1-b( Hold[banana\[Gamma]Paolo]  g0 \[Mu]^-\[Epsilon]+g0^2 \[Mu]^(-2 \[Epsilon])  (-2 hat\[Gamma]Paolo-b doubleBanana\[Gamma]Paolog +K banana^2) )/.K->0;*)
(**)
(*\[CapitalGamma]\[Gamma]Paolo=1-b (g0 \[Mu]^-\[Epsilon] Hold[banana\[Gamma]Paolo]+ g0 \[Mu]^(-2 \[Epsilon]) (-b doubleBanana\[Gamma]Paolog g[t]-2 hat\[Gamma]Paolo g[\[Phi]\[Chi]]));*)
(**)
(*\[CapitalGamma]\[Gamma]2 =1+(-(b banana\[Gamma]2)  g0 \[Mu]^-\[Epsilon]+g0^2 \[Mu]^(-2 \[Epsilon]) (b^2 doubleBanana\[Gamma]2g  +b^2 hatg\[Gamma]2+b^2 hat\[Gamma]1\[Gamma]2+b^2 hat\[Gamma]2\[Gamma]2- b hat\[Gamma]Paolo\[Gamma]2-b Hold[b-1] doubleBanana\[Gamma]Grad\[Gamma]2 ) );*)
(**)
(*((\[CapitalGamma]\[Gamma]Paolo-1)+1)/(\[CapitalGamma]\[Gamma])^0/.z[_]->1;*)
(*FS/@(%/.replaceRule)*)
(**)
(*\[Gamma]Function[%,gPaolo,"print"->True,"g0Order"->loopOrder]*)
(*%/.g[_]:>g*)
(*(Expand/@%)/.Hold[banana\[Gamma]Paolo]^2->Hold[banana\[Gamma]Paolo]^2//ReleaseHold*)
(*(*Replace[Normal[%],a_/;!(FreeQ[a,g^2]):>(a/.(*banana\[Gamma]Grad*) banana\[Gamma]Paolo->0),{1}]*)
(*%/.K->1+2*b/2//FS (*THIS IS CORRECT!!! IF ONE DOES not ALLOW FOR banana*banana\[Gamma]Paolo, THEN THE VALUE OF K MUST BE K=1+b	.*)*)
(**)(*%/.b->1*)%/.hideSubDivs //FS*)
(*%/.replaceDiagrams//FullSimplify//Factor*)
(*%//.replaceRule//FS*)
(*Series[%,{\[Epsilon],0,loopOrder}]*)
(*RG\[Gamma]PaoloAsCoupling=ReleaseHold[%]//Collect[#,g,FS]&*)
(*%/.\[Mu]->1*)
(*%/.b->1*)


(* ::Subitem::Closed:: *)
(*Compare with previous result:  		IT WORKS!!!*)


(* ::Input:: *)
(*(*Now: With \[Gamma]Paolo as coupling*)*)
(*RG\[Gamma]2*)
(*RG\[Gamma]PaoloAsCoupling*)
(*%%-(%/b/.\[Mu]->1)*)
(*Normal[%]//Collect[#,g,FS]&*)
(**)
(*(*Previously*)*)
(*(1-b) g+1/2 (-1+b) (2+3 b) g^2===%*)


(* ::Input:: *)
(*(*Now: With \[Gamma]Paolo not ass coupling (subdivs removal by hand)*)*)
(*RG\[Gamma]2*)
(*RG\[Gamma]Paolo*)
(*%%+(%)*)
(*Normal[%]//Collect[#,g,FS]&*)
(**)
(*(*Previously*)*)
(*(1-b) g+1/2 (-1+b) (2+3 b) g^2===%*)


(* ::Subsubsection:: *)
(*WITH WF REN*)


(* ::Item::Closed:: *)
(*Together with manual simplification*)


(* ::Input:: *)
(*loopOrder=2;*)
(**)
(**)
(*(*replaceRule=Flatten@{GradImmediateIntNotAllowed->0,a2->-3/(b)+1-a,a->0,h->h,h2->h2};*)*)
(*g=Collect[(Series[\[CapitalGamma]gtPartialProd//.replaceRule,{g0,0,loopOrder+1}]//Normal),{g0},FS];*)
(**)
(*(1+\[CapitalGamma]\[Gamma]2)/(\[CapitalGamma]\[Gamma])^1/.z[_]->1;*)
(*FS/@(%/.replaceRule)*)
(**)
(*\[Gamma]Function[%,g,"print"->tTrue]*)
(*Replace[Normal[%],a_/;!(FreeQ[a,g^2]):>(a/.(*banana\[Gamma]Grad*) banana\[Gamma]Paolo->0),{1}]*)
(*(*%/.b->1*)%/.hideSubDivs //FS*)
(**)
(*%/.K->1/2 (1+4 b)//FS (*????THIS IS CORRECT!!! IF ONE DOES NOT ALLOW FOR banana*banana\[Gamma]Paolo, THEN THE VALUE OF K MUST BE K=1+3b/2*)*)
(**)
(*%/.replaceDiagrams//FullSimplify//Factor*)
(*%//.replaceRule//FS*)
(**)
(*ReleaseHold[%]//Collect[#,g,Factor]&*)
(**)
(*%/.b->1*)


(* ::Input:: *)
(*%/.g->gstar2+O[\[Epsilon]]^3//FS;*)
(**)


(* ::Input:: *)
(*(-8-32 b+16 K+4 \[Epsilon]-13 b \[Epsilon]+9 b^2 \[Epsilon])//Collect[#,\[Epsilon],FS]&*)
(*%/. {\[Epsilon]->0,H2->0}*)
(*Solve[%==0,K]*)


(* ::Item::Closed:: *)
(*Without removal by hand: using \[Lambda] (a bit messy, better think it through)*)


(* ::Input:: *)
(*loopOrder=2;*)
(**)
(*(*replaceRule=Flatten@{GradImmediateIntNotAllowed->0,a2->-3/(b)+1-a,a->0,h->h,h2->h2};*)*)
(*g=Normal[Series[\[CapitalGamma]gt,{g0,0,loopOrder+1}]]/.replaceRule;*)
(**)
(*(1+\[CapitalGamma]\[Gamma]2)/(\[CapitalGamma]\[Gamma])^0/.z[_]->1;*)
(*FS/@(%/.replaceRule);*)
(**)
(*\[Gamma]Function[%,g,"print"->tTrue];*)
(**)
(*Replace[Normal[%],a_/;!(FreeQ[a,g^2]):>(a/.(*banana\[Gamma]Grad*) banana\[Gamma]Paolo->0),{1}]*)
(*Normal@%%/. introduce\[Lambda]//Expand//Collect[#,{g,\[Epsilon],\[Lambda],Hold[b-1]},FS]&*)
(*%/.\[Lambda]^n_/;n>3->0(*/.\[Lambda]->1*)*)
(**)
(*%%%-%//FS*)
(*%/.\[Lambda]->1*)


(* ::Input:: *)
(*introduce\[Lambda]={bananag->bananag \[Lambda]^(3/2),banana\[Gamma]1->banana\[Gamma]1 \[Lambda]^(3/2),banana\[Gamma]2->banana\[Gamma]2 \[Lambda],banana\[Gamma]Paolo->banana\[Gamma]Paolo \[Lambda]^2,banana\[Gamma]Grad->banana\[Gamma]Grad \[Lambda]^2,bananaMultigCT->bananaMultigCT \[Lambda],bananaMultigCTGrad->bananaMultigCTGrad \[Lambda],banana\[Gamma]PlusCT->banana\[Gamma]PlusCT \[Lambda],banana\[Gamma]PaoloCT->banana\[Gamma]PaoloCT \[Lambda],banana\[Gamma]GradCT->banana\[Gamma]GradCT \[Lambda]^2,banana\[Gamma]MinusCT->banana\[Gamma]MinusCT \[Lambda],banana\[Gamma]2CT->banana\[Gamma]2CT \[Lambda]}*)


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
(**)
(*Normal@%/. introduce\[Lambda]//Expand//Collect[#,{g,\[Epsilon],\[Lambda],Hold[b-1]},FS]&*)
(*%/.\[Lambda]^n_/;n>3->0(*/.\[Lambda]->1*)*)
(*%/.\[Lambda]->1*)
(*(*%/.K->1+3b/2//FS *)*)
(*(*%/.b->1*)%/.hideSubDivs //FS*)
(*%/.replaceDiagrams//FullSimplify//Factor*)
(*%//.replaceRule//FS*)
(*ReleaseHold[%]//Collect[#,g,FS]&*)
(*%/.b->1*)


(* ::Item::Closed:: *)
(*Try to separate \[Gamma]2 and \[Gamma]Paolo: IT  WORKS PERFECTLY!!!!! HOWEVER, NOTICE THAT \[Gamma]Paolo USES A DIFFERENT COUPLING!! (as it is expected, since it heavily contains green-blue interactions, and not just red-red)*)


(* ::Input:: *)
(*\[CapitalGamma]\[Gamma]2*)


(* ::Input:: *)
(*\[CapitalGamma]\[Gamma]2 =1+(-(b banana\[Gamma]2)  g0 \[Mu]^-\[Epsilon]+g0^2 \[Mu]^(-2 \[Epsilon]) (b^2 doubleBanana\[Gamma]2g  +b^2 hatg\[Gamma]2+b^2 hat\[Gamma]1\[Gamma]2+b^2 hat\[Gamma]2\[Gamma]2- b hat\[Gamma]Paolo\[Gamma]2-b Hold[b-1] doubleBanana\[Gamma]Grad\[Gamma]2 ) );(*K should be K=1+b: half SUBDIV b*doubleBanana\[Gamma]Paolog + SUBDIV IN b*hat\[Gamma]Paolo\[Gamma]2 + SUBDIV IN hat\[Gamma]Paolo	.*)*)
(*(*HOWEVER, IF ONE DOES NOT ALLOW FOR banana*banana\[Gamma]Paolo, THEN THE VALUE OF K MUST BE K=1+3b/2! BECAUSE NOW ITS:*)
(* FULL SUBDIV b*doubleBanana\[Gamma]Paolog + SUBDIV IN b*hat\[Gamma]Paolo\[Gamma]2 + SUBDIV IN hat\[Gamma]Paolo	.*)*)
(**)
(*(*This is the contribution to Subscript[g, \[Phi]\[Psi]], but actually, the observable for \[Gamma]Paolo tout court is the one below*)*)
(*\[CapitalGamma]\[Gamma]Paolo=1+(-( - banana\[Gamma]Paolo)  g0 \[Mu]^-\[Epsilon]+g0^2 \[Mu]^(-2 \[Epsilon]) (-2 hat\[Gamma]Paolo-b doubleBanana\[Gamma]Paolog +K banana^2) )(*/.K->(1+2*b/2)*);(*K should be K=1+b: half SUBDIV b*doubleBanana\[Gamma]Paolog + SUBDIV IN b*hat\[Gamma]Paolo\[Gamma]2 + SUBDIV IN hat\[Gamma]Paolo	.*)*)
(*(*HOWEVER, IF ONE DOES NOT ALLOW FOR banana*banana\[Gamma]Paolo, THEN THE VALUE OF K MUST BE K=1+3b/2! BECAUSE NOW ITS:*)
(* FULL SUBDIV b*doubleBanana\[Gamma]Paolog + SUBDIV IN b*hat\[Gamma]Paolo\[Gamma]2 + SUBDIV IN hat\[Gamma]Paolo	.*)*)
(**)
(*(*This is the observable for \[Gamma]Paolo tout court. It's n-Loop correction will produce a n+1-Loop correction to the rest. THIS IS LIKE A COULING ITSELF, WHICH ACTUALLY IS: RED-GREEN COUPLING!!	!*)*)
(*gPaolo=g0 \[Mu]^-\[Epsilon] (1+(-( - banana\[Gamma]Paolo)(b+2)  g0 \[Mu]^-\[Epsilon](*+g0^2\[Mu]^(-2 \[Epsilon]) (-2 hat\[Gamma]Paolo-b doubleBanana\[Gamma]Paolog +K banana^2) *)))(*/.K->(1+2*b/2)*);*)


(* ::Input:: *)
(**)
(**)
(**)
(*(*RG\[Gamma]2 only: FINITE	!!!!*)*)
(*loopOrder=2;*)
(**)
(*(*replaceRule=Flatten@{GradImmediateIntNotAllowed->0,a2->-3/(b)+1-a,a->0,h->h,h2->h2};*)*)
(*g=Normal[Series[\[CapitalGamma]gt,{g0,0,loopOrder+1}]]/.replaceRule;*)
(**)
(*(\[CapitalGamma]\[Gamma]2)/(\[CapitalGamma]\[Gamma])^0/.z[_]->1;*)
(*FS/@(%/.replaceRule)*)
(**)
(*\[Gamma]Function[%,g,"print"->True]*)
(*(*Replace[Normal[%],a_/;!(FreeQ[a,g^2]):>(a/.(*banana\[Gamma]Grad*) banana\[Gamma]Paolo->0),{1}]*)*)
(*(*%/.K->1+3b/2//FS*) (*THIS IS CORRECT!!! IF ONE DOES NOT ALLOW FOR banana*banana\[Gamma]Paolo, THEN THE VALUE OF K MUST BE K=1+3b/2	.*)*)
(*(*%/.b->1*)%/.hideSubDivs //FS*)
(*%/.replaceDiagrams//FullSimplify//Factor*)
(*%//.replaceRule//FS*)
(*RG\[Gamma]2=ReleaseHold[%]//FS*)
(*%/.b->1*)


(* ::Input:: *)
(*(*RG\[Gamma]Paolo only: FINITE!!!	!*)*)
(**)
(*loopOrder=2;*)
(**)
(**)
(*\[CapitalGamma]\[Gamma]Paolo=1+(-( - banana\[Gamma]Paolo)  g0 \[Mu]^-\[Epsilon]+g0^2 \[Mu]^(-2 \[Epsilon]) (-2 hat\[Gamma]Paolo-b doubleBanana\[Gamma]Paolog +K banana^2) );*)
(**)
(*(*replaceRule=Flatten@{GradImmediateIntNotAllowed->0,a2->-3/(b)+1-a,a->0,h->h,h2->h2};*)*)
(*g=g0 \[Mu]^-\[Epsilon](*Normal[Series[\[CapitalGamma]gt,{g0,0,loopOrder+1}]]/.replaceRule;*)*)
(**)
(*(\[CapitalGamma]\[Gamma]Paolo)/(\[CapitalGamma]\[Gamma])^0/.z[_]->1;*)
(*FS/@(%/.replaceRule)*)
(**)
(*\[Gamma]Function[%,g,"print"->True,"g0Order"->loopOrder]*)
(*Replace[Normal[%],a_/;!(FreeQ[a,g^2]):>(a/.(*banana\[Gamma]Grad*) banana\[Gamma]Paolo->0),{1}]*)
(*%/.K->1+2*b/2//FS (*THIS IS CORRECT!!! IF ONE DOES not ALLOW FOR banana*banana\[Gamma]Paolo, THEN THE VALUE OF K MUST BE K=1+b	.*)*)
(*(*%/.b->1*)%/.hideSubDivs //FS*)
(*%/.replaceDiagrams//FullSimplify//Factor*)
(*%//.replaceRule//FS*)
(*RG\[Gamma]Paolo=ReleaseHold[%]//Collect[#,g,FS]&*)
(*%/.b->1*)


(* ::Input:: *)
(*(*\[Gamma]Paolo as a coupling	!*)*)
(*loopOrder=1;*)
(**)
(*gPaolo=g0 \[Mu]^-\[Epsilon] (1+(-( - banana\[Gamma]Paolo)(b+2)  g0 \[Mu]^-\[Epsilon](*+g0^2\[Mu]^(-2 \[Epsilon]) (-2 hat\[Gamma]Paolo-b doubleBanana\[Gamma]Paolog +K banana^2) *)))(*/.K->(1+2*b/2)*);*)
(*g=Collect[(Series[gPaolo//.replaceRule,{g0,0,loopOrder+1}]//Normal),{g0},FS];*)
(*PPrint[%,%]*)
(*(*g=Series[g0 \[Mu]^-\[Epsilon] Zgt^(-1),{g0,0,2}]//Normal*)*)
(*\[Beta]Function[g(*/.hideSubDivs*) ,"print"->tTrue](*It's slow if the subDivs are not hidden directly in g, I think it's just because it's a long expression*)*)
(**)
(*(*Replace[Normal[%],a_/;!(FreeQ[a,g^3]):>(a/.(*banana\[Gamma]Grad*) banana\[Gamma]Paolo->0),{1}];*)
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
(*%%/.{J\[Rule]1/2 (27-22 b) b(*,J\[Rule]-(1/2) (-16+11 b)*)}//Collect[#,g,FS]&*)
(*%/.b->1//Collect[#,g,FS]&(*This is if some explicit bananaCT are used*)*)*)


(* ::Input:: *)
(*(*RG\[Gamma]Paolo with gPaolo as coupling: FINITE!!!	!*)*)
(**)
(*loopOrder=2;*)
(**)
(*(*HOWEVER, NOTICE THAT THESE ARE NOT ALL THE SAME g0!! SOME OF THEM ARE Subscript[g\[Gamma], g], other Overscript[g, ~], other Subscript[g, \[Phi]\[Chi]]	!!*)*)
(**)
(*gPaolo=g0 \[Mu]^-\[Epsilon](1 - banana\[Gamma]Paolo(b+ 2) g0 \[Mu]^-\[Epsilon](*+g0^2\[Mu]^(-2 \[Epsilon]) (-2 hat\[Gamma]Paolo-b doubleBanana\[Gamma]Paolog +K banana^2) *));*)
(**)
(*gPaolo=g0 \[Mu]^-\[Epsilon](1 - banana\[Gamma]Paolo(b g[t]+2 g[\[Phi]\[Chi]])*\[Mu]^(-\[Epsilon]*0)(*+g0^2\[Mu]^(-2 \[Epsilon]) (-2 hat\[Gamma]Paolo-b doubleBanana\[Gamma]Paolog +K banana^2) *));*)
(**)
(*\[CapitalGamma]\[Gamma]Paolo=1-b( Hold[banana\[Gamma]Paolo]  g0 \[Mu]^-\[Epsilon]+g0^2 \[Mu]^(-2 \[Epsilon])  (-2 hat\[Gamma]Paolo-b doubleBanana\[Gamma]Paolog +K banana^2) )/.K->0;*)
(**)
(*\[CapitalGamma]\[Gamma]Paolo=1-b (g0 \[Mu]^-\[Epsilon] Hold[banana\[Gamma]Paolo]+ g0 \[Mu]^(-2 \[Epsilon]) (-b doubleBanana\[Gamma]Paolog g[t]-2 hat\[Gamma]Paolo g[\[Phi]\[Chi]]));*)
(**)
(*((\[CapitalGamma]\[Gamma]Paolo-1)+1)/(\[CapitalGamma]\[Gamma])^0/.z[_]->1;*)
(*FS/@(%/.replaceRule)*)
(**)
(*\[Gamma]Function[%,gPaolo,"print"->True,"g0Order"->loopOrder]*)
(*%/.g[_]:>g*)
(*(Expand/@%)/.Hold[banana\[Gamma]Paolo]^2->Hold[banana\[Gamma]Paolo]^2//ReleaseHold*)
(*(*Replace[Normal[%],a_/;!(FreeQ[a,g^2]):>(a/.(*banana\[Gamma]Grad*) banana\[Gamma]Paolo->0),{1}]*)
(*%/.K->1+2*b/2//FS (*THIS IS CORRECT!!! IF ONE DOES not ALLOW FOR banana*banana\[Gamma]Paolo, THEN THE VALUE OF K MUST BE K=1+b	.*)*)
(**)(*%/.b->1*)%/.hideSubDivs //FS*)
(*%/.replaceDiagrams//FullSimplify//Factor*)
(*%//.replaceRule//FS*)
(*Series[%,{\[Epsilon],0,loopOrder}]*)
(*RG\[Gamma]PaoloAsCoupling=ReleaseHold[%]//Collect[#,g,FS]&*)
(*%/.\[Mu]->1*)
(*%/.b->1*)


(* ::Input:: *)
(*(*\[CapitalGamma]\[Gamma]2\[Phi]\[Psi] COMPLETELY as coupling: FINITE??	!*)*)
(**)
(*loopOrder=2;*)
(**)
(*(*HOWEVER, NOTICE THAT THESE ARE NOT ALL THE SAME g0!! SOME OF THEM ARE Subscript[g\[Gamma], g], other Overscript[g, ~], other Subscript[g, \[Phi]\[Chi]]	!!*)*)
(**)
(*gPaolo=g0 \[Mu]^-\[Epsilon](1 - banana\[Gamma]Paolo(b+ 2) g0 \[Mu]^-\[Epsilon](*+g0^2\[Mu]^(-2 \[Epsilon]) (-2 hat\[Gamma]Paolo-b doubleBanana\[Gamma]Paolog +K banana^2) *));*)
(**)
(*gPaolo=g0 \[Mu]^-\[Epsilon](1 - banana\[Gamma]Paolo(b g[t]+2 g[\[Phi]\[Chi]])*\[Mu]^(-\[Epsilon]*0)(*+g0^2\[Mu]^(-2 \[Epsilon]) (-2 hat\[Gamma]Paolo-b doubleBanana\[Gamma]Paolog +K banana^2) *));*)
(**)
(*\[CapitalGamma]\[Gamma]Paolo=1-b( Hold[banana\[Gamma]Paolo]  g0 \[Mu]^-\[Epsilon]+g0^2 \[Mu]^(-2 \[Epsilon])  (-2 hat\[Gamma]Paolo-b doubleBanana\[Gamma]Paolog +K banana^2) )/.K->0;*)
(**)
(*\[CapitalGamma]\[Gamma]Paolo=1-b (g0 \[Mu]^-\[Epsilon] Hold[banana\[Gamma]Paolo]+ g0 \[Mu]^(-2 \[Epsilon]) (-b doubleBanana\[Gamma]Paolog g[t]-2 hat\[Gamma]Paolo g[\[Phi]\[Chi]]));*)
(**)
(*\[CapitalGamma]\[Gamma]2 =1+(-(b banana\[Gamma]2)  g0 \[Mu]^-\[Epsilon]+g0^2 \[Mu]^(-2 \[Epsilon]) (b^2 doubleBanana\[Gamma]2g  +b^2 hatg\[Gamma]2+b^2 hat\[Gamma]1\[Gamma]2+b^2 hat\[Gamma]2\[Gamma]2- b hat\[Gamma]Paolo\[Gamma]2-b Hold[b-1] doubleBanana\[Gamma]Grad\[Gamma]2 ) );*)
(**)
(*((\[CapitalGamma]\[Gamma]Paolo-1)+1)/(\[CapitalGamma]\[Gamma])^0/.z[_]->1;*)
(*FS/@(%/.replaceRule)*)
(**)
(*\[Gamma]Function[%,gPaolo,"print"->True,"g0Order"->loopOrder]*)
(*%/.g[_]:>g*)
(*(Expand/@%)/.Hold[banana\[Gamma]Paolo]^2->Hold[banana\[Gamma]Paolo]^2//ReleaseHold*)
(*(*Replace[Normal[%],a_/;!(FreeQ[a,g^2]):>(a/.(*banana\[Gamma]Grad*) banana\[Gamma]Paolo->0),{1}]*)
(*%/.K->1+2*b/2//FS (*THIS IS CORRECT!!! IF ONE DOES not ALLOW FOR banana*banana\[Gamma]Paolo, THEN THE VALUE OF K MUST BE K=1+b	.*)*)
(**)(*%/.b->1*)%/.hideSubDivs //FS*)
(*%/.replaceDiagrams//FullSimplify//Factor*)
(*%//.replaceRule//FS*)
(*Series[%,{\[Epsilon],0,loopOrder}]*)
(*RG\[Gamma]PaoloAsCoupling=ReleaseHold[%]//Collect[#,g,FS]&*)
(*%/.\[Mu]->1*)
(*%/.b->1*)


(* ::Subitem::Closed:: *)
(*Compare with previous result:  		IT WORKS!!!*)


(* ::Input:: *)
(*(*Now: With \[Gamma]Paolo as coupling*)*)
(*RG\[Gamma]2*)
(*RG\[Gamma]PaoloAsCoupling*)
(*%%-(%/b/.\[Mu]->1)*)
(*Normal[%]//Collect[#,g,FS]&*)
(**)
(*(*Previously*)*)
(*(1-b) g+1/2 (-1+b) (2+3 b) g^2===%*)


(* ::Input:: *)
(*(*Now: With \[Gamma]Paolo not ass coupling (subdivs removal by hand)*)*)
(*RG\[Gamma]2*)
(*RG\[Gamma]Paolo*)
(*%%+(%)*)
(*Normal[%]//Collect[#,g,FS]&*)
(**)
(*(*Previously*)*)
(*(1-b) g+1/2 (-1+b) (2+3 b) g^2===%*)


(* ::Subsection::Closed:: *)
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


(* ::Subsection::Closed:: *)
(*Z\[Gamma]Inv		TBD*)


(* ::Input:: *)
(*replaceRule={a->0,h->1,h2->1 ,a2->-(1/b),l->3/2};*)
(*Z\[Gamma]Inv/.z[_]->1//FS*)
(*\[Eta]=\[Gamma]FunctionFromZ[%/.replaceRule,ZgtInv/.replaceRule,"print"->True,"gstar"->True]*)
(**)


(* ::Subsection:: *)
(*To compare with kay*)


(* ::Subsubsection::Closed:: *)
(*SAW-like terms*)


(* ::Input:: *)
(*g0 \[Mu]^-\[Epsilon] (((1+\[CapitalGamma]\[Gamma]1) (1+ \[CapitalGamma]\[Gamma]2)+\[CapitalGamma]g-1))/(\[CapitalGamma]\[Gamma]^2)/.z["\[Gamma]"]->0/. z[_]->1/.replaceRule/.J->0;*)
(*Series[%,{g0,0,3}]//Normal//Expand;*)
(*%/.Hold[_]->0;*)
(*List@@%;*)
(*Select[%,StringFreeQ[ToString[#],"Paolo"]&];*)
(*Select[%,!FreeQ[#,g0^3]&];*)
(*sawLikeLength=Total@Replace[%,a_/;!NumericQ[a]:>1,2];*)
(*Total@%%%;*)
(*sawLike=Series[%,{g0,0,3}]//Normal;*)
(*ReleaseHold[%]/.hideSubDivs/.b->1/.J->0*)


(* ::Input:: *)
(*\[CapitalGamma]\[Gamma]/. z[_]->1/.replaceRule;*)
(*Series[%,{g0,0,3}]//Normal//Expand;*)
(*%/.Hold[_]->0;*)
(*List@@%;*)
(*Select[%,StringFreeQ[ToString[#],"Paolo"]&];*)
(*Total@%;*)
(*sawLikeProp=Series[%,{g0,0,3}]//Normal*)
(*ReleaseHold[%]/.hideSubDivs/.b->1/.J->0*)


(* ::Input:: *)
(*ReleaseHold[sawLike/sawLikeProp^2]/.hideSubDivs/.b->1/.J->0;*)
(*Series[%,{g0,0,3}]//Normal*)
(*c*%/.g0->g0 a/.{a->(3/2)^-1,c->(2/3)^-1}//FS//Expand;*)
(**)
(*\[Beta]Function[%,"print"->tTrue]*)
(*Normal[%]/.hideSubDivs/.replaceDiagrams//FS;*)
(*Collect[%,g]*)


(* ::Input:: *)
(*(*VS (literature)*)*)
(*-((8 g^2)/3)+(14 g^3)/3*)


(* ::Input:: *)
(*(* CORRECT *)*)


(* ::Subsubsection::Closed:: *)
(*LERW-like Extra terms*)


(* ::Input:: *)
(*g0 \[Mu]^-\[Epsilon] (((1+\[CapitalGamma]\[Gamma]1) (1+ \[CapitalGamma]\[Gamma]2)+\[CapitalGamma]g-1))/(\[CapitalGamma]\[Gamma]^2)(*//.replaceRule*)/.z["\[Gamma]"]->0/. z[_]->1/.replaceRule;*)
(*Series[%,{g0,0,3}]//Normal//Expand;*)
(*List@@%;*)
(*Select[%, (StringFreeQ[ToString[#],"CT"]&) ];*)
(*Select[%, !StringFreeQ[ToString[#],"Paolo"]& ];*)
(*Select[%,!FreeQ[#,g0^3]&]*)
(*lerwLikeLength=Abs[Total@Replace[%,a_/;!NumericQ[a]:>1,2]]*)
(*Total@%%%;*)
(*lerwLike=Series[%,{g0,0,3}]//Normal;*)
(*ReleaseHold[%]*)
(*%/.hideSubDivs/.b->b/.J->0/.K->0*)


(* ::Input:: *)
(*\[CapitalGamma]\[Gamma]/. z[_]->1/.replaceRule;*)
(*Series[%,{g0,0,3}]//Normal//Expand;*)
(*List@@%;*)
(*Select[%,!StringFreeQ[ToString[#],"Paolo"]&];*)
(*Total@%;*)
(*lerwLikeProp=Series[%,{g0,0,3}]//Normal;*)
(*ReleaseHold[%]/.hideSubDivs/.b->1/.J->0*)


(* ::Input:: *)
(*ReleaseHold[(sawLike+lerwLike)/(sawLikeProp+lerwLikeProp)^2]/.hideSubDivs/.b->1/.J->0;*)
(*Series[%,{g0,0,3}]//Normal*)
(*c*%/.g0->g0 a/.{a->(3/2)^-1,c->(2/3)^-1}//FS//Expand;*)
(**)
(*\[Beta]Function[%,"print"->tTrue]*)
(*Normal[%]/.hideSubDivs/.replaceDiagrams//FS;*)
(*Collect[%,g]*)


(* ::Input:: *)
(*(*VS (literature)*)*)
(*-2 g^2+(8 g^3)/3*)


(* ::Input:: *)
(*(* CORRECT *)*)


(* ::Subsubsection::Closed:: *)
(*Nasty Grad terms*)


(* ::Input:: *)
(*g0 \[Mu]^-\[Epsilon] (((1+\[CapitalGamma]\[Gamma]1) (1+ \[CapitalGamma]\[Gamma]2)+\[CapitalGamma]g-1))/(\[CapitalGamma]\[Gamma]^2)(*//.replaceRule*)/.z["\[Gamma]"]->0/. z[_]->1/.replaceRule;*)
(*Expand[%];*)
(*%-(%/.Hold[_]->0)//Expand;*)
(*Series[%,{g0,0,3}]//Normal//Expand;*)
(*List@@%;*)
(*Select[%, (StringFreeQ[ToString[#],"CT"]&) ];*)
(*Total@%;*)
(*nastyGrad=Series[%,{g0,0,3}]//Normal;*)
(*ReleaseHold[%]*)
(*%/.hideSubDivs/.b->b/.J->0/.K->0*)


(* ::Input:: *)
(*\[CapitalGamma]\[Gamma]/. z[_]->1/.replaceRule*)
(*Expand[%];*)
(*%-(%/.Hold[_]->0)//Expand;*)
(*List@@%;*)
(*Select[%, (StringFreeQ[ToString[#],"CT"]&) ];*)
(*Total@%;*)
(*nastyGradProp=Series[%,{g0,0,3}]//Normal;*)
(*ReleaseHold[%]/.hideSubDivs/.b->b/.J->0*)


(* ::Subsubsection::Closed:: *)
(*Together*)


(* ::Input:: *)
(*{sawLikeLength,lerwLikeLength,nastyGradLength=38}*)
(*Total@%*)


(* ::Input:: *)
(*termList={sawLike,lerwLike,nastyGrad};*)
(**)
(*bb=b;*)
(**)
(*Print["27 sawLike diagrams : ",ReleaseHold[sawLike]/.hideSubDivs/.b->bb/.J->0//Collect[#,g0,FS]&]*)
(*Print["12 lerwLike diagrams : ",ReleaseHold[lerwLike]/.hideSubDivs/.b->bb/.J->0//Collect[#,g0,FS]&]*)
(*Print["38 nastyGrad diagrams : ",ReleaseHold[nastyGrad]/.hideSubDivs/.b->bb/.J->0//Collect[#,g0,FS]&]*)
(**)
(*{sawLikeProp,lerwLikeProp,nastyGradProp};*)
(**)
(*Print["sawLikeProp = ",ReleaseHold[sawLikeProp]/.hideSubDivs/.b->bb/.J->0//Collect[#,g0,FS]&]*)
(*Print["lerwLikeProp = ",ReleaseHold[lerwLikeProp]/.hideSubDivs/.b->bb/.J->0//Collect[#,g0,FS]&]*)
(*Print["nastyGradProp = ",ReleaseHold[nastyGradProp]/.hideSubDivs/.b->bb/.J->0//Collect[#,g0,FS]&]*)


(* ::Chapter::Closed:: *)
(*\[Section]\[Section] TEST: R' on 2loop  O(n=0) a.k.a. SAW (general b)*)


(* ::Input:: *)
(*(*This is correct. This is the transformation needed to match the result in the literature by Kleinert-Schulte et al*)*)
(*c*%/.g0->g0 a/.{a->(3/2)^-1,c->(2/3)^-1}//FS//Expand;*)


(* ::Input:: *)
(*sawLike /.hideSubDivs/.b->bb*)
(*sawLikeProp/.hideSubDivs/.b->bb*)


(* ::Input:: *)
(*Zg=(sawLike /.hideSubDivs/.b->bb)/.\[Epsilon]->0*)
(*c*%/.g0->g0 a/.{a->(3/2)^-1,c->(2/3)^-1}//FS//Expand*)
(*%/g0//FS*)


(* ::Input:: *)
(*prop=sawLikeProp/.hideSubDivs/.b->bb/.\[Epsilon]->0*)
(*c*%/.g0->g0 a/.{a->(3/2)^-1,c->(2/3)^-1}//FS//Expand*)


(* ::Input:: *)
(*Series[Zg/prop^2,{g0,0,3}]//Normal;*)
(*c*%/.g0->g0 a/.{a->(3/2)^-1,c->(2/3)^-1}//FS//Expand;*)
(*%/g0//FS*)
(**)
(*\[Beta]FunctionFromZ[%,3,{g0},"print"->tTrue]*)
(*%/.replaceDiagrams//FS*)
(**)
(*Print["Vs Literature:"]*)
(*reference=\[Epsilon] g-(n+8)/3g^2+(3n+14)/3 g^3*)
(*Print["For the SAW (O(n=0)): "]*)
(*reference/.n->0*)
(*c*%/.g->g a/.{a->3/2,c->2/3}//FS//Expand;*)
(**)
(*Print["For the LERW (O(n=-2)):\n My norm: ",SeriesData[g, 0, {\[Epsilon], -3, 6}, 1, 4, 1],"\n Literature norm:"]*)
(*reference/.n->-2*)
(*c*%/.g->g a/.{a->3/2,c->2/3}//FS//Expand;*)
(**)


(* ::Input:: *)
(*(*CORRECT	!*)*)


(* ::Chapter:: *)
(*\[Section]\[Section] TEST: R' on 2loop  O(n=-2) a.k.a. LERW (general b)*)


(* ::Item:: *)
(*Let me solve a putative system of eqs for the fix point*)


(* ::Input:: *)
(*Assuming[\[Epsilon]>0&&b>0,*)
(*SolveValues[{b gt + 2 g==\[Epsilon],\[Epsilon] gt - (gt^2 4b -g^2 (2b -1))==0},{gt,g}]//FS];*)
(*%/.{\[Epsilon]->Zeta[3],b->1}*)
(*sol=Select[%%,(#[[1]]/.{\[Epsilon]->Zeta[3],b->1})>0&][[1]]*)
(*%//Expand*)


(* ::Text:: *)
(*This is the really interesting check, since for b!=1, there is no cancellation with \[Gamma]Paolo, which means that one has to use two (maybe three) couplings*)


(* ::Input:: *)
(*(*This is correct. This is the transformation needed to match the result in the literature by Kleinert-Schulte et al*)*)
(*c*%/.g0->g0 a/.{a->(3/2)^-1,c->(2/3)^-1}//FS//Expand;*)


(* ::Input:: *)
(*sawLike /.hideSubDivs/.b->bb*)
(*sawLikeProp/.hideSubDivs/.b->bb*)


(* ::Input:: *)
(*Zg=(sawLike +lerwLike)/.hideSubDivs/.b->bb/.\[Epsilon]->0*)
(*c*%/.g0->g0 a/.{a->(3/2)^-1,c->(2/3)^-1}//FS//Expand*)
(*%/g0//FS*)
(*%/.b->1*)


(* ::Input:: *)
(*prop=(sawLikeProp+lerwLikeProp)/.hideSubDivs/.b->bb/.\[Epsilon]->0*)
(*c*%/.g0->g0 a/.{a->(3/2)^-1,c->(2/3)^-1}//FS//Expand;*)
(*%%/.b->1*)


(* ::Input:: *)
(**)
(*Print["Vs Literature:"]*)
(*reference=\[Epsilon] g-(n+8)/3g^2+(3n+14)/3 g^3*)
(*Print["For the SAW (O(n=0)): "]*)
(*reference/.n->0*)
(*c*%/.g->g a/.{a->3/2,c->2/3}//FS//Expand;*)
(**)
(*Print["For the LERW (O(n=-2)):\n My norm: ",SeriesData[g, 0, {\[Epsilon], -3, 6}, 1, 4, 1],"\n Literature norm:"]*)
(*reference/.n->-2*)
(*c*%/.g->g a/.{a->3/2,c->2/3}//FS//Expand;*)


(* ::Subsection::Closed:: *)
(*I do not expect this to be finite: there is no correction for the \[Gamma]Paolo Coupling*)


(* ::Input:: *)
(*Series[Zg/prop^2,{g0,0,3}]//Normal;*)
(*c*%/.g0->g0 a/.{a->(3/2)^-1,c->(2/3)^-1}//FS//Expand;*)
(*%/g0//FS*)
(**)
(*\[Beta]FunctionFromZ[%,3,{g0},"print"->tTrue]*)
(*%/.replaceDiagrams//FS*)
(**)


(* ::Input:: *)
(*(*NOT FINITE INDEED	!*)*)


(* ::Subsection:: *)
(*Adding the correction for the \[Gamma]Paolo Coupling (still not finite because missing red-delayed-green interaction???)*)


(* ::Input:: *)
(*Series[Zg/prop^2,{g0,0,3}]//Normal;*)
(*c*%/.g0->g0 a/.{a->(3/2)^-1,c->(2/3)^-1}//FS//Expand;*)
(*%/g0//FS*)
(**)
(*\[Beta]FunctionFromZ[%,3,{g0},"print"->tTrue]*)
(*%/.replaceDiagrams//FS*)
(**)


(* ::Input:: *)
(*(*??????? FINITE ????????	!*)*)


(* ::Title::Closed:: *)
(*Plots*)


(* ::Subsection:: *)
(*\[Section] New (after my simpl)*)


(* ::Input::Initialization:: *)
dfRG1L:=2-b \[Epsilon]/(2+b)
dfRG1Lsimp:=2-(b \[Epsilon])/(1+2 b)

dfRG2Lsimp:=2-(b \[Epsilon])/(1+2 b)-(b (1+b+4 b^2) \[Epsilon]^2)/(2 (1+2 b)^3)
dfRG2Lsimp2:=2-(b \[Epsilon])/(1+2 b)-(b (1+b) \[Epsilon]^2)/(2 (1+2 b)^2)

dfRG2LsimpWF:=2-(b \[Epsilon])/(1+2 b)-((1+b (7+16 b)) \[Epsilon]^2)/(8 (1+2 b)^3)(*with \[CapitalGamma]\[Gamma]^2*)
(*dfRG2LsimpWF:=2-(b \[Epsilon])/(1+2 b)-((1+3 b) (1+5 b) \[Epsilon]^2)/(8 (1+2 b)^3)(*with \[CapitalGamma]\[Gamma]^1*)*)
dfRG2LsimpWF:=2-(b \[Epsilon])/(1+2 b)-(b (7+17 b) \[Epsilon]^2)/(8 (1+2 b)^3)(*with \[CapitalGamma]\[Gamma]^2, b(b-1) and 1+g^2*)

dfRG2LsimpWF:=2-(b \[Epsilon])/(1+2 b)-(b (5+19 b) \[Epsilon]^2)/(8 (1+2 b)^3)(*with \[CapitalGamma]\[Gamma]^2, b(b-1) and -hat+sunset*)
dfRG2LsimpWF:=2-(b \[Epsilon])/(1+2 b)-(b (11+13 b) \[Epsilon]^2)/(8 (1+2 b)^3)(*with \[CapitalGamma]\[Gamma]^2, b(b-1) and +hat-sunset*)
dfRG2LsimpWF:=2-(b \[Epsilon])/(1+2 b)-(3 b (3+5 b) \[Epsilon]^2)/(8 (1+2 b)^3)(*with \[CapitalGamma]\[Gamma]^2 AND b(b-1) (1-g^2)*)

dfRG2LsimpWF:=2-(b \[Epsilon])/(1+2 b)-(b (1+b (7+4 b)) \[Epsilon]^2)/(4 (1+2 b)^3)(*NO GRAD, STILL \[CapitalGamma]\[Gamma]^2 *)

dfRG2LsimpWF:=2-(b \[Epsilon])/(1+2 b)-(3 b (7+b) \[Epsilon]^2)/(8 (1+2 b)^3)(*WITH GRAD AND THE 4 MISSING DIAGRAMS	! *)
dfRG2LsimpWF:=2-(b \[Epsilon])/(1+2 b)-(b (5+b (3+4 b)) \[Epsilon]^2)/(4 (1+2 b)^3) (*NO GRAD AND THE 4 MISSING DIAGRAMS *)

(*dfRG2Lwf:=dfWF*)
dfRG2L:=2-b \[Epsilon]/(2+b)-b (\[Epsilon]/(2+b))^2

(*dfRG2Lsimp:=2-(b \[Epsilon])/(1+2 b)-(b ^2 \[Epsilon]^2)/(1+2 b)^2*)(*BAD*)

dfSLE=1+3/(4(2b+1));


(* ::Input:: *)
(**)
(**)


(* ::Input:: *)
(*{dfRG1L,dfRG1Lsimp,dfRG2Lsimp,dfRG2Lsimp2,dfRG2LsimpWF,dfSLE};*)
(*PPrint[{#,"  \!\(\*OverscriptBox[\(-\), \(b -> 0\)]\)>  "},#/.b->0/.\[Epsilon]->2]&/@{dfRG1L,dfRG1Lsimp,dfRG2Lsimp,dfRG2Lsimp2,dfRG2LsimpWF,dfSLE};*)
(**)
(*Limit[{dfRG1L,dfRG1Lsimp,dfRG2Lsimp,dfRG2Lsimp2,dfRG2LsimpWF,dfSLE},b->\[Infinity]]//Quiet*)
(*%/.\[Epsilon]->2*)


(* ::Subsection:: *)
(*\[Section]\[Section] 2d*)


(* ::Input:: *)
(*dfRG2Lwf*)


(* ::Item::Closed:: *)
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
(**)
(**)
(**)
(*Simulation2d=ListPlot[{{1,Around[1.2486744695483691`, 0.023270605268075166`]},{2,Around[1.1151146520584079`, 0.0134148268356009]},{3,Around[1.0768665526174213`, 0.014642777375504247`]},{4,Around[1.0474461998303197`, 0.008312476568391155]},{5,Around[1.0454880607320536`, 0.0064093876238445445`]}},PlotStyle->{RGBColor[0, 1, 0],PointSize[0.005]},PlotLegends->{"Simulation Data (old)"}];*)
(**)
(**)
(*Simulation2dGemini=ListPlot[{{0,Around[1.7534581201029278`, 0.0060679884624822]},{0.5,Around[1.3994368838209397`, 0.032318425149532204`]},{1,Around[1.274522584835579, 0.008333817846449225]},{2,Around[1.1658669951861733`, 0.001939947142635663]},{3,Around[1.1073336136072602`, 0.002384187792543366]},{4,Around[1.0737383484918805`, 0.0017665587042004246`]},{5,Around[1.0670481729478147`, 0.001216345239817617]}(*,{10,1.0251\[PlusMinus]0.0012}*)},PlotStyle->{RGBColor[0, 0.66, 0],PointSize[0.1]},PlotMarkers->X,PlotLegends->{"Simulation Data (Gemini-opt1)"}];*)
(**)
(*plotSLE=Plot[dfSLE,{b,0,endRange},PlotStyle->RGBColor[1, 0, 0],PlotRange->All, PlotLegends->{Row[{"SLE: ",TraditionalForm[#]}]&@dfSLE}];*)
(**)
(*plotRG1L=Plot[#/.\[Epsilon]->2,{b,0,endRange},PlotStyle->GrayLevel[0.5],PlotRange->All,PlotLegends->{Row[{"OLD 1-Loop: ",TraditionalForm[#]}]}]&@dfRG1L;*)
(**)
(*plotRG1Lsimp=Plot[#/.\[Epsilon]->2,{b,0,endRange},PlotStyle->RGBColor[0, 0, 1],PlotRange->All,PlotLegends->{Row[{"NEW 1-Loop: ",TraditionalForm[#]}]}]&@dfRG1Lsimp;*)
(**)
(*plotRG2Lsimp=Plot[#/.\[Epsilon]->2,{b,0,endRange},PlotStyle->RGBColor[0, 1, 1],PlotRange->All,PlotLegends->{Row[{"NEW 2-Loop: ",TraditionalForm[#]}]}]&@dfRG2Lsimp;*)
(**)
(**)
(*plotRG2Lsimp2=Plot[#/.\[Epsilon]->2,{b,0,endRange},PlotStyle->RGBColor[0.64, 0, 1],PlotRange->All,PlotLegends->{Style[Row[{"FT@2-Loop, NO WF (WRONG): \!\(\*SubscriptBox[\(d\), \(f\)]\) = ",TraditionalForm[#],"\!\(\*SubscriptBox[\(|\), \(\[Epsilon] = 2\)]\)"}],FontFamily->"Times"]}]&@dfRG2Lsimp2;*)
(**)
(*plotRG2Lwf=Plot[#/.\[Epsilon]->2,{b,0,endRange},PlotStyle->RGBColor[0.64, 0, 0],PlotRange->All,PlotLegends->{Style[Row[{"FT@2-Loop with WF (NO GRAD): \!\(\*SubscriptBox[\(d\), \(f\)]\) = ",TraditionalForm[#],"\!\(\*SubscriptBox[\(|\), \(\[Epsilon] = 2\)]\)"}],FontFamily->"Times"]}]&@dfRG2LsimpWF;*)
(**)
(**)
(*(*plotRG2Lwf=Plot[dfRG2Lwf/.\[Epsilon]->2/.a->+3,{b,0,endRange},PlotStyle->,PlotRange->All];*)*)
(*(*plotRG2L=Plot[dfRG2L/.\[Epsilon]->2,{b,0,endRange},PlotStyle->,PlotRange->All];*)*)
(**)
(*Show[{plotSLE*)
(*(*,plotRG1L*)*)
(*,plotRG1Lsimp*)
(*,plotRG2Lsimp*)
(*,plotRG2Lsimp2*)
(*,plotRG2Lwf*)
(*,Simulation2d*)
(*,Simulation2dGemini*)
(*(**)
(*,plotRG2L*)}*)
(*,PlotRange->{All,{All,2}},AxesLabel->{b,Subscript[d, f]},AxesOrigin->{0,1},PlotLabel->Row[{"d = 2"}](*,PlotLegends->Placed["AllExpressions", {Right,Top}]*),ImageSize->600, AspectRatio->0.7*)
(*]*)


(* ::Input:: *)
(**)
(**)


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


(* ::Input:: *)
(*dfRG1L:=2-b \[Epsilon]/(2+b)*)
(*dfRG1Lsimp:=2-(b \[Epsilon])/(1+2 b)*)
(**)
(*dfRG2Lsimp:=2-(b \[Epsilon])/(1+2 b)-(b (1+b+4 b^2) \[Epsilon]^2)/(2 (1+2 b)^3)*)
(*dfRG2Lsimp2:=2-(b \[Epsilon])/(1+2 b)-(b (1+b) \[Epsilon]^2)/(2 (1+2 b)^2)*)
(**)
(**)
(*dfRG2LsimpWF:=2-(b \[Epsilon])/(1+2 b)-((1+b (7+16 b)) \[Epsilon]^2)/(8 (1+2 b)^3)(*with \[CapitalGamma]\[Gamma]^2*)*)
(*(*dfRG2LsimpWF:=2-(b \[Epsilon])/(1+2 b)-((1+3 b) (1+5 b) \[Epsilon]^2)/(8 (1+2 b)^3)(*with \[CapitalGamma]\[Gamma]^1*)*)*)
(*dfRG2LsimpWF:=2-(b \[Epsilon])/(1+2 b)-(b (7+17 b) \[Epsilon]^2)/(8 (1+2 b)^3)(*with \[CapitalGamma]\[Gamma]^2, b(b-1) and 1+g*)*)
(*dfRG2LsimpWF:=2-(b \[Epsilon])/(1+2 b)-(3 b (3+5 b) \[Epsilon]^2)/(8 (1+2 b)^3)(*with \[CapitalGamma]\[Gamma]^2 AND b(b-1)*)*)
(**)
(**)
(*dfRG2L:=2-b \[Epsilon]/(2+b)-b (\[Epsilon]/(2+b))^2*)
(**)
(*(*dfRG2Lsimp:=2-(b \[Epsilon])/(1+2 b)-(b ^2 \[Epsilon]^2)/(1+2 b)^2*)(*BAD*)*)
(**)
(*dfSLE=1+3/(4(2b+1));*)


(* ::Input:: *)
(**)
(**)


(* ::Input:: *)
(*PPrint[{#,"  \!\(\*OverscriptBox[\(-\), \(b -> 0\)]\)>  "},#/.b->0/.\[Epsilon]->2]&/@{dfRG1L,dfRG1Lsimp,dfRG2Lsimp,dfRG2Lsimp2,dfRG2LsimpWF};*)
(**)
(*Limit[{dfRG1L,dfRG1Lsimp,dfRG2Lsimp,dfRG2Lsimp2,dfRG2LsimpWF},b->\[Infinity]]//Quiet*)
(*%/.\[Epsilon]->1.*)


(* ::Subsubsection:: *)
(*Extra data from my simulations*)


(* ::Item::Closed:: *)
(*Some fitting modeling*)


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


(* ::Subsubsection:: *)
(*Plots*)


(* ::Input:: *)
(*inRange=0;*)
(*endRange=15;*)
(**)
(**)
(*Simulation3d=ListPlot[{{1,1.624}(*{0,2},{1,1.624},{2,Around[1.511,0.039]},{3,Around[1.483,0.028]},{4,Around[1.431,0.016]},{5,Around[1.436,0.016]}*)(*,{10,}*)},PlotStyle->{RGBColor[1, 0, 0],PointSize[0.015]},PlotLegends->(*Placed[*){Style["Result by David Wilson"(* (Gemini-opt1)"*),FontFamily->"Times"]}(*,{Right,Top}]*)];*)
(**)
(*Simulation3dGemini=ListPlot[{{0,Around[2,0.02]},{1,Around[1.61133,0.03]},{2,Around[1.511,0.039]},{3,Around[1.483,0.028]},{4,Around[1.431,0.036]},{5,Around[1.436,0.036]},{15,Around[1.2932879741937668`, 0.003437148447463247]}(*,{10,}*)}(*{(*{0,1.753\[PlusMinus]0.006},*){2,Around[1.51,0.01]}(*,{3,1.1073\[PlusMinus]0.0024},{4,1.0737\[PlusMinus]0.0018},{5,1.0670\[PlusMinus]0.0012},{10,1.0251\[PlusMinus]0.0012}*)}*),PlotStyle->{RGBColor[0, 0.66, 0],PointSize[0.01]},PlotLegends->(*Placed[*){Style["Simulated Data \!\(\**)
(*StyleBox[\"d\",\nFontSlant->\"Italic\"]\)=3"(* (Gemini-opt1)"*),FontFamily->"Times"]}(*,{Right,Top}]*)];*)
(**)
(*plotRG1L=Plot[#/.\[Epsilon]->1,{b,inRange,endRange},PlotStyle->GrayLevel[0.5],PlotRange->All,PlotLegends->Placed[{Row[{"OLD 1-Loop: ",TraditionalForm[#]}]},{Right,Top}]]&@dfRG1L;*)
(**)
(*plotRG1Lsimp=Plot[#/.\[Epsilon]->1,{b,inRange,endRange},PlotStyle->RGBColor[0, 0, 1],PlotRange->All,PlotLegends->(*Placed[*){Style[Row[{"FT@1-Loop: \!\(\*SubscriptBox[\(d\), \(f\)]\) = ",TraditionalForm[#],"\!\(\*SubscriptBox[\(|\), \(\[Epsilon] = 1\)]\)"}],FontFamily->"Times"]}(*,{Right,Top}]*)]&@dfRG1Lsimp;*)
(**)
(*plotRG2Lsimp=Plot[#/.\[Epsilon]->1,{b,0,endRange},PlotStyle->RGBColor[0, 1, 1],PlotRange->All,PlotLegends->(*Placed[*){Style[Row[{"FT@2-Loop: \!\(\*SubscriptBox[\(d\), \(f\)]\) = ",TraditionalForm[#],"\!\(\*SubscriptBox[\(|\), \(\[Epsilon] = 1\)]\)"}],FontFamily->"Times"]}(*,{Right,Top}]*)]&@dfRG2Lsimp;*)
(**)
(*plotRG2Lsimp2=Plot[#/.\[Epsilon]->1,{b,0,endRange},PlotStyle->RGBColor[0.64, 0, 1],PlotRange->All,PlotLegends->(*Placed[*){Style[Row[{"FT@2-Loop, NO WF (WRONG): \!\(\*SubscriptBox[\(d\), \(f\)]\) = ",TraditionalForm[#],"\!\(\*SubscriptBox[\(|\), \(\[Epsilon] = 1\)]\)"}],FontFamily->"Times"]}(*,{Right,Top}]*)]&@dfRG2Lsimp2;*)
(**)
(**)
(*plotRG2Lwf=Plot[#/.\[Epsilon]->1,{b,0,endRange},PlotStyle->RGBColor[0.64, 0, 0],PlotRange->All,PlotLegends->(*Placed[*){Style[Row[{"FT@2-Loop with WF (NO GRAD): \!\(\*SubscriptBox[\(d\), \(f\)]\) = ",TraditionalForm[#],"\!\(\*SubscriptBox[\(|\), \(\[Epsilon] = 1\)]\)"}],FontFamily->"Times"]}(*,{Right,Top}]*)]&@dfRG2LsimpWF;*)
(**)
(**)
(*(*fitPlot=Plot[fitFunc,{b,inRange,endRange},PlotStyle->Red,PlotRange->All];*)*)
(**)
(**)
(*Show[{(*plotRG1L*)
(*,*)plotRG1Lsimp*)
(*,plotRG2Lsimp*)
(*,plotRG2Lsimp2*)
(*,plotRG2Lwf*)
(*,Simulation3d*)
(*,Simulation3dGemini(*,Graphics[{Red,Text[Style["Result \nby David Wilson"(* (Gemini-opt1)"*),FontFamily->"Times"],{1,1.45}]}]*)*)
(*(*,Plot[(2\[VeryThinSpace]+2.85 b)/(1+2 b),{b,0,15},PlotRange->All]*)},PlotRange->{{0,endRange},{1.2,2}},AxesLabel->{b,Subscript[d, f]},AxesOrigin->{0,1.3},ImageSize->Large,PlotLabel->Row[{"d = 3"}],GridLines->{None,(Limit[{dfRG1L,dfRG1Lsimp,dfRG2Lsimp,dfRG2L,dfRG2Lsimp2},b->\[Infinity]]/.\[Epsilon]->1.//Quiet)}(*,AspectRatio->1*)]*)


(* ::Input:: *)
(**)
(**)


(* ::Input:: *)
(*(2 +2.873 b)/(1+2 b)/.b->1.*)


(* ::Input:: *)
(*(1.61133-1)*3*)


(* ::Input:: *)
(*(*GUESS: 1+1/(2b+1)*)*)


(* ::Input:: *)
(*points=Times[{1,(2#[[1]]+1)/2},#]&/@(Plus[{0,-1},#]&/@{{0,Around[2,0.001]},{0.5`,Around[1.750971791907542, 0.020449584076101025`]},{1,Around[1.6242699928290363`, 0.005509964478846202]},{3,Around[1.534, 0.006](*1.564\[PlusMinus]0.017*)},{4,Around[1.489,0.006](*1.478\[PlusMinus]0.018*)},{15,Around[1.3018565168484142`, 0.01693594856418696]}(*,{100,1.001\[PlusMinus]0.013}*)})*)
(**)
(*lm=LinearModelFit[Delete[points,{(*{-1},{-2}*)}],x,x] *)
(**)
(*Show[{ListPlot[points(*{(*{0,1.753\[PlusMinus]0.006},*){2,Around[1.51,0.01]}(*,{3,1.1073\[PlusMinus]0.0024},{4,1.0737\[PlusMinus]0.0018},{5,1.0670\[PlusMinus]0.0012},{10,1.0251\[PlusMinus]0.0012}*)}*),PlotStyle->{RGBColor[0, 0.66, 0],PointSize[0.01]},PlotLegends->{Style["Simulated Data \!\(\*\nStyleBox[\"d\",\nFontSlant->\"Italic\"]\)=3"(*(Gemini-opt1)"*),FontFamily->"Times"]}]*)
(*,Plot[lm[b],{b,0,15}]}*)
(*,PlotRange->All(*{{0,4},{0,20}}*)]*)
(**)
(*Show[{ListPlot[points(*{(*{0,1.753\[PlusMinus]0.006},*){2,Around[1.51,0.01]}(*,{3,1.1073\[PlusMinus]0.0024},{4,1.0737\[PlusMinus]0.0018},{5,1.0670\[PlusMinus]0.0012},{10,1.0251\[PlusMinus]0.0012}*)}*),PlotStyle->{RGBColor[0, 0.66, 0],PointSize[0.01]},PlotLegends->{Style["Simulated Data \!\(\*\nStyleBox[\"d\",\nFontSlant->\"Italic\"]\)=3"(*(Gemini-opt1)"*),FontFamily->"Times"]}]*)
(*,Plot[lm[b],{b,0,15}]}*)
(*,PlotRange->{{0,4},{0,10}}]*)


(* ::Input:: *)
(*lm=LinearModelFit[Delete[points,{(*{-1},{-2}*)}],x,x]*)


(* ::Input:: *)
(*Show[{ListPlot[points(*{(*{0,1.753\[PlusMinus]0.006},*){2,Around[1.51,0.01]}(*,{3,1.1073\[PlusMinus]0.0024},{4,1.0737\[PlusMinus]0.0018},{5,1.0670\[PlusMinus]0.0012},{10,1.0251\[PlusMinus]0.0012}*)}*),PlotStyle->{RGBColor[0, 0.66, 0],PointSize[0.01]},PlotLegends->Placed[{Style["Simulated Data \!\(\**)
(*StyleBox[\"d\",\nFontSlant->\"Italic\"]\)=3"(* (Gemini-opt1)"*),FontFamily->"Times"]},{Right,Top}]]*)
(*,Plot[lm[b],{b,0,100}]},PlotRange->All(*{{0,4},{0,20}}*)]*)


(* ::Input:: *)
(*points=Times[{1,2#[[1]]+1},#]&/@(Plus[{0,-1},#]&/@{{0,Around[2,0.02]},{1,Around[1.624,0.00001]},{2,Around[1.511,0.039]},{3,Around[1.483,0.028]},{4,Around[1.431,0.036]},{5,Around[1.436,0.036]}(*,{15,1.2933\[PlusMinus]0.0034}*)(*,{10,}*)})*)


(* ::Input:: *)
(*lm=LinearModelFit[points,x,x]*)


(* ::Input:: *)
(*Show[{ListPlot[points(*{(*{0,1.753\[PlusMinus]0.006},*){2,Around[1.51,0.01]}(*,{3,1.1073\[PlusMinus]0.0024},{4,1.0737\[PlusMinus]0.0018},{5,1.0670\[PlusMinus]0.0012},{10,1.0251\[PlusMinus]0.0012}*)}*),PlotStyle->{RGBColor[0, 0.66, 0],PointSize[0.01]},PlotLegends->Placed[{Style["Simulated Data \!\(\**)
(*StyleBox[\"d\",\nFontSlant->\"Italic\"]\)=3"(* (Gemini-opt1)"*),FontFamily->"Times"]},{Right,Top}]]*)
(*,Plot[lm[b],{b,0,15}]}]*)


(* ::Input:: *)
(*lm[b]/(2b+1)+1//FullSimplify*)


(* ::Input:: *)
(*lm[b]/(2b+1)+1//FullSimplify*)


(* ::Text:: *)
(*THIS LOOKS VERY PROMISING!!! I'M USING THE REPLACEMENT rule {GradImmediateIntNotAllowed:>0,h->1,h2->1,H->0,H2->0,a2->1-a-3/b,a->0,A2->1,A->1}*)


(* ::Input:: *)
(*dfRG2Lsimp2/.b->15./.\[Epsilon]->1*)
(*dfRG2Lsimp/. b->15./. \[Epsilon]->1*)
(*(*WHICH ONE IS IT????	????*)*)


(* ::Input:: *)
(**)


(* ::Subsection::Closed:: *)
(*Try to do Pade: BEST IS PadeApproximant[% , {t, 0, {0, 2}}]*)


(* ::Item::Closed:: *)
(*Test on Borel*)


(* ::Input:: *)
(*p[g_]:=Sum[(-1)^n n! g^n,{n,0,\[Infinity]}]*)


(* ::Input:: *)
(*p[g]/.g^n_.:>t^n/n!*)
(*% Exp[-t/g]/g*)
(*Integrate[%,{t,0,\[Infinity]},Assumptions->Re[g]>0]*)
(*Series[%,{g,0,5}]*)


(* ::Item:: *)
(*Continues*)


(* ::Input:: *)
(*dfRG2Lsimp2*)
(*%/.\[Epsilon]^n_.:>\[Epsilon]^n(*/(n!)*)*)
(*padeDf=PadeApproximant[% ,{\[Epsilon],0,{0,2}}]*)
(*%//FS*)
(*Series[%,{t,0,2}]*)


(* ::Input:: *)
(*dfRG2LsimpWF*)
(*%/.\[Epsilon]^n_.:>\[Epsilon]^n(*/(n!)*)*)
(*padeDfWF=PadeApproximant[% ,{\[Epsilon],0,{0,2}}]*)
(*%//FS*)
(*Series[%,{t,0,2}]*)


(* ::Input:: *)
(*(*2D	!*)*)
(*endRange=5;*)
(*plotpadeDf=Plot[#/.\[Epsilon]->2,{b,0,5},PlotStyle->RGBColor[Rational[2, 3], Rational[2, 3], 0],PlotRange->All,PlotLegends->Placed[{Style[Row[{"FT@2-Loop PadeAppr: \!\(\*SubscriptBox[\(d\), \(f\)]\) = ",TraditionalForm[#],"\!\(\*SubscriptBox[*)
(*StyleBox[\"|\",\nFontSize->24], \(\[Epsilon] = 2\)]\)"}],FontFamily->"Times"]},{Right,Top}]]&@padeDf;*)
(*plotpadeDfWF=Plot[#/.\[Epsilon]->2,{b,0,5},PlotStyle->RGBColor[0.88, Rational[2, 3], 0],PlotRange->All,PlotLegends->Placed[{Style[Row[{"FT@2-Loop with WF PadeAppr: \!\(\*SubscriptBox[\(d\), \(f\)]\) = ",TraditionalForm[#],"\!\(\*SubscriptBox[*)
(*StyleBox[\"|\",\nFontSize->24], \(\[Epsilon] = 2\)]\)"}],FontFamily->"Times"]},{Right,Top}]]&@padeDfWF;*)
(**)
(**)
(*Simulation2dGemini=ListPlot[{{0,Around[1.7534581201029278`, 0.0060679884624822]*Around[1,0.015]},{0.5,Around[1.3994368838209397`, 0.032318425149532204`]},{1,Around[1.274522584835579, 0.008333817846449225]*Around[1,0.015]},{2,Around[1.1658669951861733`, 0.001939947142635663]*Around[1,0.015]},{3,Around[1.1073336136072602`, 0.002384187792543366]*Around[1,0.015]},{4,Around[1.0737383484918805`, 0.0017665587042004246`]*Around[1,0.015]},{5,Around[1.0670481729478147`, 0.001216345239817617]*Around[1,0.015]}(*,{10,1.0251\[PlusMinus]0.0012}*)},PlotStyle->{RGBColor[0, 0.66, 0],PointSize[0.005]},(*PlotMarkers->X,*)PlotLegends->Placed[{Style["Simulation Data ",FontFamily->"Times"]},{Right,Top}]];*)
(**)
(*plotSLE=Plot[dfSLE,{b,0,endRange},PlotStyle->RGBColor[1, 0, 0],PlotRange->All, PlotLegends->Placed[{Style[Row[{"SLE: \!\(\*SubscriptBox[\(d\), \(f\)]\) = ",TraditionalForm[#]}],FontFamily->"Times"]},{Right,Top}]]&@dfSLE;*)
(**)
(*plotRG1Lsimp=Plot[#/.\[Epsilon]->2,{b,0,endRange},PlotStyle->RGBColor[0, 0, 1],PlotRange->All,PlotLegends->Placed[{Style[Row[{"FT@1-Loop: \!\(\*SubscriptBox[\(d\), \(f\)]\) = ",TraditionalForm[#],"\!\(\*SubscriptBox[*)
(*StyleBox[\"|\",\nFontSize->24], \(\[Epsilon] = 2\)]\)"}],FontFamily->"Times"]},{Right,Top}]]&@dfRG1Lsimp;*)
(**)
(**)
(**)
(*plotRG2Lsimp2=Plot[#/.\[Epsilon]->2,{b,0,endRange},PlotStyle->RGBColor[0.64, 0, 1],PlotRange->All,PlotLegends->Placed[{Style[Row[{"FT@2-Loop: \!\(\*SubscriptBox[\(d\), \(f\)]\) = ",TraditionalForm[#],"\!\(\*SubscriptBox[*)
(*StyleBox[\"|\",\nFontSize->24], \(\[Epsilon] = 2\)]\)"}],FontFamily->"Times"]},{Right,Top}]]&@dfRG2Lsimp2;*)
(**)
(**)
(*(*plotRG2Lwf=Plot[dfRG2Lwf/.\[Epsilon]->2/.a->+3,{b,0,endRange},PlotStyle->,PlotRange->All];*)*)
(*(*plotRG2L=Plot[dfRG2L/.\[Epsilon]->2,{b,0,endRange},PlotStyle->,PlotRange->All];*)*)
(**)
(**)
(*Show[{*)
(*(*,plotRG1L*)*)
(*plotRG1Lsimp*)
(*(*,plotRG2Lsimp*)*)
(*,plotRG2Lsimp2*)
(*(*,Simulation2d*)*)
(*,plotpadeDf*)
(*,plotpadeDfWF*)
(*,plotSLE*)
(*,Simulation2dGemini*)
(*}*)
(*,PlotRange->All,AxesLabel->{Style[b,Italic],Style[Subscript[d, f],Italic]},AxesOrigin->{0,1},PlotLabel->Row[{"d = 2"}](*,PlotLegends->Placed["AllExpressions", {Right,Top}]*),ImageSize->700, (*AspectRatio->0.7,*)PlotLegends->Placed[Automatic, Right]*)
(**)
(*]*)


(* ::Input:: *)
(*(*2D	!*)*)
(*inRange=0;*)
(*endRange=5;*)
(*imageSize=600;*)
(**)
(*plotpadeDf=Plot[#/.\[Epsilon]->2,{b,0,endRange},PlotStyle->RGBColor[0.64, 0, 0],PlotRange->All,PlotLegends->(*Placed[*){Style[Row[{"FT@2-Loop PadeAppr: \!\(\*SubscriptBox[\(d\), \(f\)]\) = ",TraditionalForm[#],"\!\(\*SubscriptBox[*)
(*StyleBox[\"|\",\nFontSize->24], \(\[Epsilon] = 2\)]\)"}],FontFamily->"Times"]}(*,{Right,Top}]*)]&@padeDf;*)
(**)
(*plotpadeDfWF=Plot[#/.\[Epsilon]->2,{b,0,endRange},PlotStyle->Directive[RGBColor[0.88, Rational[2, 3], 0],Dashed],PlotRange->All,PlotLegends->(*Placed[*){Style[Row[{"FT@2-Loop with WF PadeAppr: \!\(\*SubscriptBox[\(d\), \(f\)]\) = ",TraditionalForm[#],"\!\(\*SubscriptBox[*)
(*StyleBox[\"|\",\nFontSize->24], \(\[Epsilon] = 2\)]\)"}],FontFamily->"Times"]}(*,{Right,Top}]*)]&@padeDfWF;*)
(**)
(*plotSLE=Plot[dfSLE,{b,0,endRange},PlotStyle->RGBColor[1, 0, 0],PlotRange->All, PlotLegends->(*Placed[*){Style[Row[{"SLE: \!\(\*SubscriptBox[\(d\), \(f\)]\) = ",TraditionalForm[#]}],FontFamily->"Times"]}(*,{Right,Top}]*)]&@dfSLE;*)
(**)
(*Simulation2dGemini(*STILL OLD*)=ListPlot[{{0,Around[1.7534581201029278`, 0.0060679884624822]*Around[1,0.015]},{0.5,Around[1.3994368838209397`, 0.032318425149532204`]},{1,Around[1.274522584835579, 0.008333817846449225]*Around[1,0.015]},{2,Around[1.1658669951861733`, 0.001939947142635663]*Around[1,0.015]},{3,Around[1.1073336136072602`, 0.002384187792543366]*Around[1,0.015]},{4,Around[1.0737383484918805`, 0.0017665587042004246`]*Around[1,0.015]},{5,Around[1.0670481729478147`, 0.001216345239817617]*Around[1,0.015]}(*,{10,1.0251\[PlusMinus]0.0012}*)},PlotStyle->{RGBColor[0, 0.66, 0],PointSize[0.005]},(*PlotMarkers->X,*)PlotLegends->(*Placed[*){Style["Simulation Data ",FontFamily->"Times"]}(*,{Right,Top}]*)];*)
(**)
(**)
(**)
(*plotRG1L=Plot[#/.\[Epsilon]->2,{b,inRange,endRange},PlotStyle->GrayLevel[0.5],PlotRange->All,PlotLegends->(*Placed[*){Row[{"OLD 1-Loop: ",TraditionalForm[#]}]}(*,{Right,Top}]*)]&@dfRG1L;*)
(**)
(*plotRG1Lsimp=Plot[#/.\[Epsilon]->2,{b,inRange,endRange},PlotStyle->RGBColor[0, 0, 1],PlotRange->All,PlotLegends->(*Placed[*){Style[Row[{"FT@1-Loop: \!\(\*SubscriptBox[\(d\), \(f\)]\) = ",TraditionalForm[#],"\!\(\*SubscriptBox[*)
(*StyleBox[\"|\",\nFontSize->24], \(\[Epsilon] = 2\)]\)"}],FontFamily->"Times"]}(*,{Right,Top}]*)]&@dfRG1Lsimp;*)
(**)
(*plotRG2Lsimp=Plot[#/.\[Epsilon]->2,{b,0,endRange},PlotStyle->RGBColor[0, 1, 1],PlotRange->All,PlotLegends->(*Placed[*){Style[Row[{"FT@2-Loop: \!\(\*SubscriptBox[\(d\), \(f\)]\) = ",TraditionalForm[#],"\!\(\*SubscriptBox[*)
(*StyleBox[\"|\",\nFontSize->24], \(\[Epsilon] = 2\)]\)"}],FontFamily->"Times"]}(*,{Right,Top}]*)]&@dfRG2Lsimp;*)
(**)
(*plotRG2Lsimp2=Plot[#/.\[Epsilon]->2,{b,0,endRange},PlotStyle->RGBColor[0.64, 0, 1],PlotRange->All,PlotLegends->(*Placed[*){Style[Row[{"FT@2-Loop, NO WF (WRONG): \!\(\*SubscriptBox[\(d\), \(f\)]\) = ",TraditionalForm[#],"\!\(\*SubscriptBox[\(|\), \(\[Epsilon] = 2\)]\)"}],FontFamily->"Times"]}(*,{Right,Top}]*)]&@dfRG2Lsimp2;*)
(**)
(**)
(*plotRG2Lwf=Plot[#/.\[Epsilon]->2,{b,0,endRange},PlotStyle->Directive[RGBColor[Rational[2, 3], Rational[2, 3], 0],Dashed],PlotRange->All,PlotLegends->(*Placed[*){Style[Row[{"FT@2-Loop with WF (NO GRAD): \!\(\*SubscriptBox[\(d\), \(f\)]\) = ",TraditionalForm[#],"\!\(\*SubscriptBox[\(|\), \(\[Epsilon] = 2\)]\)"}],FontFamily->"Times"]}(*,{Right,Top}]*)]&@dfRG2LsimpWF;*)
(**)
(*plots={(*plotRG1L*)
(*,*)plotRG1Lsimp*)
(*(*,plotRG2Lsimp*)*)
(*,plotRG2Lsimp2*)
(*,plotRG2Lwf(*,plotRG2L*)(*,fitPlot*)*)
(*,plotpadeDf*)
(*,plotpadeDfWF*)
(**)
(*,Simulation2dGemini(*,Graphics[{Red,Text[Style["Result \nby David Wilson"(* (Gemini-opt1)"*),FontFamily->"Times"],{1,1.45}]}]*)*)
(*,plotSLE*)
(*};*)
(**)
(*gridLines=(Limit[{dfRG1Lsimp,dfRG2Lsimp2,padeDf, padeDfWF},b->\[Infinity]]/.\[Epsilon]->2.//Quiet);*)
(**)
(*Show[plots,PlotRange->{{0,endRange},{0.5,2}},AxesLabel->{Style[b,Italic],Style[Subscript[d, f],Italic]},AxesOrigin->{0,1},PlotLabel->Row[{"d = 2"}],ImageSize->imageSize(*,AspectRatio->1*),GridLines->{None,gridLines}]*)


(* ::Input:: *)
(*Limit[padeDf/.t->2.,b->\[Infinity]]*)


(* ::Input:: *)
(*(*3D	!*)*)
(*inRange=0;*)
(*endRange=100;*)
(*imageSize=600;*)
(**)
(*plotpadeDf=Plot[#/.\[Epsilon]->1,{b,0,endRange},PlotStyle->RGBColor[0.64, 0, 0],PlotRange->All,PlotLegends->(*Placed[*){Style[Row[{"FT@2-Loop PadeAppr: \!\(\*SubscriptBox[\(d\), \(f\)]\) = ",TraditionalForm[#],"\!\(\*SubscriptBox[*)
(*StyleBox[\"|\",\nFontSize->24], \(\[Epsilon] = 1\)]\)"}],FontFamily->"Times"]}(*,{Right,Top}]*)]&@padeDf;*)
(**)
(*plotpadeDfWF=Plot[#/.\[Epsilon]->1,{b,0,endRange},PlotStyle->Directive[RGBColor[0.88, Rational[2, 3], 0],Dashed],PlotRange->All,PlotLegends->(*Placed[*){Style[Row[{"FT@2-Loop with WF PadeAppr: \!\(\*SubscriptBox[\(d\), \(f\)]\) = ",TraditionalForm[#],"\!\(\*SubscriptBox[*)
(*StyleBox[\"|\",\nFontSize->24], \(\[Epsilon] = 1\)]\)"}],FontFamily->"Times"]}(*,{Right,Top}]*)]&@padeDfWF;*)
(**)
(*Simulation3d=ListPlot[{{1,1.624}(*{0,2},{1,1.624},{2,Around[1.511,0.039]},{3,Around[1.483,0.028]},{4,Around[1.431,0.016]},{5,Around[1.436,0.016]}*)(*,{10,}*)},PlotStyle->{RGBColor[1, 0, 0],PointSize[0.005]},PlotLegends->(*Placed[*){Style["Result by David Wilson"(* (Gemini-opt1)"*),FontFamily->"Times"]}(*,{Right,Top}]*)];*)
(**)
(*Simulation3dGemini(*OLD*)=ListPlot[{{0,Around[2,0.02]},{0.5,Around[1.7397436370342432`, 0.035796700707508546`]},{1,Around[1.61133,0.03]},{2,Around[1.511,0.039]},{3,Around[1.483,0.028]},{4,Around[1.431,0.036]},{5,Around[1.436,0.036]}(*,{10,}*)}(*{(*{0,1.753\[PlusMinus]0.006},*){2,Around[1.51,0.01]}(*,{3,1.1073\[PlusMinus]0.0024},{4,1.0737\[PlusMinus]0.0018},{5,1.0670\[PlusMinus]0.0012},{10,1.0251\[PlusMinus]0.0012}*)}*),PlotStyle->{RGBColor[0, 0.66, 0],PointSize[0.01]},(*PlotMarkers->X,*)PlotLegends->(*Placed[*){Style["Simulated Data "(* (Gemini-opt1)"*),FontFamily->"Times"]}(*,{Right,Top}]*)];*)
(**)
(*Simulation3dGemini(*NEW:HALO*)=ListPlot[{{0.5`,Around[1.750971791907542, 0.020449584076101025`]},{1,Around[1.6242699928290363`, 0.005509964478846202]},{3,Around[1.534, 0.006](*1.564\[PlusMinus]0.017*)},{4,Around[1.489,0.006](*1.478\[PlusMinus]0.018*)},{15,Around[1.3018565168484142`, 0.01693594856418696]},{100,Around[1.0009689747586097`, 0.01332316148971623]}},PlotStyle->{RGBColor[0, 0.66, 0],PointSize[0.01]},(*PlotMarkers->X,*)PlotLegends->(*Placed[*){Style["Simulated Data "(* (Gemini-opt1)"*),FontFamily->"Times"]}(*,{Right,Top}]*)];*)
(**)
(**)
(**)
(*plotRG1L=Plot[#/.\[Epsilon]->1,{b,inRange,endRange},PlotStyle->GrayLevel[0.5],PlotRange->All,PlotLegends->(*Placed[*){Row[{"OLD 1-Loop: ",TraditionalForm[#]}]}(*,{Right,Top}]*)]&@dfRG1L;*)
(**)
(*plotRG1Lsimp=Plot[#/.\[Epsilon]->1,{b,inRange,endRange},PlotStyle->RGBColor[0, 0, 1],PlotRange->All,PlotLegends->(*Placed[*){Style[Row[{"FT@1-Loop: \!\(\*SubscriptBox[\(d\), \(f\)]\) = ",TraditionalForm[#],"\!\(\*SubscriptBox[*)
(*StyleBox[\"|\",\nFontSize->24], \(\[Epsilon] = 1\)]\)"}],FontFamily->"Times"]}(*,{Right,Top}]*)]&@dfRG1Lsimp;*)
(**)
(*plotRG2Lsimp=Plot[#/.\[Epsilon]->1,{b,0,endRange},PlotStyle->RGBColor[0, 1, 1],PlotRange->All,PlotLegends->(*Placed[*){Style[Row[{"FT@2-Loop: \!\(\*SubscriptBox[\(d\), \(f\)]\) = ",TraditionalForm[#],"\!\(\*SubscriptBox[*)
(*StyleBox[\"|\",\nFontSize->24], \(\[Epsilon] = 1\)]\)"}],FontFamily->"Times"]}(*,{Right,Top}]*)]&@dfRG2Lsimp;*)
(**)
(*plotRG2Lsimp2=Plot[#/.\[Epsilon]->1,{b,0,endRange},PlotStyle->RGBColor[0.64, 0, 1],PlotRange->All,PlotLegends->(*Placed[*){Style[Row[{"FT@2-Loop, NO WF (WRONG): \!\(\*SubscriptBox[\(d\), \(f\)]\) = ",TraditionalForm[#],"\!\(\*SubscriptBox[\(|\), \(\[Epsilon] = 1\)]\)"}],FontFamily->"Times"]}(*,{Right,Top}]*)]&@dfRG2Lsimp2;*)
(**)
(**)
(*plotRG2Lwf=Plot[#/.\[Epsilon]->1,{b,0,endRange},PlotStyle->Directive[RGBColor[Rational[2, 3], Rational[2, 3], 0],Dashed],PlotRange->All,PlotLegends->(*Placed[*){Style[Row[{"FT@2-Loop with WF (NO GRAD): \!\(\*SubscriptBox[\(d\), \(f\)]\) = ",TraditionalForm[#],"\!\(\*SubscriptBox[\(|\), \(\[Epsilon] = 1\)]\)"}],FontFamily->"Times"]}(*,{Right,Top}]*)]&@dfRG2LsimpWF;*)
(**)
(*plots={(*plotRG1L*)
(*,*)plotRG1Lsimp*)
(*(*,plotRG2Lsimp*)*)
(*,plotRG2Lsimp2*)
(*,plotRG2Lwf(*,plotRG2L*)(*,fitPlot*)*)
(*,plotpadeDf*)
(*,plotpadeDfWF*)
(**)
(*,Simulation3dGemini(*,Graphics[{Red,Text[Style["Result \nby David Wilson"(* (Gemini-opt1)"*),FontFamily->"Times"],{1,1.45}]}]*)*)
(*,Simulation3d*)
(*,Plot[(2/(2x+1))*)
(*};*)
(**)
(*gridLines=(Limit[{dfRG1Lsimp,dfRG2Lsimp2,padeDf, padeDfWF},b->\[Infinity]]/.\[Epsilon]->1.//Quiet);*)
(**)
(*Show[plots,PlotRange->{{0,endRange},{1,2}},AxesLabel->{Style[b,Italic],Style[Subscript[d, f],Italic]},AxesOrigin->{0,1},PlotLabel->Row[{"d = 3"}],ImageSize->imageSize(*,AspectRatio->1*),GridLines->{None,gridLines}]*)
(**)
(*Show[plots,PlotRange->{{0,15},{1.3,2}},AxesLabel->{Style[b,Italic],Style[Subscript[d, f],Italic]},AxesOrigin->{0,1.3},PlotLabel->Row[{"d = 3"}],ImageSize->imageSize(*,AspectRatio->1*),GridLines->{None,gridLines}]*)
(**)
(*Show[plots,PlotRange->{{0,5},{1.3,2}},AxesLabel->{Style[b,Italic],Style[Subscript[d, f],Italic]},AxesOrigin->{0,1.3},PlotLabel->Row[{"d = 3"}],ImageSize->imageSize(*,AspectRatio->1*),GridLines->{None,gridLines}]*)


(* ::Input:: *)
(**)


(* ::Input:: *)
(*I*)


(* ::Input:: *)
(*Limit[padeDf/.t->2.,b->\[Infinity]]*)


(* ::Input:: *)
(*padeDf*)
(*Integrate[% Exp[-t/\[Epsilon]]/\[Epsilon],{t,0,\[Infinity]},Assumptions->0<\[Epsilon]<=2&&b>0]*)


(* ::Input:: *)
(*(2 ((1+4 b+3 b^2) \[Epsilon]+8 I b (1+2 b) E^(-((4+8 b)/(\[Epsilon]+b \[Epsilon]))) (\[Pi]+I ExpIntegralEi[(4+8 b)/(\[Epsilon]+b \[Epsilon])])))/((1+b)^2 \[Epsilon]);*)
(*Assuming[b>0,Series[%,{\[Epsilon],0,2}]]*)
