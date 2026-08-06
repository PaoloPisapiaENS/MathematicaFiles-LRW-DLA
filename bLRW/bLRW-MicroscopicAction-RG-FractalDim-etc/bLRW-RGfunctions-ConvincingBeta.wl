(* ::Package:: *)

(* ::Input::Initialization:: *)
(*SetOptions[$FrontEndSession,NotebookAutoSave->True]*)
(*With[{nb=EvaluationNotebook[]},RunScheduledTask[If["ModifiedInMemory"/. NotebookInformation[nb],NotebookSave[nb]],300]]
NotebookSave[]*)
FS=FullSimplify;
<<"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\Kay-initialization.m"


(* ::Input:: *)
(*Quit*)


(* ::Input:: *)
(*"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\MathematicaFiles-LRW-DLA-bLRW\\bLRW\\bLRW-MicroscopicAction-RG-FractalDim-etc\\bLRW-RGfunctions.nb"*)


(* ::Title:: *)
(*\[Beta]Function[] and \[Gamma]Function[]*)


(* ::Section:: *)
(*\[Beta]Function[] Definitions*)


(* ::Subsection::Closed:: *)
(*For the RG with effective finite quantities (i.e. renormalization without CTs)*)


(* ::Input::Initialization:: *)
ClearAll[\[Beta]Function];

Options[\[Beta]Function]={"print"->False,"g0Order"->0};

\[Beta]Function[coupling_,OptionsPattern[]]:=Module[{gr,\[Beta]f,nLoop,i},
Clear[g,g0,\[Mu],\[Epsilon]];

nLoop=OptionValue["g0Order"];
If[nLoop==0,nLoop=Exponent[coupling,g0]];

gr=Normal@Series[coupling,{g0,0,nLoop}];

If[OptionValue["print"],
Print["Initial effective couling:\n ",gr,"\n"];];

\[Beta]f=-\[Mu] D[gr,\[Mu]]//Expand;
If[OptionValue["print"],
Print["\n\[Beta]-function with bare coupling: ", \[Beta]f,"\n"];];

gr=g-coupling+g0 \[Mu]^-\[Epsilon];

If[OptionValue["print"],
Print[" Bare coupling= \n ",gr,"\n"];];

(* Invert g(g0) *)
Do[\[Beta]f=\[Beta]f/.g0^n_ \[Mu]^(-n_ \[Epsilon]):>(gr)^n \[Mu]^(n \[Epsilon])//Expand;
\[Beta]f=\[Beta]f/.(g0 ):>(gr)\[Mu]^ \[Epsilon]//Expand;
(*\[Beta]f=Series[\[Beta]f,{g0,0,nLoop}]//Expand;*)
\[Beta]f=\[Beta]f/.g0^n_/;n>nLoop:>0;
\[Beta]f=\[Beta]f/.g0^n_/;n==nLoop:>(g \[Mu]^\[Epsilon])^n//Expand;
If[OptionValue["print"],
Print[\[Beta]f//FullSimplify,"\n"];];
,{i,1,nLoop}];

(*For[i=1,i<=nLoop,i++,
\[Beta]f=\[Beta]f/.g0^n_ \[Mu]^(n_ \[Epsilon]):>(gr)^n//Expand;
\[Beta]f=\[Beta]f/.(g0 \[Mu]^\[Epsilon]):>(gr)//Expand;
];*)

\[Beta]f=Normal[\[Beta]f]/.g0^n_ :>(g \[Mu]^\[Epsilon])^n//Expand;
\[Beta]f=\[Beta]f/.(g0 ):>(g \[Mu]^\[Epsilon])//Expand;
\[Beta]f=Series[\[Beta]f,{g,0,nLoop}]//Map[Expand,#]&;
(*Print[\[Beta]f];*)
(*\[Beta]f=Normal[\[Beta]f];*)
Return[\[Beta]f//FullSimplify]]


(* ::Subsection:: *)
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


(* ::Subitem:: *)
(*Let's check Kay's ansatz for g (from his email "picture"): OK, IT IS FINITE TOO*)


(* ::Input:: *)
(*g=g0 \[Mu]^-\[Epsilon]-(b+2)banana (g0 \[Mu]^-\[Epsilon])^2+(g0 \[Mu]^-\[Epsilon])^3 ( (b^2+2)doubleBanana + 4(2b+1) hat );*)
(*\[Beta]Function[g,"print"->False]*)
(*%/.banana ->1/\[Epsilon]/.doubleBanana ->1/\[Epsilon]^2/.hat ->1/(2\[Epsilon]^2)+1/(4\[Epsilon])//FullSimplify*)
(*RGeq2=Normal[%]==0;*)


(* ::Item:: *)
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


(* ::Subsubsection:: *)
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


(* ::Input::Initialization:: *)
ClearAll[\[Gamma]Function];


Options[\[Gamma]Function]={"print"->False,"g0Order"->0};


\[Gamma]Function[observable_,bareCoupling_, OptionsPattern[]]:=Module[{U,gr,\[Gamma]f,nLoop,i},
Clear[g,g0,\[Mu],\[Epsilon]];


nLoop=OptionValue["g0Order"];
If[nLoop==0,nLoop=Exponent[bareCoupling,g0]-1];
(*Print[nLoop]*);

gr=Normal@Series[bareCoupling,{g0,0,nLoop}];

U=Normal@Series[observable,{g0,0,nLoop}];

\[Gamma]f=-\[Mu] D[Log[U],\[Mu]]//Expand;
If[OptionValue["print"],
Print[" \[Gamma]f(\!\(\*SubscriptBox[
StyleBox[\"g\",\nBackground->RGBColor[0.9, 1, 1]], \(0\)]\))= \n ",\[Gamma]f];];


gr=g-gr+g0 \[Mu]^-\[Epsilon];

If[OptionValue["print"],
Print[" Bare coupling= \n ",gr];];

Do[\[Gamma]f=\[Gamma]f/.g0^n_ :>(gr)^n \[Mu]^(n \[Epsilon])//Expand;
\[Gamma]f=\[Gamma]f/.(g0 ):>(gr)\[Mu]^\[Epsilon]//Expand;
\[Gamma]f=\[Gamma]f/.g0^n_/;n>nLoop:>0;
\[Gamma]f=\[Gamma]f/.g0^n_/;n==nLoop:>(g \[Mu]^\[Epsilon])^n//Expand;
,{i,1,nLoop}];

(*For[i=1,i<=nLoop,i++,
\[Gamma]f=\[Gamma]f/.g0^n_ \[Mu]^(n_ \[Epsilon]):>(gr)^n//Expand;
\[Gamma]f=\[Gamma]f/.(g0 \[Mu]^\[Epsilon]):>(gr)//Expand;
];*)

\[Gamma]f=\[Gamma]f/.g0^n_ :>(g)^n \[Mu]^(n \[Epsilon])//Expand;
\[Gamma]f=\[Gamma]f/.(g0 ):>(g)\[Mu]^\[Epsilon]//Expand;
(*
If[OptionValue["print"],
Print[" \[Gamma]f(g)= \n ",\[Gamma]f];];*)

\[Gamma]f=Series[\[Gamma]f,{g,0,nLoop}]//Expand;
\[Gamma]f=Factor@Simplify/@\[Gamma]f;
(*Print[\[Gamma]f];*)
(*\[Gamma]f=Normal[\[Gamma]f];*)
Return[\[Gamma]f]]


(* ::Subsection:: *)
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



(* ::Section::Closed:: *)
(*\[Section] 1-Loop after MY simplification*)


(* ::Subsection::Closed:: *)
(*\[Section]\[Section] \[Beta] function*)


(* ::Subsubsection:: *)
(*\[Section]\[Section]\[Section] Splitting contributions: g, emitter Subscript[\[Gamma], 1] and absorber "Subscript[\[Gamma], 2]"*)


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
(*\[Beta]Function[g,"print"->False];*)
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


(* ::Item:: *)
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
(*\[Gamma]Function[\[CapitalGamma]1,gg,"print"->False]*)
(*%/.banana ->1/\[Epsilon]/.doubleBanana ->1/\[Epsilon]^2/.hat ->1/(2\[Epsilon]^2)+1/(4\[Epsilon])/.sunset->-1/(8\[Epsilon])//FullSimplify//Factor*)
(**)
(*Normal@%/.g->gc1//FullSimplify;*)
(*df=2+Collect[%,{\[Epsilon],\[Epsilon]^2},FullSimplify]/.\[Epsilon]^n_/;n>2:>0*)


(* ::Subsubsection:: *)
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


(* ::Item:: *)
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


(* ::Subsubsection:: *)
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


(* ::Section:: *)
(*\[Section] 2-Loop after Simplification (partial: just the 1Loop has been done, but I want to see what happens if I update just the 1Loop term in g)*)
(*I CANNOT! IT IS NOT FINITE, I MUST GET THE 2LOOP TO CHECK!*)
(**)
(*IT SEEMS FINITE FOR h->1, l->2, 	WHY??*)


(* ::Subsection:: *)
(*\[Section]\[Section] \[Beta] function*)


(* ::Item:: *)
(*Rewritten to split into Zg, Z\[Gamma]1, Z\[Gamma]2*)


(* ::Input::Initialization:: *)
(* REFERENCE, DO NOT TOUCH *)
goodGuys=-b^3 (2 doubleBanana + 4 hat +6 hat)-b^2(doubleBanana +2 b hat)*2-b^3(6 hat + doubleBanana);

realNasties=b(b-1)(4 doubleBanana + 8 hat )+b(b-1)2 hat - b(b-1)(2  doubleBanana +4 hat )- b(b-1)( 4 doubleBanana);

betterNasties=b^2(b-1)6 hat+b^2(b-1)(2 doubleBanana +4 hat)+b^2(b-1)(2 doubleBanana)+b^2(b-1)(2 hat);

gammagGuys=b(2 hat + 2 hat) + b^2 doubleBanana +4 b^2 hat + b^2 doubleBanana;


(* ::Input::Initialization:: *)
(* IN WHAT FOLLOWS, I SUB doubleBanana-> MINUS 1/\[Epsilon]^2. SO HERE I NEED TO SUM THE BANANA SQUARED. Actually, the replacement ALREADY implements the partial subtraction of subdivergencies *)

(* GradImmediateIntNotAllowed=0 then it is not allowed. To implement it also for \[Gamma]1 and \[Gamma]2, one should set h,h2->1*)

twoLoopZ\[Gamma]1=1/b (-b^2 doubleBanana-2 b^3 hat+(1/2 b^2 (b-1)(banana)^2(* From \[CapitalGamma]Grad counterterm*))- b^2 (b-1)(a doubleBanana+h hat) (*If not all the \[CapitalGamma]grad can be used*));/.h->-1;

twoLoopZ\[Gamma]2=1/b (-b^2 doubleBanana-2 b^3 hat +(1/2 b^2 (b-1)(banana)^2(* From \[CapitalGamma]Grad counterterm*)) +b^2 (doubleBanana+1/2 (banana)^2(* From \[CapitalGamma]paoloG counterterm*))+2 b (hat +1/2 (banana)^2(* From \[CapitalGamma]paoloG counterterm*))- b^2 (b-1)(a2 doubleBanana+h2 hat) (*If not all the \[CapitalGamma]grad can be used*));/.h2->-1(*(2 b hat-2 b^3 hat)/b*)(*/.hat->(hat+1/4(banana)^2(* From \[CapitalGamma]Grad counterterm*))*)

twoLoopZg=((goodGuys+b^2(doubleBanana +2 b hat)*2(*Moved to Z\[Gamma]1 and Z\[Gamma]2 *)+b^3( doubleBanana)(*Should arise from the 1loops of Z\[Gamma]1*Z\[Gamma]2 *))
+(*realNasties modified*)
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
b^2(b-1)(2 (hat+1/2 (banana)^2(* From \[CapitalGamma]Grad counterterm*)))
+(*gammagGuys modified*)
(gammagGuys -(b(2 hat )+ b^2 doubleBanana (*Should arise from the 1loops of Z\[Gamma]1*Z\[Gamma]2 *))- b^2 doubleBanana (*Moved to Z\[Gamma]2 *))(*b(2 hat )  +4 b^2 hat *))/b; 
(*Here I'm missing the subdiv from the grad vertex. Try to remove them by hand see if the rest is finite*)

twoLoopZ\[Gamma]=(b(b-1))/2 ( sunset + (hat+1/2 (banana)^2(* From \[CapitalGamma]Grad counterterm*)))/b;


(* ::Subitem:: *)
(*Check:*)


(* ::Input:: *)
(*twoLoopZg+twoLoopZ\[Gamma]1+twoLoopZ\[Gamma]2;*)
(*FS[%*b+(- goodGuys- gammagGuys-betterNasties - realNasties)]/.banana->0*)


(* ::Subsubsection:: *)
(*\[Section]\[Section]\[Section] After splitting the contributions: b=1*)


(* ::Item::Closed:: *)
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


(* ::Item::Closed:: *)
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


(* ::Subsubsection:: *)
(*\[Section]\[Section]\[Section] After splitting the contributions: b>1  THE Z HERE COULD ACTUALLY BE Z^-1*)


(* ::Item::Closed:: *)
(*using \[Beta]Function[]*)


(* ::Input:: *)
(*Zg=1+2 g0 \[Mu]^-\[Epsilon] banana+z[g](g0 \[Mu]^-\[Epsilon])^2 twoLoopZg;*)
(*%/.b->1*)
(*Z\[Gamma]1=Simplify/@(1+b g0 \[Mu]^-\[Epsilon] banana +z[\[Gamma]1](g0 \[Mu]^-\[Epsilon])^2(twoLoopZ\[Gamma]1+(b+b^2)(banana)^2(*TO REMOVE THE SUB.DIVS*)));*)
(*%/.b->1*)
(*Z\[Gamma]2=1+(b-1) g0 \[Mu]^-\[Epsilon] banana-z[\[Gamma]2](g0 \[Mu]^-\[Epsilon])^2 twoLoopZ\[Gamma]2;*)
(**)
(*Z\[Gamma]=1+z[\[Gamma]](g0 \[Mu]^-\[Epsilon])^2/b (b(b-1))/2 ( sunset + (hat-1/2 (banana)^2(* From \[CapitalGamma]Grad counterterm*)));*)
(**)
(*Zgt=(Zg Z\[Gamma]1 Z\[Gamma]2)/Z\[Gamma]^2/.z[_]->1;*)
(*Series[Zgt,{g0,0,2}];*)
(*%/.b->1//FS*)
(**)
(*Series[(Zgt/.g0->g0 Zgt)^(-1),{g0,0,2}];*)
(*%/.b->1//FS*)


(* ::Input:: *)
(*(Zgt^(1)/.g0->g0 Zgt^(1))g0 \[Mu]^-\[Epsilon];*)
(*g=Series[%,{g0,0,3}]//Normal*)
(*g=Series[g0 \[Mu]^-\[Epsilon] Zgt^(1),{g0,0,3}]//Normal*)
(*\[Beta]Function[g,"print"->False]*)
(*%/.banana ->1/\[Epsilon]/.doubleBanana ->1/\[Epsilon]^2/.hat ->1/(2\[Epsilon]^2)+1/(4\[Epsilon])/.sunset->-1/(8\[Epsilon])//FullSimplify//Factor*)
(*RGeq2=Simplify[Normal[%]]==0;*)


(* ::Input:: *)
(*(16+11 \[Epsilon]+b (-32+56 b+5 \[Epsilon]))//Expand;*)
(*Collect[%,\[Epsilon]]*)


(* ::Input:: *)
(*SolveAlways[16-24 b-8 b^2-32 b zg-4 b z\[Gamma]+4 b^2 z\[Gamma]-8 b^2 z\[Gamma]1-32 b^3 z\[Gamma]1+40 b z\[Gamma]2-48 b^2 z\[Gamma]2+8 b^3 z\[Gamma]2==0,b]*)


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


(* ::Item:: *)
(*Using \[Beta]FunctionFromZ[] 	WITH CT-IN-CT TERMS SUBTRACTED*)


(* ::Subitem::Closed:: *)
(*b=1 for comparison(without CT-in-CT subtraction)*)


(* ::Input:: *)
(*Z\[Gamma]1=1+ g 1/\[Epsilon]-g^2 (-2/\[Epsilon]^2+1/(2\[Epsilon]));*)
(**)
(*Zg=1+2 g 1/\[Epsilon]-g^2 (-7/\[Epsilon]^2(*5/7*)+5/(2\[Epsilon]));(*a=5/7*)*)
(**)
(**)
(*Zgt=Zg Z\[Gamma]1 /.z[_]->1;*)
(*Series[Zgt,{g,0,2}]//FS//Normal*)


(* ::Subitem:: *)
(*b>1*)


(* ::Input::Initialization:: *)
(*This is Probably necessarely *)
twoLoopZ\[Gamma]1=twoLoopZ\[Gamma]1/.(banana)^2->0(banana)^2/2;
twoLoopZg=twoLoopZg/.(banana)^2->0(banana)^2/2;
twoLoopZ\[Gamma]2=twoLoopZ\[Gamma]2/.(banana)^2->0(banana)^2/2;
twoLoopZ\[Gamma]=twoLoopZ\[Gamma]/.(banana)^2->2(banana)^2/2;


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


(* ::Item:: *)
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


(* ::Item:: *)
(*Closer inspection of the Zinv*)


(* ::Input:: *)
(*Z\[Gamma]1Inv /.z[_]->1//Collect[#,{g,\[Epsilon]},FS]&*)
(*ZgInv/.z[_]->1//Collect[#,{g,\[Epsilon]},FS]&*)
(*Z\[Gamma]2Inv/.z[_]->1//Collect[#,{g,\[Epsilon]},FS]&*)
(*Z\[Gamma]Inv/.z[_]->1//Collect[#,{g,\[Epsilon]},FS]&*)
(**)


(* ::Item:: *)
(*Some RG functions*)


(* ::Subitem:: *)
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


(* ::Subitem:: *)
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


(* ::Subitem:: *)
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


(* ::Item:: *)
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


(* ::Subsection::Closed:: *)
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


(* ::Subsection::Closed:: *)
(*\[Section]\[Section] \[CapitalGamma]1 observable NEW AFTER SIMPLIFICATION AND SPLIT OF CONTRIBUTIONS*)


(* ::Subsubsection:: *)
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


(* ::Item:: *)
(**)


(* ::Input:: *)
(*gc2*)


(* ::Input:: *)
(*\[Gamma]FunctionFromZ[Z\[Gamma]1,Zg*Z\[Gamma]1,0,"print"->True]*)


(* ::Input:: *)
(*Normal[SeriesData[g, 0, {-1, 1}, 1, 3, 1]]/.g->\[Epsilon]/3+(2 \[Epsilon]^2)/9*)
(*Series[%,{\[Epsilon],0,2}]*)


(* ::Section:: *)
(*Plots*)


(* ::Code:: *)
(*dfRG1L:=2-b \[Epsilon]/(2+b)*)
(*dfRG2Lwf:=df*)
(*dfRG2L:=2-b \[Epsilon]/(2+b)-b (\[Epsilon]/(2+b))^2*)
(*dfSLE=1+3/(4(2b+1));*)


(* ::Subsection:: *)
(*\[Section] New (after my simpl)*)


(* ::Input::Initialization:: *)
dfRG1L:=2-b \[Epsilon]/(2+b)
dfRG1Lsimp:=2-(b \[Epsilon])/(1+2 b)

dfRG2Lsimp:=Collect[fractalDim,\[Epsilon],FS]

dfRG2Lwf:=dfWF
dfRG2L:=2-b \[Epsilon]/(2+b)-b (\[Epsilon]/(2+b))^2

(*dfRG2Lsimp:=2-(b \[Epsilon])/(1+2 b)-(b ^2 \[Epsilon]^2)/(1+2 b)^2*)(*BAD*)

dfSLE=1+3/(4(2b+1));


(* ::Input:: *)
(*{dfRG1L,dfRG1Lsimp,dfRG2Lsimp,dfRG2Lwf,dfRG2L,dfRG2Lsimp,dfSLE}/.b->0/.\[Epsilon]->2*)
(*Limit[{dfRG1L,dfRG1Lsimp,dfRG2Lsimp,dfRG2Lwf,dfRG2L,dfRG2Lsimp,dfSLE},b->\[Infinity]]/.\[Epsilon]->2*)


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
(*,PlotRange->{0.5,2},AxesLabel->{b,Subscript[d, f]},AxesOrigin->{0,1},PlotLabel->Row[{"d = 2"}](*PlotLegends->Placed["AllExpressions", {Right,Top}]*)*)
(*]*)


(* ::Input:: *)
(*(* The errors are under hestimated *)*)


(* ::Item:: *)
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
(*plotRG2Lsimp=Plot[#/.\[Epsilon]->2,{b,0,endRange},PlotStyle->RGBColor[0, 1, 1],PlotRange->All,PlotLegends->Placed[{Row[{"NEW 2-Loop: ",TraditionalForm[#]}]},{Right,Top}]]&@dfRG2Lsimp;*)
(**)
(**)
(*(*plotRG2Lwf=Plot[dfRG2Lwf/.\[Epsilon]->2/.a->+3,{b,0,endRange},PlotStyle->,PlotRange->All];*)*)
(*(*plotRG2L=Plot[dfRG2L/.\[Epsilon]->2,{b,0,endRange},PlotStyle->,PlotRange->All];*)*)
(**)
(*Show[{plotSLE*)
(*(*,plotRG1L*)*)
(*,plotRG1Lsimp(**)
(*,plotRG2Lsimp*)*)
(*(*,Simulation2d*)*)
(*,Simulation2dGemini*)
(*(*,plotRG2Lwf*)
(*,plotRG2L*)}*)
(*,PlotRange->{1,2},AxesLabel->{b,Subscript[d, f]},AxesOrigin->{0,1}(*,PlotLabel->Row[{"d = 2"}]*)(*PlotLegends->Placed["AllExpressions", {Right,Top}]*)(*, ImageSize->100*)*)
(*]*)


(* ::Subsubsection:: *)
(*\[Section]\[Section] 3d*)


(* ::Item::Closed:: *)
(*Limits*)


(* ::Subitem::Closed:: *)
(*b->\[Infinity]*)


(* ::Input:: *)
(*Limit[dfRG2Lwf,b->\[Infinity]]*)
(*%/.\[Epsilon]->1*)
(*%/.a->-2*)
(**)
(*Limit[dfRG2L,b->\[Infinity]]*)
(*%/.\[Epsilon]->1*)


(* ::Subitem::Closed:: *)
(*b->0*)


(* ::Input:: *)
(*Limit[dfRG2Lwf,b->0]*)
(*%/.\[Epsilon]->1*)
(*%/.a->-2*)
(**)
(*Limit[dfRG2L,b->0]*)
(*%/.\[Epsilon]->1*)


(* ::Subsubsection:: *)
(*Extra data from my simulations*)


(* ::Input:: *)
(**)
(*dfRG1Lsimp:=2-(b \[Epsilon])/(1+2 b);*)
(*dfSLE=1+3/(4(2b+1));*)


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
(*plotRG2Lsimp=Plot[#/.\[Epsilon]->1,{b,0,endRange},PlotStyle->RGBColor[0, 1, 1],PlotRange->All,PlotLegends->Placed[{Row[{"NEW 2-Loop: ",TraditionalForm[#]}]},{Right,Top}]]&@dfRG2Lsimp;*)
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
(*(*,plotRG2Lsimp*)(*,plotRG2Lwf,plotRG2L*)(*,fitPlot*)*)
(*,Simulation3d*)
(*,Simulation3dGemini(*,Graphics[{Red,Text[Style["Result \nby David Wilson"(* (Gemini-opt1)"*),FontFamily->"Times"],{1,1.45}]}]*)*)
(*},PlotRange->{{0,5},{1.3,2}},AxesLabel->{b,Subscript[d, f]},AxesOrigin->{0,1.3}(*,PlotLabel->Row[{"d = 3"}]*)]*)


(* ::Input:: *)
(*fitFunc/.b->3*)
