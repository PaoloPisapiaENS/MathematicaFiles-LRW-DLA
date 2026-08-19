(* ::Package:: *)

(* ::Title:: *)
(*Initialization*)


(*SetOptions[$FrontEndSession,NotebookAutoSave->True]*)
(*With[{nb=EvaluationNotebook[]},RunScheduledTask[If[\
"ModifiedInMemory"/. NotebookInformation[nb],NotebookSave[nb]],300]]
NotebookSave[]*)


<< PaoloInitialization`
?? PaoloInitialization`*


$Paolofontsize = 15
$Paolofont


(*Quit*)


FrontEndTokenExecute["SelectAll"]
FrontEndTokenExecute["SelectionCloseAllGroups"]


(* ::Title:: *)
(*βFunction[] and γFunction[]*)


(* ::Section:: *)
(*βFunction[] Definitions*)


(* ::Subsection:: *)
(*For the RG with effective finite quantities (i.e. renormalization without CTs)*)


(*This is the old version*)
ClearAll[\[Beta]Function];

Options[\[Beta]Function] = {"print" -> False, "g0Order" -> 0};

\[Beta]Function[coupling_, OptionsPattern[]] := 
 Module[{gr, \[Beta]f, nLoop, i},
  Clear[g, g0, \[Mu], \[Epsilon]];
  
  nLoop = OptionValue["g0Order"];
  If[nLoop == 0, nLoop = Exponent[coupling, g0]];
  
  gr = Normal@Series[coupling, {g0, 0, nLoop}];
  
  If[OptionValue["print"],
   Print["Initial effective couling:\n ", gr, "\n"];];
  
  \[Beta]f = -\[Mu] D[gr, \[Mu]] // Expand;
  If[OptionValue["print"],
   Print["\n\[Beta]-function with bare coupling: ", \[Beta]f, 
     "\n"];];
  
  gr = g - coupling + g0 \[Mu]^-\[Epsilon];
  
  If[OptionValue["print"],
   Print[" Bare coupling= \n ", gr, "\n"];];
  
  (* Invert g(g0) *)
  Do[\[Beta]f = \[Beta]f /. 
      g0^n_ \[Mu]^(-n_ \[Epsilon]) :> (gr)^n \[Mu]^(n \[Epsilon]) // 
     Expand;
   \[Beta]f = \[Beta]f /. (g0 ) :> (gr) \[Mu]^ \[Epsilon] // Expand;
   (*\[Beta]f=Series[\[Beta]f,{g0,0,nLoop}]//Expand;*)
   \[Beta]f = \[Beta]f /. g0^n_ /; n > nLoop :> 0;
   \[Beta]f = \[Beta]f /. 
      g0^n_ /; n == nLoop :> (g \[Mu]^\[Epsilon])^n // Expand;
   If[OptionValue["print"],
    Print[\[Beta]f // FullSimplify, "\n"];];
   , {i, 1, nLoop}];
  
  (*For[i=1,i<=nLoop,i++,
  \[Beta]f=\[Beta]f/.g0^n_ \[Mu]^(n_ \[Epsilon]):>(gr)^n//Expand;
  \[Beta]f=\[Beta]f/.(g0 \[Mu]^\[Epsilon]):>(gr)//Expand;
  ];*)
  
  \[Beta]f = 
   Normal[\[Beta]f] /. g0^n_ :> (g \[Mu]^\[Epsilon])^n // Expand;
  \[Beta]f = \[Beta]f /. (g0 ) :> (g \[Mu]^\[Epsilon]) // Expand;
  \[Beta]f = Series[\[Beta]f, {g, 0, nLoop}] // Map[Expand, #] &;
  (*Print[\[Beta]f];*)
  (*\[Beta]f=Normal[\[Beta]f];*)
  Return[\[Beta]f // FullSimplify]]


ClearAll[\[Beta]Function];

Options[\[Beta]Function] = {"print" -> False, "g0Order" -> 0};

\[Beta]Function[coupling_, OptionsPattern[]] := 
 Module[{gr, gB, \[Gamma], \[Beta]f, nLoop, i},
  Clear[g, g0, \[Mu], \[Epsilon]];
  
  nLoop = OptionValue["g0Order"];
  If[nLoop == 0, nLoop = Exponent[coupling, g0]];
  
  gr = Normal@Series[coupling, {g0, 0, nLoop}];
  
  If[OptionValue["print"],
   Print["Initial effective couling:\n ", gr, "\n"];];
  
  \[Beta]f = -\[Mu] D[gr, \[Mu]] // Expand;
  \[Beta]f = Series[\[Beta]f, {g0, 0, nLoop}];
  If[OptionValue["print"],
   Print[" \[Beta]-function with bare coupling:\n\t", \[Beta]f, 
     "\n"];];
  
  (* Invert g(g0) *)
  (*gr=g-coupling+g0 \[Mu]^-\[Epsilon]+O[g0]^nLoop;*)
  gB = (g0 + (g - gr)*\[Mu]^\[Epsilon] // Expand) + 
    O[\[Gamma]]^(nLoop + 1);
  
  gB = (gB /. {g -> g \[Gamma], g0 -> g0 \[Gamma]});
  
  If[OptionValue["print"],
   Print[" Initial bare coupling: \n\t g0(g)=", gB, "\n"];];
  gB = (gB //. g0 -> gB/\[Gamma]) // Expand;
  gB = Normal[gB] /. \[Gamma] -> 1;
  
  If[OptionValue["print"],
   Print[" Bare coupling: \n\t g0(g)=", gB, "\n"];];
  
  (*
  Do[\[Beta]f=\[Beta]f/.g0^n_ \[Mu]^(-n_ \[Epsilon]):>(gr)^n\[Mu]^(
  n \[Epsilon])//Expand;
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
  \[Beta]f = Normal[\[Beta]f] /. (g0 ) :> (gB) // Expand;
  \[Beta]f = Series[\[Beta]f, {g, 0, nLoop}] // Map[Expand, #] &;
  (*Print[\[Beta]f];*)
  (*\[Beta]f=Normal[\[Beta]f];*)
  
  Return[\[Beta]f // FS]
  ]


(* ::Item:: *)
(*Test on inverting g(g0)*)


(*\[Beta]as function of g0*)
\[Epsilon] \[Mu]^-\[Epsilon] g0 - 
  2 ((1 + 2 b) banana \[Epsilon] \[Mu]^(-2 \[Epsilon])) g0^2 + 
  SeriesData[g0, 0, {}, 1, 3, 1] ;
(*g as function of g0*)
g0 \[Mu]^-\[Epsilon] - (a g0^2 \[Mu]^(-2 \[Epsilon])) + 
  b g0^3 \[Mu]^(-3 \[Epsilon]) - c g0^4 \[Mu]^(-4 \[Epsilon]);
(*Inversion to get g0 as a function of g*)
gg0 = (g0 + (g - %)*\[Mu]^\[Epsilon] // Expand) + O[\[Gamma]]^5
gg0 = (gg0 /. {g -> g \[Gamma], g0 -> g0 \[Gamma]})
gg0 = (gg0 //. g0 -> gg0/\[Gamma]) // Expand

Clear[gg0]


(*Check: yep!*)


(Normal[
SeriesData[\[Gamma], 
     0, {g \[Mu]^\[Epsilon], 
      a g^2 \[Mu]^\[Epsilon], (2 a^2 - b) g^3 \[Mu]^\[Epsilon], (
       5 a^3 - 5 a b + c) g^4 \[Mu]^\[Epsilon]}, 1, 5, 
     1]] /. \[Gamma] -> 1) /. 
 g -> g0 \[Mu]^-\[Epsilon] - (a g0^2 \[Mu]^(-2 \[Epsilon])) + 
   b g0^3 \[Mu]^(-3 \[Epsilon]) - c g0^4 \[Mu]^(-4 \[Epsilon])
Series[%, {g0, 0, 6}]


(* ::Text:: *)
(*RowBox[{, RowBox[{TemplateBox[<|boxes -> FormBox[RowBox[{β, , ϵ, SubscriptBox[g, R], FractionBox[1, RowBox[{1, +, SubscriptBox[g, R], SubscriptBox[∂, SubscriptBox[g, R]], log, Z}]]}], TraditionalForm], errors -> {}, input -> \beta = \epsilon g_R \frac{1}{1+g_R \partial_{g_R}\log Z}, state -> Boxes|>, TeXAssistantTemplate],  , with,  , TemplateBox[<|boxes -> FormBox[RowBox[{SubscriptBox[g, B], , SubscriptBox[g, R], Z, SuperscriptBox[μ, ϵ]}], TraditionalForm], errors -> {}, input -> g_B = g_R Z \mu^\epsilon, state -> Boxes|>, TeXAssistantTemplate]}]}]*)


(* ::Subsection:: *)
(*9             9            9             9
{I can write β as  , Cell[RowBox[{, RowBox[{TemplateBox[<|boxes -> FormBox[RowBox[{β, , ϵ, SubscriptBox[g, R], FractionBox[1, RowBox[{1, +, SubscriptBox[g, R], SubscriptBox[∂, SubscriptBox[g, R]], log, Z}]]}], TraditionalForm], errors -> {}, input -> \beta = \epsilon g_R \frac{1}{1+g_R \partial_{g_R}\log Z}, state -> Boxes|>, TeXAssistantTemplate],  , with,  , TemplateBox[<|boxes -> FormBox[RowBox[{SubscriptBox[g, B], , SubscriptBox[g, R], Z, SuperscriptBox[μ, ϵ]}], TraditionalForm], errors -> {}, input -> g_B = g_R Z \mu^\epsilon, state -> Boxes|>, TeXAssistantTemplate]}]}], Text, CellChangeTimes -> {3.99415 10 , {3.99415 10 , 3.99415 10 }, 3.99422 10 }]}*)


(* ::Text:: *)
(*RowBox[{RowBox[{But,  , then,  , I,  , am,  , not,  , sure,  , I,  , can,  , generalize,  , it}], ...}]*)


ClearAll[\[Beta]FunctionFromZ];

Options[\[Beta]FunctionFromZ] = {"print" -> False};

\[Beta]FunctionFromZ[Zg_, LoopOrder_ : 0, gg_ : {g}, 
  OptionsPattern[]] := Module[{z, \[Beta]f, nLoop, i},
  Clear[g, g0, \[Mu], \[Epsilon]];
  
  z = Expand[Zg];
  
  nLoop = LoopOrder;
  If[nLoop == 0, nLoop = Exponent[z, gg[[1]]] + 1];
  
  z = Normal@Series[Zg, Sequence @@ ({#, 0, nLoop} & /@ gg)];
  
  If[OptionValue["print"],
   Print["RG factor:\n ", z, "\n"];];
  
  \[Beta]f = \[Epsilon] gg[[1]] 1/(1 + gg[[1]] D[Log[z], gg[[1]]]);
  
  \[Beta]f = 
   Series[\[Beta]f, Sequence @@ ({#, 0, nLoop} & /@ gg)] // 
    Map[Expand, #] &;
  
  If[OptionValue["print"],
   Print["\n\[Beta]-function: ", \[Beta]f, "\n"];];
  
  Return[Map[Expand, \[Beta]f]]]


\!\(TraditionalForm\`\[Beta] == \[Epsilon] \*
SubscriptBox[
StyleBox["g", "TI"], 
StyleBox["R", "TI"]] + \*
SubscriptBox[
StyleBox["g", "TI"], 
StyleBox["R", "TI"]] \[Mu] 
\*SubscriptBox[\(\[PartialD]\), \(\[Mu]\)]log \*
StyleBox["Z", "TI"]\)  with  \!\(TraditionalForm\`\*
SubscriptBox[
StyleBox["g", "TI"], 
StyleBox["B", "TI"]] == \*
SubscriptBox[
StyleBox["g", "TI"], 
StyleBox["R", "TI"]] \*
StyleBox["Z", "TI"] 
\*SuperscriptBox[\(\[Mu]\), \(\[Epsilon]\)]\)


(* ::Subsection:: *)
(*{I can also write it as (this is a mess to implement with the derivative wrt μ) NOT IMPLEMENTED, Cell[RowBox[{TemplateBox[<|boxes -> FormBox[RowBox[{β, , ϵ, SubscriptBox[g, R], +, SubscriptBox[g, R], μ, SubscriptBox[∂, μ], log, Z}], TraditionalForm], errors -> {}, input -> \beta = \epsilon g_R + g_R \mu \partial_{\mu}\log Z, state -> Boxes|>, TeXAssistantTemplate],  , with,  , TemplateBox[<|boxes -> FormBox[RowBox[{SubscriptBox[g, B], , SubscriptBox[g, R], Z, SuperscriptBox[μ, ϵ]}], TraditionalForm], errors -> {}, input -> g_B = g_R Z \mu^\epsilon, state -> Boxes|>, TeXAssistantTemplate]}], Input],  }*)


ClearAll[\[Beta]FunctionFromZ2];

Options[\[Beta]FunctionFromZ2] = {"print" -> False};

\[Beta]FunctionFromZ2[Zg_, LoopOrder_ : 0, OptionsPattern[]] := 
 Module[{z, \[Beta]f, nLoop, i},
  Clear[g, g0, \[Mu], \[Epsilon]];
  
  z = Expand[Zg];
  
  nLoop = LoopOrder;
  If[nLoop == 0, nLoop = Exponent[z, g] + 1];
  
  z = Normal@Series[Zg, {g, 0, nLoop}];
  
  If[OptionValue["print"],
   Print["RG factor:\n ", z, "\n"];];
  
  \[Beta]f = \[Epsilon] g 1/(1 + g D[Log[z], g]);
  
  \[Beta]f = Series[\[Beta]f, {g, 0, nLoop}] // Map[Expand, #] &;
  
  If[OptionValue["print"],
   Print["\n\[Beta]-function: ", \[Beta]f, "\n"];];
  
  Return[Map[Expand, \[Beta]f]]]


(* ::Subsection:: *)
(*Tests and Results*)


(* ::Subsection:: *)
(*§ b-LRW 2-loop:*)


(* ::Subsubsection:: *)
(*Using my result*)


g = g0 \[Mu]^-\[Epsilon] - (b + 
      2) banana (g0 \[Mu]^-\[Epsilon])^2 + (g0 \[Mu]^-\[Epsilon])^3 \
(b + 2) ( doubleBanana + 2 (b + 1) hat);
\[Beta]Function[g, "print" -> False]
% /. banana -> 1/\[Epsilon] /. doubleBanana -> 1/\[Epsilon]^2 /. 
  hat -> 1/(2 \[Epsilon]^2) + 1/(4 \[Epsilon]) // FullSimplify
RGeq2 = Normal[%] == 0;


(*Nice, this is finite*)


(* ::Subitem:: *)
(*Let’s check Kay’s ansatz for g (from his email “picture”): OK, IT IS FINITE TOO*)


g = g0 \[Mu]^-\[Epsilon] - (b + 
      2) banana (g0 \[Mu]^-\[Epsilon])^2 + (g0 \[Mu]^-\[Epsilon])^3 ( \
(b^2 + 2) doubleBanana + 4 (2 b + 1) hat );
\[Beta]Function[g, "print" -> False]
% /. banana -> 1/\[Epsilon] /. doubleBanana -> 1/\[Epsilon]^2 /. 
  hat -> 1/(2 \[Epsilon]^2) + 1/(4 \[Epsilon]) // FullSimplify
RGeq2 = Normal[%] == 0;


(* ::Item:: *)
(*Let’s get the 2-Loop critical g*)


gc1 = \[Epsilon]/(b + 2);
gc2 = gc1 + A \[Epsilon]^2
RGeq2 /. g -> gc2;
Expand[%];
% /. \[Epsilon]^n_ /; n > 3 :> 0;
gc2 = Collect[
  gc2 /. Flatten@Solve[%, A] // 
   FullSimplify, {\[Epsilon], \[Epsilon]^2}, FullSimplify]


(*OK*)


(* ::Subsubsection:: *)
(*Using my result EXTENDED WITH WAVE-FUNCTION RENORMALIZATION*)


g = g0 \[Mu]^-\[Epsilon] - (b + 
      2) banana (g0 \[Mu]^-\[Epsilon])^2 + (g0 \[Mu]^-\[Epsilon])^3 ( \
(b + 2) ( doubleBanana + 2 (b + 1) hat) - a b (b - 1) 1/2 sunset);
\[Beta]Function[g, "print" -> False]
% /. banana -> 1/\[Epsilon] /. doubleBanana -> 1/\[Epsilon]^2 /. 
    hat -> 1/(2 \[Epsilon]^2) + 1/(4 \[Epsilon]) /. 
   sunset -> -1/(8 \[Epsilon]) // FullSimplify // Factor
RGeq2 = Normal[%] == 0;


(*Nice, this is finite*)


(* ::Section:: *)
(*γFunction[] Definitions*)


(* ::Subsection:: *)
(*γFunction[]*)


(*Old code*)


ClearAll[\[Gamma]Function];


Options[\[Gamma]Function] = {"print" -> False, "g0Order" -> 0};


\[Gamma]Function[observable_, bareCoupling_, OptionsPattern[]] := 
 Module[{U, gr, \[Gamma]f, nLoop, i},
  Clear[g, g0, \[Mu], \[Epsilon]];
  
  
  nLoop = OptionValue["g0Order"];
  If[nLoop == 0, nLoop = Exponent[bareCoupling, g0] - 1];
  (*Print[nLoop]*);
  
  gr = Normal@Series[bareCoupling, {g0, 0, nLoop}];
  
  U = Normal@Series[observable, {g0, 0, nLoop}];
  
  \[Gamma]f = -\[Mu] D[Log[U], \[Mu]] // Expand;
  If[OptionValue["print"],
   Print[" \[Gamma]f(\!\(\*SubscriptBox[
StyleBox[\"g\",\nBackground->RGBColor[0.9, 1, 1]], \(0\)]\))= \n ", \
\[Gamma]f];];
  
  
  gr = g - gr + g0 \[Mu]^-\[Epsilon];
  
  If[OptionValue["print"],
   Print[" Bare coupling= \n ", gr];];
  
  Do[\[Gamma]f = \[Gamma]f /. g0^n_ :> (gr)^n \[Mu]^(n \[Epsilon]) // 
     Expand;
   \[Gamma]f = \[Gamma]f /. (g0 ) :> (gr) \[Mu]^\[Epsilon] // Expand;
   \[Gamma]f = \[Gamma]f /. g0^n_ /; n > nLoop :> 0;
   \[Gamma]f = \[Gamma]f /. 
      g0^n_ /; n == nLoop :> (g \[Mu]^\[Epsilon])^n // Expand;
   , {i, 1, nLoop}];
  
  (*For[i=1,i<=nLoop,i++,
  \[Gamma]f=\[Gamma]f/.g0^n_ \[Mu]^(n_ \[Epsilon]):>(gr)^n//Expand;
  \[Gamma]f=\[Gamma]f/.(g0 \[Mu]^\[Epsilon]):>(gr)//Expand;
  ];*)
  
  \[Gamma]f = \[Gamma]f /. g0^n_ :> (g)^n \[Mu]^(n \[Epsilon]) // 
    Expand;
  \[Gamma]f = \[Gamma]f /. (g0 ) :> (g) \[Mu]^\[Epsilon] // Expand;
  (*
  If[OptionValue["print"],
  Print[" \[Gamma]f(g)= \n ",\[Gamma]f];];*)
  
  \[Gamma]f = Series[\[Gamma]f, {g, 0, nLoop}] // Expand;
  \[Gamma]f = Factor@Simplify /@ \[Gamma]f;
  (*Print[\[Gamma]f];*)
  (*\[Gamma]f=Normal[\[Gamma]f];*)
  Return[\[Gamma]f]]


ClearAll[\[Gamma]Function];


Options[\[Gamma]Function] = {"print" -> False, "g0Order" -> 0};


\[Gamma]Function[observable_, bareCoupling_, OptionsPattern[]] := 
 Module[{U, gB, gr, \[Gamma], \[Gamma]f, nLoop, i},
  Clear[g, g0, \[Mu], \[Epsilon]];
  
  
  nLoop = OptionValue["g0Order"];
  If[nLoop == 0, nLoop = Exponent[bareCoupling, g0] - 1];
  (*Print[nLoop]*);
  
  gr = Normal@Series[bareCoupling, {g0, 0, nLoop}];
  
  U = Normal@Series[observable, {g0, 0, nLoop}];
  
  \[Gamma]f = -\[Mu] D[Log[U], \[Mu]] // Expand;
  If[OptionValue["print"],
   Print[" \[Gamma]f(\!\(\*SubscriptBox[
StyleBox[\"g\",\nBackground->RGBColor[0.9, 1, 1]], \(0\)]\))=-\[Mu] \
D[Log[U],\[Mu]]= ", \[Gamma]f];];
  
  \[Gamma]f = Series[\[Gamma]f, {g0, 0, nLoop}];
  If[OptionValue["print"],
   Print["\t\t= ", \[Gamma]f];];
  
  (*Invert g(g0)*)
  
  (*gr=g-gr+g0 \[Mu]^-\[Epsilon];
  
  If[OptionValue["print"],
  Print[" Bare coupling= \n ",gr];];*)
  
  gB = (g0 + (g - gr)*\[Mu]^\[Epsilon] // Expand) + 
    O[\[Gamma]]^(nLoop + 1);
  
  gB = (gB /. {g -> g  \[Gamma], g0 -> g0  \[Gamma]});
  
  If[OptionValue["print"], 
   Print[" Initial bare coupling: \n\t g0(g)=", gB, "\n"];];
  
  gB = (gB //. g0 -> gB/\[Gamma]) // Expand;
  gB = Normal[gB] /. \[Gamma] -> 1;
  
  If[OptionValue["print"], 
   Print[" Bare coupling: \n\t g0(g)=", gB, "\n"];];
  
  
  \[Gamma]f = Normal[\[Gamma]f] /. (g0 ) :> (gB) // Expand;
  \[Gamma]f = Series[\[Gamma]f, {g, 0, nLoop}] // Map[Expand, #] &;
  
  Return[\[Gamma]f // FS]]


(* ::Subsection:: *)
(*γFunctionFromZ[]*)


Times @@ {1, 2, a^-1, c^2}


\[Gamma]FunctionFromZ[{1}, 1, 0]


(*Don't use the List feature, it is not the correct operation!*)
ClearAll[\[Gamma]FunctionFromZ];

Options[\[Gamma]FunctionFromZ] = {"print" -> False, "gstar" -> True};

\[Gamma]FunctionFromZ[Zobservable_List, ZCoupling_, 
  options : OptionsPattern[]] :=
 \[Gamma]FunctionFromZ[Zobservable, ZCoupling, options, 0]

\[Gamma]FunctionFromZ[Zobservable_List, ZCoupling_, OptionsPattern[], 
  LoopOrder_ : 0] := 
 Module[{U, gr, \[Gamma]f, \[Beta], factor, eq, gstar, nLoop, i},
  Clear[g, g0, \[Mu], \[Epsilon]];
  
  U = Zobservable;
  factor = Length[U];
  U = Times @@ U;
  
  nLoop = LoopOrder;
  If[nLoop == 0, nLoop = Exponent[U, g]];
  (*Print[nLoop]*);
  
  gr = Normal@Series[ZCoupling, {g, 0, nLoop}];
  
  U = Series[U, {g, 0, nLoop}];
  
  \[Gamma]f = - D[Log[U], g] // Expand;
  If[OptionValue["print"],
   Print[
     Style[" - dLogZ/dg= ", {RGBColor[0, 0, 1], Bold}], \[Gamma]f];];
  
  \[Beta] = \[Beta]FunctionFromZ[gr];
  If[OptionValue["print"],
   Print[Style[" \[Beta]= ", {RGBColor[0, 0, 1], Bold}], \[Beta]];];
  If[OptionValue["gstar"],
   gstar = 
    Select[Flatten@
      SolveValues[Simplify[Normal[\[Beta]]] == 0, g], # =!= 0 &];
   
   If[OptionValue["print"],
    Print[
      Style[" Possible \!\(\*SuperscriptBox[\(g\), \(*\)]\)s= ", \
{RGBColor[0, 0, 1], Bold}], gstar];];
   
   gstar = 
    Series[gstar, {\[Epsilon], 0, nLoop}, Assumptions -> b > 0] // 
     Expand;
   gstar = Select[Normal@gstar, (# /. \[Epsilon] -> 0) == 0 &];
   
   
   If[OptionValue["print"],
    Print[
      Style[" Selected \!\(\*SuperscriptBox[\(g\), \(*\)]\)s= ", \
{RGBColor[0, 0, 1], Bold}], gstar];];
   ];
  
  \[Gamma]f = \[Gamma]f*\[Beta]/factor;
  
  If[OptionValue["print"],
   Print[
     Style[Row[{" \[Gamma]f= - \[Beta]/", factor , 
        "* dLogZ/dg= "}], {RGBColor[0, 0, 1], Bold}], FS[\[Gamma]f]];];
  
  If[OptionValue["gstar"],
   \[Gamma]f = Normal[\[Gamma]f] /. g -> gstar[[1]];
   ];
  
  \[Gamma]f = Series[\[Gamma]f, {\[Epsilon], 0, nLoop}] // Expand;
  
  \[Gamma]f = Factor@(FullSimplify /@ \[Gamma]f);
  (*Print[\[Gamma]f];*)
  (*\[Gamma]f=Normal[\[Gamma]f];*)
  Return[Normal[\[Gamma]f]]
  ]


\[Gamma]FunctionFromZ[Zobservable_, ZCoupling_, 
  options : OptionsPattern[]] :=
 \[Gamma]FunctionFromZ[{Zobservable}, ZCoupling, options, 0]

\[Gamma]FunctionFromZ[Zobservable_, ZCoupling_, 
  options : OptionsPattern[], LoopOrder_] :=
 \[Gamma]FunctionFromZ[{Zobservable}, ZCoupling, options, LoopOrder]


(* ::Title:: *)
(*{§ , 1-Loop after MY simplification}*)


(* ::Subsection:: *)
(*§§ β function*)


(* ::Subsection:: *)
(*FormBox[SubscriptBox[γ, 2], TraditionalForm]*)


(* ::Subsubsection:: *)
(*{§§§ Splitting contributions: , Cell[FormBox[g, TraditionalForm]], , emitter , Cell[FormBox[SubscriptBox[γ, 1], TraditionalForm]],  and absorber “, Cell[FormBox[SubscriptBox[γ, 2], TraditionalForm], Subsection], ”}*)


(* ::Item:: *)
(*with βFunction[] and Kay’s approach*)


Zgt = (Zg Z\[Gamma]1 Z\[Gamma]2)/Z\[Gamma]^2


Zg = 1 + 2 g0 \[Mu]^-\[Epsilon] banana;
Z\[Gamma]1 = 1 + b g0 \[Mu]^-\[Epsilon] banana;
Z\[Gamma]2 = 1 + (b - 1) g0 \[Mu]^-\[Epsilon] banana;
Z\[Gamma] = 1;

Zgt = (Zg Z\[Gamma]1 Z\[Gamma]2)/Z\[Gamma]^2;
Series[%, {g0, 0, 1}]


(Zgt^(-1) /. g0 -> g0 Zgt^(-1)) g0 \[Mu]^-\[Epsilon];
g = Series[%, {g0, 0, 2}] // Normal
(*g=Series[g0 \[Mu]^-\[Epsilon] Zgt^(-1),{g0,0,2}]//Normal*)
\[Beta]Function[g, "print" -> True];
% /. banana -> 1/\[Epsilon] /. doubleBanana -> 1/\[Epsilon]^2 /. 
    hat -> 1/(2 \[Epsilon]^2) + 1/(4 \[Epsilon]) /. 
   sunset -> -1/(8 \[Epsilon]) // FullSimplify // Factor
RGeq2 = Simplify[Normal[%]] == 0;


(* ::Item:: *)
(*Let’s get the 1-Loop critical g**)


Select[Flatten@Solve[RGeq2 /. g^2 -> 0, g], #[[2]] =!= 0 &]


SolveValues[RGeq2 /. g^2 -> 0, g]


gc1 = SolveValues[RGeq2 /. g^2 -> 0, g][[2]]


(* OK *)


(* ::Item:: *)
(*with βFunctionFromZ*)


Zg = 1 + 2 g0 \[Mu]^-\[Epsilon] banana;
Z\[Gamma]1 = 1 + b g0 \[Mu]^-\[Epsilon] banana;
Z\[Gamma]2 = 1 + (b - 1) g0 \[Mu]^-\[Epsilon] banana;
Z\[Gamma] = 1;

Zgt = (Zg Z\[Gamma]1 Z\[Gamma]2)/Z\[Gamma]^2 /. 
   g0 -> g \[Mu]^\[Epsilon];
Simplify /@ (Series[% /. banana -> 1/\[Epsilon], {g, 0, 1}])

\[Beta]FunctionFromZ[Zgt, 2]
% /. banana -> 1/\[Epsilon] /. doubleBanana -> 1/\[Epsilon]^2 /. 
    hat -> 1/(2 \[Epsilon]^2) + 1/(4 \[Epsilon]) /. 
   sunset -> -1/(8 \[Epsilon]) // FullSimplify // Factor


(*It works!!*)


(* ::Subsection:: *)
(*§§ Γ_1 observable*)


(* ::Subsubsection:: *)
(*§§§ As it is*)


gg = g0 \[Mu]^-\[Epsilon] - (2 b + 1) banana (g0 \[Mu]^-\[Epsilon])^2;

\[CapitalGamma]1 = 1 - b g0 \[Mu]^-\[Epsilon] banana ;

\[Gamma]Function[\[CapitalGamma]1, gg, "print" -> True]
% /. banana -> 1/\[Epsilon] /. doubleBanana -> 1/\[Epsilon]^2 /. 
    hat -> 1/(2 \[Epsilon]^2) + 1/(4 \[Epsilon]) /. 
   sunset -> -1/(8 \[Epsilon]) // FullSimplify // Factor

Normal@% /. g -> gc1 // FullSimplify;
df = 2 + 
   Collect[%, {\[Epsilon], \[Epsilon]^2}, 
    FullSimplify] /. \[Epsilon]^n_ /; n > 2 :> 0


(* ::Subsubsection:: *)
(*§§§ After splitting the contributions*)


(* ::Item:: *)
(*with γFunction[]*)


gg = Series[g0 \[Mu]^-\[Epsilon] Zgt^(-1), {g0, 0, 2}] // Normal;

Z\[Gamma]1 = 1 + b g0 \[Mu]^-\[Epsilon] banana;
\[CapitalGamma]1 = 1 - b g0 \[Mu]^-\[Epsilon] banana ;

\[Gamma]Function[Z\[Gamma]1^-1, gg, "print" -> False]
% /. banana -> 1/\[Epsilon] /. doubleBanana -> 1/\[Epsilon]^2 /. 
    hat -> 1/(2 \[Epsilon]^2) + 1/(4 \[Epsilon]) /. 
   sunset -> -1/(8 \[Epsilon]) // FullSimplify // Factor

Normal@% /. g -> gc1 // FullSimplify;
df = 2 + 
   Collect[%, {\[Epsilon], \[Epsilon]^2}, 
    FullSimplify] /. \[Epsilon]^n_ /; n > 2 :> 0


(* ::Item:: *)
(*Test with γFunctionFromZ*)


Zg = 1 + 2 g 1/\[Epsilon];
Z\[Gamma]1 = 1 + b g 1/\[Epsilon];
Z\[Gamma]2 = 1 + (b - 1) g 1/\[Epsilon];

\[Gamma]FunctionFromZ[Z\[Gamma]1, Zg*Z\[Gamma]1*Z\[Gamma]2, 0, 
 "print" -> True]


(*Works!!*)


(* ::Subsection:: *)
(*§§ Γ_2 observable*)


(* ::Subsubsection:: *)
(*§§§ After splitting the contributions*)


gg = Series[g0 \[Mu]^-\[Epsilon] Zgt^(-1), {g0, 0, 2}] // Normal;

Z\[Gamma]2 = 1 + (b - 1) g0 \[Mu]^-\[Epsilon] banana;

\[Gamma]Function[Z\[Gamma]2^-1, gg, "print" -> False]
% /. banana -> 1/\[Epsilon] /. doubleBanana -> 1/\[Epsilon]^2 /. 
    hat -> 1/(2 \[Epsilon]^2) + 1/(4 \[Epsilon]) /. 
   sunset -> -1/(8 \[Epsilon]) // FullSimplify // Factor

Normal@% /. g -> gc1 // FullSimplify;
absorber = 
 Collect[%, {\[Epsilon], \[Epsilon]^2}, 
   FullSimplify] /. \[Epsilon]^n_ /; n > 2 :> 0


(* ::Title:: *)
(*{§ 2-Loop after Simplification (partial: just the 1Loop has been done, but I want to see what happens if I update just the 1Loop term in g), IT SEEMS GOOD FOR                                                                    }
 I CANNOT! IT IS NOT FINITE, I MUST GET THE 2LOOP TO CHECK!                                                                                  {GradImmediateIntNotAllowed :> 0, h -> 1, h2 -> 1, a2 -> 1 - a - 3/b, a -> 0}, 	WHY??*)


replaceDiagrams = {banana -> 1/\[Epsilon], 
  doubleBanana -> 1/\[Epsilon]^2, 
  hat -> 1/(2 \[Epsilon]^2) + 1/(4 \[Epsilon]), 
  sunset -> -1/(8 \[Epsilon])}

hideSubDivs = {bananag -> banana, banana\[Gamma]1 -> banana, 
  banana\[Gamma]2 -> banana, banana\[Gamma]Paolo -> banana, 
  banana\[Gamma]Grad -> banana, doubleBananag -> doubleBanana, 
  doubleBanana\[Gamma]1g -> doubleBanana, 
  doubleBanana\[Gamma]Grad -> doubleBanana, 
  doubleBanana\[Gamma]Paolog -> doubleBanana, 
  doubleBanana\[Gamma]2g -> doubleBanana, hatg -> hat, 
  hat\[Gamma]1 -> hat, hat\[Gamma]2 -> hat, hatg\[Gamma]1 -> hat, 
  hat\[Gamma]1g -> hat, hatg\[Gamma]2\[Gamma]1 -> hat, 
  hat\[Gamma]Paolo -> hat, hat\[Gamma]Grad -> hat, 
  hat\[Gamma]2g -> hat, hatg\[Gamma]2 -> hat, 
  hat\[Gamma]1\[Gamma]2 -> hat, hat\[Gamma]Paolo\[Gamma]1 -> hat, 
  hat\[Gamma]Paolo\[Gamma]2 -> hat}


(* ::Chapter:: *)
(*§§ 2loop b=1*)


(* ::Section:: *)
(*§§§ β-function After splitting the contributions: b=1*)


(* ::Subsection:: *)
(*{Here I just replace Z_gt by Z_g*Z_γ1, but this should be the wrong way to compute β... However the result is correct , I FINALLY MADE UP MY MIND AND CONVINCED MYSELF THAT THIS IS CORRECT}*)


Zg = 1 + 2 g 1/\[Epsilon] - 
   g^2 (-7/\[Epsilon]^2 5/7 + 5/(2 \[Epsilon]));(*a=5/7*)

Z\[Gamma]1 = 
  1 + g 1/\[Epsilon] - g^2 (-2/\[Epsilon]^2 + 1/(2 \[Epsilon]));

loopOrder = 2;

Zgt = Zg Z\[Gamma]1 /. z[_] -> 1;
Series[Zgt, {g, 0, loopOrder}] // FS // Normal
Series[%, {\[Epsilon], 0, 0}] // FS // Normal;

\[Beta]FunctionFromZ[Series[Zgt, {g, 0, loopOrder}] // FS // Normal, 
 loopOrder + 1]
RGeq2 = Simplify[Normal[%]] == 0;


(* ::Subsection:: *)
(*Using Kay’s approach*)


(* ::Item:: *)
(*No contribution splitting*)


Zg = g0 \[Mu]^-\[Epsilon] - (b + 
       2) banana (g0 \[Mu]^-\[Epsilon])^2 + (g0 \[Mu]^-\[Epsilon])^3 \
( (b + 2) ( doubleBanana + 2 (b + 1) hat)) /. b -> 1;

Series[%, {g0, 0, 3}]


g = Series[Zg, {g0, 0, 3}] // Normal
(*g=Series[g0 \[Mu]^-\[Epsilon] Zgt^(-1),{g0,0,2}]//Normal*)
\[Beta]Function[g, "print" -> tTrue]
% /. banana -> 1/\[Epsilon] /. doubleBanana -> 1/\[Epsilon]^2 /. 
    hat -> 1/(2 \[Epsilon]^2) + 1/(4 \[Epsilon]) /. 
   sunset -> -1/(8 \[Epsilon]) // FullSimplify // Factor
RGeq2 = Simplify[Normal[%]] == 0;


(*Correct!*)


(* ::Item:: *)
(*Splitting the contributions*)


Zg = g0 \[Mu]^-\[Epsilon] - (g0 \[Mu]^-\[Epsilon])^2 2*(bananag) + \
(g0 \[Mu]^-\[Epsilon])^3 ( 
     2 doubleBananag + 4 hatg + 4 hat\[Gamma]1g + 
      2 hatg\[Gamma]1 );(*a=5/7*)

Z\[Gamma]1 = - (g0 \[Mu]^-\[Epsilon]) banana\[Gamma]1 + (g0 \[Mu]^-\
\[Epsilon])^2 ( doubleBanana\[Gamma]1g + hatg\[Gamma]1 + hat\[Gamma]1);

loopOrder = 2;

Zg + g0 \[Mu]^-\[Epsilon] Z\[Gamma]1 ;
Series[%, {g0, 0, loopOrder + 1}]


g = Series[
   Zg + g0 \[Mu]^-\[Epsilon] Z\[Gamma]1 , {g0, 0, loopOrder + 1}] // 
  Normal
(*g=Series[g0 \[Mu]^-\[Epsilon] Zgt^(-1),{g0,0,2}]//Normal*)
\[Beta]Function[g, "print" -> tTrue]

% /. hideSubDivs 
% /. replaceDiagrams // FullSimplify // Factor
RGeq2 = Simplify[Normal[%]] == 0;


(*Correct!*)


(* ::Subsection:: *)
(*Let’s get the 2-Loop critical g**)


gc2 = gc1 + B \[Epsilon]^2 /. b -> 1;
RGeq2 /. g -> gc2;
Series[%, {\[Epsilon], 0, 3}];
Flatten@Solve[Normal[%], B] // FS
gc2 = (gc2 /. %) // FullSimplify;
gc2 = Collect[Expand@gc2, \[Epsilon], Simplify]


(* CORRECT !!!*)


(* ::Section:: *)
(*§§§ γ-functions After splitting the contributions: b=1*)


(* ::Subsection:: *)
(*Γγ1*)


loopOrder = 2;

g = Normal[
          Series[Zg + g0 \[Mu]^-\[Epsilon] Z\[Gamma]1, {g0, 0, 
            loopOrder + 1}]] /. bananag -> banana /. 
        banana\[Gamma]1 -> banana /. doubleBananag -> doubleBanana /. 
      doubleBanana\[Gamma]1g -> doubleBanana /. hatg -> hat /. 
    hat\[Gamma]1 -> hat /. hatg\[Gamma]1 -> hat /. hat\[Gamma]1g -> hat

(1 + Z\[Gamma]1)/(\[CapitalGamma]\[Gamma])^0 /. z[_] -> 1 /. 
         bananag -> banana /. banana\[Gamma]1 -> banana /. 
       doubleBananag -> doubleBanana /. 
      doubleBanana\[Gamma]1g -> doubleBanana /. hatg -> hat /. 
    hat\[Gamma]1 -> hat /. hatg\[Gamma]1 -> hat /. 
  hat\[Gamma]1g -> hat;

\[Gamma]Function[%, g, "print" -> True]
% /. banana -> 1/\[Epsilon] /. doubleBanana -> 1/\[Epsilon]^2 /. 
    hat -> 1/(2 \[Epsilon]^2) + 1/(4 \[Epsilon]) /. 
   sunset -> -1/(8 \[Epsilon]) // FullSimplify // Factor
% /. g -> gc2 + O[\[Epsilon]]^3


(*Correct!*)


(* ::Chapter:: *)
(*§§ 2loop b>1*)


(* ::Section:: *)
(*Rewritten to split into Zg, Zγ1, Zγ2*)


(* REFERENCE, DO NOT TOUCH *)
goodGuys = -b^3 (2 doubleBanana + 4 hat + 4 hat + 2 hat) - 
   b^2 (doubleBanana + 2 b hat)*2 - b^3 (6 hat + doubleBanana);

realNasties = 
  b (b - 1) (4 doubleBanana + 8 hat ) + b (b - 1) 2 hat - 
   b (b - 1) (2  doubleBanana + 4 hat ) - b (b - 1) ( 4 doubleBanana);

betterNasties = 
  b^2 (b - 1) 6 hat + b^2 (b - 1) (2 doubleBanana + 4 hat) + 
   b^2 (b - 1) (2 doubleBanana) + b^2 (b - 1) (2 hat);

gammagGuys = 
  b (2 hat + 2 hat) + b^2 doubleBanana + 4 b^2 hat + b^2 doubleBanana;


ClearAll[h, GradImmediateIntNotAllowed]


GradImmediateIntNotAllowed /: (GradImmediateIntNotAllowed -> 
   0) := {GradImmediateIntNotAllowed :> 0, h -> 1, h2 -> 1}


(* IN WHAT FOLLOWS, I SUB doubleBanana-> MINUS 1/\[Epsilon]^2. SO \
HERE I NEED TO SUM THE BANANA SQUARED. Actually, the replacement \
ALREADY implements the partial subtraction of subdivergencies *)

(* GradImmediateIntNotAllowed=0 then it is not allowed. To implement \
it also for \[Gamma]1 and \[Gamma]2, one should set h,h2->1*)
GradImmediateIntNotAllowed /: (GradImmediateIntNotAllowed -> 
   0) := {GradImmediateIntNotAllowed :> 0, h -> 1, h2 -> 1, H -> 0}

twoLoopZ\[Gamma]1 = 
    1/b (-b^2 doubleBanana - 
       2 b^3 hat + (1/2 b^2 (b - 1) (banana)^2(* 
        From \[CapitalGamma]Grad counterterm*)) - 
       b^2 (b - 1) (a doubleBanana + 
          h hat) (*If not all the \[CapitalGamma]grad can be used*)); \
/. h -> -1;

twoLoopZ\[Gamma]2 = 
   1/b (-b^2 doubleBanana - 
      2 b^3 hat + (1/2 b^2 (b - 1) (banana)^2(* 
       From \[CapitalGamma]Grad counterterm*)) + 
      b^2 (doubleBanana + 1/2 (banana)^2(* 
        From \[CapitalGamma]paoloG counterterm*)) + 
      2 b (hat + 1/2 (banana)^2(* 
        From \[CapitalGamma]paoloG counterterm*)) - 
      b^2 (b - 1) (a2 doubleBanana + 
         h2 hat) (*If not all the \[CapitalGamma]grad can be used*)); \
/. h2 -> -1(*(2 b hat-2 b^3 hat)/b*)(*/.hat->(hat+1/4(banana)^2(* \
From \[CapitalGamma]Grad counterterm*))*)

twoLoopZg = (-b^3 (2 doubleBananag + 4 hatg + 4 hat\[Gamma]1g + 
        2 hatg\[Gamma]1 + 6 hat\[Gamma]2)(*-b^3 (doubleBanana+6 hat)-
     b^3 (2 doubleBanana+10 hat)-2 b^2 (doubleBanana+2 b \
hat)(*goodGuys*)+2b^2(doubleBanana +2 b hat)(*Moved to Z\[Gamma]1 and \
Z\[Gamma]2 (thus subtracted here) *)+b^3( 
     doubleBanana)(*Should arise from the 1loops of Z\[Gamma]1*
     Z\[Gamma]2 (thus subtracted here) *)*)
     +(*realNasties modified	.*)
     (* ONLY \[Gamma]Grad 1) in my notes*)
     b (b - 1) (4 (doubleBanana + (banana)^2(* 
          From \[CapitalGamma]Grad counterterm*)) + 
        8 (hat + 1/2 (banana)^2(* 
          From \[CapitalGamma]Grad counterterm*)) )
     +(* ONLY \[Gamma]Grad 3) in my notes*)
     b (b - 1) 2 (hat + 1/2 (banana)^2(* 
       From \[CapitalGamma]Grad counterterm*)) \
GradImmediateIntNotAllowed
     -  (* \[Gamma]Grad + \[Gamma]Plus 1) in my notes *)
     b (b - 
        1) (2  (doubleBanana + (banana)^2(* 
          From \[CapitalGamma]Grad counterterm*)) + 
        4 (hat + 1/2 (banana)^2(* 
          From \[CapitalGamma]Grad counterterm*)) )
     -(* \[Gamma]Grad + \[Gamma]Plus 2) in my notes *)
      b (b - 1) ( 
       4 (doubleBanana + (banana)^2(* 
         From \[CapitalGamma]Grad counterterm*)))
     +(*betterNasties modified*)
     (* ONLY \[Gamma]Grad 2) in my notes*)
     b^2 (b - 1) 6 (hat + 1/2 (banana)^2(* 
       From \[CapitalGamma]Grad counterterm*))
     +(* \[Gamma]Grad + \[Gamma]Minus 1) in my notes *)
     b^2 (b - 
        1) (2 (doubleBanana + (banana)^2(* 
          From \[CapitalGamma]Grad counterterm*)) + 
        4 (hat + 1/2 (banana)^2(* 
          From \[CapitalGamma]Grad counterterm*)))
     +(* \[Gamma]Grad + \[Gamma]Minus 2) in my notes *)
     b^2 (b - 
        1) (2 (doubleBanana + (banana)^2(* 
         From \[CapitalGamma]Grad counterterm*)))
     +(* \[Gamma]Grad + \[Gamma]Minus 2) in my notes (continues) *)
     b^2 (b - 
        1) (2 (hat + 1/2 (banana)^2(* 
         From \[CapitalGamma]Grad counterterm*)))
     +(*gammagGuys modified*)
     (gammagGuys - (b (2 hat ) + 
         b^2 doubleBanana (*Should arise from the 1loops of \
Z\[Gamma]1*Z\[Gamma]2 *)) - 
       b^2 doubleBanana (*Moved to Z\[Gamma]2 *))(*b(2 hat )  +4 b^2 \
hat *))/b; 
(*Here I'm missing the subdiv from the grad vertex. Try to remove \
them by hand see if the rest is finite*)

twoLoopZ\[Gamma] = (b (b - 1))/
   2 ( sunset + (hat + 1/2 (banana)^2(* 
      From \[CapitalGamma]Grad counterterm*))) \
GradImmediateIntNotAllowed/b;


(*This is Probably necessarely *)
twoLoopZ\[Gamma]1 = 
  twoLoopZ\[Gamma]1 /. (banana)^2 -> 0 (banana)^2/2;
twoLoopZg = twoLoopZg /. (banana)^2 -> 0 (banana)^2/2;
twoLoopZ\[Gamma]2 = 
  twoLoopZ\[Gamma]2 /. (banana)^2 -> 0 (banana)^2/2;
twoLoopZ\[Gamma] = twoLoopZ\[Gamma] /. (banana)^2 -> 2 (banana)^2/2;


(* ::Subitem:: *)
(*Check:*)


twoLoopZg + twoLoopZ\[Gamma]1 + twoLoopZ\[Gamma]2;
FS[%*b + (- goodGuys - gammagGuys - betterNasties - realNasties)] /. 
 banana -> 0


(* ::Section:: *)
(*§§§ After splitting the contributions: b>1  THE Z HERE COULD ACTUALLY BE Z^-1*)


(* ::Item:: *)
(*Using βFunctionFromZ[]*)


Zg = 1 + 2 g0 \[Mu]^-\[Epsilon] banana + 
   z[g] (g0 \[Mu]^-\[Epsilon])^2 twoLoopZg;
% /. b -> 1;
Z\[Gamma]1 = 
  Simplify /@ (1 + b g0 \[Mu]^-\[Epsilon] banana + 
     z[\[Gamma]1] (g0 \[Mu]^-\[Epsilon])^2 (twoLoopZ\[Gamma]1(*+(b+
       b^2)(banana)^2*)(*TO REMOVE THE SUB.DIVS*)));
% /. b -> 1;
Z\[Gamma]2 = 
  1 + (b - 1) g0 \[Mu]^-\[Epsilon] banana - 
   z[\[Gamma]2] (g0 \[Mu]^-\[Epsilon])^2 twoLoopZ\[Gamma]2;

Z\[Gamma] = 
  1 + z[\[Gamma]] (g0 \[Mu]^-\[Epsilon])^2/b (b (b - 1))/
    2 ( sunset + (hat - 1/2 (banana)^2(* 
       From \[CapitalGamma]Grad counterterm*)));

Zgt = (Zg Z\[Gamma]1 Z\[Gamma]2)/Z\[Gamma]^2 /. z[_] -> 1;
Series[Zgt, {g0, 0, 2}];
% /. b -> 1 // FS;

Series[(Zgt /. g0 -> g0 Zgt)^(-1), {g0, 0, 2}];
% /. b -> 1 // FS;

Zgt /. g0 -> g \[Mu]^\[Epsilon];
% /. g -> g %;
Series[%, {g, 0, 2}] // Normal


Zgt /. g0 -> g \[Mu]^\[Epsilon];
% /. g -> g %;
Series[%, {g, 0, 2}] // Normal;
\[Beta]FunctionFromZ[%, 3]
% /. banana -> 1/\[Epsilon] /. doubleBanana -> 1/\[Epsilon]^2 /. 
    hat -> 1/(2 \[Epsilon]^2) + 1/(4 \[Epsilon]) /. 
   sunset -> -1/(8 \[Epsilon]) // FullSimplify // Factor


(* ::Subsection:: *)
(*Using βFunctionFromZ[] 	WITH CT-IN-CT TERMS SUBTRACTED*)


(* ::Subsubsection:: *)
(*b=1 for comparison(without CT-in-CT subtraction)*)


Z\[Gamma]1 = 
  1 + g 1/\[Epsilon] - g^2 (-2/\[Epsilon]^2 + 1/(2 \[Epsilon]));

Zg = 1 + 2 g 1/\[Epsilon] - 
   g^2 (-7/\[Epsilon]^2(*5/7*)+ 5/(2 \[Epsilon]));(*a=5/7*)


Zgt = Zg Z\[Gamma]1 /. z[_] -> 1;
Series[Zgt, {g, 0, 2}] // FS // Normal


(* ::Subsubsection:: *)
(*b>1*)


(*This is a guess, to check if \[CapitalGamma]grad is needed *)
twoLoopZ\[Gamma]1 = twoLoopZ\[Gamma]1 /. (b - 1) -> 0;
twoLoopZg = twoLoopZg /. (b - 1) -> 0;
twoLoopZ\[Gamma]2 = twoLoopZ\[Gamma]2 /. (b - 1) -> 0;
twoLoopZ\[Gamma] = twoLoopZ\[Gamma] /. (b - 1) -> 0;


Z\[Gamma]1 = 
  Simplify /@ (1 - b g0 \[Mu]^-\[Epsilon] banana - 
     z["\[Gamma]1"] (g0 \[Mu]^-\[Epsilon])^2 (twoLoopZ\[Gamma]1(*+(b+
       b^2)(banana)^2*)(*TO REMOVE THE SUB.DIVS*)));
% /. g0 -> g \[Mu]^\[Epsilon];
% /. g -> g %^(1);
Series[%^(-1), {g, 0, 2}];
% /. banana -> 1/\[Epsilon] /. doubleBanana -> -1/\[Epsilon]^2 /. 
   hat -> -1/(2 \[Epsilon]^2) + 1/(4 \[Epsilon]) /. 
  sunset -> -1/(8 \[Epsilon]);
% /. b -> 1 /. z[_] -> 1 // Map[Expand, #] &
Z\[Gamma]1Inv = Expand /@ %%;
Print["Actual Z\[Gamma]1 = ", 1 + g 1/\[Epsilon], "-", 
  g^2 (-2/\[Epsilon]^2 + 1/(2 \[Epsilon])) ];


Zg = 1 - 2 g0 \[Mu]^-\[Epsilon] banana - 
   z["g"] (g0 \[Mu]^-\[Epsilon])^2 twoLoopZg;
% /. g0 -> g \[Mu]^\[Epsilon];
% /. g -> g %^(1);
Series[%^(-1), {g, 0, 2}];
% /. banana -> 1/\[Epsilon] /. doubleBanana -> -1/\[Epsilon]^2 /. 
   hat -> -1/(2 \[Epsilon]^2) + 1/(4 \[Epsilon]) /. 
  sunset -> -1/(8 \[Epsilon]);
% /. b -> 1 /. z[_] -> 1 // Map[Expand, #] &
ZgInv = Expand /@ %%;
Print["Actual Zg without subtraction = ", 1 + 2 g 1/\[Epsilon], "-", 
  g^2 (-7/\[Epsilon]^2(*5/7*)+ 5/(2 \[Epsilon])) ];



Z\[Gamma]2 = 
  1 - (b - 1) g0 \[Mu]^-\[Epsilon] banana - 
   z["\[Gamma]2"] (g0 \[Mu]^-\[Epsilon])^2 twoLoopZ\[Gamma]2;
% /. g0 -> g \[Mu]^\[Epsilon];
% /. g -> g %^(1);
Series[%^(-1), {g, 0, 2}];
% /. banana -> 1/\[Epsilon] /. doubleBanana -> -1/\[Epsilon]^2 /. 
   hat -> -1/(2 \[Epsilon]^2) + 1/(4 \[Epsilon]) /. 
  sunset -> -1/(8 \[Epsilon]);
% /. b -> 1 /. z[_] -> 1 // Map[Expand, #] &
Z\[Gamma]2Inv = Expand /@ %%;
Print["Actual Z\[Gamma]2  = ", 1];

Z\[Gamma] = 
  1 - z["\[Gamma]"] (g0 \[Mu]^-\[Epsilon])^2 twoLoopZ\[Gamma];
% /. g0 -> g \[Mu]^\[Epsilon];
% /. g -> g %^(1);
Series[%^(-1), {g, 0, 2}];
% /. banana -> 1/\[Epsilon] /. doubleBanana -> -1/\[Epsilon]^2 /. 
   hat -> -1/(2 \[Epsilon]^2) + 1/(4 \[Epsilon]) /. 
  sunset -> -1/(8 \[Epsilon]);
% /. b -> 1 /. z[_] -> 1 // Map[Expand, #] &
Z\[Gamma]Inv = Expand /@ %%;
Print["Actual Z\[Gamma]  = ", 1];

Zgt = ((Zg Z\[Gamma]1 Z\[Gamma]2)/
    Z\[Gamma]^2(*-(g0 \[Mu]^-\[Epsilon])^2 ((2 b)/\[Epsilon]^2+(
    2 b(b-1))/\[Epsilon]^2)*)) /. z[_] -> 1;
Series[Zgt, {g0, 0, 2}];

Zgt /. g0 -> g \[Mu]^\[Epsilon];
% /. g -> g %^(1);
Series[%^(-1), {g, 0, 2}];
% /. banana -> 1/\[Epsilon] /. doubleBanana -> -1/\[Epsilon]^2 /. 
     hat -> -1/(2 \[Epsilon]^2) + 1/(4 \[Epsilon]) /. 
    sunset -> -1/(8 \[Epsilon]) // FullSimplify // Factor;
% /. b -> 1 // FS;


ZgtInv = 
  FS /@ (((Z\[Gamma]1Inv *ZgInv *Z\[Gamma]2Inv)/Z\[Gamma]Inv^2 /. 
       z[_] -> 1) - 
     g^2 ((2 b)/\[Epsilon]^2(*b*)+ (2 (b - 1))/\[Epsilon]^2(*b*)+ (
        b(**b*) (b - 1))/\[Epsilon]^2)(*The CT-in-CT terms. 
     The extra b in the first two terms comes from splitting \
Subscript[c, g] into contributions from \[Gamma]Grad or not (only not=
     extra b)*)- (g^2 (-1 + 
         b) b (-1/\[Epsilon]^2))(*Terms I left out but that I could \
need to count instead*));
(*ZgtInv=ZgtInv/.g->g ZgtInv^(1);*)

FS /@ (ZgtInv /. b -> 1)
Print["Actual b=1 without CT-CT subtraction: ", 
 1 + (3 g)/\[Epsilon], "+", (g^2 (11 - 3 \[Epsilon]))/\[Epsilon]^2]
Print["Actual b=1 with: ", 1 + (3 g)/\[Epsilon], "+", (
 g^2 (9 - 3 \[Epsilon]))/\[Epsilon]^2]


replaceRule = {a -> 0, h -> 1, h2 -> 1, a2 -> -(1/b), l -> 3/2}
replaceRule = {a2 -> -3/(2 b), a -> -3/(2 b), 
  GradImmediateIntNotAllowed -> 0, h -> 1, h2 -> 1}


Series[ZgtInv, {g, 0, 2}] // Normal
\[Beta]FunctionFromZ[ZgtInv /. {a -> a, h -> 1 h}, 3]
(*%/.banana ->1/\[Epsilon]/.doubleBanana ->1/\[Epsilon]^2/.hat ->1/(2\
\[Epsilon]^2)+1/(4\[Epsilon])/.sunset->-1/(8\[Epsilon])//FullSimplify//\
Factor;*)
% /. replaceRule // FS
% /. b -> 1


(-(1/4) + (9 b)/4 + 4 b^2 + GradImmediateIntNotAllowed - 
    b GradImmediateIntNotAllowed - (b h)/2 + (b^2 h)/2 - (b h2)/2 + (
    b^2 h2)/2 + 6/\[Epsilon] - (8 b)/\[Epsilon] + (
    2 a b)/\[Epsilon] + (2 a2 b)/\[Epsilon] + (2 b^2)/\[Epsilon] - (
    2 a b^2)/\[Epsilon] - (2 a2 b^2)/\[Epsilon] - (
    2 GradImmediateIntNotAllowed)/\[Epsilon] + (
    2 b GradImmediateIntNotAllowed)/\[Epsilon] + (b h)/\[Epsilon] - (
    b^2 h)/\[Epsilon] + (b h2)/\[Epsilon] - (
    b^2 h2)/\[Epsilon]) /. {GradImmediateIntNotAllowed -> 0, h -> 1, 
   h2 -> 1} // FS


(3 + (a + a2) b) /. a2 -> -3/(2 b) /. a -> -3/(2 b) // FS


(* ::Item:: *)
(*Various attempts to make it finite*)


Collect[Expand@(-2 + 4 \[Epsilon] + 
     b (10 + b (-8 + \[Epsilon]) + \[Epsilon])), {\[Epsilon]}, FS];
Collect[Expand@(8 (-8 + b) (-1 + b)), {\[Epsilon]}, FS];
Collect[Expand@(4/\[Epsilon] - (6 b)/\[Epsilon] - (
     4 a b)/\[Epsilon] + (2 b^2)/\[Epsilon] + (4 a b^2)/\[Epsilon] - (
     2 b h)/\[Epsilon] + (2 b^2 h)/\[Epsilon]), {\[Epsilon]}, FS];
Collect[Expand@(-((b h)/2) + (b^2 h)/2 - (b h2)/2 + (b^2 h2)/2 + 
     8/\[Epsilon] - (10 b)/\[Epsilon] + (2 a b)/\[Epsilon] + (
     2 a2 b)/\[Epsilon] + (2 b^2)/\[Epsilon] - (
     2 a b^2)/\[Epsilon] - (2 a2 b^2)/\[Epsilon] + (
     b h)/\[Epsilon] - (b^2 h)/\[Epsilon] + (b h2)/\[Epsilon] - (
     b^2 h2)/\[Epsilon] - (4 l)/\[Epsilon] + (
     4 b l)/\[Epsilon]), {\[Epsilon]}, FS];
Collect[Expand@(8/\[Epsilon] - (18 b)/\[Epsilon] + (2 a b)/\[Epsilon] \
+ (2 a2 b)/\[Epsilon] + (10 b^2)/\[Epsilon] - (2 a b^2)/\[Epsilon] - \
(2 a2 b^2)/\[Epsilon] + (b h)/\[Epsilon] - (b^2 h)/\[Epsilon] + (b \
h2)/\[Epsilon] - (b^2 h2)/\[Epsilon]), {\[Epsilon]}, FS]
Solve[% == 0, l]
% /. a -> 0
% /. h -> 1


(* ::Item:: *)
(*Closer inspection of the Zinv*)


Z\[Gamma]1Inv /. z[_] -> 1 // Collect[#, {g, \[Epsilon]}, FS] &
ZgInv /. z[_] -> 1 // Collect[#, {g, \[Epsilon]}, FS] &
Z\[Gamma]2Inv /. z[_] -> 1 // Collect[#, {g, \[Epsilon]}, FS] &
Z\[Gamma]Inv /. z[_] -> 1 // Collect[#, {g, \[Epsilon]}, FS] &


(* ::Subsubsection:: *)
(*Some RG functions*)


(* ::Subitem:: *)
(*Zγ1Inv*)


replaceRule = {a -> 0, h -> 1, h2 -> 1 , a2 -> -(1/b), l -> 3/2};
replaceRule = {a2 -> -(3/(2 b)), a -> -(3/(2 b)), 
   GradImmediateIntNotAllowed -> 0, h -> 1, h2 -> 1};
Z\[Gamma]1Inv/Z\[Gamma]Inv^0 /. z[_] -> 1 // FS;
obsWithoutZ = \[Gamma]FunctionFromZ[% /. replaceRule, 
  ZgtInv /. replaceRule, "print" -> True, "gstar" -> True]

0
{Z\[Gamma]1Inv*Z\[Gamma]Inv^-1} /. z[_] -> 1 // FS;
obsWithZ = \[Gamma]FunctionFromZ[% /. replaceRule, 
  ZgtInv /. replaceRule, "print" -> True, "gstar" -> True]


(24 + \[Epsilon] + 3 b (-8 + \[Epsilon] - 4 b \[Epsilon])) // 
 Collect[#, \[Epsilon]] &


(* ::Subitem:: *)
(*Zγ2Inv*)


Z\[Gamma]2Inv/Z\[Gamma]Inv^0 /. z[_] -> 1 // FS
\[Gamma]FunctionFromZ[%, 
 ZgtInv /. {a -> 0, h -> 1(*,l\[Rule]1/8 (16-2 b+4 a2 b+2 b h2-
   b \[Epsilon]-b h2 \[Epsilon])*)}, "print" -> True, "gstar" -> tTrue]


(-1 + b) (2 (-2 + \[Epsilon]) + 
    b (2 - 4 a2 - 2 h2 + 2 \[Epsilon] + 
       h2 \[Epsilon])) /. \[Epsilon] -> 0
+(8/\[Epsilon]) - (9 b)/\[Epsilon] + (
 2 a2 b)/\[Epsilon] + b^2/\[Epsilon] - (2 a2 b^2)/\[Epsilon] + (
 b h2)/\[Epsilon] - (b^2 h2)/\[Epsilon] - (4 l)/\[Epsilon] + (
 4 b l)/\[Epsilon]
Solve[{%% == 0, % == 0}, {a2, l}] // FS
% /. h2 -> 1


replaceRule = {a -> 0, h -> 1, h2 -> 1 , a2 -> -(1/b), l -> 3/2};
Z\[Gamma]2Inv/Z\[Gamma]Inv^0 /. z[_] -> 1 // FS
\[Gamma]FunctionFromZ[% /. replaceRule, ZgtInv /. replaceRule, 
 "print" -> True, "gstar" -> True]


(* ::Subitem:: *)
(*ZγInv*)


replaceRule = {a -> 0, h -> 1, h2 -> 1 , a2 -> -(1/b), l -> 3/2};
Z\[Gamma]Inv /. z[_] -> 1 // FS
\[Eta] = \[Gamma]FunctionFromZ[% /. replaceRule, 
  ZgtInv /. replaceRule, "print" -> True, "gstar" -> True]


(* ::Subitem:: *)
(*a,h and l to make them finite*)


\[Beta]FunctionFromZ[ZgtInv /. {a -> a, h -> 1 h, z[_] -> 1}, 3]
\[Gamma]FunctionFromZ[Z\[Gamma]1Inv /. {a -> a, h -> 1 h, z[_] -> 1}, 
 ZgtInv /. {a -> a, h -> 1 h}, "print" -> tTrue, "gstar" -> tTrue]
\[Gamma]FunctionFromZ[Z\[Gamma]2Inv /. {a -> a, h -> 1 h, z[_] -> 1}, 
 ZgtInv /. {a -> a, h -> 1 h}, "print" -> tTrue, "gstar" -> tTrue]


Solve[{8/\[Epsilon] - (10 b)/\[Epsilon] + (4 a b)/\[Epsilon] + (
    2 b^2)/\[Epsilon] - (4 a b^2)/\[Epsilon] + (2 b h)/\[Epsilon] - (
    2 b^2 h)/\[Epsilon] - (4 l)/\[Epsilon] + (4 b l)/\[Epsilon] == 
   0, (-1 + b) b (-1 + 2 a + h) == 
   0, (-1 + b) (2 + b (-1 + 2 a + h)) == 0}, {a, h, l}]
{8/\[Epsilon] - (10 b)/\[Epsilon] + (4 a b)/\[Epsilon] + (
    2 b^2)/\[Epsilon] - (4 a b^2)/\[Epsilon] + (2 b h)/\[Epsilon] - (
    2 b^2 h)/\[Epsilon] - (4 l)/\[Epsilon] + (4 b l)/\[Epsilon] == 
   0, (-1 + b) b (-1 + 2 a + h) == 
   0, (-1 + b) (2 + b (-1 + 2 a + h)) == 0} /. {a -> 0, h -> 1 , 
  l -> 2}


(* ::Subsubsection:: *)
(*df ???*)


fractalDim = Collect[2 + (obsWithZ // FS) - \[Eta], \[Epsilon], FS]
{% /. b -> 0 /. \[Epsilon] -> 2, 
 Limit[%, b -> \[Infinity]] /. \[Epsilon] -> 2}

Collect[2 + (obsWithZ // FS) + \[Eta], \[Epsilon], FS]
{% /. b -> 0 /. \[Epsilon] -> 2, 
 Limit[%, b -> \[Infinity]] /. \[Epsilon] -> 2}

fractalDim = Collect[2 + (obsWithZ // FS), \[Epsilon], FS]
{% /. b -> 0 /. \[Epsilon] -> 2, 
 Limit[%, b -> \[Infinity]] /. \[Epsilon] -> 2}


Collect[2 + (obsWithoutZ // FS), \[Epsilon], FS]
{% /. b -> 0 /. \[Epsilon] -> 2, 
 Limit[%, b -> \[Infinity]] /. \[Epsilon] -> 2}


(* ::Subsection:: *)
(*using βFunction[] and Kay’s approach*)


(* ::Subsubsection:: *)
(*Splitting the contributions*)


(* ::Text:: *)
(*To make sense of this we differentiate the diagrams according to the subdivergences*)


replaceRule = {GradImmediateIntNotAllowed :> 0, h -> 1, h2 -> 1, 
   H -> 0, H2 -> 0, a2 -> 1 - a - 3/b, a -> 0, A2 -> 1 - A + 3/b, 
   A -> 1};


(*Logic change: I write A and H in front of the diagrams we obtain \
from the Grad term. Before, we used a and h to subtract these terms \
from the complete expression.*)
\[CapitalGamma]\[Gamma]1small = (-b banana g0 \[Mu]^-\[Epsilon] + 
    b g0^2 (b doubleBanana\[Gamma]1g - (b - 
          1) doubleBanana\[Gamma]Grad + 
       a Hold[b - 1] doubleBanana\[Gamma]Grad - h hat + 
       b (2 + h) hat) \[Mu]^(-2 \[Epsilon]) z["\[Gamma]1"]);

\[CapitalGamma]\[Gamma]1 = (-b banana\[Gamma]1 g0 \[Mu]^-\[Epsilon] + 
    g0^2 \[Mu]^(-2 \[Epsilon])
      b (b doubleBanana\[Gamma]1g - 
       A Hold[b - 1] doubleBanana\[Gamma]Grad + (b hatg\[Gamma]1 + 
         b hat\[Gamma]1 + b hatg\[Gamma]2\[Gamma]1 - 
         hat\[Gamma]Paolo\[Gamma]1) - 
       H Hold[b - 1] hat\[Gamma]Grad)  z["\[Gamma]1"]);
PPrint[{%, " = "}, %, "\n"]

(*Logic change: I write A2 and H2 in front of the diagrams we obtain \
from the Grad term. Before,we used a2 and h2 to subtract these terms \
from the complete expression.*)
\[CapitalGamma]\[Gamma]2small = - 
     g0 \[Mu]^-\[Epsilon] (b - 1) banana + 
   g0^2 \[Mu]^(-2 \[Epsilon]) (2 b^2 hat - 2 hat + 
      b (b - 1) (a2 doubleBanana + h2 hat)) z["\[Gamma]2"];

\[CapitalGamma]\[Gamma]2 = (-(b banana\[Gamma]2 - 
        banana\[Gamma]Paolo)  g0 \[Mu]^-\[Epsilon] + 
    g0^2 \[Mu]^(-2 \[Epsilon]) (b^2 doubleBanana\[Gamma]2g - 
       b doubleBanana\[Gamma]Paolog - 
       b Hold[b - 1] A2 doubleBanana\[Gamma]Grad + b^2 hatg\[Gamma]2 +
        b^2 hat\[Gamma]1\[Gamma]2 + b^2 hat\[Gamma]2 - 
       b hat\[Gamma]Paolo\[Gamma]2 - 2 hat\[Gamma]Paolo - 
       b Hold[b - 1] H2 hat\[Gamma]Grad) z["\[Gamma]2"]);
PPrint[{%, 
  " = "}, %, {"\n=", \[CapitalGamma]\[Gamma]2 //. replaceRule, "\n"}]
(*\[CapitalGamma]g =g0 \[Mu]^-\[Epsilon](1-(2 b bananag-2 (b-1)banana\
\[Gamma]Grad )g0 \[Mu]^-\[Epsilon]-g0^2\[Mu]^(-2 \[Epsilon]) (-4 \
(-1+b) doubleBanana+2 (-1+b) b doubleBanana+b^2 doubleBanana+2  hat+4 \
b hat+8 (-1+b) b hat+2 (-1+b)  GradImmediateIntNotAllowed hat-(-1+b) \
(2 doubleBanana+4 hat)+(-1+b) b (2 doubleBanana+4 hat)-b^2 \
(doubleBanana+6 hat)+(-1+b)(4 doubleBanana+8 hat)-b^2 (2 \
doubleBanana+10 hat))  z["g"]);*)

\[CapitalGamma]g = (1 - (2 b bananag - 
       2 Hold[b - 1] banana\[Gamma]Grad ) g0 \[Mu]^-\[Epsilon] - 
    g0^2 \[Mu]^(-2 \[Epsilon]) (1/
       b (-4 Hold[(b - 1)] b doubleBanana + 
         2 Hold[(b - 1)] b^2 doubleBanana + 2 b hat + 4 b^2 hat + 
         8 Hold[(b - 1)] b^2 hat + 
         2 Hold[(b - 1)] b GradImmediateIntNotAllowed hat - 
         Hold[(b - 1)] b (2 doubleBanana + 4 hat) + 
         Hold[(b - 1)] b^2 (2 doubleBanana + 4 hat) + 
         Hold[(b - 1)] b (4 doubleBanana + 8 hat) - 
         b^3 (2 doubleBananag + 4 hatg + 2 hatg\[Gamma]1 + 
            4 hat\[Gamma]1g + 2 hatg\[Gamma]2 + 4 hat\[Gamma]2g))));
PPrint[{%, " = "}, %, "\n"]

\[CapitalGamma]\[Gamma] = 
  1 - 1/2 (-1 + b) g0^2 \[Mu]^(-2 \[Epsilon])
     GradImmediateIntNotAllowed (banana^2/2 + hat + sunset) z[
     "\[Gamma]"];
PPrint[{%, " = "}, %, "\n"]


\[CapitalGamma]\[Gamma]2small /. hideSubDivs // FS
\[CapitalGamma]\[Gamma]2 /. hideSubDivs // FS

(%%) - (%) // FS
% //. replaceRule // FS
% // ReleaseHold


loopOrder = 2;

\[CapitalGamma]gtsmall = (g0 \[Mu]^-\[Epsilon] \
\[CapitalGamma]\[Gamma]1small + 
      g0 \[Mu]^-\[Epsilon] \[CapitalGamma]\[Gamma]2small + 
      g0 \[Mu]^-\[Epsilon] \[CapitalGamma]g )/\[CapitalGamma]\[Gamma] \
^2 /. z[_] -> 1(*//.replaceRule*);

\[CapitalGamma]gt = 
  g0 \[Mu]^-\[Epsilon] (\[CapitalGamma]\[Gamma]1 + \[CapitalGamma]\
\[Gamma]2 + \[CapitalGamma]g \
-\[CapitalGamma]\[Gamma]1*\[CapitalGamma]\[Gamma]2)/\[CapitalGamma]\
\[Gamma] ^2 /. z[_] -> 1(*//.replaceRule*);

FS /@ (Series[%, {g0, 0, loopOrder + 1}])
Normal[%] /. hideSubDivs


\[CapitalGamma]gtsmall - \[CapitalGamma]gt;
% /. hideSubDivs // FS
% //. replaceRule // ReleaseHold


replaceRule = {GradImmediateIntNotAllowed :> 0, h -> 1, h2 -> 1, 
   H -> 0, H2 -> 0, a2 -> 1 - a - 3/b, a -> 0, A2 -> A2, A -> A};

g = Collect[(Series[\[CapitalGamma]gt //. replaceRule, {g0, 0, 
       loopOrder + 1}] // Normal), {g0}, FS];
PPrint[%, %]
(*g=Series[g0 \[Mu]^-\[Epsilon] Zgt^(-1),{g0,0,2}]//Normal*)
\[Beta]Function[g(*/.hideSubDivs*)  , 
 "print" -> 
  tTrue](*It's slow if the subDivs are not hidden directly in g, I \
think it's just because it's a long expression*)

% /. hideSubDivs 
% /. replaceDiagrams // FullSimplify // Factor
ReleaseHold[%] // FS
RGeq2 = Simplify[Normal[%]] == 0;


SeriesData[g, 0, {\[Epsilon], -1 - 2 b, b + 5 b^2}, 1, 4, 1]


-2 (3 + \[Epsilon]) + 
  b (10 - 3 \[Epsilon] + b (-4 + 11 \[Epsilon])) - 
  2 Hold[b - 1] (4 + \[Epsilon] + b (-6 + A + A2 + 3 \[Epsilon]) + 
     4 Hold[b - 1]) // Collect[#, \[Epsilon], FS] &


(*HERE I "FORGOT" ABOUT THE DIAGRAM WITH BOTH A BANANA FOR THE \
EMITTER AND FOR THE ABSOBER*)
g \[Epsilon] + 
  g^2 \[Epsilon] (-b (2 bananag + banana\[Gamma]1 + banana\[Gamma]2) +
      banana\[Gamma]Paolo + 2 banana\[Gamma]Grad Hold[b - 1]) - 
  2 g^3 \[Epsilon] (banana\[Gamma]Paolo^2 + 
     b^2 ((2 bananag + banana\[Gamma]1 + banana\[Gamma]2)^2 - 
        2 doubleBananag - doubleBanana\[Gamma]1g - 
        doubleBanana\[Gamma]2g - 4 hatg - 3 hatg\[Gamma]1 - 
        3 hatg\[Gamma]2 - hatg\[Gamma]2\[Gamma]1 - hat\[Gamma]1 - 
        4 hat\[Gamma]1g - hat\[Gamma]1\[Gamma]2 - hat\[Gamma]2 - 
        4 hat\[Gamma]2g) + 2 (hat + hat\[Gamma]Paolo) + 
     b (-2 (2 bananag + banana\[Gamma]1 + 
           banana\[Gamma]2) banana\[Gamma]Paolo + 
        doubleBanana\[Gamma]Paolog + 4 hat + 
        hat\[Gamma]Paolo\[Gamma]1 + hat\[Gamma]Paolo\[Gamma]2) + 
     Hold[b - 1] (4 banana\[Gamma]Grad banana\[Gamma]Paolo - 
        2 doubleBanana + 3 doubleBanana\[Gamma]Grad + 4 hat + 
        b (-4 (2 bananag + banana\[Gamma]1 + 
              banana\[Gamma]2) banana\[Gamma]Grad + 4 doubleBanana + 
           doubleBanana\[Gamma]Grad + 12 hat) + 
        4 banana\[Gamma]Grad^2 Hold[b - 1]));
Collect[%, {\[Epsilon] , g, b}, FS];
PPrint["\[Beta]", %, "style" -> {FontSize -> 17}]


(*Here I added it with \[CapitalGamma]\[Gamma]1*\[CapitalGamma]\
\[Gamma]2 *)
g \[Epsilon] + 
  g^2 \[Epsilon] (-b (2 bananag + banana\[Gamma]1 + banana\[Gamma]2) +
      banana\[Gamma]Paolo + 2 banana\[Gamma]Grad Hold[b - 1]) - 
  2 g^3 \[Epsilon] (banana\[Gamma]Paolo^2 + 
     b^2 ((2 bananag + 
          banana\[Gamma]1)^2 + (4 bananag + 
           3 banana\[Gamma]1) banana\[Gamma]2 + banana\[Gamma]2^2 - 
        2 doubleBananag - doubleBanana\[Gamma]1g - 
        doubleBanana\[Gamma]2g - 4 hatg - 3 hatg\[Gamma]1 - 
        3 hatg\[Gamma]2 - hatg\[Gamma]2\[Gamma]1 - hat\[Gamma]1 - 
        4 hat\[Gamma]1g - hat\[Gamma]1\[Gamma]2 - hat\[Gamma]2 - 
        4 hat\[Gamma]2g) + 2 (hat + hat\[Gamma]Paolo) + 
     b (-((4 bananag + 3 banana\[Gamma]1 + 
             2 banana\[Gamma]2) banana\[Gamma]Paolo) + 
        doubleBanana\[Gamma]Paolog + 4 hat + 
        hat\[Gamma]Paolo\[Gamma]1 + hat\[Gamma]Paolo\[Gamma]2) + 
     Hold[b - 1] (4 banana\[Gamma]Grad banana\[Gamma]Paolo - 
        2 doubleBanana + 4 hat + 
        b (-4 (2 bananag + banana\[Gamma]1 + 
              banana\[Gamma]2) banana\[Gamma]Grad + 
           4 doubleBanana + (A + A2) doubleBanana\[Gamma]Grad + 
           12 hat) + 4 banana\[Gamma]Grad^2 Hold[b - 1]));
Collect[%, {\[Epsilon] , g, b}, FS];
PPrint["\[Beta]", %, "style" -> {FontSize -> 17}]


-(2 bananag + banana\[Gamma]1)^2 - (4 bananag + 
    3 banana\[Gamma]1) banana\[Gamma]2 - banana\[Gamma]2^2
% + (2 bananag + banana\[Gamma]1 + banana\[Gamma]2)^2 // FS


(* ::Subsubsection:: *)
(*g* at 2loop*)


gstar2 = Select[Flatten@SolveValues[RGeq2, g], # =!= 0 &];

gstar2 = 
 Series[gstar2, {\[Epsilon], 0, loopOrder}, Assumptions -> b > 0] // 
  Expand
% /. \[Epsilon] -> 0
gstar2 = Select[Normal@gstar2, (# /. \[Epsilon] -> 0) == 0 &][[1]]


(* ::Subsubsection:: *)
(*{RG functions: Γγ1 & , Cell[FormBox[SubscriptBox[d, f], TraditionalForm]]}*)


loopOrder = 2;

replaceRule = 
  Flatten@{GradImmediateIntNotAllowed -> 0, a2 -> -3/(b) + 1 - a, 
    a -> 0, h -> h, h2 -> h2};
g = Normal[Series[\[CapitalGamma]gt, {g0, 0, loopOrder + 1}]] /. 
   replaceRule;

(1 + \[CapitalGamma]\[Gamma]1)/(\[CapitalGamma]\[Gamma])^0 /. 
  z[_] -> 1;
FS /@ (% /. replaceRule);
PPrint[{\[CapitalGamma]\[Gamma]1, "="}, \[CapitalGamma]\[Gamma]1]

obsWithoutZ = \[Gamma]Function[%%, g, "print" -> tTrue]
(*%/.b->1*)
% /. hideSubDivs
% /. replaceDiagrams // FullSimplify // Factor
% /. g -> gstar2 + O[\[Epsilon]]^3 // FS

df2 = 2 + Normal@%


(*If GradImmediateIntNotAllowed\[RuleDelayed]0, then following is the \
same as above*)
g = Normal[Series[\[CapitalGamma]gt, {g0, 0, loopOrder + 1}]] /. 
   replaceRule;
(1 + \[CapitalGamma]\[Gamma]1)*(\[CapitalGamma]\[Gamma])^-1 /. 
  z[_] -> 1;
FS /@ (% /. replaceRule)

obsWithZ = \[Gamma]Function[%, g, "print" -> tTrue]
(*%/.b->1*)
% /. hideSubDivs
% /. replaceDiagrams // FullSimplify // Factor
% /. g -> gstar2 + O[\[Epsilon]]^3 // FS


(* ::Subsubsection:: *)
(*RG functions: Γγ2		not finite (is this an observable of the theory?)*)


loopOrder = 2;

replaceRule = 
  Flatten@{GradImmediateIntNotAllowed -> 0, a2 -> -3/(b) + 1 - a, 
    a -> 0, h -> h, h2 -> h2};
g = Normal[Series[\[CapitalGamma]gt, {g0, 0, loopOrder + 1}]] //. 
  replaceRule

(1 + \[CapitalGamma]\[Gamma]2)/(\[CapitalGamma]\[Gamma])^0 /. 
  z[_] -> 1;
FS /@ (% //. replaceRule)

\[Gamma]Function[%, g, "print" -> tTrue]
(*%/.b->1*)
% /. banana -> 1/\[Epsilon] /. doubleBanana -> 1/\[Epsilon]^2 /. 
    hat -> 1/(2 \[Epsilon]^2) + 1/(4 \[Epsilon]) /. 
   sunset -> -1/(8 \[Epsilon]) // FullSimplify // Factor
% /. g -> gstar2 + O[\[Epsilon]]^3 // FS;


-8 + 4 b + 2 \[Epsilon] + 3 b \[Epsilon] // Collect[#, \[Epsilon]] &


(* ::Subsubsection:: *)
(*ZγInv		TBD*)


replaceRule = {a -> 0, h -> 1, h2 -> 1 , a2 -> -(1/b), l -> 3/2};
Z\[Gamma]Inv /. z[_] -> 1 // FS
\[Eta] = \[Gamma]FunctionFromZ[% /. replaceRule, 
  ZgtInv /. replaceRule, "print" -> True, "gstar" -> True]


(* ::Chapter:: *)
(*§§ Γ1 observable 		BAD AND OLD*)


\[CapitalGamma]2 = 1 - c b (b - 1) g0 \[Mu]^-\[Epsilon] banana ;

gg = g0 \[Mu]^-\[Epsilon] -(2 b + 
       1) banana (g0 \[Mu]^-\[Epsilon])^2/\[CapitalGamma]2 +(g0 \
\[Mu]^-\[Epsilon])^3 (- goodGuys - gammagGuys - betterNasties - 
       realNasties) /. {c -> (1 + 6 b + 3 b^2)/(b (1 + 2 b))};
gg

\[CapitalGamma]1 = 
  1 - b g0 \[Mu]^-\[Epsilon] banana /\[CapitalGamma]2 + (g0 \[Mu]^-\
\[Epsilon])^2 b ( doubleBanana + 2 b hat );

\[Gamma]Function[\[CapitalGamma]1, gg, "print" -> False]
% /. banana -> 1/\[Epsilon] /. doubleBanana -> 1/\[Epsilon]^2 /. 
    hat -> 1/(2 \[Epsilon]^2) + 1/(4 \[Epsilon]) /. 
   sunset -> -1/(8 \[Epsilon]) // FullSimplify // Factor

Normal@% /. g -> gc2 // FullSimplify;
df = 2 + Normal[Series[%, {\[Epsilon], 0, 2}]]


Normal[Series[
   b (1 + b (-1 - 2 (-1 + b) c + \[Epsilon])), {\[Epsilon], 0, 
    0}]] // FS
Flatten@Solve[% == 0, c] // FS


Normal[Series[df /. {c -> -(1/(2 b))}, {\[Epsilon], 0, 2}]]


(* ::Chapter:: *)
(*§§ Γ1 observable NEW AFTER SIMPLIFICATION AND SPLIT OF CONTRIBUTIONS*)


Zg = 1 + 2 g 1/\[Epsilon] - 
   g^2 (-7/\[Epsilon]^2 5/7 + 5/(2 \[Epsilon]));


(* ::Subsubsection:: *)
(*9            9             9            9
{§§§ b=1      , Cell[RowBox[{RowBox[{Zg, =, RowBox[{1, +, RowBox[{2,  , g,  , FractionBox[1, ϵ]}], -, RowBox[{RowBox[{g, ^, 2}],  , RowBox[{(, RowBox[{RowBox[{FractionBox[RowBox[{-, 7}], RowBox[{ϵ, ^, 2}]], FractionBox[5, 7]}], +, FractionBox[5, RowBox[{2, ϵ}]]}], )}]}]}]}], ;}], Input, CellChangeTimes -> {3.99199 10 , 3.99199 10 , {3.99199 10 , 3.99199 10 }}],                                                                        }
 IT WORKS FOR                                                                                                                                                                                                                                                                                                                                                                IT COMES FROM THE FACT THAT THE CT CAN BE PUT ONE INSIDE THE OTHER!!!!*)


Zg = 1 + 2 g 1/\[Epsilon] - 
   g^2 (-7/\[Epsilon]^2 5/7 + 5/(2 \[Epsilon]));(*a=5/7*)

Z\[Gamma]1 = 
  1 + g 1/\[Epsilon] - g^2 (-2/\[Epsilon]^2 + 1/(2 \[Epsilon]));

Zgt = Zg Z\[Gamma]1 /. z[_] -> 1;
Series[Zgt, {g, 0, 2}];
Expand /@ %;
% // Normal
\[Beta]FunctionFromZ[%]


(* ::Item:: *)
(*Small check that it gives the same result and that it is the inverse of the effective coupling*)


Series[Zgt, {g, 0, 2}];
Expand /@ %;
%
(1 - 3 g/\[Epsilon] + 
  3 g^2 (1/\[Epsilon]^2 + 4 (1/(2 \[Epsilon]^2) + 1/(4 \[Epsilon]))))

(% /. g -> g*%^(-1))*%% // FS


(1 - 3 g/\[Epsilon] + 
    3 g^2 (1/\[Epsilon]^2 + 
       4 (1/(2 \[Epsilon]^2) + 1/(4 \[Epsilon]))))^(-1);
Series[% /. g -> g*%, {g, 0, 2}];
Expand /@ %;
% // Normal


\[Beta]Function[
 g0 \[Mu]^(-\[Epsilon])*((1 - 3 g/\[Epsilon] + 
      3 g^2 (1/\[Epsilon]^2 + 
         4 (1/(2 \[Epsilon]^2) + 1/(4 \[Epsilon])))) /. 
    g -> g0 \[Mu]^(-\[Epsilon]))]


gc2


\[Gamma]FunctionFromZ[Z\[Gamma]1, Zg*Z\[Gamma]1, 0, "print" -> True]


Normal[
SeriesData[g, 0, {-1, 1}, 1, 3, 1]] /. 
 g -> \[Epsilon]/3 + (2 \[Epsilon]^2)/9
Series[%, {\[Epsilon], 0, 2}]


(* ::Title:: *)
(*Plots*)


dfRG1L := 2 - b \[Epsilon]/(2 + b)
dfRG2Lwf := df
dfRG2L := 2 - b \[Epsilon]/(2 + b) - b (\[Epsilon]/(2 + b))^2
dfSLE = 1 + 3/(4 (2 b + 1));


(* ::Subsection:: *)
(*§ New (after my simpl)*)


dfRG1L := 2 - b \[Epsilon]/(2 + b)
dfRG1Lsimp := 2 - (b \[Epsilon])/(1 + 2 b)

dfRG2Lsimp := 
 2 - (b \[Epsilon])/(1 + 2 b) - (b (1 + b + 4 b^2) \[Epsilon]^2)/(
  2 (1 + 2 b)^3)


dfRG2Lwf := dfWF
dfRG2L := 2 - b \[Epsilon]/(2 + b) - b (\[Epsilon]/(2 + b))^2

(*dfRG2Lsimp:=2-(b \[Epsilon])/(1+2 b)-(b ^2 \[Epsilon]^2)/(1+2 \
b)^2*)(*BAD*)

dfSLE = 1 + 3/(4 (2 b + 1));


{dfRG1L, dfRG1Lsimp, dfRG2Lsimp, dfRG2Lwf, dfRG2L, dfRG2Lsimp, 
  dfSLE};
PPrint[{#, "->"}, # /. b -> 0 /. \[Epsilon] -> 2] & /@ {dfRG1L, 
   dfRG1Lsimp, dfRG2Lsimp, dfRG2Lwf, dfRG2L, dfRG2Lsimp, dfSLE};

Limit[{dfRG1L, dfRG1Lsimp, dfRG2Lsimp, dfRG2Lwf, dfRG2L, dfRG2Lsimp, 
   dfSLE}, b -> \[Infinity]] /. \[Epsilon] -> 2


(* ::Subsection:: *)
(*§§ 2d*)


dfRG2Lwf


(* ::Item:: *)
(*Limits*)


(* ::Subitem:: *)
(*b->∞*)


Limit[dfRG2Lwf, b -> \[Infinity]]
% /. \[Epsilon] -> 2
% /. a -> -2
Limit[dfRG2L, b -> \[Infinity]]
% /. \[Epsilon] -> 2


(* ::Subitem:: *)
(*b->0*)


Limit[dfRG2Lwf, b -> 0]
% /. \[Epsilon] -> 2
% /. a -> -2
Limit[dfRG2L, b -> 0]
% /. \[Epsilon] -> 2


(* ::Subsubsection:: *)
(*Plots*)


endRange = 5;
dfRG2Lwf := dfWF;


Simulation2d = 
  ListPlot[{{1, Around[
     1.2486744695483691`, 0.023270605268075166`]}, {2, Around[
     1.1151146520584079`, 0.0134148268356009]}, {3, Around[
     1.0768665526174213`, 0.014642777375504247`]}, {4, Around[
     1.0474461998303197`, 0.008312476568391155]}, {5, Around[
     1.0454880607320536`, 0.0064093876238445445`]}}, 
   PlotStyle -> {RGBColor[0, 1, 0], PointSize[0.005]}, 
   PlotLegends -> Placed[{"Simulation Data (old)"}, {Right, Top}]];


Simulation2dGemini = 
  ListPlot[{{0, Around[1.7534581201029278`, 0.0060679884624822]}, {1, 
     Around[1.274522584835579, 0.008333817846449225]}, {2, Around[
     1.1658669951861733`, 0.001939947142635663]}, {3, Around[
     1.1073336136072602`, 0.002384187792543366]}, {4, Around[
     1.0737383484918805`, 0.0017665587042004246`]}, {5, Around[
     1.0670481729478147`, 0.001216345239817617]}(*,{10,
    1.0251\[PlusMinus]0.0012}*)}, 
   PlotStyle -> {RGBColor[0, 0.66, 0], PointSize[0.1]}, 
   PlotMarkers -> X, 
   PlotLegends -> 
    Placed[{"Simulation Data (Gemini-opt1)"}, {Right, Top}]];

plotSLE = 
  Plot[dfSLE, {b, 0, endRange}, PlotStyle -> RGBColor[1, 0, 0], 
   PlotRange -> All, 
   PlotLegends -> 
    Placed[{Row[{"SLE: ", TraditionalForm[#]}] &@dfSLE}, {Right, 
      Top}]];

plotRG1L = 
  Plot[# /. \[Epsilon] -> 2, {b, 0, endRange}, 
     PlotStyle -> RGBColor[0.64, 0, 1], PlotRange -> All, 
     PlotLegends -> 
      Placed[{Row[{"OLD 1-Loop: ", TraditionalForm[#]}]}, {Right, 
        Top}]] &@dfRG1L;

plotRG1Lsimp = 
  Plot[# /. \[Epsilon] -> 2, {b, 0, endRange}, 
     PlotStyle -> RGBColor[0, 0, 1], PlotRange -> All, 
     PlotLegends -> 
      Placed[{Row[{"NEW 1-Loop: ", TraditionalForm[#]}]}, {Right, 
        Top}]] &@dfRG1Lsimp;

plotRG2Lsimp = 
  Plot[# /. \[Epsilon] -> 2, {b, 0, endRange}, 
     PlotStyle -> RGBColor[0, 1, 1], PlotRange -> All, 
     PlotLegends -> 
      Placed[{Row[{"NEW 2-Loop: ", TraditionalForm[#]}]}, {Right, 
        Top}]] &@dfRG2Lsimp;


(*plotRG2Lwf=Plot[dfRG2Lwf/.\[Epsilon]->2/.a->+3,{b,0,endRange},\
PlotStyle->,PlotRange->All];*)
(*plotRG2L=Plot[dfRG2L/.\[Epsilon]->2,{b,0,endRange},PlotStyle->,\
PlotRange->All];*)

Show[{plotSLE
  , plotRG1L
  , plotRG1Lsimp
  , plotRG2Lsimp
  , Simulation2d
  , Simulation2dGemini
  (*,plotRG2Lwf
  ,plotRG2L*)}
 , PlotRange -> All, AxesLabel -> {b, Subscript[d, f]}, 
 AxesOrigin -> {0, 1}, 
 PlotLabel -> 
  Row[{"d = 2"}](*PlotLegends->Placed["AllExpressions", {Right,Top}]*)
 ]


(* The errors are under hestimated *)


(* ::Item:: *)
(*Plot for Kay*)


endRange = 5;
dfRG2Lwf := dfWF;


Simulation2d = 
  ListPlot[{{1, Around[
     1.2486744695483691`, 0.023270605268075166`]}, {2, Around[
     1.1151146520584079`, 0.0134148268356009]}, {3, Around[
     1.0768665526174213`, 0.014642777375504247`]}, {4, Around[
     1.0474461998303197`, 0.008312476568391155]}, {5, Around[
     1.0454880607320536`, 0.0064093876238445445`]}}, 
   PlotStyle -> {RGBColor[0, 1, 0], PointSize[0.005]}, 
   PlotLegends -> Placed[{"Simulation Data (old)"}, {Right, Top}]];


Simulation2dGemini = 
  ListPlot[{{0, 
     Around[1.7534581201029278`, 
      0.019063147975801702`](*1.753\[PlusMinus]0.006*)}
    , {1, Around[1.25127, 0.0214579](*1.275\[PlusMinus]0.008*)}
    , {2, Around[1.148, 0.014](*1.1659\[PlusMinus]0.0019*)}
    , {3, Around[1.1072, 0.0120271](*1.1073\[PlusMinus]0.0024*)}
    , {4, Around[1.08667, 0.01](*1.0737\[PlusMinus]0.0018*)}
    , {5, Around[1.06705, 0.006](*1.0670\[PlusMinus]0.0012*)}(*,{10,
    1.0251\[PlusMinus]0.0012}*)}
   , PlotStyle -> {RGBColor[0, 0.66, 0], 
     PointSize[0.005]},(*PlotMarkers->x,*)
   PlotLegends -> Placed[{Style["Simulated Data \!\(\*
StyleBox[\"d\",\nFontSlant->\"Italic\"]\)=2"(* (Gemini-opt1)"*), 
       FontFamily -> "Times"]}, {Right, Top}]];

plotSLE = 
  Plot[dfSLE, {b, 0, endRange}, PlotStyle -> RGBColor[1, 0, 0], 
   PlotRange -> All, 
   PlotLegends -> 
    Placed[{Style[
       Row[{"SLE: \!\(\*SubscriptBox[\(d\), \(f\)]\) = ", 
           TraditionalForm[#]}] &@dfSLE, 
       FontFamily -> "Times"]}, {Right, Top}]];

plotRG1L = 
  Plot[# /. \[Epsilon] -> 2, {b, 0, endRange}, 
     PlotStyle -> RGBColor[0.64, 0, 1], PlotRange -> All, 
     PlotLegends -> 
      Placed[{Row[{"OLD 1-Loop: ", TraditionalForm[#]}]}, {Right, 
        Top}]] &@dfRG1L;

plotRG1Lsimp = 
  Plot[# /. \[Epsilon] -> 2, {b, 0, endRange}, 
     PlotStyle -> RGBColor[0, 0, 1], PlotRange -> All, 
     PlotLegends -> 
      Placed[{Style[
         Row[{"FT@1-Loop: \!\(\*SubscriptBox[\(d\), \(f\)]\) = ", 
           TraditionalForm[#], 
           "\!\(\*SubscriptBox[\(|\), \(\[Epsilon] = 2\)]\)"}], 
         FontFamily -> "Times"]}, {Right, Top}]] &@dfRG1Lsimp;

plotRG2Lsimp = 
  Plot[# /. \[Epsilon] -> 2, {b, 0, endRange}, 
     PlotStyle -> RGBColor[0, 1, 1], PlotRange -> All, 
     PlotLegends -> 
      Placed[{Style[
         Row[{"FT@2-Loop: \!\(\*SubscriptBox[\(d\), \(f\)]\) = ", 
           TraditionalForm[#], 
           "\!\(\*SubscriptBox[\(|\), \(\[Epsilon] = 2\)]\)"}], 
         FontFamily -> "Times"]}, {Right, Top}]] &@dfRG2Lsimp;


(*plotRG2Lwf=Plot[dfRG2Lwf/.\[Epsilon]->2/.a->+3,{b,0,endRange},\
PlotStyle->,PlotRange->All];*)
(*plotRG2L=Plot[dfRG2L/.\[Epsilon]->2,{b,0,endRange},PlotStyle->,\
PlotRange->All];*)

Show[{plotSLE
  (*,plotRG1L*)
  , plotRG1Lsimp
  , plotRG2Lsimp
  (*,Simulation2d*)
  , Simulation2dGemini
  (*,plotRG2Lwf
  ,plotRG2L*)}
 , PlotRange -> {0, 2}, AxesLabel -> {b, Subscript[d, f]}, 
 AxesOrigin -> {0, 
   0}(*,PlotLabel->Row[{"d = \
2"}]*)(*PlotLegends->Placed["AllExpressions", {Right,Top}]*)(*, \
ImageSize->100*)
 ]


(* ::Subsection:: *)
(*§§ 3d*)


dfRG1L := 2 - b \[Epsilon]/(2 + b)
dfRG1Lsimp := 2 - (b \[Epsilon])/(1 + 2 b)

dfRG2Lsimp := 
 2 - (b \[Epsilon])/(1 + 2 b) - (b (1 + b + 4 b^2) \[Epsilon]^2)/(
  2 (1 + 2 b)^3)

dfRG2Lwf := dfWF
dfRG2L := 2 - b \[Epsilon]/(2 + b) - b (\[Epsilon]/(2 + b))^2

(*dfRG2Lsimp:=2-(b \[Epsilon])/(1+2 b)-(b ^2 \[Epsilon]^2)/(1+2 \
b)^2*)(*BAD*)

dfSLE = 1 + 3/(4 (2 b + 1));


{dfRG1L, dfRG1Lsimp, dfRG2Lsimp, dfRG2Lwf, dfRG2L, dfRG2Lsimp, 
  dfSLE};
PPrint[{#, "->"}, # /. b -> 0 /. \[Epsilon] -> 1] & /@ {dfRG1L, 
   dfRG1Lsimp, dfRG2Lsimp, dfRG2Lwf, dfRG2L, dfRG2Lsimp};

Limit[{dfRG1L, dfRG1Lsimp, dfRG2Lsimp, dfRG2Lwf, dfRG2L, dfRG2Lsimp}, 
  b -> \[Infinity]] /. \[Epsilon] -> 1


(* ::Subsubsection:: *)
(*Extra data from my simulations*)


(* It actually seems the corect functional form *)


(* I use a big number instead of \[Infinity] as it cannot handle it*)


model = 2 - d b/(a + 2 b);
nlm = NonlinearModelFit[{{0, 2}, {1, 1.624}(*,{2,
   1.47}*), {100000, 1.5}}, {model}, {a, d}, b]


fitFunc = model /. nlm["BestFitParameters"];


(* OR *)


fitFunc = 
 Fit[{{0, 2}, {1, 1.624}(*,{2,1.47}*), {100000, 1.5}}, {2, b/(
   2 + b), (b/(2 + b))^2}, {b}]


dfRG1Lsimp


Limit[dfRG1Lsimp /. \[Epsilon] -> 1, b -> \[Infinity]]


inRange = 0;
endRange = 5;


Simulation3d = 
  ListPlot[{{1, 1.624}(*{0,2},{1,1.624},{2,Around[1.511,0.039]},{3,
    Around[1.483,0.028]},{4,Around[1.431,0.016]},{5,Around[1.436,
    0.016]}*)(*,{10,}*)}, 
   PlotStyle -> {RGBColor[1, 0, 0], PointSize[0.015]}, 
   PlotLegends -> 
    Placed[{Style["Result by David Wilson"(* (Gemini-opt1)"*), 
       FontFamily -> "Times"]}, {Right, Top}]];

Simulation3dGemini = 
  ListPlot[{{0, Around[2, 0.02]}, {1, Around[1.61133, 0.03]}, {2, 
     Around[1.511, 0.039]}, {3, Around[1.483, 0.028]}, {4, 
     Around[1.431, 0.036]}, {5, 
     Around[1.436, 0.036]}(*,{10,}*)}(*{(*{0,
   1.753\[PlusMinus]0.006},*){2,Around[1.51,0.01]}(*,{3,
   1.1073\[PlusMinus]0.0024},{4,1.0737\[PlusMinus]0.0018},{5,
   1.0670\[PlusMinus]0.0012},{10,1.0251\[PlusMinus]0.0012}*)}*), 
   PlotStyle -> {RGBColor[0, 0.66, 0], PointSize[0.01]}, 
   PlotLegends -> Placed[{Style["Simulated Data \!\(\*
StyleBox[\"d\",\nFontSlant->\"Italic\"]\)=3"(* (Gemini-opt1)"*), 
       FontFamily -> "Times"]}, {Right, Top}]];

plotRG1L = 
  Plot[# /. \[Epsilon] -> 1, {b, inRange, endRange}, 
     PlotStyle -> RGBColor[0.64, 0, 1], PlotRange -> All, 
     PlotLegends -> 
      Placed[{Row[{"OLD 1-Loop: ", TraditionalForm[#]}]}, {Right, 
        Top}]] &@dfRG1L;

plotRG1Lsimp = 
  Plot[# /. \[Epsilon] -> 1, {b, inRange, endRange}, 
     PlotStyle -> RGBColor[0, 0, 1], PlotRange -> All, 
     PlotLegends -> 
      Placed[{Style[
         Row[{"FT@1-Loop: \!\(\*SubscriptBox[\(d\), \(f\)]\) = ", 
           TraditionalForm[#], 
           "\!\(\*SubscriptBox[\(|\), \(\[Epsilon] = 3\)]\)"}], 
         FontFamily -> "Times"]}, {Right, Top}]] &@dfRG1Lsimp;

plotRG2Lsimp = 
  Plot[# /. \[Epsilon] -> 1, {b, 0, endRange}, 
     PlotStyle -> RGBColor[0, 1, 1], PlotRange -> All, 
     PlotLegends -> 
      Placed[{Style[
         Row[{"FT@2-Loop: \!\(\*SubscriptBox[\(d\), \(f\)]\) = ", 
           TraditionalForm[#], 
           "\!\(\*SubscriptBox[\(|\), \(\[Epsilon] = 3\)]\)"}], 
         FontFamily -> "Times"]}, {Right, Top}]] &@dfRG2Lsimp;

(*plotRG2Lwf=Plot[dfRG2Lwf/.\[Epsilon]->1/.a->+3,{b,inRange,endRange},\
PlotStyle->,PlotRange->All];
plotRG2L=Plot[dfRG2L/.\[Epsilon]->1,{b,inRange,endRange},PlotStyle->,\
PlotRange->All];*)


(*fitPlot=Plot[fitFunc,{b,inRange,endRange},PlotStyle->Red,PlotRange->\
All];*)


Show[{(*plotRG1L
  ,*)plotRG1Lsimp
  , plotRG2Lsimp(*,plotRG2Lwf,plotRG2L*)(*,fitPlot*)
  , Simulation3d
  , Simulation3dGemini(*,Graphics[{Red,Text[Style[
  "Result \nby David Wilson"(* (Gemini-opt1)"*),FontFamily->
  "Times"],{1,1.45}]}]*)
  }, PlotRange -> {{0, 5}, {1.3, 2}}, 
 AxesLabel -> {b, Subscript[d, f]}, AxesOrigin -> {0, 1.3}, 
 ImageSize -> Large(*,PlotLabel->Row[{"d = 3"}]*)]


(* ::Text:: *)
(*RowBox[{THIS,  , LOOKS,  , VERY,  , RowBox[{RowBox[{PROMISING, !!}], !}],  , RowBox[{I, '}], M,  , USING,  , THE,  , REPLACEMENT,  , rule,  , RowBox[{{, RowBox[{RowBox[{GradImmediateIntNotAllowed, , 0}], ,, RowBox[{h, , 1}], ,, RowBox[{h2, , 1}], ,, RowBox[{a2, , RowBox[{1, -, a, -, FractionBox[3, b]}]}], ,, RowBox[{a, , 0}], ,, RowBox[{h, , h}], ,, RowBox[{h2, , h2}]}], }}]}]*)


fitFunc /. b -> 3


PaoloInitialization`content