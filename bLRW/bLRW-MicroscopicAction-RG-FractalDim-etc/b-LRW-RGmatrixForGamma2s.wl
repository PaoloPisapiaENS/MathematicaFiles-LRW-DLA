(* ::Package:: *)

(* ::Subtitle:: *)
(*This notebook is used to analyze the RG matrix mixing the \[Gamma]2 vertices*)


(* ::Title:: *)
(*Initialization*)


(* ::Input:: *)
(*(*Quit*)*)


(* ::Input::Initialization:: *)
<<PaoloInitialization`
??PaoloInitialization`*


(* ::Input:: *)
(*(*SetOptions[EvaluationNotebook[],CommonDefaultFormatTypes->{"Output"->StandardForm}];*)*)


(* ::Input:: *)
(*"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\MathematicaFiles-LRW-DLA-bLRW\\bLRW\\bLRW-MicroscopicAction-RG-FractalDim-etc\\bLRW-RGfunctions.nb"*)


(* ::Input:: *)
(*(*FrontEndTokenExecute["SelectAll"]*)
(*FrontEndTokenExecute["SelectionCloseAllGroups"]*)*)


(* ::Title:: *)
(*b=1*)


(* ::Input:: *)
(*basis={\[Gamma]0,\[Gamma]p,-\[Gamma]m}*)
(*m=\!\(\**)
(*TagBox[*)
(*RowBox[{"(", "", GridBox[{*)
(*{*)
(*RowBox[{"-", "\[Gamma]0"}], *)
(*RowBox[{"-", *)
(*FractionBox["\[Gamma]0", "2"]}], "0"},*)
(*{*)
(*FractionBox["\[Gamma]p", "2"], "0", "0"},*)
(*{*)
(*FractionBox["\[Gamma]p", "4"], *)
(*FractionBox["\[Gamma]0", "4"], "0"}*)
(*},*)
(*GridBoxAlignment->{"Columns" -> {{Center}}, "Rows" -> {{Baseline}}},*)
(*GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.7]}, Offset[0.27999999999999997`]}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}}], "", ")"}],*)
(*Function[BoxForm`e$, MatrixForm[BoxForm`e$]]]\)(*In the basis (\[NoBreak]\[Gamma]0*)
(*\[Gamma]p*)
(*-\[Gamma]m*)
(**)
(*\[NoBreak])*);*)
(*MatrixForm[%]*)
(**)
(*m . basis*)


(* ::Input:: *)
(*(*%[[1;;2,1;;2]];*)*)
(*DiagonalizableMatrixQ[m]*)
(*({s,j}=JordanDecomposition[m])//Map[MatrixForm,#]&;*)
(**)
(**)
(*MatrixForm/@{j,s}*)
(*m . ((Transpose@s)[[3]])//MatrixForm*)
(*(m-(-1/2) IdentityMatrix[3]) . Transpose[s][[2]];*)
(*s . j . Inverse[s]==m*)


(* ::Input:: *)
(*newBais=Transpose[s] . basis*)


(* ::Input:: *)
(*Transpose[s] . m . Inverse[Transpose[s]]//MatrixForm*)
(*% . newBais*)
(**)



