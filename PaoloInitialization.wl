(* ::Package:: *)

(* ::Input:: *)
(*(* These are the initialization routines for Paolo, inspired by the one for Kay *)*)


(* ::Input:: *)
(*(*Quit*)*)


(* ::Input::Initialization:: *)
BeginPackage["PaoloInitialization`"]


(* ::Chapter::Closed:: *)
(*List of function usages*)


(* ::Input::Initialization:: *)
PaoloInitialization::usage="PaoloInitialization` is a custom initialization package contains all the default settings and functions that Paolo likes";


(* ::Input::Initialization:: *)
FS::usage="FS is short for FullSimplify";
PE::usage="PE is short for PowerExpand";
TF::usage="TF is short for TeXForm";


(* ::Input::Initialization:: *)
$Paolofont::usage="Set the desired font once"; 
$Paolofontsize::usage="Set the desired font size once";


(* ::Input::Initialization:: *)
myNotebookEventActions ::usage="A unique function to set the custom NotebookEventActions (currently: CollapseAll, AutoExportWL)";
CollapseAll::usage="CollapseAll[] collapses all with short cut Ctrl+Alt+A (i.e. Alt Gr+A). It works thank to AutoHotkey sending \[ARing] when these keys are pressed";
AutoExportWL::usage="AutoExportWL[] enables automatic synchronization and export to a companion .wl file every time Ctrl+S is pressed in the current notebook";


(* ::Input::Initialization:: *)
PPrint::usage="PPrint[textReplaceable_List, var, options, textToKeep(optional)] prints formatted (using options) text with variable evaluation. e.g. a=2; PPrint[{a,\"=\"},a] outputs a=2.\n"<>"PPrint[textReplaceable_NotList,var, options, textToKeep(optional)] defaults to PPrint[{textReplaceable,\" = \"},var, options, textToKeep(optional)]\n"<>
"Options: {\"style\"\[Rule]{FontFamily\[Rule]\"Times\",FontSize\[Rule]13}}";


(* ::Chapter::Closed:: *)
(*Print-out message*)


(* ::Input::Initialization:: *)
$Paolofontsize=13;
$Paolofont="Times";


(* ::Input::Initialization:: *)
$PaoloInitializationVersion::usage="$PaoloInitializationVersion is the current version of the DrawGraph package.";
$PaoloInitializationReleaseDate::usage="$PaoloInitializationReleaseDate is the release date of the current version.";$PaoloInitializationReleaseDate="2026-08-18";$PaoloInitializationVersion=Quiet@Check[StringReplace[FileBaseName[$Input],Except["p"<>DigitCharacter]->"."],"1"];


(* ::Input::Initialization:: *)
$PaoloInitializationPrint::usage="$PaoloInitializationPrint=True turns on the debug printing of the package.";
If[!ValueQ[$PaoloInitializationPrint],$PaoloInitializationPrint=True];


(* ::Input::Initialization:: *)
PaoloInitialization`Private`MyPrint=If[$PaoloInitializationPrint,Print[##]]&;


(* ::Input::Initialization:: *)
PaoloInitialization`Private`MyPrint[Style["~~~~~~~~~~~~~~~~ PaoloInitialization v"<>ToString[$PaoloInitializationVersion]<>" ~~~~~~~~~~~~~~~~~~~~\n\
Author: Paolo Pisapia\n\
Release Date: "<>$PaoloInitializationReleaseDate<>"\n\
Timestamp: "<>DateString[FileDate[$InputFileName]]<>"\nRead from: "<>$InputFileName<>"\n\
The package contains all the default settings and functions that Paolo likes. \
It takes inspiration from Kay Wiese's initialization file.\n\nSee ?PaoloInitialization`* for a list of functions. ",{Bold,RGBColor[0, 0, Rational[2, 3]]}]];


(* ::Input::Initialization:: *)
(*Print[Style["###################################\n This is Paolo-initialization running\n###################################",{}]];*)


(* ::Chapter:: *)
(*Implementation*)


(* ::Input::Initialization:: *)
Begin["Private`"]


(* ::Input:: *)
(*(*Quit*)*)


(* ::Section::Closed:: *)
(*Useful abbreviations, font selection, directory set-up...*)


(* ::Input::Initialization:: *)
(* standard abbreviations *)
FS:=FullSimplify
PE:=PowerExpand
TF:=TeXForm


(* ::Input::Initialization:: *)
(* font selection for plotting *)
If[!NumberQ[Kayplotfontsize],Kayplotfontsize=13];
Kayplotfont="Times";
SetOptions[Plot,BaseStyle->{FontFamily->Kayplotfont,FontSize->Kayplotfontsize,AxesStyle->Directive[Black,Kayplotfontsize]}];
(* SetOptions[Show,BaseStyle\[Rule]{FontFamily\[Rule]Kayplotfont,FontSize\[Rule]Kayplotfontsize,AxesStyle\[Rule]Directive[Black,Kayplotfontsize]}];*)
SetOptions[ListPlot,BaseStyle->{FontFamily->Kayplotfont,FontSize->Kayplotfontsize,AxesStyle->Directive[Black,Kayplotfontsize]}];
SetOptions[ListLinePlot,BaseStyle->{FontFamily->Kayplotfont,FontSize->Kayplotfontsize,AxesStyle->Directive[Black,Kayplotfontsize]}];
SetOptions[LogLogPlot,BaseStyle->{FontFamily->Kayplotfont,FontSize->Kayplotfontsize,AxesStyle->Directive[Black,Kayplotfontsize]}];
SetOptions[ListLogLogPlot,BaseStyle->{FontFamily->Kayplotfont,FontSize->Kayplotfontsize,AxesStyle->Directive[Black,Kayplotfontsize]}];
SetOptions[DiscretePlot,BaseStyle->{FontFamily->Kayplotfont,FontSize->Kayplotfontsize,AxesStyle->Directive[Black,Kayplotfontsize]}];
SetOptions[ParametricPlot,BaseStyle->{FontFamily->Kayplotfont,FontSize->Kayplotfontsize,AxesStyle->Directive[Black,Kayplotfontsize]}];
SetOptions[LogPlot ,BaseStyle->{FontFamily->Kayplotfont,FontSize->Kayplotfontsize,AxesStyle->Directive[Black,Kayplotfontsize]}];
SetOptions[ LogLinearPlot,BaseStyle->{FontFamily->Kayplotfont,FontSize->Kayplotfontsize,AxesStyle->Directive[Black,Kayplotfontsize]}];
SetOptions[PolarPlot ,BaseStyle->{FontFamily->Kayplotfont,FontSize->Kayplotfontsize,AxesStyle->Directive[Black,Kayplotfontsize]}];
SetOptions[ Plot3D,BaseStyle->{FontFamily->Kayplotfont,FontSize->Kayplotfontsize,AxesStyle->Directive[Black,Kayplotfontsize]}];
SetOptions[ ListPlot3D,BaseStyle->{FontFamily->Kayplotfont,FontSize->Kayplotfontsize,AxesStyle->Directive[Black,Kayplotfontsize]}];
SetOptions[ ContourPlot,BaseStyle->{FontFamily->Kayplotfont,FontSize->Kayplotfontsize,AxesStyle->Directive[Black,Kayplotfontsize]}];
SetOptions[ Graphics,BaseStyle->{FontFamily->Kayplotfont,FontSize->Kayplotfontsize,AxesStyle->Directive[Black,Kayplotfontsize]}];


(* set directories *)
Module[{notebookdirectory=NotebookDirectory[]},
Print[Style["Directory to read/export is: \""<>notebookdirectory<>"\"",{RGBColor[0, 0, Rational[2, 3]]}]];
SetDirectory[notebookdirectory];
]


(* ::Section:: *)
(*myNotebookEventActions[]: CollapseAll & AutoExportWL *)


(* ::Subsection:: *)
(*CollapseAll[]*)


(* ::Input:: *)
(*(*Clear[CollapseAll]*)*)


(* ::Input::Initialization:: *)
CollapseAll[]:={{"KeyDown","\[ARing]"}:>(FrontEndTokenExecute[EvaluationNotebook[],"SelectAll"];
FrontEndTokenExecute[EvaluationNotebook[],"SelectionCloseAllGroups"];)};


(* ::Subsection:: *)
(*AutoExportWL[]: Save -> Generate *.wl*)


(* ::Input:: *)
(*(*Clear[AutoExportWL]*)*)


(* ::Input::Initialization:: *)
AutoExportWL[]:={{"MenuCommand","Save"}:>(NotebookSave[EvaluationNotebook[]];
With[{nbPath=Quiet@NotebookFileName[EvaluationNotebook[]]},If[StringQ[nbPath],Module[{nbExpr,rawCells,processedCells,wlPath},nbExpr=NotebookGet[EvaluationNotebook[]];
wlPath=StringReplace[nbPath,RegularExpression["\\.nb$"]->".wl"];
(*1. Match genuine leaf cells*)rawCells=Cases[nbExpr,Cell[content_,style_String,opts___?OptionQ]:>{content,style,{opts}},Infinity];
(*2. Convert each cell into text format*)processedCells=Table[With[{content=item[[1]],style=item[[2]],opts=item[[3]]},Switch[style,(*Drop outputs*)"Output"|"Print"|"Message",Nothing,(*Code/Input cells*)"Input"|"Code",Module[{codeText,lines},codeText=UsingFrontEnd@First@FrontEndExecute[FrontEnd`ExportPacket[Cell[If[Head[content]===BoxData,content,BoxData[content]],"Input"],"InputText"]];
If[StringQ[codeText]&&StringTrim[codeText]=!="",lines=StringSplit[StringTrim[codeText],"\n"];
StringRiffle[lines,"\n"],Nothing]],(*Structural headings*)"Title"|"Subtitle"|"Chapter"|"Section"|"Subsection"|"Subsubsection"|"Text"|"Item"|"Subitem",Module[{txt,isClosed,tag},txt=ToString[content/. {TextData->Identity,BoxData->Identity,StyleBox[s_,___]:>s}];
txt=StringTrim[txt];
If[txt=!="",isClosed=MatchQ[Open/. opts,False];
tag="(* ::"<>style<>If[isClosed,"::Closed:: *)",":: *)"];
tag<>"\n(*"<>txt<>"*)",Nothing]],_,Nothing]],{item,rawCells}];
(*3. Export to.wl file*)
Export[wlPath,StringJoin["(* ::Package:: *)\n\n",StringRiffle[DeleteCases[processedCells,Nothing],"\n\n\n"]],"Text"];]]])};


(* ::Subsection:: *)
(*myNotebookEventActions[]*)


(* ::Input:: *)
(*(*getAutoExportWLRule[]:={{"MenuCommand","Save"}:>(With[{nb=SelectedNotebook[]},(*NotebookSave[nb];*)*)
(*With[{nbPath=Quiet@NotebookFileName[nb]},If[!StringQ[nbPath],Print[Style["[AutoExportWL Error] Notebook has no valid path on disk (unsaved/untitled).",Red]];,Module[{t0,nbExpr,rawCells,processedCells,wlPath,byteSize,exportRes},t0=AbsoluteTime[];*)
(*wlPath=StringReplace[nbPath,RegularExpression["\\.nb$"]->".wl"];*)
(*Print[Style["[AutoExportWL] Starting export for: "<>FileNameTake[nbPath],Blue]];*)
(*(*1. Read Notebook Expression*)nbExpr=Check[NotebookGet[nb],$Failed];*)
(*If[nbExpr===$Failed||Head[nbExpr]=!=Notebook,Print[Style["[AutoExportWL Error] Failed to read NotebookGet[].",Red]];*)
(*Return[];];*)
(*byteSize=ByteCount[nbExpr];*)
(*Print[Style[StringTemplate["[AutoExportWL] Notebook in-memory size: `` MB (` ` cells)"][Round[byteSize/(1024.^2),0.01],Length[First[nbExpr]]],Gray]];*)
(*(*2. Extract Cells with a 5-second safety timeout*)rawCells=TimeConstrained[Cases[nbExpr,Cell[content_,style_String,opts___?OptionQ]:>{content,style,{opts}},Infinity],5,(*timeout in seconds*)$TimedOut];*)
(*If[rawCells===$TimedOut,Print[Style["[AutoExportWL Error] Cell extraction TIMED OUT (Notebook structure too deep/large).",Red]];*)
(*Return[];];*)
(*Print[Style["[AutoExportWL] Found "<>ToString[Length[rawCells]]<>" raw cells. Processing...",Gray]];*)
(*(*3. Convert Cells*)processedCells=TimeConstrained[Table[With[{content=item[[1]],style=item[[2]],opts=item[[3]]},Switch[style,"Output"|"Print"|"Message",Nothing,"Input"|"Code",Module[{codeText,lines},codeText=UsingFrontEnd@First@FrontEndExecute[FrontEnd`ExportPacket[Cell[If[Head[content]===BoxData,content,BoxData[content]],"Input"],"InputText"]];*)
(*If[StringQ[codeText]&&StringTrim[codeText]=!="",lines=StringSplit[StringTrim[codeText],"\n"];*)
(*StringRiffle[lines,"\n"],Nothing]],"Title"|"Subtitle"|"Chapter"|"Section"|"Subsection"|"Subsubsection"|"Text"|"Item"|"Subitem",Module[{txt,isClosed,tag},txt=ToString[content/. {TextData->Identity,BoxData->Identity,StyleBox[s_,___]:>s}];*)
(*txt=StringTrim[txt];*)
(*If[txt=!="",isClosed=MatchQ[Open/. opts,False];*)
(*tag="(* ::"<>style<>If[isClosed,"::Closed:: *)",":: *)"];*)
(*tag<>"\n(*"<>txt<>"*)",Nothing]],_,Nothing]],{item,rawCells}],10,(*10-second processing timeout*)$TimedOut];*)
(*If[processedCells===$TimedOut,Print[Style["[AutoExportWL Error] FrontEnd formatting TIMED OUT during cell conversion loop.",Red]];*)
(*Return[];];*)
(*(*4. Export File*)exportRes=Export[wlPath,StringJoin["(* ::Package:: *)\n\n",StringRiffle[DeleteCases[processedCells,Nothing],"\n\n\n"]],"Text"];*)
(*If[exportRes===$Failed,Print[Style["[AutoExportWL Error] File system write failed for: "<>wlPath,Red]];,Print[Style[StringTemplate["[AutoExportWL Success] Exported in `` s -> ``"][Round[AbsoluteTime[]-t0,0.01],FileNameTake[wlPath]],RGBColor[0,0.6,0.2]]];];]]]])*)
(*};*)
(**)
(*ClearAll[getAutoExportWLRule];*)
(**)
(*getAutoExportWLRule[]:={{"MenuCommand","Save"}:>(With[{nb=EvaluationNotebook[]},With[{nbPath=NotebookFileName[nb]},If[StringQ[nbPath],Module[{wlPath},wlPath=StringReplace[nbPath,RegularExpression["(?i)\\.nb$"]->".wl"];*)
(*(*1. Native Save.nb*)NotebookSave[nb];*)
(*(*2. Native Save As.wl using the Front End's built-in exporter*)NotebookSave[nb,wlPath];*)
(*Print[wlPath]]]]])*)
(*};*)*)


(* ::Input::Initialization:: *)
(*Clear[myNotebookEventActions]*)

myNotebookEventActions[]:=SetOptions[EvaluationNotebook[],NotebookEventActions->Join[CollapseAll[],(*AutoExportWL[]*)(*getAutoExportWLRule[],*){PassEventsDown->True}]];

(*Automatically activate on load in the notebook evaluating the package*)
If[$Notebooks,myNotebookEventActions[];
Print[Style["Collapse all with Ctrl+Alt+A, or Alt Gr+A.\n"(*<>
"Auto-export to .wl enabled for this notebook on Ctrl+S."*),RGBColor[0, 0, Rational[2, 3]]]]];


(* ::Section::Closed:: *)
(*PPrint[]*)


(* ::Input::Initialization:: *)
Clear[PPrint];
SetAttributes[PPrint,HoldAll];


Options[PPrint]={"style"->{FontFamily->$Paolofont,FontSize->$Paolofontsize}};

PPrint[textReplaceable_,var_,options:OptionsPattern[]]:=PPrint[textReplaceable,var,options,""];

PPrint[textReplaceable_List,var_,OptionsPattern[],textToKeep_]:=Module[{varName,evalVar,textReplaced,textKept,prevInput},

evalVar=var;

varName=Which[(*If a regular named symbol is passed*)
MatchQ[Unevaluated[var],_Symbol],SymbolName[Unevaluated[var]],

(*If % is passed,extract the assigned variable name from previous input line*)
MatchQ[Unevaluated[var],Unevaluated[%]],((InString[$Line-1]//ToExpression)/.RowBox->List//Flatten)[[1]]
];

textReplaced=Unevaluated[Unevaluated[textReplaceable]/. HoldPattern[var]->varName];

textKept=textToKeep;If[textToKeep==="",evalVar,textToKeep];
Print[Style[Row[Flatten@{textReplaced,evalVar,textKept}],OptionValue["style"]]]
];

PPrint[textReplaceable_,var_,options:OptionsPattern[],textToKeep_:""]:=PPrint[{textReplaceable," = "},var,options,textToKeep];


(*With[{textReplaced=textReplaceable/.var->SymbolName[Unevaluated[var]],textKept=If[textToKeep=="",var,textToKeep]},Print[Row[Flatten@{textReplaced,textKept}]]];*)


(* ::Input:: *)
(*{a,b};*)
(*(InString[$Line-1]//ToExpression)/.RowBox->List*)
(*(Flatten[%]/. List->StringJoin)*)


(* ::Title::Closed:: *)
(*The end*)


(* ::Input::Initialization:: *)
End[]
EndPackage[]
