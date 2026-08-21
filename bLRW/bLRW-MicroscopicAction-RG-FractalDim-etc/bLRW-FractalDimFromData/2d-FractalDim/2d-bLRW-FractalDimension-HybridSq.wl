(* ::Package:: *)

(* ::Title::Closed:: *)
(*Initialization*)


(* ::Input:: *)
(*(*Quit*)*)


(* ::Input::Initialization:: *)
(*SetOptions[$FrontEndSession,NotebookAutoSave->True]*)
(*With[{nb=EvaluationNotebook[]},RunScheduledTask[If["ModifiedInMemory"/. NotebookInformation[nb],NotebookSave[nb]],300]]
NotebookSave[]*)


(* ::Input::Initialization:: *)
<<PaoloInitialization`
??PaoloInitialization`*


(* ::Input:: *)
(*"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\MathematicaFiles-LRW-DLA-bLRW\\bLRW\\bLRW-MicroscopicAction-RG-FractalDim-etc\\bLRW-RGfunctions.nb"*)


(* ::Input:: *)
(*(*FrontEndTokenExecute["SelectAll"]*)
(*FrontEndTokenExecute["SelectionCloseAllGroups"]*)*)


(* ::Title::Closed:: *)
(*Data analysis for the Fractal Dimension of the b-LRW on 2d square Lattice. HybridSq.*)
(*Optimization with Gemini for big data sets*)


(* ::Input:: *)
(*Quit[]*)


(* ::Input:: *)
(*FrontEndTokenExecute["SelectAll"]*)
(*FrontEndTokenExecute["SelectionCloseAllGroups"]*)


(* ::Input::Initialization:: *)
SetOptions[EvaluationNotebook[],NotebookEventActions->{"Open":>(Print["Notebook opened at ",DateString[]];
<<"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\MathematicaFiles-LRW-DLA-bLRW\\bLRW\\bLRW-MicroscopicAction-RG-FractalDim-etc\\bLRW-FractalDimFromData\\2d-FractalDim\\2d-bLRW-FractalDimension.m")}]
<<"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\MathematicaFiles-LRW-DLA-bLRW\\bLRW\\bLRW-MicroscopicAction-RG-FractalDim-etc\\bLRW-FractalDimFromData\\2d-FractalDim\\2d-bLRW-FractalDimension.m"
Directory[]


(* ::Input:: *)
(*SetOptions[$FrontEnd,CommonDefaultFormatTypes->{"Output"->StandardForm}];*)
(*SetOptions[$FrontEndSession,CommonDefaultFormatTypes->{"Output"->StandardForm}];*)


(* ::Title:: *)
(*Implementation of Kay's method to extract a variance from noisy data: mean of batches and variance estimate from there, then mean over estimates*)


(* ::Section::Closed:: *)
(*StdDevEstimate[] definition and tests*)


(* ::Input:: *)
(*(*Check if one can localize the progress variables activeTasks etc*)
(*SEE VERSION BELOW WITH MONITOR[]*)*)


(* ::Input:: *)
(*ClearAll[StdDevEstimate];*)
(**)
(*Options[StdDevEstimate]={"print"->False};*)
(**)
(*StdDevEstimate[data_,binSize_,options:OptionsPattern[]]:=StdDevEstimate[data,binSize,options,0];*)
(**)
(*StdDevEstimate[data_,binSize_,OptionsPattern[],repeat_:0]:=*)
(*Module[{size,locBinSize,repeatNumber,binNumber,scramble,means,stdDevs},*)
(**)
(*size=Length[data];*)
(*locBinSize=binSize;*)
(*repeatNumber=repeat;*)
(**)
(*If[locBinSize>size/2,*)
(*Print[Style["### WARNING: binSize too large, defaulting to half the data length ###",RGBColor[1, 0, 0]]];*)
(*locBinSize=Floor[size/2]];*)
(**)
(*binNumber=Floor[size/locBinSize];*)
(**)
(*If[OptionValue["print"],*)
(*Print[Style[" binNumber= ",{RGBColor[0, 0, 1],Bold}],binNumber];];*)
(**)
(*If[repeatNumber==0,*)
(*repeatNumber=Ceiling[size/binNumber];];*)
(**)
(*If[OptionValue["print"],*)
(*Print[Style[" repeatNumber= ",{RGBColor[0, 0, 1],Bold}],repeatNumber];];*)
(**)
(**)
(*(*totalTasks=repeatNumber;*)
(*activeTasks={1};*)
(*completedCount=0;*)
(*startTime=AbsoluteTime[];(*Capture exact start time*)*)
(**)
(*(*Print the live-updating progress panel with a timer*)FormatTime[sec_]:=With[{s=Round[sec]},StringRiffle[IntegerString[#,10,2]&/@{Quotient[s,3600],Mod[Quotient[s,60],60],Mod[s,60]},":"]];*)
(*Print[Panel[Column[{Row[{Style["Task Progress: ",Bold],Dynamic[completedCount]," / ",totalTasks}],Dynamic[ProgressIndicator[completedCount,{0,totalTasks},ImageSize->Large]],(*Dynamic Timer Row*)Dynamic[With[{elapsed=Round[AbsoluteTime[]-startTime]},Row[{Style["Elapsed: ",Gray],FormatTime[elapsed],"   |   ",Style["ETA: ",Gray],If[completedCount>0,With[{remaining=Round[(elapsed/completedCount)*(totalTasks-completedCount)]},FormatTime[remaining]],"Calculating..."]}]]],Row[{Style["Active Worker Queue: ",Gray],Dynamic[Length[activeTasks]]}]},Spacings->1],Background->GrayLevel[0.95],FrameMargins->15]];*)
(**)
(*(*Actual computation*)*)
(*stdDevs=Table[*)
(**)
(*scramble=RandomSample[data];*)
(*scramble=Partition[scramble,UpTo[locBinSize]];*)
(*means=Mean/@scramble;*)
(*completedCount++;*)
(*{N[StandardDeviation[means]/Sqrt[Length[means]]](*,Mean[means]*)}*)
(*,repeatNumber*)
(*];*)*)
(*(*Initialize DynamicModule WITH explicit starting values*)DynamicModule[{totalTasks=repeatNumber,activeTasks={1},completedCount=0,startTime=AbsoluteTime[]},(*Define localized formatting helper*)With[{formatTime=Function[sec,With[{s=Round[sec]},StringRiffle[IntegerString[#,10,2]&/@{Quotient[s,3600],Mod[Quotient[s,60],60],Mod[s,60]},":"]]]},(*Print the progress panel*)Print[Panel[Column[{Row[{Style["Task Progress: ",Bold],Dynamic[completedCount]," / ",totalTasks}],Dynamic[ProgressIndicator[completedCount,{0,totalTasks},ImageSize->Large]],(*Dynamic Timer Row*)Dynamic[With[{elapsed=Round[AbsoluteTime[]-startTime]},Row[{Style["Elapsed: ",Gray],formatTime[elapsed],"   |   ",Style["ETA: ",Gray],If[completedCount>0,With[{remaining=Round[(elapsed/completedCount)*(totalTasks-completedCount)]},formatTime[remaining]],"00:00:00"]}]]],Row[{Style["Active Worker Queue: ",Gray],Dynamic[Length[activeTasks]]}]},Spacings->1],Background->GrayLevel[0.95],FrameMargins->15]];*)
(**)
(**)
(*(*Actual computation*)*)
(*stdDevs=Table[scramble=RandomSample[data];*)
(*scramble=Partition[scramble,UpTo[locBinSize]];*)
(*means=Mean/@scramble;*)
(*(*Increment counter:updates dynamic display instantly*)completedCount++;*)
(*{N[StandardDeviation[means]/Sqrt[Length[means]]]}*)
(*,repeatNumber];*)
(**)
(*activeTasks={}; (*Clear queue readout when finished*)*)
(*]*)
(*];*)
(**)
(**)
(**)
(*stdDevs={Mean[stdDevs[[All,1]]](*,Mean[stdDevs[[All,2]]]*)};*)
(**)
(*If[OptionValue["print"],*)
(*(*Print[Style[" Mean from batching= ",{,Bold}],stdDevs[[2]]//N];*)*)
(*Print[Style[" Std deviation/\!\(\*SqrtBox[\(N\)]\) estimate= ",{RGBColor[0, 0, 1],Bold}],stdDevs[[1]]//N];];*)
(*stdDevs[[1]]*Sqrt[size]*)
(*]*)


(* ::Input::Initialization:: *)
ClearAll[StdDevEstimate];

Options[StdDevEstimate]={"print"->False};

StdDevEstimate[data_,binSize_,options:OptionsPattern[]]:=StdDevEstimate[data,binSize,options,0];

StdDevEstimate[data_,binSize_,OptionsPattern[],repeat_:0]:=Module[{size,locBinSize,repeatNumber,binNumber,scramble,means,stdDevs,completedCount=0,startTime=AbsoluteTime[],formatTime},

size=Length[data];
locBinSize=binSize;
repeatNumber=repeat;

If[locBinSize>size/2,Print[Style["### WARNING: binSize too large, defaulting to half the data length ###",RGBColor[1,0,0]]];
locBinSize=Floor[size/2]];

binNumber=Floor[size/locBinSize];

If[OptionValue["print"],Print[Style[" binNumber= ",{RGBColor[0,0,1],Bold}],binNumber]];
If[repeatNumber==0,repeatNumber=Ceiling[size/binNumber]];

If[OptionValue["print"],Print[Style[" repeatNumber= ",{RGBColor[0,0,1],Bold}],repeatNumber]];

formatTime[sec_]:=With[{s=Round[sec]},StringRiffle[IntegerString[#,10,2]&/@{Quotient[s,3600],Mod[Quotient[s,60],60],Mod[s,60]},":"]];

(*Monitor tracks the calculation and shows a live panel*)
Monitor[stdDevs=Table[scramble=RandomSample[data];
scramble=Partition[scramble,UpTo[locBinSize]];
means=Mean/@scramble;
completedCount++;
{N[StandardDeviation[means]/Sqrt[Length[means]]]},repeatNumber],
(*Progress panel layout inside Monitor*)
Panel[Column[{Row[{Style["Task Progress: ",Bold],completedCount," / ",repeatNumber}],ProgressIndicator[completedCount,{0,repeatNumber},ImageSize->Large],With[{elapsed=Round[AbsoluteTime[]-startTime]},Row[{Style["Elapsed: ",Gray],formatTime[elapsed],"   |   ",Style["ETA: ",Gray],If[completedCount>0,With[{remaining=Round[(elapsed/completedCount)*(repeatNumber-completedCount)]},formatTime[remaining]],"00:00:00"]}]]},Spacings->1],Background->GrayLevel[0.95],FrameMargins->15]
];

stdDevs={Mean[stdDevs[[All,1]]]};

If[OptionValue["print"],Print[Style[" Std deviation/\!\(\*SqrtBox[\(N\)]\) estimate= ",{RGBColor[0,0,1],Bold}],stdDevs[[1]]//N]];

stdDevs[[1]]*Sqrt[size]]


(* ::Item::Closed:: *)
(*examples*)


(* ::Input:: *)
(*Partition[test,UpTo[4]]*)


(* ::Input:: *)
(*test={1,2,3,4,5,6,7,8,9};*)
(*StdDevEstimate[test,3,"print"->True]//N*)
(*Mean[test]//N*)
(*StandardDeviation[test]/Sqrt[Length[test]]//N*)


(* ::Subsection::Closed:: *)
(*Primary test: idd gaussian variables: checked!*)


(* ::Input:: *)
(*normData=Table[RandomReal[NormalDistribution[]],10000000];*)
(*normData=Developer`ToPackedArray[normData];*)


(* ::Input:: *)
(*Histogram[normData,1000]*)


(* ::Input:: *)
(*Mean[normData]*)
(*StandardDeviation[normData]*)
(*%/Sqrt[Length[normData]]*)
(**)
(*Sqrt[Mean[normData^2]-Mean[normData]^2]*)


(* ::Input:: *)
(*StdDevEstimate[normData,10,"print"->True]//N*)


(* ::Section::Closed:: *)
(*BootstrapEstimate[] definition and tests		TOO SLOW*)


(* ::Text:: *)
(*The  only differences are*)
(*1) RandomChoise instead of RandomSample, which DOES NOT REMOVE THE EXTRACTED VALUE*)
(*2) Use of quintile to extract asymmetric deviations*)


(* ::Input::Initialization:: *)
ClearAll[BootstrapEstimate];

Options[BootstrapEstimate]={"print"->False};

BootstrapEstimate[data_,options:OptionsPattern[]]:=BootstrapEstimate[data,options,20000];

BootstrapEstimate[data_,OptionsPattern[],repeat_]:=Module[{size,dataMean,repeatNumber,lowerCI,upperCI,deltaMinus, deltaPlus,sample,bootmeans,completedCount=0,startTime=AbsoluteTime[],formatTime},

size=Length[data];
dataMean=Mean[data];
repeatNumber=repeat;

formatTime[sec_]:=With[{s=Round[sec]},StringRiffle[IntegerString[#,10,2]&/@{Quotient[s,3600],Mod[Quotient[s,60],60],Mod[s,60]},":"]];

(*Monitor tracks the calculation and shows a live panel*)
Monitor[
bootmeans=Table[sample=RandomChoice[data,size];
completedCount++;
Mean[sample],repeatNumber],

(*Progress panel layout inside Monitor*)
Panel[Column[{Row[{Style["Task Progress: ",Bold],completedCount," / ",repeatNumber}],ProgressIndicator[completedCount,{0,repeatNumber},ImageSize->Large],With[{elapsed=Round[AbsoluteTime[]-startTime]},Row[{Style["Elapsed: ",Gray],formatTime[elapsed],"   |   ",Style["ETA: ",Gray],If[completedCount>0,With[{remaining=Round[(elapsed/completedCount)*(repeatNumber-completedCount)]},formatTime[remaining]],"00:00:00"]}]]},Spacings->1],Background->GrayLevel[0.95],FrameMargins->15]
];


{lowerCI,upperCI}=Quantile[bootmeans,{0.15865,0.84135}];

deltaMinus=dataMean-lowerCI;
deltaPlus=upperCI-dataMean;

(*5. Assign with Around*)
Around[dataMean,{deltaMinus,deltaPlus}]
]


(* ::Subsection::Closed:: *)
(*Primary test: idd gaussian variables: checked!*)


(* ::Input:: *)
(*normData=Table[RandomReal[NormalDistribution[]],10000000];*)
(*normData=Developer`ToPackedArray[normData];*)


(* ::Input:: *)
(*Histogram[normData,1000]*)


(* ::Input:: *)
(*Mean[normData]*)
(*StandardDeviation[normData]*)
(*%/Sqrt[Length[normData]]*)
(**)
(*Sqrt[Mean[normData^2]-Mean[normData]^2]*)


(* ::Input:: *)
(*BootstrapEstimate[normData,"print"->True]//N*)


(* ::Section:: *)
(*Application to b=0.5		TBD*)


(* ::Input:: *)
(*b=0.5;*)
(**)
(*rawData=Import["data05Sqaure-HybridSq.mx"];*)
(**)
(*Length[rawData]*)


(* ::Input:: *)
(*rawData=Pick[rawData,Unitize[Length/@rawData],1];*)
(**)
(*Length[rawData]*)


(* ::Input:: *)
(**)


(* ::Input:: *)
(*rawData=Pick[rawData,Length/@rawData,2];*)
(*Length[rawData]*)


(* ::Item::Closed:: *)
(*Run once to export MX file*)


(* ::Input:: *)
(*rawData=data05Square=Import["D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d-FromCluster\\b05-clean_merged_data-HybridSq.csv","CSV"];*)
(*(*Immediately lock it into a Packed Array*)*)
(*rawData=Developer`ToPackedArray[rawData];*)
(* (* MODIFY FILE NAME *)*)
(*Length[rawData]*)


(* ::Input:: *)
(*rawData=Pick[rawData,Unitize[Length/@rawData],1];*)
(**)
(*Length[rawData]*)


(* ::Input:: *)
(*rawData=Pick[rawData,Length/@rawData,2];*)
(*Length[rawData]*)


(* ::Input:: *)
(*Export["data05Sqaure-HybridSq.mx",rawData,"MX"]*)


(* ::Subsection::Closed:: *)
(*Continues: gather and so on*)


(* ::Input:: *)
(*(*1. Gather rows by their first element (x) at C-speed*)*)
(*gathered=GatherBy[rawData,First];*)
(**)
(*(*2. Extract the unique X values directly from the gathered groups*)*)
(*xValues=gathered[[All,1,1]];*)
(**)
(*(*3. Extract the Y values for each group*)*)
(*yGroups=gathered[[All,All,2]];*)
(**)
(*(*4. Map Mean and StandardDeviation across the groups in bulk*)*)
(*means=Mean/@yGroups/. 0.->1.0`*^-8;*)
(**)
(*(*Standard deviation throws an error/indeterminacy if length is 1,so we replace Indeterminate with 0 globally at the end*)*)
(*stdDevs=Check[StandardDeviation[#],1.0`*^-8](*/Sqrt[Length[#]]*)&/@yGroups/. Indeterminate->1.0`*^-8;*)
(*stdDevs=stdDevs/. 0.->1.0`*^-8;*)
(**)
(*(* stdDev on mean*)*)
(*stdDevsOnMean=(1/Sqrt[Length[#]]&/@yGroups)*stdDevs;*)
(**)
(*(* Maximum deviation*)*)
(*maxDevs=MapThread[Max[Abs[#1-#2]]&,{means//N,yGroups}]/. 0.->1.0`*^-8;*)
(**)
(**)
(*(*5. Combine them using the Threaded Around wrapper*)*)
(**)
(*averaged=Transpose[{xValues,means}];*)
(*averagedWithErrors=Transpose[{xValues,MapThread[Around,{means,stdDevs}]}];*)
(*averagedWithErrorsOnMean=Transpose[{xValues,MapThread[Around,{means,stdDevsOnMean}]}];*)
(*averagedWithMaxDev=Transpose[{xValues,MapThread[Around,{means,maxDevs}]}];*)


(* ::Input:: *)
(*(*NOT USING IT*)
(**)
(* Remove the Sqrt[N] factor from the StdDev estimates*)*)
(*(*MapThread[Times[#1,Sqrt[#2]]&,{stdDevs,yGroups}]*)
(**)
(*averagedWithErrors=Transpose[{xValues,MapThread[Around,{means,%}]}];*)*)


(* ::Input:: *)
(**)
(*(*Standard deviation with my code (Kay's trick for better estimate)*)*)
(*stdDevsEstimated=Check[StdDevEstimate[#,10,"print"->tTrue],1.0`*^-8]&/@yGroups/. Indeterminate->1.0`*^-8;*)
(*stdDevsEstimated=stdDevsEstimated/. 0.->1.0`*^-8;*)
(**)
(*averagedWithEstimatedStdDevs=Transpose[{xValues,MapThread[Around,{means,stdDevsEstimated}]}];*)


(* ::Input:: *)
(**)
(*stdDevsEstimatedOnMean=10/Sqrt[Length[#]]&/@yGroups*stdDevsEstimated;*)
(**)
(*averagedWithEstimatedStdDevsOnMean=Transpose[{xValues,MapThread[Around,{means,stdDevsEstimatedOnMean}]}];*)


(* ::Item::Closed:: *)
(*Fixed size analysis to try and get the best parameters (e.g. bin size) -> Around ??? (with the estimate it's a bit bigger)		TBD*)


(* ::Input:: *)
(*(* Take the 11th element which contains many points. As it can be seen in the Histogram below, the distribution is far from being symmetric *)*)


(* ::Input:: *)
(*gathered[[1]]*)


(* ::Input:: *)
(*Ordering[gathered][[-1]]*)
(*gathered[[%]]*)


(* ::Input:: *)
(*gathered[[9;;11]];*)
(*Length/@%*)
(*histoData=%%[[All,All,2]];*)
(*Histogram[#,Length[#],PlotRange->All]&/@%*)


(* ::Input:: *)
(*Skewness[histoData]//N*)


(* ::Subitem::Closed:: *)
(*identification of best binSize -> best of both skewness ans kurtosis:  289*)


(* ::Input:: *)
(*Partition[histoData,UpTo[50]];*)
(*Length@%*)
(*partData=Mean/@%%;*)
(*Histogram[partData,Automatic,"Probability",PlotRange->All]*)
(*Skewness[partData]//N*)
(*Kurtosis[partData]//N*)


(* ::Input:: *)
(*{x,y}=Transpose*)
(*Clear[x,y]*)


(* ::Input:: *)
(*listOfBinSizes={listOfBinSizesSkewness,listOfBinSizesKurtosis}=Transpose[Table[*)
(*part=Partition[histoData,UpTo[binSize]];*)
(*part=Mean/@part;*)
(*{{Abs[Skewness[part]//N],binSize},{Abs[Kurtosis[part]//N],binSize}}*)
(*,{binSize,1,300,2}]];*)


(* ::Input:: *)
(*(Ordering/@{listOfBinSizesSkewness,listOfBinSizesKurtosis})[[All,1;;10]]*)
(*Part[listOfBinSizesSkewness,#]&/@%[[1]]*)
(*Part[listOfBinSizesKurtosis,#]&/@%%[[2]]*)


(* ::Input:: *)
(**)


(* ::Input:: *)
(*listOfBinSizesSkewness[[143]]*)
(**)
(*Partition[histoData,UpTo[%[[2]]]];*)
(*Length@%*)
(*partData=Mean/@%%;*)
(*Histogram[partData,Automatic,"Probability",PlotRange->All]*)
(*Skewness[partData]//N*)
(*Kurtosis[partData]//N*)
(**)
(**)
(*listOfBinSizesKurtosis[[128]]*)
(**)
(*Partition[histoData,UpTo[%[[2]]]];*)
(*Length@%*)
(*partData=Mean/@%%;*)
(*Histogram[partData,Automatic,"Probability",PlotRange->All]*)
(*Skewness[partData]//N*)
(*Kurtosis[partData]//N*)
(**)


(* ::Input:: *)
(*(*Best of both worlds*)*)


(* ::Input:: *)
(*listOfBinSizesSkewness[[145]]*)
(**)
(*Partition[histoData,UpTo[%[[2]]]];*)
(*Length@%*)
(*partData=Mean/@%%;*)
(*Histogram[partData,Automatic,"Probability",PlotRange->All]*)
(*Skewness[partData]//N*)
(*Kurtosis[partData]//N*)
(**)


(* ::Subitem::Closed:: *)
(*StdDev Difference: with the estimate it's a bit bigger*)


(* ::Input:: *)
(*StandardDeviation[histoData]//N*)


(* ::Input:: *)
(*StdDevEstimate[histoData,289,"print"->True]*)


(* ::Item::Closed:: *)
(*Check how the distribution and the mean change with a lot of statistic 			TBD*)


(* ::Input:: *)
(*gatheredLogSpacing=GatherBy[logSpacing,First];*)
(*Length/@%*)


(* ::Input:: *)
(*Ordering[gatheredLogSpacing][[-1]];*)
(*sample=Part[gatheredLogSpacing,#]&@%;*)


(* ::Input:: *)
(*sample[[1;;2]]*)


(* ::Input:: *)
(*meanSample=Mean[sample[[All,2]]]//N*)


(* ::Subitem::Closed:: *)
(*Find it in gathered*)


(* ::Input:: *)
(*Position[gathered,_?(#[[1,1]]==599&),{1}]*)


(* ::Input:: *)
(*comparison=gathered[[775]];*)


(* ::Input:: *)
(*meanComparison=Mean[comparison[[All,2]]]//N*)


(* ::Subitem::Closed:: *)
(*Histograms*)


(* ::Input:: *)
(*{sample,comparison};*)
(*Length/@%*)
(*histoData=%%[[All,All,2]];*)
(*Histogram[#,Length[#],PlotRange->All]&/@%*)


(* ::Subsection::Closed:: *)
(*Take the Log*)


(* ::Input:: *)
(*averaged=Select[averaged,#[[1]]=!=""&&#[[1]]>5&];*)
(*averagedWithErrors=Select[averagedWithErrors,#[[1]]=!=""&&#[[1]]>5&];*)
(*averagedWithErrorsOnMean=Select[averagedWithErrorsOnMean,#[[1]]=!=""&&#[[1]]>5&];*)
(*averagedWithMaxDev=Select[averagedWithMaxDev,#[[1]]=!=""&&#[[1]]>5&];*)
(*(*averagedWithEstimatedStdDevs=Select[averagedWithEstimatedStdDevs,#[[1]]=!=""&];*)
(*averagedWithEstimatedStdDevsOnMean=Select[averagedWithEstimatedStdDevsOnMean,#[[1]]=!=""&];*)*)


(* ::Input:: *)
(*logAveraged=Log[averaged];*)
(*logAveragedWithErrors=Log[averagedWithErrors]/. 0->Around[1.0`*^-6,1.0`*^-6];*)
(*logAveragedWithErrorsOnMean=Log[averagedWithErrorsOnMean]/. 0->Around[1.0`*^-6,1.0`*^-6];*)
(*logAveragedWithMaxDev=Log[averagedWithMaxDev]/. 0->Around[1.0`*^-6,1.0`*^-6];*)
(*(*logAveragedWithEstimatedStdDevs=Log[averagedWithEstimatedStdDevs]/. 0->Around[1.0`*^-6,1.0`*^-6];*)
(*logAveragedWithEstimatedStdDevsOnMean=Log[averagedWithEstimatedStdDevsOnMean]/. 0->Around[1.0`*^-6,1.0`*^-6];*)*)


(* ::Input:: *)
(*logAveragedWithErrors[[1;;10]]*)


(* ::Input:: *)
(*logAveragedWithEstimatedStdDevs[[1;;10]]*)


(* ::Input:: *)
(*(*Check which error is bigger*)*)
(*(Part[#,All,2,2]&/@{logAveragedWithEstimatedStdDevs,-logAveragedWithErrors});*)
(*Mean[(#[[1]]-#[[2]]&@%)]*)


(* ::Input:: *)
(*maxx=Max[averaged[[All,1]]];*)
(*maxy=Max[averaged[[All,2]]];*)
(* *)
(*Show[{ListPlot[logAveragedWithMaxDev,PlotStyle->{GrayLevel[0],Directive[Opacity[0.3]],PointSize->0.001},AxesLabel->{"Log[L]","Log[N]"}]*)
(*,ListPlot[logAveragedWithErrors,PlotStyle->{RGBColor[0, 0.78, 1],PointSize->0.01},PlotLegends->PointLegend[{"logAveragedWithErrors"},LegendMarkerSize->10,LegendMarkers->Graphics[Disk[]]]]*)
(*,ListPlot[logAveragedWithErrorsOnMean,PlotStyle->{RGBColor[1, 0.55, 1],Directive[Opacity[0.6]],PointSize->0.005},PlotLegends->PointLegend[{"logAveragedWithErrorsOnMean"},LegendMarkerSize->10,LegendMarkers->Graphics[Disk[]]]]*)
(*,Plot[#,{x,Log[0+1],Log[maxx]},PlotStyle->{RGBColor[0, 0, 1],Dashed},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@(x (dfSLE/. bb->N[b])-0.9)*)
(*}*)
(*,PlotLabel->Row[{" b = ",b}],PlotRange->{All,{0,Log[maxy]}},AxesOrigin->{1,0},ImageSize->700]*)
(*(**)
(*Show[{ListPlot[logAveragedWithMaxDev,PlotStyle->{,Directive[Opacity[0.3]],PointSize->0.001},AxesLabel->{"Log[L]","Log[N]"}]*)
(*,ListPlot[logAveragedWithEstimatedStdDevs,PlotStyle->{,Directive[Opacity[0.8]],PointSize->0.008},PlotLegends->PointLegend[{"logAveragedWithEstimatedStdDevs"},LegendMarkerSize->10,LegendMarkers->Graphics[Disk[]]]]*)
(*,ListPlot[logAveragedWithEstimatedStdDevsOnMean,PlotStyle->{,Directive[Opacity[0.6]],PointSize->0.005},PlotLegends->PointLegend[{"logAveragedWithEstimatedStdDevsOnMean"},LegendMarkerSize->10,LegendMarkers->Graphics[Disk[]]]]*)
(*,Plot[#,{x,Log[0+1],Log[maxx]},PlotStyle->{,Dashed},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@(x (dfSLE/. bb->N[b])-0.9)*)
(*}*)
(*,PlotLabel->Row[{" b = ",b}],PlotRange->{All,{0,Log[maxy]}},AxesOrigin->{1,0},ImageSize->700]*)*)
(**)
(**)


(* ::Input:: *)
(*logAveragedWithErrors//Length*)
(*weights=Map[#[[2]]["Uncertainty"]&,logAveragedWithErrors];*)
(*%//Length*)
(*Position[weights,_?(Element[#,Reals]=!=True&)]*)
(**)
(**)
(*weights=Map[#[[2]]["Uncertainty"]&,logAveragedWithMaxDev];*)
(*%//Length*)
(*Position[weights,_?(Element[#,Reals]=!=True&)]*)
(**)
(**)
(*weights=Map[#[[2]]["Uncertainty"]&,logAveragedWithEstimatedStdDevs];*)
(*%//Length*)
(*Position[weights,_?(Element[#,Reals]=!=True&)]*)


(* ::Subsection::Closed:: *)
(*Looking for the best fitting strategy*)


(* ::Subsubsection::Closed:: *)
(*Linear fit with Errors obtained with StandardDeviation[]*)


(* ::Input:: *)
(*fitFunc=a+c Exp[-\[Omega] x]+df x;*)
(*fitFunc=a+df x;*)
(**)
(*lmAveragedWithStdDevs=NonlinearModelFit[logAveragedWithErrors,{fitFunc,{a<0}},{a,df},x,MaxIterations->10000];*)
(**)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@lmAveragedWithStdDevs*)
(**)
(*lmAveragedWithStdDevsGlobal=NonlinearModelFit[logAveragedWithErrors,{fitFunc,{a<0}},{a,df},x,MaxIterations->1000,Method->"NMinimize"];*)
(**)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@lmAveragedWithStdDevsGlobal*)
(**)


(* ::Input:: *)
(*synchronizedPlots=Map[Show[#,ImageSize->500]&,{ListPlot[lmAveragedWithStdDevs["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis lmAveragedWithStdDevs,\nAdjustedRSquared=",lmAveragedWithStdDevs["AdjustedRSquared"]}]]*)
(**)
(*,ListPlot[lmAveragedWithStdDevsGlobal["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis lmAveragedWithStdDevsGlobal,\nAdjustedRSquared=",lmAveragedWithStdDevsGlobal["AdjustedRSquared"]}]]}];*)
(**)
(*Multicolumn[synchronizedPlots,2,Appearance->"Framed"]*)
(**)


(* ::Input:: *)
(*maxx=Max[averaged[[All,1]]];*)
(*maxy=Max[averaged[[All,2]]];*)
(**)
(*thresholdBelow=0;*)
(*thresholdAbove=maxx-0;*)
(**)
(*Show[{ListPlot[logAveragedWithMaxDev,PlotStyle->{GrayLevel[0],Directive[Opacity[0.3]]},AxesLabel->{"Log[L]","Log[N]"}]*)
(*,ListPlot[logAveragedWithErrors,PlotStyle->RGBColor[0, 0.78, 1],PlotLegends->{"logAveragedWithErrors"}]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 0.68, 0.5],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@lmAveragedWithStdDevs[x]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.85, 0, 0.5],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@lmAveragedWithStdDevsGlobal[x]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0, 0, 1],Dashed},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@(x (dfSLE/.bb->N[b])-1)*)
(*}*)
(*,PlotLabel->Row[{" b = ",b}]*)
(*,PlotRange->{All,{0,Log[maxy]}},AxesOrigin->{1,0},ImageSize->700]*)


(* ::Subsubsection::Closed:: *)
(*NonLinear fit Errors obtained with StandardDeviation[]. IT STRUGGLES TO FIND THE RIGHT FIT WITH a+c E^(-x \[Omega])+df x.*)


(* ::Input:: *)
(*fitFunc=a+c Exp[-\[Omega] x]+df x;*)
(*(*fitFunc=a+c Exp[- x]+df x;*)*)
(*(**)
(*nlmAveragedWithStdDevs=NonlinearModelFit[logAveragedWithErrors,{fitFunc,{(*-2<a<2,*)a<0(*,0.5<\[Omega]<=3*),c<0(*,1<df<1.1*)}},{a,c,\[Omega](*{a,-0.5},{c,-30.},{\[Omega],2.}*),df(*,{df,1.024}*)},x,MaxIterations->1000];*)
(*nlmAveragedWithStdDevsUnconstrained=NonlinearModelFit[logAveragedWithErrors,{fitFunc,{-2<a<2,0.5<\[Omega]<=3(*,1<df<1.1*)}},{a,c,\[Omega](*{a,-0.5},{c,-30.},{\[Omega],2.}*),df(*,{df,1.024}*)},x,MaxIterations->1000];*)
(**)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@nlmAveragedWithStdDevs*)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@nlmAveragedWithStdDevsUnconstrained*)*)
(**)
(**)
(*nlmAveragedWithStdDevsGlobal=NonlinearModelFit[logAveragedWithErrors,{fitFunc,{(*-2<a<2,*)a<0,c<0,0.5<\[Omega]<=1.1,1<df<1.1}},{(*a,c,\[Omega],df*){a,-0.5},{c,-30.},{\[Omega],1.},{df,1.024}},x,MaxIterations->1000,Method->"NMinimize"];*)
(**)
(*nlmAveragedWithStdDevsUnconstrainedGlobal=NonlinearModelFit[logAveragedWithErrors,fitFunc,{{a,-0.5},{c,-30.},\[Omega](*,{\[Omega],2.}*),df(*,{df,1.024}*)},x,MaxIterations->1000,Method->"NMinimize"];*)
(**)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@nlmAveragedWithStdDevsGlobal*)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@nlmAveragedWithStdDevsUnconstrainedGlobal*)
(**)


(* ::Input:: *)
(*synchronizedPlots=Map[Show[#,ImageSize->500]&,{ListPlot[nlmAveragedWithStdDevs["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis nlmAveragedWithStdDevs,\nAdjustedRSquared=",nlmAveragedWithStdDevs["AdjustedRSquared"]}]]*)
(*,ListPlot[nlmAveragedWithStdDevsUnconstrained["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis nlmAveragedWithStdDevsUnconstrained,\nAdjustedRSquared=",nlmAveragedWithStdDevsUnconstrained["AdjustedRSquared"]}]]*)
(*,ListPlot[nlmAveragedWithStdDevsGlobal["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis nlmAveragedWithStdDevsGlobal,\nAdjustedRSquared=",nlmAveragedWithStdDevsGlobal["AdjustedRSquared"]}]]*)
(*,ListPlot[nlmAveragedWithStdDevsUnconstrainedGlobal["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis nlmAveragedWithStdDevsUnconstrainedGlobal,\nAdjustedRSquared=",nlmAveragedWithStdDevsUnconstrainedGlobal["AdjustedRSquared"]}]]}];*)
(**)
(*Multicolumn[synchronizedPlots,2,Appearance->"Framed"]*)
(**)


(* ::Input:: *)
(*maxx=Max[averaged[[All,1]]];*)
(*maxy=Max[averaged[[All,2]]];*)
(**)
(*thresholdBelow=0;*)
(*thresholdAbove=maxx-0;*)
(**)
(*Show[{ListPlot[logAveragedWithMaxDev,PlotStyle->{GrayLevel[0],Directive[Opacity[0.3]]},AxesLabel->{"Log[L]","Log[N]"}]*)
(*,ListPlot[logAveragedWithErrors,PlotStyle->RGBColor[1, 0.55, 1],PlotLegends->{"logAveragedWithErrors"}](**)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{,Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@nlmAveragedWithStdDevs[x]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{,Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@nlmAveragedWithStdDevsUnconstrained[x]*)*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.85, 0, 0.5],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@nlmAveragedWithStdDevsGlobal[x]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 0.68, 0.8],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@nlmAveragedWithStdDevsUnconstrainedGlobal[x]*)
(**)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0, 0, 1],Dashed},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@(x (dfSLE/.bb->N[b])-1)*)
(*}*)
(*,PlotLabel->Row[{" b = ",b}]*)
(*,PlotRange->{All,{0,Log[maxy]}},AxesOrigin->{1,0},ImageSize->700]*)


(* ::Subsection:: *)
(*Drop both first and/or last few 		WORKS PRETTY WELL (LACKING STATISTICS AT BIG L)*)


(* ::Subsubsection:: *)
(*Linear*)


(* ::Input:: *)
(*maxx=Max[averaged[[All,1]]];*)
(*maxy=Max[averaged[[All,2]]];*)
(**)
(*thresholdBelow=25;*)
(*thresholdAbove=maxx-650;*)
(*(*Let's drop some*)*)
(*droppedWithMaxDev=Select[logAveragedWithMaxDev,Log[thresholdBelow]<#[[1]]<Log[thresholdAbove]&];*)
(*droppedWithErrors=Select[logAveragedWithErrors,Log[thresholdBelow]<#[[1]]<Log[thresholdAbove]&];*)
(*droppedWithErrorsOnMean=Select[logAveragedWithErrorsOnMean,Log[thresholdBelow]<#[[1]]<Log[thresholdAbove]&];*)
(*(*droppedWithEstimatedStdDevs=Select[logAveragedWithEstimatedStdDevs,Log[thresholdBelow]<#[[1]]<Log[thresholdAbove]&];*)
(*droppedWithEstimatedStdDevsOnMean=Select[logAveragedWithEstimatedStdDevsOnMean,Log[thresholdBelow]<#[[1]]<Log[thresholdAbove]&];*)
(**)*)
(*(*drop specific bad point*)*)
(*Length[droppedWithMaxDev];*)
(*droppedWithMaxDev=DeleteCases[droppedWithMaxDev,_?(6.6<N[#[[1]]]<6.75&)];*)
(*Length[droppedWithMaxDev];*)
(*droppedWithErrors=DeleteCases[droppedWithErrors,_?(6.6<N[#[[1]]]<6.75&)];*)
(*droppedWithErrorsOnMean=DeleteCases[droppedWithErrorsOnMean,_?(6.6<N[#[[1]]]<6.75&)];*)
(*(*droppedWithEstimatedStdDevs=DeleteCases[droppedWithEstimatedStdDevs,_?(6.6<N[#[[1]]]<6.75&)];*)
(*droppedWithEstimatedStdDevsOnMean=DeleteCases[droppedWithEstimatedStdDevsOnMean,_?(6.6<N[#[[1]]]<6.75&)];*)
(**)*)
(**)
(**)
(*lmdroppedWithMaxDev=LinearModelFit[droppedWithMaxDev,x,x,Weights->Automatic];*)
(*lmdroppedWithErrors=NonlinearModelFit[droppedWithErrors,{a+df x,{a<0,1<df<2}},{a,df},x,Weights->Automatic,Method->"NMinimize"]*)
(*lmdroppedWithErrorsOnMean=NonlinearModelFit[droppedWithErrorsOnMean,{a+df x,{a<0,1<df<2}},{a,df},x,Weights->Automatic,Method->"NMinimize"]*)
(*(*lmdroppedWithEstimatedStdDevs=NonlinearModelFit[droppedWithEstimatedStdDevs,{a+df x,{a<0,1<df<2}},{a,df},x,Weights->Automatic,Method->"NMinimize"];*)
(*lmdroppedWithEstimatedStdDevsOnMean=NonlinearModelFit[droppedWithEstimatedStdDevsOnMean,{a+df x,{a<0,1<df<2}},{a,df},x,Weights->Automatic,Method->"NMinimize"];*)
(**)*)
(**)
(*Show[{ListPlot[logAveragedWithMaxDev,PlotStyle->{GrayLevel[0],Directive[Opacity[0.3]]},AxesLabel->{"Log[L]","Log[N]"}]*)
(*,ListPlot[droppedWithErrors,PlotStyle->{RGBColor[1, 0.78, 0.13],PointSize->0.005},PlotLegends->PointLegend[{"droppedWithErrors"},LegendMarkerSize->10,LegendMarkers->Graphics[Disk[]]]]*)
(*,ListPlot[droppedWithErrorsOnMean,PlotStyle->{RGBColor[0, 0.78, 1],PointSize->0.005},PlotLegends->PointLegend[{"droppedWithErrorsOnMean"},LegendMarkerSize->10,LegendMarkers->Graphics[Disk[]]]]*)
(*,Plot[#[x],{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 0, 0.5],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#[x]]}],Right]]&@lmdroppedWithMaxDev*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[1, 0.68, 0.6],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@lmdroppedWithErrors[x]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 0.68, 0.5],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@lmdroppedWithErrorsOnMean[x]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0, 0, 1],Dashed},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@(x (dfSLE/.bb->N[b])-1)*)
(*(*,Plot[#,{x,0,Log[thresholdAbove]},PlotStyle->{,Dashed},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@(x (dfSLE/.bb->N[b])-0.7-5Exp[-0.991 x])*)*)
(*}*)
(*,Epilog->{Directive[Dashed,GrayLevel[0.5]],*)
(*Line[{{Log[thresholdBelow],0},{Log[thresholdBelow],Log[maxy]//N}}]*)
(*,Line[{{Log[thresholdAbove],0},{Log[thresholdAbove],Log[maxy]//N}}]}*)
(*,PlotLabel->Row[{" b = ",b}]*)
(*,PlotRange->{All,{0,Log[maxy]}}(*PlotRange->{{Log[thresholdBelow],Log[maxy]},{4,7.2}}*),AxesOrigin->{(*Log[thresholdBelow]*)1,0},ImageSize->700]*)


(* ::Input:: *)
(*Show[{ListPlot[logAveragedWithMaxDev,PlotStyle->{GrayLevel[0],Directive[Opacity[0.3]]},AxesLabel->{"Log[L]","Log[N]"}]*)
(*,ListPlot[droppedWithEstimatedStdDevs,PlotStyle->{RGBColor[1, 0.78, 0.13],PointSize->0.005},PlotLegends->PointLegend[{"droppedWithEstimatedStdDevs"},LegendMarkerSize->10,LegendMarkers->Graphics[Disk[]]]]*)
(*,ListPlot[droppedWithEstimatedStdDevsOnMean,PlotStyle->{RGBColor[0.49, 0.02, 1],PointSize->0.005},PlotLegends->PointLegend[{"droppedWithEstimatedStdDevsOnMean"},LegendMarkerSize->10,LegendMarkers->Graphics[Disk[]]]]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 0.68, 0.5],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@lmdroppedWithEstimatedStdDevs[x]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.85, 0, 0.5],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@lmdroppedWithEstimatedStdDevsOnMean[x]*)
(**)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0, 0, 1],Dashed},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@(x (dfSLE/.bb->N[b])-1)*)
(*(*,Plot[#,{x,0,Log[thresholdAbove]},PlotStyle->{,Dashed},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@(x (dfSLE/.bb->N[b])-0.7-5Exp[-0.991 x])*)*)
(*}*)
(*,Epilog->{Directive[Dashed,GrayLevel[0.5]],*)
(*Line[{{Log[thresholdBelow],0},{Log[thresholdBelow],Log[maxy]//N}}]*)
(*,Line[{{Log[thresholdAbove],0},{Log[thresholdAbove],Log[maxy]//N}}]}*)
(*,PlotLabel->Row[{" b = ",b}]*)
(*,PlotRange->{All,{0,Log[maxy]}}(*PlotRange->{{Log[thresholdBelow],Log[maxy]},{4,7.2}}*),AxesOrigin->{(*Log[thresholdBelow]*)1,0},ImageSize->700]*)


(* ::Input:: *)
(*Sort[droppedWithMaxDev//N]*)


(* ::Subsubsection::Closed:: *)
(*Non-linear*)


(* ::Input:: *)
(*maxx=Max[rawData[[All,1]]];*)
(*maxy=Max[rawData[[All,2]]];*)
(**)
(*thresholdBelow=23;*)
(*thresholdAbove=maxx-500;*)
(**)
(*droppedWithMaxDev=Select[logAveragedWithMaxDev,Log[thresholdBelow]<#[[1]]<Log[thresholdAbove]&];*)
(*droppedWithErrors=Select[logAveragedWithErrors,Log[thresholdBelow]<#[[1]]<Log[thresholdAbove]&];*)
(**)
(*fitFunc=a+c Exp[- x]+df x;*)
(**)
(*nlmdroppedWithMaxDev=NonlinearModelFit[droppedWithMaxDev,fitFunc,{{a,-0.5},{c,-30.},{\[Omega],1.},df(*,{df,1.024}*)},x,MaxIterations->1000,Method->"NMinimize"];*)
(*nlmdroppedWithErrors=NonlinearModelFit[droppedWithErrors,fitFunc,{{a,-0.5},{c,-30.},{\[Omega],1.},df(*,{df,1.024}*)},x,MaxIterations->1000,Method->"NMinimize"];*)
(*(*lmpdropped=LinearModelFit[dropped,fitFuncs,x];*)*)
(**)
(*Show[{ListPlot[logAveragedWithMaxDev,PlotStyle->{GrayLevel[0],Directive[Opacity[0.3]]},AxesLabel->{"Log[L]","Log[N]"}]*)
(*,ListPlot[logAveragedWithErrors,PlotStyle->RGBColor[0, 0.78, 1],PlotLegends->{"logAveragedWithErrors"}]*)
(**)
(*(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{,Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@nlmdroppedWithMaxDev[x]*)*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[1, 0.68, 0.6],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@nlmdroppedWithErrors[x]*)
(*(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{,Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@lmAveragedWithStdDevs[x]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{,Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@lmAveragedWithStdDevsGlobal[x]*)*)
(**)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0, 0, 1],Dashed},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@(x (dfSLE/.bb->N[b])-1)*)
(*,Plot[#,{x,0,Log[thresholdAbove]},PlotStyle->{RGBColor[0, 1, 1],Dashed},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@(x (dfSLE/.bb->N[b])-0.7-5Exp[-0.991 x])*)
(*}*)
(*,Epilog->{Directive[Dashed,GrayLevel[0.5]],*)
(*Line[{{Log[thresholdBelow],0},{Log[thresholdBelow],Log[maxy]//N}}]*)
(*,Line[{{Log[thresholdAbove],0},{Log[thresholdAbove],Log[maxy]//N}}]}*)
(*,PlotLabel->Row[{" b = ",b}]*)
(*,PlotRange->{All,{0,All}}(*PlotRange->{{Log[thresholdBelow],Log[maxy]},{4,7.2}}*),AxesOrigin->{(*Log[thresholdBelow]*)1,0},ImageSize->700]*)


(* ::Subsection::Closed:: *)
(*With and without Method -> "NMinimize" (which looks for the global minimum, I think it's always better)*)


(* ::Item::Closed:: *)
(*No errors*)


(* ::Input:: *)
(*fitFunc=a+c Exp[-\[Omega] x]+df x;*)
(*fitFunc=a-c Exp[- x]+df x;*)
(*nlmAveraged=NonlinearModelFit[logAveraged,{fitFunc(*,{-2<a<2,0<\[Omega]<28}*)},{a,c,(*c,\[Omega],*)df},x];*)
(*nlmAveragedGlobal=NonlinearModelFit[logAveraged,{fitFunc(*,{-2<a<2,0<\[Omega]<28}*)},{a,c,(*c,\[Omega],*)df},x,Method->"NMinimize"];*)
(**)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@nlmAveraged*)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@nlmAveragedGlobal*)


(* ::Input:: *)
(*(*Extract and plot residuals*)*)
(**)
(*nlmAveraged["AdjustedRSquared"]*)
(*ListPlot[nlmAveraged["FitResiduals"],Filling->Axis,PlotLabel->"Residual Analysis"]*)
(**)
(*nlmAveragedGlobal["AdjustedRSquared"]*)
(*ListPlot[nlmAveragedGlobal["FitResiduals"],Filling->Axis,PlotLabel->"Residual Analysis"]*)


(* ::Input:: *)
(*maxx=Max[rawData[[All,1]]];*)
(*maxy=Max[rawData[[All,2]]];*)
(**)
(*thresholdBelow=0;*)
(*thresholdAbove=maxx-0;*)
(**)
(*Show[{ListPlot[logAveragedWithMaxDev,PlotStyle->{GrayLevel[0],Directive[Opacity[0.3]]},AxesLabel->{"Log[L]","Log[N]"}]*)
(*,ListPlot[logAveragedWithErrors,PlotStyle->RGBColor[0, 1, 1]]*)
(*,ListPlot[logAveragedWithEstimatedStdDevs,PlotStyle->{RGBColor[1, 0.55, 1],Directive[Opacity[0.3]]}]*)
(**)
(*(*,Plot[lmp[x],{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{,Thickness->0.004}]*)*)
(**)
(*,Plot[nlmAveraged[x],{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 1, 0.5],Thickness->0.004}]*)
(*,Plot[nlmAveragedGlobal[x],{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 0, 0.5],Thickness->0.004}]*)
(**)
(*,Plot[x (dfSLE/.bb->b)-1,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0, 0, 1],Dashed}]}*)
(*,(*PlotRange->All,*)PlotLabel->Row[{" b = ",b}]*)
(*(*,Epilog->{Directive[Dashed,],*)
(*Line[{{Log[thresholdBelow],0},{Log[thresholdBelow],Log[maxy]//N}}]*)
(*,Line[{{Log[thresholdAbove],0},{Log[thresholdAbove],Log[maxy]//N}}]}*)*)
(*,PlotRange->{{1,All},{0,All}},AxesOrigin->{1,0}]*)
(**)
(**)
(*Print[ (*"Full data - Linear fit : Subscript[d, f]=",Around[Quiet@lm["ParameterTable"][[1]][[1,3,2]],\[Pi]*lm["ParameterErrors"][[2]]],*)"*)
(*\!\(\*TemplateBox[<|\"color\" -> RGBColor[0.5, 1, 0.5]|>,\n\"RGBColorSwatchTemplate\"]\) Averaged data fit without errors and " ,fitFunc,": \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@nlmAveraged["ParameterTable"][[1,1,-1,2]],\[Pi]*Quiet@nlmAveraged["ParameterTable"][[1,1,-1,3]]],"*)
(*\!\(\*TemplateBox[<|\"color\" -> RGBColor[0.5, 0, 0.5]|>,\n\"RGBColorSwatchTemplate\"]\) Averaged data fit without errors, global minimum and " ,fitFunc,": \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@nlmAveragedGlobal["ParameterTable"][[1,1,-1,2]],\[Pi]*Quiet@nlmAveragedGlobal["ParameterTable"][[1,1,-1,3]]],"*)
(**)
(*\!\(\*TemplateBox[<|\"color\" -> RGBColor[0, 0, 1]|>,\n\"RGBColorSwatchTemplate\"]\) Result from the litterature (exact with SLE): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Input:: *)
(*Series[Log[1+x],{x,0,2}]*)


(* ::Input:: *)
(*Quiet@nlmAveragedGlobal["ParameterTable"][[1,1,-1,3]]*)


(* ::Item::Closed:: *)
(*Errors obtained with StandardDeviation[]*)


(* ::Input:: *)
(*fitFunc=a+c Exp[-\[Omega] x]+df x;*)
(*fitFunc=a+c Exp[- x]+df x;*)
(**)
(*nlmAveragedWithStdDevs=NonlinearModelFit[logAveragedWithErrors,{fitFunc,{(*-2<a<2,*)a<0(*,0.5<\[Omega]<=3*),c<0(*,1<df<1.1*)}},{a,c,\[Omega](*{a,-0.5},{c,-30.},{\[Omega],2.}*),df(*,{df,1.083}*)},x,MaxIterations->10000];*)
(*nlmAveragedWithStdDevsUnconstrained=NonlinearModelFit[logAveragedWithErrors,{fitFunc,{(*-2<a<2,*)(*,0.5<\[Omega]<=3*)(*,1<df<1.1*)}},{a,c,\[Omega](*{a,-0.5},{c,-30.},{\[Omega],2.}*),df(*,{df,1.083}*)},x,MaxIterations->10000];*)
(**)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@nlmAveragedWithStdDevs*)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@nlmAveragedWithStdDevsUnconstrained*)
(**)
(*nlmAveragedWithStdDevsGlobal=NonlinearModelFit[logAveragedWithErrors,{fitFunc,{(*-2<a<2,*)a<0(*,0.5<\[Omega]<=3*),c<0(*,1<df<1.1*)}},{a,c,\[Omega](*{a,-0.5},{c,-30.},{\[Omega],2.}*),df(*,{df,1.083}*)},x,MaxIterations->1000,Method->"NMinimize"];*)
(*nlmAveragedWithStdDevsUnconstrainedGlobal=NonlinearModelFit[logAveragedWithErrors,fitFunc,{a,c,\[Omega](*{a,-0.5},{c,-30.},{\[Omega],2.}*),df(*,{df,1.083}*)},x,MaxIterations->1000,Method->"NMinimize"];*)
(**)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@nlmAveragedWithStdDevsGlobal*)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@nlmAveragedWithStdDevsUnconstrainedGlobal*)
(**)


(* ::Input:: *)
(*synchronizedPlots=Map[Show[#,ImageSize->500]&,{ListPlot[nlmAveragedWithStdDevs["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis nlmAveragedWithStdDevs,\nAdjustedRSquared=",nlmAveragedWithStdDevs["AdjustedRSquared"]}]]*)
(*,ListPlot[nlmAveragedWithStdDevsUnconstrained["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis nlmAveragedWithStdDevsUnconstrained,\nAdjustedRSquared=",nlmAveragedWithStdDevsUnconstrained["AdjustedRSquared"]}]]*)
(*,ListPlot[nlmAveragedWithStdDevsGlobal["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis nlmAveragedWithStdDevsGlobal,\nAdjustedRSquared=",nlmAveragedWithStdDevsGlobal["AdjustedRSquared"]}]]*)
(*,ListPlot[nlmAveragedWithStdDevsUnconstrainedGlobal["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis nlmAveragedWithStdDevsUnconstrainedGlobal,\nAdjustedRSquared=",nlmAveragedWithStdDevsUnconstrainedGlobal["AdjustedRSquared"]}]]}];*)
(**)
(*Multicolumn[synchronizedPlots,2,Appearance->"Framed"]*)
(**)


(* ::Input:: *)
(*maxx=Max[rawData[[All,1]]];*)
(*maxy=Max[rawData[[All,2]]];*)
(**)
(*thresholdBelow=0;*)
(*thresholdAbove=maxx-0;*)
(**)
(*Show[{ListPlot[logAveragedWithMaxDev,PlotStyle->{GrayLevel[0],Directive[Opacity[0.3]]},AxesLabel->{"Log[L]","Log[N]"}]*)
(*,ListPlot[logAveragedWithErrors,PlotStyle->RGBColor[1, 0.47000000000000003`, 0],PlotLegends->{"logAveragedWithErrors"}]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 0, 0.5],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@nlmAveragedWithStdDevs[x]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 0.68, 0.5],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@nlmAveragedWithStdDevsUnconstrained[x]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.85, 0, 0.5],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@nlmAveragedWithStdDevsGlobal[x]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 0.68, 0.8],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@nlmAveragedWithStdDevsUnconstrainedGlobal[x]*)
(**)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0, 0, 1],Dashed},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@(x (dfSLE/.bb->N[b])-1)*)
(*}*)
(*,PlotLabel->Row[{" b = ",b}]*)
(*,PlotRange->{All,{0,All}},AxesOrigin->{1,0},ImageSize->700]*)
(**)
(*Print[ "Averaged data fit with errors and " ,fitFunc," \!\(\*TemplateBox[<|\"color\" -> RGBColor[0.5, 0, 0.5]|>,\n\"RGBColorSwatchTemplate\"]\): \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@nlmAveragedWithStdDevs["ParameterTable"][[1]][[1,-1,2]],\[Pi]*nlmAveragedWithStdDevs["ParameterTable"][[1]][[1,-1,3]]],", Parameters:",Quiet@nlmAveragedWithStdDevs["ParameterTable"],"*)
(*Averaged data fit with errors and " ,fitFunc," (unconstrained) \!\(\*TemplateBox[<|\"color\" -> RGBColor[0.5, 0.68, 0.5]|>,\n\"RGBColorSwatchTemplate\"]\): \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@nlmAveragedWithStdDevsUnconstrained["ParameterTable"][[1]][[1,-1,2]],\[Pi]*nlmAveragedWithStdDevsUnconstrained["ParameterTable"][[1]][[1,-1,3]]],", Parameters:",Quiet@nlmAveragedWithStdDevsUnconstrained["ParameterTable"],"*)
(**)
(*Result from the litterature (exact with SLE) \!\(\*TemplateBox[<|\"color\" -> RGBColor[0, 0, 1]|>,\n\"RGBColorSwatchTemplate\"]\): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Item::Closed:: *)
(*Errors obtained with StdDevEstimate[]*)


(* ::Input:: *)
(*fitFunc=a+c Exp[-\[Omega] x]+df x;*)
(*(*fitFunc=a+c Exp[- x]+df x;*)*)
(**)
(*nlmAveragedWithEstimatedStdDevs=NonlinearModelFit[logAveragedWithEstimatedStdDevs,{fitFunc,{(*-2<a<2,*)a<0,0.5<\[Omega]<=3,c<0(*,1<df<1.1*)}},{a,c,\[Omega](*{a,-0.5},{c,-30.},{\[Omega],2.}*),df(*,{df,1.083}*)},x,MaxIterations->1000];*)
(*nlmAveragedWithEstimatedStdDevsUnconstrained=NonlinearModelFit[logAveragedWithEstimatedStdDevs,{fitFunc,{(*-2<a<2,*)0.5<\[Omega]<=30(*,1<df<1.1*)}},{a,c,\[Omega](*{a,-0.5},{c,-30.},{\[Omega],2.}*),df(*,{df,1.083}*)},x,MaxIterations->1000];*)
(**)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@nlmAveragedWithEstimatedStdDevs*)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@nlmAveragedWithEstimatedStdDevsUnconstrained*)
(**)
(*nlmAveragedWithEstimatedStdDevsGlobal=NonlinearModelFit[logAveragedWithEstimatedStdDevs,{fitFunc,{(*-2<a<2,*)a<0,0.5<\[Omega]<=30,c<0(*,1<df<1.1*)}},{a,c,\[Omega](*{a,-0.5},{c,-30.},{\[Omega],2.}*),df(*,{df,1.083}*)},x,MaxIterations->1000,Method->"NMinimize"];*)
(*nlmAveragedWithEstimatedStdDevsUnconstrainedGlobal=NonlinearModelFit[logAveragedWithEstimatedStdDevs,{fitFunc,\[Omega]<=3},{a,c,\[Omega](*{a,-0.5},{c,-30.},{\[Omega],2.}*),df(*,{df,1.083}*)},x,MaxIterations->1000,Method->"NMinimize"];*)
(**)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@nlmAveragedWithEstimatedStdDevsGlobal*)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@nlmAveragedWithEstimatedStdDevsUnconstrainedGlobal*)
(**)


(* ::Input:: *)
(*synchronizedPlots=Map[Show[#,ImageSize->500]&,{ListPlot[nlmAveragedWithEstimatedStdDevs["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis nlmAveragedWithEstimatedStdDevs,\nAdjustedRSquared=",nlmAveragedWithEstimatedStdDevs ["AdjustedRSquared"]}]]*)
(*,ListPlot[nlmAveragedWithEstimatedStdDevsUnconstrained["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis nlmAveragedWithEstimatedStdDevsUnconstrained,\nAdjustedRSquared=",nlmAveragedWithEstimatedStdDevsUnconstrained["AdjustedRSquared"]}]]*)
(*,ListPlot[nlmAveragedWithEstimatedStdDevsGlobal["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis nlmAveragedWithEstimatedStdDevsGlobal,\nAdjustedRSquared=",nlmAveragedWithEstimatedStdDevsGlobal["AdjustedRSquared"]}]]*)
(*,ListPlot[nlmAveragedWithEstimatedStdDevsUnconstrainedGlobal["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis nlmAveragedWithEstimatedStdDevsUnconstrainedGlobal,\nAdjustedRSquared=",nlmAveragedWithEstimatedStdDevsUnconstrainedGlobal["AdjustedRSquared"]}]]}];*)
(**)
(*Multicolumn[synchronizedPlots,2,Appearance->"Framed"]*)
(**)


(* ::Input:: *)
(*maxx=Max[rawData[[All,1]]];*)
(*maxy=Max[rawData[[All,2]]];*)
(**)
(*thresholdBelow=0;*)
(*thresholdAbove=maxx-0;*)
(**)
(*Show[{ListPlot[logAveragedWithMaxDev,PlotStyle->{GrayLevel[0],Directive[Opacity[0.3]]},AxesLabel->{"Log[L]","Log[N]"}]*)
(*,ListPlot[logAveragedWithEstimatedStdDevs,PlotStyle->RGBColor[1, 0.47000000000000003`, 0],PlotLegends->{"logAveragedWithEstimatedStdDevs"}]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 0, 0.5],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@nlmAveragedWithEstimatedStdDevs[x]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 0.68, 0.5],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@nlmAveragedWithEstimatedStdDevsUnconstrained[x]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.85, 0, 0.5],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@nlmAveragedWithEstimatedStdDevsGlobal[x]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 0.68, 0.8],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@nlmAveragedWithEstimatedStdDevsUnconstrainedGlobal[x]*)
(**)
(*,Plot[x (dfSLE/.bb->b)-1,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0, 0, 1],Dashed}]*)
(*}*)
(*,PlotLabel->Row[{" b = ",b}]*)
(*,PlotRange->{All,{0,All}},AxesOrigin->{1,0},ImageSize->700]*)
(**)
(*Print[ "Averaged data fit with errors and " ,fitFunc," \!\(\*TemplateBox[<|\"color\" -> RGBColor[0.5, 0, 0.5]|>,\n\"RGBColorSwatchTemplate\"]\): \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@nlmAveragedWithEstimatedStdDevs["ParameterTable"][[1]][[1,-1,2]],\[Pi]*nlmAveragedWithEstimatedStdDevs["ParameterTable"][[1]][[1,-1,3]]],", Parameters:",Quiet@nlmAveragedWithEstimatedStdDevs["ParameterTable"],"*)
(*Averaged data fit with errors and " ,fitFunc," (unconstrained) \!\(\*TemplateBox[<|\"color\" -> RGBColor[0.5, 0.68, 0.5]|>,\n\"RGBColorSwatchTemplate\"]\): \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@nlmAveragedWithEstimatedStdDevsUnconstrained["ParameterTable"][[1]][[1,-1,2]],\[Pi]*nlmAveragedWithEstimatedStdDevsUnconstrained["ParameterTable"][[1]][[1,-1,3]]],", Parameters:",Quiet@nlmAveragedWithEstimatedStdDevsUnconstrained["ParameterTable"],"*)
(**)
(*Result from the litterature (exact with SLE) \!\(\*TemplateBox[<|\"color\" -> RGBColor[0, 0, 1]|>,\n\"RGBColorSwatchTemplate\"]\): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Item::Closed:: *)
(*Errors obtained with MaxDev[]*)


(* ::Input:: *)
(*fitFunc=a+c Exp[-\[Omega] x]+df x;*)
(*fitFunc=a+c Exp[- x]+df x;*)
(**)
(*nlmAveragedWithMaxStdDevs=NonlinearModelFit[logAveragedWithMaxDev,{fitFunc,{(*-2<a<2,*)a<0(*,0.5<\[Omega]<=3*),c<0(*,1<df<1.1*)}},{a,c,\[Omega](*{a,-0.5},{c,-30.},{\[Omega],2.}*),df(*,{df,1.083}*)},x,MaxIterations->10000];*)
(*nlmAveragedWithMaxStdDevsUnconstrained=NonlinearModelFit[logAveragedWithMaxDev,{fitFunc,{(*-2<a<2,*)(*,0.5<\[Omega]<=3*)(*,1<df<1.1*)}},{a,c,\[Omega](*{a,-0.5},{c,-30.},{\[Omega],2.}*),df(*,{df,1.083}*)},x,MaxIterations->10000];*)
(**)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@nlmAveragedWithMaxStdDevs*)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@nlmAveragedWithMaxStdDevsUnconstrained*)
(**)
(*nlmAveragedWithMaxStdDevsGlobal=NonlinearModelFit[logAveragedWithMaxDev,{fitFunc,{(*-2<a<2,*)a<0(*,0.5<\[Omega]<=3*),c<0(*,1<df<1.1*)}},{a,c,\[Omega](*{a,-0.5},{c,-30.},{\[Omega],2.}*),df(*,{df,1.083}*)},x,MaxIterations->1000,Method->"NMinimize"];*)
(*nlmAveragedWithMaxStdDevsUnconstrainedGlobal=NonlinearModelFit[logAveragedWithMaxDev,fitFunc,{a,c,\[Omega](*{a,-0.5},{c,-30.},{\[Omega],2.}*),df(*,{df,1.083}*)},x,MaxIterations->1000,Method->"NMinimize"];*)
(**)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@nlmAveragedWithMaxStdDevsGlobal*)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@nlmAveragedWithMaxStdDevsUnconstrainedGlobal*)
(**)


(* ::Input:: *)
(*synchronizedPlots=Map[Show[#,ImageSize->500]&,{ListPlot[nlmAveragedWithMaxStdDevs["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis nlmAveragedWithMaxStdDevs,\nAdjustedRSquared=",nlmAveragedWithMaxStdDevs ["AdjustedRSquared"]}]]*)
(*,ListPlot[nlmAveragedWithMaxStdDevsUnconstrained["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis nlmAveragedWithMaxStdDevsUnconstrained,\nAdjustedRSquared=",nlmAveragedWithMaxStdDevsUnconstrained["AdjustedRSquared"]}]]*)
(*,ListPlot[nlmAveragedWithMaxStdDevsGlobal["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis nlmAveragedWithMaxStdDevsGlobal,\nAdjustedRSquared=",nlmAveragedWithMaxStdDevsGlobal["AdjustedRSquared"]}]]*)
(*,ListPlot[nlmAveragedWithMaxStdDevsUnconstrainedGlobal["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis nlmAveragedWithMaxStdDevsUnconstrainedGlobal,\nAdjustedRSquared=",nlmAveragedWithMaxStdDevsUnconstrainedGlobal["AdjustedRSquared"]}]]}];*)
(**)
(*Multicolumn[synchronizedPlots,2,Appearance->"Framed"]*)
(**)


(* ::Input:: *)
(*maxx=Max[rawData[[All,1]]];*)
(*maxy=Max[rawData[[All,2]]];*)
(**)
(*thresholdBelow=0;*)
(*thresholdAbove=maxx-0;*)
(**)
(*Show[{ListPlot[logAveragedWithMaxDev,PlotStyle->{GrayLevel[0],Directive[Opacity[0.3]]},AxesLabel->{"Log[L]","Log[N]"}]*)
(*,ListPlot[logAveragedWithMaxDev,PlotStyle->RGBColor[1, 0.47000000000000003`, 0],PlotLegends->{"logAveragedWithMaxDev"}]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 0, 0.5],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@nlmAveragedWithMaxStdDevs[x]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 0.68, 0.5],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@nlmAveragedWithMaxStdDevsUnconstrained[x]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.85, 0, 0.5],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@nlmAveragedWithMaxStdDevsGlobal[x]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 0.68, 0.8],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@nlmAveragedWithMaxStdDevsUnconstrainedGlobal[x]*)
(**)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0, 0, 1],Dashed},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@(x (dfSLE/.bb->N[b])-1)*)
(*}*)
(*,PlotLabel->Row[{" b = ",b}]*)
(*,PlotRange->{All,{0,All}},AxesOrigin->{1,0},ImageSize->700]*)
(**)
(*Print[ "Averaged data fit with errors and " ,fitFunc," \!\(\*TemplateBox[<|\"color\" -> RGBColor[0.5, 0, 0.5]|>,\n\"RGBColorSwatchTemplate\"]\): \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@nlmAveragedWithMaxStdDevs["ParameterTable"][[1]][[1,-1,2]],\[Pi]*nlmAveragedWithMaxStdDevs["ParameterTable"][[1]][[1,-1,3]]],", Parameters:",Quiet@nlmAveragedWithMaxStdDevs["ParameterTable"],"*)
(*Averaged data fit with errors and " ,fitFunc," (unconstrained) \!\(\*TemplateBox[<|\"color\" -> RGBColor[0.5, 0.68, 0.5]|>,\n\"RGBColorSwatchTemplate\"]\): \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@nlmAveragedWithMaxStdDevsUnconstrained["ParameterTable"][[1]][[1,-1,2]],\[Pi]*nlmAveragedWithMaxStdDevsUnconstrained["ParameterTable"][[1]][[1,-1,3]]],", Parameters:",Quiet@nlmAveragedWithMaxStdDevsUnconstrained["ParameterTable"],"*)
(**)
(*Result from the litterature (exact with SLE) \!\(\*TemplateBox[<|\"color\" -> RGBColor[0, 0, 1]|>,\n\"RGBColorSwatchTemplate\"]\): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Subsubsection::Closed:: *)
(*Forgot what this is*)


(* ::Input:: *)
(*fitFuncs=Table[x^(-i),{i,-1,3}];*)
(*(*fitFuncs=Append[Table[x^(-i),{i,-1,0}],Exp[-x]];*)*)
(*(*fitFuncs={x,1,Exp[-x],Exp[-2 x]};(*Exp[-1.3 x]*)*)*)
(**)
(*fitFunc=a+c Exp[-\[Omega] x]+df x;*)
(**)
(*lmAveraged=LinearModelFit[logAveragedWithErrors,x,x(*,Weights->Automatic*)];*)
(*Print[Row[{"lmAveraged: ",#//Normal}]]&@%*)
(*lmMaxDev=LinearModelFit[logAveragedWithMaxDev,x,x(*,Weights->Automatic*)];*)
(*Print[Row[{"lmMaxDev: ",#//Normal}]]&@%*)
(**)
(**)
(*fitFuncs={x,1,Exp[-6x]};*)
(**)
(*lmpAveraged=LinearModelFit[logAveragedWithErrors,fitFuncs,x];*)
(*Print[Row[{"\nlmpAveraged: ",#//Normal}]]&@%*)
(*lmpMaxDev=LinearModelFit[logAveragedWithMaxDev,fitFuncs,x];*)
(*Print[Row[{"lmpMaxDev: ",#//Normal}]]&@%*)
(*lmpWithEstimatedStdDevs=LinearModelFit[logAveragedWithEstimatedStdDevs,fitFuncs,x];*)
(*Print[Row[{"lmpMaxDev: ",#//Normal}]]&@%*)
(**)
(*fitFuncs={x,1,Exp[-x]};*)
(**)
(*lmpAveraged=LinearModelFit[logAveragedWithErrors,fitFuncs,x];*)
(*Print[Row[{"\nlmpAveraged: ",#//Normal}]]&@%*)
(*lmpMaxDev=LinearModelFit[logAveragedWithMaxDev,fitFuncs,x];*)
(*Print[Row[{"lmpMaxDev: ",#//Normal}]]&@%*)
(*lmpWithEstimatedStdDevs=LinearModelFit[logAveragedWithEstimatedStdDevs,fitFuncs,x];*)
(*Print[Row[{"lmpMaxDev: ",#//Normal}]]&@%*)
(**)


(* ::Input:: *)
(*fitFunc=a+c Exp[-\[Omega] x]+c2 Exp[-\[Omega]2 x]+df x;*)
(**)
(*nlmAveraged=NonlinearModelFit[logAveragedWithErrors,{fitFunc,{-2<a<2,2.<=\[Omega]<=3.,0.1<=\[Omega]2<=1.}},{{a,-0.5},{c,-30.},\[Omega],c2,\[Omega]2,df(*{df,1.083}*)},x,MaxIterations->1000];*)
(*Print[Row[{"nlmAveraged2 with " ,fitFunc ": ",#//Normal}]]&@%*)
(**)
(*NonlinearModelFit[logAveragedWithMaxDev,{fitFunc(*,{-2<a<2,2.<=\[Omega]<=3.,0.1<=\[Omega]2<=1.}*)},{a,c,\[Omega],c2,\[Omega]2,df(*{df,1.083}*)},x,MaxIterations->1000];*)
(*Print[Row[{"nlmMaxDev without constraints: ",#//Normal}]]&@%*)
(**)
(**)
(*nlmMaxDev=NonlinearModelFit[logAveragedWithMaxDev,{fitFunc,{-2<a<2,2.<=\[Omega]<=3.,0.1<=\[Omega]2<=1.}},{{a,-0.5},{c,-30.},\[Omega],c2,\[Omega]2,df(*{df,1.083}*)},x,MaxIterations->1000];*)
(*Print[Row[{"nlmMaxDev: ",#//Normal}]]&@%*)
(**)
(*Print["Result from the litterature (exact with SLE) : ",(dfSLE/.bb->b)," = ",Style[(dfSLE/.bb->b/1.),RGBColor[0, 0, 1]]]*)


(* ::Input:: *)
(*(*Extract and plot residuals*)*)
(**)
(*nlmMaxDev["AdjustedRSquared"]*)
(*ListPlot[nlmMaxDev["FitResiduals"],Filling->Axis,PlotLabel->"Residual Analysis"]*)


(* ::Input:: *)
(*maxx=Max[rawData[[All,1]]];*)
(*maxy=Max[rawData[[All,2]]];*)
(**)
(*thresholdBelow=0;*)
(*thresholdAbove=maxx-0;*)
(**)
(*Show[{ListPlot[logAveragedWithMaxDev,PlotStyle->{GrayLevel[0],Directive[Opacity[0.3]]},AxesLabel->{"Log[L]","Log[N]"}]*)
(*,ListPlot[logAveragedWithEstimatedStdDevs,PlotStyle->RGBColor[1, 0.55, 1]]*)
(*,ListPlot[logAveragedWithErrors,PlotStyle->{RGBColor[0, 1, 1],Directive[Opacity[0.3]]}]*)
(*(*,Plot[lmp[x],{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{,Thickness->0.004}]*)*)
(*,Plot[lmAveraged[x],{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[1, 0, 0],Thickness->0.004}]*)
(*,Plot[lmpAveraged[x],{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 0, 0.5],Thickness->0.004}]*)
(*,Plot[fitFunc/. fitSol,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[1, Rational[2, 3], 1],Thickness->0.004}]*)
(*,Plot[nlmAveraged[x],{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 1, 0.5],Thickness->0.004}]*)
(**)
(*,Plot[nlmMaxDev[x],{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.76, 0.63, 0.19],Thickness->0.004}]*)
(**)
(*,Plot[x (dfSLE/.bb->b)-1,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0, 0, 1],Dashed}]}*)
(*,(*PlotRange->All,*)PlotLabel->Row[{" b = ",b}]*)
(*(*,Epilog->{Directive[Dashed,],*)
(*Line[{{Log[thresholdBelow],0},{Log[thresholdBelow],Log[maxy]//N}}]*)
(*,Line[{{Log[thresholdAbove],0},{Log[thresholdAbove],Log[maxy]//N}}]}*)*)
(*,PlotRange->{{7,7.5},{6.5,All}},AxesOrigin->{1,0}]*)
(**)
(**)
(**)
(*Print[ (*"Full data - Linear fit : Subscript[d, f]=",Around[Quiet@lm["ParameterTable"][[1]][[1,3,2]],\[Pi]*lm["ParameterErrors"][[2]]],*)"*)
(*Averaged data fit with errors \!\(\*TemplateBox[<|\"color\" -> RGBColor[1, 0, 0]|>,\n\"RGBColorSwatchTemplate\"]\): \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lmAveraged["ParameterTable"][[1]][[1,3,2]],\[Pi]*lmAveraged["ParameterErrors"][[2]]],"*)
(*Averaged data fit with errors and " ,Total@fitFuncs," \!\(\*TemplateBox[<|\"color\" -> RGBColor[0.5, 0, 0.5]|>,\n\"RGBColorSwatchTemplate\"]\): \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lmpAveraged["ParameterTable"][[1]][[1,2,2]],\[Pi]*lmpAveraged["ParameterErrors"][[1]]],"*)
(**)
(*",fitFunc,"  fit  \!\(\*TemplateBox[<|\"color\" -> RGBColor[1, Rational[2, 3], 1]|>,\n\"RGBColorSwatchTemplate\"]\): \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[df/.fitSol,Null],", Parameters:",fitSol,"*)
(*Averaged data fit with errors and " ,fitFunc,"nonlinearModel \!\(\*TemplateBox[<|\"color\" -> RGBColor[0.5, 1, 0.5]|>,\n\"RGBColorSwatchTemplate\"]\): \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@nlmAveraged["ParameterTable"][[1,1,5,2]],\[Pi]*Quiet@nlmAveraged["ParameterTable"][[1,1,5,3]]],"*)
(**)
(*Averaged data fit with MaxDevs and " ,fitFunc,"nonlinearModel \!\(\*TemplateBox[<|\"color\" -> RGBColor[0.76, 0.63, 0.19]|>,\n\"RGBColorSwatchTemplate\"]\): \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@nlmMaxDev["ParameterTable"][[1,1,-1,2]],(*\[Pi]**)Quiet@nlmMaxDev["ParameterTable"][[1,1,-1,3]]],", Parameters:",Quiet@nlmMaxDev["ParameterTable"],(*,"*)
(**)
(*Full data - ",Total@fitFuncs," fit: Subscript[d, f]=",Around[Quiet@lmp["ParameterTable"][[1]][[1,2,2]],\[Pi]*lmp["ParameterErrors"][[1]]]*)"*)
(**)
(*Result from the litterature (exact with SLE) \!\(\*TemplateBox[<|\"color\" -> RGBColor[0, 0, 1]|>,\n\"RGBColorSwatchTemplate\"]\): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Input:: *)
(*Quiet@lmpAveraged["ParameterTable"][[1]]*)
(*Quiet@nlmAveraged["ParameterTable"][[1]]*)


(* ::Input:: *)
(*Quiet@nlmMaxDev["ParameterTable"][[1,All,1;;3]]//Normal*)


(* ::Subsection::Closed:: *)
(*Scan through different shifts to find the best linear fit, i.e. that maximizes AdjustedRSquared*)
(*	TBD*)


(* ::Subsubsection::Closed:: *)
(*With errors from StandardDeviation[]*)


(* ::Input:: *)
(*shift:=Plus[{-4,0},#]&;*)
(**)
(*logAveragedWithErrorsShifted=Log[shift/@averagedWithErrors]/. {a_,0}:>{a,Around[1.0`*^-6,1.0`*^-6]};*)
(*fitFunc=a+df  x;lmAveragedWithErrorsShifted=NonlinearModelFit[Select[logAveragedWithErrorsShifted,#[[1]]>=0&],{fitFunc(*,{-2<a<2,0<\[Omega]<28}*)},{a,(*c,\[Omega],*)df},x]*)


(* ::Input:: *)
(*listOfFits=Table[*)
(*shift:=Plus[{-i,0},#]&;*)
(**)
(*logAveragedWithErrorsShifted=Log[shift/@averagedWithErrors]/. {a_,0}:>{a,Around[1.0`*^-6,1.0`*^-6]};*)
(*fitFunc=a+df  x;lmAveragedWithErrorsShifted=NonlinearModelFit[Select[logAveragedWithErrorsShifted,#[[1]]>=0&],{fitFunc(*,{-2<a<2,0<\[Omega]<28}*)},{a,(*c,\[Omega],*)df},x]*)
(*,*)
(*{i,-5,5,0.1}];*)


(* ::Input:: *)
(*listOfFits[[50;;60]]*)


(* ::Input:: *)
(*Length[listOfFits]*)


(* ::Input:: *)
(*#["AdjustedRSquared"]&/@listOfFits*)
(*best=Ordering[%,-1]*)
(*listOfFits[[%]]*)


(* ::Input:: *)
(*(best[[1]]-51)/10//N;*)
(*Print["Best position is with shift ",%]*)
(*shift:=Plus[{-(best[[1]]-51)/10//N,0},#]&*)
(**)
(*logAveragedWithMaxDevShifted=Log[shift/@averagedWithMaxDev]/.{a_,0}->{a,Around[1.0`*^-6,1.0`*^-6]};*)
(*logAveragedWithErrorsShifted=Log[shift/@averagedWithErrors]/. {a_,0}->{a,Around[1.0`*^-6,1.0`*^-6]};*)
(**)
(**)
(*Show[{ListPlot[logAveragedWithMaxDevShifted,PlotStyle->{GrayLevel[0],Directive[Opacity[0.3]]},AxesLabel->{Row[{"Log[L-",(best[[1]]-51)/10//N,"]"}],"Log[N]"}]*)
(*,ListPlot[logAveragedWithErrorsShifted,PlotStyle->RGBColor[0, 1, 1]]*)
(*(*,ListPlot[logAveragedWithEstimatedStdDevsShifted,PlotStyle->{,Directive[Opacity[0.3]]}]*)*)
(**)
(*(*,Plot[lmp[x],{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{,Thickness->0.004}]*)*)
(**)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 0, 0.5],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],{Right,Top}]]&@(listOfFits[[best[[1]]]][x])*)
(**)
(*,Plot[x (dfSLE/.bb->b)-1,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0, 0, 1],Dashed}]}*)
(*,(*PlotRange->All,*)PlotLabel->Row[{" b = ",b}]*)
(*(*,Epilog->{Directive[Dashed,],*)
(*Line[{{Log[thresholdBelow],0},{Log[thresholdBelow],Log[maxy]//N}}]*)
(*,Line[{{Log[thresholdAbove],0},{Log[thresholdAbove],Log[maxy]//N}}]}*)*)
(*,PlotRange->{All,{0,All}},AxesOrigin->{1,0}]*)
(**)
(*ListPlot[listOfFits[[best[[1]]]]["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis, AdjustedRSquared=",listOfFits[[best[[1]]]]["AdjustedRSquared"]}]]*)
(**)


(* ::Subsubsection::Closed:: *)
(*With errors from MaxDev[]*)


(* ::Input:: *)
(*listOfFits=Table[*)
(*shift:=Plus[{-i,0},#]&;*)
(**)
(*logAveragedWithMaxDevShifted=Log[shift/@averagedWithErrors]/. {a_,0}->{a,Around[1.0`*^-6,1.0`*^-6]};*)
(*fitFunc=a+df  x;lmAveragedWithMaxDevShifted=NonlinearModelFit[Select[logAveragedWithMaxDevShifted,#[[1]]>0&],{fitFunc(*,{-2<a<2,0<\[Omega]<28}*)},{a,(*c,\[Omega],*)df},x]*)
(*,*)
(*{i,-5,5,0.1}];*)


(* ::Input:: *)
(*Length[listOfFits]*)


(* ::Input:: *)
(*#["AdjustedRSquared"]&/@listOfFits*)
(*best=Ordering[%,-1]*)
(*listOfFits[[%]]*)


(* ::Input:: *)
(*(best[[1]]-51)/10//N;*)
(*Print["Best position is with shift ",%]*)
(*shift:=Plus[{-(best[[1]]-51)/10//N,0},#]&*)
(**)
(*logAveragedWithMaxDevShifted=Log[shift/@averagedWithMaxDev]/.{a_,0}->{a,Around[1.0`*^-6,1.0`*^-6]};*)
(*logAveragedWithErrorsShifted=Log[shift/@averagedWithErrors]/. {a_,0}->{a,Around[1.0`*^-6,1.0`*^-6]};*)
(**)
(**)
(*Show[{ListPlot[logAveragedWithMaxDevShifted,PlotStyle->{GrayLevel[0],Directive[Opacity[0.3]]},AxesLabel->{Row[{"Log[L-",(best[[1]]-51)/10//N,"]"}],"Log[N]"}]*)
(*,ListPlot[logAveragedWithErrorsShifted,PlotStyle->RGBColor[0, 1, 1]]*)
(*(*,ListPlot[logAveragedWithEstimatedStdDevsShifted,PlotStyle->{,Directive[Opacity[0.3]]}]*)*)
(**)
(*(*,Plot[lmp[x],{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{,Thickness->0.004}]*)*)
(**)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 0, 0.5],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],{Right,Top}]]&@(listOfFits[[best[[1]]]][x])*)
(**)
(*,Plot[x (dfSLE/.bb->b)-1,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0, 0, 1],Dashed}]}*)
(*,(*PlotRange->All,*)PlotLabel->Row[{" b = ",b}]*)
(*(*,Epilog->{Directive[Dashed,],*)
(*Line[{{Log[thresholdBelow],0},{Log[thresholdBelow],Log[maxy]//N}}]*)
(*,Line[{{Log[thresholdAbove],0},{Log[thresholdAbove],Log[maxy]//N}}]}*)*)
(*,PlotRange->{All,{0,All}},AxesOrigin->{1,0}]*)
(**)
(*ListPlot[listOfFits[[best[[1]]]]["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis, AdjustedRSquared=",listOfFits[[best[[1]]]]["AdjustedRSquared"]}]]*)
(**)


(* ::Subsection::Closed:: *)
(*Take the Log WITH A SHIFT*)


(* ::Input:: *)
(*shift:=Plus[{-4,0},#]&; (*In the code, the stopping condition is with R-1*)*)
(**)
(*logAveragedShifted=Log[shift/@averaged];*)
(*logAveragedWithErrorsShifted=Log[shift/@averagedWithErrors]/. {a_,0}->{a,Around[1.0`*^-6,1.0`*^-6]};*)
(*logAveragedWithEstimatedStdDevsShifted=Log[shift/@averagedWithEstimatedStdDevs]/. {a_,0}->{a,Around[1.0`*^-6,1.0`*^-6]};*)
(*logAveragedWithMaxDevShifted=Log[shift/@averagedWithMaxDev]/.{a_,0}->{a,Around[1.0`*^-6,1.0`*^-6]};*)


(* ::Input:: *)
(*fitFunc=a+df x;*)
(*lmAveragedShifted=NonlinearModelFit[logAveragedShifted,{fitFunc(*,{-2<a<2,0<\[Omega]<28}*)},{a,(*c,\[Omega],*)df},x];*)
(*lmAveragedWithErrorsShifted=NonlinearModelFit[Select[logAveragedWithErrorsShifted,#[[1]]>0&],{fitFunc(*,{-2<a<2,0<\[Omega]<28}*)},{a,(*c,\[Omega],*)df},x];*)
(*(*lmAveragedWithEstimatedStdDevsShifted=NonlinearModelFit[Select[logAveragedWithEstimatedStdDevsShifted,#[[1]]>0&],{fitFunc(*,{-2<a<2,0<\[Omega]<28}*)},{a,(*c,\[Omega],*)df},x];*)*)
(*lmAveragedWithMaxDevShifted=NonlinearModelFit[Select[logAveragedWithMaxDevShifted,#[[1]]>0&],{fitFunc(*,{-2<a<2,0<\[Omega]<28}*)},{a,(*c,\[Omega],*)df},x];*)
(**)
(**)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@lmAveragedShifted*)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@lmAveragedWithErrorsShifted *)
(*(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@lmAveragedWithEstimatedStdDevsShifted *)*)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@lmAveragedWithMaxDevShifted*)


(* ::Input:: *)
(*synchronizedPlots=Map[Show[#,ImageSize->500]&,{ListPlot[lmAveragedShifted["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis, AdjustedRSquared=",lmAveragedShifted["AdjustedRSquared"]}]]*)
(*,ListPlot[lmAveragedWithErrorsShifted["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis, AdjustedRSquared=",lmAveragedWithErrorsShifted["AdjustedRSquared"]}]]*)
(*(*,ListPlot[lmAveragedWithEstimatedStdDevsShifted["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis, AdjustedRSquared=",lmAveragedWithEstimatedStdDevsShifted["AdjustedRSquared"]}]]*)*)
(*,ListPlot[lmAveragedWithMaxDevShifted["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis, AdjustedRSquared=",lmAveragedWithMaxDevShifted["AdjustedRSquared"]}]]}];*)
(**)
(*Multicolumn[synchronizedPlots,2,Appearance->"Framed"]*)
(**)


(* ::Input:: *)
(*maxx=Max[rawData[[All,1]]];*)
(*maxy=Max[rawData[[All,2]]];*)
(**)
(*thresholdBelow=0;*)
(*thresholdAbove=maxx-0;*)
(**)
(*Show[{ListPlot[logAveragedWithMaxDevShifted,PlotStyle->{GrayLevel[0],Directive[Opacity[0.3]]},AxesLabel->{"Log[L-4]","Log[N]"}]*)
(*,(*ListPlot[logAveragedWithErrorsShifted,PlotStyle->]*)ListPlot[logAveragedWithErrorsShifted,PlotStyle->RGBColor[0, 0.78, 1],PlotLegends->{"logAveragedWithErrorsShifted"}]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 0.68, 0.5],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@lmAveragedShifted[x]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.85, 0, 0.5],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@lmAveragedWithErrorsShifted[x]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0, 0, 1],Dashed},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@(x (dfSLE/.bb->N[b])-1)}*)
(*,(*PlotRange->All,*)PlotLabel->Row[{" b = ",b}]*)
(*(*,Epilog->{Directive[Dashed,],*)
(*Line[{{Log[thresholdBelow],0},{Log[thresholdBelow],Log[maxy]//N}}]*)
(*,Line[{{Log[thresholdAbove],0},{Log[thresholdAbove],Log[maxy]//N}}]}*)*)
(*,PlotRange->{{1,All},{0,All}},AxesOrigin->{1,0},ImageSize->Large]*)


(* ::Subsubsection::Closed:: *)
(*Drop some*)


(* ::Input:: *)
(*maxx=Max[rawData[[All,1]]];*)
(*maxy=Max[rawData[[All,2]]];*)
(**)
(*thresholdBelow=70;*)
(*thresholdAbove=maxx-0;*)
(**)
(**)
(**)
(*fitFunc=a+df x;*)
(**)
(*lmAveragedShiftedDropped=NonlinearModelFit[Select[logAveragedShifted,#[[1]]>Log[thresholdBelow]&],{fitFunc(*,{-2<a<2,0<\[Omega]<28}*)},{a,(*c,\[Omega],*)df},x,Method->"NMinimize"];*)
(*lmAveragedWithErrorsShiftedDropped=NonlinearModelFit[Select[logAveragedWithErrorsShifted,#[[1]]>Log[thresholdBelow]&],{fitFunc(*,{-2<a<2,0<\[Omega]<28}*)},{a,(*c,\[Omega],*)df},x,Method->"NMinimize"];*)
(*(*lmAveragedWithEstimatedStdDevsShifted=NonlinearModelFit[Select[logAveragedWithEstimatedStdDevsShifted,#[[1]]>0&],{fitFunc(*,{-2<a<2,0<\[Omega]<28}*)},{a,(*c,\[Omega],*)df},x];*)*)
(*lmAveragedWithMaxDevShiftedDropped=NonlinearModelFit[Select[logAveragedWithMaxDevShifted,#[[1]]>Log[thresholdBelow]&],{fitFunc(*,{-2<a<2,0<\[Omega]<28}*)},{a,(*c,\[Omega],*)df},x,Method->"NMinimize"];*)
(**)
(**)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@lmAveragedShiftedDropped*)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@lmAveragedWithErrorsShiftedDropped *)
(*(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@lmAveragedWithEstimatedStdDevsShiftedDropped *)*)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@lmAveragedWithMaxDevShiftedDropped*)


(* ::Input:: *)
(*synchronizedPlots=Map[Show[#,ImageSize->500]&,{ListPlot[lmAveragedShiftedDropped["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis, AdjustedRSquared=",lmAveragedShiftedDropped["AdjustedRSquared"]}]]*)
(*,ListPlot[lmAveragedWithErrorsShiftedDropped["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis, AdjustedRSquared=",lmAveragedWithErrorsShiftedDropped["AdjustedRSquared"]}]]*)
(*(*,ListPlot[lmAveragedWithEstimatedStdDevsShiftedDropped["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis, AdjustedRSquared=",lmAveragedWithEstimatedStdDevsShiftedDropped["AdjustedRSquared"]}]]*)*)
(*,ListPlot[lmAveragedWithMaxDevShiftedDropped["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis, AdjustedRSquared=",lmAveragedWithMaxDevShiftedDropped["AdjustedRSquared"]}]]}];*)
(**)
(*Multicolumn[synchronizedPlots,2,Appearance->"Framed"]*)
(**)


(* ::Input:: *)
(*Show[{ListPlot[logAveragedWithMaxDevShifted,PlotStyle->{GrayLevel[0],Directive[Opacity[0.3]]},AxesLabel->{"Log[L-4]","Log[N]"}]*)
(*,(*ListPlot[logAveragedWithErrorsShifted,PlotStyle->]*)ListPlot[logAveragedWithErrorsShifted,PlotStyle->RGBColor[0, 0.78, 1],PlotLegends->{"logAveragedWithErrorsShifted"}]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 0.68, 0.5],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@lmAveragedShiftedDropped[x]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.85, 0, 0.5],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@lmAveragedWithErrorsShiftedDropped[x]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0, 0, 1],Dashed},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@(x (dfSLE/.bb->N[b])-1)}*)
(*,(*PlotRange->All,*)PlotLabel->Row[{" b = ",b}]*)
(*(*,Epilog->{Directive[Dashed,],*)
(*Line[{{Log[thresholdBelow],0},{Log[thresholdBelow],Log[maxy]//N}}]*)
(*,Line[{{Log[thresholdAbove],0},{Log[thresholdAbove],Log[maxy]//N}}]}*)*)
(*,PlotRange->{{1,All},{0,All}},AxesOrigin->{1,0},ImageSize->Large]*)


(* ::Subsubsection::Closed:: *)
(*Check with enforced Minimum*)


(* ::Input::Closed:: *)
(*fitFunc=a+df x;*)
(*lmAveragedShiftedGlobal=NonlinearModelFit[logAveragedShifted,{fitFunc(*,{-2<a<2,0<\[Omega]<28}*)},{a,(*c,\[Omega],*)df},x,Method->"NMinimize"];*)
(*lmAveragedWithErrorsShiftedGlobal=NonlinearModelFit[Select[logAveragedWithErrorsShifted,#[[1]]>0&],{fitFunc(*,{-2<a<2,0<\[Omega]<28}*)},{a,(*c,\[Omega],*)df},x,Method->"NMinimize"];*)
(*lmAveragedWithEstimatedStdDevsShiftedGlobal=NonlinearModelFit[Select[logAveragedWithEstimatedStdDevsShifted,#[[1]]>0&],{fitFunc(*,{-2<a<2,0<\[Omega]<28}*)},{a,(*c,\[Omega],*)df},x,Method->"NMinimize"];*)
(*lmAveragedWithMaxDevShiftedGlobal=NonlinearModelFit[Select[logAveragedWithMaxDevShifted,#[[1]]>0&],{fitFunc(*,{-2<a<2,0<\[Omega]<28}*)},{a,(*c,\[Omega],*)df},x,Method->"NMinimize"];*)
(**)
(**)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@lmAveragedShiftedGlobal*)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@lmAveragedWithErrorsShiftedGlobal *)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@lmAveragedWithEstimatedStdDevsShiftedGlobal *)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@lmAveragedWithMaxDevShiftedGlobal*)
(**)
(*Print[]*)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@lmAveragedShifted*)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@lmAveragedWithErrorsShifted *)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@lmAveragedWithEstimatedStdDevsShifted *)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@lmAveragedWithMaxDevShifted*)


(* ::Print:: *)
(*Row[{"lmAveragedWithEstimatedStdDevsShiftedGlobal"," with ",a+df x,": ",-0.412897+1.03982 x}]*)


(* ::Print:: *)
(*Row[{"lmAveragedWithEstimatedStdDevsShifted"," with ",a+df x,": ",-0.412908+1.03982 x}]*)


(* ::Input:: *)
(*synchronizedPlots=Map[Show[#,ImageSize->500]&,{ListPlot[lmAveragedShifted["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis, AdjustedRSquared=",lmAveragedShifted["AdjustedRSquared"]}]]*)
(*,ListPlot[lmAveragedWithErrorsShifted["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis, AdjustedRSquared=",lmAveragedWithErrorsShifted["AdjustedRSquared"]}]]*)
(*,ListPlot[lmAveragedWithEstimatedStdDevsShifted["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis, AdjustedRSquared=",lmAveragedWithEstimatedStdDevsShifted["AdjustedRSquared"]}]]*)
(*,ListPlot[lmAveragedWithMaxDevShifted["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis, AdjustedRSquared=",lmAveragedWithMaxDevShifted["AdjustedRSquared"]}]]}];*)
(**)
(*Multicolumn[synchronizedPlots,2,Appearance->"Framed"]*)
(**)


(* ::Subsubsection::Closed:: *)
(*\[Chi]^2 with analytical b (from SLE)*)


(* ::Input:: *)
(*dfSLE/.bb->b*)


(* ::Input::Closed:: *)
(*fitFunc=a+(dfSLE/.bb->b) x;*)
(*lmAveragedShiftedSLE=NonlinearModelFit[logAveragedShifted,{fitFunc(*,{-2<a<2,0<\[Omega]<28}*)},{a,(*c,\[Omega],*)df},x,Method->"NMinimize"];*)
(*lmAveragedWithErrorsShiftedSLE=NonlinearModelFit[Select[logAveragedWithErrorsShifted,#[[1]]>0&],{fitFunc(*,{-2<a<2,0<\[Omega]<28}*)},{a,(*c,\[Omega],*)df},x,Method->"NMinimize"];*)
(*lmAveragedWithEstimatedStdDevsShiftedSLE=NonlinearModelFit[Select[logAveragedWithEstimatedStdDevsShifted,#[[1]]>0&],{fitFunc(*,{-2<a<2,0<\[Omega]<28}*)},{a,(*c,\[Omega],*)df},x,Method->"NMinimize"];*)
(*lmAveragedWithMaxDevShiftedSLE=NonlinearModelFit[Select[logAveragedWithMaxDevShifted,#[[1]]>0&],{fitFunc(*,{-2<a<2,0<\[Omega]<28}*)},{a,(*c,\[Omega],*)df},x,Method->"NMinimize"];*)
(**)
(**)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@lmAveragedShiftedSLE*)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@lmAveragedWithErrorsShiftedSLE *)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@lmAveragedWithEstimatedStdDevsShiftedSLE *)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@lmAveragedWithMaxDevShiftedSLE*)
(**)


(* ::Print:: *)
(*Row[{"lmAveragedWithEstimatedStdDevsShiftedSLE"," with ",a+(13 x)/12,": ",-0.713206+(13 x)/12}]*)


(* ::Input:: *)
(*synchronizedPlots=Map[Show[#,ImageSize->500]&,{ListPlot[lmAveragedShiftedSLE["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis, AdjustedRSquared=",lmAveragedShifted["AdjustedRSquared"]}]]*)
(*,ListPlot[lmAveragedWithErrorsShiftedSLE["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis, AdjustedRSquared=",lmAveragedWithErrorsShifted["AdjustedRSquared"]}]]*)
(*,ListPlot[lmAveragedWithEstimatedStdDevsShiftedSLE["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis, AdjustedRSquared=",lmAveragedWithEstimatedStdDevsShifted["AdjustedRSquared"]}]]*)
(*,ListPlot[lmAveragedWithMaxDevShiftedSLE["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis, AdjustedRSquared=",lmAveragedWithMaxDevShifted["AdjustedRSquared"]}]]}];*)
(**)
(*Multicolumn[synchronizedPlots,2,Appearance->"Framed"]*)
(**)


(* ::Input:: *)
(*Show[{ListPlot[logAveragedWithMaxDevShifted,PlotStyle->{GrayLevel[0],Directive[Opacity[0.3]]},AxesLabel->{Row[{"Log[L-",(best[[1]]-51)/10//N,"]"}],"Log[N]"}]*)
(*,ListPlot[logAveragedWithErrorsShifted,PlotStyle->{RGBColor[0, 1, 1],PointSize->0.0003}]*)
(*(*,ListPlot[logAveragedWithEstimatedStdDevsShifted,PlotStyle->{,Directive[Opacity[0.3]]}]*)*)
(**)
(*(*,Plot[lmp[x],{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{,Thickness->0.004}]*)*)
(**)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 0, 0.5],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],{Right,Top}]]&@(lmAveragedShiftedSLE[x])*)
(**)
(*,Plot[x (dfSLE/.bb->b)-1,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0, 0, 1],Dashed}]}*)
(*,(*PlotRange->All,*)PlotLabel->Row[{" b = ",b}]*)
(*(*,Epilog->{Directive[Dashed,],*)
(*Line[{{Log[thresholdBelow],0},{Log[thresholdBelow],Log[maxy]//N}}]*)
(*,Line[{{Log[thresholdAbove],0},{Log[thresholdAbove],Log[maxy]//N}}]}*)*)
(*,PlotRange->{All,{0,All}},AxesOrigin->{1,0}]*)


(* ::Subsection::Closed:: *)
(*Extra analysis			*)


(* ::Subsubsection::Closed:: *)
(*Plot with different drops below*)


(* ::Input:: *)
(**)
(*thresholdAbove=maxx-450;*)
(*dfDropped=ParallelTable[With[{*)
(*lmdropped=LinearModelFit[Select[logAveragedWithErrorsOnMean,Log[i]<=#[[1]]<Log[thresholdAbove]&&!(6.6<#[[1]]<6.75)&],x,x,Weights->Automatic]},*)
(*{i,Quiet@lmdropped["ParameterTable"][[1]][[1,3,2]]}],{i,5,500}];*)


(* ::Input:: *)
(*dfDropped[[15;;30]]*)


(* ::Input:: *)
(**)
(*lmDrops=LinearModelFit[DeleteCases[dfDropped,{x_,_}/;(x<0)],{1},x];*)
(**)
(*Show[*)
(*{ListPlot[dfDropped,PlotRange->{All,All}]*)
(*(*,Plot[fit[x],{x,0,1000},PlotStyle->Red]*)*)
(*,Plot[dfSLE/.bb->b/1.,{x,0,1000},PlotStyle->RGBColor[0, 0, 1]]*)
(*,Plot[lmDrops[x],{x,0,1000},PlotStyle->RGBColor[0, Rational[2, 3], 0]]*)
(*}*)
(*,AxesLabel->{"Dropping threshold",Subscript[d, f]},PlotRange->{All,{1.1,1.42}}]*)


(* ::Input:: *)
(*dfTogether[[20;;30]]*)


(* ::Subsubsection::Closed:: *)
(*Plot with different drops Above BAD		TBD*)


(* ::Input:: *)
(*thresholdBelow=200;(*Fix this*)*)
(**)
(*dfDroppedAbove=Table[With[{*)
(*lmdropped=LinearModelFit[Log[DeleteCases[rawData,{a_,_}/;(a<thresholdBelow ||  a>(maxx-i))]],x,x]},*)
(*{i,Quiet@lmdropped["ParameterTable"][[1]][[1,3,2]]}],{i,0,1500}];*)


(* ::Input:: *)
(*fitFunc=a+d Exp[c x];*)
(*fit=FindFit[dfDroppedAbove,fitFunc,{a,c,d},x]*)
(**)
(*Show[*)
(*{ListPlot[dfDroppedAbove,PlotRange->{All,All}]*)
(*,Plot[fitFunc/.fit,{x,2,1500},PlotStyle->Red]*)
(*,Plot[dfSLE/.bb->b/1.,{x,0,1000},PlotStyle->Blue]*)
(*},PlotRange->{All,All}]*)


(* ::Input:: *)
(*ListPlot[dfDroppedAbove,PlotRange->{All,All}]*)


(* ::Subsubsection::Closed:: *)
(*Plot with different POSITIONS of the same WINDOW:   (i)<  a < (i + 500)		TBD*)


(* ::Input:: *)
(*window=500;(*Set this*)*)
(**)
(*dfDroppedWindow=ParallelTable[With[{*)
(*lmdropped=LinearModelFit[Select[logAveragedWithErrors,Log[i]<#[[1]]<Log[window+i]&],x,x]},*)
(*{i,Quiet@lmdropped["ParameterTable"][[1]][[1,3,2]]}],{i,0,maxx-window}];*)


(* ::Input:: *)
(*Show[*)
(*{ListPlot[dfDroppedWindow,PlotRange->{All,All}]*)
(*,Plot[dfSLE/.bb->b/1.,{x,0,1500},PlotStyle->RGBColor[0, 0, 1],PlotLegends->SwatchLegend[{"SLE"}]]*)
(*},PlotRange->{All,All},PlotLabel->Row[{"Moving window of size ",window}],AxesLabel->{"Window position",Subscript[d, f]},ImageSize->500]*)


(* ::Subsubsection::Closed:: *)
(*Plot with different POSITIONS of the same WINDOW:  Changing window 		TBD*)


(* ::Input:: *)
(**)


(* ::Input:: *)
(*windowPlots=Table[*)
(*dfDroppedWindow={window,ParallelTable[With[{*)
(*lmdropped=LinearModelFit[Select[logAveragedWithErrors,Log[i]<#[[1]]<Log[window+i]&],x,x]},*)
(*{i,Around[Quiet@lmdropped["ParameterTable"][[1]][[1,3,2]],\[Pi]*lmdropped["ParameterErrors"][[2]]]}],{i,0,maxx-window,20}]}*)
(*,{window,100,1000,50}];*)


(* ::Input:: *)
(*windowPlots[[1]]*)


(* ::Input:: *)
(*showWindowPlots=Show[*)
(*{ListPlot[#[[2]],PlotRange->{All,All}]*)
(*,Plot[dfSLE/.bb->b/1.,{x,0,2000-window},PlotStyle->RGBColor[0, 0, 1]]*)
(*},PlotRange->{All,{0.9,1.1}},PlotLabel->Row[{"Window size = ",#[[1]]}]]&/@windowPlots*)


(* ::Input:: *)
(*synchronizedPlots=Map[Show[#,PlotRange->{All,{0.99,1.05}},ImageSize->220]&,showWindowPlots];*)
(**)
(*Multicolumn[synchronizedPlots,4,Appearance->"Framed"]*)


(* ::Input:: *)
(*Map[Show[#,PlotRange->{All,{1,1.005}},ImageSize->280]&,showWindowPlots[[6;;8]]];*)
(*Multicolumn[%,3,Appearance->"Framed"]*)


(* ::Subsection::Closed:: *)
(*Just two: df=(Log(n)-Log(n'))/(Log(L)-Log(L'))*)


(* ::Input:: *)
(*maxx=Max[rawData[[All,1]]];*)
(*maxy=Max[rawData[[All,2]]];*)
(**)
(*thresholdBelow=25;*)
(*thresholdAbove=maxx-0*500;*)
(**)
(*droppedWithMaxDev=Select[logAveragedWithMaxDev,Log[thresholdBelow]<#[[1]]<Log[thresholdAbove]&];*)
(*droppedWithErrors=Select[logAveragedWithErrors,Log[thresholdBelow]<#[[1]]<Log[thresholdAbove]&];*)
(**)
(**)
(*lmdroppedWithMaxDev=LinearModelFit[droppedWithMaxDev,x,x,Weights->Automatic];*)
(*lmdroppedWithErrors=NonlinearModelFit[droppedWithErrors,a+df x,{a,df},x,Weights->Automatic(*,Method->"NMinimize"*)]*)
(*(*lmpdropped=LinearModelFit[dropped,fitFuncs,x];*)*)
(**)
(*Show[{ListPlot[logAveragedWithMaxDev,PlotStyle->{GrayLevel[0],Directive[Opacity[0.3]]},AxesLabel->{"Log[L]","Log[N]"}]*)
(*,ListPlot[logAveragedWithErrors,PlotStyle->RGBColor[0, 0.78, 1],PlotLegends->{"logAveragedWithErrors"}]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 0, 0.5],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@lmdroppedWithMaxDev[x]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[1, 0.68, 0.6],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@lmdroppedWithErrors[x]*)
(*(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{,Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@lmAveragedWithStdDevs[x]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{,Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@lmAveragedWithStdDevsGlobal[x]*)*)
(**)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0, 0, 1],Dashed},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@(x (dfSLE/.bb->N[b])-1)*)
(*,Plot[#,{x,0,Log[thresholdAbove]},PlotStyle->{RGBColor[0, 1, 1],Dashed},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@(x (dfSLE/.bb->N[b])-0.7-5Exp[-0.991 x])*)
(*}*)
(*,Epilog->{Directive[Dashed,GrayLevel[0.5]],*)
(*Line[{{Log[thresholdBelow],0},{Log[thresholdBelow],Log[maxy]//N}}]*)
(*,Line[{{Log[thresholdAbove],0},{Log[thresholdAbove],Log[maxy]//N}}]}*)
(*,PlotLabel->Row[{" b = ",b}]*)
(*,PlotRange->{All,{0,All}}(*PlotRange->{{Log[thresholdBelow],Log[maxy]},{4,7.2}}*),AxesOrigin->{(*Log[thresholdBelow]*)1,0},ImageSize->700]*)


(* ::Input:: *)
(*Sorted=Sort[droppedWithErrors];*)


(* ::Input:: *)
(*Length[Sorted]*)


(* ::Input:: *)
(*Sorted[[{12,13,7}]]//N*)
(*With[{L1=%[[1,1]],L2=%[[2,1]],n1=%[[1,2]],n2=%[[2,2]]},(n1-n2)/(L1-L2)]*)


(* ::Subsubsection::Closed:: *)
(*Using nearest neighbors points (bad)*)


(* ::Input:: *)
(*dfList=ParallelTable[With[{L1=Sorted[[i,1]],L2=Sorted[[i+1,1]],n1=Sorted[[i,2]],n2=Sorted[[i+1,2]]},(n1-n2)/(L1-L2)],{i,1,Length[Sorted]-1}]*)


(* ::Input:: *)
(*Drop[dfList,-35];*)
(*fitOfdfList=NonlinearModelFit[%,df,{df},x]*)
(*Show[{ListPlot[%%],Plot[fitOfdfList[x],{x,0,50}]},PlotRange->All]*)


(* ::Subsubsection::Closed:: *)
(*Using points further apart (badish)*)


(* ::Input:: *)
(*distance=5;*)
(*dfList=ParallelTable[With[{L1=Sorted[[i,1]],L2=Sorted[[i+distance,1]],n1=Sorted[[i,2]],n2=Sorted[[i+distance,2]]},(n1-n2)/(L1-L2)],{i,1,Length[Sorted]-distance}]*)


(* ::Input:: *)
(*Drop[Drop[dfList,5],-10];*)
(*fitOfdfList=NonlinearModelFit[%,df+a x+c x^2,{df,a,c},x]*)
(*Show[{ListPlot[%%],Plot[fitOfdfList[x],{x,0,50}]},PlotRange->All]*)


(* ::Section::Closed:: *)
(*Application to b=4*)


(* ::Input:: *)
(*b=4;*)
(**)
(*rawData=Import["data4Sqaure-HybridSq.mx"];*)
(**)
(*Length[rawData]*)


(* ::Input:: *)
(*(*Filter out empty elements without unpacking remaining data*)*)
(*rawData=Pick[rawData,Unitize[Length/@rawData],1];*)
(**)
(*Length[rawData]*)


(* ::Item::Closed:: *)
(*Run once to export MX file*)


(* ::Input:: *)
(*data4Square=Import["D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d-FromCluster\\b4-clean_merged_data-HybridSq.csv","CSV"];*)
(*(*Immediately lock it into a Packed Array*)*)
(*data4Square=Developer`ToPackedArray[data4Square];*)
(* (* MODIFY FILE NAME *)*)
(*Length[data4Square]*)


(* ::Input:: *)
(*rawData=Join[data4Square, {}];*)
(*rawData=Developer`ToPackedArray[rawData];*)
(**)
(*Length[rawData]*)
(**)
(*rawData=Pick[rawData,Unitize[Length/@rawData],1];*)
(**)
(*Length[rawData]*)


(* ::Input:: *)
(*Export["data4Sqaure-HybridSq.mx",rawData,"MX"]*)


(* ::Item::Closed:: *)
(*Continues*)


(* ::Input:: *)
(*N[Check[StandardDeviation[#],1.0`*^-8]/Sqrt[Length[#]]&/@yGroups[[1;;10]]]*)


(* ::Input:: *)
(*(*1. Gather rows by their first element (x) at C-speed*)*)
(*gathered=GatherBy[rawData,First];*)
(**)
(*(*2. Extract the unique X values directly from the gathered groups*)*)
(*xValues=gathered[[All,1,1]];*)
(**)
(*(*3. Extract the Y values for each group*)*)
(*yGroups=gathered[[All,All,2]];*)
(**)
(*(*4. Map Mean and StandardDeviation across the groups in bulk*)*)
(*means=Mean/@yGroups/. 0.->1.0`*^-8;*)
(**)
(*(*Standard deviation throws an error/indeterminacy if length is 1,so we replace Indeterminate with 0 globally at the end*)*)
(*stdDevs=Check[StandardDeviation[#],1.0`*^-8]/Sqrt[Length[#]]&/@yGroups/. Indeterminate->1.0`*^-8;*)
(*stdDevs=stdDevs/. 0.->1.0`*^-8;*)
(**)
(*(* Maximum deviation*)*)
(*maxDevs=MapThread[Max[Abs[#1-#2]]&,{means//N,yGroups}]/. 0.->1.0`*^-8;*)
(**)
(**)
(*(*5. Combine them using the Threaded Around wrapper*)*)
(**)
(*averaged=Transpose[{xValues,means}];*)
(*averagedWithErrors=Transpose[{xValues,MapThread[Around,{means,stdDevs}]}];*)
(*averagedWithMaxDev=Transpose[{xValues,MapThread[Around,{means,maxDevs}]}];*)


(* ::Input:: *)
(*(*NOT USING IT*)
(**)
(* Remove the Sqrt[N] factor from the StdDev estimates*)*)
(*(*MapThread[Times[#1,Sqrt[#2]]&,{stdDevs,yGroups}]*)
(**)
(*averagedWithErrors=Transpose[{xValues,MapThread[Around,{means,%}]}];*)*)


(* ::Input:: *)
(**)
(*(*Standard deviation with my code (Kay's trick for better estimate)*)*)
(*stdDevsEstimated=StdDevEstimate[#,280,"print"->tTrue]&/@yGroups/. Indeterminate->1.0`*^-8;*)
(*stdDevsEstimated=stdDevsEstimated/. 0.->1.0`*^-8;*)
(**)
(*averagedWithEstimatedStdDevs=Transpose[{xValues,MapThread[Around,{means,stdDevsEstimated}]}];*)


(* ::Item::Closed:: *)
(*Fixed size analysis to try and get the best parameters (e.g. bin size) -> Around ??? (with the estimate it's a bit bigger)		TBD*)


(* ::Input:: *)
(*(* Take the 11th element which contains many points. As it can be seen in the Histogram below, the distribution is far from being symmetric *)*)


(* ::Input:: *)
(*gathered[[1]]*)


(* ::Input:: *)
(*Ordering[gathered][[-1]]*)
(*gathered[[%]]*)


(* ::Input:: *)
(*gathered[[9;;11]];*)
(*Length/@%*)
(*histoData=%%[[All,All,2]];*)
(*Histogram[#,Length[#],PlotRange->All]&/@%*)


(* ::Input:: *)
(*Skewness[histoData]//N*)


(* ::Subitem::Closed:: *)
(*identification of best binSize -> best of both skewness ans kurtosis:  289*)


(* ::Input:: *)
(*Partition[histoData,UpTo[50]];*)
(*Length@%*)
(*partData=Mean/@%%;*)
(*Histogram[partData,Automatic,"Probability",PlotRange->All]*)
(*Skewness[partData]//N*)
(*Kurtosis[partData]//N*)


(* ::Input:: *)
(*{x,y}=Transpose*)
(*Clear[x,y]*)


(* ::Input:: *)
(*listOfBinSizes={listOfBinSizesSkewness,listOfBinSizesKurtosis}=Transpose[Table[*)
(*part=Partition[histoData,UpTo[binSize]];*)
(*part=Mean/@part;*)
(*{{Abs[Skewness[part]//N],binSize},{Abs[Kurtosis[part]//N],binSize}}*)
(*,{binSize,1,300,2}]];*)


(* ::Input:: *)
(*(Ordering/@{listOfBinSizesSkewness,listOfBinSizesKurtosis})[[All,1;;10]]*)
(*Part[listOfBinSizesSkewness,#]&/@%[[1]]*)
(*Part[listOfBinSizesKurtosis,#]&/@%%[[2]]*)


(* ::Input:: *)
(**)


(* ::Input:: *)
(*listOfBinSizesSkewness[[143]]*)
(**)
(*Partition[histoData,UpTo[%[[2]]]];*)
(*Length@%*)
(*partData=Mean/@%%;*)
(*Histogram[partData,Automatic,"Probability",PlotRange->All]*)
(*Skewness[partData]//N*)
(*Kurtosis[partData]//N*)
(**)
(**)
(*listOfBinSizesKurtosis[[128]]*)
(**)
(*Partition[histoData,UpTo[%[[2]]]];*)
(*Length@%*)
(*partData=Mean/@%%;*)
(*Histogram[partData,Automatic,"Probability",PlotRange->All]*)
(*Skewness[partData]//N*)
(*Kurtosis[partData]//N*)
(**)


(* ::Input:: *)
(*(*Best of both worlds*)*)


(* ::Input:: *)
(*listOfBinSizesSkewness[[145]]*)
(**)
(*Partition[histoData,UpTo[%[[2]]]];*)
(*Length@%*)
(*partData=Mean/@%%;*)
(*Histogram[partData,Automatic,"Probability",PlotRange->All]*)
(*Skewness[partData]//N*)
(*Kurtosis[partData]//N*)
(**)


(* ::Subitem::Closed:: *)
(*StdDev Difference: with the estimate it's a bit bigger*)


(* ::Input:: *)
(*StandardDeviation[histoData]//N*)


(* ::Input:: *)
(*StdDevEstimate[histoData,289,"print"->True]*)


(* ::Item::Closed:: *)
(*Check how the distribution and the mean change with a lot of statistic 			TBD*)


(* ::Input:: *)
(*gatheredLogSpacing=GatherBy[logSpacing,First];*)
(*Length/@%*)


(* ::Input:: *)
(*Ordering[gatheredLogSpacing][[-1]];*)
(*sample=Part[gatheredLogSpacing,#]&@%;*)


(* ::Input:: *)
(*sample[[1;;2]]*)


(* ::Input:: *)
(*meanSample=Mean[sample[[All,2]]]//N*)


(* ::Subitem::Closed:: *)
(*Find it in gathered*)


(* ::Input:: *)
(*Position[gathered,_?(#[[1,1]]==599&),{1}]*)


(* ::Input:: *)
(*comparison=gathered[[775]];*)


(* ::Input:: *)
(*meanComparison=Mean[comparison[[All,2]]]//N*)


(* ::Subitem::Closed:: *)
(*Histograms*)


(* ::Input:: *)
(*{sample,comparison};*)
(*Length/@%*)
(*histoData=%%[[All,All,2]];*)
(*Histogram[#,Length[#],PlotRange->All]&/@%*)


(* ::Subsection::Closed:: *)
(*Take the Log*)


(* ::Input:: *)
(*logAveraged=Log[averaged];*)
(*logAveragedWithErrors=Log[averagedWithErrors]/. 0->Around[1.0`*^-6,1.0`*^-6];*)
(*logAveragedWithEstimatedStdDevs=Log[averagedWithEstimatedStdDevs]/. 0->Around[1.0`*^-6,1.0`*^-6];*)
(*logAveragedWithMaxDev=Log[averagedWithMaxDev]/. 0->Around[1.0`*^-6,1.0`*^-6];*)


(* ::Input:: *)
(*logAveragedWithEstimatedStdDevs[[1]]*)


(* ::Input:: *)
(*(*Check which error is bigger*)*)
(*(Part[#,All,2,2]&/@{logAveragedWithEstimatedStdDevs,-logAveragedWithErrors});*)
(*Mean[(#[[1]]-#[[2]]&@%)]*)


(* ::Input:: *)
(**)


(* ::Input:: *)
(*Show[{ListPlot[logAveragedWithMaxDev,PlotStyle->{GrayLevel[0],Directive[Opacity[0.3]]},AxesLabel->{"Log[L]","Log[N]"}]*)
(*,ListPlot[logAveragedWithErrors,PlotStyle->RGBColor[0, 0.78, 1]]*)
(*(*,ListPlot[logAveragedWithEstimatedStdDevs,PlotStyle->{,Directive[Opacity[0.6]]}]*)*)
(*}*)
(*,PlotRange->{{6.3,6.5},All},AxesOrigin->{4,0}];*)
(**)
(*Show[{ListPlot[logAveragedWithMaxDev,PlotStyle->{GrayLevel[0],Directive[Opacity[0.3]],PointSize->0.001},AxesLabel->{"Log[L]","Log[N]"}]*)
(*,ListPlot[logAveragedWithErrors,PlotStyle->{RGBColor[0, 0.78, 1],PointSize->0.005}]*)
(*(*,ListPlot[logAveragedWithEstimatedStdDevs,PlotStyle->{,Directive[Opacity[0.6]]}]*)*)
(*}*)
(*,PlotRange->{All,All},AxesOrigin->{1,0}]*)
(**)
(**)


(* ::Input:: *)
(*logAveragedWithErrors//Length*)
(*weights=Map[#[[2]]["Uncertainty"]&,logAveragedWithErrors];*)
(*%//Length*)
(*Position[weights,_?(Element[#,Reals]=!=True&)]*)
(**)
(**)
(*weights=Map[#[[2]]["Uncertainty"]&,logAveragedWithMaxDev];*)
(*%//Length*)
(*Position[weights,_?(Element[#,Reals]=!=True&)]*)
(**)
(**)
(*weights=Map[#[[2]]["Uncertainty"]&,logAveragedWithEstimatedStdDevs];*)
(*%//Length*)
(*Position[weights,_?(Element[#,Reals]=!=True&)]*)


(* ::Subsection::Closed:: *)
(*Looking for the best fitting strategy*)


(* ::Subsubsection::Closed:: *)
(*Linear fit with Errors obtained with StandardDeviation[]*)


(* ::Input:: *)
(*fitFunc=a+c Exp[-\[Omega] x]+df x;*)
(*fitFunc=a+df x;*)
(**)
(*lmAveragedWithStdDevs=NonlinearModelFit[logAveragedWithErrors,{fitFunc,{(*-2<a<2,*)a<0(*,0.5<\[Omega]<=3*),c<0(*,1<df<1.1*)}},{a,c,\[Omega](*{a,-0.5},{c,-30.},{\[Omega],2.}*),df(*,{df,1.083}*)},x,MaxIterations->10000];*)
(**)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@lmAveragedWithStdDevs*)
(**)
(*lmAveragedWithStdDevsGlobal=NonlinearModelFit[logAveragedWithErrors,{fitFunc,{(*-2<a<2,*)a<0(*,0.5<\[Omega]<=3*),c<0(*,1<df<1.1*)}},{a,c,\[Omega](*{a,-0.5},{c,-30.},{\[Omega],2.}*),df(*,{df,1.083}*)},x,MaxIterations->1000,Method->"NMinimize"];*)
(**)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@lmAveragedWithStdDevsGlobal*)
(**)


(* ::Input:: *)
(*synchronizedPlots=Map[Show[#,ImageSize->500]&,{ListPlot[lmAveragedWithStdDevs["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis lmAveragedWithStdDevs,\nAdjustedRSquared=",lmAveragedWithStdDevs["AdjustedRSquared"]}]]*)
(**)
(*,ListPlot[lmAveragedWithStdDevsGlobal["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis lmAveragedWithStdDevsGlobal,\nAdjustedRSquared=",lmAveragedWithStdDevsGlobal["AdjustedRSquared"]}]]}];*)
(**)
(*Multicolumn[synchronizedPlots,2,Appearance->"Framed"]*)
(**)


(* ::Input:: *)
(*maxx=Max[rawData[[All,1]]];*)
(*maxy=Max[rawData[[All,2]]];*)
(**)
(*thresholdBelow=0;*)
(*thresholdAbove=maxx-0;*)
(**)
(*Show[{ListPlot[logAveragedWithMaxDev,PlotStyle->{GrayLevel[0],Directive[Opacity[0.3]]},AxesLabel->{"Log[L]","Log[N]"}]*)
(*,ListPlot[logAveragedWithErrors,PlotStyle->RGBColor[0, 0.78, 1],PlotLegends->{"logAveragedWithErrors"}]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 0.68, 0.5],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@lmAveragedWithStdDevs[x]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.85, 0, 0.5],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@lmAveragedWithStdDevsGlobal[x]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0, 0, 1],Dashed},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@(x (dfSLE/.bb->N[b])-1)*)
(*}*)
(*,PlotLabel->Row[{" b = ",b}]*)
(*,PlotRange->{All,{0,All}},AxesOrigin->{1,0},ImageSize->700]*)


(* ::Subsubsection::Closed:: *)
(*NonLinear fit Errors obtained with StandardDeviation[]. IT STRUGGLES TO FIND THE RIGHT FIT WITH a+c E^(-x \[Omega])+df x.*)


(* ::Input:: *)
(*fitFunc=a+c Exp[-\[Omega] x]+df x;*)
(*(*fitFunc=a+c Exp[- x]+df x;*)*)
(*(**)
(*nlmAveragedWithStdDevs=NonlinearModelFit[logAveragedWithErrors,{fitFunc,{(*-2<a<2,*)a<0(*,0.5<\[Omega]<=3*),c<0(*,1<df<1.1*)}},{a,c,\[Omega](*{a,-0.5},{c,-30.},{\[Omega],2.}*),df(*,{df,1.024}*)},x,MaxIterations->1000];*)
(*nlmAveragedWithStdDevsUnconstrained=NonlinearModelFit[logAveragedWithErrors,{fitFunc,{-2<a<2,0.5<\[Omega]<=3(*,1<df<1.1*)}},{a,c,\[Omega](*{a,-0.5},{c,-30.},{\[Omega],2.}*),df(*,{df,1.024}*)},x,MaxIterations->1000];*)
(**)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@nlmAveragedWithStdDevs*)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@nlmAveragedWithStdDevsUnconstrained*)*)
(**)
(**)
(*nlmAveragedWithStdDevsGlobal=NonlinearModelFit[logAveragedWithErrors,{fitFunc,{(*-2<a<2,*)a<0,c<0,0.5<\[Omega]<=1.1,1<df<1.1}},{(*a,c,\[Omega],df*){a,-0.5},{c,-30.},{\[Omega],1.},{df,1.024}},x,MaxIterations->1000,Method->"NMinimize"];*)
(**)
(*nlmAveragedWithStdDevsUnconstrainedGlobal=NonlinearModelFit[logAveragedWithErrors,fitFunc,{{a,-0.5},{c,-30.},\[Omega](*,{\[Omega],2.}*),df(*,{df,1.024}*)},x,MaxIterations->1000,Method->"NMinimize"];*)
(**)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@nlmAveragedWithStdDevsGlobal*)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@nlmAveragedWithStdDevsUnconstrainedGlobal*)
(**)


(* ::Input:: *)
(*synchronizedPlots=Map[Show[#,ImageSize->500]&,{ListPlot[nlmAveragedWithStdDevs["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis nlmAveragedWithStdDevs,\nAdjustedRSquared=",nlmAveragedWithStdDevs["AdjustedRSquared"]}]]*)
(*,ListPlot[nlmAveragedWithStdDevsUnconstrained["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis nlmAveragedWithStdDevsUnconstrained,\nAdjustedRSquared=",nlmAveragedWithStdDevsUnconstrained["AdjustedRSquared"]}]]*)
(*,ListPlot[nlmAveragedWithStdDevsGlobal["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis nlmAveragedWithStdDevsGlobal,\nAdjustedRSquared=",nlmAveragedWithStdDevsGlobal["AdjustedRSquared"]}]]*)
(*,ListPlot[nlmAveragedWithStdDevsUnconstrainedGlobal["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis nlmAveragedWithStdDevsUnconstrainedGlobal,\nAdjustedRSquared=",nlmAveragedWithStdDevsUnconstrainedGlobal["AdjustedRSquared"]}]]}];*)
(**)
(*Multicolumn[synchronizedPlots,2,Appearance->"Framed"]*)
(**)


(* ::Input:: *)
(*maxx=Max[rawData[[All,1]]];*)
(*maxy=Max[rawData[[All,2]]];*)
(**)
(*thresholdBelow=0;*)
(*thresholdAbove=maxx-0;*)
(**)
(*Show[{ListPlot[logAveragedWithMaxDev,PlotStyle->{GrayLevel[0],Directive[Opacity[0.3]]},AxesLabel->{"Log[L]","Log[N]"}]*)
(*,ListPlot[logAveragedWithErrors,PlotStyle->RGBColor[1, 0.55, 1],PlotLegends->{"logAveragedWithErrors"}](**)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{,Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@nlmAveragedWithStdDevs[x]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{,Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@nlmAveragedWithStdDevsUnconstrained[x]*)*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.85, 0, 0.5],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@nlmAveragedWithStdDevsGlobal[x]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 0.68, 0.8],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@nlmAveragedWithStdDevsUnconstrainedGlobal[x]*)
(**)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0, 0, 1],Dashed},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@(x (dfSLE/.bb->N[b])-1)*)
(*}*)
(*,PlotLabel->Row[{" b = ",b}]*)
(*,PlotRange->{All,{0,All}},AxesOrigin->{1,0},ImageSize->700]*)


(* ::Subsection::Closed:: *)
(*Drop both first and/or last few 		WORKS PRETTY WELL (LACKING STATISTICS AT BIG L)*)


(* ::Subsubsection::Closed:: *)
(*Linear*)


(* ::Input:: *)
(*maxx=Max[rawData[[All,1]]];*)
(*maxy=Max[rawData[[All,2]]];*)
(**)
(*thresholdBelow=25;*)
(*thresholdAbove=maxx-0*500;*)
(**)
(*droppedWithMaxDev=Select[logAveragedWithMaxDev,Log[thresholdBelow]<#[[1]]<Log[thresholdAbove]&];*)
(*droppedWithErrors=Select[logAveragedWithErrors,Log[thresholdBelow]<#[[1]]<Log[thresholdAbove]&];*)
(**)
(**)
(*lmdroppedWithMaxDev=LinearModelFit[droppedWithMaxDev,x,x,Weights->Automatic];*)
(*lmdroppedWithErrors=NonlinearModelFit[droppedWithErrors,a+df x,{a,df},x,Weights->Automatic(*,Method->"NMinimize"*)]*)
(*(*lmpdropped=LinearModelFit[dropped,fitFuncs,x];*)*)
(**)
(*Show[{ListPlot[logAveragedWithMaxDev,PlotStyle->{GrayLevel[0],Directive[Opacity[0.3]]},AxesLabel->{"Log[L]","Log[N]"}]*)
(*,ListPlot[logAveragedWithErrors,PlotStyle->RGBColor[0, 0.78, 1],PlotLegends->{"logAveragedWithErrors"}]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 0, 0.5],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@lmdroppedWithMaxDev[x]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[1, 0.68, 0.6],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@lmdroppedWithErrors[x]*)
(*(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{,Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@lmAveragedWithStdDevs[x]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{,Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@lmAveragedWithStdDevsGlobal[x]*)*)
(**)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0, 0, 1],Dashed},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@(x (dfSLE/.bb->N[b])-1)*)
(*,Plot[#,{x,0,Log[thresholdAbove]},PlotStyle->{RGBColor[0, 1, 1],Dashed},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@(x (dfSLE/.bb->N[b])-0.7-5Exp[-0.991 x])*)
(*}*)
(*,Epilog->{Directive[Dashed,GrayLevel[0.5]],*)
(*Line[{{Log[thresholdBelow],0},{Log[thresholdBelow],Log[maxy]//N}}]*)
(*,Line[{{Log[thresholdAbove],0},{Log[thresholdAbove],Log[maxy]//N}}]}*)
(*,PlotLabel->Row[{" b = ",b}]*)
(*,PlotRange->{All,{0,All}}(*PlotRange->{{Log[thresholdBelow],Log[maxy]},{4,7.2}}*),AxesOrigin->{(*Log[thresholdBelow]*)1,0},ImageSize->700]*)


(* ::Subsubsection::Closed:: *)
(*Non-linear*)


(* ::Input:: *)
(*maxx=Max[rawData[[All,1]]];*)
(*maxy=Max[rawData[[All,2]]];*)
(**)
(*thresholdBelow=23;*)
(*thresholdAbove=maxx-500;*)
(**)
(*droppedWithMaxDev=Select[logAveragedWithMaxDev,Log[thresholdBelow]<#[[1]]<Log[thresholdAbove]&];*)
(*droppedWithErrors=Select[logAveragedWithErrors,Log[thresholdBelow]<#[[1]]<Log[thresholdAbove]&];*)
(**)
(*fitFunc=a+c Exp[- x]+df x;*)
(**)
(*nlmdroppedWithMaxDev=NonlinearModelFit[droppedWithMaxDev,fitFunc,{{a,-0.5},{c,-30.},{\[Omega],1.},df(*,{df,1.024}*)},x,MaxIterations->1000,Method->"NMinimize"];*)
(*nlmdroppedWithErrors=NonlinearModelFit[droppedWithErrors,fitFunc,{{a,-0.5},{c,-30.},{\[Omega],1.},df(*,{df,1.024}*)},x,MaxIterations->1000,Method->"NMinimize"];*)
(*(*lmpdropped=LinearModelFit[dropped,fitFuncs,x];*)*)
(**)
(*Show[{ListPlot[logAveragedWithMaxDev,PlotStyle->{GrayLevel[0],Directive[Opacity[0.3]]},AxesLabel->{"Log[L]","Log[N]"}]*)
(*,ListPlot[logAveragedWithErrors,PlotStyle->RGBColor[0, 0.78, 1],PlotLegends->{"logAveragedWithErrors"}]*)
(**)
(*(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{,Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@nlmdroppedWithMaxDev[x]*)*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[1, 0.68, 0.6],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@nlmdroppedWithErrors[x]*)
(*(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{,Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@lmAveragedWithStdDevs[x]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{,Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@lmAveragedWithStdDevsGlobal[x]*)*)
(**)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0, 0, 1],Dashed},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@(x (dfSLE/.bb->N[b])-1)*)
(*,Plot[#,{x,0,Log[thresholdAbove]},PlotStyle->{RGBColor[0, 1, 1],Dashed},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@(x (dfSLE/.bb->N[b])-0.7-5Exp[-0.991 x])*)
(*}*)
(*,Epilog->{Directive[Dashed,GrayLevel[0.5]],*)
(*Line[{{Log[thresholdBelow],0},{Log[thresholdBelow],Log[maxy]//N}}]*)
(*,Line[{{Log[thresholdAbove],0},{Log[thresholdAbove],Log[maxy]//N}}]}*)
(*,PlotLabel->Row[{" b = ",b}]*)
(*,PlotRange->{All,{0,All}}(*PlotRange->{{Log[thresholdBelow],Log[maxy]},{4,7.2}}*),AxesOrigin->{(*Log[thresholdBelow]*)1,0},ImageSize->700]*)


(* ::Subsection::Closed:: *)
(*With and without Method -> "NMinimize" (which looks for the global minimum, I think it's always better)*)


(* ::Item::Closed:: *)
(*No errors*)


(* ::Input:: *)
(*fitFunc=a+c Exp[-\[Omega] x]+df x;*)
(*fitFunc=a-c Exp[- x]+df x;*)
(*nlmAveraged=NonlinearModelFit[logAveraged,{fitFunc(*,{-2<a<2,0<\[Omega]<28}*)},{a,c,(*c,\[Omega],*)df},x];*)
(*nlmAveragedGlobal=NonlinearModelFit[logAveraged,{fitFunc(*,{-2<a<2,0<\[Omega]<28}*)},{a,c,(*c,\[Omega],*)df},x,Method->"NMinimize"];*)
(**)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@nlmAveraged*)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@nlmAveragedGlobal*)


(* ::Input:: *)
(*(*Extract and plot residuals*)*)
(**)
(*nlmAveraged["AdjustedRSquared"]*)
(*ListPlot[nlmAveraged["FitResiduals"],Filling->Axis,PlotLabel->"Residual Analysis"]*)
(**)
(*nlmAveragedGlobal["AdjustedRSquared"]*)
(*ListPlot[nlmAveragedGlobal["FitResiduals"],Filling->Axis,PlotLabel->"Residual Analysis"]*)


(* ::Input:: *)
(*maxx=Max[rawData[[All,1]]];*)
(*maxy=Max[rawData[[All,2]]];*)
(**)
(*thresholdBelow=0;*)
(*thresholdAbove=maxx-0;*)
(**)
(*Show[{ListPlot[logAveragedWithMaxDev,PlotStyle->{GrayLevel[0],Directive[Opacity[0.3]]},AxesLabel->{"Log[L]","Log[N]"}]*)
(*,ListPlot[logAveragedWithErrors,PlotStyle->RGBColor[0, 1, 1]]*)
(*,ListPlot[logAveragedWithEstimatedStdDevs,PlotStyle->{RGBColor[1, 0.55, 1],Directive[Opacity[0.3]]}]*)
(**)
(*(*,Plot[lmp[x],{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{,Thickness->0.004}]*)*)
(**)
(*,Plot[nlmAveraged[x],{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 1, 0.5],Thickness->0.004}]*)
(*,Plot[nlmAveragedGlobal[x],{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 0, 0.5],Thickness->0.004}]*)
(**)
(*,Plot[x (dfSLE/.bb->b)-1,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0, 0, 1],Dashed}]}*)
(*,(*PlotRange->All,*)PlotLabel->Row[{" b = ",b}]*)
(*(*,Epilog->{Directive[Dashed,],*)
(*Line[{{Log[thresholdBelow],0},{Log[thresholdBelow],Log[maxy]//N}}]*)
(*,Line[{{Log[thresholdAbove],0},{Log[thresholdAbove],Log[maxy]//N}}]}*)*)
(*,PlotRange->{{1,All},{0,All}},AxesOrigin->{1,0}]*)
(**)
(**)
(*Print[ (*"Full data - Linear fit : Subscript[d, f]=",Around[Quiet@lm["ParameterTable"][[1]][[1,3,2]],\[Pi]*lm["ParameterErrors"][[2]]],*)"*)
(*\!\(\*TemplateBox[<|\"color\" -> RGBColor[0.5, 1, 0.5]|>,\n\"RGBColorSwatchTemplate\"]\) Averaged data fit without errors and " ,fitFunc,": \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@nlmAveraged["ParameterTable"][[1,1,-1,2]],\[Pi]*Quiet@nlmAveraged["ParameterTable"][[1,1,-1,3]]],"*)
(*\!\(\*TemplateBox[<|\"color\" -> RGBColor[0.5, 0, 0.5]|>,\n\"RGBColorSwatchTemplate\"]\) Averaged data fit without errors, global minimum and " ,fitFunc,": \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@nlmAveragedGlobal["ParameterTable"][[1,1,-1,2]],\[Pi]*Quiet@nlmAveragedGlobal["ParameterTable"][[1,1,-1,3]]],"*)
(**)
(*\!\(\*TemplateBox[<|\"color\" -> RGBColor[0, 0, 1]|>,\n\"RGBColorSwatchTemplate\"]\) Result from the litterature (exact with SLE): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Input:: *)
(*Series[Log[1+x],{x,0,2}]*)


(* ::Input:: *)
(*Quiet@nlmAveragedGlobal["ParameterTable"][[1,1,-1,3]]*)


(* ::Item::Closed:: *)
(*Errors obtained with StandardDeviation[]*)


(* ::Input:: *)
(*fitFunc=a+c Exp[-\[Omega] x]+df x;*)
(*fitFunc=a+c Exp[- x]+df x;*)
(**)
(*nlmAveragedWithStdDevs=NonlinearModelFit[logAveragedWithErrors,{fitFunc,{(*-2<a<2,*)a<0(*,0.5<\[Omega]<=3*),c<0(*,1<df<1.1*)}},{a,c,\[Omega](*{a,-0.5},{c,-30.},{\[Omega],2.}*),df(*,{df,1.083}*)},x,MaxIterations->10000];*)
(*nlmAveragedWithStdDevsUnconstrained=NonlinearModelFit[logAveragedWithErrors,{fitFunc,{(*-2<a<2,*)(*,0.5<\[Omega]<=3*)(*,1<df<1.1*)}},{a,c,\[Omega](*{a,-0.5},{c,-30.},{\[Omega],2.}*),df(*,{df,1.083}*)},x,MaxIterations->10000];*)
(**)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@nlmAveragedWithStdDevs*)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@nlmAveragedWithStdDevsUnconstrained*)
(**)
(*nlmAveragedWithStdDevsGlobal=NonlinearModelFit[logAveragedWithErrors,{fitFunc,{(*-2<a<2,*)a<0(*,0.5<\[Omega]<=3*),c<0(*,1<df<1.1*)}},{a,c,\[Omega](*{a,-0.5},{c,-30.},{\[Omega],2.}*),df(*,{df,1.083}*)},x,MaxIterations->1000,Method->"NMinimize"];*)
(*nlmAveragedWithStdDevsUnconstrainedGlobal=NonlinearModelFit[logAveragedWithErrors,fitFunc,{a,c,\[Omega](*{a,-0.5},{c,-30.},{\[Omega],2.}*),df(*,{df,1.083}*)},x,MaxIterations->1000,Method->"NMinimize"];*)
(**)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@nlmAveragedWithStdDevsGlobal*)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@nlmAveragedWithStdDevsUnconstrainedGlobal*)
(**)


(* ::Input:: *)
(*synchronizedPlots=Map[Show[#,ImageSize->500]&,{ListPlot[nlmAveragedWithStdDevs["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis nlmAveragedWithStdDevs,\nAdjustedRSquared=",nlmAveragedWithStdDevs["AdjustedRSquared"]}]]*)
(*,ListPlot[nlmAveragedWithStdDevsUnconstrained["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis nlmAveragedWithStdDevsUnconstrained,\nAdjustedRSquared=",nlmAveragedWithStdDevsUnconstrained["AdjustedRSquared"]}]]*)
(*,ListPlot[nlmAveragedWithStdDevsGlobal["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis nlmAveragedWithStdDevsGlobal,\nAdjustedRSquared=",nlmAveragedWithStdDevsGlobal["AdjustedRSquared"]}]]*)
(*,ListPlot[nlmAveragedWithStdDevsUnconstrainedGlobal["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis nlmAveragedWithStdDevsUnconstrainedGlobal,\nAdjustedRSquared=",nlmAveragedWithStdDevsUnconstrainedGlobal["AdjustedRSquared"]}]]}];*)
(**)
(*Multicolumn[synchronizedPlots,2,Appearance->"Framed"]*)
(**)


(* ::Input:: *)
(*maxx=Max[rawData[[All,1]]];*)
(*maxy=Max[rawData[[All,2]]];*)
(**)
(*thresholdBelow=0;*)
(*thresholdAbove=maxx-0;*)
(**)
(*Show[{ListPlot[logAveragedWithMaxDev,PlotStyle->{GrayLevel[0],Directive[Opacity[0.3]]},AxesLabel->{"Log[L]","Log[N]"}]*)
(*,ListPlot[logAveragedWithErrors,PlotStyle->RGBColor[1, 0.47000000000000003`, 0],PlotLegends->{"logAveragedWithErrors"}]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 0, 0.5],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@nlmAveragedWithStdDevs[x]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 0.68, 0.5],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@nlmAveragedWithStdDevsUnconstrained[x]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.85, 0, 0.5],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@nlmAveragedWithStdDevsGlobal[x]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 0.68, 0.8],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@nlmAveragedWithStdDevsUnconstrainedGlobal[x]*)
(**)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0, 0, 1],Dashed},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@(x (dfSLE/.bb->N[b])-1)*)
(*}*)
(*,PlotLabel->Row[{" b = ",b}]*)
(*,PlotRange->{All,{0,All}},AxesOrigin->{1,0},ImageSize->700]*)
(**)
(*Print[ "Averaged data fit with errors and " ,fitFunc," \!\(\*TemplateBox[<|\"color\" -> RGBColor[0.5, 0, 0.5]|>,\n\"RGBColorSwatchTemplate\"]\): \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@nlmAveragedWithStdDevs["ParameterTable"][[1]][[1,-1,2]],\[Pi]*nlmAveragedWithStdDevs["ParameterTable"][[1]][[1,-1,3]]],", Parameters:",Quiet@nlmAveragedWithStdDevs["ParameterTable"],"*)
(*Averaged data fit with errors and " ,fitFunc," (unconstrained) \!\(\*TemplateBox[<|\"color\" -> RGBColor[0.5, 0.68, 0.5]|>,\n\"RGBColorSwatchTemplate\"]\): \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@nlmAveragedWithStdDevsUnconstrained["ParameterTable"][[1]][[1,-1,2]],\[Pi]*nlmAveragedWithStdDevsUnconstrained["ParameterTable"][[1]][[1,-1,3]]],", Parameters:",Quiet@nlmAveragedWithStdDevsUnconstrained["ParameterTable"],"*)
(**)
(*Result from the litterature (exact with SLE) \!\(\*TemplateBox[<|\"color\" -> RGBColor[0, 0, 1]|>,\n\"RGBColorSwatchTemplate\"]\): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Item::Closed:: *)
(*Errors obtained with StdDevEstimate[]*)


(* ::Input:: *)
(*fitFunc=a+c Exp[-\[Omega] x]+df x;*)
(*(*fitFunc=a+c Exp[- x]+df x;*)*)
(**)
(*nlmAveragedWithEstimatedStdDevs=NonlinearModelFit[logAveragedWithEstimatedStdDevs,{fitFunc,{(*-2<a<2,*)a<0,0.5<\[Omega]<=3,c<0(*,1<df<1.1*)}},{a,c,\[Omega](*{a,-0.5},{c,-30.},{\[Omega],2.}*),df(*,{df,1.083}*)},x,MaxIterations->1000];*)
(*nlmAveragedWithEstimatedStdDevsUnconstrained=NonlinearModelFit[logAveragedWithEstimatedStdDevs,{fitFunc,{(*-2<a<2,*)0.5<\[Omega]<=30(*,1<df<1.1*)}},{a,c,\[Omega](*{a,-0.5},{c,-30.},{\[Omega],2.}*),df(*,{df,1.083}*)},x,MaxIterations->1000];*)
(**)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@nlmAveragedWithEstimatedStdDevs*)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@nlmAveragedWithEstimatedStdDevsUnconstrained*)
(**)
(*nlmAveragedWithEstimatedStdDevsGlobal=NonlinearModelFit[logAveragedWithEstimatedStdDevs,{fitFunc,{(*-2<a<2,*)a<0,0.5<\[Omega]<=30,c<0(*,1<df<1.1*)}},{a,c,\[Omega](*{a,-0.5},{c,-30.},{\[Omega],2.}*),df(*,{df,1.083}*)},x,MaxIterations->1000,Method->"NMinimize"];*)
(*nlmAveragedWithEstimatedStdDevsUnconstrainedGlobal=NonlinearModelFit[logAveragedWithEstimatedStdDevs,{fitFunc,\[Omega]<=3},{a,c,\[Omega](*{a,-0.5},{c,-30.},{\[Omega],2.}*),df(*,{df,1.083}*)},x,MaxIterations->1000,Method->"NMinimize"];*)
(**)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@nlmAveragedWithEstimatedStdDevsGlobal*)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@nlmAveragedWithEstimatedStdDevsUnconstrainedGlobal*)
(**)


(* ::Input:: *)
(*synchronizedPlots=Map[Show[#,ImageSize->500]&,{ListPlot[nlmAveragedWithEstimatedStdDevs["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis nlmAveragedWithEstimatedStdDevs,\nAdjustedRSquared=",nlmAveragedWithEstimatedStdDevs ["AdjustedRSquared"]}]]*)
(*,ListPlot[nlmAveragedWithEstimatedStdDevsUnconstrained["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis nlmAveragedWithEstimatedStdDevsUnconstrained,\nAdjustedRSquared=",nlmAveragedWithEstimatedStdDevsUnconstrained["AdjustedRSquared"]}]]*)
(*,ListPlot[nlmAveragedWithEstimatedStdDevsGlobal["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis nlmAveragedWithEstimatedStdDevsGlobal,\nAdjustedRSquared=",nlmAveragedWithEstimatedStdDevsGlobal["AdjustedRSquared"]}]]*)
(*,ListPlot[nlmAveragedWithEstimatedStdDevsUnconstrainedGlobal["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis nlmAveragedWithEstimatedStdDevsUnconstrainedGlobal,\nAdjustedRSquared=",nlmAveragedWithEstimatedStdDevsUnconstrainedGlobal["AdjustedRSquared"]}]]}];*)
(**)
(*Multicolumn[synchronizedPlots,2,Appearance->"Framed"]*)
(**)


(* ::Input:: *)
(*maxx=Max[rawData[[All,1]]];*)
(*maxy=Max[rawData[[All,2]]];*)
(**)
(*thresholdBelow=0;*)
(*thresholdAbove=maxx-0;*)
(**)
(*Show[{ListPlot[logAveragedWithMaxDev,PlotStyle->{GrayLevel[0],Directive[Opacity[0.3]]},AxesLabel->{"Log[L]","Log[N]"}]*)
(*,ListPlot[logAveragedWithEstimatedStdDevs,PlotStyle->RGBColor[1, 0.47000000000000003`, 0],PlotLegends->{"logAveragedWithEstimatedStdDevs"}]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 0, 0.5],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@nlmAveragedWithEstimatedStdDevs[x]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 0.68, 0.5],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@nlmAveragedWithEstimatedStdDevsUnconstrained[x]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.85, 0, 0.5],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@nlmAveragedWithEstimatedStdDevsGlobal[x]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 0.68, 0.8],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@nlmAveragedWithEstimatedStdDevsUnconstrainedGlobal[x]*)
(**)
(*,Plot[x (dfSLE/.bb->b)-1,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0, 0, 1],Dashed}]*)
(*}*)
(*,PlotLabel->Row[{" b = ",b}]*)
(*,PlotRange->{All,{0,All}},AxesOrigin->{1,0},ImageSize->700]*)
(**)
(*Print[ "Averaged data fit with errors and " ,fitFunc," \!\(\*TemplateBox[<|\"color\" -> RGBColor[0.5, 0, 0.5]|>,\n\"RGBColorSwatchTemplate\"]\): \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@nlmAveragedWithEstimatedStdDevs["ParameterTable"][[1]][[1,-1,2]],\[Pi]*nlmAveragedWithEstimatedStdDevs["ParameterTable"][[1]][[1,-1,3]]],", Parameters:",Quiet@nlmAveragedWithEstimatedStdDevs["ParameterTable"],"*)
(*Averaged data fit with errors and " ,fitFunc," (unconstrained) \!\(\*TemplateBox[<|\"color\" -> RGBColor[0.5, 0.68, 0.5]|>,\n\"RGBColorSwatchTemplate\"]\): \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@nlmAveragedWithEstimatedStdDevsUnconstrained["ParameterTable"][[1]][[1,-1,2]],\[Pi]*nlmAveragedWithEstimatedStdDevsUnconstrained["ParameterTable"][[1]][[1,-1,3]]],", Parameters:",Quiet@nlmAveragedWithEstimatedStdDevsUnconstrained["ParameterTable"],"*)
(**)
(*Result from the litterature (exact with SLE) \!\(\*TemplateBox[<|\"color\" -> RGBColor[0, 0, 1]|>,\n\"RGBColorSwatchTemplate\"]\): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Item::Closed:: *)
(*Errors obtained with MaxDev[]*)


(* ::Input:: *)
(*fitFunc=a+c Exp[-\[Omega] x]+df x;*)
(*fitFunc=a+c Exp[- x]+df x;*)
(**)
(*nlmAveragedWithMaxStdDevs=NonlinearModelFit[logAveragedWithMaxDev,{fitFunc,{(*-2<a<2,*)a<0(*,0.5<\[Omega]<=3*),c<0(*,1<df<1.1*)}},{a,c,\[Omega](*{a,-0.5},{c,-30.},{\[Omega],2.}*),df(*,{df,1.083}*)},x,MaxIterations->10000];*)
(*nlmAveragedWithMaxStdDevsUnconstrained=NonlinearModelFit[logAveragedWithMaxDev,{fitFunc,{(*-2<a<2,*)(*,0.5<\[Omega]<=3*)(*,1<df<1.1*)}},{a,c,\[Omega](*{a,-0.5},{c,-30.},{\[Omega],2.}*),df(*,{df,1.083}*)},x,MaxIterations->10000];*)
(**)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@nlmAveragedWithMaxStdDevs*)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@nlmAveragedWithMaxStdDevsUnconstrained*)
(**)
(*nlmAveragedWithMaxStdDevsGlobal=NonlinearModelFit[logAveragedWithMaxDev,{fitFunc,{(*-2<a<2,*)a<0(*,0.5<\[Omega]<=3*),c<0(*,1<df<1.1*)}},{a,c,\[Omega](*{a,-0.5},{c,-30.},{\[Omega],2.}*),df(*,{df,1.083}*)},x,MaxIterations->1000,Method->"NMinimize"];*)
(*nlmAveragedWithMaxStdDevsUnconstrainedGlobal=NonlinearModelFit[logAveragedWithMaxDev,fitFunc,{a,c,\[Omega](*{a,-0.5},{c,-30.},{\[Omega],2.}*),df(*,{df,1.083}*)},x,MaxIterations->1000,Method->"NMinimize"];*)
(**)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@nlmAveragedWithMaxStdDevsGlobal*)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@nlmAveragedWithMaxStdDevsUnconstrainedGlobal*)
(**)


(* ::Input:: *)
(*synchronizedPlots=Map[Show[#,ImageSize->500]&,{ListPlot[nlmAveragedWithMaxStdDevs["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis nlmAveragedWithMaxStdDevs,\nAdjustedRSquared=",nlmAveragedWithMaxStdDevs ["AdjustedRSquared"]}]]*)
(*,ListPlot[nlmAveragedWithMaxStdDevsUnconstrained["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis nlmAveragedWithMaxStdDevsUnconstrained,\nAdjustedRSquared=",nlmAveragedWithMaxStdDevsUnconstrained["AdjustedRSquared"]}]]*)
(*,ListPlot[nlmAveragedWithMaxStdDevsGlobal["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis nlmAveragedWithMaxStdDevsGlobal,\nAdjustedRSquared=",nlmAveragedWithMaxStdDevsGlobal["AdjustedRSquared"]}]]*)
(*,ListPlot[nlmAveragedWithMaxStdDevsUnconstrainedGlobal["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis nlmAveragedWithMaxStdDevsUnconstrainedGlobal,\nAdjustedRSquared=",nlmAveragedWithMaxStdDevsUnconstrainedGlobal["AdjustedRSquared"]}]]}];*)
(**)
(*Multicolumn[synchronizedPlots,2,Appearance->"Framed"]*)
(**)


(* ::Input:: *)
(*maxx=Max[rawData[[All,1]]];*)
(*maxy=Max[rawData[[All,2]]];*)
(**)
(*thresholdBelow=0;*)
(*thresholdAbove=maxx-0;*)
(**)
(*Show[{ListPlot[logAveragedWithMaxDev,PlotStyle->{GrayLevel[0],Directive[Opacity[0.3]]},AxesLabel->{"Log[L]","Log[N]"}]*)
(*,ListPlot[logAveragedWithMaxDev,PlotStyle->RGBColor[1, 0.47000000000000003`, 0],PlotLegends->{"logAveragedWithMaxDev"}]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 0, 0.5],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@nlmAveragedWithMaxStdDevs[x]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 0.68, 0.5],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@nlmAveragedWithMaxStdDevsUnconstrained[x]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.85, 0, 0.5],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@nlmAveragedWithMaxStdDevsGlobal[x]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 0.68, 0.8],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@nlmAveragedWithMaxStdDevsUnconstrainedGlobal[x]*)
(**)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0, 0, 1],Dashed},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@(x (dfSLE/.bb->N[b])-1)*)
(*}*)
(*,PlotLabel->Row[{" b = ",b}]*)
(*,PlotRange->{All,{0,All}},AxesOrigin->{1,0},ImageSize->700]*)
(**)
(*Print[ "Averaged data fit with errors and " ,fitFunc," \!\(\*TemplateBox[<|\"color\" -> RGBColor[0.5, 0, 0.5]|>,\n\"RGBColorSwatchTemplate\"]\): \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@nlmAveragedWithMaxStdDevs["ParameterTable"][[1]][[1,-1,2]],\[Pi]*nlmAveragedWithMaxStdDevs["ParameterTable"][[1]][[1,-1,3]]],", Parameters:",Quiet@nlmAveragedWithMaxStdDevs["ParameterTable"],"*)
(*Averaged data fit with errors and " ,fitFunc," (unconstrained) \!\(\*TemplateBox[<|\"color\" -> RGBColor[0.5, 0.68, 0.5]|>,\n\"RGBColorSwatchTemplate\"]\): \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@nlmAveragedWithMaxStdDevsUnconstrained["ParameterTable"][[1]][[1,-1,2]],\[Pi]*nlmAveragedWithMaxStdDevsUnconstrained["ParameterTable"][[1]][[1,-1,3]]],", Parameters:",Quiet@nlmAveragedWithMaxStdDevsUnconstrained["ParameterTable"],"*)
(**)
(*Result from the litterature (exact with SLE) \!\(\*TemplateBox[<|\"color\" -> RGBColor[0, 0, 1]|>,\n\"RGBColorSwatchTemplate\"]\): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Subsubsection::Closed:: *)
(*Forgot what this is*)


(* ::Input:: *)
(*fitFuncs=Table[x^(-i),{i,-1,3}];*)
(*(*fitFuncs=Append[Table[x^(-i),{i,-1,0}],Exp[-x]];*)*)
(*(*fitFuncs={x,1,Exp[-x],Exp[-2 x]};(*Exp[-1.3 x]*)*)*)
(**)
(*fitFunc=a+c Exp[-\[Omega] x]+df x;*)
(**)
(*lmAveraged=LinearModelFit[logAveragedWithErrors,x,x(*,Weights->Automatic*)];*)
(*Print[Row[{"lmAveraged: ",#//Normal}]]&@%*)
(*lmMaxDev=LinearModelFit[logAveragedWithMaxDev,x,x(*,Weights->Automatic*)];*)
(*Print[Row[{"lmMaxDev: ",#//Normal}]]&@%*)
(**)
(**)
(*fitFuncs={x,1,Exp[-6x]};*)
(**)
(*lmpAveraged=LinearModelFit[logAveragedWithErrors,fitFuncs,x];*)
(*Print[Row[{"\nlmpAveraged: ",#//Normal}]]&@%*)
(*lmpMaxDev=LinearModelFit[logAveragedWithMaxDev,fitFuncs,x];*)
(*Print[Row[{"lmpMaxDev: ",#//Normal}]]&@%*)
(*lmpWithEstimatedStdDevs=LinearModelFit[logAveragedWithEstimatedStdDevs,fitFuncs,x];*)
(*Print[Row[{"lmpMaxDev: ",#//Normal}]]&@%*)
(**)
(*fitFuncs={x,1,Exp[-x]};*)
(**)
(*lmpAveraged=LinearModelFit[logAveragedWithErrors,fitFuncs,x];*)
(*Print[Row[{"\nlmpAveraged: ",#//Normal}]]&@%*)
(*lmpMaxDev=LinearModelFit[logAveragedWithMaxDev,fitFuncs,x];*)
(*Print[Row[{"lmpMaxDev: ",#//Normal}]]&@%*)
(*lmpWithEstimatedStdDevs=LinearModelFit[logAveragedWithEstimatedStdDevs,fitFuncs,x];*)
(*Print[Row[{"lmpMaxDev: ",#//Normal}]]&@%*)
(**)


(* ::Input:: *)
(*fitFunc=a+c Exp[-\[Omega] x]+c2 Exp[-\[Omega]2 x]+df x;*)
(**)
(*nlmAveraged=NonlinearModelFit[logAveragedWithErrors,{fitFunc,{-2<a<2,2.<=\[Omega]<=3.,0.1<=\[Omega]2<=1.}},{{a,-0.5},{c,-30.},\[Omega],c2,\[Omega]2,df(*{df,1.083}*)},x,MaxIterations->1000];*)
(*Print[Row[{"nlmAveraged2 with " ,fitFunc ": ",#//Normal}]]&@%*)
(**)
(*NonlinearModelFit[logAveragedWithMaxDev,{fitFunc(*,{-2<a<2,2.<=\[Omega]<=3.,0.1<=\[Omega]2<=1.}*)},{a,c,\[Omega],c2,\[Omega]2,df(*{df,1.083}*)},x,MaxIterations->1000];*)
(*Print[Row[{"nlmMaxDev without constraints: ",#//Normal}]]&@%*)
(**)
(**)
(*nlmMaxDev=NonlinearModelFit[logAveragedWithMaxDev,{fitFunc,{-2<a<2,2.<=\[Omega]<=3.,0.1<=\[Omega]2<=1.}},{{a,-0.5},{c,-30.},\[Omega],c2,\[Omega]2,df(*{df,1.083}*)},x,MaxIterations->1000];*)
(*Print[Row[{"nlmMaxDev: ",#//Normal}]]&@%*)
(**)
(*Print["Result from the litterature (exact with SLE) : ",(dfSLE/.bb->b)," = ",Style[(dfSLE/.bb->b/1.),RGBColor[0, 0, 1]]]*)


(* ::Input:: *)
(*(*Extract and plot residuals*)*)
(**)
(*nlmMaxDev["AdjustedRSquared"]*)
(*ListPlot[nlmMaxDev["FitResiduals"],Filling->Axis,PlotLabel->"Residual Analysis"]*)


(* ::Input:: *)
(*maxx=Max[rawData[[All,1]]];*)
(*maxy=Max[rawData[[All,2]]];*)
(**)
(*thresholdBelow=0;*)
(*thresholdAbove=maxx-0;*)
(**)
(*Show[{ListPlot[logAveragedWithMaxDev,PlotStyle->{GrayLevel[0],Directive[Opacity[0.3]]},AxesLabel->{"Log[L]","Log[N]"}]*)
(*,ListPlot[logAveragedWithEstimatedStdDevs,PlotStyle->RGBColor[1, 0.55, 1]]*)
(*,ListPlot[logAveragedWithErrors,PlotStyle->{RGBColor[0, 1, 1],Directive[Opacity[0.3]]}]*)
(*(*,Plot[lmp[x],{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{,Thickness->0.004}]*)*)
(*,Plot[lmAveraged[x],{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[1, 0, 0],Thickness->0.004}]*)
(*,Plot[lmpAveraged[x],{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 0, 0.5],Thickness->0.004}]*)
(*,Plot[fitFunc/. fitSol,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[1, Rational[2, 3], 1],Thickness->0.004}]*)
(*,Plot[nlmAveraged[x],{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 1, 0.5],Thickness->0.004}]*)
(**)
(*,Plot[nlmMaxDev[x],{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.76, 0.63, 0.19],Thickness->0.004}]*)
(**)
(*,Plot[x (dfSLE/.bb->b)-1,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0, 0, 1],Dashed}]}*)
(*,(*PlotRange->All,*)PlotLabel->Row[{" b = ",b}]*)
(*(*,Epilog->{Directive[Dashed,],*)
(*Line[{{Log[thresholdBelow],0},{Log[thresholdBelow],Log[maxy]//N}}]*)
(*,Line[{{Log[thresholdAbove],0},{Log[thresholdAbove],Log[maxy]//N}}]}*)*)
(*,PlotRange->{{7,7.5},{6.5,All}},AxesOrigin->{1,0}]*)
(**)
(**)
(**)
(*Print[ (*"Full data - Linear fit : Subscript[d, f]=",Around[Quiet@lm["ParameterTable"][[1]][[1,3,2]],\[Pi]*lm["ParameterErrors"][[2]]],*)"*)
(*Averaged data fit with errors \!\(\*TemplateBox[<|\"color\" -> RGBColor[1, 0, 0]|>,\n\"RGBColorSwatchTemplate\"]\): \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lmAveraged["ParameterTable"][[1]][[1,3,2]],\[Pi]*lmAveraged["ParameterErrors"][[2]]],"*)
(*Averaged data fit with errors and " ,Total@fitFuncs," \!\(\*TemplateBox[<|\"color\" -> RGBColor[0.5, 0, 0.5]|>,\n\"RGBColorSwatchTemplate\"]\): \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lmpAveraged["ParameterTable"][[1]][[1,2,2]],\[Pi]*lmpAveraged["ParameterErrors"][[1]]],"*)
(**)
(*",fitFunc,"  fit  \!\(\*TemplateBox[<|\"color\" -> RGBColor[1, Rational[2, 3], 1]|>,\n\"RGBColorSwatchTemplate\"]\): \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[df/.fitSol,Null],", Parameters:",fitSol,"*)
(*Averaged data fit with errors and " ,fitFunc,"nonlinearModel \!\(\*TemplateBox[<|\"color\" -> RGBColor[0.5, 1, 0.5]|>,\n\"RGBColorSwatchTemplate\"]\): \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@nlmAveraged["ParameterTable"][[1,1,5,2]],\[Pi]*Quiet@nlmAveraged["ParameterTable"][[1,1,5,3]]],"*)
(**)
(*Averaged data fit with MaxDevs and " ,fitFunc,"nonlinearModel \!\(\*TemplateBox[<|\"color\" -> RGBColor[0.76, 0.63, 0.19]|>,\n\"RGBColorSwatchTemplate\"]\): \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@nlmMaxDev["ParameterTable"][[1,1,-1,2]],(*\[Pi]**)Quiet@nlmMaxDev["ParameterTable"][[1,1,-1,3]]],", Parameters:",Quiet@nlmMaxDev["ParameterTable"],(*,"*)
(**)
(*Full data - ",Total@fitFuncs," fit: Subscript[d, f]=",Around[Quiet@lmp["ParameterTable"][[1]][[1,2,2]],\[Pi]*lmp["ParameterErrors"][[1]]]*)"*)
(**)
(*Result from the litterature (exact with SLE) \!\(\*TemplateBox[<|\"color\" -> RGBColor[0, 0, 1]|>,\n\"RGBColorSwatchTemplate\"]\): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Input:: *)
(*Quiet@lmpAveraged["ParameterTable"][[1]]*)
(*Quiet@nlmAveraged["ParameterTable"][[1]]*)


(* ::Input:: *)
(*Quiet@nlmMaxDev["ParameterTable"][[1,All,1;;3]]//Normal*)


(* ::Subsection::Closed:: *)
(*Scan through different shifts to find the best linear fit, i.e. that maximizes AdjustedRSquared*)
(*	TBD*)


(* ::Subsubsection::Closed:: *)
(*With errors from StandardDeviation[]*)


(* ::Input:: *)
(*shift:=Plus[{-4,0},#]&;*)
(**)
(*logAveragedWithErrorsShifted=Log[shift/@averagedWithErrors]/. {a_,0}:>{a,Around[1.0`*^-6,1.0`*^-6]};*)
(*fitFunc=a+df  x;lmAveragedWithErrorsShifted=NonlinearModelFit[Select[logAveragedWithErrorsShifted,#[[1]]>=0&],{fitFunc(*,{-2<a<2,0<\[Omega]<28}*)},{a,(*c,\[Omega],*)df},x]*)


(* ::Input:: *)
(*listOfFits=Table[*)
(*shift:=Plus[{-i,0},#]&;*)
(**)
(*logAveragedWithErrorsShifted=Log[shift/@averagedWithErrors]/. {a_,0}:>{a,Around[1.0`*^-6,1.0`*^-6]};*)
(*fitFunc=a+df  x;lmAveragedWithErrorsShifted=NonlinearModelFit[Select[logAveragedWithErrorsShifted,#[[1]]>=0&],{fitFunc(*,{-2<a<2,0<\[Omega]<28}*)},{a,(*c,\[Omega],*)df},x]*)
(*,*)
(*{i,-5,5,0.1}];*)


(* ::Input:: *)
(*listOfFits[[50;;60]]*)


(* ::Input:: *)
(*Length[listOfFits]*)


(* ::Input:: *)
(*#["AdjustedRSquared"]&/@listOfFits*)
(*best=Ordering[%,-1]*)
(*listOfFits[[%]]*)


(* ::Input:: *)
(*(best[[1]]-51)/10//N;*)
(*Print["Best position is with shift ",%]*)
(*shift:=Plus[{-(best[[1]]-51)/10//N,0},#]&*)
(**)
(*logAveragedWithMaxDevShifted=Log[shift/@averagedWithMaxDev]/.{a_,0}->{a,Around[1.0`*^-6,1.0`*^-6]};*)
(*logAveragedWithErrorsShifted=Log[shift/@averagedWithErrors]/. {a_,0}->{a,Around[1.0`*^-6,1.0`*^-6]};*)
(**)
(**)
(*Show[{ListPlot[logAveragedWithMaxDevShifted,PlotStyle->{GrayLevel[0],Directive[Opacity[0.3]]},AxesLabel->{Row[{"Log[L-",(best[[1]]-51)/10//N,"]"}],"Log[N]"}]*)
(*,ListPlot[logAveragedWithErrorsShifted,PlotStyle->RGBColor[0, 1, 1]]*)
(*(*,ListPlot[logAveragedWithEstimatedStdDevsShifted,PlotStyle->{,Directive[Opacity[0.3]]}]*)*)
(**)
(*(*,Plot[lmp[x],{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{,Thickness->0.004}]*)*)
(**)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 0, 0.5],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],{Right,Top}]]&@(listOfFits[[best[[1]]]][x])*)
(**)
(*,Plot[x (dfSLE/.bb->b)-1,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0, 0, 1],Dashed}]}*)
(*,(*PlotRange->All,*)PlotLabel->Row[{" b = ",b}]*)
(*(*,Epilog->{Directive[Dashed,],*)
(*Line[{{Log[thresholdBelow],0},{Log[thresholdBelow],Log[maxy]//N}}]*)
(*,Line[{{Log[thresholdAbove],0},{Log[thresholdAbove],Log[maxy]//N}}]}*)*)
(*,PlotRange->{All,{0,All}},AxesOrigin->{1,0}]*)
(**)
(*ListPlot[listOfFits[[best[[1]]]]["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis, AdjustedRSquared=",listOfFits[[best[[1]]]]["AdjustedRSquared"]}]]*)
(**)


(* ::Subsubsection::Closed:: *)
(*With errors from MaxDev[]*)


(* ::Input:: *)
(*listOfFits=Table[*)
(*shift:=Plus[{-i,0},#]&;*)
(**)
(*logAveragedWithMaxDevShifted=Log[shift/@averagedWithErrors]/. {a_,0}->{a,Around[1.0`*^-6,1.0`*^-6]};*)
(*fitFunc=a+df  x;lmAveragedWithMaxDevShifted=NonlinearModelFit[Select[logAveragedWithMaxDevShifted,#[[1]]>0&],{fitFunc(*,{-2<a<2,0<\[Omega]<28}*)},{a,(*c,\[Omega],*)df},x]*)
(*,*)
(*{i,-5,5,0.1}];*)


(* ::Input:: *)
(*Length[listOfFits]*)


(* ::Input:: *)
(*#["AdjustedRSquared"]&/@listOfFits*)
(*best=Ordering[%,-1]*)
(*listOfFits[[%]]*)


(* ::Input:: *)
(*(best[[1]]-51)/10//N;*)
(*Print["Best position is with shift ",%]*)
(*shift:=Plus[{-(best[[1]]-51)/10//N,0},#]&*)
(**)
(*logAveragedWithMaxDevShifted=Log[shift/@averagedWithMaxDev]/.{a_,0}->{a,Around[1.0`*^-6,1.0`*^-6]};*)
(*logAveragedWithErrorsShifted=Log[shift/@averagedWithErrors]/. {a_,0}->{a,Around[1.0`*^-6,1.0`*^-6]};*)
(**)
(**)
(*Show[{ListPlot[logAveragedWithMaxDevShifted,PlotStyle->{GrayLevel[0],Directive[Opacity[0.3]]},AxesLabel->{Row[{"Log[L-",(best[[1]]-51)/10//N,"]"}],"Log[N]"}]*)
(*,ListPlot[logAveragedWithErrorsShifted,PlotStyle->RGBColor[0, 1, 1]]*)
(*(*,ListPlot[logAveragedWithEstimatedStdDevsShifted,PlotStyle->{,Directive[Opacity[0.3]]}]*)*)
(**)
(*(*,Plot[lmp[x],{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{,Thickness->0.004}]*)*)
(**)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 0, 0.5],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],{Right,Top}]]&@(listOfFits[[best[[1]]]][x])*)
(**)
(*,Plot[x (dfSLE/.bb->b)-1,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0, 0, 1],Dashed}]}*)
(*,(*PlotRange->All,*)PlotLabel->Row[{" b = ",b}]*)
(*(*,Epilog->{Directive[Dashed,],*)
(*Line[{{Log[thresholdBelow],0},{Log[thresholdBelow],Log[maxy]//N}}]*)
(*,Line[{{Log[thresholdAbove],0},{Log[thresholdAbove],Log[maxy]//N}}]}*)*)
(*,PlotRange->{All,{0,All}},AxesOrigin->{1,0}]*)
(**)
(*ListPlot[listOfFits[[best[[1]]]]["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis, AdjustedRSquared=",listOfFits[[best[[1]]]]["AdjustedRSquared"]}]]*)
(**)


(* ::Subsection::Closed:: *)
(*Take the Log WITH A SHIFT*)


(* ::Input:: *)
(*shift:=Plus[{-4,0},#]&; (*In the code, the stopping condition is with R-1*)*)
(**)
(*logAveragedShifted=Log[shift/@averaged];*)
(*logAveragedWithErrorsShifted=Log[shift/@averagedWithErrors]/. {a_,0}->{a,Around[1.0`*^-6,1.0`*^-6]};*)
(*logAveragedWithEstimatedStdDevsShifted=Log[shift/@averagedWithEstimatedStdDevs]/. {a_,0}->{a,Around[1.0`*^-6,1.0`*^-6]};*)
(*logAveragedWithMaxDevShifted=Log[shift/@averagedWithMaxDev]/.{a_,0}->{a,Around[1.0`*^-6,1.0`*^-6]};*)


(* ::Input:: *)
(*fitFunc=a+df x;*)
(*lmAveragedShifted=NonlinearModelFit[logAveragedShifted,{fitFunc(*,{-2<a<2,0<\[Omega]<28}*)},{a,(*c,\[Omega],*)df},x];*)
(*lmAveragedWithErrorsShifted=NonlinearModelFit[Select[logAveragedWithErrorsShifted,#[[1]]>0&],{fitFunc(*,{-2<a<2,0<\[Omega]<28}*)},{a,(*c,\[Omega],*)df},x];*)
(*(*lmAveragedWithEstimatedStdDevsShifted=NonlinearModelFit[Select[logAveragedWithEstimatedStdDevsShifted,#[[1]]>0&],{fitFunc(*,{-2<a<2,0<\[Omega]<28}*)},{a,(*c,\[Omega],*)df},x];*)*)
(*lmAveragedWithMaxDevShifted=NonlinearModelFit[Select[logAveragedWithMaxDevShifted,#[[1]]>0&],{fitFunc(*,{-2<a<2,0<\[Omega]<28}*)},{a,(*c,\[Omega],*)df},x];*)
(**)
(**)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@lmAveragedShifted*)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@lmAveragedWithErrorsShifted *)
(*(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@lmAveragedWithEstimatedStdDevsShifted *)*)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@lmAveragedWithMaxDevShifted*)


(* ::Input:: *)
(*synchronizedPlots=Map[Show[#,ImageSize->500]&,{ListPlot[lmAveragedShifted["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis, AdjustedRSquared=",lmAveragedShifted["AdjustedRSquared"]}]]*)
(*,ListPlot[lmAveragedWithErrorsShifted["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis, AdjustedRSquared=",lmAveragedWithErrorsShifted["AdjustedRSquared"]}]]*)
(*(*,ListPlot[lmAveragedWithEstimatedStdDevsShifted["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis, AdjustedRSquared=",lmAveragedWithEstimatedStdDevsShifted["AdjustedRSquared"]}]]*)*)
(*,ListPlot[lmAveragedWithMaxDevShifted["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis, AdjustedRSquared=",lmAveragedWithMaxDevShifted["AdjustedRSquared"]}]]}];*)
(**)
(*Multicolumn[synchronizedPlots,2,Appearance->"Framed"]*)
(**)


(* ::Input:: *)
(*maxx=Max[rawData[[All,1]]];*)
(*maxy=Max[rawData[[All,2]]];*)
(**)
(*thresholdBelow=0;*)
(*thresholdAbove=maxx-0;*)
(**)
(*Show[{ListPlot[logAveragedWithMaxDevShifted,PlotStyle->{GrayLevel[0],Directive[Opacity[0.3]]},AxesLabel->{"Log[L-4]","Log[N]"}]*)
(*,(*ListPlot[logAveragedWithErrorsShifted,PlotStyle->]*)ListPlot[logAveragedWithErrorsShifted,PlotStyle->RGBColor[0, 0.78, 1],PlotLegends->{"logAveragedWithErrorsShifted"}]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 0.68, 0.5],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@lmAveragedShifted[x]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.85, 0, 0.5],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@lmAveragedWithErrorsShifted[x]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0, 0, 1],Dashed},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@(x (dfSLE/.bb->N[b])-1)}*)
(*,(*PlotRange->All,*)PlotLabel->Row[{" b = ",b}]*)
(*(*,Epilog->{Directive[Dashed,],*)
(*Line[{{Log[thresholdBelow],0},{Log[thresholdBelow],Log[maxy]//N}}]*)
(*,Line[{{Log[thresholdAbove],0},{Log[thresholdAbove],Log[maxy]//N}}]}*)*)
(*,PlotRange->{{1,All},{0,All}},AxesOrigin->{1,0},ImageSize->Large]*)


(* ::Subsubsection::Closed:: *)
(*Drop some*)


(* ::Input:: *)
(*maxx=Max[rawData[[All,1]]];*)
(*maxy=Max[rawData[[All,2]]];*)
(**)
(*thresholdBelow=70;*)
(*thresholdAbove=maxx-0;*)
(**)
(**)
(**)
(*fitFunc=a+df x;*)
(**)
(*lmAveragedShiftedDropped=NonlinearModelFit[Select[logAveragedShifted,#[[1]]>Log[thresholdBelow]&],{fitFunc(*,{-2<a<2,0<\[Omega]<28}*)},{a,(*c,\[Omega],*)df},x,Method->"NMinimize"];*)
(*lmAveragedWithErrorsShiftedDropped=NonlinearModelFit[Select[logAveragedWithErrorsShifted,#[[1]]>Log[thresholdBelow]&],{fitFunc(*,{-2<a<2,0<\[Omega]<28}*)},{a,(*c,\[Omega],*)df},x,Method->"NMinimize"];*)
(*(*lmAveragedWithEstimatedStdDevsShifted=NonlinearModelFit[Select[logAveragedWithEstimatedStdDevsShifted,#[[1]]>0&],{fitFunc(*,{-2<a<2,0<\[Omega]<28}*)},{a,(*c,\[Omega],*)df},x];*)*)
(*lmAveragedWithMaxDevShiftedDropped=NonlinearModelFit[Select[logAveragedWithMaxDevShifted,#[[1]]>Log[thresholdBelow]&],{fitFunc(*,{-2<a<2,0<\[Omega]<28}*)},{a,(*c,\[Omega],*)df},x,Method->"NMinimize"];*)
(**)
(**)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@lmAveragedShiftedDropped*)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@lmAveragedWithErrorsShiftedDropped *)
(*(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@lmAveragedWithEstimatedStdDevsShiftedDropped *)*)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@lmAveragedWithMaxDevShiftedDropped*)


(* ::Input:: *)
(*synchronizedPlots=Map[Show[#,ImageSize->500]&,{ListPlot[lmAveragedShiftedDropped["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis, AdjustedRSquared=",lmAveragedShiftedDropped["AdjustedRSquared"]}]]*)
(*,ListPlot[lmAveragedWithErrorsShiftedDropped["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis, AdjustedRSquared=",lmAveragedWithErrorsShiftedDropped["AdjustedRSquared"]}]]*)
(*(*,ListPlot[lmAveragedWithEstimatedStdDevsShiftedDropped["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis, AdjustedRSquared=",lmAveragedWithEstimatedStdDevsShiftedDropped["AdjustedRSquared"]}]]*)*)
(*,ListPlot[lmAveragedWithMaxDevShiftedDropped["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis, AdjustedRSquared=",lmAveragedWithMaxDevShiftedDropped["AdjustedRSquared"]}]]}];*)
(**)
(*Multicolumn[synchronizedPlots,2,Appearance->"Framed"]*)
(**)


(* ::Input:: *)
(*Show[{ListPlot[logAveragedWithMaxDevShifted,PlotStyle->{GrayLevel[0],Directive[Opacity[0.3]]},AxesLabel->{"Log[L-4]","Log[N]"}]*)
(*,(*ListPlot[logAveragedWithErrorsShifted,PlotStyle->]*)ListPlot[logAveragedWithErrorsShifted,PlotStyle->RGBColor[0, 0.78, 1],PlotLegends->{"logAveragedWithErrorsShifted"}]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 0.68, 0.5],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@lmAveragedShiftedDropped[x]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.85, 0, 0.5],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@lmAveragedWithErrorsShiftedDropped[x]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0, 0, 1],Dashed},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@(x (dfSLE/.bb->N[b])-1)}*)
(*,(*PlotRange->All,*)PlotLabel->Row[{" b = ",b}]*)
(*(*,Epilog->{Directive[Dashed,],*)
(*Line[{{Log[thresholdBelow],0},{Log[thresholdBelow],Log[maxy]//N}}]*)
(*,Line[{{Log[thresholdAbove],0},{Log[thresholdAbove],Log[maxy]//N}}]}*)*)
(*,PlotRange->{{1,All},{0,All}},AxesOrigin->{1,0},ImageSize->Large]*)


(* ::Subsubsection::Closed:: *)
(*Check with enforced Minimum*)


(* ::Input::Closed:: *)
(*fitFunc=a+df x;*)
(*lmAveragedShiftedGlobal=NonlinearModelFit[logAveragedShifted,{fitFunc(*,{-2<a<2,0<\[Omega]<28}*)},{a,(*c,\[Omega],*)df},x,Method->"NMinimize"];*)
(*lmAveragedWithErrorsShiftedGlobal=NonlinearModelFit[Select[logAveragedWithErrorsShifted,#[[1]]>0&],{fitFunc(*,{-2<a<2,0<\[Omega]<28}*)},{a,(*c,\[Omega],*)df},x,Method->"NMinimize"];*)
(*lmAveragedWithEstimatedStdDevsShiftedGlobal=NonlinearModelFit[Select[logAveragedWithEstimatedStdDevsShifted,#[[1]]>0&],{fitFunc(*,{-2<a<2,0<\[Omega]<28}*)},{a,(*c,\[Omega],*)df},x,Method->"NMinimize"];*)
(*lmAveragedWithMaxDevShiftedGlobal=NonlinearModelFit[Select[logAveragedWithMaxDevShifted,#[[1]]>0&],{fitFunc(*,{-2<a<2,0<\[Omega]<28}*)},{a,(*c,\[Omega],*)df},x,Method->"NMinimize"];*)
(**)
(**)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@lmAveragedShiftedGlobal*)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@lmAveragedWithErrorsShiftedGlobal *)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@lmAveragedWithEstimatedStdDevsShiftedGlobal *)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@lmAveragedWithMaxDevShiftedGlobal*)
(**)
(*Print[]*)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@lmAveragedShifted*)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@lmAveragedWithErrorsShifted *)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@lmAveragedWithEstimatedStdDevsShifted *)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@lmAveragedWithMaxDevShifted*)


(* ::Print:: *)
(*Row[{"lmAveragedWithEstimatedStdDevsShiftedGlobal"," with ",a+df x,": ",-0.412897+1.03982 x}]*)


(* ::Print:: *)
(*Row[{"lmAveragedWithEstimatedStdDevsShifted"," with ",a+df x,": ",-0.412908+1.03982 x}]*)


(* ::Input:: *)
(*synchronizedPlots=Map[Show[#,ImageSize->500]&,{ListPlot[lmAveragedShifted["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis, AdjustedRSquared=",lmAveragedShifted["AdjustedRSquared"]}]]*)
(*,ListPlot[lmAveragedWithErrorsShifted["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis, AdjustedRSquared=",lmAveragedWithErrorsShifted["AdjustedRSquared"]}]]*)
(*,ListPlot[lmAveragedWithEstimatedStdDevsShifted["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis, AdjustedRSquared=",lmAveragedWithEstimatedStdDevsShifted["AdjustedRSquared"]}]]*)
(*,ListPlot[lmAveragedWithMaxDevShifted["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis, AdjustedRSquared=",lmAveragedWithMaxDevShifted["AdjustedRSquared"]}]]}];*)
(**)
(*Multicolumn[synchronizedPlots,2,Appearance->"Framed"]*)
(**)


(* ::Subsubsection::Closed:: *)
(*\[Chi]^2 with analytical b (from SLE)*)


(* ::Input:: *)
(*dfSLE/.bb->b*)


(* ::Input::Closed:: *)
(*fitFunc=a+(dfSLE/.bb->b) x;*)
(*lmAveragedShiftedSLE=NonlinearModelFit[logAveragedShifted,{fitFunc(*,{-2<a<2,0<\[Omega]<28}*)},{a,(*c,\[Omega],*)df},x,Method->"NMinimize"];*)
(*lmAveragedWithErrorsShiftedSLE=NonlinearModelFit[Select[logAveragedWithErrorsShifted,#[[1]]>0&],{fitFunc(*,{-2<a<2,0<\[Omega]<28}*)},{a,(*c,\[Omega],*)df},x,Method->"NMinimize"];*)
(*lmAveragedWithEstimatedStdDevsShiftedSLE=NonlinearModelFit[Select[logAveragedWithEstimatedStdDevsShifted,#[[1]]>0&],{fitFunc(*,{-2<a<2,0<\[Omega]<28}*)},{a,(*c,\[Omega],*)df},x,Method->"NMinimize"];*)
(*lmAveragedWithMaxDevShiftedSLE=NonlinearModelFit[Select[logAveragedWithMaxDevShifted,#[[1]]>0&],{fitFunc(*,{-2<a<2,0<\[Omega]<28}*)},{a,(*c,\[Omega],*)df},x,Method->"NMinimize"];*)
(**)
(**)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@lmAveragedShiftedSLE*)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@lmAveragedWithErrorsShiftedSLE *)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@lmAveragedWithEstimatedStdDevsShiftedSLE *)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@lmAveragedWithMaxDevShiftedSLE*)
(**)


(* ::Print:: *)
(*Row[{"lmAveragedWithEstimatedStdDevsShiftedSLE"," with ",a+(13 x)/12,": ",-0.713206+(13 x)/12}]*)


(* ::Input:: *)
(*synchronizedPlots=Map[Show[#,ImageSize->500]&,{ListPlot[lmAveragedShiftedSLE["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis, AdjustedRSquared=",lmAveragedShifted["AdjustedRSquared"]}]]*)
(*,ListPlot[lmAveragedWithErrorsShiftedSLE["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis, AdjustedRSquared=",lmAveragedWithErrorsShifted["AdjustedRSquared"]}]]*)
(*,ListPlot[lmAveragedWithEstimatedStdDevsShiftedSLE["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis, AdjustedRSquared=",lmAveragedWithEstimatedStdDevsShifted["AdjustedRSquared"]}]]*)
(*,ListPlot[lmAveragedWithMaxDevShiftedSLE["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis, AdjustedRSquared=",lmAveragedWithMaxDevShifted["AdjustedRSquared"]}]]}];*)
(**)
(*Multicolumn[synchronizedPlots,2,Appearance->"Framed"]*)
(**)


(* ::Input:: *)
(*Show[{ListPlot[logAveragedWithMaxDevShifted,PlotStyle->{GrayLevel[0],Directive[Opacity[0.3]]},AxesLabel->{Row[{"Log[L-",(best[[1]]-51)/10//N,"]"}],"Log[N]"}]*)
(*,ListPlot[logAveragedWithErrorsShifted,PlotStyle->{RGBColor[0, 1, 1],PointSize->0.0003}]*)
(*(*,ListPlot[logAveragedWithEstimatedStdDevsShifted,PlotStyle->{,Directive[Opacity[0.3]]}]*)*)
(**)
(*(*,Plot[lmp[x],{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{,Thickness->0.004}]*)*)
(**)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 0, 0.5],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],{Right,Top}]]&@(lmAveragedShiftedSLE[x])*)
(**)
(*,Plot[x (dfSLE/.bb->b)-1,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0, 0, 1],Dashed}]}*)
(*,(*PlotRange->All,*)PlotLabel->Row[{" b = ",b}]*)
(*(*,Epilog->{Directive[Dashed,],*)
(*Line[{{Log[thresholdBelow],0},{Log[thresholdBelow],Log[maxy]//N}}]*)
(*,Line[{{Log[thresholdAbove],0},{Log[thresholdAbove],Log[maxy]//N}}]}*)*)
(*,PlotRange->{All,{0,All}},AxesOrigin->{1,0}]*)


(* ::Subsection::Closed:: *)
(*Extra analysis			*)


(* ::Subsubsection::Closed:: *)
(*Plot with different drops below*)


(* ::Input:: *)
(**)
(*thresholdAbove=maxx-0;*)
(*dfDropped=ParallelTable[With[{*)
(*lmdropped=LinearModelFit[Select[logAveragedWithErrors,Log[i]<=#[[1]]<Log[thresholdAbove]&],x,x,Weights->Automatic]},*)
(*{i,Quiet@lmdropped["ParameterTable"][[1]][[1,3,2]]}],{i,5,500}];*)


(* ::Input:: *)
(*dfDropped[[15;;30]]*)


(* ::Input:: *)
(*lmDrops=LinearModelFit[DeleteCases[dfDropped,{x_,_}/;(x<0)],{1},x];*)
(**)
(*Show[*)
(*{ListPlot[dfDropped,PlotRange->{All,All}]*)
(*(*,Plot[fit[x],{x,0,1000},PlotStyle->Red]*)*)
(*,Plot[dfSLE/.bb->b/1.,{x,0,1000},PlotStyle->RGBColor[0, 0, 1]]*)
(*,Plot[lmDrops[x],{x,0,1000},PlotStyle->RGBColor[0, Rational[2, 3], 0]]*)
(*}*)
(*,AxesLabel->{"Dropping threshold",Subscript[d, f]},PlotRange->All]*)


(* ::Input:: *)
(*dfTogether[[20;;30]]*)


(* ::Subsubsection::Closed:: *)
(*Plot with different drops Above BAD		TBD*)


(* ::Input:: *)
(*thresholdBelow=200;(*Fix this*)*)
(**)
(*dfDroppedAbove=Table[With[{*)
(*lmdropped=LinearModelFit[Log[DeleteCases[rawData,{a_,_}/;(a<thresholdBelow ||  a>(maxx-i))]],x,x]},*)
(*{i,Quiet@lmdropped["ParameterTable"][[1]][[1,3,2]]}],{i,0,1500}];*)


(* ::Input:: *)
(*fitFunc=a+d Exp[c x];*)
(*fit=FindFit[dfDroppedAbove,fitFunc,{a,c,d},x]*)
(**)
(*Show[*)
(*{ListPlot[dfDroppedAbove,PlotRange->{All,All}]*)
(*,Plot[fitFunc/.fit,{x,2,1500},PlotStyle->Red]*)
(*,Plot[dfSLE/.bb->b/1.,{x,0,1000},PlotStyle->Blue]*)
(*},PlotRange->{All,All}]*)


(* ::Input:: *)
(*ListPlot[dfDroppedAbove,PlotRange->{All,All}]*)


(* ::Subsubsection::Closed:: *)
(*Plot with different POSITIONS of the same WINDOW:   (i)<  a < (i + 500)		TBD*)


(* ::Input:: *)
(*window=500;(*Set this*)*)
(**)
(*dfDroppedWindow=ParallelTable[With[{*)
(*lmdropped=LinearModelFit[Select[logAveragedWithErrors,Log[i]<#[[1]]<Log[window+i]&],x,x]},*)
(*{i,Quiet@lmdropped["ParameterTable"][[1]][[1,3,2]]}],{i,0,maxx-window}];*)


(* ::Input:: *)
(*Show[*)
(*{ListPlot[dfDroppedWindow,PlotRange->{All,All}]*)
(*,Plot[dfSLE/.bb->b/1.,{x,0,1500},PlotStyle->RGBColor[0, 0, 1],PlotLegends->SwatchLegend[{"SLE"}]]*)
(*},PlotRange->{All,All},PlotLabel->Row[{"Moving window of size ",window}],AxesLabel->{"Window position",Subscript[d, f]},ImageSize->500]*)


(* ::Subsubsection::Closed:: *)
(*Plot with different POSITIONS of the same WINDOW:  Changing window 		TBD*)


(* ::Input:: *)
(**)


(* ::Input:: *)
(*windowPlots=Table[*)
(*dfDroppedWindow={window,ParallelTable[With[{*)
(*lmdropped=LinearModelFit[Select[logAveragedWithErrors,Log[i]<#[[1]]<Log[window+i]&],x,x]},*)
(*{i,Around[Quiet@lmdropped["ParameterTable"][[1]][[1,3,2]],\[Pi]*lmdropped["ParameterErrors"][[2]]]}],{i,0,maxx-window,20}]}*)
(*,{window,100,1000,50}];*)


(* ::Input:: *)
(*windowPlots[[1]]*)


(* ::Input:: *)
(*showWindowPlots=Show[*)
(*{ListPlot[#[[2]],PlotRange->{All,All}]*)
(*,Plot[dfSLE/.bb->b/1.,{x,0,2000-window},PlotStyle->RGBColor[0, 0, 1]]*)
(*},PlotRange->{All,{0.9,1.1}},PlotLabel->Row[{"Window size = ",#[[1]]}]]&/@windowPlots*)


(* ::Input:: *)
(*synchronizedPlots=Map[Show[#,PlotRange->{All,{0.99,1.05}},ImageSize->220]&,showWindowPlots];*)
(**)
(*Multicolumn[synchronizedPlots,4,Appearance->"Framed"]*)


(* ::Input:: *)
(*Map[Show[#,PlotRange->{All,{1,1.005}},ImageSize->280]&,showWindowPlots[[6;;8]]];*)
(*Multicolumn[%,3,Appearance->"Framed"]*)


(* ::Subsection::Closed:: *)
(*Just two: df=(Log(n)-Log(n'))/(Log(L)-Log(L'))*)


(* ::Input:: *)
(*maxx=Max[rawData[[All,1]]];*)
(*maxy=Max[rawData[[All,2]]];*)
(**)
(*thresholdBelow=25;*)
(*thresholdAbove=maxx-0*500;*)
(**)
(*droppedWithMaxDev=Select[logAveragedWithMaxDev,Log[thresholdBelow]<#[[1]]<Log[thresholdAbove]&];*)
(*droppedWithErrors=Select[logAveragedWithErrors,Log[thresholdBelow]<#[[1]]<Log[thresholdAbove]&];*)
(**)
(**)
(*lmdroppedWithMaxDev=LinearModelFit[droppedWithMaxDev,x,x,Weights->Automatic];*)
(*lmdroppedWithErrors=NonlinearModelFit[droppedWithErrors,a+df x,{a,df},x,Weights->Automatic(*,Method->"NMinimize"*)]*)
(*(*lmpdropped=LinearModelFit[dropped,fitFuncs,x];*)*)
(**)
(*Show[{ListPlot[logAveragedWithMaxDev,PlotStyle->{GrayLevel[0],Directive[Opacity[0.3]]},AxesLabel->{"Log[L]","Log[N]"}]*)
(*,ListPlot[logAveragedWithErrors,PlotStyle->RGBColor[0, 0.78, 1],PlotLegends->{"logAveragedWithErrors"}]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 0, 0.5],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@lmdroppedWithMaxDev[x]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[1, 0.68, 0.6],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@lmdroppedWithErrors[x]*)
(*(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{,Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@lmAveragedWithStdDevs[x]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{,Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@lmAveragedWithStdDevsGlobal[x]*)*)
(**)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0, 0, 1],Dashed},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@(x (dfSLE/.bb->N[b])-1)*)
(*,Plot[#,{x,0,Log[thresholdAbove]},PlotStyle->{RGBColor[0, 1, 1],Dashed},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@(x (dfSLE/.bb->N[b])-0.7-5Exp[-0.991 x])*)
(*}*)
(*,Epilog->{Directive[Dashed,GrayLevel[0.5]],*)
(*Line[{{Log[thresholdBelow],0},{Log[thresholdBelow],Log[maxy]//N}}]*)
(*,Line[{{Log[thresholdAbove],0},{Log[thresholdAbove],Log[maxy]//N}}]}*)
(*,PlotLabel->Row[{" b = ",b}]*)
(*,PlotRange->{All,{0,All}}(*PlotRange->{{Log[thresholdBelow],Log[maxy]},{4,7.2}}*),AxesOrigin->{(*Log[thresholdBelow]*)1,0},ImageSize->700]*)


(* ::Input:: *)
(*Sorted=Sort[droppedWithErrors];*)


(* ::Input:: *)
(*Length[Sorted]*)


(* ::Input:: *)
(*Sorted[[{12,13,7}]]//N*)
(*With[{L1=%[[1,1]],L2=%[[2,1]],n1=%[[1,2]],n2=%[[2,2]]},(n1-n2)/(L1-L2)]*)


(* ::Subsubsection::Closed:: *)
(*Using nearest neighbors points (bad)*)


(* ::Input:: *)
(*dfList=ParallelTable[With[{L1=Sorted[[i,1]],L2=Sorted[[i+1,1]],n1=Sorted[[i,2]],n2=Sorted[[i+1,2]]},(n1-n2)/(L1-L2)],{i,1,Length[Sorted]-1}]*)


(* ::Input:: *)
(*Drop[dfList,-35];*)
(*fitOfdfList=NonlinearModelFit[%,df,{df},x]*)
(*Show[{ListPlot[%%],Plot[fitOfdfList[x],{x,0,50}]},PlotRange->All]*)


(* ::Subsubsection::Closed:: *)
(*Using points further apart (badish)*)


(* ::Input:: *)
(*distance=5;*)
(*dfList=ParallelTable[With[{L1=Sorted[[i,1]],L2=Sorted[[i+distance,1]],n1=Sorted[[i,2]],n2=Sorted[[i+distance,2]]},(n1-n2)/(L1-L2)],{i,1,Length[Sorted]-distance}]*)


(* ::Input:: *)
(*Drop[Drop[dfList,5],-10];*)
(*fitOfdfList=NonlinearModelFit[%,df+a x+c x^2,{df,a,c},x]*)
(*Show[{ListPlot[%%],Plot[fitOfdfList[x],{x,0,50}]},PlotRange->All]*)


(* ::Section:: *)
(*Application to b=15		TBD*)


(* ::Input:: *)
(*b=15;*)
(**)
(*rawData=Import["data15Sqaure-HybridSq.mx"];*)
(**)
(*Length[rawData]*)


(* ::Input:: *)
(*rawData=Pick[rawData,Unitize[Length/@rawData],1];*)
(**)
(*Length[rawData]*)


(* ::Input:: *)
(*rawData=Pick[rawData,Length/@rawData,2];*)
(*Length[rawData]*)


(* ::Item::Closed:: *)
(*Run once to export MX file*)


(* ::Input:: *)
(*rawData=data15Square=Import["D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d-FromCluster\\b15-clean_merged_data-HybridSq.csv","CSV"];*)
(*(*Immediately lock it into a Packed Array*)*)
(*rawData=Developer`ToPackedArray[rawData];*)
(* (* MODIFY FILE NAME *)*)
(*Length[rawData]*)


(* ::Input:: *)
(*rawData=Pick[rawData,Unitize[Length/@rawData],1];*)
(**)
(*Length[rawData]*)


(* ::Input:: *)
(*rawData=Pick[rawData,Length/@rawData,2];*)
(*Length[rawData]*)


(* ::Input:: *)
(*Export["data15Sqaure-HybridSq.mx",rawData,"MX"]*)


(* ::Subsection::Closed:: *)
(*GatherBy x values*)


(* ::Input:: *)
(*(*1. Gather rows by their first element (x) at C-speed*)*)
(*gathered=GatherBy[rawData,First];*)
(**)
(*(*2. Extract the unique X values directly from the gathered groups*)*)
(*xValues=gathered[[All,1,1]];*)
(**)
(*(*3. Extract the Y values for each group*)*)
(*yGroups=gathered[[All,All,2]];*)
(**)
(*(*4. Map Mean and StandardDeviation across the groups in bulk*)*)
(*means=Mean/@yGroups/. 0.->1.0`*^-8;*)
(**)
(*(*Standard deviation throws an error/indeterminacy if length is 1,so we replace Indeterminate with 0 globally at the end*)*)
(*stdDevs=Check[StandardDeviation[#],1.0`*^-8](*/Sqrt[Length[#]]*)&/@yGroups/. Indeterminate->1.0`*^-8;*)
(*stdDevs=stdDevs/. 0.->1.0`*^-8;*)
(*(* stdDev on mean*)*)
(*stdDevsOnMean=(1/Sqrt[Length[#]]&/@yGroups)*stdDevs;*)
(**)
(*(* Maximum deviation*)*)
(*maxDevs=MapThread[Max[Abs[#1-#2]]&,{means//N,yGroups}]/. 0.->1.0`*^-8;*)
(**)
(*(*5. Combine them using the Threaded Around wrapper*)*)
(*averaged=Transpose[{xValues,means}];*)
(*averagedWithErrors=Transpose[{xValues,MapThread[Around,{means,stdDevs}]}];*)
(*averagedWithErrorsOnMean=Transpose[{xValues,MapThread[Around,{means,stdDevsOnMean}]}];*)
(*averagedWithMaxDev=Transpose[{xValues,MapThread[Around,{means,maxDevs}]}];*)


(* ::Input:: *)
(*(*NOT USING IT*)
(**)
(* Remove the Sqrt[N] factor from the StdDev estimates*)*)
(*(*MapThread[Times[#1,Sqrt[#2]]&,{stdDevs,yGroups}]*)
(**)
(*averagedWithErrors=Transpose[{xValues,MapThread[Around,{means,%}]}];*)*)


(* ::Input:: *)
(**)
(*(*Standard deviation with my code (Kay's trick for better estimate)*)*)
(*stdDevsEstimated=StdDevEstimate[#,280,"print"->tTrue]&/@yGroups/. Indeterminate->1.0`*^-8;*)
(*stdDevsEstimated=stdDevsEstimated/. 0.->1.0`*^-8;*)
(**)
(*averagedWithEstimatedStdDevs=Transpose[{xValues,MapThread[Around,{means,stdDevsEstimated}]}];*)


(* ::Item::Closed:: *)
(*Fixed size analysis to try and get the best parameters (e.g. bin size) -> Around ??? (with the estimate it's a bit bigger)		TBD*)


(* ::Input:: *)
(*(* Take the 11th element which contains many points. As it can be seen in the Histogram below, the distribution is far from being symmetric *)*)


(* ::Input:: *)
(*gathered[[1]]*)


(* ::Input:: *)
(*Ordering[gathered][[-1]]*)
(*gathered[[%]]*)


(* ::Input:: *)
(*gathered[[9;;11]];*)
(*Length/@%*)
(*histoData=%%[[All,All,2]];*)
(*Histogram[#,Length[#],PlotRange->All]&/@%*)


(* ::Input:: *)
(*Skewness[histoData]//N*)


(* ::Subitem::Closed:: *)
(*identification of best binSize -> best of both skewness ans kurtosis:  289*)


(* ::Input:: *)
(*Partition[histoData,UpTo[50]];*)
(*Length@%*)
(*partData=Mean/@%%;*)
(*Histogram[partData,Automatic,"Probability",PlotRange->All]*)
(*Skewness[partData]//N*)
(*Kurtosis[partData]//N*)


(* ::Input:: *)
(*{x,y}=Transpose*)
(*Clear[x,y]*)


(* ::Input:: *)
(*listOfBinSizes={listOfBinSizesSkewness,listOfBinSizesKurtosis}=Transpose[Table[*)
(*part=Partition[histoData,UpTo[binSize]];*)
(*part=Mean/@part;*)
(*{{Abs[Skewness[part]//N],binSize},{Abs[Kurtosis[part]//N],binSize}}*)
(*,{binSize,1,300,2}]];*)


(* ::Input:: *)
(*(Ordering/@{listOfBinSizesSkewness,listOfBinSizesKurtosis})[[All,1;;10]]*)
(*Part[listOfBinSizesSkewness,#]&/@%[[1]]*)
(*Part[listOfBinSizesKurtosis,#]&/@%%[[2]]*)


(* ::Input:: *)
(**)


(* ::Input:: *)
(*listOfBinSizesSkewness[[143]]*)
(**)
(*Partition[histoData,UpTo[%[[2]]]];*)
(*Length@%*)
(*partData=Mean/@%%;*)
(*Histogram[partData,Automatic,"Probability",PlotRange->All]*)
(*Skewness[partData]//N*)
(*Kurtosis[partData]//N*)
(**)
(**)
(*listOfBinSizesKurtosis[[128]]*)
(**)
(*Partition[histoData,UpTo[%[[2]]]];*)
(*Length@%*)
(*partData=Mean/@%%;*)
(*Histogram[partData,Automatic,"Probability",PlotRange->All]*)
(*Skewness[partData]//N*)
(*Kurtosis[partData]//N*)
(**)


(* ::Input:: *)
(*(*Best of both worlds*)*)


(* ::Input:: *)
(*listOfBinSizesSkewness[[145]]*)
(**)
(*Partition[histoData,UpTo[%[[2]]]];*)
(*Length@%*)
(*partData=Mean/@%%;*)
(*Histogram[partData,Automatic,"Probability",PlotRange->All]*)
(*Skewness[partData]//N*)
(*Kurtosis[partData]//N*)
(**)


(* ::Subitem::Closed:: *)
(*StdDev Difference: with the estimate it's a bit bigger*)


(* ::Input:: *)
(*StandardDeviation[histoData]//N*)


(* ::Input:: *)
(*StdDevEstimate[histoData,289,"print"->True]*)


(* ::Item::Closed:: *)
(*Check how the distribution and the mean change with a lot of statistic 			TBD*)


(* ::Input:: *)
(*Ordering[gathered]*)
(*%[[-1]]*)
(*sample=Part[gathered,#]&@%;*)


(* ::Input:: *)
(*sample[[1;;2]]*)


(* ::Input:: *)
(*meanSample=Mean[sample[[All,2]]]//N*)


(* ::Subitem::Closed:: *)
(*Find it in gathered*)


(* ::Input:: *)
(*Position[gathered,_?(#[[1,1]]==599&),{1}]*)


(* ::Input:: *)
(*comparison=gathered[[775]];*)


(* ::Input:: *)
(*meanComparison=Mean[comparison[[All,2]]]//N*)


(* ::Subitem::Closed:: *)
(*Histograms*)


(* ::Input:: *)
(*{sample(*,comparison*)};*)
(*Length/@%*)
(*histoData=%%[[All,All,2]];*)
(*Histogram[#,Length[#],PlotRange->All,ImageSize->Large]&/@%*)


(* ::Input:: *)
(*Around[]*)


(* ::Subsubsection::Closed:: *)
(*Using bootmean (gemini implementation, this method is very similar to Kay's)	TDB*)


(* ::Input:: *)
(*yGroups=Developer`ToPackedArray[#]&/@yGroups;*)


(* ::Input:: *)
(*Developer`PackedArrayQ/@yGroups*)


(* ::Input:: *)
(*B=20000;*)
(*bootMeans=Map[Table[Mean[RandomChoice[#,Length[#]]],B]&,yGroups];*)
(*(*Histogram[%,PlotRange->All]*)*)


(* ::Input:: *)
(*B=20000;*)
(*Module[{sampleMean,bootMeans,lowerCI,upperCI,deltaMinus,deltaPlus},*)
(*sampleMean=Mean[#];*)
(*bootMeans=With[{n=Length[#]},Dot[RandomChoice[#,{B,n}],ConstantArray[1./n,n]]];*)
(**)
(*Print["So far so good"];*)
(**)
(*{lowerCI,upperCI}=Quantile[bootMeans,{0.15865,0.84135}];*)
(**)
(*deltaMinus=sampleMean-lowerCI;*)
(*deltaPlus=upperCI-sampleMean;*)
(**)
(*(*5. Assign with Around*)*)
(*Around[sampleMean,{deltaMinus,deltaPlus}]*)
(*]&@yGroups[[2]]*)


(* ::Input:: *)
(*(*3. 68.27% (1-sigma equivalent) confidence bounds via Percentile Bootstrap*)*)
(*{lowerCI,upperCI}=ParallelMap[Quantile[#,{0.15865,0.84135}]&,bootMeans];*)


(* ::Input:: *)
(**)


(* ::Input:: *)
(*(*4. Derive asymmetric deviations*)*)
(*deltaMinus=means-lowerCI*)
(*deltaPlus=upperCI-means*)
(**)
(*(*5. Assign with Around*)*)
(*meanWithUncertainty=Around[sampleMean,{deltaMinus,deltaPlus}]*)


(* ::Subsection::Closed:: *)
(*Take the Log*)


(* ::Input:: *)
(*averaged=Select[averaged,#[[1]]=!=""&];*)
(*averagedWithErrors=Select[averagedWithErrors,#[[1]]=!=""&];*)
(*averagedWithEstimatedStdDevs=Select[averagedWithEstimatedStdDevs,#[[1]]=!=""&];*)
(*averagedWithErrorsOnMean=Select[averagedWithErrorsOnMean,#[[1]]=!=""&];*)
(*averagedWithMaxDev=Select[averagedWithMaxDev,#[[1]]=!=""&];*)


(* ::Input:: *)
(*logAveraged=Log[averaged];*)
(*logAveragedWithErrors=Log[averagedWithErrors]/. 0->Around[1.0`*^-6,1.0`*^-6];*)
(*logAveragedWithErrorsOnMean=Log[averagedWithErrorsOnMean]/. 0->Around[1.0`*^-6,1.0`*^-6];logAveragedWithEstimatedStdDevs=Log[averagedWithEstimatedStdDevs]/. 0->Around[1.0`*^-6,1.0`*^-6];*)
(*logAveragedWithMaxDev=Log[averagedWithMaxDev]/. 0->Around[1.0`*^-6,1.0`*^-6];*)


(* ::Input:: *)
(*logAveragedWithErrors[[1;;10]]*)


(* ::Input:: *)
(*maxx=Max[averaged[[All,1]]];*)
(*maxy=Max[averaged[[All,2]]];*)
(* *)
(*Show[{ListPlot[logAveragedWithMaxDev,PlotStyle->{GrayLevel[0],Directive[Opacity[0.3]],PointSize->0.001},AxesLabel->{"Log[L]","Log[N]"}]*)
(*,ListPlot[logAveragedWithErrors,PlotStyle->{RGBColor[0, 0.78, 1],PointSize->0.01},PlotLegends->PointLegend[{"logAveragedWithErrors"},LegendMarkerSize->10,LegendMarkers->Graphics[Disk[]]]]*)
(*,ListPlot[logAveragedWithErrorsOnMean,PlotStyle->{RGBColor[1, 0.55, 1],Directive[Opacity[0.6]],PointSize->0.005},PlotLegends->PointLegend[{"logAveragedWithErrorsOnMean"},LegendMarkerSize->10,LegendMarkers->Graphics[Disk[]]]]*)
(*,Plot[#,{x,Log[0+1],Log[maxx]},PlotStyle->{RGBColor[0, 0, 1],Dashed},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@(x (dfSLE/. bb->N[b])-0.9)*)
(*}*)
(*,PlotLabel->Row[{" b = ",b}],PlotRange->{All,{0,Log[maxy]}},AxesOrigin->{1,0},ImageSize->700]*)
(**)
(*(*Show[{ListPlot[logAveragedWithMaxDev,PlotStyle->{,Directive[Opacity[0.3]],PointSize->0.001},AxesLabel->{"Log[L]","Log[N]"}]*)
(*,ListPlot[logAveragedWithEstimatedStdDevs,PlotStyle->{,Directive[Opacity[0.8]],PointSize->0.008},PlotLegends->PointLegend[{"logAveragedWithEstimatedStdDevs"},LegendMarkerSize->10,LegendMarkers->Graphics[Disk[]]]]*)
(*,ListPlot[logAveragedWithEstimatedStdDevsOnMean,PlotStyle->{,Directive[Opacity[0.6]],PointSize->0.005},PlotLegends->PointLegend[{"logAveragedWithEstimatedStdDevsOnMean"},LegendMarkerSize->10,LegendMarkers->Graphics[Disk[]]]]*)
(*,Plot[#,{x,Log[0+1],Log[maxx]},PlotStyle->{,Dashed},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@(x (dfSLE/. bb->N[b])-0.9)*)
(*}*)
(*,PlotLabel->Row[{" b = ",b}],PlotRange->{All,{0,Log[maxy]}},AxesOrigin->{1,0},ImageSize->700]*)*)
(**)
(**)


(* ::Input:: *)
(*logAveragedWithErrors//Length*)
(*weights=Map[#[[2]]["Uncertainty"]&,logAveragedWithErrors];*)
(*%//Length*)
(*Position[weights,_?(Element[#,Reals]=!=True&)]*)
(**)
(**)
(*weights=Map[#[[2]]["Uncertainty"]&,logAveragedWithMaxDev];*)
(*%//Length*)
(*Position[weights,_?(Element[#,Reals]=!=True&)]*)
(**)
(**)
(*weights=Map[#[[2]]["Uncertainty"]&,logAveragedWithEstimatedStdDevs];*)
(*%//Length*)
(*Position[weights,_?(Element[#,Reals]=!=True&)]*)


(* ::Subsection::Closed:: *)
(*Looking for the best fitting strategy*)


(* ::Subsubsection::Closed:: *)
(*Linear fit with Errors obtained with StandardDeviation[]*)


(* ::Input:: *)
(*fitFunc=a+c Exp[-\[Omega] x]+df x;*)
(*fitFunc=a+df x;*)
(**)
(*lmAveragedWithStdDevs=NonlinearModelFit[logAveragedWithErrors,{fitFunc,{a<0}},{a,df},x,MaxIterations->10000];*)
(**)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@lmAveragedWithStdDevs*)
(**)
(*lmAveragedWithStdDevsGlobal=NonlinearModelFit[logAveragedWithErrors,{fitFunc,{a<0}},{a,df},x,MaxIterations->1000,Method->"NMinimize"];*)
(**)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@lmAveragedWithStdDevsGlobal*)
(**)


(* ::Input:: *)
(*synchronizedPlots=Map[Show[#,ImageSize->500]&,{ListPlot[lmAveragedWithStdDevs["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis lmAveragedWithStdDevs,\nAdjustedRSquared=",lmAveragedWithStdDevs["AdjustedRSquared"]}]]*)
(**)
(*,ListPlot[lmAveragedWithStdDevsGlobal["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis lmAveragedWithStdDevsGlobal,\nAdjustedRSquared=",lmAveragedWithStdDevsGlobal["AdjustedRSquared"]}]]}];*)
(**)
(*Multicolumn[synchronizedPlots,2,Appearance->"Framed"]*)
(**)


(* ::Input:: *)
(*maxx=Max[averaged[[All,1]]];*)
(*maxy=Max[rawData[[All,2]]];*)
(**)
(*thresholdBelow=0;*)
(*thresholdAbove=maxx-0;*)
(**)
(*Show[{ListPlot[logAveragedWithMaxDev,PlotStyle->{GrayLevel[0],Directive[Opacity[0.3]]},AxesLabel->{"Log[L]","Log[N]"}]*)
(*,ListPlot[logAveragedWithErrors,PlotStyle->RGBColor[0, 0.78, 1],PlotLegends->{"logAveragedWithErrors"}]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 0.68, 0.5],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@lmAveragedWithStdDevs[x]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.85, 0, 0.5],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@lmAveragedWithStdDevsGlobal[x]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0, 0, 1],Dashed},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@(x (dfSLE/.bb->N[b])-1)*)
(*}*)
(*,PlotLabel->Row[{" b = ",b}]*)
(*,PlotRange->{All,{0,All}},AxesOrigin->{1,0},ImageSize->700]*)


(* ::Subsubsection::Closed:: *)
(*NonLinear fit Errors obtained with StandardDeviation[]. IT STRUGGLES TO FIND THE RIGHT FIT WITH a+c E^(-x \[Omega])+df x.*)


(* ::Input:: *)
(*fitFunc=a+c Exp[-\[Omega] x]+df x;*)
(*(*fitFunc=a+c Exp[- x]+df x;*)*)
(**)
(*nlmAveragedWithStdDevs=NonlinearModelFit[logAveragedWithErrors,{fitFunc,{a<0,c<0,0.5<\[Omega]<=3,1<df<1.1}},{a,c,\[Omega](*{a,-0.5},{c,-30.},{\[Omega],2.}*),df(*,{df,1.024}*)},x,MaxIterations->1000];*)
(*nlmAveragedWithStdDevsUnconstrained=NonlinearModelFit[logAveragedWithErrors,fitFunc,{a,c,\[Omega](*{a,-0.5},{c,-30.},{\[Omega],2.}*),df(*,{df,1.024}*)},x,MaxIterations->1000];*)
(**)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@nlmAveragedWithStdDevs*)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@nlmAveragedWithStdDevsUnconstrained*)
(**)
(**)
(*nlmAveragedWithStdDevsGlobal=NonlinearModelFit[logAveragedWithErrors,{fitFunc,{(*-2<a<2,*)a<0,c<0,0.5<\[Omega]<=1.1,1<df<1.1}},{(*a,c,\[Omega],df*){a,-0.5},{c,-30.},{\[Omega],1.},{df,1.024}},x,MaxIterations->1000,Method->"NMinimize"];*)
(**)
(*nlmAveragedWithStdDevsUnconstrainedGlobal=NonlinearModelFit[logAveragedWithErrors,fitFunc,{a,c,\[Omega](*,{\[Omega],2.}*),df(*,{df,1.024}*)},x,MaxIterations->1000,Method->"NMinimize"];*)
(**)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@nlmAveragedWithStdDevsGlobal*)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@nlmAveragedWithStdDevsUnconstrainedGlobal*)
(**)


(* ::Input:: *)
(*synchronizedPlots=Map[Show[#,ImageSize->500]&,{ListPlot[nlmAveragedWithStdDevs["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis nlmAveragedWithStdDevs,\nAdjustedRSquared=",nlmAveragedWithStdDevs["AdjustedRSquared"]}]]*)
(*,ListPlot[nlmAveragedWithStdDevsUnconstrained["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis nlmAveragedWithStdDevsUnconstrained,\nAdjustedRSquared=",nlmAveragedWithStdDevsUnconstrained["AdjustedRSquared"]}]]*)
(*,ListPlot[nlmAveragedWithStdDevsGlobal["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis nlmAveragedWithStdDevsGlobal,\nAdjustedRSquared=",nlmAveragedWithStdDevsGlobal["AdjustedRSquared"]}]]*)
(*,ListPlot[nlmAveragedWithStdDevsUnconstrainedGlobal["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis nlmAveragedWithStdDevsUnconstrainedGlobal,\nAdjustedRSquared=",nlmAveragedWithStdDevsUnconstrainedGlobal["AdjustedRSquared"]}]]}];*)
(**)
(*Multicolumn[synchronizedPlots,2,Appearance->"Framed"]*)
(**)


(* ::Input:: *)
(*maxx=Max[rawData[[All,1]]];*)
(*maxy=Max[rawData[[All,2]]];*)
(**)
(*thresholdBelow=0;*)
(*thresholdAbove=maxx-0;*)
(**)
(*Show[{ListPlot[logAveragedWithMaxDev,PlotStyle->{GrayLevel[0],Directive[Opacity[0.3]]},AxesLabel->{"Log[L]","Log[N]"}]*)
(*,ListPlot[logAveragedWithErrors,PlotStyle->RGBColor[1, 0.55, 1],PlotLegends->{"logAveragedWithErrors"}]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 0, 0.5],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@nlmAveragedWithStdDevs[x]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 0.68, 0.5],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@nlmAveragedWithStdDevsUnconstrained[x]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.85, 0, 0.5],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@nlmAveragedWithStdDevsGlobal[x]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 0.68, 0.8],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@nlmAveragedWithStdDevsUnconstrainedGlobal[x]*)
(**)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0, 0, 1],Dashed},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@(x (dfSLE/.bb->N[b])-1)*)
(*}*)
(*,PlotLabel->Row[{" b = ",b}]*)
(*,PlotRange->{All,{0,All}},AxesOrigin->{1,0},ImageSize->700]*)


(* ::Subsection:: *)
(*Drop both first and/or last few 		WORKS PRETTY WELL (LACKING STATISTICS AT BIG L)*)


(* ::Subsubsection:: *)
(*Linear*)


(* ::Input:: *)
(*maxx=Max[averaged[[All,1]]];*)
(*maxy=Max[averaged[[All,2]]];*)
(**)
(*thresholdBelow=15;*)
(*thresholdAbove=maxx-650;*)
(**)
(*(*Let's drop some*)*)
(*droppedWithMaxDev=Select[logAveragedWithMaxDev,Log[thresholdBelow]<#[[1]]<Log[thresholdAbove]&];*)
(*droppedWithErrors=Select[logAveragedWithErrors,Log[thresholdBelow]<#[[1]]<Log[thresholdAbove]&];*)
(*droppedWithErrorsOnMean=Select[logAveragedWithErrorsOnMean,Log[thresholdBelow]<#[[1]]<Log[thresholdAbove]&];*)
(*(*droppedWithEstimatedStdDevs=Select[logAveragedWithEstimatedStdDevs,Log[thresholdBelow]<#[[1]]<Log[thresholdAbove]&];*)*)
(*(*droppedWithEstimatedStdDevsOnMean=Select[logAveragedWithEstimatedStdDevsOnMean,Log[thresholdBelow]<#[[1]]<Log[thresholdAbove]&];*)*)
(*(**)
(*(*drop specific bad point*)*)
(*Length[droppedWithMaxDev];*)
(*droppedWithMaxDev=DeleteCases[droppedWithMaxDev,_?(6.6<N[#[[1]]]<6.75&)];*)
(*Length[droppedWithMaxDev];*)
(*droppedWithErrors=DeleteCases[droppedWithErrors,_?(6.6<N[#[[1]]]<6.75&)];*)
(*droppedWithErrorsOnMean=DeleteCases[droppedWithErrorsOnMean,_?(6.6<N[#[[1]]]<6.75&)];*)
(*(*droppedWithEstimatedStdDevs=DeleteCases[droppedWithEstimatedStdDevs,_?(6.6<N[#[[1]]]<6.75&)];*)*)
(*(*droppedWithEstimatedStdDevsOnMean=DeleteCases[droppedWithEstimatedStdDevsOnMean,_?(6.6<N[#[[1]]]<6.75&)];*)*)
(**)*)
(**)
(**)
(*lmdroppedWithMaxDev=LinearModelFit[droppedWithMaxDev,x,x,Weights->Automatic];*)
(*lmdroppedWithErrors=NonlinearModelFit[droppedWithErrors,{a+df x,{a<0,1<df<2}},{a,df},x,Weights->Automatic,Method->"NMinimize"]*)
(*lmdroppedWithErrorsOnMean=NonlinearModelFit[droppedWithErrorsOnMean,{a+df x,{a<0,1<df<2}},{a,df},x,Weights->Automatic,Method->"NMinimize"]*)
(*(*lmdroppedWithEstimatedStdDevs=NonlinearModelFit[droppedWithEstimatedStdDevs,{a+df x,{a<0,1<df<2}},{a,df},x,Weights->Automatic,Method->"NMinimize"];*)*)
(*(*lmdroppedWithEstimatedStdDevsOnMean=NonlinearModelFit[droppedWithEstimatedStdDevsOnMean,{a+df x,{a<0,1<df<2}},{a,df},x,Weights->Automatic,Method->"NMinimize"];*)*)
(*(*lmpdropped=LinearModelFit[dropped,fitFuncs,x];*)*)
(**)
(**)
(*Show[{ListPlot[logAveragedWithMaxDev,PlotStyle->{GrayLevel[0],Directive[Opacity[0.3]]},AxesLabel->{"Log[L]","Log[N]"}]*)
(*,ListPlot[droppedWithErrors,PlotStyle->{RGBColor[1, 0.78, 0.13],PointSize->0.005},PlotLegends->PointLegend[{"droppedWithErrors"},LegendMarkerSize->10,LegendMarkers->Graphics[Disk[]]]]*)
(*,ListPlot[droppedWithErrorsOnMean,PlotStyle->{RGBColor[0, 0.78, 1],PointSize->0.005},PlotLegends->PointLegend[{"droppedWithErrorsOnMean"},LegendMarkerSize->10,LegendMarkers->Graphics[Disk[]]]]*)
(*,Plot[#[x],{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 0, 0.5],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#[x]]}],Right]]&@lmdroppedWithMaxDev*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[1, 0.68, 0.6],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@lmdroppedWithErrors[x]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 0.68, 0.5],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@lmdroppedWithErrorsOnMean[x]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0, 0, 1],Dashed},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@(x (dfSLE/.bb->N[b])-1)*)
(*(*,Plot[#,{x,0,Log[thresholdAbove]},PlotStyle->{,Dashed},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@(x (dfSLE/.bb->N[b])-0.7-5Exp[-0.991 x])*)*)
(*}*)
(*,Epilog->{Directive[Dashed,GrayLevel[0.5]],*)
(*Line[{{Log[thresholdBelow],0},{Log[thresholdBelow],Log[maxy]//N}}]*)
(*,Line[{{Log[thresholdAbove],0},{Log[thresholdAbove],Log[maxy]//N}}]}*)
(*,PlotLabel->Row[{" b = ",b}]*)
(*,PlotRange->{All,{0,Log[maxy]}}(*PlotRange->{{Log[thresholdBelow],Log[maxy]},{4,7.2}}*),AxesOrigin->{(*Log[thresholdBelow]*)1,0},ImageSize->700]*)


(* ::Input:: *)
(*Show[{ListPlot[logAveragedWithMaxDev,PlotStyle->{GrayLevel[0],Directive[Opacity[0.3]]},AxesLabel->{"Log[L]","Log[N]"}]*)
(*,ListPlot[droppedWithEstimatedStdDevs,PlotStyle->{RGBColor[1, 0.78, 0.13],PointSize->0.005},PlotLegends->PointLegend[{"droppedWithEstimatedStdDevs"},LegendMarkerSize->10,LegendMarkers->Graphics[Disk[]]]]*)
(*,ListPlot[droppedWithEstimatedStdDevsOnMean,PlotStyle->{RGBColor[0.49, 0.02, 1],PointSize->0.005},PlotLegends->PointLegend[{"droppedWithEstimatedStdDevsOnMean"},LegendMarkerSize->10,LegendMarkers->Graphics[Disk[]]]]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 0.68, 0.5],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@lmdroppedWithEstimatedStdDevs[x]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.85, 0, 0.5],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@lmdroppedWithEstimatedStdDevsOnMean[x]*)
(**)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0, 0, 1],Dashed},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@(x (dfSLE/.bb->N[b])-1)*)
(*(*,Plot[#,{x,0,Log[thresholdAbove]},PlotStyle->{,Dashed},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@(x (dfSLE/.bb->N[b])-0.7-5Exp[-0.991 x])*)*)
(*}*)
(*,Epilog->{Directive[Dashed,GrayLevel[0.5]],*)
(*Line[{{Log[thresholdBelow],0},{Log[thresholdBelow],Log[maxy]//N}}]*)
(*,Line[{{Log[thresholdAbove],0},{Log[thresholdAbove],Log[maxy]//N}}]}*)
(*,PlotLabel->Row[{" b = ",b}]*)
(*,PlotRange->{All,{0,Log[maxy]}}(*PlotRange->{{Log[thresholdBelow],Log[maxy]},{4,7.2}}*),AxesOrigin->{(*Log[thresholdBelow]*)1,0},ImageSize->700]*)


(* ::Input:: *)
(*Sort[droppedWithMaxDev//N]*)


(* ::Subsubsection::Closed:: *)
(*Non-linear*)


(* ::Input:: *)
(*maxx=Max[rawData[[All,1]]];*)
(*maxy=Max[rawData[[All,2]]];*)
(**)
(*thresholdBelow=23;*)
(*thresholdAbove=maxx-500;*)
(**)
(*droppedWithMaxDev=Select[logAveragedWithMaxDev,Log[thresholdBelow]<#[[1]]<Log[thresholdAbove]&];*)
(*droppedWithErrors=Select[logAveragedWithErrors,Log[thresholdBelow]<#[[1]]<Log[thresholdAbove]&];*)
(**)
(*fitFunc=a+c Exp[- x]+df x;*)
(**)
(*nlmdroppedWithMaxDev=NonlinearModelFit[droppedWithMaxDev,fitFunc,{{a,-0.5},{c,-30.},{\[Omega],1.},df(*,{df,1.024}*)},x,MaxIterations->1000,Method->"NMinimize"];*)
(*nlmdroppedWithErrors=NonlinearModelFit[droppedWithErrors,fitFunc,{{a,-0.5},{c,-30.},{\[Omega],1.},df(*,{df,1.024}*)},x,MaxIterations->1000,Method->"NMinimize"];*)
(*(*lmpdropped=LinearModelFit[dropped,fitFuncs,x];*)*)
(**)
(*Show[{ListPlot[logAveragedWithMaxDev,PlotStyle->{GrayLevel[0],Directive[Opacity[0.3]]},AxesLabel->{"Log[L]","Log[N]"}]*)
(*,ListPlot[logAveragedWithErrors,PlotStyle->RGBColor[0, 0.78, 1],PlotLegends->{"logAveragedWithErrors"}]*)
(**)
(*(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{,Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@nlmdroppedWithMaxDev[x]*)*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[1, 0.68, 0.6],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@nlmdroppedWithErrors[x]*)
(*(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{,Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@lmAveragedWithStdDevs[x]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{,Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@lmAveragedWithStdDevsGlobal[x]*)*)
(**)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0, 0, 1],Dashed},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@(x (dfSLE/.bb->N[b])-1)*)
(*,Plot[#,{x,0,Log[thresholdAbove]},PlotStyle->{RGBColor[0, 1, 1],Dashed},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@(x (dfSLE/.bb->N[b])-0.7-5Exp[-0.991 x])*)
(*}*)
(*,Epilog->{Directive[Dashed,GrayLevel[0.5]],*)
(*Line[{{Log[thresholdBelow],0},{Log[thresholdBelow],Log[maxy]//N}}]*)
(*,Line[{{Log[thresholdAbove],0},{Log[thresholdAbove],Log[maxy]//N}}]}*)
(*,PlotLabel->Row[{" b = ",b}]*)
(*,PlotRange->{All,{0,All}}(*PlotRange->{{Log[thresholdBelow],Log[maxy]},{4,7.2}}*),AxesOrigin->{(*Log[thresholdBelow]*)1,0},ImageSize->700]*)


(* ::Subsection::Closed:: *)
(*With and without Method -> "NMinimize" (which looks for the global minimum, I think it's always better)*)


(* ::Item::Closed:: *)
(*No errors*)


(* ::Input:: *)
(*fitFunc=a+c Exp[-\[Omega] x]+df x;*)
(*fitFunc=a-c Exp[- x]+df x;*)
(*nlmAveraged=NonlinearModelFit[logAveraged,{fitFunc(*,{-2<a<2,0<\[Omega]<28}*)},{a,c,(*c,\[Omega],*)df},x];*)
(*nlmAveragedGlobal=NonlinearModelFit[logAveraged,{fitFunc(*,{-2<a<2,0<\[Omega]<28}*)},{a,c,(*c,\[Omega],*)df},x,Method->"NMinimize"];*)
(**)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@nlmAveraged*)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@nlmAveragedGlobal*)


(* ::Input:: *)
(*(*Extract and plot residuals*)*)
(**)
(*nlmAveraged["AdjustedRSquared"]*)
(*ListPlot[nlmAveraged["FitResiduals"],Filling->Axis,PlotLabel->"Residual Analysis"]*)
(**)
(*nlmAveragedGlobal["AdjustedRSquared"]*)
(*ListPlot[nlmAveragedGlobal["FitResiduals"],Filling->Axis,PlotLabel->"Residual Analysis"]*)


(* ::Input:: *)
(*maxx=Max[rawData[[All,1]]];*)
(*maxy=Max[rawData[[All,2]]];*)
(**)
(*thresholdBelow=0;*)
(*thresholdAbove=maxx-0;*)
(**)
(*Show[{ListPlot[logAveragedWithMaxDev,PlotStyle->{GrayLevel[0],Directive[Opacity[0.3]]},AxesLabel->{"Log[L]","Log[N]"}]*)
(*,ListPlot[logAveragedWithErrors,PlotStyle->RGBColor[0, 1, 1]]*)
(*,ListPlot[logAveragedWithEstimatedStdDevs,PlotStyle->{RGBColor[1, 0.55, 1],Directive[Opacity[0.3]]}]*)
(**)
(*(*,Plot[lmp[x],{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{,Thickness->0.004}]*)*)
(**)
(*,Plot[nlmAveraged[x],{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 1, 0.5],Thickness->0.004}]*)
(*,Plot[nlmAveragedGlobal[x],{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 0, 0.5],Thickness->0.004}]*)
(**)
(*,Plot[x (dfSLE/.bb->b)-1,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0, 0, 1],Dashed}]}*)
(*,(*PlotRange->All,*)PlotLabel->Row[{" b = ",b}]*)
(*(*,Epilog->{Directive[Dashed,],*)
(*Line[{{Log[thresholdBelow],0},{Log[thresholdBelow],Log[maxy]//N}}]*)
(*,Line[{{Log[thresholdAbove],0},{Log[thresholdAbove],Log[maxy]//N}}]}*)*)
(*,PlotRange->{{1,All},{0,All}},AxesOrigin->{1,0}]*)
(**)
(**)
(*Print[ (*"Full data - Linear fit : Subscript[d, f]=",Around[Quiet@lm["ParameterTable"][[1]][[1,3,2]],\[Pi]*lm["ParameterErrors"][[2]]],*)"*)
(*\!\(\*TemplateBox[<|\"color\" -> RGBColor[0.5, 1, 0.5]|>,\n\"RGBColorSwatchTemplate\"]\) Averaged data fit without errors and " ,fitFunc,": \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@nlmAveraged["ParameterTable"][[1,1,-1,2]],\[Pi]*Quiet@nlmAveraged["ParameterTable"][[1,1,-1,3]]],"*)
(*\!\(\*TemplateBox[<|\"color\" -> RGBColor[0.5, 0, 0.5]|>,\n\"RGBColorSwatchTemplate\"]\) Averaged data fit without errors, global minimum and " ,fitFunc,": \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@nlmAveragedGlobal["ParameterTable"][[1,1,-1,2]],\[Pi]*Quiet@nlmAveragedGlobal["ParameterTable"][[1,1,-1,3]]],"*)
(**)
(*\!\(\*TemplateBox[<|\"color\" -> RGBColor[0, 0, 1]|>,\n\"RGBColorSwatchTemplate\"]\) Result from the litterature (exact with SLE): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Input:: *)
(*Series[Log[1+x],{x,0,2}]*)


(* ::Input:: *)
(*Quiet@nlmAveragedGlobal["ParameterTable"][[1,1,-1,3]]*)


(* ::Item::Closed:: *)
(*Errors obtained with StandardDeviation[]*)


(* ::Input:: *)
(*fitFunc=a+c Exp[-\[Omega] x]+df x;*)
(*fitFunc=a+c Exp[- x]+df x;*)
(**)
(*nlmAveragedWithStdDevs=NonlinearModelFit[logAveragedWithErrors,{fitFunc,{(*-2<a<2,*)a<0(*,0.5<\[Omega]<=3*),c<0(*,1<df<1.1*)}},{a,c,\[Omega](*{a,-0.5},{c,-30.},{\[Omega],2.}*),df(*,{df,1.083}*)},x,MaxIterations->10000];*)
(*nlmAveragedWithStdDevsUnconstrained=NonlinearModelFit[logAveragedWithErrors,{fitFunc,{(*-2<a<2,*)(*,0.5<\[Omega]<=3*)(*,1<df<1.1*)}},{a,c,\[Omega](*{a,-0.5},{c,-30.},{\[Omega],2.}*),df(*,{df,1.083}*)},x,MaxIterations->10000];*)
(**)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@nlmAveragedWithStdDevs*)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@nlmAveragedWithStdDevsUnconstrained*)
(**)
(*nlmAveragedWithStdDevsGlobal=NonlinearModelFit[logAveragedWithErrors,{fitFunc,{(*-2<a<2,*)a<0(*,0.5<\[Omega]<=3*),c<0(*,1<df<1.1*)}},{a,c,\[Omega](*{a,-0.5},{c,-30.},{\[Omega],2.}*),df(*,{df,1.083}*)},x,MaxIterations->1000,Method->"NMinimize"];*)
(*nlmAveragedWithStdDevsUnconstrainedGlobal=NonlinearModelFit[logAveragedWithErrors,fitFunc,{a,c,\[Omega](*{a,-0.5},{c,-30.},{\[Omega],2.}*),df(*,{df,1.083}*)},x,MaxIterations->1000,Method->"NMinimize"];*)
(**)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@nlmAveragedWithStdDevsGlobal*)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@nlmAveragedWithStdDevsUnconstrainedGlobal*)
(**)


(* ::Input:: *)
(*synchronizedPlots=Map[Show[#,ImageSize->500]&,{ListPlot[nlmAveragedWithStdDevs["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis nlmAveragedWithStdDevs,\nAdjustedRSquared=",nlmAveragedWithStdDevs["AdjustedRSquared"]}]]*)
(*,ListPlot[nlmAveragedWithStdDevsUnconstrained["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis nlmAveragedWithStdDevsUnconstrained,\nAdjustedRSquared=",nlmAveragedWithStdDevsUnconstrained["AdjustedRSquared"]}]]*)
(*,ListPlot[nlmAveragedWithStdDevsGlobal["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis nlmAveragedWithStdDevsGlobal,\nAdjustedRSquared=",nlmAveragedWithStdDevsGlobal["AdjustedRSquared"]}]]*)
(*,ListPlot[nlmAveragedWithStdDevsUnconstrainedGlobal["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis nlmAveragedWithStdDevsUnconstrainedGlobal,\nAdjustedRSquared=",nlmAveragedWithStdDevsUnconstrainedGlobal["AdjustedRSquared"]}]]}];*)
(**)
(*Multicolumn[synchronizedPlots,2,Appearance->"Framed"]*)
(**)


(* ::Input:: *)
(*maxx=Max[rawData[[All,1]]];*)
(*maxy=Max[rawData[[All,2]]];*)
(**)
(*thresholdBelow=0;*)
(*thresholdAbove=maxx-0;*)
(**)
(*Show[{ListPlot[logAveragedWithMaxDev,PlotStyle->{GrayLevel[0],Directive[Opacity[0.3]]},AxesLabel->{"Log[L]","Log[N]"}]*)
(*,ListPlot[logAveragedWithErrors,PlotStyle->RGBColor[1, 0.47000000000000003`, 0],PlotLegends->{"logAveragedWithErrors"}]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 0, 0.5],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@nlmAveragedWithStdDevs[x]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 0.68, 0.5],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@nlmAveragedWithStdDevsUnconstrained[x]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.85, 0, 0.5],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@nlmAveragedWithStdDevsGlobal[x]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 0.68, 0.8],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@nlmAveragedWithStdDevsUnconstrainedGlobal[x]*)
(**)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0, 0, 1],Dashed},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@(x (dfSLE/.bb->N[b])-1)*)
(*}*)
(*,PlotLabel->Row[{" b = ",b}]*)
(*,PlotRange->{All,{0,All}},AxesOrigin->{1,0},ImageSize->700]*)
(**)
(*Print[ "Averaged data fit with errors and " ,fitFunc," \!\(\*TemplateBox[<|\"color\" -> RGBColor[0.5, 0, 0.5]|>,\n\"RGBColorSwatchTemplate\"]\): \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@nlmAveragedWithStdDevs["ParameterTable"][[1]][[1,-1,2]],\[Pi]*nlmAveragedWithStdDevs["ParameterTable"][[1]][[1,-1,3]]],", Parameters:",Quiet@nlmAveragedWithStdDevs["ParameterTable"],"*)
(*Averaged data fit with errors and " ,fitFunc," (unconstrained) \!\(\*TemplateBox[<|\"color\" -> RGBColor[0.5, 0.68, 0.5]|>,\n\"RGBColorSwatchTemplate\"]\): \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@nlmAveragedWithStdDevsUnconstrained["ParameterTable"][[1]][[1,-1,2]],\[Pi]*nlmAveragedWithStdDevsUnconstrained["ParameterTable"][[1]][[1,-1,3]]],", Parameters:",Quiet@nlmAveragedWithStdDevsUnconstrained["ParameterTable"],"*)
(**)
(*Result from the litterature (exact with SLE) \!\(\*TemplateBox[<|\"color\" -> RGBColor[0, 0, 1]|>,\n\"RGBColorSwatchTemplate\"]\): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Item::Closed:: *)
(*Errors obtained with StdDevEstimate[]*)


(* ::Input:: *)
(*fitFunc=a+c Exp[-\[Omega] x]+df x;*)
(*(*fitFunc=a+c Exp[- x]+df x;*)*)
(**)
(*nlmAveragedWithEstimatedStdDevs=NonlinearModelFit[logAveragedWithEstimatedStdDevs,{fitFunc,{(*-2<a<2,*)a<0,0.5<\[Omega]<=3,c<0(*,1<df<1.1*)}},{a,c,\[Omega](*{a,-0.5},{c,-30.},{\[Omega],2.}*),df(*,{df,1.083}*)},x,MaxIterations->1000];*)
(*nlmAveragedWithEstimatedStdDevsUnconstrained=NonlinearModelFit[logAveragedWithEstimatedStdDevs,{fitFunc,{(*-2<a<2,*)0.5<\[Omega]<=30(*,1<df<1.1*)}},{a,c,\[Omega](*{a,-0.5},{c,-30.},{\[Omega],2.}*),df(*,{df,1.083}*)},x,MaxIterations->1000];*)
(**)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@nlmAveragedWithEstimatedStdDevs*)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@nlmAveragedWithEstimatedStdDevsUnconstrained*)
(**)
(*nlmAveragedWithEstimatedStdDevsGlobal=NonlinearModelFit[logAveragedWithEstimatedStdDevs,{fitFunc,{(*-2<a<2,*)a<0,0.5<\[Omega]<=30,c<0(*,1<df<1.1*)}},{a,c,\[Omega](*{a,-0.5},{c,-30.},{\[Omega],2.}*),df(*,{df,1.083}*)},x,MaxIterations->1000,Method->"NMinimize"];*)
(*nlmAveragedWithEstimatedStdDevsUnconstrainedGlobal=NonlinearModelFit[logAveragedWithEstimatedStdDevs,{fitFunc,\[Omega]<=3},{a,c,\[Omega](*{a,-0.5},{c,-30.},{\[Omega],2.}*),df(*,{df,1.083}*)},x,MaxIterations->1000,Method->"NMinimize"];*)
(**)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@nlmAveragedWithEstimatedStdDevsGlobal*)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@nlmAveragedWithEstimatedStdDevsUnconstrainedGlobal*)
(**)


(* ::Input:: *)
(*synchronizedPlots=Map[Show[#,ImageSize->500]&,{ListPlot[nlmAveragedWithEstimatedStdDevs["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis nlmAveragedWithEstimatedStdDevs,\nAdjustedRSquared=",nlmAveragedWithEstimatedStdDevs ["AdjustedRSquared"]}]]*)
(*,ListPlot[nlmAveragedWithEstimatedStdDevsUnconstrained["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis nlmAveragedWithEstimatedStdDevsUnconstrained,\nAdjustedRSquared=",nlmAveragedWithEstimatedStdDevsUnconstrained["AdjustedRSquared"]}]]*)
(*,ListPlot[nlmAveragedWithEstimatedStdDevsGlobal["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis nlmAveragedWithEstimatedStdDevsGlobal,\nAdjustedRSquared=",nlmAveragedWithEstimatedStdDevsGlobal["AdjustedRSquared"]}]]*)
(*,ListPlot[nlmAveragedWithEstimatedStdDevsUnconstrainedGlobal["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis nlmAveragedWithEstimatedStdDevsUnconstrainedGlobal,\nAdjustedRSquared=",nlmAveragedWithEstimatedStdDevsUnconstrainedGlobal["AdjustedRSquared"]}]]}];*)
(**)
(*Multicolumn[synchronizedPlots,2,Appearance->"Framed"]*)
(**)


(* ::Input:: *)
(*maxx=Max[rawData[[All,1]]];*)
(*maxy=Max[rawData[[All,2]]];*)
(**)
(*thresholdBelow=0;*)
(*thresholdAbove=maxx-0;*)
(**)
(*Show[{ListPlot[logAveragedWithMaxDev,PlotStyle->{GrayLevel[0],Directive[Opacity[0.3]]},AxesLabel->{"Log[L]","Log[N]"}]*)
(*,ListPlot[logAveragedWithEstimatedStdDevs,PlotStyle->RGBColor[1, 0.47000000000000003`, 0],PlotLegends->{"logAveragedWithEstimatedStdDevs"}]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 0, 0.5],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@nlmAveragedWithEstimatedStdDevs[x]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 0.68, 0.5],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@nlmAveragedWithEstimatedStdDevsUnconstrained[x]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.85, 0, 0.5],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@nlmAveragedWithEstimatedStdDevsGlobal[x]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 0.68, 0.8],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@nlmAveragedWithEstimatedStdDevsUnconstrainedGlobal[x]*)
(**)
(*,Plot[x (dfSLE/.bb->b)-1,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0, 0, 1],Dashed}]*)
(*}*)
(*,PlotLabel->Row[{" b = ",b}]*)
(*,PlotRange->{All,{0,All}},AxesOrigin->{1,0},ImageSize->700]*)
(**)
(*Print[ "Averaged data fit with errors and " ,fitFunc," \!\(\*TemplateBox[<|\"color\" -> RGBColor[0.5, 0, 0.5]|>,\n\"RGBColorSwatchTemplate\"]\): \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@nlmAveragedWithEstimatedStdDevs["ParameterTable"][[1]][[1,-1,2]],\[Pi]*nlmAveragedWithEstimatedStdDevs["ParameterTable"][[1]][[1,-1,3]]],", Parameters:",Quiet@nlmAveragedWithEstimatedStdDevs["ParameterTable"],"*)
(*Averaged data fit with errors and " ,fitFunc," (unconstrained) \!\(\*TemplateBox[<|\"color\" -> RGBColor[0.5, 0.68, 0.5]|>,\n\"RGBColorSwatchTemplate\"]\): \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@nlmAveragedWithEstimatedStdDevsUnconstrained["ParameterTable"][[1]][[1,-1,2]],\[Pi]*nlmAveragedWithEstimatedStdDevsUnconstrained["ParameterTable"][[1]][[1,-1,3]]],", Parameters:",Quiet@nlmAveragedWithEstimatedStdDevsUnconstrained["ParameterTable"],"*)
(**)
(*Result from the litterature (exact with SLE) \!\(\*TemplateBox[<|\"color\" -> RGBColor[0, 0, 1]|>,\n\"RGBColorSwatchTemplate\"]\): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Item::Closed:: *)
(*Errors obtained with MaxDev[]*)


(* ::Input:: *)
(*fitFunc=a+c Exp[-\[Omega] x]+df x;*)
(*fitFunc=a+c Exp[- x]+df x;*)
(**)
(*nlmAveragedWithMaxStdDevs=NonlinearModelFit[logAveragedWithMaxDev,{fitFunc,{(*-2<a<2,*)a<0(*,0.5<\[Omega]<=3*),c<0(*,1<df<1.1*)}},{a,c,\[Omega](*{a,-0.5},{c,-30.},{\[Omega],2.}*),df(*,{df,1.083}*)},x,MaxIterations->10000];*)
(*nlmAveragedWithMaxStdDevsUnconstrained=NonlinearModelFit[logAveragedWithMaxDev,{fitFunc,{(*-2<a<2,*)(*,0.5<\[Omega]<=3*)(*,1<df<1.1*)}},{a,c,\[Omega](*{a,-0.5},{c,-30.},{\[Omega],2.}*),df(*,{df,1.083}*)},x,MaxIterations->10000];*)
(**)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@nlmAveragedWithMaxStdDevs*)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@nlmAveragedWithMaxStdDevsUnconstrained*)
(**)
(*nlmAveragedWithMaxStdDevsGlobal=NonlinearModelFit[logAveragedWithMaxDev,{fitFunc,{(*-2<a<2,*)a<0(*,0.5<\[Omega]<=3*),c<0(*,1<df<1.1*)}},{a,c,\[Omega](*{a,-0.5},{c,-30.},{\[Omega],2.}*),df(*,{df,1.083}*)},x,MaxIterations->1000,Method->"NMinimize"];*)
(*nlmAveragedWithMaxStdDevsUnconstrainedGlobal=NonlinearModelFit[logAveragedWithMaxDev,fitFunc,{a,c,\[Omega](*{a,-0.5},{c,-30.},{\[Omega],2.}*),df(*,{df,1.083}*)},x,MaxIterations->1000,Method->"NMinimize"];*)
(**)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@nlmAveragedWithMaxStdDevsGlobal*)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@nlmAveragedWithMaxStdDevsUnconstrainedGlobal*)
(**)


(* ::Input:: *)
(*synchronizedPlots=Map[Show[#,ImageSize->500]&,{ListPlot[nlmAveragedWithMaxStdDevs["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis nlmAveragedWithMaxStdDevs,\nAdjustedRSquared=",nlmAveragedWithMaxStdDevs ["AdjustedRSquared"]}]]*)
(*,ListPlot[nlmAveragedWithMaxStdDevsUnconstrained["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis nlmAveragedWithMaxStdDevsUnconstrained,\nAdjustedRSquared=",nlmAveragedWithMaxStdDevsUnconstrained["AdjustedRSquared"]}]]*)
(*,ListPlot[nlmAveragedWithMaxStdDevsGlobal["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis nlmAveragedWithMaxStdDevsGlobal,\nAdjustedRSquared=",nlmAveragedWithMaxStdDevsGlobal["AdjustedRSquared"]}]]*)
(*,ListPlot[nlmAveragedWithMaxStdDevsUnconstrainedGlobal["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis nlmAveragedWithMaxStdDevsUnconstrainedGlobal,\nAdjustedRSquared=",nlmAveragedWithMaxStdDevsUnconstrainedGlobal["AdjustedRSquared"]}]]}];*)
(**)
(*Multicolumn[synchronizedPlots,2,Appearance->"Framed"]*)
(**)


(* ::Input:: *)
(*maxx=Max[rawData[[All,1]]];*)
(*maxy=Max[rawData[[All,2]]];*)
(**)
(*thresholdBelow=0;*)
(*thresholdAbove=maxx-0;*)
(**)
(*Show[{ListPlot[logAveragedWithMaxDev,PlotStyle->{GrayLevel[0],Directive[Opacity[0.3]]},AxesLabel->{"Log[L]","Log[N]"}]*)
(*,ListPlot[logAveragedWithMaxDev,PlotStyle->RGBColor[1, 0.47000000000000003`, 0],PlotLegends->{"logAveragedWithMaxDev"}]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 0, 0.5],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@nlmAveragedWithMaxStdDevs[x]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 0.68, 0.5],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@nlmAveragedWithMaxStdDevsUnconstrained[x]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.85, 0, 0.5],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@nlmAveragedWithMaxStdDevsGlobal[x]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 0.68, 0.8],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@nlmAveragedWithMaxStdDevsUnconstrainedGlobal[x]*)
(**)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0, 0, 1],Dashed},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@(x (dfSLE/.bb->N[b])-1)*)
(*}*)
(*,PlotLabel->Row[{" b = ",b}]*)
(*,PlotRange->{All,{0,All}},AxesOrigin->{1,0},ImageSize->700]*)
(**)
(*Print[ "Averaged data fit with errors and " ,fitFunc," \!\(\*TemplateBox[<|\"color\" -> RGBColor[0.5, 0, 0.5]|>,\n\"RGBColorSwatchTemplate\"]\): \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@nlmAveragedWithMaxStdDevs["ParameterTable"][[1]][[1,-1,2]],\[Pi]*nlmAveragedWithMaxStdDevs["ParameterTable"][[1]][[1,-1,3]]],", Parameters:",Quiet@nlmAveragedWithMaxStdDevs["ParameterTable"],"*)
(*Averaged data fit with errors and " ,fitFunc," (unconstrained) \!\(\*TemplateBox[<|\"color\" -> RGBColor[0.5, 0.68, 0.5]|>,\n\"RGBColorSwatchTemplate\"]\): \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@nlmAveragedWithMaxStdDevsUnconstrained["ParameterTable"][[1]][[1,-1,2]],\[Pi]*nlmAveragedWithMaxStdDevsUnconstrained["ParameterTable"][[1]][[1,-1,3]]],", Parameters:",Quiet@nlmAveragedWithMaxStdDevsUnconstrained["ParameterTable"],"*)
(**)
(*Result from the litterature (exact with SLE) \!\(\*TemplateBox[<|\"color\" -> RGBColor[0, 0, 1]|>,\n\"RGBColorSwatchTemplate\"]\): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Subsubsection::Closed:: *)
(*Forgot what this is*)


(* ::Input:: *)
(*fitFuncs=Table[x^(-i),{i,-1,3}];*)
(*(*fitFuncs=Append[Table[x^(-i),{i,-1,0}],Exp[-x]];*)*)
(*(*fitFuncs={x,1,Exp[-x],Exp[-2 x]};(*Exp[-1.3 x]*)*)*)
(**)
(*fitFunc=a+c Exp[-\[Omega] x]+df x;*)
(**)
(*lmAveraged=LinearModelFit[logAveragedWithErrors,x,x(*,Weights->Automatic*)];*)
(*Print[Row[{"lmAveraged: ",#//Normal}]]&@%*)
(*lmMaxDev=LinearModelFit[logAveragedWithMaxDev,x,x(*,Weights->Automatic*)];*)
(*Print[Row[{"lmMaxDev: ",#//Normal}]]&@%*)
(**)
(**)
(*fitFuncs={x,1,Exp[-6x]};*)
(**)
(*lmpAveraged=LinearModelFit[logAveragedWithErrors,fitFuncs,x];*)
(*Print[Row[{"\nlmpAveraged: ",#//Normal}]]&@%*)
(*lmpMaxDev=LinearModelFit[logAveragedWithMaxDev,fitFuncs,x];*)
(*Print[Row[{"lmpMaxDev: ",#//Normal}]]&@%*)
(*lmpWithEstimatedStdDevs=LinearModelFit[logAveragedWithEstimatedStdDevs,fitFuncs,x];*)
(*Print[Row[{"lmpMaxDev: ",#//Normal}]]&@%*)
(**)
(*fitFuncs={x,1,Exp[-x]};*)
(**)
(*lmpAveraged=LinearModelFit[logAveragedWithErrors,fitFuncs,x];*)
(*Print[Row[{"\nlmpAveraged: ",#//Normal}]]&@%*)
(*lmpMaxDev=LinearModelFit[logAveragedWithMaxDev,fitFuncs,x];*)
(*Print[Row[{"lmpMaxDev: ",#//Normal}]]&@%*)
(*lmpWithEstimatedStdDevs=LinearModelFit[logAveragedWithEstimatedStdDevs,fitFuncs,x];*)
(*Print[Row[{"lmpMaxDev: ",#//Normal}]]&@%*)
(**)


(* ::Input:: *)
(*fitFunc=a+c Exp[-\[Omega] x]+c2 Exp[-\[Omega]2 x]+df x;*)
(**)
(*nlmAveraged=NonlinearModelFit[logAveragedWithErrors,{fitFunc,{-2<a<2,2.<=\[Omega]<=3.,0.1<=\[Omega]2<=1.}},{{a,-0.5},{c,-30.},\[Omega],c2,\[Omega]2,df(*{df,1.083}*)},x,MaxIterations->1000];*)
(*Print[Row[{"nlmAveraged2 with " ,fitFunc ": ",#//Normal}]]&@%*)
(**)
(*NonlinearModelFit[logAveragedWithMaxDev,{fitFunc(*,{-2<a<2,2.<=\[Omega]<=3.,0.1<=\[Omega]2<=1.}*)},{a,c,\[Omega],c2,\[Omega]2,df(*{df,1.083}*)},x,MaxIterations->1000];*)
(*Print[Row[{"nlmMaxDev without constraints: ",#//Normal}]]&@%*)
(**)
(**)
(*nlmMaxDev=NonlinearModelFit[logAveragedWithMaxDev,{fitFunc,{-2<a<2,2.<=\[Omega]<=3.,0.1<=\[Omega]2<=1.}},{{a,-0.5},{c,-30.},\[Omega],c2,\[Omega]2,df(*{df,1.083}*)},x,MaxIterations->1000];*)
(*Print[Row[{"nlmMaxDev: ",#//Normal}]]&@%*)
(**)
(*Print["Result from the litterature (exact with SLE) : ",(dfSLE/.bb->b)," = ",Style[(dfSLE/.bb->b/1.),RGBColor[0, 0, 1]]]*)


(* ::Input:: *)
(*(*Extract and plot residuals*)*)
(**)
(*nlmMaxDev["AdjustedRSquared"]*)
(*ListPlot[nlmMaxDev["FitResiduals"],Filling->Axis,PlotLabel->"Residual Analysis"]*)


(* ::Input:: *)
(*maxx=Max[rawData[[All,1]]];*)
(*maxy=Max[rawData[[All,2]]];*)
(**)
(*thresholdBelow=0;*)
(*thresholdAbove=maxx-0;*)
(**)
(*Show[{ListPlot[logAveragedWithMaxDev,PlotStyle->{GrayLevel[0],Directive[Opacity[0.3]]},AxesLabel->{"Log[L]","Log[N]"}]*)
(*,ListPlot[logAveragedWithEstimatedStdDevs,PlotStyle->RGBColor[1, 0.55, 1]]*)
(*,ListPlot[logAveragedWithErrors,PlotStyle->{RGBColor[0, 1, 1],Directive[Opacity[0.3]]}]*)
(*(*,Plot[lmp[x],{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{,Thickness->0.004}]*)*)
(*,Plot[lmAveraged[x],{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[1, 0, 0],Thickness->0.004}]*)
(*,Plot[lmpAveraged[x],{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 0, 0.5],Thickness->0.004}]*)
(*,Plot[fitFunc/. fitSol,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[1, Rational[2, 3], 1],Thickness->0.004}]*)
(*,Plot[nlmAveraged[x],{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 1, 0.5],Thickness->0.004}]*)
(**)
(*,Plot[nlmMaxDev[x],{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.76, 0.63, 0.19],Thickness->0.004}]*)
(**)
(*,Plot[x (dfSLE/.bb->b)-1,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0, 0, 1],Dashed}]}*)
(*,(*PlotRange->All,*)PlotLabel->Row[{" b = ",b}]*)
(*(*,Epilog->{Directive[Dashed,],*)
(*Line[{{Log[thresholdBelow],0},{Log[thresholdBelow],Log[maxy]//N}}]*)
(*,Line[{{Log[thresholdAbove],0},{Log[thresholdAbove],Log[maxy]//N}}]}*)*)
(*,PlotRange->{{7,7.5},{6.5,All}},AxesOrigin->{1,0}]*)
(**)
(**)
(**)
(*Print[ (*"Full data - Linear fit : Subscript[d, f]=",Around[Quiet@lm["ParameterTable"][[1]][[1,3,2]],\[Pi]*lm["ParameterErrors"][[2]]],*)"*)
(*Averaged data fit with errors \!\(\*TemplateBox[<|\"color\" -> RGBColor[1, 0, 0]|>,\n\"RGBColorSwatchTemplate\"]\): \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lmAveraged["ParameterTable"][[1]][[1,3,2]],\[Pi]*lmAveraged["ParameterErrors"][[2]]],"*)
(*Averaged data fit with errors and " ,Total@fitFuncs," \!\(\*TemplateBox[<|\"color\" -> RGBColor[0.5, 0, 0.5]|>,\n\"RGBColorSwatchTemplate\"]\): \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lmpAveraged["ParameterTable"][[1]][[1,2,2]],\[Pi]*lmpAveraged["ParameterErrors"][[1]]],"*)
(**)
(*",fitFunc,"  fit  \!\(\*TemplateBox[<|\"color\" -> RGBColor[1, Rational[2, 3], 1]|>,\n\"RGBColorSwatchTemplate\"]\): \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[df/.fitSol,Null],", Parameters:",fitSol,"*)
(*Averaged data fit with errors and " ,fitFunc,"nonlinearModel \!\(\*TemplateBox[<|\"color\" -> RGBColor[0.5, 1, 0.5]|>,\n\"RGBColorSwatchTemplate\"]\): \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@nlmAveraged["ParameterTable"][[1,1,5,2]],\[Pi]*Quiet@nlmAveraged["ParameterTable"][[1,1,5,3]]],"*)
(**)
(*Averaged data fit with MaxDevs and " ,fitFunc,"nonlinearModel \!\(\*TemplateBox[<|\"color\" -> RGBColor[0.76, 0.63, 0.19]|>,\n\"RGBColorSwatchTemplate\"]\): \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@nlmMaxDev["ParameterTable"][[1,1,-1,2]],(*\[Pi]**)Quiet@nlmMaxDev["ParameterTable"][[1,1,-1,3]]],", Parameters:",Quiet@nlmMaxDev["ParameterTable"],(*,"*)
(**)
(*Full data - ",Total@fitFuncs," fit: Subscript[d, f]=",Around[Quiet@lmp["ParameterTable"][[1]][[1,2,2]],\[Pi]*lmp["ParameterErrors"][[1]]]*)"*)
(**)
(*Result from the litterature (exact with SLE) \!\(\*TemplateBox[<|\"color\" -> RGBColor[0, 0, 1]|>,\n\"RGBColorSwatchTemplate\"]\): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Input:: *)
(*Quiet@lmpAveraged["ParameterTable"][[1]]*)
(*Quiet@nlmAveraged["ParameterTable"][[1]]*)


(* ::Input:: *)
(*Quiet@nlmMaxDev["ParameterTable"][[1,All,1;;3]]//Normal*)


(* ::Subsection::Closed:: *)
(*Scan through different shifts to find the best linear fit, i.e. that maximizes AdjustedRSquared*)
(*	TBD*)


(* ::Subsubsection::Closed:: *)
(*With errors from StandardDeviation[]*)


(* ::Input:: *)
(*shift:=Plus[{-4,0},#]&;*)
(**)
(*logAveragedWithErrorsShifted=Log[shift/@averagedWithErrors]/. {a_,0}:>{a,Around[1.0`*^-6,1.0`*^-6]};*)
(*fitFunc=a+df  x;lmAveragedWithErrorsShifted=NonlinearModelFit[Select[logAveragedWithErrorsShifted,#[[1]]>=0&],{fitFunc(*,{-2<a<2,0<\[Omega]<28}*)},{a,(*c,\[Omega],*)df},x]*)


(* ::Input:: *)
(*listOfFits=Table[*)
(*shift:=Plus[{-i,0},#]&;*)
(**)
(*logAveragedWithErrorsShifted=Log[shift/@averagedWithErrors]/. {a_,0}:>{a,Around[1.0`*^-6,1.0`*^-6]};*)
(*fitFunc=a+df  x;lmAveragedWithErrorsShifted=NonlinearModelFit[Select[logAveragedWithErrorsShifted,#[[1]]>=0&],{fitFunc(*,{-2<a<2,0<\[Omega]<28}*)},{a,(*c,\[Omega],*)df},x]*)
(*,*)
(*{i,-5,5,0.1}];*)


(* ::Input:: *)
(*listOfFits[[50;;60]]*)


(* ::Input:: *)
(*Length[listOfFits]*)


(* ::Input:: *)
(*#["AdjustedRSquared"]&/@listOfFits*)
(*best=Ordering[%,-1]*)
(*listOfFits[[%]]*)


(* ::Input:: *)
(*(best[[1]]-51)/10//N;*)
(*Print["Best position is with shift ",%]*)
(*shift:=Plus[{-(best[[1]]-51)/10//N,0},#]&*)
(**)
(*logAveragedWithMaxDevShifted=Log[shift/@averagedWithMaxDev]/.{a_,0}->{a,Around[1.0`*^-6,1.0`*^-6]};*)
(*logAveragedWithErrorsShifted=Log[shift/@averagedWithErrors]/. {a_,0}->{a,Around[1.0`*^-6,1.0`*^-6]};*)
(**)
(**)
(*Show[{ListPlot[logAveragedWithMaxDevShifted,PlotStyle->{GrayLevel[0],Directive[Opacity[0.3]]},AxesLabel->{Row[{"Log[L-",(best[[1]]-51)/10//N,"]"}],"Log[N]"}]*)
(*,ListPlot[logAveragedWithErrorsShifted,PlotStyle->RGBColor[0, 1, 1]]*)
(*(*,ListPlot[logAveragedWithEstimatedStdDevsShifted,PlotStyle->{,Directive[Opacity[0.3]]}]*)*)
(**)
(*(*,Plot[lmp[x],{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{,Thickness->0.004}]*)*)
(**)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 0, 0.5],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],{Right,Top}]]&@(listOfFits[[best[[1]]]][x])*)
(**)
(*,Plot[x (dfSLE/.bb->b)-1,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0, 0, 1],Dashed}]}*)
(*,(*PlotRange->All,*)PlotLabel->Row[{" b = ",b}]*)
(*(*,Epilog->{Directive[Dashed,],*)
(*Line[{{Log[thresholdBelow],0},{Log[thresholdBelow],Log[maxy]//N}}]*)
(*,Line[{{Log[thresholdAbove],0},{Log[thresholdAbove],Log[maxy]//N}}]}*)*)
(*,PlotRange->{All,{0,All}},AxesOrigin->{1,0}]*)
(**)
(*ListPlot[listOfFits[[best[[1]]]]["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis, AdjustedRSquared=",listOfFits[[best[[1]]]]["AdjustedRSquared"]}]]*)
(**)


(* ::Subsubsection::Closed:: *)
(*With errors from MaxDev[]*)


(* ::Input:: *)
(*listOfFits=Table[*)
(*shift:=Plus[{-i,0},#]&;*)
(**)
(*logAveragedWithMaxDevShifted=Log[shift/@averagedWithErrors]/. {a_,0}->{a,Around[1.0`*^-6,1.0`*^-6]};*)
(*fitFunc=a+df  x;lmAveragedWithMaxDevShifted=NonlinearModelFit[Select[logAveragedWithMaxDevShifted,#[[1]]>0&],{fitFunc(*,{-2<a<2,0<\[Omega]<28}*)},{a,(*c,\[Omega],*)df},x]*)
(*,*)
(*{i,-5,5,0.1}];*)


(* ::Input:: *)
(*Length[listOfFits]*)


(* ::Input:: *)
(*#["AdjustedRSquared"]&/@listOfFits*)
(*best=Ordering[%,-1]*)
(*listOfFits[[%]]*)


(* ::Input:: *)
(*(best[[1]]-51)/10//N;*)
(*Print["Best position is with shift ",%]*)
(*shift:=Plus[{-(best[[1]]-51)/10//N,0},#]&*)
(**)
(*logAveragedWithMaxDevShifted=Log[shift/@averagedWithMaxDev]/.{a_,0}->{a,Around[1.0`*^-6,1.0`*^-6]};*)
(*logAveragedWithErrorsShifted=Log[shift/@averagedWithErrors]/. {a_,0}->{a,Around[1.0`*^-6,1.0`*^-6]};*)
(**)
(**)
(*Show[{ListPlot[logAveragedWithMaxDevShifted,PlotStyle->{GrayLevel[0],Directive[Opacity[0.3]]},AxesLabel->{Row[{"Log[L-",(best[[1]]-51)/10//N,"]"}],"Log[N]"}]*)
(*,ListPlot[logAveragedWithErrorsShifted,PlotStyle->RGBColor[0, 1, 1]]*)
(*(*,ListPlot[logAveragedWithEstimatedStdDevsShifted,PlotStyle->{,Directive[Opacity[0.3]]}]*)*)
(**)
(*(*,Plot[lmp[x],{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{,Thickness->0.004}]*)*)
(**)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 0, 0.5],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],{Right,Top}]]&@(listOfFits[[best[[1]]]][x])*)
(**)
(*,Plot[x (dfSLE/.bb->b)-1,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0, 0, 1],Dashed}]}*)
(*,(*PlotRange->All,*)PlotLabel->Row[{" b = ",b}]*)
(*(*,Epilog->{Directive[Dashed,],*)
(*Line[{{Log[thresholdBelow],0},{Log[thresholdBelow],Log[maxy]//N}}]*)
(*,Line[{{Log[thresholdAbove],0},{Log[thresholdAbove],Log[maxy]//N}}]}*)*)
(*,PlotRange->{All,{0,All}},AxesOrigin->{1,0}]*)
(**)
(*ListPlot[listOfFits[[best[[1]]]]["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis, AdjustedRSquared=",listOfFits[[best[[1]]]]["AdjustedRSquared"]}]]*)
(**)


(* ::Subsection::Closed:: *)
(*Take the Log WITH A SHIFT*)


(* ::Input:: *)
(*shift:=Plus[{-4,0},#]&; (*In the code, the stopping condition is with R-1*)*)
(**)
(*logAveragedShifted=Log[shift/@averaged];*)
(*logAveragedWithErrorsShifted=Log[shift/@averagedWithErrors]/. {a_,0}->{a,Around[1.0`*^-6,1.0`*^-6]};*)
(*logAveragedWithEstimatedStdDevsShifted=Log[shift/@averagedWithEstimatedStdDevs]/. {a_,0}->{a,Around[1.0`*^-6,1.0`*^-6]};*)
(*logAveragedWithMaxDevShifted=Log[shift/@averagedWithMaxDev]/.{a_,0}->{a,Around[1.0`*^-6,1.0`*^-6]};*)


(* ::Input:: *)
(*fitFunc=a+df x;*)
(*lmAveragedShifted=NonlinearModelFit[logAveragedShifted,{fitFunc(*,{-2<a<2,0<\[Omega]<28}*)},{a,(*c,\[Omega],*)df},x];*)
(*lmAveragedWithErrorsShifted=NonlinearModelFit[Select[logAveragedWithErrorsShifted,#[[1]]>0&],{fitFunc(*,{-2<a<2,0<\[Omega]<28}*)},{a,(*c,\[Omega],*)df},x];*)
(*(*lmAveragedWithEstimatedStdDevsShifted=NonlinearModelFit[Select[logAveragedWithEstimatedStdDevsShifted,#[[1]]>0&],{fitFunc(*,{-2<a<2,0<\[Omega]<28}*)},{a,(*c,\[Omega],*)df},x];*)*)
(*lmAveragedWithMaxDevShifted=NonlinearModelFit[Select[logAveragedWithMaxDevShifted,#[[1]]>0&],{fitFunc(*,{-2<a<2,0<\[Omega]<28}*)},{a,(*c,\[Omega],*)df},x];*)
(**)
(**)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@lmAveragedShifted*)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@lmAveragedWithErrorsShifted *)
(*(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@lmAveragedWithEstimatedStdDevsShifted *)*)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@lmAveragedWithMaxDevShifted*)


(* ::Input:: *)
(*synchronizedPlots=Map[Show[#,ImageSize->500]&,{ListPlot[lmAveragedShifted["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis, AdjustedRSquared=",lmAveragedShifted["AdjustedRSquared"]}]]*)
(*,ListPlot[lmAveragedWithErrorsShifted["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis, AdjustedRSquared=",lmAveragedWithErrorsShifted["AdjustedRSquared"]}]]*)
(*(*,ListPlot[lmAveragedWithEstimatedStdDevsShifted["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis, AdjustedRSquared=",lmAveragedWithEstimatedStdDevsShifted["AdjustedRSquared"]}]]*)*)
(*,ListPlot[lmAveragedWithMaxDevShifted["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis, AdjustedRSquared=",lmAveragedWithMaxDevShifted["AdjustedRSquared"]}]]}];*)
(**)
(*Multicolumn[synchronizedPlots,2,Appearance->"Framed"]*)
(**)


(* ::Input:: *)
(*maxx=Max[rawData[[All,1]]];*)
(*maxy=Max[rawData[[All,2]]];*)
(**)
(*thresholdBelow=0;*)
(*thresholdAbove=maxx-0;*)
(**)
(*Show[{ListPlot[logAveragedWithMaxDevShifted,PlotStyle->{GrayLevel[0],Directive[Opacity[0.3]]},AxesLabel->{"Log[L-4]","Log[N]"}]*)
(*,(*ListPlot[logAveragedWithErrorsShifted,PlotStyle->]*)ListPlot[logAveragedWithErrorsShifted,PlotStyle->RGBColor[0, 0.78, 1],PlotLegends->{"logAveragedWithErrorsShifted"}]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 0.68, 0.5],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@lmAveragedShifted[x]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.85, 0, 0.5],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@lmAveragedWithErrorsShifted[x]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0, 0, 1],Dashed},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@(x (dfSLE/.bb->N[b])-1)}*)
(*,(*PlotRange->All,*)PlotLabel->Row[{" b = ",b}]*)
(*(*,Epilog->{Directive[Dashed,],*)
(*Line[{{Log[thresholdBelow],0},{Log[thresholdBelow],Log[maxy]//N}}]*)
(*,Line[{{Log[thresholdAbove],0},{Log[thresholdAbove],Log[maxy]//N}}]}*)*)
(*,PlotRange->{{1,All},{0,All}},AxesOrigin->{1,0},ImageSize->Large]*)


(* ::Subsubsection::Closed:: *)
(*Drop some*)


(* ::Input:: *)
(*maxx=Max[rawData[[All,1]]];*)
(*maxy=Max[rawData[[All,2]]];*)
(**)
(*thresholdBelow=70;*)
(*thresholdAbove=maxx-0;*)
(**)
(**)
(**)
(*fitFunc=a+df x;*)
(**)
(*lmAveragedShiftedDropped=NonlinearModelFit[Select[logAveragedShifted,#[[1]]>Log[thresholdBelow]&],{fitFunc(*,{-2<a<2,0<\[Omega]<28}*)},{a,(*c,\[Omega],*)df},x,Method->"NMinimize"];*)
(*lmAveragedWithErrorsShiftedDropped=NonlinearModelFit[Select[logAveragedWithErrorsShifted,#[[1]]>Log[thresholdBelow]&],{fitFunc(*,{-2<a<2,0<\[Omega]<28}*)},{a,(*c,\[Omega],*)df},x,Method->"NMinimize"];*)
(*(*lmAveragedWithEstimatedStdDevsShifted=NonlinearModelFit[Select[logAveragedWithEstimatedStdDevsShifted,#[[1]]>0&],{fitFunc(*,{-2<a<2,0<\[Omega]<28}*)},{a,(*c,\[Omega],*)df},x];*)*)
(*lmAveragedWithMaxDevShiftedDropped=NonlinearModelFit[Select[logAveragedWithMaxDevShifted,#[[1]]>Log[thresholdBelow]&],{fitFunc(*,{-2<a<2,0<\[Omega]<28}*)},{a,(*c,\[Omega],*)df},x,Method->"NMinimize"];*)
(**)
(**)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@lmAveragedShiftedDropped*)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@lmAveragedWithErrorsShiftedDropped *)
(*(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@lmAveragedWithEstimatedStdDevsShiftedDropped *)*)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@lmAveragedWithMaxDevShiftedDropped*)


(* ::Input:: *)
(*synchronizedPlots=Map[Show[#,ImageSize->500]&,{ListPlot[lmAveragedShiftedDropped["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis, AdjustedRSquared=",lmAveragedShiftedDropped["AdjustedRSquared"]}]]*)
(*,ListPlot[lmAveragedWithErrorsShiftedDropped["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis, AdjustedRSquared=",lmAveragedWithErrorsShiftedDropped["AdjustedRSquared"]}]]*)
(*(*,ListPlot[lmAveragedWithEstimatedStdDevsShiftedDropped["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis, AdjustedRSquared=",lmAveragedWithEstimatedStdDevsShiftedDropped["AdjustedRSquared"]}]]*)*)
(*,ListPlot[lmAveragedWithMaxDevShiftedDropped["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis, AdjustedRSquared=",lmAveragedWithMaxDevShiftedDropped["AdjustedRSquared"]}]]}];*)
(**)
(*Multicolumn[synchronizedPlots,2,Appearance->"Framed"]*)
(**)


(* ::Input:: *)
(*Show[{ListPlot[logAveragedWithMaxDevShifted,PlotStyle->{GrayLevel[0],Directive[Opacity[0.3]]},AxesLabel->{"Log[L-4]","Log[N]"}]*)
(*,(*ListPlot[logAveragedWithErrorsShifted,PlotStyle->]*)ListPlot[logAveragedWithErrorsShifted,PlotStyle->RGBColor[0, 0.78, 1],PlotLegends->{"logAveragedWithErrorsShifted"}]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 0.68, 0.5],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@lmAveragedShiftedDropped[x]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.85, 0, 0.5],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@lmAveragedWithErrorsShiftedDropped[x]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0, 0, 1],Dashed},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@(x (dfSLE/.bb->N[b])-1)}*)
(*,(*PlotRange->All,*)PlotLabel->Row[{" b = ",b}]*)
(*(*,Epilog->{Directive[Dashed,],*)
(*Line[{{Log[thresholdBelow],0},{Log[thresholdBelow],Log[maxy]//N}}]*)
(*,Line[{{Log[thresholdAbove],0},{Log[thresholdAbove],Log[maxy]//N}}]}*)*)
(*,PlotRange->{{1,All},{0,All}},AxesOrigin->{1,0},ImageSize->Large]*)


(* ::Subsubsection::Closed:: *)
(*Check with enforced Minimum*)


(* ::Input::Closed:: *)
(*fitFunc=a+df x;*)
(*lmAveragedShiftedGlobal=NonlinearModelFit[logAveragedShifted,{fitFunc(*,{-2<a<2,0<\[Omega]<28}*)},{a,(*c,\[Omega],*)df},x,Method->"NMinimize"];*)
(*lmAveragedWithErrorsShiftedGlobal=NonlinearModelFit[Select[logAveragedWithErrorsShifted,#[[1]]>0&],{fitFunc(*,{-2<a<2,0<\[Omega]<28}*)},{a,(*c,\[Omega],*)df},x,Method->"NMinimize"];*)
(*lmAveragedWithEstimatedStdDevsShiftedGlobal=NonlinearModelFit[Select[logAveragedWithEstimatedStdDevsShifted,#[[1]]>0&],{fitFunc(*,{-2<a<2,0<\[Omega]<28}*)},{a,(*c,\[Omega],*)df},x,Method->"NMinimize"];*)
(*lmAveragedWithMaxDevShiftedGlobal=NonlinearModelFit[Select[logAveragedWithMaxDevShifted,#[[1]]>0&],{fitFunc(*,{-2<a<2,0<\[Omega]<28}*)},{a,(*c,\[Omega],*)df},x,Method->"NMinimize"];*)
(**)
(**)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@lmAveragedShiftedGlobal*)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@lmAveragedWithErrorsShiftedGlobal *)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@lmAveragedWithEstimatedStdDevsShiftedGlobal *)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@lmAveragedWithMaxDevShiftedGlobal*)
(**)
(*Print[]*)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@lmAveragedShifted*)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@lmAveragedWithErrorsShifted *)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@lmAveragedWithEstimatedStdDevsShifted *)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@lmAveragedWithMaxDevShifted*)


(* ::Print:: *)
(*Row[{"lmAveragedWithEstimatedStdDevsShiftedGlobal"," with ",a+df x,": ",-0.412897+1.03982 x}]*)


(* ::Print:: *)
(*Row[{"lmAveragedWithEstimatedStdDevsShifted"," with ",a+df x,": ",-0.412908+1.03982 x}]*)


(* ::Input:: *)
(*synchronizedPlots=Map[Show[#,ImageSize->500]&,{ListPlot[lmAveragedShifted["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis, AdjustedRSquared=",lmAveragedShifted["AdjustedRSquared"]}]]*)
(*,ListPlot[lmAveragedWithErrorsShifted["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis, AdjustedRSquared=",lmAveragedWithErrorsShifted["AdjustedRSquared"]}]]*)
(*,ListPlot[lmAveragedWithEstimatedStdDevsShifted["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis, AdjustedRSquared=",lmAveragedWithEstimatedStdDevsShifted["AdjustedRSquared"]}]]*)
(*,ListPlot[lmAveragedWithMaxDevShifted["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis, AdjustedRSquared=",lmAveragedWithMaxDevShifted["AdjustedRSquared"]}]]}];*)
(**)
(*Multicolumn[synchronizedPlots,2,Appearance->"Framed"]*)
(**)


(* ::Subsubsection::Closed:: *)
(*\[Chi]^2 with analytical b (from SLE)*)


(* ::Input:: *)
(*dfSLE/.bb->b*)


(* ::Input::Closed:: *)
(*fitFunc=a+(dfSLE/.bb->b) x;*)
(*lmAveragedShiftedSLE=NonlinearModelFit[logAveragedShifted,{fitFunc(*,{-2<a<2,0<\[Omega]<28}*)},{a,(*c,\[Omega],*)df},x,Method->"NMinimize"];*)
(*lmAveragedWithErrorsShiftedSLE=NonlinearModelFit[Select[logAveragedWithErrorsShifted,#[[1]]>0&],{fitFunc(*,{-2<a<2,0<\[Omega]<28}*)},{a,(*c,\[Omega],*)df},x,Method->"NMinimize"];*)
(*lmAveragedWithEstimatedStdDevsShiftedSLE=NonlinearModelFit[Select[logAveragedWithEstimatedStdDevsShifted,#[[1]]>0&],{fitFunc(*,{-2<a<2,0<\[Omega]<28}*)},{a,(*c,\[Omega],*)df},x,Method->"NMinimize"];*)
(*lmAveragedWithMaxDevShiftedSLE=NonlinearModelFit[Select[logAveragedWithMaxDevShifted,#[[1]]>0&],{fitFunc(*,{-2<a<2,0<\[Omega]<28}*)},{a,(*c,\[Omega],*)df},x,Method->"NMinimize"];*)
(**)
(**)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@lmAveragedShiftedSLE*)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@lmAveragedWithErrorsShiftedSLE *)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@lmAveragedWithEstimatedStdDevsShiftedSLE *)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@lmAveragedWithMaxDevShiftedSLE*)
(**)


(* ::Print:: *)
(*Row[{"lmAveragedWithEstimatedStdDevsShiftedSLE"," with ",a+(13 x)/12,": ",-0.713206+(13 x)/12}]*)


(* ::Input:: *)
(*synchronizedPlots=Map[Show[#,ImageSize->500]&,{ListPlot[lmAveragedShiftedSLE["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis, AdjustedRSquared=",lmAveragedShifted["AdjustedRSquared"]}]]*)
(*,ListPlot[lmAveragedWithErrorsShiftedSLE["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis, AdjustedRSquared=",lmAveragedWithErrorsShifted["AdjustedRSquared"]}]]*)
(*,ListPlot[lmAveragedWithEstimatedStdDevsShiftedSLE["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis, AdjustedRSquared=",lmAveragedWithEstimatedStdDevsShifted["AdjustedRSquared"]}]]*)
(*,ListPlot[lmAveragedWithMaxDevShiftedSLE["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis, AdjustedRSquared=",lmAveragedWithMaxDevShifted["AdjustedRSquared"]}]]}];*)
(**)
(*Multicolumn[synchronizedPlots,2,Appearance->"Framed"]*)
(**)


(* ::Input:: *)
(*Show[{ListPlot[logAveragedWithMaxDevShifted,PlotStyle->{GrayLevel[0],Directive[Opacity[0.3]]},AxesLabel->{Row[{"Log[L-",(best[[1]]-51)/10//N,"]"}],"Log[N]"}]*)
(*,ListPlot[logAveragedWithErrorsShifted,PlotStyle->{RGBColor[0, 1, 1],PointSize->0.0003}]*)
(*(*,ListPlot[logAveragedWithEstimatedStdDevsShifted,PlotStyle->{,Directive[Opacity[0.3]]}]*)*)
(**)
(*(*,Plot[lmp[x],{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{,Thickness->0.004}]*)*)
(**)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 0, 0.5],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],{Right,Top}]]&@(lmAveragedShiftedSLE[x])*)
(**)
(*,Plot[x (dfSLE/.bb->b)-1,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0, 0, 1],Dashed}]}*)
(*,(*PlotRange->All,*)PlotLabel->Row[{" b = ",b}]*)
(*(*,Epilog->{Directive[Dashed,],*)
(*Line[{{Log[thresholdBelow],0},{Log[thresholdBelow],Log[maxy]//N}}]*)
(*,Line[{{Log[thresholdAbove],0},{Log[thresholdAbove],Log[maxy]//N}}]}*)*)
(*,PlotRange->{All,{0,All}},AxesOrigin->{1,0}]*)


(* ::Subsection::Closed:: *)
(*Extra analysis			*)


(* ::Subsubsection::Closed:: *)
(*Plot with different drops below*)


(* ::Input:: *)
(**)
(*thresholdAbove=maxx-250;*)
(*dfDropped=ParallelTable[With[{*)
(*lmdropped=LinearModelFit[Select[logAveragedWithErrors,Log[i]<=#[[1]]<Log[thresholdAbove]&],x,x,Weights->Automatic]},*)
(*{i,Quiet@lmdropped["ParameterTable"][[1]][[1,3,2]]}],{i,5,500}];*)


(* ::Input:: *)
(*dfDropped[[15;;30]]*)


(* ::Input:: *)
(*lmDrops=LinearModelFit[DeleteCases[dfDropped,{x_,_}/;(x<0)],{1},x];*)
(**)
(*Show[*)
(*{ListPlot[dfDropped,PlotRange->{All,All}]*)
(*(*,Plot[fit[x],{x,0,1000},PlotStyle->Red]*)*)
(*,Plot[dfSLE/.bb->b/1.,{x,0,1000},PlotStyle->RGBColor[0, 0, 1]]*)
(*,Plot[lmDrops[x],{x,0,1000},PlotStyle->RGBColor[0, Rational[2, 3], 0]]*)
(*}*)
(*,AxesLabel->{"Dropping threshold",Subscript[d, f]},PlotRange->{All,{0.9,1.7}}]*)


(* ::Input:: *)
(*dfTogether[[20;;30]]*)


(* ::Subsubsection::Closed:: *)
(*Plot with different drops Above BAD		TBD*)


(* ::Input:: *)
(*thresholdBelow=200;(*Fix this*)*)
(**)
(*dfDroppedAbove=Table[With[{*)
(*lmdropped=LinearModelFit[Log[DeleteCases[rawData,{a_,_}/;(a<thresholdBelow ||  a>(maxx-i))]],x,x]},*)
(*{i,Quiet@lmdropped["ParameterTable"][[1]][[1,3,2]]}],{i,0,1500}];*)


(* ::Input:: *)
(*fitFunc=a+d Exp[c x];*)
(*fit=FindFit[dfDroppedAbove,fitFunc,{a,c,d},x]*)
(**)
(*Show[*)
(*{ListPlot[dfDroppedAbove,PlotRange->{All,All}]*)
(*,Plot[fitFunc/.fit,{x,2,1500},PlotStyle->Red]*)
(*,Plot[dfSLE/.bb->b/1.,{x,0,1000},PlotStyle->Blue]*)
(*},PlotRange->{All,All}]*)


(* ::Input:: *)
(*ListPlot[dfDroppedAbove,PlotRange->{All,All}]*)


(* ::Subsubsection::Closed:: *)
(*Plot with different POSITIONS of the same WINDOW:   (i)<  a < (i + 500)		TBD*)


(* ::Input:: *)
(*window=500;(*Set this*)*)
(**)
(*dfDroppedWindow=ParallelTable[With[{*)
(*lmdropped=LinearModelFit[Select[logAveragedWithErrors,Log[i]<#[[1]]<Log[window+i]&],x,x]},*)
(*{i,Quiet@lmdropped["ParameterTable"][[1]][[1,3,2]]}],{i,0,maxx-window}];*)


(* ::Input:: *)
(*Show[*)
(*{ListPlot[dfDroppedWindow,PlotRange->{All,All}]*)
(*,Plot[dfSLE/.bb->b/1.,{x,0,1500},PlotStyle->RGBColor[0, 0, 1],PlotLegends->SwatchLegend[{"SLE"}]]*)
(*},PlotRange->{All,All},PlotLabel->Row[{"Moving window of size ",window}],AxesLabel->{"Window position",Subscript[d, f]},ImageSize->500]*)


(* ::Subsubsection::Closed:: *)
(*Plot with different POSITIONS of the same WINDOW:  Changing window 		TBD*)


(* ::Input:: *)
(**)


(* ::Input:: *)
(*windowPlots=Table[*)
(*dfDroppedWindow={window,ParallelTable[With[{*)
(*lmdropped=LinearModelFit[Select[logAveragedWithErrors,Log[i]<#[[1]]<Log[window+i]&],x,x]},*)
(*{i,Around[Quiet@lmdropped["ParameterTable"][[1]][[1,3,2]],\[Pi]*lmdropped["ParameterErrors"][[2]]]}],{i,0,maxx-window,20}]}*)
(*,{window,100,1000,50}];*)


(* ::Input:: *)
(*windowPlots[[1]]*)


(* ::Input:: *)
(*showWindowPlots=Show[*)
(*{ListPlot[#[[2]],PlotRange->{All,All}]*)
(*,Plot[dfSLE/.bb->b/1.,{x,0,2000-window},PlotStyle->RGBColor[0, 0, 1]]*)
(*},PlotRange->{All,{0.9,1.1}},PlotLabel->Row[{"Window size = ",#[[1]]}]]&/@windowPlots*)


(* ::Input:: *)
(*synchronizedPlots=Map[Show[#,PlotRange->{All,{0.99,1.05}},ImageSize->220]&,showWindowPlots];*)
(**)
(*Multicolumn[synchronizedPlots,4,Appearance->"Framed"]*)


(* ::Input:: *)
(*Map[Show[#,PlotRange->{All,{1,1.005}},ImageSize->280]&,showWindowPlots[[6;;8]]];*)
(*Multicolumn[%,3,Appearance->"Framed"]*)


(* ::Subsection::Closed:: *)
(*Just two: df=(Log(n)-Log(n'))/(Log(L)-Log(L'))*)


(* ::Input:: *)
(*maxx=Max[rawData[[All,1]]];*)
(*maxy=Max[rawData[[All,2]]];*)
(**)
(*thresholdBelow=25;*)
(*thresholdAbove=maxx-0*500;*)
(**)
(*droppedWithMaxDev=Select[logAveragedWithMaxDev,Log[thresholdBelow]<#[[1]]<Log[thresholdAbove]&];*)
(*droppedWithErrors=Select[logAveragedWithErrors,Log[thresholdBelow]<#[[1]]<Log[thresholdAbove]&];*)
(**)
(**)
(*lmdroppedWithMaxDev=LinearModelFit[droppedWithMaxDev,x,x,Weights->Automatic];*)
(*lmdroppedWithErrors=NonlinearModelFit[droppedWithErrors,a+df x,{a,df},x,Weights->Automatic(*,Method->"NMinimize"*)]*)
(*(*lmpdropped=LinearModelFit[dropped,fitFuncs,x];*)*)
(**)
(*Show[{ListPlot[logAveragedWithMaxDev,PlotStyle->{GrayLevel[0],Directive[Opacity[0.3]]},AxesLabel->{"Log[L]","Log[N]"}]*)
(*,ListPlot[logAveragedWithErrors,PlotStyle->RGBColor[0, 0.78, 1],PlotLegends->{"logAveragedWithErrors"}]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 0, 0.5],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@lmdroppedWithMaxDev[x]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[1, 0.68, 0.6],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@lmdroppedWithErrors[x]*)
(*(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{,Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@lmAveragedWithStdDevs[x]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{,Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@lmAveragedWithStdDevsGlobal[x]*)*)
(**)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0, 0, 1],Dashed},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@(x (dfSLE/.bb->N[b])-1)*)
(*,Plot[#,{x,0,Log[thresholdAbove]},PlotStyle->{RGBColor[0, 1, 1],Dashed},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@(x (dfSLE/.bb->N[b])-0.7-5Exp[-0.991 x])*)
(*}*)
(*,Epilog->{Directive[Dashed,GrayLevel[0.5]],*)
(*Line[{{Log[thresholdBelow],0},{Log[thresholdBelow],Log[maxy]//N}}]*)
(*,Line[{{Log[thresholdAbove],0},{Log[thresholdAbove],Log[maxy]//N}}]}*)
(*,PlotLabel->Row[{" b = ",b}]*)
(*,PlotRange->{All,{0,All}}(*PlotRange->{{Log[thresholdBelow],Log[maxy]},{4,7.2}}*),AxesOrigin->{(*Log[thresholdBelow]*)1,0},ImageSize->700]*)


(* ::Input:: *)
(*Sorted=Sort[droppedWithErrors];*)


(* ::Input:: *)
(*Length[Sorted]*)


(* ::Input:: *)
(*Sorted[[{12,13,7}]]//N*)
(*With[{L1=%[[1,1]],L2=%[[2,1]],n1=%[[1,2]],n2=%[[2,2]]},(n1-n2)/(L1-L2)]*)


(* ::Subsubsection::Closed:: *)
(*Using nearest neighbors points (bad)*)


(* ::Input:: *)
(*dfList=ParallelTable[With[{L1=Sorted[[i,1]],L2=Sorted[[i+1,1]],n1=Sorted[[i,2]],n2=Sorted[[i+1,2]]},(n1-n2)/(L1-L2)],{i,1,Length[Sorted]-1}]*)


(* ::Input:: *)
(*Drop[dfList,-35];*)
(*fitOfdfList=NonlinearModelFit[%,df,{df},x]*)
(*Show[{ListPlot[%%],Plot[fitOfdfList[x],{x,0,50}]},PlotRange->All]*)


(* ::Subsubsection::Closed:: *)
(*Using points further apart (badish)*)


(* ::Input:: *)
(*distance=5;*)
(*dfList=ParallelTable[With[{L1=Sorted[[i,1]],L2=Sorted[[i+distance,1]],n1=Sorted[[i,2]],n2=Sorted[[i+distance,2]]},(n1-n2)/(L1-L2)],{i,1,Length[Sorted]-distance}]*)


(* ::Input:: *)
(*Drop[Drop[dfList,5],-10];*)
(*fitOfdfList=NonlinearModelFit[%,df+a x+c x^2,{df,a,c},x]*)
(*Show[{ListPlot[%%],Plot[fitOfdfList[x],{x,0,50}]},PlotRange->All]*)


(* ::Title::Closed:: *)
(*Asymmetric distribution: Bias-corrected and accelarated (gemini suggestion) *)
(*I THINK THAT THIS IS EXACTLY KAY'S TRICK, with the subtle difference that here RESAMPLING is allowed.*)


(* ::Subsection::Closed:: *)
(*Example*)


(* ::Input:: *)
(*(*Generate sample data from a heavily skewed distribution*)*)
(*SeedRandom[1234];*)
(*data=RandomVariate[LogNormalDistribution[0,1.5],50];*)
(*Histogram[%,PlotRange->All]*)
(**)
(*(*1. Compute the sample mean*)*)
(*sampleMean=Mean[data]*)
(**)
(*(*2. Bootstrap resamples for the mean*)*)
(*B=20000;*)
(*bootMeans=Table[Mean[RandomChoice[data,Length[data]]],B];*)
(*Histogram[%,PlotRange->All]*)
(**)
(*(*3. 68.27% (1-sigma equivalent) confidence bounds via Percentile Bootstrap*)*)
(*{lowerCI,upperCI}=Quantile[bootMeans,{0.15865,0.84135}];*)
(**)
(*(*4. Derive asymmetric deviations*)*)
(*deltaMinus=sampleMean-lowerCI;*)
(*deltaPlus=upperCI-sampleMean;*)
(**)
(*(*5. Assign with Around*)*)
(*meanWithUncertainty=Around[sampleMean,{deltaMinus,deltaPlus}]*)
(**)
(*StandardDeviation[data]/Sqrt[Length[data]]*)


(* ::Input:: *)
(*(*Generate sample data from a heavily skewed distribution*)*)
(*SeedRandom[1234];*)
(*data=RandomVariate[NormalDistribution[0,1.5],10000];*)
(*Histogram[%,PlotRange->All]*)
(**)
(*(*1. Compute the sample mean*)*)
(*sampleMean=Mean[data]*)
(**)
(*(*2. Bootstrap resamples for the mean*)*)
(*B=20000;*)
(*bootMeans=Table[Mean[RandomChoice[data,Length[data]]],B];*)
(*Histogram[%,PlotRange->All]*)
(**)
(*(*3. 68.27% (1-sigma equivalent) confidence bounds via Percentile Bootstrap*)*)
(*{lowerCI,upperCI}=Quantile[bootMeans,{0.15865,0.84135}];*)
(**)
(*(*4. Derive asymmetric deviations*)*)
(*deltaMinus=sampleMean-lowerCI*)
(*deltaPlus=upperCI-sampleMean*)
(**)
(*(*5. Assign with Around*)*)
(*meanWithUncertainty=Around[sampleMean,{deltaMinus,deltaPlus}]*)


(* ::Input:: *)
(*StandardDeviation[data]/Sqrt[Length[data]]*)
