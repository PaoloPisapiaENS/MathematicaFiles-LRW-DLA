(* ::Package:: *)

(* ::Title:: *)
(*Data analysis for the Fractal Dimension of the b-LRW on 2d square Lattice. After validation. *)
(*Optimization with Gemini for big data sets*)


(* ::Input:: *)
(*Quit[]*)


(* ::Input::Initialization:: *)
SetOptions[EvaluationNotebook[],NotebookEventActions->{"Open":>(Print["Notebook opened at ",DateString[]];
<<"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\MathematicaFiles-LRW-DLA-bLRW\\bLRW\\bLRW-MicroscopicAction-RG-FractalDim-etc\\bLRW-FractalDimFromData\\2d-FractalDim\\2d-bLRW-FractalDimension.m")}]
<<"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\MathematicaFiles-LRW-DLA-bLRW\\bLRW\\bLRW-MicroscopicAction-RG-FractalDim-etc\\bLRW-FractalDimFromData\\2d-FractalDim\\2d-bLRW-FractalDimension.m"
Directory[]


(* ::Section::Closed:: *)
(*b4-clean_merged_data-AfterValidation.csv (+extra analysis)*)
(*SEE BELOW FOR ESTIMATE USING KAY'S TRICK*)


(* ::Input::Initialization:: *)
b=4;

rawData=Import["data4Sqaure-AfterValidation.mx"];

Length[rawData]


(* ::Item:: *)
(*Import set-up, run once every time b4-clean_merged_data-AfterValidation.csv gets updated *)


(* ::Input:: *)
(*rawData=Import["D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d-FromCluster\\b4-clean_merged_data-AfterValidation.csv","CSV"];*)
(*(*Immediately lock it into a Packed Array*)*)
(*rawData=data4Square=Developer`ToPackedArray[rawData];*)
(* (* MODIFY FILE NAME *)*)
(*Length[rawData]*)


(* ::Input:: *)
(*Export["data4Sqaure-AfterValidation.mx",rawData,"MX"]*)


(* ::Subsection::Closed:: *)
(*RawData analysis: too slow with all these data points*)


(* ::Input:: *)
(*(*Instantly grab 200,000 completely random rows from your matrix*)*)
(*randomLinearSample=RandomSample[rawData,200000];*)
(*randomLogSample=Log[randomLinearSample];*)
(**)
(*ListPlot[randomLinearSample,PlotStyle->{PointSize[0.004],Directive[Opacity[0.3]]},FrameLabel->{"L","N"},PlotLabel->"Linear Scale Distribution (200k random points)",PlotRange->All];*)
(**)
(*ListPlot[randomLinearSample,PlotStyle->{PointSize[0.004],Directive[Opacity[0.3]]},FrameLabel->{"Log[L]","Log[N]"},PlotLabel->"Log-Log Scale Distribution (200k random points)",PlotRange->All];*)


(* ::Input:: *)
(*fitFuncs=Table[x^(-i),{i,-1,3}];*)
(*(*fitFuncs=Append[Table[x^(-i),{i,-1,0}],Exp[-x]];*)*)
(*(**)
(*fitFuncs={x,1,Exp[-x],Exp[-2 x]};(*Exp[-1.3 x]*)*)*)
(*fitFuncs={x,1,Exp[-2x]};*)
(**)
(*logData=Log[rawData];*)


(* ::Input:: *)
(*lmp=LinearModelFit[logData,fitFuncs,x];*)


(* ::Input:: *)
(*lmp//Normal*)


(* ::Input:: *)
(*lm=LinearModelFit[logData,x,x];*)
(*%//Normal*)


(* ::Input:: *)
(*fitFunc=a+c Exp[-\[Omega] x]+df x;*)


(* ::Input:: *)
(*fitSol=FindFit[logData,{fitFunc,{-2<a<1.1,\[Omega]<4}},{a,c,\[Omega],df},x]*)


(* ::Input:: *)
(*Show[{ListPlot[randomLogSample,PlotStyle->{PointSize[0.003],Directive[Opacity[0.3]]},AxesLabel->{"Log[L]","Log[N]"},DataRange->{0,50}],*)
(*Plot[lm[x],{x,0,1000},PlotStyle->{RGBColor[1, 0, 0],Thickness->0.004}]*)
(**)
(*,Plot[lmp[x],{x,0,1000},PlotStyle->{RGBColor[0, Rational[2, 3], 0],Thickness->0.004}]*)
(*,Plot[fitFunc/.fitSol,{x,0,1000},PlotStyle->{RGBColor[1, Rational[2, 3], 1],Thickness->0.004}]*)
(**)
(*,Plot[x (dfSLE/.bb->b)-1,{x,1,10000},PlotStyle->{RGBColor[0, 0, 1],Dashed}]}*)
(*,PlotLabel->Row[{" b = ",b}](*,PlotRange->{{1,Log[200]},{0,Log[100]}}*)*)
(*]*)
(**)
(*Print[ "Linear fit \!\(\*TemplateBox[<|\"color\" -> RGBColor[1, 0, 0]|>,\n\"RGBColorSwatchTemplate\"]\): \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lm["ParameterTable"][[1]][[1,3,2]],\[Pi]*lm["ParameterErrors"][[2]]],"*)
(*",Total@fitFuncs," fit \!\(\*TemplateBox[<|\"color\" -> RGBColor[0, Rational[2, 3], 0]|>,\n\"RGBColorSwatchTemplate\"]\): \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lmp["ParameterTable"][[1]][[1,2,2]],\[Pi]*lmp["ParameterErrors"][[1]]],"*)
(*",fitFunc," fit \!\(\*TemplateBox[<|\"color\" -> RGBColor[1, Rational[2, 3], 1]|>,\n\"RGBColorSwatchTemplate\"]\): \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[df/.fitSol,Null],", Parameters:",fitSol,"*)
(*Result from the litterature (exact with SLE) \!\(\*TemplateBox[<|\"color\" -> RGBColor[0, 0, 1]|>,\n\"RGBColorSwatchTemplate\"]\): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Input:: *)
(*Quiet@lm["ParameterTable"][[1]]*)
(*Quiet@lmp["ParameterTable"][[1]]*)


(* ::Subsection:: *)
(*RawData analysis: Random Sampling*)


(* ::Subsubsection::Closed:: *)
(*Test*)


(* ::Input:: *)
(*(*Instantly grab 100,000 random rows from your matrix*)*)
(*randomLinearSample=RandomSample[rawData,100000];*)
(*randomLogSample=Log[randomLinearSample];*)
(**)
(*(*ListPlot[randomLinearSample,PlotStyle->{PointSize[0.004],Directive[Opacity[0.3]]},FrameLabel->{"L","N"},PlotLabel->"Linear Scale Distribution (100k random points)",PlotRange->All];*)
(**)
(*ListPlot[randomLogSample,PlotStyle->{PointSize[0.004],Directive[Opacity[0.3]]},FrameLabel->{"Log[L]","Log[N]"},PlotLabel->"Log-Log Scale Distribution (100k random points)",PlotRange->All];*)*)


(* ::Input:: *)
(*fitFuncs=Table[x^(-i),{i,-1,3}];*)
(*(*fitFuncs=Append[Table[x^(-i),{i,-1,0}],Exp[-x]];*)*)
(*(**)
(*fitFuncs={x,1,Exp[-x],Exp[-2 x]};(*Exp[-1.3 x]*)*)*)
(*fitFuncs={x,1,Exp[-2x]};*)
(**)
(*logData=Log[rawData];*)


(* ::Input:: *)
(*lmp=LinearModelFit[randomLogSample,fitFuncs,x];*)
(*%//Normal*)


(* ::Input:: *)
(*lm=LinearModelFit[randomLogSample,x,x];*)
(*%//Normal*)


(* ::Input:: *)
(*fitFunc=a+c Exp[-\[Omega] x]+df x;*)
(*(*fitSol=FindFit[randomLogSample,{fitFunc,{-2<a<1.1,\[Omega]<4}},{a,c,\[Omega],df},x]*)*)
(*nlm=NonlinearModelFit[randomLogSample,{fitFunc,{-2<a<1.1,\[Omega]<1.65}},{a,c,\[Omega],df},x];*)
(*%//Normal*)


(* ::Input:: *)
(*randomSamplePlot=ListPlot[randomLogSample,PlotStyle->{PointSize[0.003],Directive[Opacity[0.3]]},AxesLabel->{"Log[L]","Log[N]"},DataRange->{0,50}];*)


(* ::Input:: *)
(*maxx=Max[rawData[[All,1]]];*)
(*maxy=Max[rawData[[All,2]]];*)
(**)
(*thresholdBelow=0;*)
(*thresholdAbove=maxx-0;*)
(**)
(*Show[{randomSamplePlot,*)
(*Plot[lm[x],{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[1, 0, 0],Thickness->0.004}]*)
(**)
(*,Plot[lmp[x],{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0, Rational[2, 3], 0],Thickness->0.004}]*)
(*,Plot[nlm[x],{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[1, Rational[2, 3], 1],Thickness->0.004}]*)
(*,Plot[x (dfSLE/.bb->b)-1,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0, 0, 1],Dashed}]}*)
(*,PlotRange->{{1,All},{0,8}},PlotLabel->Row[{" b = ",b}]*)
(*]*)
(**)
(*Print["Linear fit with random sample \!\(\*TemplateBox[<|\"color\" -> RGBColor[1, 0, 0]|>,\n\"RGBColorSwatchTemplate\"]\): \!\(\*SubscriptBox[\(d\), \(f\)]\) =",Around[Quiet@lm["ParameterTable"][[1]][[1,3,2]],\[Pi]*lm["ParameterErrors"][[2]]],"*)
(**)
(*Random sample fit with ",Total@fitFuncs," \!\(\*TemplateBox[<|\"color\" -> RGBColor[0, Rational[2, 3], 0]|>,\n\"RGBColorSwatchTemplate\"]\): \!\(\*SubscriptBox[\(d\), \(f\)]\) =",Around[Quiet@lmp["ParameterTable"][[1]][[1,2,2]],\[Pi]*lmp["ParameterErrors"][[1]]],"*)
(**)
(*Random sample fit with ",fitFunc," \!\(\*TemplateBox[<|\"color\" -> RGBColor[1, Rational[2, 3], 1]|>,\n\"RGBColorSwatchTemplate\"]\): \!\(\*SubscriptBox[\(d\), \(f\)]\) =",Around[Quiet@nlm["ParameterTable"][[1,1,5,2]],\[Pi]*Quiet@nlm["ParameterTable"][[1,1,5,3]]],"*)
(*	Parameters:",Map[Row[{#[[1]],"=",Around[#[[2]],#[[3]]]}]&,Quiet@nlm["ParameterTable"][[1,1,2;;-1,1;;3]]],"*)
(**)
(*Result from the litterature (exact with SLE) : ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Input:: *)
(*Quiet@lm["ParameterTable"][[1]]*)
(*Quiet@lmp["ParameterTable"][[1]]*)
(*Quiet@nlm["ParameterTable"][[1]]*)
(**)
(*Quiet@nlm["BestFitParameters"][[-1,2]]*)
(*Quiet@nlm["ParameterErrors"][[-1]]*)


(* ::Subsubsection:: *)
(*Repeated Random Sampling*)


(* ::Input::Initialization:: *)
Global`logData=Log[rawData];

fitFunc=a+c Exp[-\[Omega] x]+df x;


(* ::Input::Initialization:: *)
Quiet@LaunchKernels[];
DistributeDefinitions[rawData];


(* ::Item::Closed:: *)
(*My preliminary implementation*)


(* ::Input:: *)
(*Check[nlm=NonlinearModelFit[randomLogSample,{fitFunc,{-2<a<1.1,\[Omega]<2.4}},{a,c,\[Omega],df},x,MaxIterations->100],Nothing]*)


(* ::Input:: *)
(*{nlm=Check[NonlinearModelFit[randomLogSample,{fitFunc,{-2<a<1.1,\[Omega]<2.4}},{a,c,\[Omega],df},x,MaxIterations->100],*)
(*(*If ANY warning/error occurs (like max iterations hit),return Nothing*)*)
(*$Failed];*)
(**)
(*If[FailureQ[nlm],Nothing,Around[nlm["BestFitParameters"][[1,2]],nlm["ParameterErrors"][[1]]]]}*)


(* ::Input:: *)
(*someDf2=ParallelTable[Block[{randomSample,randomLogSample,fitFunc,nlm,value,error},*)
(**)
(*(*randomSample=RandomSample[rawData,100000];*)
(*randomLogSample=Log[randomSample];*)*)
(*randomLogSample=RandomSample[Log[rawData],100000];*)
(**)
(*fitFunc=a+c Exp[-\[Omega] x]+df x;*)
(**)
(*(*nlm=NonlinearModelFit[randomLogSample,{fitFunc,{-2<a<1.1,\[Omega]<2.4}},{a,c,\[Omega],df},x,ConfidenceLevel->0.9,MaxIterations->500];*)*)
(*nlm=Check[NonlinearModelFit[randomLogSample,{fitFunc,{-2<a<1.1,\[Omega]<2.4}},{a,c,\[Omega],df},x,MaxIterations->500],*)
(*(*If ANY warning/error occurs (like max iterations hit),return $Failed*)*)
(*$Failed];*)
(**)
(*If[FailureQ[nlm],Nothing,*)
(*(*Rule[#[[1]],Around[#[[2]],#[[3]]]]&@(Quiet@nlm["ParameterTable"][[1,1,-1,1;;3]]);*)*)
(*(*fractalDim=Around@@@(Quiet@nlm["ParameterTable"][[1,1,-1,2;;3]]);*)*)
(*value=Quiet@nlm["BestFitParameters"][[-1,2]];*)
(*error=Quiet@nlm["ParameterErrors"][[-1]];*)
(**)
(*Around[value,error]]*)
(*]*)
(**)
(*,50]*)


(* ::Item::Closed:: *)
(*Old Gemini's code*)


(* ::Input:: *)
(*(*Mixing parallel with batching (implemented with Gemini)*)*)


(* ::Input:: *)
(*totalIterations=100;*)
(*batchSize=$ProcessorCount; (*Set this to the number of your parallel kernels*)*)
(*numBatches=Ceiling[totalIterations/batchSize];*)
(**)
(*(*Pre-allocate or prepare a file to save data*)*)
(*partialResults={};*)
(**)
(*Do[Print["Processing Batch ",b," of ",numBatches,"..."];*)
(**)
(*(*1. Fire up a small,controlled parallel batch*)*)
(*batchResults=ParallelTable[Block[{randomSample,randomLogSample,fitFunc,nlm,value,error},*)
(**)
(*(*randomSample=RandomSample[rawData,100000];*)
(*randomLogSample=Log[randomSample];*)*)
(*randomLogSample=RandomSample[Log[rawData],100000];*)
(**)
(*fitFunc=a+c Exp[-\[Omega] x]+df x;*)
(**)
(*(*nlm=NonlinearModelFit[randomLogSample,{fitFunc,{-2<a<1.1,\[Omega]<2.4}},{a,c,\[Omega],df},x,ConfidenceLevel->0.9,MaxIterations->500];*)*)
(*nlm=Check[NonlinearModelFit[randomLogSample,{fitFunc,{-2<a<1.1,\[Omega]<2.4}},{a,c,\[Omega],df},x,MaxIterations->500],*)
(*(*If ANY warning/error occurs (like max iterations hit),return $Failed*)*)
(*$Failed];*)
(**)
(*If[FailureQ[nlm],Nothing,*)
(*(*Rule[#[[1]],Around[#[[2]],#[[3]]]]&@(Quiet@nlm["ParameterTable"][[1,1,-1,1;;3]]);*)*)
(*(*fractalDim=Around@@@(Quiet@nlm["ParameterTable"][[1,1,-1,2;;3]]);*)*)
(*value=Quiet@nlm["BestFitParameters"][[-1,2]];*)
(*error=Quiet@nlm["ParameterErrors"][[-1]];*)
(**)
(*Around[value,error]]*)
(*]*)
(**)
(*,batchSize];*)
(**)
(*(*2. Append results to master list*)*)
(*partialResults=Join[partialResults,batchResults];*)
(*(*3. Save to disk immediately so you never lose progress*)*)
(*DumpSave["partial_fit_results.mx",partialResults];*)
(*(*4. Force memory cleanup for the master kernel*)*)
(*Clear[batchResults];*)
(*ClearSystemCache[];*)
(*,{b,1,numBatches}];*)


(* ::Input:: *)
(*Clear[partialResults]*)
(*Get[$InitialDirectory<>"\partial_fit_results.mx"]*)
(*partialResults*)


(* ::Subsubsection::Closed:: *)
(*New Gemini's code: optimized parallelization*)


(* ::Input:: *)
(*Block[{sample,nlm,value,error,sampleSize},*)
(**)
(*sampleSize=200000;*)
(*sample=Log[RandomSample[rawData,sampleSize]];*)
(**)
(*nlm=Check[NonlinearModelFit[sample,{fitFunc,{-2<a<1.1,\[Omega]<2.3}},{a,c,\[Omega],df},x,MaxIterations->500],*)
(*(*If ANY warning/error occurs (like max iterations hit),return $Failed*)*)
(*$Failed];*)
(**)
(**)
(*If[FailureQ[nlm],Nothing,*)
(*value=Quiet@nlm["BestFitParameters"][[-1,2]];*)
(*error=Quiet@nlm["ParameterErrors"][[-1]];*)
(**)
(*Check[Around[value,error],Nothing]]*)
(*]*)


(* ::Input:: *)
(*taskToSubmit[]*)
(*WaitNext[{%}]*)


(* ::Input:: *)
(*fitFunc=a+c Exp[-\[Omega] x]+df x;*)
(*(*DistributeDefinitions[fitFunc];*)*)
(**)
(*totalTasks=1000;*)
(*resultsFile="live_fit_results.mx";*)
(*batchSize=$ProcessorCount;*)
(*numBatches=Ceiling[totalIterations/batchSize];*)
(**)
(*(*Initialize an empty list on disk if it doesn't exist*)*)
(*If[!FileExistsQ[resultsFile],*)
(*savedResults={};*)
(*DumpSave[resultsFile,savedResults]];*)
(**)
(*taskToSubmit[]:=ParallelSubmit[*)
(*Block[{sampleSize,sample,fitFunc,nlm,value,error},*)
(**)
(*sampleSize=200000;*)
(*sample=Log[RandomSample[rawData,sampleSize]];*)
(**)
(*fitFunc=a+c Exp[-\[Omega] x]+df x;*)
(*nlm=Check[NonlinearModelFit[sample,{fitFunc,{-2<a<1.1,\[Omega]<2.4}},{a,c,\[Omega],df},x,MaxIterations->500],*)
(*(*If ANY warning/error occurs (like max iterations hit),return $Failed*)*)
(*$Failed];*)
(**)
(**)
(*If[FailureQ[nlm],Nothing,*)
(*value=Quiet@nlm["BestFitParameters"][[-1,2]];*)
(*error=Quiet@nlm["ParameterErrors"][[-1]];*)
(**)
(*Check[Around[value,error],Nothing]]*)
(*]*)
(*];*)
(**)
(*(*1. Initialize status monitoring variable*)*)
(*statusString="Initializing tasks...";*)
(*Print[Dynamic[statusString]]; (*Displays a single,auto-refreshing line*)*)
(*(*Print the live-updating progress panel*)(*Print[Panel[Column[{Row[{Style["Task Progress: ",Bold],Dynamic[completedCount]," / ",totalTasks}],Dynamic[ProgressIndicator[completedCount,{0,totalTasks},ImageSize->Large]],Row[{Style["Active Worker Queue: ",Gray],Dynamic[Length[activeTasks]]}]},Spacings->1],Background->GrayLevel[0.95],FrameMargins->15]]*)*)
(*activeTasks={};*)
(*completedCount=0;*)
(*startTime=AbsoluteTime[]; (*Capture exact start time*)*)
(**)
(*(*Print the live-updating progress panel with a timer*)*)
(*FormatTime[sec_]:=With[{s=Round[sec]},StringRiffle[IntegerString[#,10,2]&/@{Quotient[s,3600],Mod[Quotient[s,60],60],Mod[s,60]},":"]];*)
(*Print[Panel[Column[{Row[{Style["Task Progress: ",Bold],Dynamic[completedCount]," / ",totalTasks}],Dynamic[ProgressIndicator[completedCount,{0,totalTasks},ImageSize->Large]],*)
(*(*Dynamic Timer Row*)Dynamic[With[{elapsed=Round[AbsoluteTime[]-startTime]//N},Row[{Style["Elapsed: ",Gray],FormatTime[elapsed],"   |   ",Style["ETA: ",Gray],If[completedCount>0,With[{remaining=Round[(elapsed/completedCount)*(totalTasks-completedCount)]//N},FormatTime[remaining]],"Calculating..."]}]]],Row[{Style["Active Worker Queue: ",Gray],Dynamic[Length[activeTasks]]}]},Spacings->1],Background->GrayLevel[0.95],FrameMargins->15]]*)
(**)
(**)
(*(* Run the extraction *)*)
(*Block[{},*)
(**)
(*(*1. Prime the queue: launch 1 task per CPU core*)*)
(*activeTasks=Table[*)
(*taskToSubmit[]*)
(*,batchSize];*)
(**)
(**)
(*(*2. Dynamic loop:Process results as they finish live*)*)
(*While[Length[activeTasks]>0,*)
(**)
(*(*2. Update the dynamic variable instead of calling Print*)statusString=StringForm["Completed `` out of `` tasks. Active queue: ``",completedCount,totalTasks,Length[activeTasks]];*)
(**)
(*(*WaitNext pauses until ANY single core finishes,returning its data*)*)
(*{finishedValue,finishedTask,activeTasks}=WaitNext[activeTasks];*)
(*completedCount++;*)
(*(*If it yielded a valid fit (not Nothing),append and save instantly*)*)
(*If[finishedValue=!=Nothing,*)
(*Get[resultsFile];*)
(*(*Load current list*)*)
(*AppendTo[savedResults,finishedValue];*)
(*DumpSave[resultsFile,savedResults];*)
(*(*Flush directly to disk*)*)
(*Clear[savedResults];];*)
(**)
(*(*3. Feed the queue:If tasks remain,deploy a replacement core immediately*)*)
(**)
(*If[completedCount+Length[activeTasks]<totalTasks,*)
(*AppendTo[activeTasks,*)
(*taskToSubmit[]];*)
(*];*)
(*ClearSystemCache[];*)
(*];*)
(*(*3. Mark execution complete*)*)
(*statusString=Style["All tasks finished!",RGBColor[0, Rational[2, 3], 0]];*)
(*];*)
(**)
(**)
(*Get[resultsFile];*)
(*Mean[savedResults]*)


(* ::Input:: *)
(*resultsFile="live_fit_results.mx";*)
(*FileNameJoin[{Directory[],resultsFile}]*)
(*Get[resultsFile];*)
(*savedResults;*)
(*Mean[%]*)
(*%/.Around->List*)
(*{%[[1]],7*%[[2]]}*)


(* ::Item::Closed:: *)
(*This  was  just  a  test to check if table works*)


(* ::Input:: *)
(*Table[Block[{randomLogSample,fitFunc,nlm,value,error},*)
(**)
(*randomLogSample=RandomSample[logData,100000];*)
(**)
(*fitFunc=a+c Exp[-\[Omega] x]+df x;*)
(**)
(*nlm=NonlinearModelFit[randomLogSample,{fitFunc,{-2<a<1.1,\[Omega]<2.4}},{a,c,\[Omega],df},x];*)
(**)
(*(*Rule[#[[1]],Around[#[[2]],#[[3]]]]&@(Quiet@nlm["ParameterTable"][[1,1,-1,1;;3]]);*)*)
(*(*fractalDim=Around@@@(Quiet@nlm["ParameterTable"][[1,1,-1,2;;3]]);*)*)
(*value=Quiet@nlm["BestFitParameters"][[-1,2]];*)
(*error=Quiet@nlm["ParameterErrors"][[-1]];*)
(**)
(*Around[value,error]*)
(*]*)
(**)
(*,5]*)


(* ::Item:: *)
(*Mean*)


(* ::Input:: *)
(*Join[someDf,partialResults,savedResults,someDf3,{Around[1.0855621664024575`, 0.0010723458386046835`],Around[1.085712492895057, 0.001065854165723666],Around[1.0860223744153656`, 0.0010844510928279282`],Around[1.0862199396628205`, 0.0010715255279747607`],Around[1.0875409151452466`, 0.0010754736049851214`]}];*)
(*Select[%,(Head[#]==Around)&]*)
(*Mean[%]*)


(* ::Input:: *)
(*Mean[someDf]*)


(* ::Subsection:: *)
(*Test with mean over same L BEFORE LOG: Log[<y>]	VERY GOOD AGREEMENT?*)


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
(*stdDevs=StandardDeviation/@yGroups/. Indeterminate->1.0`*^-8;*)
(*stdDevs=stdDevs/. 0.->1.0`*^-8;*)
(**)
(*(* Maximum deviation*)*)
(*maxDevs=MapThread[Max[Abs[#1-#2]]&,{means//N,yGroups}];*)
(**)
(**)
(*(*5. Combine them using the Threaded Around wrapper*)*)
(**)
(*averaged=Transpose[{xValues,means}];*)
(*averagedWithErrors=Transpose[{xValues,MapThread[Around,{means,stdDevs}]}];*)
(*averagedWithMaxDev=Transpose[{xValues,MapThread[Around,{means,maxDevs}]}];*)


(* ::Input:: *)
(*Averagedb4AfterValidation=averaged;*)


(* ::Item::Closed:: *)
(*Gemini says this is slow*)


(* ::Input:: *)
(*maxx=Max[rawData[[All,1]]];*)
(*maxy=Max[rawData[[All,2]]];*)
(**)
(*thresholdBelow=44;*)
(*thresholdAbove=maxx-0;*)
(**)
(*grouped=GroupBy[rawData,First->Last];*)
(**)
(*(*Map over the groups to create {x,Around[mean,stdDev]} pairs*)*)
(*Averagedb4AfterValidation=KeyValueMap[*)
(*Function[{x,yValues},*)
(*{x,If[Length[yValues]>1,*)
(*Around[Mean[yValues],StandardDeviation[yValues]],*)
(*Around[Mean[yValues],1] (*Error is 0 if there is only 1 data point*)]}],grouped];*)
(**)
(*Averaged=KeyValueMap[{#1,Mean[#2]}&,GroupBy[rawData,First->Last]];*)


(* ::Item::Closed:: *)
(*Continues here*)


(* ::Input:: *)
(*logAveraged=Log[averaged];*)
(*logAveragedWithErrors=Log[averagedWithErrors]/. 0->Around[1.0`*^-6,1.0`*^-6];*)
(*logAveragedWithMaxDev=Log[averagedWithMaxDev]/. 0->Around[1.0`*^-6,1.0`*^-6];*)


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


(* ::Input:: *)
(*fitFuncs=Table[x^(-i),{i,-1,3}];*)
(*(*fitFuncs=Append[Table[x^(-i),{i,-1,0}],Exp[-x]];*)*)
(*(*fitFuncs={x,1,Exp[-x],Exp[-2 x]};(*Exp[-1.3 x]*)*)*)
(*fitFuncs={x,1,Exp[-2 x],Exp[-x]};*)
(*fitFunc=a+c Exp[-\[Omega] x]+df x;*)
(**)
(*lmAveraged=LinearModelFit[logAveragedWithErrors,x,x(*,Weights->Automatic*)];*)
(*Print[Row[{"lmAveraged: ",#//Normal}]]&@%*)
(*lmMaxDev=LinearModelFit[logAveragedWithMaxDev,x,x(*,Weights->Automatic*)];*)
(*Print[Row[{"lmMaxDev: ",#//Normal}]]&@%*)
(**)
(*lmpAveraged=LinearModelFit[logAveragedWithErrors,fitFuncs,x];*)
(*Print[Row[{"lmpAveraged: ",#//Normal}]]&@%*)
(**)
(*lmpMaxDev=LinearModelFit[logAveragedWithMaxDev,fitFuncs,x];*)
(*Print[Row[{"lmpMaxDev: ",#//Normal}]]&@%*)
(**)
(*fitFunc=a+c Exp[-\[Omega] x]+df x;*)
(*fitSol=FindFit[logAveraged,{fitFunc,{-2<a<2,0<\[Omega]<28}},{a,c,\[Omega],df},x]*)
(**)
(*nlmAveraged=NonlinearModelFit[logAveragedWithErrors,{fitFunc,{-2<a<2,1.9<\[Omega]<=22.1}},{{a,-0.5},{c,-30.},{\[Omega],2.},{df,1.083}},x];*)
(*Print[Row[{"nlmAveraged: ",#//Normal}]]&@%*)
(**)
(*fitFunc=a+c Exp[-\[Omega] x]+c2 Exp[-\[Omega]2 x]+df x;*)
(**)
(*nlmAveraged2=NonlinearModelFit[logAveragedWithErrors,{fitFunc,{-2<a<2,2.<=\[Omega]<=3.,0.1<=\[Omega]2<=1.}},{{a,-0.5},{c,-30.},\[Omega],c2,\[Omega]2,df(*{df,1.083}*)},x,MaxIterations->1000]//Quiet;*)
(*Print[Row[{"nlmAveraged2: ",#//Normal}]]&@%*)
(*nlmMaxDev=NonlinearModelFit[logAveragedWithMaxDev,{fitFunc,{-2<a<2,2.<=\[Omega]<=3.,0.1<=\[Omega]2<=1.}},{{a,-0.5},{c,-30.},\[Omega],c2,\[Omega]2,df(*{df,1.083}*)},x,MaxIterations->1000]//Quiet;*)
(*Print[Row[{"nlmMaxDev: ",#//Normal}]]&@%*)


(* ::Input:: *)
(*maxx=Max[rawData[[All,1]]];*)
(*maxy=Max[rawData[[All,2]]];*)
(**)
(*thresholdBelow=0;*)
(*thresholdAbove=maxx-0;*)
(**)
(*Show[{ListPlot[logAveragedWithMaxDev,PlotStyle->{GrayLevel[0],Directive[Opacity[0.3]]},AxesLabel->{"Log[L]","Log[N]"}]*)
(*,ListPlot[logAveragedWithErrors,PlotStyle->RGBColor[1, 0, 0],PlotLegends->{"logAveragedWithErrors"}(*Placed[SwatchLegend[Automatic],Right]*)]*)
(*(*,Plot[lmp[x],{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{,Thickness->0.004}]*)*)
(*,(*Plot[lmAveraged[x],{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{,Thickness->0.004}]*)
(*,*)Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 0, 0.5],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@lmpAveraged[x]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[1, Rational[2, 3], 1],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@(fitFunc/. fitSol)*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 1, 0.5],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@nlmAveraged[x]*)
(**)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.76, 0.63, 0.19],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@nlmMaxDev[x]*)
(**)
(*,Plot[x (dfSLE/.bb->b)-1,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0, 0, 1],Dashed}]}*)
(*,(*PlotRange->All,*)PlotLabel->Row[{" b = ",b}]*)
(*(*,Epilog->{Directive[Dashed,],*)
(*Line[{{Log[thresholdBelow],0},{Log[thresholdBelow],Log[maxy]//N}}]*)
(*,Line[{{Log[thresholdAbove],0},{Log[thresholdAbove],Log[maxy]//N}}]}*)*)
(*,PlotRange->{All,{0,All}},AxesOrigin->{1,0},ImageSize->700]*)
(**)
(*Print[ (*"Full data - Linear fit : Subscript[d, f]=",Around[Quiet@lm["ParameterTable"][[1]][[1,3,2]],\[Pi]*lm["ParameterErrors"][[2]]],*)(*"*)
(*Averaged data fit with errors : Subscript[d, f]=",Around[Quiet@lmAveraged["ParameterTable"][[1]][[1,3,2]],\[Pi]*lmAveraged["ParameterErrors"][[2]]],*)"*)
(*Averaged data fit with errors and " ,Total@fitFuncs," \!\(\*TemplateBox[<|\"color\" -> RGBColor[0.5, 0, 0.5]|>,\n\"RGBColorSwatchTemplate\"]\): \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lmpAveraged["ParameterTable"][[1]][[1,2,2]],\[Pi]*lmpAveraged["ParameterErrors"][[1]]],"*)
(**)
(*",fitFunc,"  fit  \!\(\*TemplateBox[<|\"color\" -> RGBColor[1, Rational[2, 3], 1]|>,\n\"RGBColorSwatchTemplate\"]\): \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[df/.fitSol,Null],", Parameters:",fitSol,"*)
(*Averaged data fit with errors and " ,fitFunc,"nonlinearModel \!\(\*TemplateBox[<|\"color\" -> RGBColor[0.5, 1, 0.5]|>,\n\"RGBColorSwatchTemplate\"]\): \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@nlmAveraged["ParameterTable"][[1,1,-1,2]],\[Pi]*Quiet@nlmAveraged["ParameterTable"][[1,1,-1,3]]],"*)
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
(*Test with mean over same L After LOG: <Log[y]>, what we want??? NO WE WANT THE OPPOSITE SINCE <N>~(L^Subscript[d, f])!!!!*)


(* ::Input:: *)
(*(*1. Take the Log of all 4,000,000 raw points instantly using vectorization*)*)
(*logPackedData=Log[rawData];*)
(**)
(*(*2. Group by the log(x) column*)*)
(*gatheredLog=GatherBy[logPackedData,First];*)
(**)
(*(*3. Pull out the exact x-values on the log scale*)*)
(*logXVals=N[gatheredLog[[All,1,1]]];*)
(*logYGroups=N[gatheredLog[[All,All,2]]];*)


(* ::Input:: *)
(*(*4. Compute the Means and Standard Deviations directly in log space*)*)
(*logMeans=Mean/@logYGroups/. 0.->1.0`*^-10;*)
(*logStdDevs=StandardDeviation/@logYGroups/. Indeterminate->1.0`*^-10;*)
(*logStdDevs=logStdDevs/. 0.->1.0`*^-10;*)
(*(*logStdDevs=Sqrt[(logStdDevs/logMeans^2)];*)*)
(*logStdDevs=Sqrt[logStdDevs];*)


(* ::Input:: *)
(*(*5. Create your clean,mathematically sound Around matrix*)*)
(*Averagedb4AfterValidation=LogData=Transpose[{logXVals,MapThread[Around,{logMeans,logStdDevs}]}];*)


(* ::Input:: *)
(*LogData[[1]]*)
(*Position[%,_?(Element[#,Reals]=!=True&)]*)


(* ::Input:: *)
(*(*6. Run the weighted fit*)*)
(*lmAveraged=LinearModelFit[LogData,x,x,Weights->Automatic];*)
(*lmAveraged//Normal*)
(*lmAveraged["ParameterTable"]*)


(* ::Item:: *)
(**)


(* ::Input:: *)
(*fitFuncs=Table[x^(-i),{i,-1,3}];*)
(*(*fitFuncs=Append[Table[x^(-i),{i,-1,0}],Exp[-x]];*)*)
(*(*fitFuncs={x,1,Exp[-x],Exp[-2 x]};(*Exp[-1.3 x]*)*)*)
(*fitFuncs={x,1,Exp[-2 x]};*)
(*fitFunc=a+c Exp[-\[Omega] x]+df x;*)
(**)
(*lmAveraged=LinearModelFit[LogData,x,x(*,Weights->Automatic*)];*)
(*%//Normal*)
(**)
(*lmpAveraged=LinearModelFit[LogData,fitFuncs,x];*)
(*%//Normal*)
(**)
(*(*fitFunc=a+c Exp[-\[Omega] x]+df x;*)
(*fitSol=FindFit[LogData[[All]][],{fitFunc,{-2<a<2,0<\[Omega]<28}},{a,c,\[Omega],df},x]*)*)
(**)
(*nlmAveraged=NonlinearModelFit[LogData,{fitFunc(*,{-2<a<2,0<\[Omega]<3}*)},{a,c,\[Omega],df},x];*)
(*%//Normal*)


(* ::Input:: *)
(*maxx=Max[rawData[[All,1]]];*)
(*maxy=Max[rawData[[All,2]]];*)
(**)
(*thresholdBelow=0;*)
(*thresholdAbove=maxx-0;*)
(**)
(*Show[{(*ListPlot[randomLogSample,PlotStyle->,AxesLabel->{Log[L],Log[N]}]*)
(*,*)ListPlot[LogData,PlotStyle->RGBColor[0, 1, 1]]*)
(*(*,Plot[lmp[x],{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{,Thickness->0.004}]*)*)
(*,Plot[lmAveraged[x],{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[1, 0, 0],Thickness->0.004}]*)
(*,Plot[lmpAveraged[x],{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 0, 0.5],Thickness->0.004}]*)
(*,Plot[nlmAveraged[x],{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[1, Rational[2, 3], 1],Thickness->0.004}]*)
(**)
(*,Plot[x (dfSLE/.bb->b)-1,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0, 0, 1],Dashed}]}*)
(**)
(*,PlotLabel->Row[{" b = ",b}]*)
(*,PlotRange->{All,{0,8}},AxesOrigin->{1,0}]*)
(**)
(*Print[ "Averaged data fit with errors \!\(\*TemplateBox[<|\"color\" -> RGBColor[1, 0, 0]|>,\n\"RGBColorSwatchTemplate\"]\): \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lmAveraged["ParameterTable"][[1]][[1,3,2]],\[Pi]*lmAveraged["ParameterErrors"][[2]]],"*)
(*Averaged data fit with errors and " ,fitFunc," \!\(\*TemplateBox[<|\"color\" -> RGBColor[1, Rational[2, 3], 1]|>,\n\"RGBColorSwatchTemplate\"]\): \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@nlmAveraged["ParameterTable"][[1,1,5,2]],\[Pi]*Quiet@nlmAveraged["ParameterTable"][[1,1,5,3]]],"*)
(*	Parameters:",Map[Row[{#[[1]],"=",Around[#[[2]],#[[3]]]}]&,Quiet@nlmAveraged["ParameterTable"][[1,1,2;;-1,1;;3]]],"*)
(*Averaged data fit with errors and " ,Total@fitFuncs," \!\(\*TemplateBox[<|\"color\" -> RGBColor[0.5, 0, 0.5]|>,\n\"RGBColorSwatchTemplate\"]\): \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lmpAveraged["ParameterTable"][[1]][[1,2,2]],\[Pi]*lmpAveraged["ParameterErrors"][[1]]],"*)
(**)
(*Result from the litterature (exact with SLE) \!\(\*TemplateBox[<|\"color\" -> RGBColor[0, 0, 1]|>,\n\"RGBColorSwatchTemplate\"]\): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Input:: *)
(*Quiet@lmpAveraged["ParameterTable"][[1]]*)
(*Quiet@nlmAveraged["ParameterTable"][[1]]*)


(* ::Subsection::Closed:: *)
(*Drop both first and/or last few*)


(* ::Input:: *)
(*maxx=Max[rawData[[All,1]]];*)
(*maxy=Max[rawData[[All,2]]];*)
(**)
(*thresholdBelow=6;*)
(*thresholdAbove=maxx-0;*)
(**)
(*dropped=droppedb4AfterValidation=Select[LogData,Log[thresholdBelow]<#[[1]]<Log[thresholdAbove]&];*)
(**)
(*lmdropped=LinearModelFit[dropped,x,x,Weights->Automatic];*)
(**)
(*lmpdropped=LinearModelFit[dropped,fitFuncs,x];*)
(**)
(*Show[{ListPlot[LogData,PlotStyle->GrayLevel[0],AxesLabel->{"Log[L]","Log[N]"}]*)
(*,ListPlot[dropped,PlotStyle->RGBColor[0, 1, 1]]*)
(**)
(*,Plot[lmpAveraged[x],{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0, Rational[2, 3], 0],Thickness->0.004}]*)
(*(*,Plot[fitFunc/.fitSol,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{,Thickness->0.004}]*)*)
(**)
(*,Plot[lmdropped[x],{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[1, 0, 0],Thickness->0.004}]*)
(*,Plot[lmpdropped[x],{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 0, 0.5],Thickness->0.004}]*)
(**)
(**)
(*,Plot[x (dfSLE/.bb->b)-1,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0, 0, 1],Dashed}]*)
(*}*)
(**)
(*,(*PlotRange->All,*)PlotLabel->Row[{" b = ",b}]*)
(*,Epilog->{Directive[Dashed,GrayLevel[0.5]],*)
(*Line[{{Log[thresholdBelow],0},{Log[thresholdBelow],Log[maxy]//N}}]*)
(*,Line[{{Log[thresholdAbove],0},{Log[thresholdAbove],Log[maxy]//N}}]}*)
(*,PlotRange->{All,{0,8}},AxesOrigin->{1,0}]*)
(**)
(*Print[ "Full averaged data - Linear fit : \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lmAveraged["ParameterTable"][[1]][[1,3,2]],\[Pi]*lmAveraged["ParameterErrors"][[2]]],"*)
(*Dropped data - Linear fit \!\(\*TemplateBox[<|\"color\" -> RGBColor[1, 0, 0]|>,\n\"RGBColorSwatchTemplate\"]\): \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lmdropped["ParameterTable"][[1]][[1,3,2]],\[Pi]*lmdropped["ParameterErrors"][[2]]],"*)
(**)
(*Full averaged data - ",Total@fitFuncs," fit \!\(\*TemplateBox[<|\"color\" -> RGBColor[0, Rational[2, 3], 0]|>,\n\"RGBColorSwatchTemplate\"]\): \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lmpAveraged["ParameterTable"][[1]][[1,2,2]],\[Pi]*lmpAveraged["ParameterErrors"][[1]]],"*)
(*Dropped data with " ,Total@fitFuncs," fit \!\(\*TemplateBox[<|\"color\" -> RGBColor[0.5, 0, 0.5]|>,\n\"RGBColorSwatchTemplate\"]\): \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lmpdropped["ParameterTable"][[1]][[1,2,2]],\[Pi]*lmpdropped["ParameterErrors"][[1]]],"*)
(**)
(*Result from the litterature (exact with SLE) \!\(\*TemplateBox[<|\"color\" -> RGBColor[0, 0, 1]|>,\n\"RGBColorSwatchTemplate\"]\): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Subsection::Closed:: *)
(*Extra analysis*)


(* ::Subsubsection:: *)
(*Plot with different drops below*)


(* ::Input:: *)
(*dfDropped=ParallelTable[With[{*)
(*lmdropped=LinearModelFit[Log[DeleteCases[rawData,{a_,_}/;(a<i )]],x,x]},*)
(*{i,Quiet@lmdropped["ParameterTable"][[1]][[1,3,2]]}],{i,5,100}];*)


(* ::Input:: *)
(*dfDroppedMore=ParallelTable[With[{*)
(*lmdropped=LinearModelFit[Log[DeleteCases[rawData,{a_,_}/;(a<i )]],x,x]},*)
(*{i,Quiet@lmdropped["ParameterTable"][[1]][[1,3,2]]}],{i,101,200}];*)


(* ::Input:: *)
(*dfDroppedMore2=ParallelTable[With[{*)
(*lmdropped=LinearModelFit[Log[DeleteCases[rawData,{a_,_}/;(a<i )]],x,x]},*)
(*{i,Quiet@lmdropped["ParameterTable"][[1]][[1,3,2]]}],{i,201,400}];*)


(* ::Input:: *)
(*dfDroppedMore3=ParallelTable[With[{*)
(*lmdropped=LinearModelFit[Log[DeleteCases[rawData,{a_,_}/;(a<i )]],x,x]},*)
(*{i,Quiet@lmdropped["ParameterTable"][[1]][[1,3,2]]}],{i,401,800}];*)


(* ::Input:: *)
(*dfDroppedMore4=ParallelTable[With[{*)
(*lmdropped=LinearModelFit[Log[DeleteCases[rawData,{a_,_}/;(a<i )]],x,x]},*)
(*{i,Quiet@lmdropped["ParameterTable"][[1]][[1,3,2]]}],{i,801,1000}];*)


(* ::Input:: *)
(*dfTogether=Join[dfDropped,dfDroppedMore,dfDroppedMore2,dfDroppedMore3,dfDroppedMore4];*)
(*(*fit=LinearModelFit[dfTogether,{1/Log[x],1},x];*)
(*Quiet@fit["ParameterTable"][[1]]*)*)
(**)
(*lmDrops=LinearModelFit[DeleteCases[dfTogether,{x_,_}/;(x<400)],{1},x]*)
(**)
(*Show[*)
(*{ListPlot[dfTogether,PlotRange->{All,All}]*)
(*(*,Plot[fit[x],{x,0,1000},PlotStyle->Red]*)*)
(*,Plot[dfSLE/.bb->b/1.,{x,0,1000},PlotStyle->RGBColor[0, 0, 1]]*)
(*,Plot[lmDrops[x],{x,0,1000},PlotStyle->RGBColor[0, Rational[2, 3], 0]]*)
(*}*)
(*,AxesLabel->{"Dropping threshold",Subscript[d, f]},PlotRange->All]*)


(* ::Subsubsection::Closed:: *)
(*Plot with different drops Above BAD*)


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
(*Plot with different POSITIONS of the same WINDOW:   (i)<  a < (i + 500)*)


(* ::Input:: *)
(*window=500;(*Set this*)*)
(**)
(*dfDroppedWindow=Table[With[{*)
(*lmdropped=LinearModelFit[Log[DeleteCases[rawData,{a_,_}/;(a<(i) ||  a>(i+window))]],x,x]},*)
(*{i,Quiet@lmdropped["ParameterTable"][[1]][[1,3,2]]}],{i,0,1500}];*)


(* ::Input:: *)
(**)
(*Show[*)
(*{ListPlot[dfDroppedWindow,PlotRange->{All,All}]*)
(*,Plot[dfSLE/.bb->b/1.,{x,0,1500},PlotStyle->Blue]*)
(*},PlotRange->{All,All}]*)


(* ::Subsubsection:: *)
(*Plot with different POSITIONS of the same WINDOW:  Changing window*)


(* ::Input:: *)
(*window=500;(*Set this*)*)


(* ::Input:: *)
(*windowPlots=Table[*)
(*dfDroppedWindow={window,Table[With[{*)
(*lmdropped=LinearModelFit[Log[DeleteCases[rawData,{a_,_}/;(a<(i) ||  a>(i+window))]],x,x]},*)
(*{i,Around[Quiet@lmdropped["ParameterTable"][[1]][[1,3,2]],\[Pi]*lmdropped["ParameterErrors"][[2]]]}],{i,0,2000-window,20}]}*)
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


(* ::Section::Closed:: *)
(*b=15 With points from the CLUSTER with Square BC with Log (+extra analysis)*)


(* ::Input:: *)
(*b=15;*)
(**)
(*rawData=Import["D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d-FromCluster\\b15-clean_merged_data-Square.csv","CSV"];*)
(*(*Immediately lock it into a Packed Array*)*)
(*rawData=data15square=Developer`ToPackedArray[rawData];*)
(* (* MODIFY FILE NAME *)*)
(**)
(*Length[rawData]*)


(* ::Input:: *)
(*ListPlot[rawData]*)
(*ListLogLogPlot[rawData]*)


(* ::Subsection::Closed:: *)
(*Test with mean over same L BEFORE LOG: Log[<y>]	VERY GOOD AGREEMENT?*)


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
(*stdDevs=StandardDeviation/@yGroups/. Indeterminate->1.0`*^-8;*)
(*stdDevs=stdDevs/. 0.->1.0`*^-8;*)
(**)
(*(* Maximum deviation*)*)
(*maxDevs=MapThread[Max[Abs[#1-#2]]&,{means//N,yGroups}];*)
(**)
(**)
(*(*5. Combine them using the Threaded Around wrapper*)*)
(**)
(*averaged=Transpose[{xValues,means}];*)
(*averagedWithErrors=Transpose[{xValues,MapThread[Around,{means,stdDevs}]}];*)
(*averagedWithMaxDev=Transpose[{xValues,MapThread[Around,{means,maxDevs}]}];*)


(* ::Input:: *)
(*Averagedb15square=averaged;*)


(* ::Item:: *)
(**)


(* ::Input:: *)
(*logAveraged=Log[averaged];*)
(*logAveragedWithErrors=Log[averagedWithErrors]/. 0->Around[1.0`*^-6,1.0`*^-6];*)
(*logAveragedWithMaxDev=Log[averagedWithMaxDev]/. 0->Around[1.0`*^-6,1.0`*^-6];*)


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


(* ::Input:: *)
(*fitFuncs=Table[x^(-i),{i,-1,3}];*)
(*(*fitFuncs=Append[Table[x^(-i),{i,-1,0}],Exp[-x]];*)*)
(*(*fitFuncs={x,1,Exp[-x],Exp[-2 x]};(*Exp[-1.3 x]*)*)*)
(*fitFuncs={x,1,Exp[-2 x],Exp[-x]};*)
(*fitFunc=a+c Exp[-\[Omega] x]+df x;*)
(**)
(*lmAveraged=LinearModelFit[logAveragedWithErrors,x,x(*,Weights->Automatic*)];*)
(*Print[Row[{"lmAveraged: ",#//Normal}]]&@%*)
(*lmMaxDev=LinearModelFit[logAveragedWithMaxDev,x,x(*,Weights->Automatic*)];*)
(*Print[Row[{"lmMaxDev: ",#//Normal}]]&@%*)
(**)
(*lmpAveraged=LinearModelFit[logAveragedWithErrors,fitFuncs,x];*)
(*Print[Row[{"lmpAveraged: ",#//Normal}]]&@%*)
(**)
(*lmpMaxDev=LinearModelFit[logAveragedWithMaxDev,fitFuncs,x];*)
(*Print[Row[{"lmpMaxDev: ",#//Normal}]]&@%*)
(**)
(*fitFunc=a+c Exp[-\[Omega] x]+df x;*)
(*fitSol=FindFit[logAveraged,{fitFunc,{-2<a<2,0<\[Omega]<28}},{a,c,\[Omega],df},x]*)
(**)
(*nlmAveraged=NonlinearModelFit[logAveragedWithErrors,{fitFunc,{-2<a<2,1.9<\[Omega]<=22.1}},{{a,-0.5},{c,-30.},{\[Omega],2.},{df,1.083}},x];*)
(*Print[Row[{"nlmAveraged: ",#//Normal}]]&@%*)
(**)
(*fitFunc=a+c Exp[-\[Omega] x]+c2 Exp[-\[Omega]2 x]+df x;*)
(**)
(*nlmAveraged=NonlinearModelFit[logAveragedWithErrors,{fitFunc,{-2<a<2,2.<=\[Omega]<=3.,0.1<=\[Omega]2<=1.}},{{a,-0.5},{c,-30.},\[Omega],c2,\[Omega]2,df(*{df,1.083}*)},x,MaxIterations->1000]//Quiet;*)
(*Print[Row[{"nlmAveraged2: ",#//Normal}]]&@%*)
(*nlmMaxDev=NonlinearModelFit[logAveragedWithMaxDev,{fitFunc,{(*-2<a<2,*)2.<=\[Omega]<=15.,0.1<=\[Omega]2<2.}},{a(*{a,-0.5}*),c(*{c,-30.}*),\[Omega],c2,\[Omega]2,df(*{df,1.083}*)},x,MaxIterations->1000]//Quiet;*)
(*Print[Row[{"nlmMaxDev: ",#//Normal}]]&@%*)
(*nlmMaxDev["ParameterTable"]*)


(* ::Input:: *)
(*maxx=Max[rawData[[All,1]]];*)
(*maxy=Max[rawData[[All,2]]];*)
(**)
(*thresholdBelow=0;*)
(*thresholdAbove=maxx-0;*)
(**)
(*Show[{ListPlot[logAveragedWithMaxDev,PlotStyle->{GrayLevel[0],Directive[Opacity[0.3]]},AxesLabel->{"Log[L]","Log[N]"}]*)
(*,ListPlot[logAveragedWithErrors,PlotStyle->RGBColor[0, 1, 1]]*)
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
(*,PlotRange->{All,{0,All}},AxesOrigin->{1,0}]*)
(**)
(*Print[ (*"Full data - Linear fit : Subscript[d, f]=",Around[Quiet@lm["ParameterTable"][[1]][[1,3,2]],\[Pi]*lm["ParameterErrors"][[2]]],*)"*)
(*Averaged data fit with errors \!\(\*TemplateBox[<|\"color\" -> RGBColor[1, 0, 0]|>,\n\"RGBColorSwatchTemplate\"]\): \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lmAveraged["ParameterTable"][[1]][[1,3,2]],\[Pi]*lmAveraged["ParameterErrors"][[2]]],"*)
(*Averaged data fit with errors and " ,Total@fitFuncs," \!\(\*TemplateBox[<|\"color\" -> RGBColor[0.5, 0, 0.5]|>,\n\"RGBColorSwatchTemplate\"]\): \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lmpAveraged["ParameterTable"][[1]][[1,2,2]],\[Pi]*lmpAveraged["ParameterErrors"][[1]]],"*)
(**)
(*",fitFunc,"  fit  \!\(\*TemplateBox[<|\"color\" -> RGBColor[1, Rational[2, 3], 1]|>,\n\"RGBColorSwatchTemplate\"]\): \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[df/.fitSol,Null],", Parameters:",fitSol,"*)
(*Averaged data fit with errors and " ,fitFunc,"nonlinearModel \!\(\*TemplateBox[<|\"color\" -> RGBColor[0.5, 1, 0.5]|>,\n\"RGBColorSwatchTemplate\"]\): \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@nlmAveraged["ParameterTable"][[1,1,5,2]],(*\[Pi]**)Quiet@nlmAveraged["ParameterTable"][[1,1,5,3]]],"*)
(**)
(*Averaged data fit with MaxDevs and " ,fitFunc,"nonlinearModel \!\(\*TemplateBox[<|\"color\" -> RGBColor[0.76, 0.63, 0.19]|>,\n\"RGBColorSwatchTemplate\"]\): \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@nlmMaxDev["ParameterTable"][[1,1,-1,2]],(*\[Pi]**)Quiet@nlmMaxDev["ParameterTable"][[1,1,-1,3]]],", *)
(*	",Row[{"Parameters:",Quiet@nlmMaxDev["ParameterTable"]}],(*,"*)
(**)
(*Full data - ",Total@fitFuncs," fit: Subscript[d, f]=",Around[Quiet@lmp["ParameterTable"][[1]][[1,2,2]],\[Pi]*lmp["ParameterErrors"][[1]]]*)"*)
(**)
(*Result from the litterature (exact with SLE) \!\(\*TemplateBox[<|\"color\" -> RGBColor[0, 0, 1]|>,\n\"RGBColorSwatchTemplate\"]\): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Input:: *)
(*Quiet@lmpAveraged["ParameterTable"][[1]]*)
(*Quiet@nlmAveraged["ParameterTable"][[1]]*)


(* ::Input:: *)
(*Quiet@nlmMaxDev["ParameterTable"][[1,All,1;;3]]//Normal*)


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
(**)


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


(* ::Section:: *)
(*Application to b=4*)


(* ::Input:: *)
(*b=4;*)
(**)
(*rawData=Import["data4Sqaure-AfterValidation.mx"];*)
(**)
(*Length[rawData]*)


(* ::Item::Closed:: *)
(*Run once to export MX file*)


(* ::Input:: *)
(*data4Square=Import["D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d-FromCluster\\b4-clean_merged_data-AfterValidation.csv","CSV"];*)
(*(*Immediately lock it into a Packed Array*)*)
(*data4Square=Developer`ToPackedArray[rawData];*)
(* (* MODIFY FILE NAME *)*)
(*Length[data4Square]*)


(* ::Input:: *)
(*logSpacing=Import["D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d-FromCluster\\b4-clean_merged_data-SquareBC-LogSpacing.csv","CSV"];*)
(*logSpacing = Developer`ToPackedArray[logSpacing];*)
(*Length[logSpacing]*)


(* ::Input:: *)
(*rawData=Join[data4Square, logSpacing];*)
(*Length[rawData]*)


(* ::Input:: *)
(*Export["data4Sqaure-AfterValidation.mx",rawData,"MX"]*)


(* ::Item::Closed:: *)
(*Continues*)


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
(*stdDevs=StandardDeviation/@yGroups/. Indeterminate->1.0`*^-8;*)
(*stdDevs=stdDevs/. 0.->1.0`*^-8;*)
(**)
(*(* Maximum deviation*)*)
(*maxDevs=MapThread[Max[Abs[#1-#2]]&,{means//N,yGroups}];*)
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
(*Fixed size analysis to try and get the best parameters (e.g. bin size) -> Around 280-290 (with the estimate it's a bit bigger)*)


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


(* ::Subitem:: *)
(*Find it in gathered*)


(* ::Input:: *)
(*Position[gathered,_?(#[[1,1]]==599&),{1}]*)


(* ::Input:: *)
(*comparison=gathered[[775]];*)


(* ::Input:: *)
(*meanComparison=Mean[comparison[[All,2]]]//N*)


(* ::Subitem:: *)
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
(*,PlotRange->{{6.3,6.5},All},AxesOrigin->{4,0}]*)
(**)
(*Show[{ListPlot[logAveragedWithMaxDev,PlotStyle->{GrayLevel[0],Directive[Opacity[0.3]]},AxesLabel->{"Log[L]","Log[N]"}]*)
(*,ListPlot[logAveragedWithErrors,PlotStyle->RGBColor[0, 0.78, 1]]*)
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


(* ::Subsection:: *)
(*Looking for the best fitting strategy*)


(* ::Subsubsection:: *)
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


(* ::Subsection::Closed:: *)
(*Drop both first and/or last few 		TBD*)


(* ::Input:: *)
(*maxx=Max[rawData[[All,1]]];*)
(*maxy=Max[rawData[[All,2]]];*)
(**)
(*thresholdBelow=50;*)
(*thresholdAbove=maxx-0;*)
(**)
(*droppedWithMaxDev=Select[logAveragedWithMaxDev,Log[thresholdBelow]<#[[1]]<Log[thresholdAbove]&];*)
(*droppedWithErrors=Select[logAveragedWithErrors,Log[thresholdBelow]<#[[1]]<Log[thresholdAbove]&];*)
(**)
(**)
(*lmdroppedWithMaxDev=LinearModelFit[droppedWithMaxDev,x,x,Weights->Automatic];*)
(*lmdroppedWithErrors=LinearModelFit[droppedWithErrors,x,x,Weights->Automatic];*)
(*(*lmpdropped=LinearModelFit[dropped,fitFuncs,x];*)*)
(**)
(*Show[{ListPlot[logAveragedWithMaxDev,PlotStyle->{GrayLevel[0],Directive[Opacity[0.3]]},AxesLabel->{"Log[L]","Log[N]"}]*)
(*,ListPlot[logAveragedWithErrors,PlotStyle->RGBColor[0, 0.78, 1],PlotLegends->{"logAveragedWithErrors"}]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 0, 0.5],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@lmdroppedWithMaxDev[x]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[1, 0.68, 0.6],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@lmdroppedWithErrors[x]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 0.68, 0.5],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@lmAveragedWithStdDevs[x]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.85, 0, 0.5],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@lmAveragedWithStdDevsGlobal[x]*)
(**)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0, 0, 1],Dashed},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@(x (dfSLE/.bb->N[b])-1)*)
(*}*)
(*,Epilog->{Directive[Dashed,GrayLevel[0.5]],*)
(*Line[{{Log[thresholdBelow],0},{Log[thresholdBelow],Log[maxy]//N}}]*)
(*,Line[{{Log[thresholdAbove],0},{Log[thresholdAbove],Log[maxy]//N}}]}*)
(*,PlotLabel->Row[{" b = ",b}]*)
(*,PlotRange->{All,{0,All}}(*PlotRange->{{Log[thresholdBelow],Log[maxy]},{4,7.2}}*),AxesOrigin->{(*Log[thresholdBelow]*)1,0},ImageSize->700]*)


(* ::Subsubsection::Closed:: *)
(*FindFit VS NonlinearModelFit: *)
(*FindFit just returns the solution while NonlinearModelFit returns all the fit statistics*)


(* ::Input:: *)
(*fitFunc=a+c Exp[-\[Omega] x]+df x;*)
(*FindFit[logAveraged,{fitFunc(*,{-2<a<2,0<\[Omega]<28}*)},{a,c,\[Omega],df},x]*)
(*NonlinearModelFit[logAveraged,{fitFunc(*,{-2<a<2,0<\[Omega]<28}*)},{a,c,\[Omega],df},x];*)
(*%//Normal*)


(* ::Subsubsection:: *)
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
(*	It seems to always be  shift=4 (also 4.1 if the first point is included)*)


(* ::Subsubsection:: *)
(*With errors from StandardDeviation[]*)


(* ::Input:: *)
(*listOfFits=Table[*)
(*shift:=Plus[{-i,0},#]&;*)
(**)
(*logAveragedWithErrorsShifted=Log[shift/@averagedWithErrors]/. {a_,0}->{a,Around[1.0`*^-6,1.0`*^-6]};*)
(*fitFunc=a+df  x;lmAveragedWithErrorsShifted=NonlinearModelFit[Select[logAveragedWithErrorsShifted,#[[1]]>=0&],{fitFunc(*,{-2<a<2,0<\[Omega]<28}*)},{a,(*c,\[Omega],*)df},x]*)
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


(* ::Subsubsection:: *)
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


(* ::Subsection:: *)
(*Take the Log WITH A SHIFT*)


(* ::Input:: *)
(*shift:=Plus[{-1,0},#]&; (*In the code, the stopping condition is with R-1*)*)
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


(* ::Subsubsection:: *)
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


(* ::Input:: *)
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


(* ::Input:: *)
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
(*Average over big L closed to one another NO  SHIFT	->	Does not seem to help*)


(* ::Input:: *)
(*logAveraged=Log[averaged];*)
(*logAveragedWithErrors=Log[averagedWithErrors]/. 0->Around[1.0`*^-6,1.0`*^-6];*)
(*logAveragedWithEstimatedStdDevs=Log[averagedWithEstimatedStdDevs]/. 0->Around[1.0`*^-6,1.0`*^-6];*)
(*logAveragedWithMaxDev=Log[averagedWithMaxDev]/. 0->Around[1.0`*^-6,1.0`*^-6];*)


(* ::Subitem::Closed:: *)
(*Quick check on error propagation with Around *)


(* ::Input:: *)
(*Log[Around[864.3, 51.137923104944136`]]*)
(*Around[Log[864.],51./864.]*)


(* ::Item:: *)
(*Continues*)


(* ::Input:: *)
(*binSize=0.05;*)
(*logAveraged//N;*)
(**)
(*GroupBy[%,Floor[First[#],binSize]&];*)
(*binnedPoints=Values@%;*)
(*bigLlumpedTogether=Mean/@%;*)
(**)
(*Length[logAveraged]*)
(*Length[bigLlumpedTogether]*)


(* ::Input:: *)
(*binSize=0.05;*)
(*logAveragedWithEstimatedStdDevs//N;*)
(**)
(*GroupBy[%,Floor[First[#],binSize]&];*)
(*binnedPoints=Values@%;*)
(*bigLlumpedTogetherEstimatedStdDevs=Mean/@%;*)
(**)
(*Length[logAveragedWithEstimatedStdDevs]*)
(*Length[bigLlumpedTogetherEstimatedStdDevs]*)


(* ::Input:: *)
(*binSize=0.05;*)
(*logAveragedWithMaxDev//N;*)
(**)
(*GroupBy[%,Floor[First[#],binSize]&];*)
(*binnedPoints=Values@%;*)
(*bigLlumpedTogetherWithMaxDev=Mean/@%;*)
(**)
(*Length[logAveragedWithMaxDev]*)
(*Length[bigLlumpedTogetherWithMaxDev]*)


(* ::Input:: *)
(*Show[{ListPlot[logAveragedWithMaxDev,PlotStyle->{GrayLevel[0],Directive[Opacity[0.3]]},AxesLabel->{"Log[L-4]","Log[N]"}]*)
(*(*,ListPlot[logAveragedWithErrorsShifted,PlotStyle->]*)*)
(*,ListPlot[logAveragedWithEstimatedStdDevs,PlotStyle->{RGBColor[1, 0.55, 1],Directive[Opacity[0.3]]}]*)
(*,ListPlot[bigLlumpedTogetherEstimatedStdDevs,PlotStyle->{RGBColor[0.17, 0.54, 0],Directive[Opacity[0.3]],PointSize->0.003}]*)
(*,ListPlot[bigLlumpedTogetherWithMaxDev,PlotStyle->{RGBColor[0.17, 0.8, 1],Directive[Opacity[0.3]],PointSize->0.003}]*)
(*(*,Plot[lmp[x],{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{,Thickness->0.004}]*)*)
(**)
(*(*,Plot[lmAveragedWithErrorsShifted[x],{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{,Thickness->0.004}]*)*)
(**)
(*,Plot[x (dfSLE/.bb->b)-1,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0, 0, 1],Dashed}]}*)
(*,(*PlotRange->All,*)PlotLabel->Row[{" b = ",b}]*)
(*(*,Epilog->{Directive[Dashed,],*)
(*Line[{{Log[thresholdBelow],0},{Log[thresholdBelow],Log[maxy]//N}}]*)
(*,Line[{{Log[thresholdAbove],0},{Log[thresholdAbove],Log[maxy]//N}}]}*)*)
(*,PlotRange->{All,{0,All}},AxesOrigin->{1,0}]*)


(* ::Input:: *)
(*fitFunc=a-df  Exp[-x]+df x;*)
(*par={a,(*c,\[Omega],*)df};*)
(*lmAveragedLumpedTogether=NonlinearModelFit[Select[bigLlumpedTogether,#[[1]]>0&],{fitFunc(*,{-2<a<2,0<\[Omega]<28}*)},par,x];*)
(*lmAveragedWithErrorsLumpedTogether=NonlinearModelFit[Select[bigLlumpedTogetherEstimatedStdDevs,#[[1]]>0&],{fitFunc(*,{-2<a<2,0<\[Omega]<28}*)},par,x];*)
(*(*lmAveragedWithEstimatedStdDevsShifted=NonlinearModelFit[Select[logAveragedWithEstimatedStdDevsShifted,#[[1]]>0&],{fitFunc(*,{-2<a<2,0<\[Omega]<28}*)},{a,(*c,\[Omega],*)df},x];*)*)
(*lmAveragedWithMaxDevLumpedTogether=NonlinearModelFit[Select[bigLlumpedTogetherWithMaxDev,#[[1]]>0&],{fitFunc(*,{-2<a<2,0<\[Omega]<28}*)},par,x];*)
(**)
(**)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@lmAveragedLumpedTogether*)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@lmAveragedWithErrorsLumpedTogether *)
(*(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@lmAveragedWithEstimatedStdDevsShifted *)*)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@lmAveragedWithMaxDevLumpedTogether*)


(* ::Input:: *)
(*synchronizedPlots=Map[Show[#,ImageSize->500]&,{ListPlot[lmAveragedLumpedTogether["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis, AdjustedRSquared=",lmAveragedLumpedTogether["AdjustedRSquared"]}]]*)
(*,ListPlot[lmAveragedWithErrorsLumpedTogether["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis, AdjustedRSquared=",lmAveragedWithErrorsLumpedTogether["AdjustedRSquared"]}]]*)
(*(*,ListPlot[lmAveragedWithEstimatedStdDevsShifted["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis, AdjustedRSquared=",lmAveragedWithEstimatedStdDevsShifted["AdjustedRSquared"]}]]*)*)
(*,ListPlot[lmAveragedWithMaxDevLumpedTogether["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis of lmAveragedWithMaxDevLumpedTogether\nAdjustedRSquared=",lmAveragedWithMaxDevLumpedTogether["AdjustedRSquared"]}]]}];*)
(**)
(*Multicolumn[synchronizedPlots,2,Appearance->"Framed"]*)
(**)


(* ::Input:: *)
(*Show[{ListPlot[logAveragedWithMaxDevShifted,PlotStyle->{GrayLevel[0],Directive[Opacity[0.3]]},AxesLabel->{"Log[L-4]","Log[N]"}]*)
(*(*,ListPlot[logAveragedWithErrorsShifted,PlotStyle->]*)*)
(*,ListPlot[logAveragedWithEstimatedStdDevsShifted,PlotStyle->{RGBColor[1, 0.55, 1],Directive[Opacity[0.3]]}]*)
(*,ListPlot[bigLlumpedTogetherEstimatedStdDevs,PlotStyle->{RGBColor[0.17, 0.54, 0],Directive[Opacity[0.8]],PointSize->0.009}]*)
(*,ListPlot[bigLlumpedTogetherWithMaxDevShifted,PlotStyle->{RGBColor[0.17, 0.8, 1],Directive[Opacity[0.3]],PointSize->0.003}]*)
(**)
(*(*,Plot[lmp[x],{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{,Thickness->0.004}]*)*)
(**)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 1, 0.5],Thickness->0.002},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],{Right,Top}]]&@(lmAveragedWithErrorsLumpedTogether[x])*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.66, 0.16, 1],Thickness->0.002},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],{Right,Top}]]&@(lmAveragedWithMaxDevLumpedTogether[x])*)
(**)
(*,Plot[x (dfSLE/.bb->b)-1,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0, 0, 1],Dashed}]}*)
(*,(*PlotRange->All,*)PlotLabel->Row[{" b = ",b}]*)
(*(*,Epilog->{Directive[Dashed,],*)
(*Line[{{Log[thresholdBelow],0},{Log[thresholdBelow],Log[maxy]//N}}]*)
(*,Line[{{Log[thresholdAbove],0},{Log[thresholdAbove],Log[maxy]//N}}]}*)*)
(*,PlotRange->{All,{0,All}},AxesOrigin->{1,0}]*)


(* ::Subsection::Closed:: *)
(*Average over big L closed to one another AFTER SHIFT	->	Does not seem to help*)


(* ::Input:: *)
(*shift:=Plus[{-4,0},#]&;*)
(**)
(*logAveragedShifted=Log[shift/@averaged];*)
(*logAveragedWithErrorsShifted=Log[shift/@averagedWithErrors]/. {a_,0}->{a,Around[1.0`*^-6,1.0`*^-6]};*)
(*logAveragedWithEstimatedStdDevsShifted=Log[shift/@averagedWithEstimatedStdDevs]/. {a_,0}->{a,Around[1.0`*^-6,1.0`*^-6]};*)
(*logAveragedWithMaxDevShifted=Log[shift/@averagedWithMaxDev]/.{a_,0}->{a,Around[1.0`*^-6,1.0`*^-6]};*)


(* ::Input:: *)
(*binSize=0.05;*)
(*logAveragedShifted//N;*)
(**)
(*GroupBy[%,Floor[First[#],binSize]&];*)
(*binnedPoints=Values@%;*)
(*bigLlumpedTogether=Mean/@%;*)
(**)
(*Length[logAveragedShifted]*)
(*Length[bigLlumpedTogether]*)


(* ::Input:: *)
(*binSize=0.05;*)
(*logAveragedWithEstimatedStdDevsShifted//N;*)
(**)
(*GroupBy[%,Floor[First[#],binSize]&];*)
(*binnedPoints=Values@%;*)
(*bigLlumpedTogetherEstimatedStdDevs=Mean/@%;*)
(**)
(*Length[logAveragedWithEstimatedStdDevsShifted]*)
(*Length[bigLlumpedTogetherEstimatedStdDevs]*)


(* ::Input:: *)
(*binSize=0.05;*)
(*logAveragedWithMaxDevShifted//N;*)
(**)
(*GroupBy[%,Floor[First[#],binSize]&];*)
(*binnedPoints=Values@%;*)
(*bigLlumpedTogetherWithMaxDevShifted=Mean/@%;*)
(**)
(*Length[logAveragedWithMaxDevShifted]*)
(*Length[bigLlumpedTogetherWithMaxDevShifted]*)


(* ::Input:: *)
(*Show[{ListPlot[logAveragedWithMaxDevShifted,PlotStyle->{GrayLevel[0],Directive[Opacity[0.3]]},AxesLabel->{"Log[L-4]","Log[N]"}]*)
(*(*,ListPlot[logAveragedWithErrorsShifted,PlotStyle->]*)*)
(*,ListPlot[logAveragedWithEstimatedStdDevsShifted,PlotStyle->{RGBColor[1, 0.55, 1],Directive[Opacity[0.3]]}]*)
(*,ListPlot[bigLlumpedTogetherEstimatedStdDevs,PlotStyle->{RGBColor[0.17, 0.54, 0],Directive[Opacity[0.3]],PointSize->0.003}]*)
(*,ListPlot[bigLlumpedTogetherWithMaxDevShifted,PlotStyle->{RGBColor[0.17, 0.8, 1],Directive[Opacity[0.3]],PointSize->0.003}]*)
(*(*,Plot[lmp[x],{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{,Thickness->0.004}]*)*)
(**)
(*(*,Plot[lmAveragedWithErrorsShifted[x],{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{,Thickness->0.004}]*)*)
(**)
(*,Plot[x (dfSLE/.bb->b)-1,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0, 0, 1],Dashed}]}*)
(*,(*PlotRange->All,*)PlotLabel->Row[{" b = ",b}]*)
(*(*,Epilog->{Directive[Dashed,],*)
(*Line[{{Log[thresholdBelow],0},{Log[thresholdBelow],Log[maxy]//N}}]*)
(*,Line[{{Log[thresholdAbove],0},{Log[thresholdAbove],Log[maxy]//N}}]}*)*)
(*,PlotRange->{All,{0,All}},AxesOrigin->{1,0}]*)


(* ::Input:: *)
(*fitFunc=a+df x;*)
(*lmAveragedLumpedTogether=NonlinearModelFit[Select[bigLlumpedTogether,#[[1]]>0&],{fitFunc(*,{-2<a<2,0<\[Omega]<28}*)},{a,(*c,\[Omega],*)df},x];*)
(*lmAveragedWithErrorsLumpedTogether=NonlinearModelFit[Select[bigLlumpedTogetherEstimatedStdDevs,#[[1]]>0&],{fitFunc(*,{-2<a<2,0<\[Omega]<28}*)},{a,(*c,\[Omega],*)df},x];*)
(*(*lmAveragedWithEstimatedStdDevsShifted=NonlinearModelFit[Select[logAveragedWithEstimatedStdDevsShifted,#[[1]]>0&],{fitFunc(*,{-2<a<2,0<\[Omega]<28}*)},{a,(*c,\[Omega],*)df},x];*)*)
(*lmAveragedWithMaxDevLumpedTogether=NonlinearModelFit[Select[bigLlumpedTogetherWithMaxDevShifted,#[[1]]>0&],{fitFunc(*,{-2<a<2,0<\[Omega]<28}*)},{a,(*c,\[Omega],*)df},x];*)
(**)
(**)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@lmAveragedLumpedTogether*)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@lmAveragedWithErrorsLumpedTogether *)
(*(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@lmAveragedWithEstimatedStdDevsShifted *)*)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@lmAveragedWithMaxDevLumpedTogether*)


(* ::Input:: *)
(*synchronizedPlots=Map[Show[#,ImageSize->500]&,{ListPlot[lmAveragedLumpedTogether["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis, AdjustedRSquared=",lmAveragedLumpedTogether["AdjustedRSquared"]}]]*)
(*,ListPlot[lmAveragedWithErrorsLumpedTogether["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis, AdjustedRSquared=",lmAveragedWithErrorsLumpedTogether["AdjustedRSquared"]}]]*)
(*(*,ListPlot[lmAveragedWithEstimatedStdDevsShifted["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis, AdjustedRSquared=",lmAveragedWithEstimatedStdDevsShifted["AdjustedRSquared"]}]]*)*)
(*,ListPlot[lmAveragedWithMaxDevLumpedTogether["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis of lmAveragedWithMaxDevLumpedTogether\nAdjustedRSquared=",lmAveragedWithMaxDevLumpedTogether["AdjustedRSquared"]}]]}];*)
(**)
(*Multicolumn[synchronizedPlots,2,Appearance->"Framed"]*)
(**)


(* ::Input:: *)
(*Show[{ListPlot[logAveragedWithMaxDevShifted,PlotStyle->{GrayLevel[0],Directive[Opacity[0.3]]},AxesLabel->{"Log[L-4]","Log[N]"}]*)
(*(*,ListPlot[logAveragedWithErrorsShifted,PlotStyle->]*)*)
(*,ListPlot[logAveragedWithEstimatedStdDevsShifted,PlotStyle->{RGBColor[1, 0.55, 1],Directive[Opacity[0.3]]}]*)
(*,ListPlot[bigLlumpedTogetherEstimatedStdDevs,PlotStyle->{RGBColor[0.17, 0.54, 0],Directive[Opacity[0.8]],PointSize->0.009}]*)
(*,ListPlot[bigLlumpedTogetherWithMaxDevShifted,PlotStyle->{RGBColor[0.17, 0.8, 1],Directive[Opacity[0.3]],PointSize->0.003}]*)
(**)
(*(*,Plot[lmp[x],{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{,Thickness->0.004}]*)*)
(**)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 1, 0.5],Thickness->0.002},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],{Right,Top}]]&@(lmAveragedWithErrorsLumpedTogether[x])*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.66, 0.16, 1],Thickness->0.002},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],{Right,Top}]]&@(lmAveragedWithMaxDevLumpedTogether[x])*)
(**)
(*,Plot[x (dfSLE/.bb->b)-1,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0, 0, 1],Dashed}]}*)
(*,(*PlotRange->All,*)PlotLabel->Row[{" b = ",b}]*)
(*(*,Epilog->{Directive[Dashed,],*)
(*Line[{{Log[thresholdBelow],0},{Log[thresholdBelow],Log[maxy]//N}}]*)
(*,Line[{{Log[thresholdAbove],0},{Log[thresholdAbove],Log[maxy]//N}}]}*)*)
(*,PlotRange->{All,{0,All}},AxesOrigin->{1,0}]*)


(* ::Subsection::Closed:: *)
(*Extra analysis			TBD*)


(* ::Subsubsection:: *)
(*Plot with different drops below*)


(* ::Input:: *)
(*dfDropped=ParallelTable[With[{*)
(*lmdropped=LinearModelFit[Select[logAveragedWithErrors,#[[1]]>=Log[i]&],x,x]},*)
(*{i,Quiet@lmdropped["ParameterTable"][[1]][[1,3,2]]}],{i,5,100}];*)


(* ::Input:: *)
(*dfDroppedMore=ParallelTable[With[{*)
(*lmdropped=LinearModelFit[Select[logAveragedWithErrors,#[[1]]>=Log[i]&],x,x]},*)
(*{i,Quiet@lmdropped["ParameterTable"][[1]][[1,3,2]]}],{i,101,200}];*)


(* ::Input:: *)
(*dfDroppedMore2=ParallelTable[With[{*)
(*lmdropped=LinearModelFit[Select[logAveragedWithErrors,#[[1]]>=Log[i]&],x,x]},*)
(*{i,Quiet@lmdropped["ParameterTable"][[1]][[1,3,2]]}],{i,201,400}];*)


(* ::Input:: *)
(*dfDroppedMore3=ParallelTable[With[{*)
(*lmdropped=LinearModelFit[Select[logAveragedWithErrors,#[[1]]>=Log[i]&],x,x]},*)
(*{i,Quiet@lmdropped["ParameterTable"][[1]][[1,3,2]]}],{i,401,800}];*)


(* ::Input:: *)
(*dfDroppedMore4=ParallelTable[With[{*)
(*lmdropped=LinearModelFit[Select[logAveragedWithErrors,#[[1]]>=Log[i]&],x,x]},*)
(*{i,Quiet@lmdropped["ParameterTable"][[1]][[1,3,2]]}],{i,801,1000}];*)


(* ::Input:: *)
(*dfTogether=Join[dfDropped,dfDroppedMore,dfDroppedMore2,dfDroppedMore3,dfDroppedMore4];*)
(*(*fit=LinearModelFit[dfTogether,{1/Log[x],1},x];*)
(*Quiet@fit["ParameterTable"][[1]]*)*)
(**)
(*lmDrops=LinearModelFit[DeleteCases[dfTogether,{x_,_}/;(x<0)],{1},x]*)
(**)
(*Show[*)
(*{ListPlot[dfTogether,PlotRange->{All,All}]*)
(*(*,Plot[fit[x],{x,0,1000},PlotStyle->Red]*)*)
(*,Plot[dfSLE/.bb->b/1.,{x,0,1000},PlotStyle->RGBColor[0, 0, 1]]*)
(*,Plot[lmDrops[x],{x,0,1000},PlotStyle->RGBColor[0, Rational[2, 3], 0]]*)
(*}*)
(*,AxesLabel->{"Dropping threshold",Subscript[d, f]},PlotRange->All]*)


(* ::Subsubsection::Closed:: *)
(*Plot with different drops Above BAD*)


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


(* ::Subsubsection:: *)
(*Plot with different POSITIONS of the same WINDOW:   (i)<  a < (i + 500)*)


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


(* ::Subsubsection:: *)
(*Plot with different POSITIONS of the same WINDOW:  Changing window*)


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


(* ::Section::Closed:: *)
(*Application to b=15*)


(* ::Input:: *)
(*b=15;*)
(**)
(*rawData=Import["data15Square-AfterValidation.mx"];*)
(**)
(*Length[rawData]*)


(* ::Item::Closed:: *)
(*Import set-up, run once every time b4-clean_merged_data-AfterValidation.csv gets updated *)


(* ::Input:: *)
(*data15=Import["D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d-FromCluster\\b15-clean_merged_data-AfterValidation.csv","CSV"];*)
(**)
(*data15LogBCe10=ToExpression/@Import[FileNameJoin[{"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d-FromCluster\\b15-clean_merged_data-LogBC-e-10.csv"}],"CSV"];*)
(**)
(*data15LogBC=ToExpression/@Import[FileNameJoin[{"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d-FromCluster\\b15-clean_merged_data-LogBC.csv"}],"CSV"];*)
(**)
(*data15square=ToExpression/@Import[FileNameJoin[{"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d-FromCluster\\b15-clean_merged_data-Square.csv"}],"CSV"];*)
(**)
(*rawData=Join[data15,data15LogBCe10, data15LogBC, data15square];*)
(**)
(*(*Immediately lock it into a Packed Array*)*)
(*rawData=data15Square=Developer`ToPackedArray[rawData];*)
(* (* MODIFY FILE NAME *)*)
(*Length[rawData]*)


(* ::Input:: *)
(*Export["data15Square-AfterValidation.mx",rawData,"MX"]*)


(* ::Item:: *)
(*Continues with means over the same L*)


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
(*stdDevs=StandardDeviation/@yGroups/. Indeterminate->1.0`*^-8;*)
(*stdDevs=stdDevs/. 0.->1.0`*^-8;*)
(**)
(*(* Maximum deviation*)*)
(*maxDevs=MapThread[Max[Abs[#1-#2]]&,{means//N,yGroups}];*)
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
(*Fixed size analysis to try and get the best parameters (e.g. bin size) -> Around 280-290 (with the estimate it's a bit bigger)*)


(* ::Input:: *)
(*(* Take the 11th element which contains many points. As it can be seen in the Histogram below, the distribution is far from being symmetric *)*)


(* ::Input:: *)
(*Length/@gathered*)


(* ::Input:: *)
(*Ordering[gathered][[-1]]*)
(*gathered[[%]]*)


(* ::Input:: *)
(*gathered[[{1}]];*)
(*Length/@%*)
(*histoData=(%%[[All,All,2]])[[1]];*)
(*Histogram[#,Length[#],PlotRange->All]&@%*)


(* ::Input:: *)
(*Skewness[histoData]//N*)


(* ::Subitem:: *)
(*identification of best binSize -> best of both skewness ans kurtosis:  289*)


(* ::Input:: *)
(*Partition[histoData,UpTo[100]];*)
(*Length@%*)
(*partData=Mean/@%%;*)
(*Histogram[partData,Automatic,"Probability",PlotRange->All]*)
(*Skewness[partData]//N*)
(*Kurtosis[partData]//N*)


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
(*listOfBinSizesSkewness[[148]]*)
(**)
(*Partition[histoData,UpTo[%[[2]]]];*)
(*Length@%*)
(*partData=Mean/@%%;*)
(*Histogram[partData,Automatic,"Probability",PlotRange->All]*)
(*Skewness[partData]//N*)
(*Kurtosis[partData]//N*)
(**)
(**)
(*listOfBinSizesKurtosis[[49]]*)
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
(*listOfBinSizesSkewness[[124]]*)
(**)
(*Partition[histoData,UpTo[%[[2]]]];*)
(*Length@%*)
(*partData=Mean/@%%;*)
(*Histogram[partData,Automatic,"Probability",PlotRange->All]*)
(*Skewness[partData]//N*)
(*Kurtosis[partData]//N*)
(**)


(* ::Subitem:: *)
(*StdDev Difference: with the estimate it's a bit bigger*)


(* ::Input:: *)
(*StandardDeviation[histoData]//N*)


(* ::Input:: *)
(*StdDevEstimate[histoData,295,"print"->True]*)


(* ::Item:: *)
(*Take the Log*)


(* ::Input:: *)
(*logAveraged=Log[averaged];*)
(*logAveragedWithErrors=Log[averagedWithErrors]/. 0->Around[1.0`*^-6,1.0`*^-6];*)
(*logAveragedWithEstimatedStdDevs=Log[averagedWithEstimatedStdDevs]/. 0->Around[1.0`*^-6,1.0`*^-6];*)
(*logAveragedWithMaxDev=Log[averagedWithMaxDev]/. 0->Around[1.0`*^-6,1.0`*^-6];*)


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


(* ::Input:: *)
(**)


(* ::Input:: *)
(*(*Show[{ListPlot[logAveragedWithMaxDev,PlotStyle->{,Directive[Opacity[0.3]]},AxesLabel->{"Log[L]","Log[N]"}]*)
(*,ListPlot[logAveragedWithErrors,PlotStyle->]*)
(*(*,ListPlot[logAveragedWithEstimatedStdDevs,PlotStyle->{,Directive[Opacity[0.6]]}]*)*)
(*}*)
(*,PlotRange->{{6.3,6.5},All},AxesOrigin->{4,0}]*)
(**)*)
(*Show[{ListPlot[logAveragedWithMaxDev,PlotStyle->{GrayLevel[0],Directive[Opacity[0.3]]},AxesLabel->{"Log[L]","Log[N]"}]*)
(*,ListPlot[logAveragedWithErrors,PlotStyle->{RGBColor[0, 0.78, 1],PointSize->0.001}]*)
(*(*,ListPlot[logAveragedWithEstimatedStdDevs,PlotStyle->{,Directive[Opacity[0.6]]}]*)*)
(*}*)
(*,PlotRange->{All,{0,All}},AxesOrigin->{1,0}]*)
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


(* ::Subsubsection:: *)
(*With and without Method -> "NMinimize" (which looks for the global minimum, I think it's always better)*)


(* ::Item::Closed:: *)
(*No errors*)


(* ::Input:: *)
(*fitFunc=a+c Exp[-\[Omega] x]+df x;*)
(*fitFunc=a-c Exp[- x]+df x;*)
(**)
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
(*,ListPlot[logAveragedWithErrors,PlotStyle->RGBColor[1, 0.55, 1]]*)
(*(*,ListPlot[logAveragedWithEstimatedStdDevs,PlotStyle->{,Directive[Opacity[0.3]]}]*)*)
(**)
(*(*,Plot[lmp[x],{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{,Thickness->0.004}]*)*)
(**)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 1, 0.5],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@nlmAveraged[x]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 0, 0.5],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@nlmAveragedGlobal[x]*)
(**)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0, 0, 1],Dashed},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@(x (dfSLE/.bb->N[b])-0.7)*)
(*}*)
(*,PlotLabel->Row[{" b = ",b}]*)
(**)
(*,PlotRange->{{1,All},{0,All}},AxesOrigin->{1,0},ImageSize->700]*)


(* ::Input:: *)
(*Series[Log[1+x],{x,0,2}]*)


(* ::Input:: *)
(*Quiet@nlmAveragedGlobal["ParameterTable"][[1,1,-1,3]]*)


(* ::Item:: *)
(*Errors obtained with StandardDeviation[]*)


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
(*nlmAveragedWithStdDevsGlobal=NonlinearModelFit[logAveragedWithErrors,{fitFunc,{(*-2<a<2,*)a<0,0.5<\[Omega]<=3,c<0,1<df<1.1}},{a,c,\[Omega](*{a,-0.5},{c,-30.},{\[Omega],2.}*),df(*,{df,1.024}*)},x,MaxIterations->1000,Method->"NMinimize"];*)
(*nlmAveragedWithStdDevsUnconstrainedGlobal=NonlinearModelFit[logAveragedWithErrors,fitFunc,{a,c,\[Omega](*{a,-0.5},{c,-30.},{\[Omega],2.}*),df(*,{df,1.024}*)},x,MaxIterations->1000,Method->"NMinimize"];*)
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
(**)
(*Print[ "Averaged data fit with errors and " ,fitFunc," \!\(\*TemplateBox[<|\"color\" -> RGBColor[0.5, 0, 0.5]|>,\n\"RGBColorSwatchTemplate\"]\): \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@nlmAveragedWithStdDevs["ParameterTable"][[1]][[1,-1,2]],\[Pi]*nlmAveragedWithStdDevs["ParameterTable"][[1]][[1,-1,3]]],", Parameters:",Quiet@nlmAveragedWithStdDevs["ParameterTable"],"*)
(*Averaged data fit with errors and " ,fitFunc," (unconstrained) \!\(\*TemplateBox[<|\"color\" -> RGBColor[0.5, 0.68, 0.5]|>,\n\"RGBColorSwatchTemplate\"]\): \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@nlmAveragedWithStdDevsUnconstrained["ParameterTable"][[1]][[1,-1,2]],\[Pi]*nlmAveragedWithStdDevsUnconstrained["ParameterTable"][[1]][[1,-1,3]]],", Parameters:",Quiet@nlmAveragedWithStdDevsUnconstrained["ParameterTable"],"*)
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


(* ::Subsection:: *)
(*Drop both first and/or last few*)


(* ::Input:: *)
(*maxx=Max[rawData[[All,1]]];*)
(*maxy=Max[rawData[[All,2]]];*)
(**)
(*thresholdBelow=104;*)
(*thresholdAbove=maxx-0;*)
(**)
(*droppedWithMaxDev=Select[logAveragedWithMaxDev,Log[thresholdBelow]<#[[1]]<Log[thresholdAbove]&];*)
(*droppedWithErrors=Select[logAveragedWithErrors,Log[thresholdBelow]<#[[1]]<Log[thresholdAbove]&];*)
(**)
(**)
(*lmdroppedWithMaxDev=LinearModelFit[droppedWithMaxDev,x,x,Weights->Automatic];*)
(*lmdroppedWithErrors=LinearModelFit[droppedWithErrors,x,x,Weights->Automatic];*)
(*(*lmpdropped=LinearModelFit[dropped,fitFuncs,x];*)*)
(**)
(*Show[{ListPlot[logAveragedWithMaxDev,PlotStyle->{GrayLevel[0],Directive[Opacity[0.3]]},AxesLabel->{"Log[L]","Log[N]"}]*)
(*,ListPlot[logAveragedWithErrors,PlotStyle->RGBColor[1, 0.47000000000000003`, 0],PlotLegends->{"logAveragedWithErrors"}]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 0, 0.5],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@lmdroppedWithMaxDev[x]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 0.68, 0.5],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@lmdroppedWithErrors[x]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.85, 0, 0.5],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@nlmAveragedWithMaxStdDevsGlobal[x]*)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 0.68, 0.8],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@nlmAveragedWithMaxStdDevsUnconstrainedGlobal[x]*)
(**)
(*,Plot[#,{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0, 0, 1],Dashed},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@(x (dfSLE/.bb->N[b])-0.64)*)
(*}*)
(*,Epilog->{Directive[Dashed,GrayLevel[0.5]],*)
(*Line[{{Log[thresholdBelow],0},{Log[thresholdBelow],Log[maxy]//N}}]*)
(*,Line[{{Log[thresholdAbove],0},{Log[thresholdAbove],Log[maxy]//N}}]}*)
(*,PlotLabel->Row[{" b = ",b}]*)
(*,PlotRange->{{Log[thresholdBelow],Log[maxy]},{4,7.2}},AxesOrigin->{Log[thresholdBelow],0},ImageSize->700]*)


(* ::Subsection:: *)
(*Extra analysis*)


(* ::Subsubsection:: *)
(*Plot with different drops below*)


(* ::Input:: *)
(*dfDropped=ParallelTable[With[{*)
(*lmdropped=LinearModelFit[Select[logAveragedWithErrors,#[[1]]>=Log[i]&],x,x]},*)
(*{i,Quiet@lmdropped["ParameterTable"][[1]][[1,3,2]]}],{i,5,100}];*)


(* ::Input:: *)
(*dfDroppedMore=ParallelTable[With[{*)
(*lmdropped=LinearModelFit[Select[logAveragedWithErrors,#[[1]]>=Log[i]&],x,x]},*)
(*{i,Quiet@lmdropped["ParameterTable"][[1]][[1,3,2]]}],{i,101,200}];*)


(* ::Input:: *)
(*dfDroppedMore2=ParallelTable[With[{*)
(*lmdropped=LinearModelFit[Select[logAveragedWithErrors,#[[1]]>=Log[i]&],x,x]},*)
(*{i,Quiet@lmdropped["ParameterTable"][[1]][[1,3,2]]}],{i,201,400}];*)


(* ::Input:: *)
(*dfDroppedMore3=ParallelTable[With[{*)
(*lmdropped=LinearModelFit[Select[logAveragedWithErrors,#[[1]]>=Log[i]&],x,x]},*)
(*{i,Quiet@lmdropped["ParameterTable"][[1]][[1,3,2]]}],{i,401,800}];*)


(* ::Input:: *)
(*dfDroppedMore4=ParallelTable[With[{*)
(*lmdropped=LinearModelFit[Select[logAveragedWithErrors,#[[1]]>=Log[i]&],x,x]},*)
(*{i,Quiet@lmdropped["ParameterTable"][[1]][[1,3,2]]}],{i,801,1000}];*)


(* ::Input:: *)
(*dfTogether=Join[dfDropped,dfDroppedMore,dfDroppedMore2,dfDroppedMore3,dfDroppedMore4];*)
(*(*fit=LinearModelFit[dfTogether,{1/Log[x],1},x];*)
(*Quiet@fit["ParameterTable"][[1]]*)*)
(**)
(*lmDrops=LinearModelFit[DeleteCases[dfTogether,{x_,_}/;(x<0)],{1},x]*)
(**)
(*Show[*)
(*{ListPlot[dfTogether,PlotRange->{All,All}]*)
(*(*,Plot[fit[x],{x,0,1000},PlotStyle->Red]*)*)
(*,Plot[dfSLE/.bb->b/1.,{x,0,1000},PlotStyle->RGBColor[0, 0, 1]]*)
(*,Plot[lmDrops[x],{x,0,1000},PlotStyle->RGBColor[0, Rational[2, 3], 0]]*)
(*}*)
(*,AxesLabel->{"Dropping threshold",Subscript[d, f]},PlotRange->All]*)


(* ::Subsubsection::Closed:: *)
(*Plot with different drops Above BAD*)


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


(* ::Subsubsection:: *)
(*Plot with different POSITIONS of the same WINDOW:   (i)<  a < (i + 500)*)


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


(* ::Subsubsection:: *)
(*Plot with different POSITIONS of the same WINDOW:  Changing window*)


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
