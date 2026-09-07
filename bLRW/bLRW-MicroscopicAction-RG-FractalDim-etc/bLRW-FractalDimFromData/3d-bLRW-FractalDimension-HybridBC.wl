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
(*Data analysis for the Fractal Dimension of the b-LRW on 3d square Lattice*)


(* ::Title:: *)
(*After optimization*)


(* ::Chapter:: *)
(*Import data. MODIFY THE HIGHLIGHTED PARTS EVERY TIME*)


(* ::Section::Closed:: *)
(*b=0 With all the points from the CLUSTER			TBD*)


(* ::Subsection:: *)
(*Raw data*)


(* ::Input:: *)
(*ListPlot[rawData]*)


(* ::Input:: *)
(*ListLogLogPlot[rawData]*)


(* ::Input:: *)
(*lm=LinearModelFit[Log[rawData],x,x];*)
(**)
(*Show[ListLogLogPlot[rawData],*)
(*Plot[lm[x],{x,0,1000},PlotStyle->Red]*)
(*,PlotLabel->Row[{" b = ",b}]]*)
(**)
(*Print[ "\!\(\*SubscriptBox[\(d\), \(f\)]\)=",df/.FindFit[Log[rawData],a+df *x,{a,df},x],"\[PlusMinus]",lm["ParameterErrors"][[2]],"*)
(*Result from the litterature (exact with SLE): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Subsection:: *)
(*Drop the first few*)


(* ::Input:: *)
(*threshold=10;*)
(**)
(*dropped=droppedb0=DeleteCases[rawData,{a_,_}/;a<threshold];*)
(*lm=LinearModelFit[Log[dropped],x,x];*)
(**)
(*Show[{ListLogLogPlot[rawData,PlotStyle->Black],ListLogLogPlot[dropped,PlotStyle->Green],*)
(*Plot[lm[x],{x,Log[threshold-1],1000},PlotStyle->Red]}*)
(*,(*PlotRange->All,*)PlotLabel->Row[{" b = ",b}]]*)
(**)
(*Print[ "\!\(\*SubscriptBox[\(d\), \(f\)]\)=",df/.FindFit[Log[dropped],a+df *x,{a,df},x],"\[PlusMinus]",lm["ParameterErrors"][[2]],"*)
(*Result from the litterature (exact with SLE): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Input:: *)
(*(*Good enough?*)*)


(* ::Section:: *)
(*b=1		TBD*)


(* ::Input:: *)
(*b=1;*)
(**)
(*rawData=Import["data1-3d-HybridSq.mx"];*)
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
(*rawData=data1=Import["D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\3d-FromCluster\\b1-3d-clean_merged_data-HybridSq.csv","CSV"];*)
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
(*Export["data1-3d-HybridSq.mx",rawData,"MX"]*)


(* ::Subsection:: *)
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
(*stdDevs=Check[StandardDeviation[#],missing](*/Sqrt[Length[#]]*)&/@yGroups(*/. Indeterminate->1.0`*^-8*);*)


(* ::Input:: *)
(*(*Position[yGroups,_?(Length[#]<50&),1]*)
(*yGroups[[Rest[Flatten@%]]]*)*)


(* ::Input:: *)
(*missingIndices=Flatten@Position[stdDevs,missing]*)
(**)
(*Part[#,missingIndices]&/@{means,stdDevs}*)
(*Times@@%*)
(*stdDevs[[missingIndices]]=%/.missing->Mean[Select[stdDevs/means//N,FreeQ[#,missing]&]]//N*)


(* ::Input:: *)
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
(*(**)
(*(*Standard deviation with my code (Kay's trick for better estimate)*)*)
(*stdDevsEstimated=StdDevEstimate[#,280,"print"->tTrue]&/@yGroups/. Indeterminate->1.0`*^-8;*)
(*stdDevsEstimated=stdDevsEstimated/. 0.->1.0`*^-8;*)
(**)
(*averagedWithEstimatedStdDevs=Transpose[{xValues,MapThread[Around,{means,stdDevsEstimated}]}];*)*)


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


(* ::Subitem:: *)
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


(* ::Subsubsection::Closed:: *)
(*Check how the distribution and the mean change with a lot of statistic 			TBD*)


(* ::Input:: *)
(*Ordering[gathered]*)
(*%[[-10]]*)
(*sample=Part[gathered,#]&@%;*)
(*Length@%*)


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


(* ::Subitem:: *)
(*Histograms*)


(* ::Input:: *)
(*{sample(*,comparison*)};*)
(*Length/@%*)
(*histoData=%%[[All,All,2]];*)
(*Histogram[#,Length[#],PlotRange->All,ImageSize->Large]&/@%*)


(* ::Input:: *)
(*(*Check higher-order moments*)*)
(*sampleY=RandomSample[sample[[All,2]]];*)
(*sampleY[[{1,-1}]]*)
(*(*Skewness[sampleY]//N*)
(*Kurtosis[sampleY]//N*)*)


(* ::Input:: *)
(*(*Mean stability plot:does the cumulative mean stabilize,or jump suddenly?*)*)
(*accumulated=Accumulate[sampleY];*)


(* ::Input:: *)
(*rescaledAccumulated=accumulated/Range[Length[sampleY]];*)


(* ::Input:: *)
(*xmin=50000;*)
(*Show[{*)
(*ListLinePlot[rescaledAccumulated,PlotRange->{{xmin,All(*xmin+1000*)},{45.8,45.9}},AxesLabel->{"n","Running Mean"}]*)
(*,Plot[Mean[sampleY],{x,0,Length[sampleY]},PlotStyle->RGBColor[Rational[2, 3], 0, 0]]}]*)


(* ::Item:: *)
(*Diagnose if the SEM is underestimating: Binning test (kay's??)*)


(* ::Input:: *)
(*binningError[data_List,maxBlock_Integer:1000]:=Module[{n=Length[data],blockSizes,errors},*)
(**)
(*(*Pick block sizes that divide into reasonable chunks*)*)
(*blockSizes=Select[Range[2,maxBlock],Mod[n,#]==0&];*)
(**)
(*errors=Table[With[{blocks=Partition[data,k]},StandardDeviation[Mean/@blocks]/Sqrt[Length[blocks]]],{k,blockSizes}];*)
(**)
(*Transpose[{blockSizes,errors}]];*)


(* ::Input:: *)
(*bData=binningError[sampleY,40000];*)


(* ::Input:: *)
(*ListLinePlot[bData,AxesLabel->{"Block Size (k)","Estimated Error on Mean"},PlotRange->All,ImageSize->Large]*)


(* ::Subsection:: *)
(*Take the Log*)


(* ::Input:: *)
(*(*Drop points with low statistic*)*)
(*averagedWithErrorsOnMeanPurged=Delete[averagedWithErrorsOnMean,Rest[Position[yGroups,_?(Length[#]<500&),{1}]]];*)
(*Length@averagedWithErrorsOnMean*)
(*Length@averagedWithErrorsOnMeanPurged*)


(* ::Input:: *)
(*(*exluded=9;*)
(**)
(*averaged=Select[averaged,#[[1]]=!=""&&#[[1]]>exluded&];*)
(*averagedWithErrors=Select[averagedWithErrors,#[[1]]=!=""&&#[[1]]>exluded&];*)
(**)
(*averagedWithErrorsOnMean=Select[averagedWithErrorsOnMean,#[[1]]=!=""&&#[[1]]>exluded&];*)
(*averagedWithErrorsOnMeanPurged=Select[averagedWithErrorsOnMeanPurged,#[[1]]=!=""&&#[[1]]>exluded&];*)
(**)
(*averagedWithMaxDev=Select[averagedWithMaxDev,#[[1]]=!=""&&#[[1]]>exluded&];*)
(*(*averagedWithEstimatedStdDevs=Select[averagedWithEstimatedStdDevs,#[[1]]=!=""&];*)
(*averagedWithEstimatedStdDevsOnMean=Select[averagedWithEstimatedStdDevsOnMean,#[[1]]=!=""&];*)*)*)


(* ::Input:: *)
(*logAveraged=Log[averaged];*)
(*logAveragedWithErrors=Log[averagedWithErrors]/. 0->Around[1.0`*^-6,1.0`*^-6];*)
(**)
(*logAveragedWithErrorsOnMean=Log[averagedWithErrorsOnMean]/. 0->Around[1.0`*^-6,1.0`*^-6];*)
(*logAveragedWithErrorsOnMeanPurged=Log[averagedWithErrorsOnMeanPurged]/. 0->Around[1.0`*^-6,1.0`*^-6];*)
(**)
(*logAveragedWithMaxDev=Log[averagedWithMaxDev]/. 0->Around[1.0`*^-6,1.0`*^-6];*)
(*(*logAveragedWithEstimatedStdDevs=Log[averagedWithEstimatedStdDevs]/. 0->Around[1.0`*^-6,1.0`*^-6];*)
(*logAveragedWithEstimatedStdDevsOnMean=Log[averagedWithEstimatedStdDevsOnMean]/. 0->Around[1.0`*^-6,1.0`*^-6];*)*)


(* ::Input:: *)
(*logAveragedWithErrors[[1;;10]]*)


(* ::Input:: *)
(*maxx=Max[averaged[[All,1]]];*)
(*maxy=Max[averaged[[All,2]]];*)
(* *)
(*Show[{ListPlot[logAveragedWithMaxDev,PlotStyle->{GrayLevel[0],Directive[Opacity[0.3]],PointSize->0.001},AxesLabel->{"Log[L]","Log[N]"}]*)
(*,ListPlot[logAveragedWithErrorsOnMeanPurged,PlotStyle->{RGBColor[0, 0.78, 1],PointSize->0.01},PlotLegends->PointLegend[{"logAveragedWithErrorsOnMeanPurged"},LegendMarkerSize->10,LegendMarkers->Graphics[Disk[]]]]*)
(*,ListPlot[logAveragedWithErrorsOnMean,PlotStyle->{RGBColor[1, 0.55, 1],Directive[Opacity[0.6]],PointSize->0.005},PlotLegends->PointLegend[{"logAveragedWithErrorsOnMean"},LegendMarkerSize->10,LegendMarkers->Graphics[Disk[]]]]*)
(*,Plot[(x (1.624)-1.4),{x,Log[0+1],Log[maxx]},PlotStyle->{RGBColor[0, 0, 1],Dashed},PlotLegends->Placed[SwatchLegend[{"David Wilson"}],Right]]*)
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


(* ::Item::Closed:: *)
(*small check*)


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


(* ::Subsubsection:: *)
(*NonLinear fit Errors obtained with StandardDeviation[].  WORKS PRETTY WELL (LACKING STATISTICS AT BIG L)*)


(* ::Item::Closed:: *)
(*fitFunc = a + c Exp[-\[Omega] x] + df x*)


(* ::Input:: *)
(*fitFunc=a+c Exp[-\[Omega] x]+df x;*)
(*(*fitFunc=a+c Exp[- x]+df x;*)*)
(**)
(*nlmAveragedWithErrorsOnMean=NonlinearModelFit[logAveragedWithErrorsOnMean,{fitFunc,{a<0,c<0,0.5<\[Omega]<=2,1<df<1.1}},{a,c,\[Omega](*{a,-0.5},{c,-30.},{\[Omega],2.}*),df(*,{df,1.024}*)},x,MaxIterations->1000];*)
(*nlmAveragedWithErrorsOnMeanUnconstrained=NonlinearModelFit[logAveragedWithErrorsOnMean,fitFunc,{a,c,\[Omega](*{a,-0.5},{c,-30.},{\[Omega],2.}*),df(*,{df,1.024}*)},x,MaxIterations->1000];*)
(**)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@nlmAveragedWithErrorsOnMean*)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@nlmAveragedWithErrorsOnMeanUnconstrained*)
(**)
(**)
(*nlmAveragedWithErrorsOnMeanGlobal=NonlinearModelFit[logAveragedWithErrorsOnMean,{fitFunc,{(*-2<a<2,*)a<0,c<0,0.5<\[Omega]<=1.1,1<df<1.1}},{(*a,c,\[Omega],df*){a,-0.5},{c,-30.},{\[Omega],1.},{df,1.024}},x,MaxIterations->1000,Method->"NMinimize"];*)
(**)
(*nlmAveragedWithErrorsOnMeanUnconstrainedGlobal=NonlinearModelFit[logAveragedWithErrorsOnMean,fitFunc,{a,c,\[Omega](*,{\[Omega],2.}*),df(*,{df,1.024}*)},x,MaxIterations->1000,Method->"NMinimize"];*)
(**)
(*nlmAveragedWithErrorsOnMeanUnconstrainedGlobalPurged=NonlinearModelFit[logAveragedWithErrorsOnMeanPurged,{fitFunc,a<0,1<df<2},{{a,-1},c,{\[Omega],2.},{df,1.64}},x,MaxIterations->1000,Method->"NMinimize"];*)
(**)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@nlmAveragedWithErrorsOnMeanGlobal*)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@nlmAveragedWithErrorsOnMeanUnconstrainedGlobal*)
(**)


(* ::Input:: *)
(*maxx=Max[averaged[[All,1]]];*)
(*maxy=Max[averaged[[All,2]]];*)
(**)
(*thresholdBelow=0;*)
(*thresholdAbove=maxx-0;*)
(**)
(*Show[{ListPlot[logAveragedWithMaxDev,PlotStyle->{GrayLevel[0],Directive[Opacity[0.3]]},AxesLabel->{"Log[L]","Log[N]"}]*)
(*,ListPlot[logAveragedWithErrorsOnMean,PlotStyle->RGBColor[1, 0.78, 0.13],PlotLegends->{"logAveragedWithErrorsOnMean"}]*)
(*,ListPlot[logAveragedWithErrorsOnMeanPurged,PlotStyle->{RGBColor[0, 0.78, 1],PointSize->0.005},PlotLegends->PointLegend[{"logAveragedWithErrorsOnMeanPurged"},LegendMarkerSize->5,LegendMarkers->Graphics[Disk[]]]]*)
(*,*)
(*Plot[ReleaseHold[#][x],{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 0, 0.5],Thickness->0.002},PlotLegends->Placed[SwatchLegend[{Row[{#,":\n ",TraditionalForm[ReleaseHold[#][x]]}]}],Right]]&@HoldForm[nlmAveragedWithErrorsOnMean]*)
(*,*)
(*Plot[ReleaseHold[#][x],{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 0.68, 0.5],Thickness->0.002},PlotLegends->Placed[SwatchLegend[{Row[{#,":\n ",TraditionalForm[ReleaseHold[#][x]]}]}],Right]]&@HoldForm[nlmAveragedWithErrorsOnMeanUnconstrained]*)
(*,*)
(**)
(*Plot[ReleaseHold[#][x],{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.85, 0, 0.5],Thickness->0.002},PlotLegends->Placed[SwatchLegend[{Row[{#,":\n ",TraditionalForm[ReleaseHold[#][x]]}]}],Right]]&@HoldForm[nlmAveragedWithErrorsOnMeanUnconstrainedGlobal],*)
(**)
(**)
(*Plot[ReleaseHold[#][x],{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 0.68, 0.8],Thickness->0.002},PlotLegends->Placed[SwatchLegend[{Row[{#,":\n ",TraditionalForm[ReleaseHold[#][x]]}]}],Right]]&@HoldForm[nlmAveragedWithErrorsOnMeanUnconstrainedGlobalPurged]*)
(*,Plot[(x (1.624)-1.4),{x,Log[0+1],Log[maxx]},PlotStyle->{RGBColor[0, 0, 1],Dashed},PlotLegends->Placed[SwatchLegend[{"David Wilson: 1.624"}],Right]]*)
(*}*)
(*,PlotLabel->Row[{" b = ",b}]*)
(*,PlotRange->{All,{0,All}},AxesOrigin->{1,0},ImageSize->600]*)


(* ::Input:: *)
(*Around[nlmAveragedWithErrorsOnMeanUnconstrainedGlobal["ParameterTable"][[1,1,-1,2]],nlmAveragedWithErrorsOnMeanUnconstrainedGlobal["ParameterTable"][[1,1,-1,3]]*\[Pi]]*)
(**)
(*Around[nlmAveragedWithErrorsOnMeanUnconstrainedGlobalPurged["ParameterTable"][[1,1,-1,2]],nlmAveragedWithErrorsOnMeanUnconstrainedGlobalPurged["ParameterTable"][[1,1,-1,3]]*\[Pi]]*)


(* ::Input:: *)
(*ListPlot[Transpose[{logAveragedWithErrorsOnMean[[All,1]],nlmAveragedWithErrorsOnMeanUnconstrainedGlobal["FitResiduals"]}],Filling->Axis,*)
(*AxesLabel->{"x","Residuals (y - y_fit)"},PlotLabel->"FitResiduals - nlmAveragedWithErrorsOnMeanUnconstrainedGlobal",ImageSize->Large]*)
(**)
(**)
(*ListPlot[Transpose[{logAveragedWithErrorsOnMeanPurged[[All,1]],nlmAveragedWithErrorsOnMeanUnconstrainedGlobalPurged["FitResiduals"]}],Filling->Axis,*)
(*AxesLabel->{"x","Residuals (y - y_fit)"},PlotLabel->"FitResiduals - nlmAveragedWithErrorsOnMeanUnconstrainedGlobalPurged",ImageSize->Large]*)


(* ::Input:: *)
(*nlmAveragedWithErrorsOnMeanUnconstrainedGlobal["ANOVATable"][[1,1,3,3]]*)
(*nlmAveragedWithErrorsOnMeanUnconstrainedGlobalPurged["ANOVATable"][[1,1,3,3]]*)


(* ::Item::Closed:: *)
(*fitFunc = a + c Exp[-x] + df x*)


(* ::Input:: *)
(*(*fitFunc=a+c Exp[-\[Omega] x]+df x;*)*)
(*fitFunc=a+c Exp[- x]+df x;*)
(**)
(*nlmAveragedWithErrorsOnMean=NonlinearModelFit[logAveragedWithErrorsOnMean,{fitFunc,{a<0,c<0,0.5<\[Omega]<=2,1<df<1.1}},{a,c,\[Omega](*{a,-0.5},{c,-30.},{\[Omega],2.}*),df(*,{df,1.024}*)},x,MaxIterations->1000];*)
(*nlmAveragedWithErrorsOnMeanUnconstrained=NonlinearModelFit[logAveragedWithErrorsOnMean,fitFunc,{a,c,\[Omega](*{a,-0.5},{c,-30.},{\[Omega],2.}*),df(*,{df,1.024}*)},x,MaxIterations->1000];*)
(**)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@nlmAveragedWithErrorsOnMean*)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@nlmAveragedWithErrorsOnMeanUnconstrained*)
(**)
(**)
(*nlmAveragedWithErrorsOnMeanGlobal=NonlinearModelFit[logAveragedWithErrorsOnMean,{fitFunc,{(*-2<a<2,*)a<0,c<0,0.5<\[Omega]<=1.1,1<df<1.1}},{(*a,c,\[Omega],df*){a,-0.5},{c,-30.},{\[Omega],1.},{df,1.024}},x,MaxIterations->1000,Method->"NMinimize"];*)
(**)
(*nlmAveragedWithErrorsOnMeanUnconstrainedGlobal=NonlinearModelFit[logAveragedWithErrorsOnMean,fitFunc,{a,c,\[Omega](*,{\[Omega],2.}*),df(*,{df,1.024}*)},x,MaxIterations->1000,Method->"NMinimize"];*)
(**)
(*nlmAveragedWithErrorsOnMeanUnconstrainedGlobalPurged=NonlinearModelFit[logAveragedWithErrorsOnMeanPurged,{fitFunc,a<0,1<df<2},{a,c,\[Omega](*,{\[Omega],2.}*),df(*,{df,1.024}*)},x,MaxIterations->1000,Method->"NMinimize"];*)
(**)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@nlmAveragedWithErrorsOnMeanGlobal*)
(*Function[var,Print[Row[{SymbolName[Unevaluated[var]]," with ",fitFunc,": ",var//Normal}]],{HoldFirst}]@nlmAveragedWithErrorsOnMeanUnconstrainedGlobal*)
(**)


(* ::Input:: *)
(*maxx=Max[averaged[[All,1]]];*)
(*maxy=Max[averaged[[All,2]]];*)
(**)
(*thresholdBelow=0;*)
(*thresholdAbove=maxx-0;*)
(**)
(*Show[{ListPlot[logAveragedWithMaxDev,PlotStyle->{GrayLevel[0],Directive[Opacity[0.3]]},AxesLabel->{"Log[L]","Log[N]"}]*)
(*,ListPlot[logAveragedWithErrorsOnMean,PlotStyle->RGBColor[1, 0.78, 0.13],PlotLegends->{"logAveragedWithErrorsOnMean"}]*)
(*,ListPlot[logAveragedWithErrorsOnMeanPurged,PlotStyle->{RGBColor[0, 0.78, 1],PointSize->0.005},PlotLegends->PointLegend[{"logAveragedWithErrorsOnMeanPurged"},LegendMarkerSize->10,LegendMarkers->Graphics[Disk[]]]]*)
(*,*)
(*Plot[ReleaseHold[#][x],{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 0, 0.5],Thickness->0.002},PlotLegends->Placed[SwatchLegend[{Row[{#,":\n ",TraditionalForm[ReleaseHold[#][x]]}]}],Right]]&@HoldForm[nlmAveragedWithErrorsOnMean]*)
(*,*)
(*Plot[ReleaseHold[#][x],{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 0.68, 0.5],Thickness->0.002},PlotLegends->Placed[SwatchLegend[{Row[{#,":\n ",TraditionalForm[ReleaseHold[#][x]]}]}],Right]]&@HoldForm[nlmAveragedWithErrorsOnMeanUnconstrained]*)
(*,*)
(**)
(*Plot[ReleaseHold[#][x],{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.85, 0, 0.5],Thickness->0.002},PlotLegends->Placed[SwatchLegend[{Row[{#,":\n ",TraditionalForm[ReleaseHold[#][x]]}]}],Right]]&@HoldForm[nlmAveragedWithErrorsOnMeanUnconstrainedGlobal],*)
(**)
(**)
(*Plot[ReleaseHold[#][x],{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 0.68, 0.8],Thickness->0.002},PlotLegends->Placed[SwatchLegend[{Row[{#,":\n ",TraditionalForm[ReleaseHold[#][x]]}]}],Right]]&@HoldForm[nlmAveragedWithErrorsOnMeanUnconstrainedGlobalPurged]*)
(*,Plot[(x (1.624)-1.4),{x,Log[0+1],Log[maxx]},PlotStyle->{RGBColor[0, 0, 1],Dashed},PlotLegends->Placed[SwatchLegend[{"David Wilson: 1.624"}],Right]]*)
(*}*)
(*,PlotLabel->Row[{" b = ",b}]*)
(*,PlotRange->{All,{0,All}},AxesOrigin->{1,0},ImageSize->700]*)


(* ::Input:: *)
(*Around[nlmAveragedWithErrorsOnMeanUnconstrainedGlobal["ParameterTable"][[1,1,-1,2]],nlmAveragedWithErrorsOnMeanUnconstrainedGlobal["ParameterTable"][[1,1,-1,3]]*\[Pi]]*)
(**)
(*Around[nlmAveragedWithErrorsOnMeanUnconstrainedGlobalPurged["ParameterTable"][[1,1,-1,2]],nlmAveragedWithErrorsOnMeanUnconstrainedGlobalPurged["ParameterTable"][[1,1,-1,3]]*\[Pi]]*)


(* ::Input:: *)
(*ListPlot[Transpose[{logAveragedWithErrorsOnMean[[All,1]],nlmAveragedWithErrorsOnMeanUnconstrainedGlobal["FitResiduals"]}],Filling->Axis,*)
(*AxesLabel->{"x","Residuals (y - y_fit)"},PlotLabel->"FitResiduals - nlmAveragedWithErrorsOnMeanUnconstrainedGlobal",ImageSize->Large]*)
(**)
(**)
(*ListPlot[Transpose[{logAveragedWithErrorsOnMeanPurged[[All,1]],nlmAveragedWithErrorsOnMeanUnconstrainedGlobalPurged["FitResiduals"]}],Filling->Axis,*)
(*AxesLabel->{"x","Residuals (y - y_fit)"},PlotLabel->"FitResiduals - nlmAveragedWithErrorsOnMeanUnconstrainedGlobalPurged",ImageSize->Large]*)


(* ::Input:: *)
(*nlmAveragedWithErrorsOnMeanUnconstrainedGlobal["ANOVATable"][[1,1,3,3]]*)
(*nlmAveragedWithErrorsOnMeanUnconstrainedGlobalPurged["ANOVATable"][[1,1,3,3]]*)


(* ::Subsection:: *)
(*Drop both first and/or last few 		WORKS PRETTY WELL (LACKING STATISTICS AT BIG L)*)


(* ::Subsubsection:: *)
(*Linear*)


(* ::Input:: *)
(*maxx=Max[averaged[[All,1]]];*)
(*maxy=Max[averaged[[All,2]]];*)
(**)
(*thresholdBelow=Exp[3.1];*)
(*thresholdAbove=maxx-40;*)
(**)
(*(*Let's drop some*)*)
(*(*droppedWithMaxDev=Select[logAveragedWithMaxDev,Log[thresholdBelow]<#[[1]]<=Log[thresholdAbove]&];*)
(*droppedWithErrors=Select[logAveragedWithErrors,Log[thresholdBelow]<#[[1]]<=Log[thresholdAbove]&];*)
(**)*)
(*droppedWithErrorsOnMean=Select[logAveragedWithErrorsOnMean,Log[thresholdBelow]<#[[1]]<=Log[thresholdAbove]&];*)
(*droppedWithErrorsOnMeanPurged=Select[logAveragedWithErrorsOnMeanPurged,Log[thresholdBelow]<#[[1]]<=Log[thresholdAbove]&];*)
(**)
(*(*drop specific bad point*)*)
(**)
(*(*droppedWithErrors=DeleteCases[droppedWithErrors,_?(6.68<N[#[[1]]]<6.7&)];*)*)
(*(**)
(*droppedWithErrorsOnMean=DeleteCases[droppedWithErrorsOnMean,_?(6.68<N[#[[1]]]<6.7&)];*)
(*droppedWithErrorsOnMeanPurged=DeleteCases[droppedWithErrorsOnMeanPurged,_?(6.68<N[#[[1]]]<6.7&)];*)
(**)*)
(**)
(*lmdroppedWithMaxDev=LinearModelFit[droppedWithMaxDev,x,x,Weights->Automatic];*)
(*(*lmdroppedWithErrors=LinearModelFit[droppedWithErrors,x,x,Weights->Automatic];*)(*NonlinearModelFit[droppedWithErrors,{a+df x,{a<0,1<df<2}},{a,df},x,Weights->Automatic,Method->"NMinimize"]*)*)
(**)
(*lmdroppedWithErrorsOnMean=LinearModelFit[droppedWithErrorsOnMean,x,x,Weights->Automatic];*)
(*(*NonlinearModelFit[droppedWithErrorsOnMean,{a+df x,{a<0,1<df<2}},{a,df},x,Weights->Automatic,Method->"NMinimize"]*)*)
(*lmdroppedWithErrorsOnMeanPurged=LinearModelFit[droppedWithErrorsOnMeanPurged,x,x,Weights->Automatic];*)
(*(*lmdroppedWithEstimatedStdDevs=NonlinearModelFit[droppedWithEstimatedStdDevs,{a+df x,{a<0,1<df<2}},{a,df},x,Weights->Automatic,Method->"NMinimize"];*)
(*lmdroppedWithEstimatedStdDevsOnMean=NonlinearModelFit[droppedWithEstimatedStdDevsOnMean,{a+df x,{a<0,1<df<2}},{a,df},x,Weights->Automatic,Method->"NMinimize"];*)*)
(**)
(*Show[{ListPlot[logAveragedWithMaxDev,PlotStyle->{GrayLevel[0],Directive[Opacity[0.3]]},AxesLabel->{"Log[L]","Log[N]"}]*)
(*,ListPlot[droppedWithErrorsOnMeanPurged,PlotStyle->{RGBColor[1, 0.78, 0.13],PointSize->0.01},PlotLegends->PointLegend[{"droppedWithErrorsOnMeanPurged"},LegendMarkerSize->10,LegendMarkers->Graphics[Disk[]]]]*)
(*,ListPlot[droppedWithErrorsOnMean,PlotStyle->{RGBColor[0, 0.78, 1],PointSize->0.005},PlotLegends->PointLegend[{"droppedWithErrorsOnMean"},LegendMarkerSize->10,LegendMarkers->Graphics[Disk[]]]]*)
(*,(*Plot[ReleaseHold[#][x],{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{,Thickness->0.002},PlotLegends->Placed[SwatchLegend[{Row[{#,":\n ",TraditionalForm[ReleaseHold[#][x]]}]}],Right]]&@HoldForm[lmdroppedWithMaxDev]*)
(*,*)Plot[ReleaseHold[#][x],{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[1, 0.68, 0.6],Thickness->0.003},PlotLegends->Placed[SwatchLegend[{Row[{#,":\n ",TraditionalForm[ReleaseHold[#][x]]}]}],Right]]&@HoldForm[lmdroppedWithErrorsOnMeanPurged]*)
(*,Plot[ReleaseHold[#][x],{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 0.68, 0.5],Thickness->0.002},PlotLegends->Placed[SwatchLegend[{Row[{#,":\n ",TraditionalForm[ReleaseHold[#][x]]}]}],Right]]&@HoldForm[lmdroppedWithErrorsOnMean]*)
(*,Plot[(x (1.624)-1.4),{x,Log[0+1],Log[maxx]},PlotStyle->{RGBColor[0, 0, 1],Dashed},PlotLegends->Placed[SwatchLegend[{"David Wilson: 1.624"}],Right]]*)
(*(*,Plot[#,{x,0,Log[thresholdAbove]},PlotStyle->{,Dashed},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@(x (dfSLE/.bb->N[b])-0.7-5Exp[-0.991 x])*)*)
(*}*)
(*,Epilog->{Directive[Dashed,GrayLevel[0.5]],*)
(*Line[{{Log[thresholdBelow],0},{Log[thresholdBelow],Log[maxy]//N}}]*)
(*,Line[{{Log[thresholdAbove],0},{Log[thresholdAbove],Log[maxy]//N}}]}*)
(*,PlotLabel->Row[{" b = ",b}]*)
(*,PlotRange->{{2,Log[maxx]},{0,Log[maxy]}}(*PlotRange->{{Log[thresholdBelow],Log[maxy]},{4,7.2}}*),AxesOrigin->{(*Log[thresholdBelow]*)2,0},ImageSize->700]*)


(* ::Input:: *)
(*Around[lmdroppedWithErrorsOnMean["ParameterTable"][[1,1,3,2]],lmdroppedWithErrorsOnMean["ParameterTable"][[1,1,3,3]]*\[Pi]]*)
(*Around[lmdroppedWithErrorsOnMeanPurged["ParameterTable"][[1,1,3,2]],lmdroppedWithErrorsOnMeanPurged["ParameterTable"][[1,1,3,3]]*\[Pi]]*)


(* ::Input:: *)
(*ListPlot[Transpose[{droppedWithErrorsOnMean[[All,1]],lmdroppedWithErrorsOnMean["FitResiduals"]}],Filling->Axis,*)
(*AxesLabel->{"x","Residuals (y - y_fit)"},PlotLabel->"FitResiduals - lmdroppedWithErrorsOnMean",ImageSize->Large]*)
(**)
(*ListPlot[Transpose[{droppedWithErrorsOnMeanPurged[[All,1]],Symbol["lm"<>"droppedWithErrorsOnMeanPurged"]["FitResiduals"]}],Filling->Axis,*)
(*AxesLabel->{"x","Residuals (y - y_fit)"},PlotLabel->"FitResiduals - lmdroppedWithErrorsOnMeanPurged",ImageSize->Large](*&@droppedWithErrorsOnMeanPurged*)*)


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


(* ::Subsection:: *)
(*Take the Log WITH A SHIFT		THIS IS GOOD SHIFTING ??*)


(* ::Input:: *)
(*shift:=Plus[{-1,0},#]&; (*In the code, the stopping condition is with R-1*)*)
(**)
(*logAveragedShifted=Log[shift/@averaged];*)
(*logAveragedWithErrorsShifted=Log[shift/@averagedWithErrors]/. {a_,0}->{a,Around[1.0`*^-6,1.0`*^-6]};*)
(*logAveragedWithErrorsOnMeanShifted=Log[shift/@averagedWithErrorsOnMean]/. 0->Around[1.0`*^-6,1.0`*^-6];*)
(**)
(*(*logAveragedWithEstimatedStdDevsShifted=Log[shift/@averagedWithEstimatedStdDevs]/. {a_,0}->{a,Around[1.0`*^-6,1.0`*^-6]};*)*)
(**)
(*logAveragedWithMaxDevShifted=Log[shift/@averagedWithMaxDev]/.{a_,0}->{a,Around[1.0`*^-6,1.0`*^-6]};*)


(* ::Input:: *)
(*maxx=Max[averaged[[All,1]]];*)
(*maxy=Max[averaged[[All,2]]];*)
(* *)
(*Show[{ListPlot[logAveragedWithMaxDevShifted,PlotStyle->{GrayLevel[0],Directive[Opacity[0.3]],PointSize->0.001},AxesLabel->{"Log[L]","Log[N]"}]*)
(*,ListPlot[logAveragedWithErrorsShifted,PlotStyle->{RGBColor[0, 0.78, 1],PointSize->0.01},PlotLegends->PointLegend[{"logAveragedWithErrorsShifted"},LegendMarkerSize->10,LegendMarkers->Graphics[Disk[]]]]*)
(*,ListPlot[logAveragedWithErrorsOnMeanShifted,PlotStyle->{RGBColor[1, 0.55, 1],Directive[Opacity[0.6]],PointSize->0.005},PlotLegends->PointLegend[{"logAveragedWithErrorsOnMeanShifted"},LegendMarkerSize->10,LegendMarkers->Graphics[Disk[]]]]*)
(*,Plot[(x (1.624)-1.4),{x,Log[0+1],Log[maxx]},PlotStyle->{RGBColor[0, 0, 1],Dashed},PlotLegends->Placed[SwatchLegend[{"David Wilson: 1.624"}],Right]]*)
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


(* ::Subsubsection:: *)
(*Linear Fit*)


(* ::Input:: *)
(*fitFunc=a+df x;*)
(*lmAveragedShifted=NonlinearModelFit[logAveragedShifted,{fitFunc(*,{-2<a<2,0<\[Omega]<28}*)},{a,(*c,\[Omega],*)df},x];*)
(*lmAveragedWithErrorsShifted=NonlinearModelFit[logAveragedWithErrorsShifted,{a+df x,{a<0,1<df<2}},{a,df},x,Weights->Automatic,Method->"NMinimize"];*)
(*lmAveragedWithErrorsOnMeanShifted=NonlinearModelFit[logAveragedWithErrorsOnMeanShifted,{a+df x,{a<0,1<df<2}},{a,df},x,Weights->Automatic,Method->"NMinimize"];*)
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
(*maxx=Max[averaged[[All,1]]];*)
(*maxy=Max[averaged[[All,2]]];*)
(**)
(*thresholdBelow=0;*)
(*thresholdAbove=maxx-0;*)
(**)
(*Show[{ListPlot[ReleaseHold[#],PlotStyle->{GrayLevel[0],Directive[Opacity[0.3]],PointSize->0.001},AxesLabel->{"Log[L]","Log[N]"}]&@HoldForm[logAveragedWithMaxDevShifted]*)
(*,ListPlot[ReleaseHold[#],PlotStyle->{RGBColor[0, 0.78, 1],PointSize->0.01},PlotLegends->PointLegend[{#},LegendMarkerSize->10,LegendMarkers->Graphics[Disk[]]]]&@HoldForm[logAveragedWithErrorsShifted]*)
(*,ListPlot[ReleaseHold[#],PlotStyle->{RGBColor[1, 0.55, 1],Directive[Opacity[0.6]],PointSize->0.005},PlotLegends->PointLegend[{"logAveragedWithErrorsOnMeanShifted"},LegendMarkerSize->10,LegendMarkers->Graphics[Disk[]]]]&@HoldForm[logAveragedWithErrorsOnMeanShifted]*)
(**)
(*,Plot[ReleaseHold[#][x],{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 0, 0.5],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{Row[{#,":\n ",TraditionalForm[ReleaseHold[#][x]]}]}],Right]]&@HoldForm[lmAveragedWithMaxDevShifted]*)
(*,Plot[ReleaseHold[#][x],{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[1, 0.68, 0.6],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{Row[{#,":\n ",TraditionalForm[ReleaseHold[#][x]]}]}],Right]]&@HoldForm[lmAveragedWithErrorsShifted]*)
(*,Plot[ReleaseHold[#][x],{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 0.68, 0.5],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{Row[{#,":\n ",TraditionalForm[ReleaseHold[#][x]]}]}],Right]]&@HoldForm[lmAveragedWithErrorsOnMeanShifted]*)
(**)
(*,*)
(*Plot[#,{x,Log[0+1],Log[maxx]},PlotStyle->{RGBColor[0, 0, 1],Dashed},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@(x (dfSLE/. bb->N[b])-0.9)*)
(*}*)
(*,(*PlotRange->All,*)PlotLabel->Row[{" b = ",b}]*)
(*(*,Epilog->{Directive[Dashed,],*)
(*Line[{{Log[thresholdBelow],0},{Log[thresholdBelow],Log[maxy]//N}}]*)
(*,Line[{{Log[thresholdAbove],0},{Log[thresholdAbove],Log[maxy]//N}}]}*)*)
(*,PlotRange->{{1,All},{0,All}},AxesOrigin->{1,0},ImageSize->700]*)


(* ::Subsubsection:: *)
(*Drop some*)


(* ::Input:: *)
(*maxx=Max[averaged[[All,1]]];*)
(*maxy=Max[averaged[[All,2]]];*)
(**)
(*thresholdBelow=10;*)
(*thresholdAbove=maxx-300;*)
(**)
(*(*Let's drop some*)*)
(*droppedWithMaxDevShifted=Select[logAveragedWithMaxDevShifted,Log[thresholdBelow]<#[[1]]<Log[thresholdAbove]&];*)
(*droppedWithErrorsShifted=Select[logAveragedWithErrorsShifted,Log[thresholdBelow]<#[[1]]<Log[thresholdAbove]&];*)
(*droppedWithErrorsOnMeanShifted=Select[logAveragedWithErrorsOnMeanShifted,Log[thresholdBelow]<#[[1]]<Log[thresholdAbove]&];*)
(**)
(*(*drop specific bad point*)*)
(*Length[droppedWithMaxDevShifted];*)
(*droppedWithMaxDevShifted=DeleteCases[droppedWithMaxDevShifted,_?(6.6<N[#[[1]]]<6.75&)];*)
(*Length[droppedWithMaxDevShifted];*)
(*droppedWithErrorsShifted=DeleteCases[droppedWithErrorsShifted,_?(6.6<N[#[[1]]]<6.75&)];*)
(*droppedWithErrorsOnMeanShifted=DeleteCases[droppedWithErrorsOnMeanShifted,_?(6.6<N[#[[1]]]<6.75&)];*)
(**)
(*(*Fit*)*)
(*lmdroppedWithMaxDevShifted=NonlinearModelFit[droppedWithMaxDevShifted,{a+df x,{a<0,1<df<2}},{a,df},x,Weights->Automatic,Method->"NMinimize"]*)
(*lmdroppedWithErrorsShifted=NonlinearModelFit[droppedWithErrorsShifted,{a+df x,{a<0,1<df<2}},{a,df},x,Weights->Automatic,Method->"NMinimize"]*)
(*lmdroppedWithErrorsOnMeanShifted=NonlinearModelFit[droppedWithErrorsOnMeanShifted,{a+df x,{a<0,1<df<2}},{a,df},x,Weights->Automatic,Method->"NMinimize"]*)
(**)
(**)
(*(*Plot*)*)
(*Show[{ListPlot[ReleaseHold[#],PlotStyle->{GrayLevel[0],Directive[Opacity[0.3]],PointSize->0.01},AxesLabel->{"Log[L]","Log[N]"}]&@HoldForm[logAveragedWithMaxDevShifted]*)
(*,ListPlot[ReleaseHold[#],PlotStyle->{RGBColor[0, 0.78, 1],PointSize->0.01},PlotLegends->PointLegend[{#},LegendMarkerSize->10,LegendMarkers->Graphics[Disk[]]]]&@HoldForm[droppedWithErrorsShifted]*)
(*,ListPlot[ReleaseHold[#],PlotStyle->{RGBColor[1, 0.55, 1],Directive[Opacity[0.6]],PointSize->0.005},PlotLegends->PointLegend[{#},LegendMarkerSize->10,LegendMarkers->Graphics[Disk[]]]]&@HoldForm[droppedWithErrorsOnMeanShifted]*)
(**)
(*,Plot[ReleaseHold[#][x],{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 0, 0.5],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{Row[{#,":\n ",TraditionalForm[ReleaseHold[#][x]]}]}],Right]]&@HoldForm[lmdroppedWithErrorsShifted]*)
(*,Plot[ReleaseHold[#][x],{x,Log[thresholdBelow+1],Log[thresholdAbove]},PlotStyle->{RGBColor[0.5, 0.68, 0.5],Thickness->0.004},PlotLegends->Placed[SwatchLegend[{Row[{#,":\n ",TraditionalForm[ReleaseHold[#][x]]}]}],Right]]&@HoldForm[lmdroppedWithErrorsOnMeanShifted]*)
(**)
(*,*)
(*Plot[#,{x,Log[0+1],Log[maxx]},PlotStyle->{RGBColor[0, 0, 1],Dashed},PlotLegends->Placed[SwatchLegend[{TraditionalForm[#]}],Right]]&@(x (dfSLE/. bb->N[b])-0.9)*)
(*}*)
(*,Epilog->{Directive[Dashed,GrayLevel[0.5]],*)
(*Line[{{Log[thresholdBelow+1],0},{Log[thresholdBelow+1],Log[maxy]//N}}]*)
(*,Line[{{Log[thresholdAbove],0},{Log[thresholdAbove],Log[maxy]//N}}]}*)
(*,PlotLabel->Row[{" b = ",b}]*)
(*,PlotRange->{All,{0,Log[maxy]}}(*PlotRange->{{Log[thresholdBelow],Log[maxy]},{4,7.2}}*),AxesOrigin->{(*Log[thresholdBelow]*)1,0},ImageSize->700]*)


(* ::Input:: *)
(*synchronizedPlots=Map[Show[#,ImageSize->500,PlotRange->All]&,{ListPlot[lmdroppedWithMaxDevShifted["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis, AdjustedRSquared=",lmdroppedWithMaxDevShifted["AdjustedRSquared"]}]]*)
(*,ListPlot[ lmdroppedWithErrorsShifted["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis, AdjustedRSquared=",lmdroppedWithErrorsShifted["AdjustedRSquared"]}]]*)
(*,ListPlot[lmdroppedWithErrorsOnMeanShifted["FitResiduals"],Filling->Axis,PlotLabel->Row[{"Residual Analysis, AdjustedRSquared=",lmdroppedWithErrorsOnMeanShifted["AdjustedRSquared"]}]]}];*)
(**)
(*Multicolumn[synchronizedPlots,2,Appearance->"Framed"]*)
(**)


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


(* ::Subsection:: *)
(*Plot with different drops below: \[Chi]^2 as a function of drop*)


(* ::Input:: *)
(*ListPlot[Transpose[{droppedWithErrorsOnMean[[All,1]],lmdroppedWithErrorsOnMean["FitResiduals"]}],Filling->Axis,AxesLabel->{"x","Residuals (y - y_fit)"},ImageSize->Medium]*)
(**)


(* ::Input:: *)
(*lmdroppedWithErrorsOnMean=LinearModelFit[droppedWithErrorsOnMean,x,x,Weights->Automatic,VarianceEstimatorFunction->(1&)];*)
(**)
(*(*True Chi^2*)*)
(*chi2=Total[(lmdroppedWithErrorsOnMean["FitResiduals"]/droppedWithErrorsOnMean[[All,2,2]])^2]*)
(*(*Or via the fit object*)*)
(*chi2=lmdroppedWithErrorsOnMean["ANOVATable"][[1]]*)
(**)
(**)
(*lmdroppedWithErrorsOnMean["ParameterTable"][[1]]*)
(**)
(*Around[lmdroppedWithErrorsOnMean["ParameterTable"][[1,1,3,2]],lmdroppedWithErrorsOnMean["ParameterTable"][[1,1,3,3]]*\[Pi]]*)
(**)
(**)
(*(*WITHOUT	.*)*)
(*lmdroppedWithErrorsOnMean=LinearModelFit[droppedWithErrorsOnMean,x,x,Weights->Automatic(*,VarianceEstimatorFunction->(1&)*)];*)
(**)
(*(*True Chi^2*)*)
(*chi2=Total[(lmdroppedWithErrorsOnMean["FitResiduals"]/droppedWithErrorsOnMean[[All,2,2]])^2]*)
(*(*Or via the fit object*)*)
(*chi2=lmdroppedWithErrorsOnMean["ANOVATable"][[1,1,3,3]]*)
(*0*)
(**)
(*lmdroppedWithErrorsOnMean["ParameterTable"][[1]]*)
(**)
(*Around[lmdroppedWithErrorsOnMean["ParameterTable"][[1,1,3,2]],lmdroppedWithErrorsOnMean["ParameterTable"][[1,1,3,3]]*\[Pi]]*)


(* ::Subsubsection::Closed:: *)
(*Run the sampling*)


(* ::Input:: *)
(*logAveragedWithErrorsOnMean;*)
(*%[[1]]*)
(*Sort[%%];*)
(*%[[1]]*)


(* ::Input:: *)
(**)
(*thresholdAbove=maxx-00;*)
(*\[Chi]2Dropped=ParallelTable[With[{droppedWithErrorsOnMean=Drop[Sort@logAveragedWithErrorsOnMean,i]},*)
(*lmdropped=LinearModelFit[droppedWithErrorsOnMean,x,x,Weights->Automatic,VarianceEstimatorFunction->(1&)];*)
(*{i,lmdropped["ANOVATable"][[1,1,3,3]],ListPlot[Transpose[{droppedWithErrorsOnMean[[All,1]],lmdropped["FitResiduals"]}],Filling->Axis,AxesLabel->{"x","Residuals (y - y_fit)"},ImageSize->Medium]}],*)
(*{i,0,Length[logAveragedWithErrorsOnMean]-3}];*)


(* ::Input:: *)
(*\[Chi]2Dropped[[1;;2]]*)


(* ::Input:: *)
(*Show[*)
(*{ListPlot[\[Chi]2Dropped[[All,1;;2]],PlotRange->{All,All}]*)
(*}*)
(*,AxesLabel->{"Dropped points",\[Chi]^2},PlotRange->All(*{{0,All},{0,1*10^3}}*),ImageSize->Large]*)


(* ::Input:: *)
(*Multicolumn[\[Chi]2Dropped[[All,-1]],3]*)


(* ::Subsubsection:: *)
(*Gemini's help*)


(* ::Item:: *)
(*On All data*)


(* ::Input:: *)
(*(*Sort data ascending by x*)*)
(*dataSorted=SortBy[logAveragedWithErrorsOnMean,First];*)
(**)
(*(*Scan across cutoff values xMin*)*)
(*scanResults=Table[With[{subData=Select[dataSorted,First[#]>=xMin&]},If[Length[subData]>=4,*)
(*Module[*)
(*{fit=LinearModelFit[subData,x,x,Weights->Automatic,VarianceEstimatorFunction->(1&)]},*)
(**)
(*{xMin,fit["ParameterTable"][[1,1,3,2]](*Slope*),*)
(*fit["ParameterTable"][[1,1,3,3]](*Slope Error*),*)
(*fit["ANOVATableEntries"][[2,2]]/fit["ANOVATableEntries"][[2,1]](*Reduced Chi^2*)}],Nothing]],{xMin,dataSorted[[1;;-6,1]]}];*)


(* ::Input:: *)
(*(*1. Plot Reduced Chi^2 vs xMin*)*)
(*ListLinePlot[scanResults[[All,{1,4}]],AxesLabel->{"x_min","Reduced Chi^2"},PlotRange->{0,3},GridLines->{None,{1}}]*)
(**)
(*(*2. Plot Fitted Slope vs xMin with error bands*)*)
(*ListPlot[Table[{r[[1]],Around[r[[2]],r[[3]]]},{r,scanResults}],AxesLabel->{"x_min","Slope"},PlotRange->{1.35,1.55}, GridLines->{None,{dfSLE/.bb->N[b]}}]*)


(* ::Item::Closed:: *)
(*On Purged data*)


(* ::Input:: *)
(*lmdroppedWithErrorsOnMean["ParameterTable"][[1,1,3,3]]*)


(* ::Input:: *)
(*(*Sort data ascending by x*)*)
(*dataSorted=SortBy[logAveragedWithErrorsOnMeanPurged,First];*)
(**)
(*(*Scan across cutoff values xMin*)*)
(*scanResults=Table[With[{subData=Select[dataSorted,First[#]>=xMin&]},If[Length[subData]>=4,*)
(*Module[*)
(*{fit=LinearModelFit[subData,x,x,Weights->Automatic,VarianceEstimatorFunction->(1&)]},*)
(**)
(*{xMin,fit["ParameterTable"][[1,1,3,2]](*Slope*),*)
(*fit["ParameterTable"][[1,1,3,3]](*Slope Error*),*)
(*fit["ANOVATableEntries"][[2,2]]/fit["ANOVATableEntries"][[2,1]](*Reduced Chi^2*)}],Nothing]],{xMin,dataSorted[[1;;-6,1]]}];*)


(* ::Input:: *)
(*(*1. Plot Reduced Chi^2 vs xMin*)*)
(*ListLinePlot[scanResults[[All,{1,4}]],AxesLabel->{"x_min","Reduced Chi^2"},PlotRange->{0,20},GridLines->{None,{1}}]*)
(**)
(*(*2. Plot Fitted Slope vs xMin with error bands*)*)
(*ListPlot[Table[{r[[1]],Around[r[[2]],r[[3]]]},{r,scanResults}],AxesLabel->{"x_min","Slope"},PlotRange->{1.35,1.55}, GridLines->{None,{dfSLE/.bb->N[b]}}]*)


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



