(* ::Package:: *)

(* ::Input:: *)
(*SetOptions[$FrontEnd,WindowTitle->"FullFileName"]*)


(* ::Title:: *)
(*Data analysis for the Fractal Dimension of the b-LRW on 2d square Lattice*)


(* ::Input:: *)
(*Quit[]*)


(* ::Title:: *)
(*Exact  from  SLE  (Lawler)*)


(* ::Input::Initialization:: *)
dfSLE=1+3/(4(2bb+1));
endRange=5;

plotSLE=Plot[dfSLE,{bb,0,endRange},PlotStyle->Red,PlotRange->All];

Show[{plotSLE},PlotRange->{1,2},AxesLabel->{b,Subscript[d, f]},PlotLabel->"Lawer's result (\!\(\*SubscriptBox[\(SLE\), \(\[Kappa]\)]\))",AxesOrigin->{0,1}]



(* ::Title::Closed:: *)
(*Before optimization*)


(* ::Chapter:: *)
(*Import data: WARNING: NOW NEEDS OTHER PATH TO IMPORT:"D:\Offline_Documents\University\PhD_Paris\PhD_work\Simulations\bLRW\b-LRWdata\2d-FromCluster\Old-BeforeOptimization"*)


(* ::Section:: *)
(*b=1 With all the points from the CLUSTER*)


(* ::Subsection:: *)
(*Raw data*)


(* ::Input:: *)
(*0*)


(* ::Input:: *)
(*data1=DeleteCases[Drop[ToExpression/@Import[FileNameJoin[{"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d-FromCluster\\b=1.000000-LRW-2d-square-lattice-data-3-201.csv"}],"CSV"],1],{}];*)


(* ::Input:: *)
(*data2=Drop[ToExpression/@Import[FileNameJoin[{"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d-FromCluster\\b=1.000000-LRW-2d-square-lattice-data-203-301.csv"}],"CSV"],1];*)


(* ::Input:: *)
(*data3=Drop[ToExpression/@Import[FileNameJoin[{"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d-FromCluster\\b=1.000000-LRW-2d-square-lattice-data-303-401.csv"}],"CSV"],1];*)


(* ::Input:: *)
(*data4=Drop[ToExpression/@Import[FileNameJoin[{"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d-FromCluster\\b=1.000000-LRW-2d-square-lattice-data-403-501.csv"}],"CSV"],1];*)


(* ::Input:: *)
(*rawData=joinedDatab1=Join[data1,data2,data3,data4(*,data5*)];*)


(* ::Input:: *)
(*ListPlot[rawData]*)


(* ::Input:: *)
(*ListLogLogPlot[rawData]*)


(* ::Input:: *)
(*Clear[x]*)


(* ::Input:: *)
(*FindFit[Log[rawData],a+df *x,{a,df},x]*)


(* ::Input:: *)
(*lm=LinearModelFit[Log[rawData],x,x]*)


(* ::Input:: *)
(*lm["ParameterErrors"]*)


(* ::Input:: *)
(*Show[ListLogLogPlot[rawData],*)
(*Plot[lm[x],{x,0,1000},PlotStyle->Red]*)
(*]*)
(*Print[ "\!\(\*SubscriptBox[\(d\), \(f\)]\)=",df/.FindFit[Log[rawData],a+df *x,{a,df},x],"\[PlusMinus]",lm["ParameterErrors"][[2]],"*)
(*Result from the litterature (exact with SLE): ",(dfSLE/.b->1.)]*)


(* ::Subsection::Closed:: *)
(*Drop the first few*)


(* ::Input:: *)
(*droppedb1=DeleteCases[joinedDatab1,{a_,_}/;a<10];*)
(*FindFit[Log[dropped],a+df *x,{a,df},x]*)
(*lm=LinearModelFit[Log[dropped],x,x]*)


(* ::Input:: *)
(*droppedb1=DeleteCases[joinedDatab1,{a_,_}/;a<10];*)
(*lm=LinearModelFit[Log[dropped],x,x];*)
(*Show[ListLogLogPlot[joinedData],ListLogLogPlot[dropped,PlotStyle->Black],*)
(*Plot[lm[x],{x,0,1000},PlotStyle->Red]*)
(*]*)
(*Print[ "\!\(\*SubscriptBox[\(d\), \(f\)]\)=",df/.FindFit[Log[dropped],a+df *x,{a,df},x],"\[PlusMinus]",lm["ParameterErrors"][[2]],"*)
(*Result from the litterature (exact with SLE): ",(dfSLE/.b->1.)]*)


(* ::Input:: *)
(*(*Good enough*)*)


(* ::Section::Closed:: *)
(*b=2 With all the points from the CLUSTER*)


(* ::Subsection:: *)
(*Raw data*)


(* ::Input:: *)
(*0*)


(* ::Input:: *)
(*Clear[b]*)


(* ::Input:: *)
(*data1=DeleteCases[Drop[ToExpression/@Import[FileNameJoin[{"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d-FromCluster\\b=2.000000-LRW-2d-square-lattice-data-3-201.csv"}],"CSV"],1],{}];*)


(* ::Input:: *)
(*data2=Drop[ToExpression/@Import[FileNameJoin[{"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d-FromCluster\\b=2.000000-LRW-2d-square-lattice-data-203-301.csv"}],"CSV"],1];*)


(* ::Input:: *)
(*data3=Drop[ToExpression/@Import[FileNameJoin[{"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d-FromCluster\\b=2.000000-LRW-2d-square-lattice-data-303-401.csv"}],"CSV"],1];*)


(* ::Input:: *)
(*data4=Drop[ToExpression/@Import[FileNameJoin[{"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d-FromCluster\\b=2.000000-LRW-2d-square-lattice-data-403-501.csv"}],"CSV"],1];*)


(* ::Input:: *)
(*rawData=joinedDatab2=Join[data1,data2,data3,data4(*,data5*)];*)


(* ::Input:: *)
(*ListPlot[rawData]*)


(* ::Input:: *)
(*ListLogLogPlot[rawData]*)


(* ::Input:: *)
(*Clear[x]*)
(*Clear[b]*)


(* ::Input:: *)
(*FindFit[Log[rawData],a+df *x,{a,df},x]*)


(* ::Input:: *)
(*lm=LinearModelFit[Log[rawData],x,x]*)


(* ::Input:: *)
(*lm["ParameterErrors"]*)


(* ::Input:: *)
(*Show[ListLogLogPlot[rawData],*)
(*Plot[lm[x],{x,0,1000},PlotStyle->Red]*)
(*]*)
(*Print[ "\!\(\*SubscriptBox[\(d\), \(f\)]\)=",df/.FindFit[Log[rawData],a+df *x,{a,df},x],"\[PlusMinus]",lm["ParameterErrors"][[2]],"*)
(*Result from the litterature (exact with SLE): ",(dfSLE/.b->2.)]*)


(* ::Subsection:: *)
(*Drop the first few*)


(* ::Input:: *)
(*joinedData=joinedDatab2;*)


(* ::Input:: *)
(*dropped=droppedb2=DeleteCases[joinedData,{a_,_}/;a<20];*)
(*FindFit[Log[dropped],a+df *x,{a,df},x]*)
(*lm=LinearModelFit[Log[dropped],x,x]*)


(* ::Input:: *)
(*dropped=droppedb2=DeleteCases[joinedData,{a_,_}/;a<10];*)
(*lm=LinearModelFit[Log[dropped],x,x];*)
(**)
(*Show[ListLogLogPlot[joinedData],ListLogLogPlot[dropped,PlotStyle->Black],*)
(*Plot[lm[x],{x,0,1000},PlotStyle->Red]*)
(*]*)
(*Print[ "\!\(\*SubscriptBox[\(d\), \(f\)]\)=",df/.FindFit[Log[dropped],a+df *x,{a,df},x],"\[PlusMinus]",lm["ParameterErrors"][[2]],"*)
(*Result from the litterature (exact with SLE): ",(dfSLE/.b->2.)]*)


(* ::Input:: *)
(*(*Good enough?*)*)


(* ::Section::Closed:: *)
(*b=3 With all the points from the CLUSTER*)


(* ::Subsection:: *)
(*Raw data*)


(* ::Input:: *)
(*0*)


(* ::Input:: *)
(*Clear[b]*)


(* ::Input:: *)
(*data1=DeleteCases[Drop[ToExpression/@Import[FileNameJoin[{"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d-FromCluster\\b=3.000000-LRW-2d-square-lattice-data-3-201.csv"}],"CSV"],1],{}];*)


(* ::Input:: *)
(*data2=Drop[ToExpression/@Import[FileNameJoin[{"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d-FromCluster\\b=3.000000-LRW-2d-square-lattice-data-203-301.csv"}],"CSV"],1];*)


(* ::Input:: *)
(*data3=Drop[ToExpression/@Import[FileNameJoin[{"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d-FromCluster\\b=3.000000-LRW-2d-square-lattice-data-303-401.csv"}],"CSV"],1];*)


(* ::Input:: *)
(*data4=Drop[ToExpression/@Import[FileNameJoin[{"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d-FromCluster\\b=3.000000-LRW-2d-square-lattice-data-403-501.csv"}],"CSV"],1];*)


(* ::Input:: *)
(*rawData=joinedDatab3=Join[data1,data2,data3,data4(*,data5*)];*)


(* ::Input:: *)
(*ListPlot[rawData]*)


(* ::Input:: *)
(*ListLogLogPlot[rawData]*)


(* ::Input:: *)
(*Clear[x]*)
(*Clear[b]*)


(* ::Input:: *)
(*FindFit[Log[rawData],a+df *x,{a,df},x]*)


(* ::Input:: *)
(*lm=LinearModelFit[Log[rawData],x,x]*)


(* ::Input:: *)
(*lm["ParameterErrors"]*)


(* ::Input:: *)
(*Show[ListLogLogPlot[rawData],*)
(*Plot[lm[x],{x,0,1000},PlotStyle->Red]*)
(*]*)
(*Print[ "\!\(\*SubscriptBox[\(d\), \(f\)]\)=",df/.FindFit[Log[rawData],a+df *x,{a,df},x],"\[PlusMinus]",lm["ParameterErrors"][[2]],"*)
(*Result from the litterature (exact with SLE): ",(dfSLE/.b->3.)]*)


(* ::Subsection:: *)
(*Drop the first few*)


(* ::Input:: *)
(*joinedData=joinedDatab3;*)


(* ::Input:: *)
(*dropped=droppedb3=DeleteCases[joinedData,{a_,_}/;a<20];*)
(*FindFit[Log[dropped],a+df *x,{a,df},x]*)
(*lm=LinearModelFit[Log[dropped],x,x]*)


(* ::Input:: *)
(*dropped=droppedb3=DeleteCases[joinedData,{a_,_}/;a<30];*)
(*lm=LinearModelFit[Log[dropped],x,x];*)
(**)
(*Show[ListLogLogPlot[joinedData],ListLogLogPlot[dropped,PlotStyle->Black],*)
(*Plot[lm[x],{x,0,1000},PlotStyle->Red]*)
(*]*)
(*Print[ "\!\(\*SubscriptBox[\(d\), \(f\)]\)=",df/.FindFit[Log[dropped],a+df *x,{a,df},x],"\[PlusMinus]",lm["ParameterErrors"][[2]],"*)
(*Result from the litterature (exact with SLE): ",(dfSLE/.b->3.)]*)


(* ::Input:: *)
(*(*Deviates a lot*)*)


(* ::Section:: *)
(*b=4 With all the points from the CLUSTER*)


(* ::Subsection:: *)
(*Raw data*)


(* ::Input:: *)
(*0*)


(* ::Input:: *)
(*data1=DeleteCases[Drop[ToExpression/@Import[FileNameJoin[{"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d-FromCluster\\b=4.000000-LRW-2d-square-lattice-data-3-201.csv"}],"CSV"],1],{}];*)


(* ::Input:: *)
(*data2=Drop[ToExpression/@Import[FileNameJoin[{"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d-FromCluster\\b="<>ToString[b]<>".000000-LRW-2d-square-lattice-data-203-301.csv"}],"CSV"],1];*)


(* ::Input:: *)
(*data3=Drop[ToExpression/@Import[FileNameJoin[{"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d-FromCluster\\b="<>ToString[b]<>".000000-LRW-2d-square-lattice-data-303-401.csv"}],"CSV"],1];*)


(* ::Input:: *)
(*data4=Drop[ToExpression/@Import[FileNameJoin[{"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d-FromCluster\\b="<>ToString[b]<>".000000-LRW-2d-square-lattice-data-403-501.csv"}],"CSV"],1];*)


(* ::Input:: *)
(*rawData=Join[data1,data2,data3,data4(*,data5*)];*)


(* ::Input:: *)
(*ListPlot[rawData]*)


(* ::Input:: *)
(*ListLogLogPlot[rawData]*)


(* ::Input:: *)
(*Clear[x]*)


(* ::Input:: *)
(*FindFit[Log[rawData],a+df *x,{a,df},x]*)


(* ::Input:: *)
(*lm=LinearModelFit[Log[rawData],x,x]*)


(* ::Input:: *)
(*lm["ParameterErrors"]*)


(* ::Input:: *)
(*Show[ListLogLogPlot[rawData],*)
(*Plot[lm[x],{x,0,1000},PlotStyle->Red]*)
(*]*)
(*Print[ "\!\(\*SubscriptBox[\(d\), \(f\)]\)=",df/.FindFit[Log[rawData],a+df *x,{a,df},x],"\[PlusMinus]",lm["ParameterErrors"][[2]],"*)
(*Result from the litterature (exact with SLE): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Subsection:: *)
(*Drop the first few*)


(* ::Input:: *)
(*joinedData=joinedDatab4=rawData;*)


(* ::Input:: *)
(*dropped=droppedb4=DeleteCases[joinedData,{a_,_}/;a<15];*)
(*lm=LinearModelFit[Log[dropped],x,x];*)
(**)
(*Show[ListLogLogPlot[joinedData],ListLogLogPlot[dropped,PlotStyle->Black],*)
(*Plot[lm[x],{x,0,1000},PlotStyle->Red]*)
(*]*)
(*Print[ "\!\(\*SubscriptBox[\(d\), \(f\)]\)=",df/.FindFit[Log[dropped],a+df *x,{a,df},x],"\[PlusMinus]",lm["ParameterErrors"][[2]],"*)
(*Result from the litterature (exact with SLE): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Input:: *)
(*(* Not that bad*)*)


(* ::Section::Closed:: *)
(*b=5 With all the points from the CLUSTER*)


(* ::Subsection:: *)
(*Raw data*)


(* ::Input:: *)
(*0*)


(* ::Input:: *)
(*data1=DeleteCases[Drop[ToExpression/@Import[FileNameJoin[{"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d-FromCluster\\b=5.000000-LRW-2d-square-lattice-data-3-201.csv"}],"CSV"],1],{}];*)


(* ::Input:: *)
(*data2=Drop[ToExpression/@Import[FileNameJoin[{"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d-FromCluster\\b="<>ToString[b]<>".000000-LRW-2d-square-lattice-data-203-301.csv"}],"CSV"],1];*)


(* ::Input:: *)
(*data3=Drop[ToExpression/@Import[FileNameJoin[{"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d-FromCluster\\b="<>ToString[b]<>".000000-LRW-2d-square-lattice-data-303-401.csv"}],"CSV"],1];*)


(* ::Input:: *)
(*data4=Drop[ToExpression/@Import[FileNameJoin[{"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d-FromCluster\\b="<>ToString[b]<>".000000-LRW-2d-square-lattice-data-403-501.csv"}],"CSV"],1];*)


(* ::Input:: *)
(*rawData=Join[data1,data2,data3,data4(*,data5*)];*)


(* ::Input:: *)
(*ListPlot[rawData]*)


(* ::Input:: *)
(*ListLogLogPlot[rawData]*)


(* ::Input:: *)
(*Clear[x]*)


(* ::Input:: *)
(*FindFit[Log[rawData],a+df *x,{a,df},x]*)


(* ::Input:: *)
(*lm=LinearModelFit[Log[rawData],x,x]*)


(* ::Input:: *)
(*lm["ParameterErrors"]*)


(* ::Input:: *)
(*Show[ListLogLogPlot[rawData],*)
(*Plot[lm[x],{x,0,1000},PlotStyle->Red]*)
(*]*)
(*Print[ "\!\(\*SubscriptBox[\(d\), \(f\)]\)=",df/.FindFit[Log[rawData],a+df *x,{a,df},x],"\[PlusMinus]",lm["ParameterErrors"][[2]],"*)
(*Result from the litterature (exact with SLE): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Subsection:: *)
(*Drop the first few*)


(* ::Input:: *)
(*joinedData=joinedDatab5=rawData;*)


(* ::Input:: *)
(*dropped=droppedb5=DeleteCases[joinedData,{a_,_}/;a<15];*)
(*lm=LinearModelFit[Log[dropped],x,x];*)
(**)
(*Show[ListLogLogPlot[joinedData],ListLogLogPlot[dropped,PlotStyle->Black],*)
(*Plot[lm[x],{x,0,1000},PlotStyle->Red]*)
(*]*)
(*Print[ "\!\(\*SubscriptBox[\(d\), \(f\)]\)=",df/.FindFit[Log[dropped],a+df *x,{a,df},x],"\[PlusMinus]",lm["ParameterErrors"][[2]],"*)
(*Result from the litterature (exact with SLE): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Input:: *)
(*(* Not that bad*)*)


(* ::Section::Closed:: *)
(*b=6 With all the points from the CLUSTER*)


(* ::Subsection:: *)
(*Raw data*)


(* ::Input:: *)
(*0*)


(* ::Input:: *)
(*data1=DeleteCases[Drop[ToExpression/@Import[FileNameJoin[{"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d-FromCluster\\b=6.000000-LRW-2d-square-lattice-data-3-201.csv"}],"CSV"],1],{}];*)


(* ::Input:: *)
(*data2=Drop[ToExpression/@Import[FileNameJoin[{"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d-FromCluster\\b="<>ToString[b]<>".000000-LRW-2d-square-lattice-data-203-301.csv"}],"CSV"],1];*)


(* ::Input:: *)
(*data3=Drop[ToExpression/@Import[FileNameJoin[{"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d-FromCluster\\b="<>ToString[b]<>".000000-LRW-2d-square-lattice-data-303-401.csv"}],"CSV"],1];*)


(* ::Input:: *)
(*data4=Drop[ToExpression/@Import[FileNameJoin[{"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d-FromCluster\\b="<>ToString[b]<>".000000-LRW-2d-square-lattice-data-403-501.csv"}],"CSV"],1];*)


(* ::Input:: *)
(*rawData=Join[data1,data2,data3,data4(*,data5*)];*)


(* ::Input:: *)
(*ListPlot[rawData]*)


(* ::Input:: *)
(*ListLogLogPlot[rawData]*)


(* ::Input:: *)
(*Clear[x]*)


(* ::Input:: *)
(*FindFit[Log[rawData],a+df *x,{a,df},x]*)


(* ::Input:: *)
(*lm=LinearModelFit[Log[rawData],x,x]*)


(* ::Input:: *)
(*lm["ParameterErrors"]*)


(* ::Input:: *)
(*Show[ListLogLogPlot[rawData],*)
(*Plot[lm[x],{x,0,1000},PlotStyle->Red]*)
(*]*)
(*Print[ "\!\(\*SubscriptBox[\(d\), \(f\)]\)=",df/.FindFit[Log[rawData],a+df *x,{a,df},x],"\[PlusMinus]",lm["ParameterErrors"][[2]],"*)
(*Result from the litterature (exact with SLE): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Subsection:: *)
(*Drop the first few*)


(* ::Input:: *)
(*joinedData=joinedDatab6=rawData;*)


(* ::Input:: *)
(*dropped=droppedb6=DeleteCases[joinedData,{a_,_}/;a<20];*)
(*lm=LinearModelFit[Log[dropped],x,x];*)
(**)
(*Show[ListLogLogPlot[joinedData],ListLogLogPlot[dropped,PlotStyle->Black],*)
(*Plot[lm[x],{x,0,1000},PlotStyle->Red]*)
(*]*)
(*Print[ "\!\(\*SubscriptBox[\(d\), \(f\)]\)=",df/.FindFit[Log[dropped],a+df *x,{a,df},x],"\[PlusMinus]",lm["ParameterErrors"][[2]],"*)
(*Result from the litterature (exact with SLE): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Input:: *)
(*(* Could be better *)*)


(* ::Section::Closed:: *)
(*From data, together*)


(* ::Input:: *)
(*lm1=LinearModelFit[Log[droppedb1],x,x]*)
(*lm2=LinearModelFit[Log[droppedb2],x,x]*)
(*lm3=LinearModelFit[Log[droppedb3],x,x]*)
(*lm4=LinearModelFit[Log[droppedb4],x,x]*)
(*lm5=LinearModelFit[Log[droppedb5],x,x]*)


(* ::Input:: *)
(*lm1["ParameterErrors"][[2]]*)
(*lm2["ParameterErrors"][[2]]*)
(*lm3["ParameterErrors"][[2]]*)
(*lm4["ParameterErrors"][[2]]*)
(*lm5["ParameterErrors"][[2]]*)


(* ::Input:: *)
(*Show[{ListLogLogPlot[droppedb1,PlotStyle->{Black}],*)
(*ListLogLogPlot[droppedb2,PlotStyle->{Blue}],*)
(*ListLogLogPlot[droppedb3,PlotStyle->{Red}],*)
(*ListLogLogPlot[droppedb4,PlotStyle->{Green}],*)
(*ListLogLogPlot[droppedb5,PlotStyle->{Pink}]}]*)


(* ::Chapter:: *)
(*Compare with Analytic result*)


(* ::Section:: *)
(*Exact from SLE (Lawler)*)


(* ::Input:: *)
(*Clear[b]*)


(* ::Input:: *)
(*dfSLE=1+3/(4(2bb+1));*)


(* ::Input:: *)
(*endRange=5;*)
(**)
(*plotSLE=Plot[dfSLE,{bb,0,endRange},PlotStyle->Red,PlotRange->All];*)
(**)
(*Show[{plotSLE},PlotRange->{1,2},AxesLabel->{b,Subscript[d, f]},AxesOrigin->{0,1}]*)


(* ::Section:: *)
(*Compared with SLE*)


(* ::Input:: *)
(*lm1["BestFitParameters"][[2]]*)
(**)
(*lm1["ParameterErrors"][[2]]*)
(**)


(* ::Input:: *)
(*Table[{i,Around[ToExpression["lm"<>ToString[i]]["BestFitParameters"][[2]],ToExpression["lm"<>ToString[i]]["ParameterErrors"][[2]]]},{i,1,5}]*)


(* ::Input:: *)
(*endRange=5;*)
(**)
(*b1Simulation=ListPlot[{{1,Around[1.2486744695483691`, 0.023270605268075166`]}},PlotStyle->Red];*)
(*b2Simulation=ListPlot[{{2,Around[1.1151146520584079`, 0.0134148268356009]}},PlotStyle->Red];*)
(*b3Simulation=ListPlot[{{3,Around[1.0768665526174213`, 0.014642777375504247`]}},PlotStyle->Red];*)
(*b4Simulation=ListPlot[{{4,Around[1.0474461998303197`, 0.008312476568391155]}},PlotStyle->Red];*)
(*b5Simulation=ListPlot[{{5,Around[1.0454880607320536`, 0.0064093876238445445`]}},PlotStyle->Red];*)
(**)
(*plotSLE=Plot[dfSLE,{bb,0,endRange},PlotStyle->Blue,PlotRange->All];*)
(**)
(*Show[{plotSLE,b1Simulation,b2Simulation,b3Simulation,b4Simulation,b5Simulation},PlotRange->{1,2},AxesLabel->{b,Subscript[d, f]},AxesOrigin->{0,1}]*)


(* ::Input:: *)
(**)
(**)
(*fitPlot=Plot[fitFunc,{b,inRange,endRange},PlotStyle->RGBColor[1, 0.5, 0],PlotRange->All];*)
(**)
(*Show[{plotRG1L,plotRG2Lwf,plotRG2L(*,fitPlot*),b0Simulation,b1Simulation,b2Simulation,b3Simulation,b4Simulation,b5Simulation},PlotRange->{1,2},AxesLabel->{b,Subscript[d, f]},AxesOrigin->{0,1}]*)


(* ::Title:: *)
(*After optimization*)


(* ::Chapter:: *)
(*Import data. MODIFY THE HIGHLIGHTED PARTS EVERY TIME*)


(* ::Text:: *)
(*It seems that FITTING with {x,1,1/x,1/x^2,1/x^3} gives the best result or {x,1,1/x,1/x^2}*)


(* ::Input:: *)
(*fitFuncs=Table[x^(-i),{i,-1,3}]*)


(* ::Section::Closed:: *)
(*b=0 With points from the CLUSTER*)


(* ::Subsection:: *)
(*Raw data*)


(* ::Input:: *)
(**)
(*b=0;*)
(**)
(*rawData=data0=ToExpression/@Import[FileNameJoin[{"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d-FromCluster\\b0-clean_merged_data.csv"}],"CSV"]; (* MODIFY FILE NAME *)*)


(* ::Input:: *)
(*ListPlot[rawData]*)
(*ListLogLogPlot[rawData]*)


(* ::Input:: *)
(*lm["ParameterTable"][[1]][[1,3]]*)


(* ::Input:: *)
(*Quiet@lmp["ParameterTable"][[1]][[1,2]]*)


(* ::Input:: *)
(*fitFuncs=Table[x^(-i),{i,-1,3}];*)
(**)
(*lmp=LinearModelFit[Log[rawData],fitFuncs,x];*)
(**)
(*lm=LinearModelFit[Log[rawData],x,x];*)
(**)
(*Show[{ListLogLogPlot[rawData,AxesLabel->{Log[L],Log[N]}],*)
(*Plot[lm[x],{x,0,1000},PlotStyle->{Red,Thickness->0.002}]*)
(*,Plot[lmp[x],{x,0,1000},PlotStyle->{Darker@Green,Thickness->0.002}]*)
(*,Plot[x (dfSLE/.bb->b)-1,{x,1,10000},PlotStyle->{Blue,Dashed}]}*)
(*,PlotLabel->Row[{" b = ",b}]]*)
(**)
(*Print[ "Linear fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Quiet@lm["ParameterTable"][[1]][[1,3,2]]," \[PlusMinus] ",lm["ParameterErrors"][[2]],"*)
(*",Total@fitFuncs," fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lmp["ParameterTable"][[1]][[1,2,2]],lmp["ParameterErrors"][[1]]],"*)
(*Result from the litterature (exact with SLE): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Input:: *)
(*lmp["ParameterTable"][[1]]*)


(* ::Subsection:: *)
(*Drop the first few*)


(* ::Input:: *)
(*threshold=10;*)
(**)
(*dropped=droppedb0=DeleteCases[rawData,{a_,_}/;a<threshold];*)
(*lmdropped=LinearModelFit[Log[dropped],x,x];*)
(**)
(*Show[{ListLogLogPlot[rawData,PlotStyle->Black,AxesLabel->{Log[L],Log[N]}]*)
(*,ListLogLogPlot[dropped,PlotStyle->Green]*)
(*,Plot[lmdropped[x],{x,Log[threshold-1],1000},PlotStyle->{Red,Thickness->0.002}]*)
(*,Plot[x (dfSLE/.bb->b)-1,{x,1,10000},PlotStyle->{Blue,Dashed}]}*)
(*,(*PlotRange->All,*)PlotLabel->Row[{" b = ",b}]]*)
(**)
(*Print[ "Full data: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Quiet@lm["ParameterTable"][[1]][[1,3,2]]," \[PlusMinus] ",lm["ParameterErrors"][[2]],"*)
(*Dropped data: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Quiet@lmdropped["ParameterTable"][[1]][[1,3,2]]," \[PlusMinus] ",lmdropped["ParameterErrors"][[2]],"*)
(*Result from the litterature (exact with SLE): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Section::Closed:: *)
(*b=1 With points from the CLUSTER*)


(* ::Subsection:: *)
(*Raw data*)


(* ::Input:: *)
(*b=1;*)
(**)
(*rawData=data1=ToExpression/@Import[FileNameJoin[{"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d-FromCluster\\b1-clean_merged_data.csv"}],"CSV"]; (* MODIFY FILE NAME *)*)


(* ::Input:: *)
(*ListPlot[rawData]*)
(*ListLogLogPlot[rawData]*)


(* ::Input:: *)
(*fitFuncs=Table[x^(-i),{i,-1,3}];*)
(**)
(*lmp=LinearModelFit[Log[rawData],fitFuncs,x];*)
(**)
(*lm=LinearModelFit[Log[rawData],x,x];*)
(**)
(*Show[{ListLogLogPlot[rawData,AxesLabel->{Log[L],Log[N]}],*)
(*Plot[lm[x],{x,0,1000},PlotStyle->{Red,Thickness->0.004}]*)
(*,Plot[lmp[x],{x,0,1000},PlotStyle->{Darker@Green,Thickness->0.004}]*)
(*,Plot[x (dfSLE/.bb->b)-1,{x,1,10000},PlotStyle->{Blue,Dashed}]}*)
(*,PlotLabel->Row[{" b = ",b}]]*)
(**)
(*Print[ "Linear fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Quiet@lm["ParameterTable"][[1]][[1,3,2]]," \[PlusMinus] ",lm["ParameterErrors"][[2]],"*)
(*",Total@fitFuncs," fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Quiet@lmp["ParameterTable"][[1]][[1,2,2]]," \[PlusMinus] ",lmp["ParameterErrors"][[1]],"*)
(*Result from the litterature (exact with SLE): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Subsection:: *)
(*Drop the first few*)


(* ::Input:: *)
(*threshold=70;*)
(**)
(*dropped=droppedb1=DeleteCases[rawData,{a_,_}/;a<threshold];*)
(*lmdropped=LinearModelFit[Log[dropped],x,x];*)
(**)
(*Show[{ListLogLogPlot[rawData,PlotStyle->Black,AxesLabel->{Log[L],Log[N]}]*)
(*,ListLogLogPlot[dropped,PlotStyle->Green]*)
(*,Plot[lmdropped[x],{x,Log[threshold-1],1000},PlotStyle->{Red,Thickness->0.002}]*)
(*,Plot[x (dfSLE/.bb->b)-1,{x,1,10000},PlotStyle->{Blue,Dashed}]}*)
(*,(*PlotRange->All,*)PlotLabel->Row[{" b = ",b}]]*)
(**)
(*Print[ "Full data: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Quiet@lm["ParameterTable"][[1]][[1,3,2]]," \[PlusMinus] ",lm["ParameterErrors"][[2]],"*)
(*Dropped data: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Quiet@lmdropped["ParameterTable"][[1]][[1,3,2]]," \[PlusMinus] ",lmdropped["ParameterErrors"][[2]],"*)
(*Result from the litterature (exact with SLE): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Section::Closed:: *)
(*b=1 With points from the CLUSTER UPDATED*)


(* ::Subsection:: *)
(*Raw data*)


(* ::Input:: *)
(*b=1;*)
(**)
(*rawData=data1=ToExpression/@Import[FileNameJoin[{"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d-FromCluster\\b1-clean_merged_data-update.csv"}],"CSV"]; (* MODIFY FILE NAME *)*)


(* ::Input:: *)
(*ListPlot[rawData]*)
(*ListLogLogPlot[rawData]*)


(* ::Input:: *)
(*fitFuncs=Table[x^(-i),{i,-1,3}];*)
(**)
(*lmp=LinearModelFit[Log[rawData],fitFuncs,x];*)
(**)
(*lm=LinearModelFit[Log[rawData],x,x];*)
(**)
(*Show[{ListLogLogPlot[rawData,AxesLabel->{Log[L],Log[N]}],*)
(*Plot[lm[x],{x,0,1000},PlotStyle->{Red,Thickness->0.004}]*)
(*,Plot[lmp[x],{x,0,1000},PlotStyle->{Darker@Green,Thickness->0.004}]*)
(*,Plot[x (dfSLE/.bb->b)-1,{x,1,10000},PlotStyle->{Blue,Dashed}]}*)
(*,PlotLabel->Row[{" b = ",b}]]*)
(**)
(*Print[ "Linear fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Quiet@lm["ParameterTable"][[1]][[1,3,2]]," \[PlusMinus] ",lm["ParameterErrors"][[2]],"*)
(*",Total@fitFuncs," fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Quiet@lmp["ParameterTable"][[1]][[1,2,2]]," \[PlusMinus] ",lmp["ParameterErrors"][[1]],"*)
(*Result from the litterature (exact with SLE): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Subsection:: *)
(*Drop the first few*)


(* ::Input:: *)
(*threshold=70;*)
(**)
(*dropped=droppedb1=DeleteCases[rawData,{a_,_}/;a<threshold];*)
(*lmdropped=LinearModelFit[Log[dropped],x,x];*)
(**)
(*Show[{ListLogLogPlot[rawData,PlotStyle->Black,AxesLabel->{Log[L],Log[N]}]*)
(*,ListLogLogPlot[dropped,PlotStyle->Green]*)
(*,Plot[lmdropped[x],{x,Log[threshold-1],1000},PlotStyle->{Red,Thickness->0.002}]*)
(*,Plot[x (dfSLE/.bb->b)-1,{x,1,10000},PlotStyle->{Blue,Dashed}]}*)
(*,(*PlotRange->All,*)PlotLabel->Row[{" b = ",b}]]*)
(**)
(*Print[ "Full data: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Quiet@lm["ParameterTable"][[1]][[1,3,2]]," \[PlusMinus] ",lm["ParameterErrors"][[2]],"*)
(*Dropped data: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Quiet@lmdropped["ParameterTable"][[1]][[1,3,2]]," \[PlusMinus] ",lmdropped["ParameterErrors"][[2]],"*)
(*Result from the litterature (exact with SLE): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Section::Closed:: *)
(*b=2 With points from the CLUSTER*)


(* ::Subsection:: *)
(*Raw data*)


(* ::Input:: *)
(*b=2;*)
(**)
(*rawData=data2=ToExpression/@Import[FileNameJoin[{"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d-FromCluster\\b2-clean_merged_data.csv"}],"CSV"]; (* MODIFY FILE NAME *)*)


(* ::Input:: *)
(*ListPlot[rawData]*)
(*ListLogLogPlot[rawData]*)


(* ::Input:: *)
(*fitFuncs=Table[x^(-i),{i,-1,3}];*)
(**)
(*lmp=LinearModelFit[Log[rawData],fitFuncs,x];*)
(**)
(*lm=LinearModelFit[Log[rawData],x,x];*)
(**)
(*Show[{ListLogLogPlot[rawData,AxesLabel->{Log[L],Log[N]}],*)
(*Plot[lm[x],{x,0,1000},PlotStyle->{Red,Thickness->0.004}]*)
(*,Plot[lmp[x],{x,0,1000},PlotStyle->{Darker@Green,Thickness->0.004}]*)
(*,Plot[x (dfSLE/.bb->b)-1,{x,1,10000},PlotStyle->{Blue,Dashed}]}*)
(*,PlotLabel->Row[{" b = ",b}]]*)
(**)
(*Print[ "Linear fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Quiet@lm["ParameterTable"][[1]][[1,3,2]]," \[PlusMinus] ",lm["ParameterErrors"][[2]],"*)
(*",Total@fitFuncs," fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Quiet@lmp["ParameterTable"][[1]][[1,2,2]]," \[PlusMinus] ",lmp["ParameterErrors"][[1]],"*)
(*Result from the litterature (exact with SLE): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Subsection:: *)
(*Drop the first few*)


(* ::Input:: *)
(*threshold=100;*)
(**)
(*dropped=droppedb2=DeleteCases[rawData,{a_,_}/;a<threshold];*)
(*lmdropped=LinearModelFit[Log[dropped],x,x];*)
(**)
(*Show[{ListLogLogPlot[rawData,PlotStyle->Black,AxesLabel->{Log[L],Log[N]}]*)
(*,ListLogLogPlot[dropped,PlotStyle->Green]*)
(*,Plot[lmdropped[x],{x,Log[threshold-1],1000},PlotStyle->{Red,Thickness->0.002}]*)
(*,Plot[x (dfSLE/.bb->b)-1,{x,1,10000},PlotStyle->{Blue,Dashed}]}*)
(*,(*PlotRange->All,*)PlotLabel->Row[{" b = ",b}]]*)
(**)
(*Print[ "Full data: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Quiet@lm["ParameterTable"][[1]][[1,3,2]]," \[PlusMinus] ",lm["ParameterErrors"][[2]],"*)
(*Dropped data: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Quiet@lmdropped["ParameterTable"][[1]][[1,3,2]]," \[PlusMinus] ",lmdropped["ParameterErrors"][[2]],"*)
(*Result from the litterature (exact with SLE): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Section::Closed:: *)
(*b=2 With points from the CLUSTER UPDATED*)


(* ::Input:: *)
(*b=2;*)
(**)
(*data2update=ToExpression/@Import[FileNameJoin[{"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d-FromCluster\\b2-clean_merged_data-update.csv"}],"CSV"]; *)
(**)
(*data2double=DeleteDuplicates[ToExpression/@Import[FileNameJoin[{"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d-FromCluster\\b2-clean_merged_data-V7.csv"}],"CSV"]]; *)
(**)
(*rawData=Join[data2update,data2double];*)
(*(* MODIFY FILE NAME *)*)


(* ::Input:: *)
(*ListPlot[rawData]*)
(*ListLogLogPlot[rawData]*)


(* ::Input:: *)
(*fitFuncs=Table[x^(-i),{i,-1,3}];*)
(*fitFuncs=Append[Table[x^(-i),{i,-1,0}],Exp[-x]];*)
(**)
(*lmp=LinearModelFit[Log[rawData],fitFuncs,x];*)
(**)
(*lm=LinearModelFit[Log[rawData],x,x];*)
(**)
(*Show[{ListLogLogPlot[rawData,AxesLabel->{Log[L],Log[N]},DataRange->{0,50}],*)
(*Plot[lm[x],{x,0,1000},PlotStyle->{Red,Thickness->0.004}]*)
(*,Plot[lmp[x],{x,0,1000},PlotStyle->{Darker@Green,Thickness->0.004}]*)
(*,Plot[x (dfSLE/.bb->b)-1,{x,1,10000},PlotStyle->{Blue,Dashed}]}*)
(*,PlotLabel->Row[{" b = ",b}](*,PlotRange->{{1,Log[200]},{0,Log[100]}}*)*)
(*]*)
(**)
(*Print[ "Linear fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lm["ParameterTable"][[1]][[1,3,2]],lm["ParameterErrors"][[2]]],"*)
(*",Total@fitFuncs," fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lmp["ParameterTable"][[1]][[1,2,2]],lmp["ParameterErrors"][[1]]],"*)
(*Result from the litterature (exact with SLE): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Input:: *)
(*Quiet@lmp["ParameterTable"][[1]]*)


(* ::Subsection:: *)
(*Drop the first few*)


(* ::Input:: *)
(*threshold=70;*)
(**)
(*dropped=droppedb2update=DeleteCases[rawData,{a_,_}/;a<threshold];*)
(*lmdropped=LinearModelFit[Log[dropped],x,x];*)
(**)
(*Show[{ListLogLogPlot[rawData,PlotStyle->Black,AxesLabel->{Log[L],Log[N]}]*)
(*,ListLogLogPlot[dropped,PlotStyle->RGBColor[0, 1, 0]]*)
(*,Plot[lmdropped[x],{x,Log[threshold-1],1000},PlotStyle->{Red,Thickness->0.002}]*)
(*,Plot[x (dfSLE/.bb->b)-1,{x,1,10000},PlotStyle->{Blue,Dashed}]}*)
(*,(*PlotRange->All,*)PlotLabel->Row[{" b = ",b}]]*)
(**)
(*Print[ "Full data - Linear fit : \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lm["ParameterTable"][[1]][[1,3,2]],lm["ParameterErrors"][[2]]],"*)
(*Full data - ",Total@fitFuncs," fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lmp["ParameterTable"][[1]][[1,2,2]],lmp["ParameterErrors"][[1]]],"*)
(*Dropped data: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lmdropped["ParameterTable"][[1]][[1,3,2]],lmdropped["ParameterErrors"][[2]]],"*)
(*Result from the litterature (exact with SLE): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Section::Closed:: *)
(*b=2 With points from the CLUSTER NoTolleranceBound*)


(* ::Subsection:: *)
(*Raw data*)


(* ::Input:: *)
(*b=2;*)
(**)
(*rawData=data2NoTolleranceBound=ToExpression/@Import[FileNameJoin[{"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d-FromCluster\\b2-clean_merged_data-NoTolleranceBound.csv"}],"CSV"]; (* MODIFY FILE NAME *)*)


(* ::Input:: *)
(*ListPlot[rawData]*)
(*ListLogLogPlot[rawData]*)


(* ::Input:: *)
(*fitFuncs=Table[x^(-i),{i,-1,2}];*)
(**)
(*lmp=LinearModelFit[Log[rawData],fitFuncs,x];*)
(**)
(*lm=LinearModelFit[Log[rawData],x,x];*)
(**)
(*Show[{ListLogLogPlot[rawData,AxesLabel->{Log[L],Log[N]}],*)
(*Plot[lm[x],{x,0,1000},PlotStyle->{Red,Thickness->0.004}]*)
(*,Plot[lmp[x],{x,0,1000},PlotStyle->{Darker@Green,Thickness->0.004}]*)
(*,Plot[x (dfSLE/.bb->b)-1,{x,1,10000},PlotStyle->{Blue,Dashed}]}*)
(*,PlotLabel->Row[{" b = ",b}]]*)
(**)
(*Print[ "Linear fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Quiet@lm["ParameterTable"][[1]][[1,3,2]]," \[PlusMinus] ",lm["ParameterErrors"][[2]],"*)
(*",Total@fitFuncs," fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Quiet@lmp["ParameterTable"][[1]][[1,2,2]]," \[PlusMinus] ",lmp["ParameterErrors"][[1]],"*)
(*Result from the litterature (exact with SLE): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Subsection:: *)
(*Drop the first few*)


(* ::Input:: *)
(*threshold=20;*)
(**)
(*dropped=droppedb2=DeleteCases[rawData,{a_,_}/;a<threshold];*)
(*lmdropped=LinearModelFit[Log[dropped],x,x];*)
(**)
(*Show[{ListLogLogPlot[rawData,PlotStyle->Black,AxesLabel->{Log[L],Log[N]}]*)
(*,ListLogLogPlot[dropped,PlotStyle->Green]*)
(*,Plot[lmdropped[x],{x,Log[threshold-1],1000},PlotStyle->{Red,Thickness->0.002}]*)
(*,Plot[x (dfSLE/.bb->b)-1,{x,1,10000},PlotStyle->{Blue,Dashed}]}*)
(*,(*PlotRange->All,*)PlotLabel->Row[{" b = ",b}]]*)
(**)
(*Print[ "Full data: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Quiet@lm["ParameterTable"][[1]][[1,3,2]]," \[PlusMinus] ",lm["ParameterErrors"][[2]],"*)
(*Dropped data: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Quiet@lmdropped["ParameterTable"][[1]][[1,3,2]]," \[PlusMinus] ",lmdropped["ParameterErrors"][[2]],"*)
(*Result from the litterature (exact with SLE): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Section::Closed:: *)
(*b=3 With points from the CLUSTER*)


(* ::Subsection:: *)
(*Raw data*)


(* ::Input:: *)
(*b=3;*)
(**)
(*rawData=data3=ToExpression/@Import[FileNameJoin[{"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d-FromCluster\\b3-clean_merged_data.csv"}],"CSV"]; (* MODIFY FILE NAME *)*)


(* ::Input:: *)
(*ListPlot[rawData]*)
(*ListLogLogPlot[rawData]*)


(* ::Input:: *)
(*fitFuncs=Table[x^(-i),{i,-1,3}];*)
(**)
(*lmp=LinearModelFit[Log[rawData],fitFuncs,x];*)
(**)
(*lm=LinearModelFit[Log[rawData],x,x];*)
(**)
(*Show[{ListLogLogPlot[rawData,AxesLabel->{Log[L],Log[N]}],*)
(*Plot[lm[x],{x,0,1000},PlotStyle->{Red,Thickness->0.004}]*)
(*,Plot[lmp[x],{x,0,1000},PlotStyle->{Darker@Green,Thickness->0.004}]*)
(*,Plot[x (dfSLE/.bb->b)-1,{x,1,10000},PlotStyle->{Blue,Dashed}]}*)
(*,PlotLabel->Row[{" b = ",b}]]*)
(**)
(*Print[ "Linear fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Quiet@lm["ParameterTable"][[1]][[1,3,2]]," \[PlusMinus] ",lm["ParameterErrors"][[2]],"*)
(*",Total@fitFuncs," fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Quiet@lmp["ParameterTable"][[1]][[1,2,2]]," \[PlusMinus] ",lmp["ParameterErrors"][[1]],"*)
(*Result from the litterature (exact with SLE): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Subsection:: *)
(*Drop the first few*)


(* ::Input:: *)
(*threshold=40;*)
(**)
(*dropped=droppedb3=DeleteCases[rawData,{a_,_}/;a<threshold];*)
(*lmdropped=LinearModelFit[Log[dropped],x,x];*)
(**)
(*Show[{ListLogLogPlot[rawData,PlotStyle->Black,AxesLabel->{Log[L],Log[N]}]*)
(*,ListLogLogPlot[dropped,PlotStyle->Green]*)
(*,Plot[lmdropped[x],{x,Log[threshold-1],1000},PlotStyle->{Red,Thickness->0.002}]*)
(*,Plot[x (dfSLE/.bb->b)-1,{x,1,10000},PlotStyle->{Blue,Dashed}]}*)
(*,(*PlotRange->All,*)PlotLabel->Row[{" b = ",b}]]*)
(**)
(*Print[ "Full data: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Quiet@lm["ParameterTable"][[1]][[1,3,2]]," \[PlusMinus] ",lm["ParameterErrors"][[2]],"*)
(*Dropped data: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Quiet@lmdropped["ParameterTable"][[1]][[1,3,2]]," \[PlusMinus] ",lmdropped["ParameterErrors"][[2]],"*)
(*Result from the litterature (exact with SLE): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Section::Closed:: *)
(*b=4 With points from the CLUSTER*)


(* ::Subsection:: *)
(*Raw data*)


(* ::Input:: *)
(*b=4;*)
(**)
(*rawData=data4=ToExpression/@Import[FileNameJoin[{"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d-FromCluster\\b4-clean_merged_data.csv"}],"CSV"]; (* MODIFY FILE NAME *)*)


(* ::Input:: *)
(*ListPlot[rawData]*)
(*ListLogLogPlot[rawData]*)


(* ::Input:: *)
(*fitFuncs=Table[x^(-i),{i,-1,3}];*)
(**)
(*lmp=LinearModelFit[Log[rawData],fitFuncs,x];*)
(**)
(*lm=LinearModelFit[Log[rawData],x,x];*)
(**)
(*Show[{ListLogLogPlot[rawData,AxesLabel->{Log[L],Log[N]}],*)
(*Plot[lm[x],{x,0,1000},PlotStyle->{Red,Thickness->0.004}]*)
(*,Plot[lmp[x],{x,0,1000},PlotStyle->{Darker@Green,Thickness->0.004}]*)
(*,Plot[x (dfSLE/.bb->b)-1,{x,1,10000},PlotStyle->{Blue,Dashed}]}*)
(*,PlotLabel->Row[{" b = ",b}]]*)
(**)
(*Print[ "Linear fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Quiet@lm["ParameterTable"][[1]][[1,3,2]]," \[PlusMinus] ",lm["ParameterErrors"][[2]],"*)
(*",Total@fitFuncs," fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Quiet@lmp["ParameterTable"][[1]][[1,2,2]]," \[PlusMinus] ",lmp["ParameterErrors"][[1]],"*)
(*Result from the litterature (exact with SLE): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Subsection:: *)
(*Drop the first few*)


(* ::Input:: *)
(*threshold=50;*)
(**)
(*dropped=droppedb4=DeleteCases[rawData,{a_,_}/;a<threshold];*)
(*lmdropped=LinearModelFit[Log[dropped],x,x];*)
(**)
(*Show[{ListLogLogPlot[rawData,PlotStyle->Black,AxesLabel->{Log[L],Log[N]}]*)
(*,ListLogLogPlot[dropped,PlotStyle->Green]*)
(*,Plot[lmdropped[x],{x,Log[threshold-1],1000},PlotStyle->{Red,Thickness->0.002}]*)
(*,Plot[x (dfSLE/.bb->b)-1,{x,1,10000},PlotStyle->{Blue,Dashed}]}*)
(*,(*PlotRange->All,*)PlotLabel->Row[{" b = ",b}]]*)
(**)
(*Print[ "Full data: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Quiet@lm["ParameterTable"][[1]][[1,3,2]]," \[PlusMinus] ",lm["ParameterErrors"][[2]],"*)
(*Dropped data: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Quiet@lmdropped["ParameterTable"][[1]][[1,3,2]]," \[PlusMinus] ",lmdropped["ParameterErrors"][[2]],"*)
(*Result from the litterature (exact with SLE): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Section::Closed:: *)
(*b=4 With points from the CLUSTER UPDATED*)


(* ::Subsection:: *)
(*Raw data*)


(* ::Input:: *)
(*b=4;*)
(**)
(*rawData=data4update=ToExpression/@Import[FileNameJoin[{"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d-FromCluster\\b4-clean_merged_data-update.csv"}],"CSV"]; (* MODIFY FILE NAME *)*)


(* ::Input:: *)
(*ListPlot[rawData]*)
(*ListLogLogPlot[rawData]*)


(* ::Input:: *)
(*fitFuncs=Table[x^(-i),{i,-1,3}];*)
(**)
(*lmp=LinearModelFit[Log[rawData],fitFuncs,x];*)
(**)
(*lm=LinearModelFit[Log[rawData],x,x];*)
(**)
(*Show[{ListLogLogPlot[rawData,AxesLabel->{Log[L],Log[N]}],*)
(*Plot[lm[x],{x,0,1000},PlotStyle->{Red,Thickness->0.004}]*)
(*,Plot[lmp[x],{x,0,1000},PlotStyle->{Darker@Green,Thickness->0.004}]*)
(*,Plot[x (dfSLE/.bb->b)-1,{x,1,10000},PlotStyle->{Blue,Dashed}]}*)
(*,PlotLabel->Row[{" b = ",b}]]*)
(**)
(*Print[ "Linear fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lm["ParameterTable"][[1]][[1,3,2]],lm["ParameterErrors"][[2]]],"*)
(*",Total@fitFuncs," fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lmp["ParameterTable"][[1]][[1,2,2]],lmp["ParameterErrors"][[1]]],"*)
(*Result from the litterature (exact with SLE): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Subsection:: *)
(*Drop the first few*)


(* ::Input:: *)
(*threshold=120;*)
(**)
(*dropped=droppedb4update=DeleteCases[rawData,{a_,_}/;a<threshold];*)
(*lmdropped=LinearModelFit[Log[dropped],x,x];*)
(**)
(*Show[{ListLogLogPlot[rawData,PlotStyle->Black,AxesLabel->{Log[L],Log[N]}]*)
(*,ListLogLogPlot[dropped,PlotStyle->Green]*)
(*,Plot[lmdropped[x],{x,Log[threshold-1],1000},PlotStyle->{Red,Thickness->0.002}]*)
(*,Plot[x (dfSLE/.bb->b)-1,{x,1,10000},PlotStyle->{Blue,Dashed}]}*)
(*,(*PlotRange->All,*)PlotLabel->Row[{" b = ",b}]]*)
(**)
(*Print[ "Full data: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lm["ParameterTable"][[1]][[1,3,2]],lm["ParameterErrors"][[2]]],"*)
(*Dropped data: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lmdropped["ParameterTable"][[1]][[1,3,2]],lmdropped["ParameterErrors"][[2]]],"*)
(*Result from the litterature (exact with SLE): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Section::Closed:: *)
(*b=4 With points from the CLUSTER NoTolleranceBound *)


(* ::Subsection:: *)
(*Raw data*)


(* ::Input:: *)
(*b=4;*)
(**)
(*rawData=data4NoTolleranceBound=ToExpression/@Import[FileNameJoin[{"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d-FromCluster\\b4-clean_merged_data-NoTolleranceBound.csv"}],"CSV"]; (* MODIFY FILE NAME *)*)


(* ::Input:: *)
(*ListPlot[rawData]*)
(*ListLogLogPlot[rawData]*)


(* ::Input:: *)
(*fitFuncs=Table[x^(-i),{i,-1,3}];*)
(**)
(*lmp=LinearModelFit[Log[rawData],fitFuncs,x];*)
(**)
(*lm=LinearModelFit[Log[rawData],x,x];*)
(**)
(*Show[{ListLogLogPlot[rawData,AxesLabel->{Log[L],Log[N]}],*)
(*Plot[lm[x],{x,0,1000},PlotStyle->{Red,Thickness->0.004}]*)
(*,Plot[lmp[x],{x,0,1000},PlotStyle->{Darker@Green,Thickness->0.004}]*)
(*,Plot[x (dfSLE/.bb->b)-1,{x,1,10000},PlotStyle->{Blue,Dashed}]}*)
(*,PlotLabel->Row[{" b = ",b}]]*)
(**)
(*Print[ "Linear fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Quiet@lm["ParameterTable"][[1]][[1,3,2]]," \[PlusMinus] ",lm["ParameterErrors"][[2]],"*)
(*",Total@fitFuncs," fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lmp["ParameterTable"][[1]][[1,2,2]],lmp["ParameterErrors"][[1]]],"*)
(*Result from the litterature (exact with SLE): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Subsection:: *)
(*Drop the first few*)


(* ::Input:: *)
(*threshold=50;*)
(**)
(*dropped=droppedb4NoTolleranceBound=DeleteCases[rawData,{a_,_}/;a<threshold];*)
(*lmdropped=LinearModelFit[Log[dropped],x,x];*)
(**)
(*Show[{ListLogLogPlot[rawData,PlotStyle->Black,AxesLabel->{Log[L],Log[N]}]*)
(*,ListLogLogPlot[dropped,PlotStyle->Green]*)
(*,Plot[lmdropped[x],{x,Log[threshold-1],1000},PlotStyle->{Red,Thickness->0.002}]*)
(*,Plot[x (dfSLE/.bb->b)-1,{x,1,10000},PlotStyle->{Blue,Dashed}]}*)
(*,(*PlotRange->All,*)PlotLabel->Row[{" b = ",b}]]*)
(**)
(*Print[ "Full data: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Quiet@lm["ParameterTable"][[1]][[1,3,2]]," \[PlusMinus] ",lm["ParameterErrors"][[2]],"*)
(*Dropped data: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Quiet@lmdropped["ParameterTable"][[1]][[1,3,2]]," \[PlusMinus] ",lmdropped["ParameterErrors"][[2]],"*)
(*Result from the litterature (exact with SLE): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Section::Closed:: *)
(*b4 - clean_merged_data - Square . csv (+extra analysis)*)


(* ::Input:: *)
(*b=4;*)
(**)
(*rawData=data4Square=ToExpression/@Import[FileNameJoin[{"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d-FromCluster\\b4-clean_merged_data-Square.csv"}],"CSV"];*)
(* (* MODIFY FILE NAME *)*)
(*Length[rawData]*)


(* ::Input:: *)
(*ListPlot[rawData]*)
(*ListLogLogPlot[rawData]*)


(* ::Input:: *)
(*fitFuncs=Table[x^(-i),{i,-1,3}];*)
(*(*fitFuncs=Append[Table[x^(-i),{i,-1,0}],Exp[-x]];*)*)
(*(**)
(*fitFuncs={x,1,Exp[-x],Exp[-2 x]};(*Exp[-1.3 x]*)*)*)
(*fitFuncs={x,1,Exp[-2x]};*)
(**)
(*lmp=LinearModelFit[Log[rawData],fitFuncs,x];*)
(**)
(*lm=LinearModelFit[Log[rawData],x,x];*)
(**)
(*fitFunc=a+c Exp[-\[Omega] x]+df x;*)
(*fitSol=FindFit[Log[rawData],{fitFunc,{-2<a<1.1,0<\[Omega]<3}},{a,c,\[Omega],df},x]*)
(**)
(*Show[{ListLogLogPlot[rawData,AxesLabel->{Log[L],Log[N]},DataRange->{0,50}],*)
(*Plot[lm[x],{x,0,1000},PlotStyle->{RGBColor[1, 0, 0],Thickness->0.004}]*)
(**)
(*,Plot[lmp[x],{x,0,1000},PlotStyle->{RGBColor[0, Rational[2, 3], 0],Thickness->0.004}]*)
(*,Plot[fitFunc/.fitSol,{x,0,1000},PlotStyle->{RGBColor[1, Rational[2, 3], 1],Thickness->0.004}]*)
(**)
(*,Plot[x (dfSLE/.bb->b)-1,{x,1,10000},PlotStyle->{RGBColor[0, 0, 1],Dashed}]}*)
(*,PlotLabel->Row[{" b = ",b}](*,PlotRange->{{1,Log[200]},{0,Log[100]}}*)*)
(*]*)
(**)
(*Print[ "Linear fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lm["ParameterTable"][[1]][[1,3,2]],\[Pi]*lm["ParameterErrors"][[2]]],"*)
(*",Total@fitFuncs," fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lmp["ParameterTable"][[1]][[1,2,2]],\[Pi]*lmp["ParameterErrors"][[1]]],"*)
(*",fitFunc," fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[df/.fitSol,Null],"*)
(*Result from the litterature (exact with SLE): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Input:: *)
(*Quiet@lm["ParameterTable"][[1]]*)
(*Quiet@lmp["ParameterTable"][[1]]*)


(* ::Subsection:: *)
(*Drop both first and/or last few*)


(* ::Input:: *)
(*maxx=Max[rawData[[All,1]]];*)
(*maxy=Max[rawData[[All,2]]];*)
(**)
(*thresholdBelow=44;*)
(*thresholdAbove=maxx-0;*)
(**)
(*dropped=droppedb4Square=Select[rawData,thresholdBelow<#[[1]]<thresholdAbove&];*)
(**)
(*lmdropped=LinearModelFit[Log[dropped],x,x];*)
(**)
(*lmpdropped=LinearModelFit[Log[dropped],fitFuncs,x];*)
(**)
(*Show[{ListLogLogPlot[rawData,PlotStyle->GrayLevel[0],AxesLabel->{Log[L],Log[N]}]*)
(*,ListLogLogPlot[dropped,PlotStyle->RGBColor[0, 1, 1]]*)
(*,Plot[lmp[x],{x,0,1000},PlotStyle->{RGBColor[0, Rational[2, 3], 0],Thickness->0.004}]*)
(*,Plot[lmdropped[x],{x,Log[thresholdBelow],Log[thresholdAbove-1]},PlotStyle->{RGBColor[1, 0, 0],Thickness->0.004}]*)
(*,Plot[lmpdropped[x],{x,Log[thresholdBelow],Log[thresholdAbove-1]},PlotStyle->{RGBColor[0.5, 0, 0.5],Thickness->0.004}]*)
(*,Plot[x (dfSLE/.bb->b)-1,{x,1,10000},PlotStyle->{RGBColor[0, 0, 1],Dashed}]}*)
(*,(*PlotRange->All,*)PlotLabel->Row[{" b = ",b}]*)
(*,Epilog->{Directive[Dashed,GrayLevel[0.5]],*)
(*Line[{{Log[thresholdBelow],0},{Log[thresholdBelow],Log[maxy]//N}}]*)
(*,Line[{{Log[thresholdAbove],0},{Log[thresholdAbove],Log[maxy]//N}}]}*)
(*]*)
(**)
(*Print[ "Full data - Linear fit : \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lm["ParameterTable"][[1]][[1,3,2]],\[Pi]*lm["ParameterErrors"][[2]]],"*)
(*Dropped data: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lmdropped["ParameterTable"][[1]][[1,3,2]],\[Pi]*lmdropped["ParameterErrors"][[2]]],"*)
(**)
(*Full data - ",Total@fitFuncs," fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lmp["ParameterTable"][[1]][[1,2,2]],\[Pi]*lmp["ParameterErrors"][[1]]],"*)
(*Dropped data with " ,Total@fitFuncs," fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lmpdropped["ParameterTable"][[1]][[1,2,2]],\[Pi]*lmpdropped["ParameterErrors"][[1]]],"*)
(**)
(*Result from the litterature (exact with SLE): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Subsection::Closed:: *)
(*Extra analysis*)


(* ::Subsubsection:: *)
(*Plot with different drops below*)


(* ::Input:: *)
(*dfDropped=Table[With[{*)
(*lmdropped=LinearModelFit[Log[DeleteCases[rawData,{a_,_}/;(a<i )]],x,x]},*)
(*{i,Quiet@lmdropped["ParameterTable"][[1]][[1,3,2]]}],{i,5,100}];*)


(* ::Input:: *)
(*dfDroppedMore=Table[With[{*)
(*lmdropped=LinearModelFit[Log[DeleteCases[rawData,{a_,_}/;(a<i )]],x,x]},*)
(*{i,Quiet@lmdropped["ParameterTable"][[1]][[1,3,2]]}],{i,101,200}];*)


(* ::Input:: *)
(*dfDroppedMore2=Table[With[{*)
(*lmdropped=LinearModelFit[Log[DeleteCases[rawData,{a_,_}/;(a<i )]],x,x]},*)
(*{i,Quiet@lmdropped["ParameterTable"][[1]][[1,3,2]]}],{i,201,400}];*)


(* ::Input:: *)
(*dfDroppedMore3=Table[With[{*)
(*lmdropped=LinearModelFit[Log[DeleteCases[rawData,{a_,_}/;(a<i )]],x,x]},*)
(*{i,Quiet@lmdropped["ParameterTable"][[1]][[1,3,2]]}],{i,401,800}];*)


(* ::Input:: *)
(*dfDroppedMore4=Table[With[{*)
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


(* ::Subsubsection::Closed:: *)
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


(* ::Section:: *)
(*b4-clean_merged_data-AfterValidation.csv (+extra analysis)*)


(* ::Input:: *)
(*b=4;*)
(**)
(*rawData=data4Square=*)
(*Map[ToExpression,Import[FileNameJoin[{"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d-FromCluster\\b4-clean_merged_data-AfterValidation.csv"}],"CSV"]];*)
(* (* MODIFY FILE NAME *)*)
(*Length[rawData]*)


(* ::Input:: *)
(*ListPlot[rawData]*)
(*ListLogLogPlot[rawData]*)


(* ::Input:: *)
(*fitFuncs=Table[x^(-i),{i,-1,3}];*)
(*(*fitFuncs=Append[Table[x^(-i),{i,-1,0}],Exp[-x]];*)*)
(*(**)
(*fitFuncs={x,1,Exp[-x],Exp[-2 x]};(*Exp[-1.3 x]*)*)*)
(*fitFuncs={x,1,Exp[-2x]};*)
(**)
(*logData=ParallelMap[Log,rawData];*)


(* ::Input:: *)
(**)
(*lmp=LinearModelFit[logData,fitFuncs,x];*)
(**)
(*lm=LinearModelFit[logData,x,x];*)
(**)
(*fitFunc=a+c Exp[-\[Omega] x]+df x;*)
(*fitSol=FindFit[logData,{fitFunc,{-2<a<1.1,1.5<\[Omega]<3}},{a,c,\[Omega],df},x]*)


(* ::Input:: *)
(*Show[{ListLogLogPlot[rawData,AxesLabel->{Log[L],Log[N]},DataRange->{0,50}],*)
(*Plot[lm[x],{x,0,1000},PlotStyle->{RGBColor[1, 0, 0],Thickness->0.004}]*)
(**)
(*,Plot[lmp[x],{x,0,1000},PlotStyle->{RGBColor[0, Rational[2, 3], 0],Thickness->0.004}]*)
(*,Plot[fitFunc/.fitSol,{x,0,1000},PlotStyle->{RGBColor[1, Rational[2, 3], 1],Thickness->0.004}]*)
(**)
(*,Plot[x (dfSLE/.bb->b)-1,{x,1,10000},PlotStyle->{RGBColor[0, 0, 1],Dashed}]}*)
(*,PlotLabel->Row[{" b = ",b}](*,PlotRange->{{1,Log[200]},{0,Log[100]}}*)*)
(*]*)
(**)
(*Print[ "Linear fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lm["ParameterTable"][[1]][[1,3,2]],\[Pi]*lm["ParameterErrors"][[2]]],"*)
(*",Total@fitFuncs," fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lmp["ParameterTable"][[1]][[1,2,2]],\[Pi]*lmp["ParameterErrors"][[1]]],"*)
(*",fitFunc," fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[df/.fitSol,Null],"*)
(*Result from the litterature (exact with SLE): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Input:: *)
(*Quiet@lm["ParameterTable"][[1]]*)
(*Quiet@lmp["ParameterTable"][[1]]*)


(* ::Subsection:: *)
(*Drop both first and/or last few*)


(* ::Input:: *)
(*maxx=Max[rawData[[All,1]]];*)
(*maxy=Max[rawData[[All,2]]];*)
(**)
(*thresholdBelow=44;*)
(*thresholdAbove=maxx-0;*)
(**)
(*dropped=droppedb4AfterValidation=Select[logData,Log[thresholdBelow]<#[[1]]<Log[thresholdAbove]&];*)
(**)
(*lmdropped=LinearModelFit[dropped,x,x];*)
(**)
(*lmpdropped=LinearModelFit[dropped,fitFuncs,x];*)


(* ::Input:: *)
(**)
(*Show[{ListLogLogPlot[rawData,PlotStyle->GrayLevel[0],AxesLabel->{Log[L],Log[N]}]*)
(*,ListPlot[dropped,PlotStyle->RGBColor[0, 1, 1]]*)
(*,Plot[lmp[x],{x,0,1000},PlotStyle->{RGBColor[0, Rational[2, 3], 0],Thickness->0.004}]*)
(*,Plot[lmdropped[x],{x,Log[thresholdBelow],Log[thresholdAbove-1]},PlotStyle->{RGBColor[1, 0, 0],Thickness->0.004}]*)
(*,Plot[lmpdropped[x],{x,Log[thresholdBelow],Log[thresholdAbove-1]},PlotStyle->{RGBColor[0.5, 0, 0.5],Thickness->0.004}]*)
(*,Plot[x (dfSLE/.bb->b)-1,{x,1,10000},PlotStyle->{RGBColor[0, 0, 1],Dashed}]}*)
(*,(*PlotRange->All,*)PlotLabel->Row[{" b = ",b}]*)
(*,Epilog->{Directive[Dashed,GrayLevel[0.5]],*)
(*Line[{{Log[thresholdBelow],0},{Log[thresholdBelow],Log[maxy]//N}}]*)
(*,Line[{{Log[thresholdAbove],0},{Log[thresholdAbove],Log[maxy]//N}}]}*)
(*]*)


(* ::Input:: *)
(**)
(*Print[ "Full data - Linear fit : \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lm["ParameterTable"][[1]][[1,3,2]],\[Pi]*lm["ParameterErrors"][[2]]],"*)
(*Dropped data: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lmdropped["ParameterTable"][[1]][[1,3,2]],\[Pi]*lmdropped["ParameterErrors"][[2]]],"*)
(**)
(*Full data - ",Total@fitFuncs," fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lmp["ParameterTable"][[1]][[1,2,2]],\[Pi]*lmp["ParameterErrors"][[1]]],"*)
(*Dropped data with " ,Total@fitFuncs," fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lmpdropped["ParameterTable"][[1]][[1,2,2]],\[Pi]*lmpdropped["ParameterErrors"][[1]]],"*)
(**)
(*Result from the litterature (exact with SLE): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Subsection:: *)
(*Test with mean over same L*)


(* ::Input:: *)
(*rawData[[1;;10]]*)
(*GroupBy[%,First->Last]*)
(*KeyValueMap[{#1,Mean[#2]}&,%]*)


(* ::Input:: *)
(*rawData[[1;;10]]*)
(**)
(*(*Group by x,extract y values*)*)
(*grouped=GroupBy[%,First->Last];*)
(**)
(*(*Map over the groups to create {x,Around[mean,stdDev]} pairs*)*)
(*averagedWithErrors=KeyValueMap[Function[{x,yValues},{x,If[Length[yValues]>1,Around[Mean[yValues],StandardDeviation[yValues]],Around[Mean[yValues],0] (*Error is 0 if there is only 1 data point*)]}],grouped]*)


(* ::Input:: *)
(*Log[{{1001,Around[852., 22.499382707581606`]}}]*)


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
(**)
(*lmAveraged=LinearModelFit[Log[Averaged],x,x];*)
(**)
(*lmpAveraged=LinearModelFit[Log[Averaged],fitFuncs,x];*)
(**)
(*Show[{ListLogLogPlot[rawData,PlotStyle->GrayLevel[0],AxesLabel->{Log[L],Log[N]}]*)
(*,ListLogLogPlot[Averagedb4AfterValidation,PlotStyle->RGBColor[0, 1, 1]]*)
(*,Plot[lmp[x],{x,0,1000},PlotStyle->{RGBColor[0, Rational[2, 3], 0],Thickness->0.004}]*)
(*,Plot[lmAveraged[x],{x,Log[thresholdBelow],Log[thresholdAbove-1]},PlotStyle->{RGBColor[1, 0, 0],Thickness->0.004}]*)
(*,Plot[lmpAveraged[x],{x,Log[thresholdBelow],Log[thresholdAbove-1]},PlotStyle->{RGBColor[0.5, 0, 0.5],Thickness->0.004}]*)
(*,Plot[x (dfSLE/.bb->b)-1,{x,1,10000},PlotStyle->{RGBColor[0, 0, 1],Dashed}]}*)
(*,(*PlotRange->All,*)PlotLabel->Row[{" b = ",b}]*)
(*(*,Epilog->{Directive[Dashed,],*)
(*Line[{{Log[thresholdBelow],0},{Log[thresholdBelow],Log[maxy]//N}}]*)
(*,Line[{{Log[thresholdAbove],0},{Log[thresholdAbove],Log[maxy]//N}}]}*)*)
(*]*)
(**)
(*Print[ "Full data - Linear fit : \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lm["ParameterTable"][[1]][[1,3,2]],\[Pi]*lm["ParameterErrors"][[2]]],"*)
(*Averaged data: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lmAveraged["ParameterTable"][[1]][[1,3,2]],\[Pi]*lmAveraged["ParameterErrors"][[2]]],"*)
(**)
(*Full data - ",Total@fitFuncs," fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lmp["ParameterTable"][[1]][[1,2,2]],\[Pi]*lmp["ParameterErrors"][[1]]],"*)
(*Averaged data with " ,Total@fitFuncs," fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lmpAveraged["ParameterTable"][[1]][[1,2,2]],\[Pi]*lmpAveraged["ParameterErrors"][[1]]],"*)
(**)
(*Result from the litterature (exact with SLE): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Subsection:: *)
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


(* ::Subsubsection::Closed:: *)
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
(*b=5 With points from the CLUSTER*)


(* ::Subsection:: *)
(*Raw data*)


(* ::Input:: *)
(*b=5;*)
(**)
(*rawData=data5=ToExpression/@Import[FileNameJoin[{"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d-FromCluster\\b5-clean_merged_data.csv"}],"CSV"]; (* MODIFY FILE NAME *)*)


(* ::Input:: *)
(*ListPlot[rawData]*)
(*ListLogLogPlot[rawData]*)


(* ::Input:: *)
(*fitFuncs=Table[x^(-i),{i,-1,3}];*)
(**)
(*lmp=LinearModelFit[Log[rawData],fitFuncs,x];*)
(**)
(*lm=LinearModelFit[Log[rawData],x,x];*)
(**)
(*Show[{ListLogLogPlot[rawData,AxesLabel->{Log[L],Log[N]}],*)
(*Plot[lm[x],{x,0,1000},PlotStyle->{Red,Thickness->0.004}]*)
(*,Plot[lmp[x],{x,0,1000},PlotStyle->{Darker@Green,Thickness->0.004}]*)
(*,Plot[x (dfSLE/.bb->b)-1,{x,1,10000},PlotStyle->{Blue,Dashed}]}*)
(*,PlotLabel->Row[{" b = ",b}]]*)
(**)
(*Print[ "Linear fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Quiet@lm["ParameterTable"][[1]][[1,3,2]]," \[PlusMinus] ",lm["ParameterErrors"][[2]],"*)
(*",Total@fitFuncs," fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Quiet@lmp["ParameterTable"][[1]][[1,2,2]]," \[PlusMinus] ",lmp["ParameterErrors"][[1]],"*)
(*Result from the litterature (exact with SLE): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Subsection:: *)
(*Drop the first few*)


(* ::Input:: *)
(*threshold=50;*)
(**)
(*dropped=droppedb5=DeleteCases[rawData,{a_,_}/;a<threshold];*)
(*lmdropped=LinearModelFit[Log[dropped],x,x];*)
(**)
(*Show[{ListLogLogPlot[rawData,PlotStyle->Black,AxesLabel->{Log[L],Log[N]}]*)
(*,ListLogLogPlot[dropped,PlotStyle->Green]*)
(*,Plot[lmdropped[x],{x,Log[threshold-1],1000},PlotStyle->{Red,Thickness->0.002}]*)
(*,Plot[x (dfSLE/.bb->b)-1,{x,1,10000},PlotStyle->{Blue,Dashed}]}*)
(*,(*PlotRange->All,*)PlotLabel->Row[{" b = ",b}]]*)
(**)
(*Print[ "Full data: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Quiet@lm["ParameterTable"][[1]][[1,3,2]]," \[PlusMinus] ",lm["ParameterErrors"][[2]],"*)
(*Dropped data: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Quiet@lmdropped["ParameterTable"][[1]][[1,3,2]]," \[PlusMinus] ",lmdropped["ParameterErrors"][[2]],"*)
(*Result from the litterature (exact with SLE): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Section::Closed:: *)
(*b=10 With points from the CLUSTER*)


(* ::Subsection:: *)
(*Raw data*)


(* ::Input:: *)
(*b=10;*)
(**)
(*rawData=data10=ToExpression/@Import[FileNameJoin[{"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d-FromCluster\\b10-clean_merged_data.csv"}],"CSV"]; (* MODIFY FILE NAME *)*)


(* ::Input:: *)
(*ListPlot[rawData]*)
(*ListLogLogPlot[rawData]*)


(* ::Input:: *)
(*fitFuncs=Table[x^(-i),{i,-1,3}];*)
(**)
(*lmp=LinearModelFit[Log[rawData],fitFuncs,x];*)
(**)
(*lm=LinearModelFit[Log[rawData],x,x];*)
(**)
(*Show[{ListLogLogPlot[rawData,AxesLabel->{Log[L],Log[N]}],*)
(*Plot[lm[x],{x,0,1000},PlotStyle->{Red,Thickness->0.004}]*)
(*,Plot[lmp[x],{x,0,1000},PlotStyle->{Darker@Green,Thickness->0.004}]*)
(*,Plot[x (dfSLE/.bb->b)-1,{x,1,10000},PlotStyle->{Blue,Dashed}]}*)
(*,PlotLabel->Row[{" b = ",b}]]*)
(**)
(*Print[ "Linear fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Quiet@lm["ParameterTable"][[1]][[1,3,2]]," \[PlusMinus] ",lm["ParameterErrors"][[2]],"*)
(*",Total@fitFuncs," fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Quiet@lmp["ParameterTable"][[1]][[1,2,2]]," \[PlusMinus] ",lmp["ParameterErrors"][[1]],"*)
(*Result from the litterature (exact with SLE): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Subsection:: *)
(*Drop the first few*)


(* ::Input:: *)
(*threshold=100;*)
(**)
(*dropped=droppedb10=DeleteCases[rawData,{a_,_}/;a<threshold];*)
(*lmdropped=LinearModelFit[Log[dropped],x,x];*)
(**)
(*Show[{ListLogLogPlot[rawData,PlotStyle->Black,AxesLabel->{Log[L],Log[N]}]*)
(*,ListLogLogPlot[dropped,PlotStyle->Green]*)
(*,Plot[lmdropped[x],{x,Log[threshold-1],1000},PlotStyle->{Red,Thickness->0.002}]*)
(*,Plot[x (dfSLE/.bb->b)-1,{x,1,10000},PlotStyle->{Blue,Dashed}]}*)
(*,(*PlotRange->All,*)PlotLabel->Row[{" b = ",b}]]*)
(**)
(*Print[ "Full data: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lm["ParameterTable"][[1]][[1,3,2]],lm["ParameterErrors"][[2]]],"*)
(*Dropped data: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lmdropped["ParameterTable"][[1]][[1,3,2]],lmdropped["ParameterErrors"][[2]]],"*)
(*Result from the litterature (exact with SLE): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Input:: *)
(*(* Not very good*)*)


(* ::Section::Closed:: *)
(*b=15 With points from the CLUSTER*)


(* ::Subsection:: *)
(*Raw data*)


(* ::Input:: *)
(*b=15;*)
(**)
(*rawData=data15=ToExpression/@Import[FileNameJoin[{"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d-FromCluster\\b15-clean_merged_data.csv"}],"CSV"]; (* MODIFY FILE NAME *)*)


(* ::Input:: *)
(*ListPlot[rawData]*)
(*ListLogLogPlot[rawData]*)


(* ::Input:: *)
(*fitFuncs=Table[x^(-i),{i,-1,3}];*)
(**)
(*lmp=LinearModelFit[Log[rawData],fitFuncs,x];*)
(**)
(*lm=LinearModelFit[Log[rawData],x,x];*)
(**)
(*Show[{ListLogLogPlot[rawData,AxesLabel->{Log[L],Log[N]}],*)
(*Plot[lm[x],{x,0,1000},PlotStyle->{Red,Thickness->0.004}]*)
(*,Plot[lmp[x],{x,0,1000},PlotStyle->{Darker@Green,Thickness->0.004}]*)
(*,Plot[x (dfSLE/.bb->b)-1,{x,1,10000},PlotStyle->{Blue,Dashed}]}*)
(*,PlotLabel->Row[{" b = ",b}]]*)
(**)
(*Print[ "Linear fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Quiet@lm["ParameterTable"][[1]][[1,3,2]]," \[PlusMinus] ",lm["ParameterErrors"][[2]],"*)
(*",Total@fitFuncs," fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Quiet@lmp["ParameterTable"][[1]][[1,2,2]]," \[PlusMinus] ",lmp["ParameterErrors"][[1]],"*)
(*Result from the litterature (exact with SLE): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Subsection:: *)
(*Drop the first few*)


(* ::Input:: *)
(*threshold=100;*)
(**)
(*dropped=droppedb15=DeleteCases[rawData,{a_,_}/;a<threshold];*)
(*lmdropped=LinearModelFit[Log[dropped],x,x];*)
(**)
(*Show[{ListLogLogPlot[rawData,PlotStyle->Black,AxesLabel->{Log[L],Log[N]}]*)
(*,ListLogLogPlot[dropped,PlotStyle->Green]*)
(*,Plot[lmdropped[x],{x,Log[threshold-1],1000},PlotStyle->{Red,Thickness->0.002}]*)
(*,Plot[x (dfSLE/.bb->b)-1,{x,1,10000},PlotStyle->{Blue,Dashed}]}*)
(*,(*PlotRange->All,*)PlotLabel->Row[{" b = ",b}]]*)
(**)
(*Print[ "Full data: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Quiet@lm["ParameterTable"][[1]][[1,3,2]]," \[PlusMinus] ",lm["ParameterErrors"][[2]],"*)
(*Dropped data: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Quiet@lmdropped["ParameterTable"][[1]][[1,3,2]]," \[PlusMinus] ",lmdropped["ParameterErrors"][[2]],"*)
(*Result from the litterature (exact with SLE): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Section::Closed:: *)
(*b=15 With points from the CLUSTER UPDATED*)


(* ::Subsection:: *)
(*Raw data*)


(* ::Input:: *)
(*b=15;*)
(**)
(*rawData=data15update=ToExpression/@Import[FileNameJoin[{"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d-FromCluster\\b15-clean_merged_data-update.csv"}],"CSV"]; (* MODIFY FILE NAME *)*)


(* ::Input:: *)
(*ListPlot[rawData]*)
(*ListLogLogPlot[rawData]*)


(* ::Input:: *)
(*fitFuncs=Table[x^(-i),{i,-1,3}];*)
(**)
(*lmp=LinearModelFit[Log[rawData],fitFuncs,x];*)
(**)
(*lm=LinearModelFit[Log[rawData],x,x];*)
(**)
(*Show[{ListLogLogPlot[rawData,AxesLabel->{Log[L],Log[N]}],*)
(*Plot[lm[x],{x,0,1000},PlotStyle->{Red,Thickness->0.004}]*)
(*,Plot[lmp[x],{x,0,1000},PlotStyle->{Darker@Green,Thickness->0.004}]*)
(*,Plot[x (dfSLE/.bb->b)-1,{x,1,10000},PlotStyle->{Blue,Dashed}]}*)
(*,PlotLabel->Row[{" b = ",b}]]*)
(**)
(*Print[ "Linear fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Quiet@lm["ParameterTable"][[1]][[1,3,2]]," \[PlusMinus] ",lm["ParameterErrors"][[2]],"*)
(*",Total@fitFuncs," fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Quiet@lmp["ParameterTable"][[1]][[1,2,2]]," \[PlusMinus] ",lmp["ParameterErrors"][[1]],"*)
(*Result from the litterature (exact with SLE): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Subsection:: *)
(*Drop the first few*)


(* ::Input:: *)
(*threshold=50;*)
(**)
(*dropped=droppedb15update=DeleteCases[rawData,{a_,_}/;a<threshold];*)
(*lmdropped=LinearModelFit[Log[dropped],x,x];*)
(**)
(*Show[{ListLogLogPlot[rawData,PlotStyle->Black,AxesLabel->{Log[L],Log[N]}]*)
(*,ListLogLogPlot[dropped,PlotStyle->Green]*)
(*,Plot[lmdropped[x],{x,Log[threshold-1],1000},PlotStyle->{Red,Thickness->0.002}]*)
(*,Plot[x (dfSLE/.bb->b)-1,{x,1,10000},PlotStyle->{Blue,Dashed}]}*)
(*,(*PlotRange->All,*)PlotLabel->Row[{" b = ",b}]]*)
(**)
(*Print[ "Full data: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Quiet@lm["ParameterTable"][[1]][[1,3,2]]," \[PlusMinus] ",lm["ParameterErrors"][[2]],"*)
(*Dropped data: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Quiet@lmdropped["ParameterTable"][[1]][[1,3,2]]," \[PlusMinus] ",lmdropped["ParameterErrors"][[2]],"*)
(*Result from the litterature (exact with SLE): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Section::Closed:: *)
(*b=15 With points from the CLUSTER NoTolleranceBound*)


(* ::Subsection:: *)
(*Raw data*)


(* ::Input:: *)
(*b=15;*)
(**)
(*rawData=data15NoTolleranceBound=ToExpression/@Import[FileNameJoin[{"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d-FromCluster\\b15-clean_merged_data-NoTolleranceBound.csv"}],"CSV"]; (* MODIFY FILE NAME *)*)


(* ::Input:: *)
(*ListPlot[rawData]*)
(*ListLogLogPlot[rawData]*)


(* ::Input:: *)
(*fitFuncs=Table[x^(-i),{i,-1,3}];*)
(**)
(*lmp=LinearModelFit[Log[rawData],fitFuncs,x];*)
(**)
(*lm=LinearModelFit[Log[rawData],x,x];*)
(**)
(*Show[{ListLogLogPlot[rawData,AxesLabel->{Log[L],Log[N]}],*)
(*Plot[lm[x],{x,0,1000},PlotStyle->{Red,Thickness->0.004}]*)
(*,Plot[lmp[x],{x,0,1000},PlotStyle->{Darker@Green,Thickness->0.004}]*)
(*,Plot[x (dfSLE/.bb->b)-1,{x,1,10000},PlotStyle->{Blue,Dashed}]}*)
(*,PlotLabel->Row[{" b = ",b}]]*)
(**)
(*Print[ "Linear fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Quiet@lm["ParameterTable"][[1]][[1,3,2]]," \[PlusMinus] ",lm["ParameterErrors"][[2]],"*)
(*",Total@fitFuncs," fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Quiet@lmp["ParameterTable"][[1]][[1,2,2]]," \[PlusMinus] ",lmp["ParameterErrors"][[1]],"*)
(*Result from the litterature (exact with SLE): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Subsection:: *)
(*Drop the first few*)


(* ::Input:: *)
(*threshold=50;*)
(**)
(*dropped=droppedb15NoTolleranceBound=DeleteCases[rawData,{a_,_}/;a<threshold];*)
(*lmdropped=LinearModelFit[Log[dropped],x,x];*)
(**)
(*Show[{ListLogLogPlot[rawData,PlotStyle->Black,AxesLabel->{Log[L],Log[N]}]*)
(*,ListLogLogPlot[dropped,PlotStyle->Green]*)
(*,Plot[lmdropped[x],{x,Log[threshold-1],1000},PlotStyle->{Red,Thickness->0.002}]*)
(*,Plot[x (dfSLE/.bb->b)-1,{x,1,10000},PlotStyle->{Blue,Dashed}]}*)
(*,(*PlotRange->All,*)PlotLabel->Row[{" b = ",b}]]*)
(**)
(*Print[ "Full data: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Quiet@lm["ParameterTable"][[1]][[1,3,2]]," \[PlusMinus] ",lm["ParameterErrors"][[2]],"*)
(*Dropped data: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Quiet@lmdropped["ParameterTable"][[1]][[1,3,2]]," \[PlusMinus] ",lmdropped["ParameterErrors"][[2]],"*)
(*Result from the litterature (exact with SLE): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Section::Closed:: *)
(*b=15 With points from the CLUSTER with doubles*)


(* ::Input:: *)
(*b=15;*)
(**)
(*rawData=data15double=DeleteDuplicates[ToExpression/@Import[FileNameJoin[{"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d-FromCluster\\b15-clean_merged_data-V7.csv"}],"CSV"]];*)
(* (* MODIFY FILE NAME *)*)


(* ::Input:: *)
(*ListPlot[rawData]*)
(*ListLogLogPlot[rawData]*)


(* ::Input:: *)
(*fitFuncs=Table[x^(-i),{i,-1,3}];*)
(*fitFuncs=Append[Table[x^(-i),{i,-1,0}],Exp[-x]];*)
(**)
(*fitFuncs={x,1,Exp[-x],Exp[-2 x]};(*Exp[-1.3 x]*)*)
(*fitFuncs={x,1,Exp[-5 x]};*)
(**)
(*lmp=LinearModelFit[Log[rawData],fitFuncs,x];*)
(**)
(*lm=LinearModelFit[Log[rawData],x,x];*)
(**)
(*Show[{ListLogLogPlot[rawData,AxesLabel->{Log[L],Log[N]},DataRange->{0,50}],*)
(*Plot[lm[x],{x,0,1000},PlotStyle->{Red,Thickness->0.004}]*)
(*,Plot[lmp[x],{x,0,1000},PlotStyle->{Darker@Green,Thickness->0.004}]*)
(*,Plot[x (dfSLE/.bb->b)-1,{x,1,10000},PlotStyle->{Blue,Dashed}]}*)
(*,PlotLabel->Row[{" b = ",b}](*,PlotRange->{{1,Log[200]},{0,Log[100]}}*)*)
(*]*)
(**)
(*Print[ "Linear fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lm["ParameterTable"][[1]][[1,3,2]],lm["ParameterErrors"][[2]]],"*)
(*",Total@fitFuncs," fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lmp["ParameterTable"][[1]][[1,2,2]],lmp["ParameterErrors"][[1]]],"*)
(*Result from the litterature (exact with SLE): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Input:: *)
(*Quiet@lmp["ParameterTable"][[1]]*)


(* ::Subsection:: *)
(*Drop the first few*)


(* ::Input:: *)
(*threshold=10;*)
(**)
(*dropped=droppedb15double=DeleteCases[rawData,{a_,_}/;a<threshold];*)
(*lmdropped=LinearModelFit[Log[dropped],x,x];*)
(**)
(*Show[{ListLogLogPlot[rawData,PlotStyle->Black,AxesLabel->{Log[L],Log[N]}]*)
(*,ListLogLogPlot[dropped,PlotStyle->RGBColor[0, 1, 0]]*)
(*,Plot[lmdropped[x],{x,Log[threshold-1],1000},PlotStyle->{Red,Thickness->0.004}]*)
(*,Plot[x (dfSLE/.bb->b)-1,{x,1,10000},PlotStyle->{Blue,Dashed}]}*)
(*,(*PlotRange->All,*)PlotLabel->Row[{" b = ",b}]]*)
(**)
(*Print[ "Full data - Linear fit : \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lm["ParameterTable"][[1]][[1,3,2]],lm["ParameterErrors"][[2]]],"*)
(*Full data - ",Total@fitFuncs," fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lmp["ParameterTable"][[1]][[1,2,2]],lmp["ParameterErrors"][[1]]],"*)
(*Dropped data: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lmdropped["ParameterTable"][[1]][[1,3,2]],lmdropped["ParameterErrors"][[2]]],"*)
(*Result from the litterature (exact with SLE): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Section::Closed:: *)
(*b=15 With PARTIAL points from the CLUSTER with LongDoubles*)


(* ::Input:: *)
(*b=15;*)
(**)
(*rawData=data15double=ToExpression/@Import[FileNameJoin[{"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d-FromCluster\\b15-clean_merged_data-PARTIALlongDoubles.csv"}],"CSV"];*)
(* (* MODIFY FILE NAME *)*)


(* ::Input:: *)
(*ListPlot[rawData]*)
(*ListLogLogPlot[rawData]*)


(* ::Input:: *)
(*fitFuncs=Table[x^(-i),{i,-1,3}];*)
(*(*fitFuncs=Append[Table[x^(-i),{i,-1,0}],Exp[-x]];*)
(**)*)
(*fitFuncs={x,1,Exp[-x],Exp[-2 x]};(*Exp[-1.3 x]*)*)
(**)
(*lmp=LinearModelFit[Log[rawData],fitFuncs,x];*)
(**)
(*lm=LinearModelFit[Log[rawData],x,x];*)
(**)
(*Show[{ListLogLogPlot[rawData,AxesLabel->{Log[L],Log[N]},DataRange->{0,50}],*)
(*Plot[lm[x],{x,0,1000},PlotStyle->{Red,Thickness->0.004}]*)
(*,Plot[lmp[x],{x,0,1000},PlotStyle->{Darker@Green,Thickness->0.004}]*)
(*,Plot[x (dfSLE/.bb->b)-1,{x,1,10000},PlotStyle->{Blue,Dashed}]}*)
(*,PlotLabel->Row[{" b = ",b}](*,PlotRange->{{1,Log[200]},{0,Log[100]}}*)*)
(*]*)
(**)
(*Print[ "Linear fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lm["ParameterTable"][[1]][[1,3,2]],lm["ParameterErrors"][[2]]],"*)
(*",Total@fitFuncs," fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lmp["ParameterTable"][[1]][[1,2,2]],lmp["ParameterErrors"][[1]]],"*)
(*Result from the litterature (exact with SLE): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Input:: *)
(*Quiet@lmp["ParameterTable"][[1]]*)


(* ::Subsection::Closed:: *)
(*Drop the first few*)


(* ::Input:: *)
(*threshold=300;*)
(**)
(*dropped=droppedb15Longdouble=DeleteCases[rawData,{a_,_}/;a<threshold];*)
(*lmdropped=LinearModelFit[Log[dropped],x,x];*)
(**)
(*Show[{ListLogLogPlot[rawData,PlotStyle->Black,AxesLabel->{Log[L],Log[N]}]*)
(*,ListLogLogPlot[dropped,PlotStyle->RGBColor[0, 1, 0]]*)
(*,Plot[lmdropped[x],{x,Log[threshold-1],1000},PlotStyle->{Red,Thickness->0.004}]*)
(*,Plot[x (dfSLE/.bb->b)-1,{x,1,10000},PlotStyle->{Blue,Dashed}]}*)
(*,(*PlotRange->All,*)PlotLabel->Row[{" b = ",b}]]*)
(**)
(*Print[ "Full data - Linear fit : \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lm["ParameterTable"][[1]][[1,3,2]],lm["ParameterErrors"][[2]]],"*)
(*Full data - ",Total@fitFuncs," fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lmp["ParameterTable"][[1]][[1,2,2]],lmp["ParameterErrors"][[1]]],"*)
(*Dropped data: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lmdropped["ParameterTable"][[1]][[1,3,2]],lmdropped["ParameterErrors"][[2]]],"*)
(*Result from the litterature (exact with SLE): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Subsection::Closed:: *)
(*Drop the last few*)


(* ::Input:: *)
(*threshold=1000;*)
(**)
(*dropped=droppedLastb15Longdouble=DeleteCases[rawData,{a_,_}/;a>threshold];*)
(**)
(*lmdropped=LinearModelFit[Log[dropped],x,x];*)
(**)
(**)
(*Show[{ListLogLogPlot[rawData,PlotStyle->Black,AxesLabel->{Log[L],Log[N]}]*)
(*,ListLogLogPlot[dropped,PlotStyle->RGBColor[0, 1, 0]]*)
(*,Plot[lmdropped[x],{x,0,Log[threshold-1]},PlotStyle->{Red,Thickness->0.004}]*)
(*,Plot[x (dfSLE/.bb->b)-1,{x,1,10000},PlotStyle->{Blue,Dashed}]}*)
(*,(*PlotRange->All,*)PlotLabel->Row[{" b = ",b}]]*)
(**)
(*Print[ "Full data - Linear fit : \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lm["ParameterTable"][[1]][[1,3,2]],lm["ParameterErrors"][[2]]],"*)
(*Full data - ",Total@fitFuncs," fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lmp["ParameterTable"][[1]][[1,2,2]],lmp["ParameterErrors"][[1]]],"*)
(*Dropped data: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lmdropped["ParameterTable"][[1]][[1,3,2]],lmdropped["ParameterErrors"][[2]]],"*)
(*Result from the litterature (exact with SLE): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Subsection:: *)
(*Drop both first and last few*)


(* ::Input:: *)
(*thresholdBelow=0;*)
(*thresholdAbove=900;*)
(**)
(*dropped=droppedLastb15Longdouble=Select[rawData,thresholdBelow<#[[1]]<thresholdAbove&];*)
(**)
(*lmdropped=LinearModelFit[Log[dropped],x,x];*)
(**)
(*lmpdropped=LinearModelFit[Log[dropped],fitFuncs,x];*)
(**)
(*Show[{ListLogLogPlot[rawData,PlotStyle->Black,AxesLabel->{Log[L],Log[N]}]*)
(*,ListLogLogPlot[dropped,PlotStyle->RGBColor[0, 1, 0]]*)
(*,Plot[lmdropped[x],{x,Log[thresholdBelow],Log[thresholdAbove-1]},PlotStyle->{Red,Thickness->0.004}]*)
(*,Plot[lmpdropped[x],{x,Log[thresholdBelow],Log[thresholdAbove-1]},PlotStyle->{Darker@Green,Thickness->0.004}]*)
(*,Plot[x (dfSLE/.bb->b)-1,{x,1,10000},PlotStyle->{Blue,Dashed}]}*)
(*,(*PlotRange->All,*)PlotLabel->Row[{" b = ",b}]]*)
(**)
(*Print[ "Full data - Linear fit : \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lm["ParameterTable"][[1]][[1,3,2]],lm["ParameterErrors"][[2]]],"*)
(*Full data - ",Total@fitFuncs," fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lmp["ParameterTable"][[1]][[1,2,2]],lmp["ParameterErrors"][[1]]],"*)
(*Dropped data: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lmdropped["ParameterTable"][[1]][[1,3,2]],lmdropped["ParameterErrors"][[2]]],"*)
(*Dropped data with " ,Total@fitFuncs," fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lmpdropped["ParameterTable"][[1]][[1,2,2]],lmpdropped["ParameterErrors"][[1]]],"*)
(*Result from the litterature (exact with SLE): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Section::Closed:: *)
(*b=15 With PARTIAL points from the CLUSTER with Padding=n/4*)


(* ::Input:: *)
(*b=15;*)
(**)
(*rawData=data15padding=ToExpression/@Import[FileNameJoin[{"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d-FromCluster\\b15-clean_merged_data-PARTIAL-padding.csv"}],"CSV"];*)
(* (* MODIFY FILE NAME *)*)


(* ::Input:: *)
(*ListPlot[rawData]*)
(*ListLogLogPlot[rawData]*)


(* ::Input:: *)
(*fitFuncs=Table[x^(-i),{i,-1,3}];*)
(*(*fitFuncs=Append[Table[x^(-i),{i,-1,0}],Exp[-x]];*)*)
(*(**)
(*fitFuncs={x,1,Exp[-x],Exp[-2 x]};(*Exp[-1.3 x]*)*)*)
(*fitFuncs={x,1,Exp[-2x]};*)
(**)
(*lmp=LinearModelFit[Log[rawData],fitFuncs,x];*)
(**)
(*lm=LinearModelFit[Log[rawData],x,x];*)
(**)
(*Show[{ListLogLogPlot[rawData,AxesLabel->{Log[L],Log[N]},DataRange->{0,50}],*)
(*Plot[lm[x],{x,0,1000},PlotStyle->{Red,Thickness->0.004}]*)
(*,Plot[lmp[x],{x,0,1000},PlotStyle->{Darker@Green,Thickness->0.004}]*)
(*,Plot[x (dfSLE/.bb->b)-1,{x,1,10000},PlotStyle->{Blue,Dashed}]}*)
(*,PlotLabel->Row[{" b = ",b}](*,PlotRange->{{1,Log[200]},{0,Log[100]}}*)*)
(*]*)
(**)
(*Print[ "Linear fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lm["ParameterTable"][[1]][[1,3,2]],\[Pi]*lm["ParameterErrors"][[2]]],"*)
(*",Total@fitFuncs," fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lmp["ParameterTable"][[1]][[1,2,2]],\[Pi]*lmp["ParameterErrors"][[1]]],"*)
(*Result from the litterature (exact with SLE): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Input:: *)
(*Quiet@lm["ParameterTable"][[1]]*)
(*Quiet@lmp["ParameterTable"][[1]]*)


(* ::Subsection:: *)
(*Drop both first and/or last few*)


(* ::Input:: *)
(*maxx=Max[rawData[[All,1]]];*)
(*maxy=Max[rawData[[All,2]]];*)
(**)
(*thresholdBelow=10;*)
(*thresholdAbove=maxx+1;*)
(**)
(*dropped=droppedLastb15padding=Select[rawData,thresholdBelow<#[[1]]<thresholdAbove&];*)
(**)
(*lmdropped=LinearModelFit[Log[dropped],x,x];*)
(**)
(*lmpdropped=LinearModelFit[Log[dropped],fitFuncs,x];*)
(**)
(*Show[{ListLogLogPlot[rawData,PlotStyle->GrayLevel[0],AxesLabel->{Log[L],Log[N]}]*)
(*,ListLogLogPlot[dropped,PlotStyle->RGBColor[0, 1, 0]]*)
(*,Plot[lmdropped[x],{x,Log[thresholdBelow],Log[thresholdAbove-1]},PlotStyle->{RGBColor[1, 0, 0],Thickness->0.004}]*)
(*,Plot[lmpdropped[x],{x,Log[thresholdBelow],Log[thresholdAbove-1]},PlotStyle->{RGBColor[0.5, 0, 0.5],Thickness->0.004}]*)
(*,Plot[x (dfSLE/.bb->b)-1,{x,1,10000},PlotStyle->{RGBColor[0, 0, 1],Dashed}]}*)
(*,(*PlotRange->All,*)PlotLabel->Row[{" b = ",b}]*)
(*,Epilog->{Directive[Dashed,GrayLevel[0.5]],*)
(*Line[{{Log[thresholdBelow],0},{Log[thresholdBelow],Log[maxy]//N}}]*)
(*,Line[{{Log[thresholdAbove],0},{Log[thresholdAbove],Log[maxy]//N}}]}*)
(*]*)
(**)
(*Print[ "Full data - Linear fit : \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lm["ParameterTable"][[1]][[1,3,2]],lm["ParameterErrors"][[2]]],"*)
(*Full data - ",Total@fitFuncs," fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lmp["ParameterTable"][[1]][[1,2,2]],lmp["ParameterErrors"][[1]]],"*)
(**)
(*Dropped data: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lmdropped["ParameterTable"][[1]][[1,3,2]],lmdropped["ParameterErrors"][[2]]],"*)
(*Dropped data with " ,Total@fitFuncs," fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lmpdropped["ParameterTable"][[1]][[1,2,2]],lmpdropped["ParameterErrors"][[1]]],"*)
(**)
(*Result from the litterature (exact with SLE): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Section::Closed:: *)
(*b=15 With ALL points from the CLUSTER with Padding=n/4*)


(* ::Input:: *)
(*b=15;*)
(**)
(*rawData=data15padding=ToExpression/@Import[FileNameJoin[{"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d-FromCluster\\b15-clean_merged_data-padding.csv"}],"CSV"];*)
(*Length[rawData]*)
(* (* MODIFY FILE NAME *)*)


(* ::Input:: *)
(*ListPlot[rawData]*)
(*ListLogLogPlot[rawData]*)


(* ::Input:: *)
(*fitFuncs=Table[x^(-i),{i,-1,3}];*)
(*(*fitFuncs=Append[Table[x^(-i),{i,-1,0}],Exp[-x]];*)*)
(*(**)
(*fitFuncs={x,1,Exp[-x],Exp[-2 x]};(*Exp[-1.3 x]*)*)*)
(*fitFuncs={x,1,Exp[-2x]};*)
(**)
(*lmp=LinearModelFit[Log[rawData],fitFuncs,x];*)
(**)
(*lm=LinearModelFit[Log[rawData],x,x];*)
(**)
(*Show[{ListLogLogPlot[rawData,AxesLabel->{Log[L],Log[N]},DataRange->{0,50}],*)
(*Plot[lm[x],{x,0,1000},PlotStyle->{Red,Thickness->0.004}]*)
(*,Plot[lmp[x],{x,0,1000},PlotStyle->{Darker@Green,Thickness->0.004}]*)
(*,Plot[x (dfSLE/.bb->b)-1,{x,1,10000},PlotStyle->{Blue,Dashed}]}*)
(*,PlotLabel->Row[{" b = ",b}](*,PlotRange->{{1,Log[200]},{0,Log[100]}}*)*)
(*]*)
(**)
(*Print[ "Linear fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lm["ParameterTable"][[1]][[1,3,2]],\[Pi]*lm["ParameterErrors"][[2]]],"*)
(*",Total@fitFuncs," fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lmp["ParameterTable"][[1]][[1,2,2]],\[Pi]*lmp["ParameterErrors"][[1]]],"*)
(*Result from the litterature (exact with SLE): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Input:: *)
(*Quiet@lm["ParameterTable"][[1]]*)
(*Quiet@lmp["ParameterTable"][[1]]*)


(* ::Subsection:: *)
(*Drop both first and/or last few*)


(* ::Input:: *)
(*maxx=Max[rawData[[All,1]]];*)
(*maxy=Max[rawData[[All,2]]];*)
(**)
(*thresholdBelow=10;*)
(*thresholdAbove=maxx+1;*)
(**)
(*dropped=droppedLastb15padding=Select[rawData,thresholdBelow<#[[1]]<thresholdAbove&];*)
(**)
(*lmdropped=LinearModelFit[Log[dropped],x,x];*)
(**)
(*lmpdropped=LinearModelFit[Log[dropped],fitFuncs,x];*)
(**)
(*Show[{ListLogLogPlot[rawData,PlotStyle->GrayLevel[0],AxesLabel->{Log[L],Log[N]}]*)
(*,ListLogLogPlot[dropped,PlotStyle->RGBColor[0, 1, 0]]*)
(*,Plot[lmdropped[x],{x,Log[thresholdBelow],Log[thresholdAbove-1]},PlotStyle->{RGBColor[1, 0, 0],Thickness->0.004}]*)
(*,Plot[lmpdropped[x],{x,Log[thresholdBelow],Log[thresholdAbove-1]},PlotStyle->{RGBColor[0.5, 0, 0.5],Thickness->0.004}]*)
(*,Plot[x (dfSLE/.bb->b)-1,{x,1,10000},PlotStyle->{RGBColor[0, 0, 1],Dashed}]}*)
(*,(*PlotRange->All,*)PlotLabel->Row[{" b = ",b}]*)
(*,Epilog->{Directive[Dashed,GrayLevel[0.5]],*)
(*Line[{{Log[thresholdBelow],0},{Log[thresholdBelow],Log[maxy]//N}}]*)
(*,Line[{{Log[thresholdAbove],0},{Log[thresholdAbove],Log[maxy]//N}}]}*)
(*]*)
(**)
(*Print[ "Full data - Linear fit : \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lm["ParameterTable"][[1]][[1,3,2]],lm["ParameterErrors"][[2]]],"*)
(*Full data - ",Total@fitFuncs," fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lmp["ParameterTable"][[1]][[1,2,2]],lmp["ParameterErrors"][[1]]],"*)
(**)
(*Dropped data: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lmdropped["ParameterTable"][[1]][[1,3,2]],lmdropped["ParameterErrors"][[2]]],"*)
(*Dropped data with " ,Total@fitFuncs," fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lmpdropped["ParameterTable"][[1]][[1,2,2]],lmpdropped["ParameterErrors"][[1]]],"*)
(**)
(*Result from the litterature (exact with SLE): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Subsection:: *)
(*Plot with different drops below*)


(* ::Input:: *)
(*dfDropped=Table[With[{*)
(*lmdropped=LinearModelFit[Log[DeleteCases[rawData,{a_,_}/;(a<i )]],x,x]},*)
(*{i,Quiet@lmdropped["ParameterTable"][[1]][[1,3,2]]}],{i,5,100}];*)


(* ::Input:: *)
(*dfDroppedMore=Table[With[{*)
(*lmdropped=LinearModelFit[Log[DeleteCases[rawData,{a_,_}/;(a<i )]],x,x]},*)
(*{i,Quiet@lmdropped["ParameterTable"][[1]][[1,3,2]]}],{i,101,200}];*)


(* ::Input:: *)
(*dfDroppedMore2=Table[With[{*)
(*lmdropped=LinearModelFit[Log[DeleteCases[rawData,{a_,_}/;(a<i )]],x,x]},*)
(*{i,Quiet@lmdropped["ParameterTable"][[1]][[1,3,2]]}],{i,201,400}];*)


(* ::Input:: *)
(*dfDroppedMore3=Table[With[{*)
(*lmdropped=LinearModelFit[Log[DeleteCases[rawData,{a_,_}/;(a<i )]],x,x]},*)
(*{i,Quiet@lmdropped["ParameterTable"][[1]][[1,3,2]]}],{i,401,800}];*)


(* ::Input:: *)
(*dfDroppedMore4=Table[With[{*)
(*lmdropped=LinearModelFit[Log[DeleteCases[rawData,{a_,_}/;(a<i )]],x,x]},*)
(*{i,Quiet@lmdropped["ParameterTable"][[1]][[1,3,2]]}],{i,801,1000}];*)


(* ::Input:: *)
(*dfTogether=Join[dfDropped,dfDroppedMore,dfDroppedMore2,dfDroppedMore3,dfDroppedMore4];*)
(*fit=LinearModelFit[dfTogether,{1/Log[x],1},x];*)
(*Quiet@fit["ParameterTable"][[1]]*)
(**)
(*Show[*)
(*{ListPlot[dfTogether,PlotRange->{All,All}]*)
(*,Plot[fit[x],{x,0,1000},PlotStyle->Red]*)
(*,Plot[dfSLE/.bb->b/1.,{x,0,1000},PlotStyle->Blue]*)
(*},PlotRange->{All,All}]*)


(* ::Subsection:: *)
(*Plot with different POSITIONS of the same WINDOW:  Changing window*)


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
(*},PlotRange->{All,{0.99,1.01}},PlotLabel->Row[{"Window size = ",#[[1]]}]]&/@windowPlots*)


(* ::Input:: *)
(*windowPlots[[1]]/.Around[a_,b_]->Around[a,1000*b]*)


(* ::Input:: *)
(*synchronizedPlots=Map[Show[#,PlotRange->{All,{0.99,1.01}},ImageSize->220]&,showWindowPlots];*)
(**)
(*Multicolumn[synchronizedPlots,4,Appearance->"Framed"]*)


(* ::Input:: *)
(*Map[Show[#,PlotRange->{All,{1,1.005}},ImageSize->280]&,showWindowPlots[[6;;8]]];*)
(*Multicolumn[%,3,Appearance->"Framed"]*)


(* ::Section::Closed:: *)
(*b=15 With All points from the CLUSTER with NO Padding *)


(* ::Input:: *)
(*b=15;*)
(**)
(*rawData=data15NOpadding=ToExpression/@Import[FileNameJoin[{"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d-FromCluster\\b15-clean_merged_data-NOpadding.csv"}],"CSV"];*)
(* (* MODIFY FILE NAME *)*)
(*Length[rawData]*)


(* ::Input:: *)
(*ListPlot[rawData]*)
(*ListLogLogPlot[rawData]*)


(* ::Input:: *)
(*fitFuncs=Table[x^(-i),{i,-1,3}];*)
(*(*fitFuncs=Append[Table[x^(-i),{i,-1,0}],Exp[-x]];*)*)
(*(**)
(*fitFuncs={x,1,Exp[-x],Exp[-2 x]};(*Exp[-1.3 x]*)*)*)
(*fitFuncs={x,1,Exp[-2x]};*)
(**)
(*lmp=LinearModelFit[Log[rawData],fitFuncs,x];*)
(**)
(*lm=LinearModelFit[Log[rawData],x,x];*)
(**)
(*Show[{ListLogLogPlot[rawData,AxesLabel->{Log[L],Log[N]},DataRange->{0,50}],*)
(*Plot[lm[x],{x,0,1000},PlotStyle->{Red,Thickness->0.004}]*)
(*,Plot[lmp[x],{x,0,1000},PlotStyle->{Darker@Green,Thickness->0.004}]*)
(*,Plot[x (dfSLE/.bb->b)-1,{x,1,10000},PlotStyle->{Blue,Dashed}]}*)
(*,PlotLabel->Row[{" b = ",b}](*,PlotRange->{{1,Log[200]},{0,Log[100]}}*)*)
(*]*)
(**)
(*Print[ "Linear fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lm["ParameterTable"][[1]][[1,3,2]],\[Pi]*lm["ParameterErrors"][[2]]],"*)
(*",Total@fitFuncs," fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lmp["ParameterTable"][[1]][[1,2,2]],\[Pi]*lmp["ParameterErrors"][[1]]],"*)
(*Result from the litterature (exact with SLE): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Input:: *)
(*Quiet@lm["ParameterTable"][[1]]*)
(*Quiet@lmp["ParameterTable"][[1]]*)


(* ::Subsection:: *)
(*Drop both first and/or last few*)


(* ::Input:: *)
(*maxx=Max[rawData[[All,1]]];*)
(*maxy=Max[rawData[[All,2]]];*)
(**)
(*thresholdBelow=10;*)
(*thresholdAbove=maxx+1;*)
(**)
(*dropped=droppedb15NOpadding=Select[rawData,thresholdBelow<#[[1]]<thresholdAbove&];*)
(**)
(*lmdropped=LinearModelFit[Log[dropped],x,x];*)
(**)
(*lmpdropped=LinearModelFit[Log[dropped],fitFuncs,x];*)
(**)
(*Show[{ListLogLogPlot[rawData,PlotStyle->GrayLevel[0],AxesLabel->{Log[L],Log[N]}]*)
(*,ListLogLogPlot[dropped,PlotStyle->RGBColor[0, 1, 0]]*)
(*,Plot[lmdropped[x],{x,Log[thresholdBelow],Log[thresholdAbove-1]},PlotStyle->{RGBColor[1, 0, 0],Thickness->0.004}]*)
(*,Plot[lmpdropped[x],{x,Log[thresholdBelow],Log[thresholdAbove-1]},PlotStyle->{RGBColor[0.5, 0, 0.5],Thickness->0.004}]*)
(*,Plot[x (dfSLE/.bb->b)-1,{x,1,10000},PlotStyle->{RGBColor[0, 0, 1],Dashed}]}*)
(*,(*PlotRange->All,*)PlotLabel->Row[{" b = ",b}]*)
(*,Epilog->{Directive[Dashed,GrayLevel[0.5]],*)
(*Line[{{Log[thresholdBelow],0},{Log[thresholdBelow],Log[maxy]//N}}]*)
(*,Line[{{Log[thresholdAbove],0},{Log[thresholdAbove],Log[maxy]//N}}]}*)
(*]*)
(**)
(*Print[ "Full data - Linear fit : \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lm["ParameterTable"][[1]][[1,3,2]],lm["ParameterErrors"][[2]]],"*)
(*Full data - ",Total@fitFuncs," fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lmp["ParameterTable"][[1]][[1,2,2]],lmp["ParameterErrors"][[1]]],"*)
(**)
(*Dropped data: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lmdropped["ParameterTable"][[1]][[1,3,2]],lmdropped["ParameterErrors"][[2]]],"*)
(*Dropped data with " ,Total@fitFuncs," fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lmpdropped["ParameterTable"][[1]][[1,2,2]],lmpdropped["ParameterErrors"][[1]]],"*)
(**)
(*Result from the litterature (exact with SLE): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Section::Closed:: *)
(*b15-clean_merged_data-NOpadding-ExtraStat.csv*)


(* ::Input:: *)
(*b=15;*)
(**)
(*data15NOpaddingExtra=ToExpression/@Import[FileNameJoin[{"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d-FromCluster\\b15-clean_merged_data-NOpadding-ExtraStat.csv"}],"CSV"];*)
(* (* MODIFY FILE NAME *)*)
(**)
(**)
(*rawData=Join[data15NOpadding,data15NOpaddingExtra];*)
(*Length[rawData]*)


(* ::Input:: *)
(*ListPlot[rawData]*)
(*ListLogLogPlot[rawData]*)


(* ::Input:: *)
(*fitFuncs=Table[x^(-i),{i,-1,3}];*)
(*(*fitFuncs=Append[Table[x^(-i),{i,-1,0}],Exp[-x]];*)*)
(*(**)
(*fitFuncs={x,1,Exp[-x],Exp[-2 x]};(*Exp[-1.3 x]*)*)*)
(*fitFuncs={x,1,Exp[-2x]};*)
(**)
(*lmp=LinearModelFit[Log[rawData],fitFuncs,x];*)
(**)
(*lm=LinearModelFit[Log[rawData],x,x];*)
(**)
(*Show[{ListLogLogPlot[rawData,AxesLabel->{Log[L],Log[N]},DataRange->{0,50}],*)
(*Plot[lm[x],{x,0,1000},PlotStyle->{Red,Thickness->0.004}]*)
(*,Plot[lmp[x],{x,0,1000},PlotStyle->{Darker@Green,Thickness->0.004}]*)
(*,Plot[x (dfSLE/.bb->b)-1,{x,1,10000},PlotStyle->{Blue,Dashed}]}*)
(*,PlotLabel->Row[{" b = ",b}](*,PlotRange->{{1,Log[200]},{0,Log[100]}}*)*)
(*]*)
(**)
(*Print[ "Linear fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lm["ParameterTable"][[1]][[1,3,2]],\[Pi]*lm["ParameterErrors"][[2]]],"*)
(*",Total@fitFuncs," fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lmp["ParameterTable"][[1]][[1,2,2]],\[Pi]*lmp["ParameterErrors"][[1]]],"*)
(*Result from the litterature (exact with SLE): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Input:: *)
(*Quiet@lm["ParameterTable"][[1]]*)
(*Quiet@lmp["ParameterTable"][[1]]*)


(* ::Subsection:: *)
(*Drop both first and/or last few*)


(* ::Input:: *)
(*maxx=Max[rawData[[All,1]]];*)
(*maxy=Max[rawData[[All,2]]];*)
(**)
(*thresholdBelow=10;*)
(*thresholdAbove=maxx+1;*)
(**)
(*dropped=droppedb15NOpadding=Select[rawData,thresholdBelow<#[[1]]<thresholdAbove&];*)
(**)
(*lmdropped=LinearModelFit[Log[dropped],x,x];*)
(**)
(*lmpdropped=LinearModelFit[Log[dropped],fitFuncs,x];*)
(**)
(*Show[{ListLogLogPlot[rawData,PlotStyle->GrayLevel[0],AxesLabel->{Log[L],Log[N]}]*)
(*,ListLogLogPlot[dropped,PlotStyle->RGBColor[0, 1, 0]]*)
(*,Plot[lmdropped[x],{x,Log[thresholdBelow],Log[thresholdAbove-1]},PlotStyle->{RGBColor[1, 0, 0],Thickness->0.004}]*)
(*,Plot[lmpdropped[x],{x,Log[thresholdBelow],Log[thresholdAbove-1]},PlotStyle->{RGBColor[0.5, 0, 0.5],Thickness->0.004}]*)
(*,Plot[x (dfSLE/.bb->b)-1,{x,1,10000},PlotStyle->{RGBColor[0, 0, 1],Dashed}]}*)
(*,(*PlotRange->All,*)PlotLabel->Row[{" b = ",b}]*)
(*,Epilog->{Directive[Dashed,GrayLevel[0.5]],*)
(*Line[{{Log[thresholdBelow],0},{Log[thresholdBelow],Log[maxy]//N}}]*)
(*,Line[{{Log[thresholdAbove],0},{Log[thresholdAbove],Log[maxy]//N}}]}*)
(*]*)
(**)
(*Print[ "Full data - Linear fit : \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lm["ParameterTable"][[1]][[1,3,2]],lm["ParameterErrors"][[2]]],"*)
(*Full data - ",Total@fitFuncs," fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lmp["ParameterTable"][[1]][[1,2,2]],lmp["ParameterErrors"][[1]]],"*)
(**)
(*Dropped data: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lmdropped["ParameterTable"][[1]][[1,3,2]],lmdropped["ParameterErrors"][[2]]],"*)
(*Dropped data with " ,Total@fitFuncs," fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lmpdropped["ParameterTable"][[1]][[1,2,2]],lmpdropped["ParameterErrors"][[1]]],"*)
(**)
(*Result from the litterature (exact with SLE): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Section::Closed:: *)
(*b=15 With points from the CLUSTER with Square BC with Log (+extra analysis)*)


(* ::Input:: *)
(*b=15;*)
(**)
(*rawData=data15square=ToExpression/@Import[FileNameJoin[{"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d-FromCluster\\b15-clean_merged_data-Square.csv"}],"CSV"];*)
(* (* MODIFY FILE NAME *)*)
(*Length[rawData]*)


(* ::Input:: *)
(*ListPlot[rawData]*)
(*ListLogLogPlot[rawData]*)


(* ::Input:: *)
(*fitFuncs=Table[x^(-i),{i,-1,3}];*)
(*(*fitFuncs=Append[Table[x^(-i),{i,-1,0}],Exp[-x]];*)*)
(*(**)
(*fitFuncs={x,1,Exp[-x],Exp[-2 x]};(*Exp[-1.3 x]*)*)*)
(*fitFuncs={x,1,Exp[-2x]};*)
(**)
(*lmp=LinearModelFit[Log[rawData],fitFuncs,x];*)
(**)
(*lm=LinearModelFit[Log[rawData],x,x];*)
(**)
(*Show[{ListLogLogPlot[rawData,AxesLabel->{Log[L],Log[N]},DataRange->{0,50}],*)
(*Plot[lm[x],{x,0,1000},PlotStyle->{RGBColor[1, 0, 0],Thickness->0.004}]*)
(**)
(*,Plot[lmp[x],{x,0,1000},PlotStyle->{RGBColor[0, Rational[2, 3], 0],Thickness->0.004}]*)
(*,Plot[x (dfSLE/.bb->b)-1,{x,1,10000},PlotStyle->{RGBColor[0, 0, 1],Dashed}]}*)
(*,PlotLabel->Row[{" b = ",b}](*,PlotRange->{{1,Log[200]},{0,Log[100]}}*)*)
(*]*)
(**)
(*Print[ "Linear fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lm["ParameterTable"][[1]][[1,3,2]],\[Pi]*lm["ParameterErrors"][[2]]],"*)
(*",Total@fitFuncs," fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lmp["ParameterTable"][[1]][[1,2,2]],\[Pi]*lmp["ParameterErrors"][[1]]],"*)
(*Result from the litterature (exact with SLE): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Input:: *)
(*Quiet@lm["ParameterTable"][[1]]*)
(*Quiet@lmp["ParameterTable"][[1]]*)


(* ::Input:: *)
(*Plot[{1.0255534742660863`*x,-23.148793782225646`*E^(-2 x)},{x,0,50},PlotRange->{{1.8,2},All}]*)


(* ::Input:: *)
(*-1/2 Log[1.0255534742660863`/(-2 *(-23.148793782225646`))]*)


(* ::Input:: *)
(*With[{x=44},*)
(*{1.0255534742660863`*x,-23.148793782225646`*E^(-2 x)}]*)


(* ::Subsection::Closed:: *)
(*Drop both first and/or last few*)


(* ::Input:: *)
(*maxx=Max[rawData[[All,1]]];*)
(*maxy=Max[rawData[[All,2]]];*)
(**)
(*thresholdBelow=44;*)
(*thresholdAbove=maxx-0;*)
(**)
(*dropped=droppedb15Square=Select[rawData,thresholdBelow<#[[1]]<thresholdAbove&];*)
(**)
(*lmdropped=LinearModelFit[Log[dropped],x,x];*)
(**)
(*lmpdropped=LinearModelFit[Log[dropped],fitFuncs,x];*)
(**)
(*Show[{ListLogLogPlot[rawData,PlotStyle->GrayLevel[0],AxesLabel->{Log[L],Log[N]}]*)
(*,ListLogLogPlot[dropped,PlotStyle->RGBColor[0, 1, 0]]*)
(*,Plot[lmp[x],{x,0,1000},PlotStyle->{RGBColor[0, Rational[2, 3], 0],Thickness->0.004}]*)
(*,Plot[lmdropped[x],{x,Log[thresholdBelow],Log[thresholdAbove-1]},PlotStyle->{RGBColor[1, 0, 0],Thickness->0.004}]*)
(*,Plot[lmpdropped[x],{x,Log[thresholdBelow],Log[thresholdAbove-1]},PlotStyle->{RGBColor[0.5, 0, 0.5],Thickness->0.004}]*)
(*,Plot[x (dfSLE/.bb->b)-1,{x,1,10000},PlotStyle->{RGBColor[0, 0, 1],Dashed}]}*)
(*,(*PlotRange->All,*)PlotLabel->Row[{" b = ",b}]*)
(*,Epilog->{Directive[Dashed,GrayLevel[0.5]],*)
(*Line[{{Log[thresholdBelow],0},{Log[thresholdBelow],Log[maxy]//N}}]*)
(*,Line[{{Log[thresholdAbove],0},{Log[thresholdAbove],Log[maxy]//N}}]}*)
(*]*)
(**)
(*Print[ "Full data - Linear fit : \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lm["ParameterTable"][[1]][[1,3,2]],\[Pi]*lm["ParameterErrors"][[2]]],"*)
(*Dropped data: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lmdropped["ParameterTable"][[1]][[1,3,2]],\[Pi]*lmdropped["ParameterErrors"][[2]]],"*)
(**)
(*Full data - ",Total@fitFuncs," fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lmp["ParameterTable"][[1]][[1,2,2]],\[Pi]*lmp["ParameterErrors"][[1]]],"*)
(*Dropped data with " ,Total@fitFuncs," fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lmpdropped["ParameterTable"][[1]][[1,2,2]],\[Pi]*lmpdropped["ParameterErrors"][[1]]],"*)
(**)
(*Result from the litterature (exact with SLE): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Subsection:: *)
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
(*Show[*)
(*{ListPlot[dfTogether,PlotRange->{All,All}]*)
(*(*,Plot[fit[x],{x,0,1000},PlotStyle->Red]*)*)
(*,Plot[dfSLE/.bb->b/1.,{x,0,1000},PlotStyle->Blue]*)
(*}*)
(*,AxesLabel->{"Dropping threshold",Subscript[d, f]}]*)


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
(*b15-clean_merged_data-LogBC.csv*)


(* ::Input:: *)
(*b=15;*)
(**)
(*rawData=data15LogBC=ToExpression/@Import[FileNameJoin[{"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d-FromCluster\\b15-clean_merged_data-LogBC.csv"}],"CSV"];*)
(* (* MODIFY FILE NAME *)*)
(**)
(**)
(**)
(*Length[rawData]*)


(* ::Input:: *)
(*ListPlot[rawData]*)
(*ListLogLogPlot[rawData]*)


(* ::Input:: *)
(*fitFuncs=Table[x^(-i),{i,-1,3}];*)
(*(*fitFuncs=Append[Table[x^(-i),{i,-1,0}],Exp[-x]];*)*)
(*(**)
(*fitFuncs={x,1,Exp[-x],Exp[-2 x]};(*Exp[-1.3 x]*)*)*)
(*fitFuncs={x,1,Exp[-2x]};*)
(**)
(*lmp=LinearModelFit[Log[rawData],fitFuncs,x];*)
(**)
(*lm=LinearModelFit[Log[rawData],x,x];*)
(**)
(*Show[{ListLogLogPlot[rawData,AxesLabel->{Log[L],Log[N]},DataRange->{0,50}],*)
(*Plot[lm[x],{x,0,1000},PlotStyle->{Red,Thickness->0.004}]*)
(*,Plot[lmp[x],{x,0,1000},PlotStyle->{Darker@Green,Thickness->0.004}]*)
(*,Plot[x (dfSLE/.bb->b)-1,{x,1,10000},PlotStyle->{Blue,Dashed}]}*)
(*,PlotLabel->Row[{" b = ",b}](*,PlotRange->{{1,Log[200]},{0,Log[100]}}*)*)
(*]*)
(**)
(*Print[ "Linear fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lm["ParameterTable"][[1]][[1,3,2]],\[Pi]*lm["ParameterErrors"][[2]]],"*)
(*",Total@fitFuncs," fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lmp["ParameterTable"][[1]][[1,2,2]],\[Pi]*lmp["ParameterErrors"][[1]]],"*)
(*Result from the litterature (exact with SLE): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Input:: *)
(*Quiet@lm["ParameterTable"][[1]]*)
(*Quiet@lmp["ParameterTable"][[1]]*)


(* ::Subsection:: *)
(*Drop both first and/or last few*)


(* ::Input:: *)
(*maxx=Max[rawData[[All,1]]];*)
(*maxy=Max[rawData[[All,2]]];*)
(**)
(*thresholdBelow=10;*)
(*thresholdAbove=maxx+1;*)
(**)
(*dropped=droppedb15NOpadding=Select[rawData,thresholdBelow<#[[1]]<thresholdAbove&];*)
(**)
(*lmdropped=LinearModelFit[Log[dropped],x,x];*)
(**)
(*lmpdropped=LinearModelFit[Log[dropped],fitFuncs,x];*)
(**)
(*Show[{ListLogLogPlot[rawData,PlotStyle->GrayLevel[0],AxesLabel->{Log[L],Log[N]}]*)
(*,ListLogLogPlot[dropped,PlotStyle->RGBColor[0, 1, 0]]*)
(*,Plot[lmdropped[x],{x,Log[thresholdBelow],Log[thresholdAbove-1]},PlotStyle->{RGBColor[1, 0, 0],Thickness->0.004}]*)
(*,Plot[lmpdropped[x],{x,Log[thresholdBelow],Log[thresholdAbove-1]},PlotStyle->{RGBColor[0.5, 0, 0.5],Thickness->0.004}]*)
(*,Plot[x (dfSLE/.bb->b)-1,{x,1,10000},PlotStyle->{RGBColor[0, 0, 1],Dashed}]}*)
(*,(*PlotRange->All,*)PlotLabel->Row[{" b = ",b}]*)
(*,Epilog->{Directive[Dashed,GrayLevel[0.5]],*)
(*Line[{{Log[thresholdBelow],0},{Log[thresholdBelow],Log[maxy]//N}}]*)
(*,Line[{{Log[thresholdAbove],0},{Log[thresholdAbove],Log[maxy]//N}}]}*)
(*]*)
(**)
(*Print[ "Full data - Linear fit : \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lm["ParameterTable"][[1]][[1,3,2]],lm["ParameterErrors"][[2]]],"*)
(*Full data - ",Total@fitFuncs," fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lmp["ParameterTable"][[1]][[1,2,2]],lmp["ParameterErrors"][[1]]],"*)
(**)
(*Dropped data: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lmdropped["ParameterTable"][[1]][[1,3,2]],lmdropped["ParameterErrors"][[2]]],"*)
(*Dropped data with " ,Total@fitFuncs," fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lmpdropped["ParameterTable"][[1]][[1,2,2]],lmpdropped["ParameterErrors"][[1]]],"*)
(**)
(*Result from the litterature (exact with SLE): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Section::Closed:: *)
(*b15-clean_merged_data-LogBC-e-10.csv (to be done)*)


(* ::Input:: *)
(*b=15;*)
(**)
(*rawData=data15LogBCe10=ToExpression/@Import[FileNameJoin[{"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d-FromCluster\\b15-clean_merged_data-LogBC-e-10.csv"}],"CSV"];*)
(* (* MODIFY FILE NAME *)*)
(**)
(**)
(*Length[rawData]*)


(* ::Input:: *)
(*ListPlot[rawData]*)
(*ListLogLogPlot[rawData]*)


(* ::Input:: *)
(*fitFuncs=Table[x^(-i),{i,-1,3}];*)
(*(*fitFuncs=Append[Table[x^(-i),{i,-1,0}],Exp[-x]];*)*)
(*(**)
(*fitFuncs={x,1,Exp[-x],Exp[-2 x]};(*Exp[-1.3 x]*)*)*)
(*fitFuncs={x,1,Exp[-2x]};*)
(**)
(*lmp=LinearModelFit[Log[rawData],fitFuncs,x];*)
(**)
(*lm=LinearModelFit[Log[rawData],x,x];*)
(**)
(*fitFunc=a+c Exp[-\[Omega] x]+df x;*)
(*fitSol=FindFit[Log[rawData],{fitFunc,{-2<a<1.1,0<\[Omega]<3}},{a,c,\[Omega],df},x];*)
(**)
(*Show[{ListLogLogPlot[rawData,AxesLabel->{Log[L],Log[N]},DataRange->{0,50}],*)
(*Plot[lm[x],{x,0,1000},PlotStyle->{RGBColor[1, 0, 0],Thickness->0.004}]*)
(**)
(*,Plot[lmp[x],{x,0,1000},PlotStyle->{RGBColor[0, Rational[2, 3], 0],Thickness->0.004}]*)
(*,Plot[fitFunc/.fitSol,{x,0,1000},PlotStyle->{RGBColor[1, Rational[2, 3], 1],Thickness->0.004}]*)
(**)
(*,Plot[x (dfSLE/.bb->b)-1,{x,1,10000},PlotStyle->{RGBColor[0, 0, 1],Dashed}]}*)
(*,PlotLabel->Row[{" b = ",b}](*,PlotRange->{{1,Log[200]},{0,Log[100]}}*)*)
(*]*)
(**)
(*Print[ "Linear fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lm["ParameterTable"][[1]][[1,3,2]],\[Pi]*lm["ParameterErrors"][[2]]],"*)
(*",Total@fitFuncs," fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lmp["ParameterTable"][[1]][[1,2,2]],\[Pi]*lmp["ParameterErrors"][[1]]],"*)
(*",fitFunc," fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[df/.fitSol,Null],"*)
(*Result from the litterature (exact with SLE): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Input:: *)
(*Quiet@lm["ParameterTable"][[1]]*)
(*Quiet@lmp["ParameterTable"][[1]]*)
(*fitSol*)


(* ::Subsection:: *)
(*Drop both first and/or last few*)


(* ::Input:: *)
(*maxx=Max[rawData[[All,1]]];*)
(*maxy=Max[rawData[[All,2]]];*)
(**)
(*thresholdBelow=10;*)
(*thresholdAbove=maxx+1;*)
(**)
(*dropped=droppedb15NOpadding=Select[rawData,thresholdBelow<#[[1]]<thresholdAbove&];*)
(**)
(*lmdropped=LinearModelFit[Log[dropped],x,x];*)
(**)
(*lmpdropped=LinearModelFit[Log[dropped],fitFuncs,x];*)
(**)
(*Show[{ListLogLogPlot[rawData,PlotStyle->GrayLevel[0],AxesLabel->{Log[L],Log[N]}]*)
(*,ListLogLogPlot[dropped,PlotStyle->RGBColor[0, 1, 0]]*)
(*,Plot[lmdropped[x],{x,Log[thresholdBelow],Log[thresholdAbove-1]},PlotStyle->{RGBColor[1, 0, 0],Thickness->0.004}]*)
(*,Plot[lmpdropped[x],{x,Log[thresholdBelow],Log[thresholdAbove-1]},PlotStyle->{RGBColor[0.5, 0, 0.5],Thickness->0.004}]*)
(*,Plot[x (dfSLE/.bb->b)-1,{x,1,10000},PlotStyle->{RGBColor[0, 0, 1],Dashed}]}*)
(*,(*PlotRange->All,*)PlotLabel->Row[{" b = ",b}]*)
(*,Epilog->{Directive[Dashed,GrayLevel[0.5]],*)
(*Line[{{Log[thresholdBelow],0},{Log[thresholdBelow],Log[maxy]//N}}]*)
(*,Line[{{Log[thresholdAbove],0},{Log[thresholdAbove],Log[maxy]//N}}]}*)
(*]*)
(**)
(*Print[ "Full data - Linear fit : \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lm["ParameterTable"][[1]][[1,3,2]],lm["ParameterErrors"][[2]]],"*)
(*Full data - ",Total@fitFuncs," fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lmp["ParameterTable"][[1]][[1,2,2]],lmp["ParameterErrors"][[1]]],"*)
(**)
(*Dropped data: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lmdropped["ParameterTable"][[1]][[1,3,2]],lmdropped["ParameterErrors"][[2]]],"*)
(*Dropped data with " ,Total@fitFuncs," fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lmpdropped["ParameterTable"][[1]][[1,2,2]],lmpdropped["ParameterErrors"][[1]]],"*)
(**)
(*Result from the litterature (exact with SLE): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Section::Closed:: *)
(*b=100 With points from the CLUSTER*)


(* ::Subsection:: *)
(*Raw data*)


(* ::Input:: *)
(*b=100;*)
(**)
(*rawData=data100=ToExpression/@Import[FileNameJoin[{"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d-FromCluster\\b100-clean_merged_data.csv"}],"CSV"]; (* MODIFY FILE NAME *)*)


(* ::Input:: *)
(*ListPlot[rawData]*)
(*ListLogLogPlot[rawData]*)


(* ::Input:: *)
(*fitFuncs=Table[x^(-i),{i,-1,3}];*)
(**)
(*lmp=LinearModelFit[Log[rawData],fitFuncs,x];*)
(**)
(*lm=LinearModelFit[Log[rawData],x,x];*)
(**)
(*Show[{ListLogLogPlot[rawData,AxesLabel->{Log[L],Log[N]}],*)
(*Plot[lm[x],{x,0,1000},PlotStyle->{Red,Thickness->0.004}]*)
(*,Plot[lmp[x],{x,0,1000},PlotStyle->{Darker@Green,Thickness->0.004}]*)
(*,Plot[x (dfSLE/.bb->b)-1,{x,1,10000},PlotStyle->{Blue,Dashed}]}*)
(*,PlotLabel->Row[{" b = ",b}]]*)
(**)
(*Print[ "Linear fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Quiet@lm["ParameterTable"][[1]][[1,3,2]]," \[PlusMinus] ",lm["ParameterErrors"][[2]],"*)
(*",Total@fitFuncs," fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Quiet@lmp["ParameterTable"][[1]][[1,2,2]]," \[PlusMinus] ",lmp["ParameterErrors"][[1]],"*)
(*Result from the litterature (exact with SLE): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Subsection:: *)
(*Drop the first few*)


(* ::Input:: *)
(*threshold=100;*)
(**)
(*dropped=droppedb100=DeleteCases[rawData,{a_,_}/;a<threshold];*)
(*lmdropped=LinearModelFit[Log[dropped],x,x];*)
(**)
(*Show[{ListLogLogPlot[rawData,PlotStyle->Black,AxesLabel->{Log[L],Log[N]}]*)
(*,ListLogLogPlot[dropped,PlotStyle->Green]*)
(*,Plot[lmdropped[x],{x,Log[threshold-1],1000},PlotStyle->{Red,Thickness->0.002}]*)
(*,Plot[x (dfSLE/.bb->b)-1,{x,1,10000},PlotStyle->{Blue,Dashed}]}*)
(*,(*PlotRange->All,*)PlotLabel->Row[{" b = ",b}]]*)
(**)
(*Print[ "Full data: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Quiet@lm["ParameterTable"][[1]][[1,3,2]]," \[PlusMinus] ",lm["ParameterErrors"][[2]],"*)
(*Dropped data: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Quiet@lmdropped["ParameterTable"][[1]][[1,3,2]]," \[PlusMinus] ",lmdropped["ParameterErrors"][[2]],"*)
(*Result from the litterature (exact with SLE): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Section::Closed:: *)
(*b=100 With points from the CLUSTER NoTolleranceBound*)


(* ::Subsection:: *)
(*Raw data*)


(* ::Input:: *)
(*b=100;*)
(**)
(*rawData=data100NoTolleranceBound=ToExpression/@Import[FileNameJoin[{"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d-FromCluster\\b100-clean_merged_data-NoTolleranceBound.csv"}],"CSV"]; (* MODIFY FILE NAME *)*)


(* ::Input:: *)
(*ListPlot[rawData]*)
(*ListLogLogPlot[rawData]*)


(* ::Input:: *)
(*fitFuncs=Table[x^(-i),{i,-1,3}];*)
(**)
(*lmp=LinearModelFit[Log[rawData],fitFuncs,x];*)
(**)
(*lm=LinearModelFit[Log[rawData],x,x];*)
(**)
(*Show[{ListLogLogPlot[rawData,AxesLabel->{Log[L],Log[N]}],*)
(*Plot[lm[x],{x,0,1000},PlotStyle->{Red,Thickness->0.004}]*)
(*,Plot[lmp[x],{x,0,1000},PlotStyle->{Darker@Green,Thickness->0.004}]*)
(*,Plot[x (dfSLE/.bb->b)-1,{x,1,10000},PlotStyle->{Blue,Dashed}]}*)
(*,PlotLabel->Row[{" b = ",b}]]*)
(**)
(*Print[ "Linear fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Quiet@lm["ParameterTable"][[1]][[1,3,2]]," \[PlusMinus] ",lm["ParameterErrors"][[2]],"*)
(*",Total@fitFuncs," fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Quiet@lmp["ParameterTable"][[1]][[1,2,2]]," \[PlusMinus] ",lmp["ParameterErrors"][[1]],"*)
(*Result from the litterature (exact with SLE): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Subsection:: *)
(*Drop the first few*)


(* ::Input:: *)
(*threshold=70;*)
(**)
(*dropped=droppedb100NoTolleranceBound=DeleteCases[rawData,{a_,_}/;a<threshold];*)
(*lmdropped=LinearModelFit[Log[dropped],x,x];*)
(**)
(*Show[{ListLogLogPlot[rawData,PlotStyle->Black,AxesLabel->{Log[L],Log[N]}]*)
(*,ListLogLogPlot[dropped,PlotStyle->Green]*)
(*,Plot[lmdropped[x],{x,Log[threshold-1],1000},PlotStyle->{Red,Thickness->0.002}]*)
(*,Plot[x (dfSLE/.bb->b)-1,{x,1,10000},PlotStyle->{Blue,Dashed}]}*)
(*,(*PlotRange->All,*)PlotLabel->Row[{" b = ",b}]]*)
(**)
(*Print[ "Full data: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Quiet@lm["ParameterTable"][[1]][[1,3,2]]," \[PlusMinus] ",lm["ParameterErrors"][[2]],"*)
(*Dropped data: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Quiet@lmdropped["ParameterTable"][[1]][[1,3,2]]," \[PlusMinus] ",lmdropped["ParameterErrors"][[2]],"*)
(*Result from the litterature (exact with SLE): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Section::Closed:: *)
(*b=100 With points from the CLUSTER V7 (+ deeper analysis)*)


(* ::Subsection::Closed:: *)
(*Raw data*)


(* ::Input:: *)
(*b=100;*)
(**)
(*rawData=data100V7=ToExpression/@Import[FileNameJoin[{"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d-FromCluster\\b100-clean_merged_data-V7.csv"}],"CSV"]; (* MODIFY FILE NAME *)*)


(* ::Input:: *)
(*ListPlot[rawData]*)
(*ListLogLogPlot[rawData]*)


(* ::Input:: *)
(*fitFuncs=Table[x^(-i),{i,-1,3}];*)
(*fitFuncs={x,1,Exp[-x]};*)
(*fitFuncs2=a+df x + c Exp[- x];*)
(**)
(*fitSol=FindFit[Log[rawData],fitFuncs2,{a,c,df},x]*)
(**)
(*lmp=LinearModelFit[Log[rawData],fitFuncs,x];*)
(**)
(*lm=LinearModelFit[Log[rawData],x,x];*)
(**)
(*Show[{ListLogLogPlot[rawData,AxesLabel->{Log[L],Log[N]}](*,*)
(*Plot[lm[x],{x,0,1000},PlotStyle->{Red,Thickness->0.004}]*)
(*,Plot[lmp[x],{x,0,1000},PlotStyle->{Darker@Green,Thickness->0.004}]*)*)
(*,Plot[fitFuncs2/.fitSol,{x,0,1000},PlotStyle->{Purple,Thickness->0.004}]*)
(*,Plot[x (dfSLE/.bb->b)-1,{x,1,10000},PlotStyle->{Blue,Dashed}]}*)
(*,PlotLabel->Row[{" b = ",b}]]*)
(**)
(*Print[ "Linear fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Quiet@lm["ParameterTable"][[1]][[1,3,2]]," \[PlusMinus] ",lm["ParameterErrors"][[2]],"*)
(*",Total@fitFuncs," fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Quiet@lmp["ParameterTable"][[1]][[1,2,2]]," \[PlusMinus] ",lmp["ParameterErrors"][[1]],"*)
(*",fitFuncs2," fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",df/.fitSol," \[PlusMinus] ??*)
(*Result from the litterature (exact with SLE): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Subsection::Closed:: *)
(*Drop the first few*)


(* ::Input:: *)
(*threshold=70;*)
(**)
(*dropped=droppedb100V7=DeleteCases[rawData,{a_,_}/;a<threshold];*)
(*lmdropped=LinearModelFit[Log[dropped],x,x];*)
(**)
(*Show[{ListLogLogPlot[rawData,PlotStyle->Black,AxesLabel->{Log[L],Log[N]}]*)
(*,ListLogLogPlot[dropped,PlotStyle->Green]*)
(*,Plot[lmdropped[x],{x,Log[threshold-1],1000},PlotStyle->{Red,Thickness->0.002}]*)
(*,Plot[x (dfSLE/.bb->b)-1,{x,1,10000},PlotStyle->{Blue,Dashed}]}*)
(*,(*PlotRange->All,*)PlotLabel->Row[{" b = ",b}]]*)
(**)
(*Print[ "Full data: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Quiet@lm["ParameterTable"][[1]][[1,3,2]]," \[PlusMinus] ",lm["ParameterErrors"][[2]],"*)
(*Dropped data: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Quiet@lmdropped["ParameterTable"][[1]][[1,3,2]]," \[PlusMinus] ",lmdropped["ParameterErrors"][[2]],"*)
(*Result from the litterature (exact with SLE): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Subsection:: *)
(*Drop the first AND LAST few*)


(* ::Input:: *)
(*Log[Max[dropped[[All,2]]]]//N*)


(* ::Input:: *)
(*thresholdBelow=300;*)
(*thresholdAbove=2400;*)
(**)
(*maxx=Max[rawData[[All,1]]];*)
(*maxy=Max[rawData[[All,2]]];*)
(**)
(*dropped=droppedb100V7=Select[rawData,thresholdBelow<#[[1]]<thresholdAbove&];*)
(**)
(*lmdropped=LinearModelFit[Log[dropped],x,x];*)
(**)
(*Show[{ListLogLogPlot[rawData,PlotStyle->Black,AxesLabel->{Log[L],Log[N]}]*)
(*,ListLogLogPlot[dropped,PlotStyle->Green]*)
(*,Plot[lmdropped[x],{x,Log[thresholdBelow],Log[thresholdAbove]},PlotStyle->{Red,Thickness->0.003}]*)
(*,Plot[x (dfSLE/.bb->b)-1,{x,1,10000},PlotStyle->{Blue,Dashed}]*)
(*,Plot[x (dfSLE/.bb->b)-1,{x,1,10000},PlotStyle->{Blue,Dashed}]*)
(*}*)
(*,(*PlotRange->All,*)PlotLabel->Row[{" b = ",b}]*)
(*,Epilog->{Directive[Dashed,Gray],*)
(*Line[{{Log[thresholdBelow],0},{Log[thresholdBelow],Log[max]//N}}]*)
(*,Line[{{Log[thresholdAbove],0},{Log[thresholdAbove],Log[maxy]//N}}]}*)
(*]*)
(**)
(*Print[ "Full data: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Quiet@lm["ParameterTable"][[1]][[1,3,2]]," \[PlusMinus] ",lm["ParameterErrors"][[2]],"*)
(*Dropped data: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Quiet@lmdropped["ParameterTable"][[1]][[1,3,2]]," \[PlusMinus] ",lmdropped["ParameterErrors"][[2]],"*)
(*Result from the litterature (exact with SLE): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Item::Closed:: *)
(*Plot with different drops below*)


(* ::Input:: *)
(*dfDropped=Table[With[{*)
(*lmdropped=LinearModelFit[Log[DeleteCases[rawData,{a_,_}/;(a<i )]],x,x]},*)
(*{i,Quiet@lmdropped["ParameterTable"][[1]][[1,3,2]]}],{i,5,100}];*)


(* ::Input:: *)
(*dfDroppedMore=Table[With[{*)
(*lmdropped=LinearModelFit[Log[DeleteCases[rawData,{a_,_}/;(a<i )]],x,x]},*)
(*{i,Quiet@lmdropped["ParameterTable"][[1]][[1,3,2]]}],{i,101,200}];*)


(* ::Input:: *)
(*dfDroppedMore2=Table[With[{*)
(*lmdropped=LinearModelFit[Log[DeleteCases[rawData,{a_,_}/;(a<i )]],x,x]},*)
(*{i,Quiet@lmdropped["ParameterTable"][[1]][[1,3,2]]}],{i,201,400}];*)


(* ::Input:: *)
(*dfDroppedMore3=Table[With[{*)
(*lmdropped=LinearModelFit[Log[DeleteCases[rawData,{a_,_}/;(a<i )]],x,x]},*)
(*{i,Quiet@lmdropped["ParameterTable"][[1]][[1,3,2]]}],{i,401,800}];*)


(* ::Input:: *)
(*dfDroppedMore4=Table[With[{*)
(*lmdropped=LinearModelFit[Log[DeleteCases[rawData,{a_,_}/;(a<i )]],x,x]},*)
(*{i,Quiet@lmdropped["ParameterTable"][[1]][[1,3,2]]}],{i,801,1000}];*)


(* ::Input:: *)
(*dfTogether=Join[dfDropped,dfDroppedMore,dfDroppedMore2,dfDroppedMore3,dfDroppedMore4];*)
(*fit=LinearModelFit[dfTogether,{1/Log[x],1},x];*)
(*Quiet@fit["ParameterTable"][[1]]*)
(**)
(*Show[*)
(*{ListPlot[dfTogether,PlotRange->{All,All}]*)
(*,Plot[fit[x],{x,0,1000},PlotStyle->Red]*)
(*,Plot[dfSLE/.bb->b/1.,{x,0,1000},PlotStyle->Blue]*)
(*}]*)


(* ::Item::Closed:: *)
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


(* ::Item::Closed:: *)
(*Plot with different POSITIONS of the same WINDOW:   (i)<  a < (i + 500)*)


(* ::Input:: *)
(*window=500;(*Fix this*)*)
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


(* ::Item::Closed:: *)
(*Plot with different POSITIONS of the same WINDOW:  Changing window*)


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
(*},PlotRange->{All,{0.99,1.01}},PlotLabel->Row[{"Window size = ",#[[1]]}]]&/@windowPlots*)


(* ::Input:: *)
(*windowPlots[[1]]/.Around[a_,b_]->Around[a,1000*b]*)


(* ::Input:: *)
(*synchronizedPlots=Map[Show[#,PlotRange->{All,{0.99,1.01}},ImageSize->220]&,showWindowPlots];*)
(**)
(*Multicolumn[synchronizedPlots,4,Appearance->"Framed"]*)


(* ::Input:: *)
(*Map[Show[#,PlotRange->{All,{1,1.005}},ImageSize->280]&,showWindowPlots[[6;;8]]];*)
(*Multicolumn[%,3,Appearance->"Framed"]*)


(* ::Subsection::Closed:: *)
(*Alternative extraction of d_f*)


(* ::Text:: *)
(*We can use Subscript[d, f]=(Log[n(L)]-Log[n(L')])/(Log[L]-Log[L'])*)


(* ::Input:: *)
(*sortedData=Sort[rawData];*)


(* ::Input:: *)
(*minL=Min[sortedData[[All,1]]]*)
(*maxL=Max[sortedData[[All,1]]]*)


(* ::Input:: *)
(*meanData=Table[*)
(*With[{fixedL=Select[#,#[[1]]==L&]},*)
(*If[fixedL=={},{},Mean[fixedL]]*)
(*]*)
(*,{L,minL,maxL,2}]&@sortedData;*)


(* ::Input:: *)
(*ListPlot[meanData]*)


(* ::Input:: *)
(*dfData=Most[Table[*)
(*Table[*)
(*With[{n1=#[[i,2]],n2=#[[j,2]],L1=#[[i,1]],L2=#[[j,1]]},*)
(*(Log[n1]-Log[n2])/(Log[L1]-Log[L2])//N]*)
(*,{j,i+1,Length[#]}]*)
(*,{i,1,Length[#]}]]&@meanData;*)


(* ::Item::Closed:: *)
(*Big systems: dfData[[-28 ;; -1]];*)


(* ::Input:: *)
(*Length[dfData];*)
(*dfData[[-28;;-1]];*)
(*ListPlot[%*)
(*,AxesLabel->{"Distance=| i_L1 - i_L2 |","d_f=\!\(\*FractionBox[\(Log[n1] - Log[n2]\), \(Log[L1] - Log[L2]\)]\)"},PlotRange->{All,All},ImageSize->600]*)


(* ::Input:: *)
(*Length[dfData]*)
(*dfData[[-200;;-1]];*)
(*ListPlot[%*)
(*,AxesLabel->{"Distance=| i_L1 - i_L2 |","d_f=\!\(\*FractionBox[\(Log[n1] - Log[n2]\), \(Log[L1] - Log[L2]\)]\)"},PlotRange->{All,All},ImageSize->600]*)


(* ::Item:: *)
(*Asymptotically good systems: dfData[[?? ;; -29]];*)


(* ::Subitem::Closed:: *)
(*-29*)


(* ::Input:: *)
(*Length[dfData];*)
(*tempData=dfData[[{-29}]];*)
(*data=Transpose[{Range[Length[%[[1]]]],%[[1]]}];*)
(**)
(**)
(*endRange=Length[data];*)
(**)
(*(*lm=LinearModelFit[data[[2;;-1]],{1,1/-Log[x]},x];*)*)
(*lm2=LinearModelFit[data[[2;;-1]],{1,1/x},x];*)
(**)
(*Show[{*)
(*ListPlot[tempData*)
(*,AxesLabel->{"Distance=| i_L1 - i_L2 |","d_f=\!\(\*FractionBox[\(Log[n1] - Log[n2]\), \(Log[L1] - Log[L2]\)]\)"},PlotRange->{All,All(*{0.5,1.2}*)},ImageSize->600]*)
(*(*,Plot[lm[x],{x,2,endRange},PlotStyle->{Red,Thickness->0.001}]*)*)
(*,Plot[lm2[x],{x,1,endRange},PlotStyle->{Blue,Thickness->0.001}]*)
(*},PlotRange->{All,All}*)
(*]*)
(**)
(*(*Quiet@lm["ParameterTable"]*)*)
(*Quiet@lm2["ParameterTable"]*)
(**)


(* ::Subitem::Closed:: *)
(*-30*)


(* ::Input:: *)
(*Length[dfData];*)
(*tempData=dfData[[{-30}]];*)
(*data=Transpose[{Range[Length[%[[1]]]],%[[1]]}];*)
(**)
(**)
(*endRange=Length[data];*)
(**)
(*(*lm=LinearModelFit[data[[2;;-1]],{1,1/-Log[x]},x];*)*)
(*lm2=LinearModelFit[data[[2;;-1]],{1,1/x},x];*)
(**)
(*Show[{*)
(*ListPlot[tempData*)
(*,AxesLabel->{"Distance=| i_L1 - i_L2 |","d_f=\!\(\*FractionBox[\(Log[n1] - Log[n2]\), \(Log[L1] - Log[L2]\)]\)"},PlotRange->{All,All(*{0.5,1.2}*)},ImageSize->600]*)
(*(*,Plot[lm[x],{x,2,endRange},PlotStyle->{Red,Thickness->0.001}]*)*)
(*,Plot[lm2[x],{x,1,endRange},PlotStyle->{Blue,Thickness->0.001}]*)
(*},PlotRange->{All,All}*)
(*]*)
(**)
(*(*Quiet@lm["ParameterTable"]*)*)
(*Quiet@lm2["ParameterTable"]*)
(**)


(* ::Subitem::Closed:: *)
(*-37*)


(* ::Input:: *)
(*Length[dfData];*)
(*tempData=dfData[[{-37}]];*)
(*data=Transpose[{Range[Length[%[[1]]]],%[[1]]}];*)
(**)
(**)
(*endRange=Length[data];*)
(**)
(*(*lm=LinearModelFit[data[[2;;-1]],{1,1/-Log[x]},x];*)*)
(*lm2=LinearModelFit[data[[2;;-1]],{1,1/x},x];*)
(**)
(*Show[{*)
(*ListPlot[tempData*)
(*,AxesLabel->{"Distance=| i_L1 - i_L2 |","d_f=\!\(\*FractionBox[\(Log[n1] - Log[n2]\), \(Log[L1] - Log[L2]\)]\)"},PlotRange->{All,All(*{0.5,1.2}*)},ImageSize->600]*)
(*(*,Plot[lm[x],{x,2,endRange},PlotStyle->{Red,Thickness->0.001}]*)*)
(*,Plot[lm2[x],{x,1,endRange},PlotStyle->{Blue,Thickness->0.001}]*)
(*},PlotRange->{All,All}*)
(*]*)
(**)
(*(*Quiet@lm["ParameterTable"]*)*)
(*Quiet@lm2["ParameterTable"]*)
(**)


(* ::Subitem:: *)
(*-38*)


(* ::Input:: *)
(*Length[dfData];*)
(*tempData=dfData[[{-39}]];*)
(*data=Transpose[{Range[Length[%[[1]]]],%[[1]]}];*)
(**)
(**)
(*endRange=Length[data];*)
(**)
(*(*lm=LinearModelFit[data[[2;;-1]],{1,1/-Log[x]},x];*)*)
(*lm2=LinearModelFit[data[[2;;-1]],{1,1/x},x];*)
(**)
(*Show[{*)
(*ListPlot[tempData*)
(*,AxesLabel->{"Distance=| i_L1 - i_L2 |","d_f=\!\(\*FractionBox[\(Log[n1] - Log[n2]\), \(Log[L1] - Log[L2]\)]\)"},PlotRange->{All,All(*{0.5,1.2}*)},ImageSize->600]*)
(*(*,Plot[lm[x],{x,2,endRange},PlotStyle->{Red,Thickness->0.001}]*)*)
(*,Plot[lm2[x],{x,1,endRange},PlotStyle->{Blue,Thickness->0.001}]*)
(*},PlotRange->{All,All}*)
(*]*)
(**)
(*(*Quiet@lm["ParameterTable"]*)*)
(*Quiet@lm2["ParameterTable"]*)
(**)


(* ::Item:: *)
(*Small systems: *)


(* ::Input:: *)
(*Length[dfData];*)
(*tempData=dfData[[1;;-29]];*)
(*data=Transpose[{Range[Length[%[[1]]]],%[[1]]}];*)
(**)
(**)
(*endRange=Length[data];*)
(**)
(*lm=LinearModelFit[data[[2;;-1]],{1,1/-Log[x]},x];*)
(**)
(*Show[{*)
(*ListPlot[tempData*)
(*,AxesLabel->{"Distance=| i_L1 - i_L2 |","d_f=\!\(\*FractionBox[\(Log[n1] - Log[n2]\), \(Log[L1] - Log[L2]\)]\)"},PlotRange->{All,All(*{0.5,1.2}*)},ImageSize->600]*)
(*,Plot[lm[x],{x,2,endRange},PlotStyle->{Red,Thickness->0.001}]*)
(*},PlotRange->{All,All}*)
(*]*)
(**)
(*Quiet@lm["ParameterTable"]*)
(**)


(* ::Section::Closed:: *)
(*b=100 With ALL the points from the CLUSTER together (more stat)*)


(* ::Subsection::Closed:: *)
(*Raw data*)


(* ::Input:: *)
(*b=100;*)
(**)
(*Length[data100]*)
(*Length[data100NoTolleranceBound]*)
(*Length[data100V7]*)
(**)
(*rawData=Join[data100,data100NoTolleranceBound,data100V7];*)
(*Length[%]*)


(* ::Input:: *)
(*ListPlot[rawData]*)
(*ListLogLogPlot[rawData]*)


(* ::Input:: *)
(*fitFuncs=Table[x^(-i),{i,-1,3}];*)
(*fitFuncs={x,1,Exp[-x],Exp[-2 x]};(*Exp[-1.3 x]*)*)
(**)
(*lmp=LinearModelFit[Log[rawData],fitFuncs,x];*)
(**)
(*lm=LinearModelFit[Log[rawData],x,x];*)
(**)
(*Show[{ListLogLogPlot[rawData,AxesLabel->{Log[L],Log[N]}],*)
(*Plot[lm[x],{x,0,1000},PlotStyle->{Red,Thickness->0.004}]*)
(*,Plot[lmp[x],{x,0,1000},PlotStyle->{Darker@Green,Thickness->0.004}]*)
(*,Plot[x (dfSLE/.bb->b)-1,{x,1,10000},PlotStyle->{Blue,Dashed}]}*)
(*,PlotLabel->Row[{" b = ",b}]]*)
(**)
(*Print[ "Linear fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Quiet@lm["ParameterTable"][[1]][[1,3,2]]," \[PlusMinus] ",lm["ParameterErrors"][[2]],"*)
(*",Total@fitFuncs," fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Quiet@lmp["ParameterTable"][[1]][[1,2,2]]," \[PlusMinus] ",lmp["ParameterErrors"][[1]],"*)
(*Result from the litterature (exact with SLE): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Subsection:: *)
(*Drop the first few*)


(* ::Input:: *)
(*threshold=70;*)
(**)
(*dropped=droppedb100Together=DeleteCases[rawData,{a_,_}/;a<threshold];*)
(*lmdropped=LinearModelFit[Log[dropped],x,x];*)
(**)
(*Show[{ListLogLogPlot[rawData,PlotStyle->Black,AxesLabel->{Log[L],Log[N]}]*)
(*,ListLogLogPlot[dropped,PlotStyle->Green]*)
(*,Plot[lmdropped[x],{x,Log[threshold-1],1000},PlotStyle->{Red,Thickness->0.002}]*)
(*,Plot[x (dfSLE/.bb->b)-1,{x,1,10000},PlotStyle->{Blue,Dashed}]}*)
(*,(*PlotRange->All,*)PlotLabel->Row[{" b = ",b}]]*)
(**)
(*Print[ "Full data: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Quiet@lm["ParameterTable"][[1]][[1,3,2]]," \[PlusMinus] ",lm["ParameterErrors"][[2]],"*)
(*Dropped data: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Quiet@lmdropped["ParameterTable"][[1]][[1,3,2]]," \[PlusMinus] ",lmdropped["ParameterErrors"][[2]],"*)
(*Result from the litterature (exact with SLE): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Section::Closed:: *)
(*b100 - clean_merged_data - Square . csv (+extra analysis)*)


(* ::Input:: *)
(*b=100;*)
(**)
(*rawData=data100Square=Join[ToExpression/@Import[FileNameJoin[{"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d-FromCluster\\b100-clean_merged_data-Square-ExtraStat.csv"}],"CSV"],ToExpression/@Import[FileNameJoin[{"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d-FromCluster\\b100-clean_merged_data-Square.csv"}],"CSV"]*)
(*];*)
(* (* MODIFY FILE NAME *)*)
(*Length[rawData]*)


(* ::Input:: *)
(*ListPlot[rawData]*)
(*ListLogLogPlot[rawData]*)


(* ::Input:: *)
(*fitFuncs=Table[x^(-i),{i,-1,3}];*)
(*(*fitFuncs=Append[Table[x^(-i),{i,-1,0}],Exp[-x]];*)*)
(*(**)
(*fitFuncs={x,1,Exp[-x],Exp[-2 x]};(*Exp[-1.3 x]*)*)*)
(*fitFuncs={x,1,Exp[-2x]};*)
(**)
(*lmp=LinearModelFit[Log[rawData],fitFuncs,x];*)
(**)
(*lm=LinearModelFit[Log[rawData],x,x];*)
(**)
(*fitFunc=a+c Exp[-\[Omega] x]+df x;*)
(*fitSol=FindFit[Log[rawData],{fitFunc,{-2<a<1.1,0<\[Omega]<3}},{a,c,\[Omega],df},x];*)
(**)
(*Show[{ListLogLogPlot[rawData,AxesLabel->{Log[L],Log[N]},DataRange->{0,50}],*)
(*Plot[lm[x],{x,0,1000},PlotStyle->{RGBColor[1, 0, 0],Thickness->0.004}]*)
(**)
(*,Plot[lmp[x],{x,0,1000},PlotStyle->{RGBColor[0, Rational[2, 3], 0],Thickness->0.004}]*)
(*,Plot[fitFunc/.fitSol,{x,0,1000},PlotStyle->{RGBColor[1, Rational[2, 3], 1],Thickness->0.004}]*)
(**)
(*,Plot[x (dfSLE/.bb->b)-1,{x,1,10000},PlotStyle->{RGBColor[0, 0, 1],Dashed}]}*)
(*,PlotLabel->Row[{" b = ",b}](*,PlotRange->{{1,Log[200]},{0,Log[100]}}*)*)
(*]*)
(**)
(*Print[ "Linear fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lm["ParameterTable"][[1]][[1,3,2]],\[Pi]*lm["ParameterErrors"][[2]]],"*)
(*",Total@fitFuncs," fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lmp["ParameterTable"][[1]][[1,2,2]],\[Pi]*lmp["ParameterErrors"][[1]]],"*)
(*",fitFunc," fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[df/.fitSol,Null],"*)
(*Result from the litterature (exact with SLE): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


(* ::Input:: *)
(*Quiet@lm["ParameterTable"][[1]]*)
(*Quiet@lmp["ParameterTable"][[1]]*)
(*fitSol*)


(* ::Subsection:: *)
(*Drop both first and/or last few*)


(* ::Input:: *)
(*maxx=Max[rawData[[All,1]]];*)
(*maxy=Max[rawData[[All,2]]];*)
(**)
(*thresholdBelow=44;*)
(*thresholdAbove=maxx-0;*)
(**)
(*dropped=droppedb100Square=Select[rawData,thresholdBelow<#[[1]]<thresholdAbove&];*)
(**)
(*lmdropped=LinearModelFit[Log[dropped],x,x];*)
(**)
(*lmpdropped=LinearModelFit[Log[dropped],fitFuncs,x];*)
(**)
(*Show[{ListLogLogPlot[rawData,PlotStyle->GrayLevel[0],AxesLabel->{Log[L],Log[N]}]*)
(*,ListLogLogPlot[dropped,PlotStyle->RGBColor[0, 1, 1]]*)
(*,Plot[lmp[x],{x,0,1000},PlotStyle->{RGBColor[0, Rational[2, 3], 0],Thickness->0.004}]*)
(*,Plot[lmdropped[x],{x,Log[thresholdBelow],Log[thresholdAbove-1]},PlotStyle->{RGBColor[1, 0, 0],Thickness->0.004}]*)
(*,Plot[lmpdropped[x],{x,Log[thresholdBelow],Log[thresholdAbove-1]},PlotStyle->{RGBColor[0.5, 0, 0.5],Thickness->0.004}]*)
(*,Plot[x (dfSLE/.bb->b)-1,{x,1,10000},PlotStyle->{RGBColor[0, 0, 1],Dashed}]}*)
(*,(*PlotRange->All,*)PlotLabel->Row[{" b = ",b}]*)
(*,Epilog->{Directive[Dashed,GrayLevel[0.5]],*)
(*Line[{{Log[thresholdBelow],0},{Log[thresholdBelow],Log[maxy]//N}}]*)
(*,Line[{{Log[thresholdAbove],0},{Log[thresholdAbove],Log[maxy]//N}}]}*)
(*]*)
(**)
(*Print[ "Full data - Linear fit : \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lm["ParameterTable"][[1]][[1,3,2]],\[Pi]*lm["ParameterErrors"][[2]]],"*)
(*Dropped data: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lmdropped["ParameterTable"][[1]][[1,3,2]],\[Pi]*lmdropped["ParameterErrors"][[2]]],"*)
(**)
(*Full data - ",Total@fitFuncs," fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lmp["ParameterTable"][[1]][[1,2,2]],\[Pi]*lmp["ParameterErrors"][[1]]],"*)
(*Dropped data with " ,Total@fitFuncs," fit: \!\(\*SubscriptBox[\(d\), \(f\)]\)=",Around[Quiet@lmpdropped["ParameterTable"][[1]][[1,2,2]],\[Pi]*lmpdropped["ParameterErrors"][[1]]],"*)
(**)
(*Result from the litterature (exact with SLE): ",(dfSLE/.bb->b)," = ",(dfSLE/.bb->b/1.)]*)


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


(* ::Subsubsection::Closed:: *)
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


(* ::Chapter:: *)
(*Compare with Analytic result*)


(* ::Section::Closed:: *)
(*Compared with SLE*)


(* ::Subsection::Closed:: *)
(*Tout court*)


(* ::Input:: *)
(*droppedList={droppedb0,droppedb1,droppedb2,droppedb3,droppedb4,droppedb5,droppedb10};*)
(**)
(*lmList=LinearModelFit[Log[#],x,x]&/@droppedList;*)


(* ::Input:: *)
(*Table[{i,Around[lmList[[i+1]]["BestFitParameters"][[2]],lmList[[i+1]]["ParameterErrors"][[2]]]},{i,0,6}](* Last one is actually 10 *)*)


(* ::Input:: *)
(*endRange=5;*)
(**)
(*Simulation2d=ListPlot[{{0,Around[1.7534581201029278`, 0.0060679884624822]},{1,Around[1.274522584835579, 0.008333817846449225]},{2,Around[1.1658669951861733`, 0.001939947142635663]},{3,Around[1.1073336136072602`, 0.002384187792543366]},{4,Around[1.0737383484918805`, 0.0017665587042004246`]},{5,Around[1.0670481729478147`, 0.001216345239817617]}(*,{10,1.0251\[PlusMinus]0.0012}*)},PlotStyle->Red];*)
(**)
(*Simulation2dupdated=ListPlot[{{1, Around[1.2470 ,0.0072 ]},{2,Around[1.1488, 0.0018]}},PlotStyle->Green];*)
(**)
(*plotSLE=Plot[dfSLE,{bb,0,endRange},PlotStyle->Blue,PlotRange->All];*)
(**)
(*Show[{plotSLE,Simulation2d,Simulation2dupdated},PlotRange->{1,2},AxesLabel->{"b",Subscript[d, f]},AxesOrigin->{0,1}]*)


(* ::Subsection:: *)
(*Normalize wrt SLE*)


(* ::Input:: *)
(*{{0,Around[1.7534581201029278`, 0.0060679884624822]},{1,Around[1.274522584835579, 0.008333817846449225]},{2,Around[1.1658669951861733`, 0.001939947142635663]},{3,Around[1.1073336136072602`, 0.002384187792543366]},{4,Around[1.0737383484918805`, 0.0017665587042004246`]},{5,Around[1.0670481729478147`, 0.001216345239817617]},{10,Around[1.025071579198033, 0.0011854752999352085`]}}/.{a_,b_}:>{a,(b-1)/(dfSLE-1./.bb->a)}*)


(* ::Input:: *)
(*Replace[{{1, Around[1.2470465422247456` ,0.0071503660373209805` ]},{2,Around[1.0770412465583747`, 0.0010238503038450958`]},{4,Around[1.0770412465583747`, 0.0010238503038450958`] }},{a_,b_}:>{a,(b-1)/(dfSLE-1./.bb->a)},1]*)


(* ::Input:: *)
(*dfSLE-1./.bb->10*)


(* ::Input:: *)
(*endRange=10;*)
(*dfRG1Lsimp:=2-(b \[Epsilon])/(1+2 b);*)
(**)
(*Simulation2dnorm=ListPlot[{{0,Around[1.0046108268039036`, 0.008090651283309599]},{1,Around[1.098090339342316, 0.0333352713857969]},{2,Around[1.1057799679078222`, 0.012932980950904421`]},{3,Around[1.0017803936677623`, 0.022252419397071416`]},{4,Around[0.8848601819025665, 0.021198704450405094`]},{5,Around[0.9833732032346162, 0.017839730183991716`]},{10,Around[0.7020042175449221, 0.033193308398185836`]}},PlotStyle->Red];*)
(**)
(*Simulation2dupdatedNorm=ListPlot[{{1,Around[0.9881861688989826, 0.028601464149283922`]},{2,Around[0.9920000000000003, 0.012]},{4,Around[0.9244949587004969, 0.01228620364614115]}},PlotStyle->Darker@Green];*)
(**)
(*Simulation2dNonLinearNorm=ListPlot[{{4,Around[0.8652252750105225, 0.27879823457167086`]}},PlotStyle->Purple];*)
(**)
(*plotSLEnorm=Plot[(dfSLE-1)/(dfSLE-1),{bb,0,endRange},PlotStyle->Blue,PlotRange->All];*)
(**)
(**)
(*plotRG1Lsimp=Plot[#/.\[Epsilon]->2,{b,0,endRange},PlotStyle->RGBColor[1, 0, 1],PlotRange->All,PlotLegends->Placed[{Row[{"NEW 1-Loop: ",TraditionalForm[#]}]},{Right,Top}]]&@((dfRG1Lsimp-1)/(3/(4(2b+1))));*)
(**)
(*Show[{plotSLEnorm,Simulation2dnorm,Simulation2dupdatedNorm,Simulation2dNonLinearNorm,plotRG1Lsimp},PlotRange->{{0,10.15},{0.5,1.3}},AxesLabel->{b,Subscript[d, f]},AxesOrigin->{0,1}]*)
