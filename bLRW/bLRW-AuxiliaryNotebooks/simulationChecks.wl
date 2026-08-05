(* ::Package:: *)

(* ::Input:: *)
(*Quit[]*)


(* ::Title::Closed:: *)
(*Various checks to verify that the simulation works fine*)


(* ::Section::Closed:: *)
(*RNG Extraction histogram*)


(* ::Input:: *)
(*(*rawData=Flatten@(ToExpression/@Import[FileNameJoin[{"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\extraction-Histogram.dat"}],"CSV"]); *)*)
(*(* MODIFY FILE NAME *)*)
(*rawData=ReadList["D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\extraction-Histogram.dat",Number];*)
(*Length@rawData;*)


(* ::Input:: *)
(*rawData[[1;;4]]*)


(* ::Input:: *)
(*(*Force the data into a flat,contiguous machine-number array*)*)
(*packedData=Developer`ToPackedArray[rawData];*)
(**)
(*(*Verify it worked (should return True)*)*)
(*Developer`PackedArrayQ[packedData]*)


(* ::Input:: *)
(*packedData[[1;;3]]*)


(* ::Input:: *)
(*maxVal=Max[packedData];*)
(**)
(*(*Fast integer or float counting*)*)
(*counts=N[BinCounts[packedData,{1,maxVal,1}]];*)
(**)
(*(*Convert counts to probabilities manually*)*)
(*heights=counts/Length[packedData];*)


(* ::Input:: *)
(*heights*)
(*ArrayRules[heights]*)


(* ::Input:: *)
(*(*Extract the non-zero data points:{bin_number,probability}*)*)
(*nonZeroData=Most[ArrayRules[heights]]/. ({bin_}->prob_)->{bin,prob};*)


(* ::Input:: *)
(*nonZeroData*)
(*Total@nonZeroData[[All,2]]*)


(* ::Input:: *)
(*(*Plot instantly since it only contains bins with actual data*)*)
(*ListPlot[Labeled[#,#[[2]]]&/@nonZeroData,Filling->Axis,PlotMarkers->None]*)


(* ::Text:: *)
(*The result is consistent with the theoretical values*)


(* ::Section::Closed:: *)
(*RNG Extraction histogram Local (./2dbLRW-extrHist.out 70 40.0 0)*)


(* ::Input:: *)
(*(*rawData=Flatten@(ToExpression/@Import[FileNameJoin[{"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\extraction-Histogram.dat"}],"CSV"]); *)*)
(*(* MODIFY FILE NAME *)*)
(*localData=BinaryReadList["D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\extractionData\\extraction-Histogram-at-70-50-b40.bin","Integer32"];*)
(*Length@localData*)


(* ::Input:: *)
(*localData[[1;;100]]*)


(* ::Input:: *)
(*(*Force the data into a flat,contiguous machine-number array*)*)
(*packedData=Developer`ToPackedArray[localData];*)
(**)
(*(*Verify it worked (should return True)*)*)
(*Developer`PackedArrayQ[packedData]*)


(* ::Input:: *)
(*maxVal=Max[N[packedData]]*)
(**)
(*(*Fast integer or float counting*)*)
(*counts=N[BinCounts[packedData,{0,maxVal+1,1}]]*)
(**)
(*(*Convert counts to probabilities manually*)*)
(*heights=counts/Length[packedData];*)


(* ::Input:: *)
(*heights*)
(*ArrayRules[heights]*)


(* ::Input:: *)
(*(*Extract the non-zero data points:{bin_number,probability}*)*)
(*nonZeroData=Most[ArrayRules[heights]]/. ({bin_}->prob_)->{bin-1,prob};*)


(* ::Input:: *)
(*nonZeroData*)
(*Total@nonZeroData[[All,2]]*)


(* ::Input:: *)
(*(*Plot instantly since it only contains bins with actual data*)*)
(*ListPlot[Labeled[#,#[[2]]]&/@nonZeroData,Filling->Axis,PlotMarkers->None,PlotRange->{{0,3},All}]*)


(* ::Text:: *)
(*The result is consistent with the theoretical values*)


(* ::Section::Closed:: *)
(*RNG Extraction histogram From cluster (./2dbLRW-extrHist.out 70 40.0 0) *)
(**)
(*WORK IN PROGESS*)


(* ::Text:: *)
(*It's so big it crashes Gemini suggested the code below*)


(* ::Subsection::Closed:: *)
(*crashing*)


(* ::Input:: *)
(*(*rawData=Flatten@(ToExpression/@Import[FileNameJoin[{"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\extraction-Histogram.dat"}],"CSV"]); *)*)
(*(* MODIFY FILE NAME *)*)
(*rawData=BinaryReadList["D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\extractionData\\extraction-Histogram-at-70-50-b40-Integer8.bin","Integer8"];*)
(*Length@rawData*)


(* ::Input:: *)
(*rawData[[1;;10]]*)


(* ::Input:: *)
(*(*Force the data into a flat,contiguous machine-number array*)*)
(*packedData=Developer`ToPackedArray[rawData];*)
(**)
(*(*Verify it worked (should return True)*)*)
(*Developer`PackedArrayQ[packedData]*)


(* ::Input:: *)
(*packedData[[1;;3]]*)


(* ::Input:: *)
(*maxVal=Max[N[packedData]];*)
(**)
(*(*Fast integer or float counting*)*)
(*counts=N[BinCounts[packedData,{0,maxVal+1,1}]];*)
(**)
(*(*Convert counts to probabilities manually*)*)
(*heights=counts/Length[packedData];*)


(* ::Input:: *)
(*heights*)
(*ArrayRules[heights]*)


(* ::Input:: *)
(*(*Extract the non-zero data points:{bin_number,probability}*)*)
(*nonZeroData=Most[ArrayRules[heights]]/. ({bin_}->prob_)->{bin-1,prob};*)


(* ::Input:: *)
(*nonZeroData*)
(*Total@nonZeroData[[All,2]]*)


(* ::Input:: *)
(*(*Plot instantly since it only contains bins with actual data*)*)
(*ListPlot[Labeled[#,#[[2]]]&/@nonZeroData,Filling->Axis,PlotMarkers->None]*)


(* ::Text:: *)
(*The result is consistent with the theoretical values*)


(* ::Subsection:: *)
(*Gemini*)


(* ::Input:: *)
(*(*1. Initialize a 4-element table to hold counts for 0,1,2,and 3*)tallyCounts={0,0,0,0};*)
(**)
(*(*2. Open a low-level binary stream to the file*)*)
(*stream=OpenRead["D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\extractionData\\extraction-Histogram-at-70-50-b40-Integer8.bin",BinaryFormat->True];*)
(**)
(*(*3. Process the file in chunks of 1 million bytes at a time*)*)
(*chunkSize=1000000;*)


(* ::Input:: *)
(*Internal`WithLocalSettings[None,*)
(*While[(chunk=BinaryReadList[stream,"Integer8",chunkSize])=!={},(*Scan through each {val,count} pair from Tally and update tallyCounts*)Scan[Function[pair,With[{val=pair[[1]],count=pair[[2]]},If[0<=val<=3,tallyCounts[[val+1]]+=count]]],Tally[chunk]]],*)
(**)
(*(*4. ALWAYS close the file stream securely if it finishes or aborts*)Close[stream]*)
(*];*)


(* ::Input:: *)
(*(*5. Compute the final probabilities from your tiny tally list*)*)
(*heights=N[tallyCounts/Total[tallyCounts]];*)
(**)
(*Print["Final Probabilities: ",heights];*)


(* ::Input:: *)
(*BarChart[heights,BarSpacing->0,PlotRange->{All,{0,10^-8}}]*)


(* ::Input:: *)
(*(*yess this is the expected PDF!!*)*)


(* ::Text:: *)
(*Rates and normalized probabilities with tot rate 1.90158222785e-119*)
(*  Rate to the top neighbor:     7.29784472775e-128       Prob:  3.83777499646e-09*)
(*  Rate to the bottom neighbor:  1.08033634696e-124       Prob:  5.68124970425e-06*)
(*  Rate to the right neighbor:   8.26607443788e-131       Prob:  4.34694556818e-12*)
(*  Rate to the left neighbor:    1.90157141718e-119       Prob:  0.999994314908*)


(* ::Input:: *)
(*(*I didn't run the code long enough to see the e-12*)*)


(* ::Section::Closed:: *)
(*L=141 path Lengths histogram*)


(* ::Input:: *)
(*(*rawData=Flatten@(ToExpression/@Import[FileNameJoin[{"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\extraction-Histogram.dat"}],"CSV"]); *)*)
(*(* MODIFY FILE NAME *)*)
(*pathWords=ReadList["D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d-FromCluster\\b=10.000000-LRW-2d-square-lattice-data-141_repeat-100000.csv",Word,RecordLists->True,WordSeparators->{","," ","\t"}];*)
(*Length@pathWords*)


(* ::Input:: *)
(*pathLenghts=ToExpression[pathWords[[All,2;;]]];*)


(* ::Input:: *)
(*(*Force the data into a flat,contiguous machine-number array*)*)
(*pathLenghts=Developer`ToPackedArray[Flatten[pathLenghts]];*)
(**)
(*(*Verify it worked (should return True)*)*)
(*Developer`PackedArrayQ[pathLenghts]*)


(* ::Input:: *)
(*pathLenghts[[1;;2]]*)


(* ::Input:: *)
(*AppendTo[h,Histogram[pathLenghts,{1},"Probability",PlotRange->All,PlotLabel->Row[{"-Log[\[Epsilon]] = ",\[Epsilon]}]]]*)
(*Histogram[pathLenghts,{1},"PDF",PlotRange->{All,All}];*)


(* ::Subsection:: *)
(*Mean*)


(* ::Input:: *)
(*Total@pathLenghts/Length[pathLenghts]//N*)


(* ::Section::Closed:: *)
(*L=21 path Lengths histograms*)


(* ::Input:: *)
(*lbar={}*)
(*h={}*)
(*\[Epsilon]=1*)


(* ::Subsection:: *)
(*1e-1 is bad: it crosses itself...*)


(* ::Subsection::Closed:: *)
(*1e-2 only crossed itself once in 10^5 runs (1min30s)*)


(* ::Input:: *)
(*\[Epsilon]=\[Epsilon]+1*)


(* ::Input:: *)
(*(*rawData=Flatten@(ToExpression/@Import[FileNameJoin[{"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\extraction-Histogram.dat"}],"CSV"]); *)*)
(*(* MODIFY FILE NAME *)*)
(*pathWords=ReadList["D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d\\b=2_LRW-2d-square-lattice-data-21_repeat-100000_tol-1e-2.csv",Word,RecordLists->True,WordSeparators->{","," ","\t"}];*)
(*Length@pathWords*)


(* ::Input:: *)
(*pathWords[[1;;2]]*)


(* ::Input:: *)
(*pathLenghts=ToExpression[pathWords[[All,2;;]]];*)


(* ::Input:: *)
(*(*Force the data into a flat,contiguous machine-number array*)*)
(*pathLenghts=Developer`ToPackedArray[Flatten[pathLenghts]];*)
(**)
(*(*Verify it worked (should return True)*)*)
(*Developer`PackedArrayQ[pathLenghts]*)


(* ::Input:: *)
(*pathLenghts[[1;;2]]*)


(* ::Input:: *)
(*AppendTo[h,Histogram[pathLenghts,{1},"Probability",PlotRange->All,PlotLabel->Row[{"-Log[\[Epsilon]] = ",\[Epsilon]}]]]*)


(* ::Subsubsection:: *)
(*Mean*)


(* ::Input:: *)
(*AppendTo[lbar,Total@pathLenghts/Length[pathLenghts]//N]*)


(* ::Subsection::Closed:: *)
(*1e-3 (no crosses) (1min56s)*)


(* ::Input:: *)
(*\[Epsilon]=\[Epsilon]+1*)


(* ::Input:: *)
(*(*rawData=Flatten@(ToExpression/@Import[FileNameJoin[{"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\extraction-Histogram.dat"}],"CSV"]); *)*)
(*(* MODIFY FILE NAME *)*)
(*pathWords=ReadList["D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d\\b=2_LRW-2d-square-lattice-data-21_repeat-100000_tol-1e-3.csv",Word,RecordLists->True,WordSeparators->{","," ","\t"}];*)
(*Length@pathWords*)


(* ::Input:: *)
(*pathWords[[1;;2]]*)


(* ::Input:: *)
(*pathLenghts=ToExpression[pathWords[[All,2;;]]];*)


(* ::Input:: *)
(*(*Force the data into a flat,contiguous machine-number array*)*)
(*pathLenghts=Developer`ToPackedArray[Flatten[pathLenghts]];*)
(**)
(*(*Verify it worked (should return True)*)*)
(*Developer`PackedArrayQ[pathLenghts]*)


(* ::Input:: *)
(*pathLenghts[[1;;2]]*)


(* ::Input:: *)
(*AppendTo[h,Histogram[pathLenghts,{1},"Probability",PlotRange->All,PlotLabel->Row[{"-Log[\[Epsilon]] = ",\[Epsilon]}]]]*)
(*Histogram[pathLenghts,{1},"PDF",PlotRange->{All,All}];*)


(* ::Subsubsection:: *)
(*Mean*)


(* ::Input:: *)
(*AppendTo[lbar,Total@pathLenghts/Length[pathLenghts]//N]*)


(* ::Subsection::Closed:: *)
(*1e-4 (2min51s)*)


(* ::Input:: *)
(*\[Epsilon]=\[Epsilon]+1*)


(* ::Input:: *)
(*(*rawData=Flatten@(ToExpression/@Import[FileNameJoin[{"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\extraction-Histogram.dat"}],"CSV"]); *)*)
(*(* MODIFY FILE NAME *)*)
(*pathWords=ReadList["D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d\\b=2_LRW-2d-square-lattice-data-21_repeat-100000_tol-1e-4.csv",Word,RecordLists->True,WordSeparators->{","," ","\t"}];*)
(*Length@pathWords*)


(* ::Input:: *)
(*pathWords[[1;;2]]*)


(* ::Input:: *)
(*pathLenghts=ToExpression[pathWords[[All,2;;]]];*)


(* ::Input:: *)
(*(*Force the data into a flat,contiguous machine-number array*)*)
(*pathLenghts=Developer`ToPackedArray[Flatten[pathLenghts]];*)
(**)
(*(*Verify it worked (should return True)*)*)
(*Developer`PackedArrayQ[pathLenghts]*)


(* ::Input:: *)
(*pathLenghts[[1;;2]]*)


(* ::Input:: *)
(*AppendTo[h,Histogram[pathLenghts,{1},"Probability",PlotRange->All,PlotLabel->Row[{"-Log[\[Epsilon]] = ",\[Epsilon]}]]]*)
(*Histogram[pathLenghts,{1},"PDF",PlotRange->{All,All}];*)


(* ::Subsubsection:: *)
(*Mean*)


(* ::Input:: *)
(*AppendTo[lbar,Total@pathLenghts/Length[pathLenghts]//N]*)


(* ::Subsection::Closed:: *)
(*1e-5 (~3min)*)


(* ::Input:: *)
(*\[Epsilon]=\[Epsilon]+1*)


(* ::Input:: *)
(*(*rawData=Flatten@(ToExpression/@Import[FileNameJoin[{"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\extraction-Histogram.dat"}],"CSV"]); *)*)
(*(* MODIFY FILE NAME *)*)
(*pathWords=ReadList["D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d\\b=2_LRW-2d-square-lattice-data-21_repeat-100000_tol-1e-5.csv",Word,RecordLists->True,WordSeparators->{","," ","\t"}];*)
(*Length@pathWords*)


(* ::Input:: *)
(*pathWords[[1;;2]]*)


(* ::Input:: *)
(*pathLenghts=ToExpression[pathWords[[All,2;;]]];*)


(* ::Input:: *)
(*(*Force the data into a flat,contiguous machine-number array*)*)
(*pathLenghts=Developer`ToPackedArray[Flatten[pathLenghts]];*)
(**)
(*(*Verify it worked (should return True)*)*)
(*Developer`PackedArrayQ[pathLenghts]*)


(* ::Input:: *)
(*pathLenghts[[1;;2]]*)


(* ::Input:: *)
(*AppendTo[h,Histogram[pathLenghts,{1},"Probability",PlotRange->All,PlotLabel->Row[{"-Log[\[Epsilon]] = ",\[Epsilon]}]]]*)
(*Histogram[pathLenghts,{1},"PDF",PlotRange->{All,All}];*)


(* ::Subsubsection:: *)
(*Mean*)


(* ::Input:: *)
(*AppendTo[lbar,Total@pathLenghts/Length[pathLenghts]//N]*)


(* ::Subsection::Closed:: *)
(*1e-6 (3min4s)*)


(* ::Input:: *)
(*\[Epsilon]=\[Epsilon]+1*)


(* ::Input:: *)
(*(*rawData=Flatten@(ToExpression/@Import[FileNameJoin[{"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\extraction-Histogram.dat"}],"CSV"]); *)*)
(*(* MODIFY FILE NAME *)*)
(*pathWords=ReadList["D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d\\b=2_LRW-2d-square-lattice-data-21_repeat-100000_tol-1e-6.csv",Word,RecordLists->True,WordSeparators->{","," ","\t"}];*)
(*Length@pathWords*)


(* ::Input:: *)
(*pathWords[[1;;2]]*)


(* ::Input:: *)
(*pathLenghts=ToExpression[pathWords[[All,2;;]]];*)


(* ::Input:: *)
(*(*Force the data into a flat,contiguous machine-number array*)*)
(*pathLenghts=Developer`ToPackedArray[Flatten[pathLenghts]];*)
(**)
(*(*Verify it worked (should return True)*)*)
(*Developer`PackedArrayQ[pathLenghts]*)


(* ::Input:: *)
(*pathLenghts[[1;;2]]*)


(* ::Input:: *)
(*AppendTo[h,Histogram[pathLenghts,{1},"Probability",PlotRange->All,PlotLabel->Row[{"-Log[\[Epsilon]] = ",\[Epsilon]}]]]*)
(*Histogram[pathLenghts,{1},"PDF",PlotRange->{All,All}];*)


(* ::Subsubsection:: *)
(*Mean*)


(* ::Input:: *)
(*AppendTo[lbar,Total@pathLenghts/Length[pathLenghts]//N]*)


(* ::Subsection::Closed:: *)
(*1e-7 (3min36s)*)


(* ::Input:: *)
(*\[Epsilon]=\[Epsilon]+1*)


(* ::Input:: *)
(*(*rawData=Flatten@(ToExpression/@Import[FileNameJoin[{"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\extraction-Histogram.dat"}],"CSV"]); *)*)
(*(* MODIFY FILE NAME *)*)
(*pathWords=ReadList["D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d\\b=2_LRW-2d-square-lattice-data-21_repeat-100000_tol-1e-7.csv",Word,RecordLists->True,WordSeparators->{","," ","\t"}];*)
(*Length@pathWords*)


(* ::Input:: *)
(*pathWords[[1;;2]]*)


(* ::Input:: *)
(*pathLenghts=ToExpression[pathWords[[All,2;;]]];*)


(* ::Input:: *)
(*(*Force the data into a flat,contiguous machine-number array*)*)
(*pathLenghts=Developer`ToPackedArray[Flatten[pathLenghts]];*)
(**)
(*(*Verify it worked (should return True)*)*)
(*Developer`PackedArrayQ[pathLenghts]*)


(* ::Input:: *)
(*pathLenghts[[1;;2]]*)


(* ::Input:: *)
(*AppendTo[h,Histogram[pathLenghts,{1},"Probability",PlotRange->All,PlotLabel->Row[{"-Log[\[Epsilon]] = ",\[Epsilon]}]]]*)
(*Histogram[pathLenghts,{1},"PDF",PlotRange->{All,All}];*)


(* ::Subsubsection:: *)
(*Mean*)


(* ::Input:: *)
(*AppendTo[lbar,Total@pathLenghts/Length[pathLenghts]//N]*)


(* ::Subsection::Closed:: *)
(*1e-8 (4min5s)*)


(* ::Input:: *)
(*\[Epsilon]=\[Epsilon]+1*)


(* ::Input:: *)
(*(*rawData=Flatten@(ToExpression/@Import[FileNameJoin[{"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\extraction-Histogram.dat"}],"CSV"]); *)*)
(*(* MODIFY FILE NAME *)*)
(*pathWords=ReadList["D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d\\b=2_LRW-2d-square-lattice-data-21_repeat-100000_tol-1e-8.csv",Word,RecordLists->True,WordSeparators->{","," ","\t"}];*)
(*Length@pathWords*)


(* ::Input:: *)
(*pathWords[[1;;2]]*)


(* ::Input:: *)
(*pathLenghts=ToExpression[pathWords[[All,2;;]]];*)


(* ::Input:: *)
(*(*Force the data into a flat,contiguous machine-number array*)*)
(*pathLenghts=Developer`ToPackedArray[Flatten[pathLenghts]];*)
(**)
(*(*Verify it worked (should return True)*)*)
(*Developer`PackedArrayQ[pathLenghts]*)


(* ::Input:: *)
(*pathLenghts[[1;;2]]*)


(* ::Input:: *)
(*AppendTo[h,Histogram[pathLenghts,{1},"Probability",PlotRange->All,PlotLabel->Row[{"-Log[\[Epsilon]] = ",\[Epsilon]}]]]*)
(*Histogram[pathLenghts,{1},"PDF",PlotRange->{All,All}];*)


(* ::Subsubsection:: *)
(*Mean*)


(* ::Input:: *)
(*AppendTo[lbar,Total@pathLenghts/Length[pathLenghts]//N]*)


(* ::Subsection::Closed:: *)
(*1e-9 (4min18s)*)


(* ::Input:: *)
(*\[Epsilon]=\[Epsilon]+1*)


(* ::Input:: *)
(*(*rawData=Flatten@(ToExpression/@Import[FileNameJoin[{"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\extraction-Histogram.dat"}],"CSV"]); *)*)
(*(* MODIFY FILE NAME *)*)
(*pathWords=ReadList["D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d\\b=2_LRW-2d-square-lattice-data-21_repeat-100000_tol-1e-9.csv",Word,RecordLists->True,WordSeparators->{","," ","\t"}];*)
(*Length@pathWords*)


(* ::Input:: *)
(*pathWords[[1;;2]]*)


(* ::Input:: *)
(*pathLenghts=ToExpression[pathWords[[All,2;;]]];*)


(* ::Input:: *)
(*(*Force the data into a flat,contiguous machine-number array*)*)
(*pathLenghts=Developer`ToPackedArray[Flatten[pathLenghts]];*)
(**)
(*(*Verify it worked (should return True)*)*)
(*Developer`PackedArrayQ[pathLenghts]*)


(* ::Input:: *)
(*pathLenghts[[1;;2]]*)


(* ::Input:: *)
(*AppendTo[h,Histogram[pathLenghts,{1},"Probability",PlotRange->All,PlotLabel->Row[{"-Log[\[Epsilon]] = ",\[Epsilon]}]]]*)
(*Histogram[pathLenghts,{1},"PDF",PlotRange->{All,All}];*)


(* ::Subsubsection:: *)
(*Mean*)


(* ::Input:: *)
(*AppendTo[lbar,Total@pathLenghts/Length[pathLenghts]//N]*)


(* ::Subsection::Closed:: *)
(*1e-10 (4min36s)*)


(* ::Input:: *)
(*\[Epsilon]=\[Epsilon]+1*)


(* ::Input:: *)
(*(*rawData=Flatten@(ToExpression/@Import[FileNameJoin[{"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\extraction-Histogram.dat"}],"CSV"]); *)*)
(*(* MODIFY FILE NAME *)*)
(*pathWords=ReadList["D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d\\b=2_LRW-2d-square-lattice-data-21_repeat-100000_tol-1e-10.csv",Word,RecordLists->True,WordSeparators->{","," ","\t"}];*)
(*Length@pathWords*)


(* ::Input:: *)
(*pathWords[[1;;2]]*)


(* ::Input:: *)
(*pathLenghts=ToExpression[pathWords[[All,2;;]]];*)


(* ::Input:: *)
(*(*Force the data into a flat,contiguous machine-number array*)*)
(*pathLenghts=Developer`ToPackedArray[Flatten[pathLenghts]];*)
(**)
(*(*Verify it worked (should return True)*)*)
(*Developer`PackedArrayQ[pathLenghts]*)


(* ::Input:: *)
(*pathLenghts[[1;;2]]*)


(* ::Input:: *)
(*AppendTo[h,Histogram[pathLenghts,{1},"Probability",PlotRange->All,PlotLabel->Row[{"-Log[\[Epsilon]] = ",\[Epsilon]}]]]*)
(*Histogram[pathLenghts,{1},"PDF",PlotRange->{All,All}];*)


(* ::Subsubsection:: *)
(*Mean*)


(* ::Input:: *)
(*AppendTo[lbar,Total@pathLenghts/Length[pathLenghts]//N]*)


(* ::Subsection::Closed:: *)
(*1e-11 (5min0s)*)


(* ::Input:: *)
(*\[Epsilon]=\[Epsilon]+1*)


(* ::Input:: *)
(*(*rawData=Flatten@(ToExpression/@Import[FileNameJoin[{"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\extraction-Histogram.dat"}],"CSV"]); *)*)
(*(* MODIFY FILE NAME *)*)
(*pathWords=ReadList["D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d\\b=2_LRW-2d-square-lattice-data-21_repeat-100000_tol-1e-11.csv",Word,RecordLists->True,WordSeparators->{","," ","\t"}];*)
(*Length@pathWords*)


(* ::Input:: *)
(*pathWords[[1;;2]]*)


(* ::Input:: *)
(*pathLenghts=ToExpression[pathWords[[All,2;;]]];*)


(* ::Input:: *)
(*(*Force the data into a flat,contiguous machine-number array*)*)
(*pathLenghts=Developer`ToPackedArray[Flatten[pathLenghts]];*)
(**)
(*(*Verify it worked (should return True)*)*)
(*Developer`PackedArrayQ[pathLenghts]*)


(* ::Input:: *)
(*pathLenghts[[1;;2]]*)


(* ::Input:: *)
(*AppendTo[h,Histogram[pathLenghts,{1},"Probability",PlotRange->All,PlotLabel->Row[{"-Log[\[Epsilon]] = ",\[Epsilon]}]]]*)
(*Histogram[pathLenghts,{1},"PDF",PlotRange->{All,All}];*)


(* ::Subsubsection:: *)
(*Mean*)


(* ::Input:: *)
(*AppendTo[lbar,Total@pathLenghts/Length[pathLenghts]//N]*)


(* ::Subsection::Closed:: *)
(*1e-12 (5min6s)*)


(* ::Input:: *)
(*\[Epsilon]=\[Epsilon]+1*)


(* ::Input:: *)
(*(*rawData=Flatten@(ToExpression/@Import[FileNameJoin[{"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\extraction-Histogram.dat"}],"CSV"]); *)*)
(*(* MODIFY FILE NAME *)*)
(*pathWords=ReadList["D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d\\b=2_LRW-2d-square-lattice-data-21_repeat-100000_tol-1e-12.csv",Word,RecordLists->True,WordSeparators->{","," ","\t"}];*)
(*Length@pathWords*)


(* ::Input:: *)
(*pathWords[[1;;2]]*)


(* ::Input:: *)
(*pathLenghts=ToExpression[pathWords[[All,2;;]]];*)


(* ::Input:: *)
(*(*Force the data into a flat,contiguous machine-number array*)*)
(*pathLenghts=Developer`ToPackedArray[Flatten[pathLenghts]];*)
(**)
(*(*Verify it worked (should return True)*)*)
(*Developer`PackedArrayQ[pathLenghts]*)


(* ::Input:: *)
(*pathLenghts[[1;;2]]*)


(* ::Input:: *)
(*AppendTo[h,Histogram[pathLenghts,{1},"Probability",PlotRange->All,PlotLabel->Row[{"-Log[\[Epsilon]] = ",\[Epsilon]}]]];*)
(*Histogram[pathLenghts,{1},"PDF",PlotRange->{All,All}];*)


(* ::Subsubsection:: *)
(*Mean*)


(* ::Input:: *)
(*AppendTo[lbar,Total@pathLenghts/Length[pathLenghts]//N]*)


(* ::Subsection::Closed:: *)
(*1e-13 (5min39s)*)


(* ::Input:: *)
(*\[Epsilon]=\[Epsilon]+1*)


(* ::Input:: *)
(*(*rawData=Flatten@(ToExpression/@Import[FileNameJoin[{"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\extraction-Histogram.dat"}],"CSV"]); *)*)
(*(* MODIFY FILE NAME *)*)
(*pathWords=ReadList["D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d\\b=2_LRW-2d-square-lattice-data-21_repeat-100000_tol-1e-13.csv",Word,RecordLists->True,WordSeparators->{","," ","\t"}];*)
(*Length@pathWords*)


(* ::Input:: *)
(*pathWords[[1;;2]]*)


(* ::Input:: *)
(*pathLenghts=ToExpression[pathWords[[All,2;;]]];*)


(* ::Input:: *)
(*(*Force the data into a flat,contiguous machine-number array*)*)
(*pathLenghts=Developer`ToPackedArray[Flatten[pathLenghts]];*)
(**)
(*(*Verify it worked (should return True)*)*)
(*Developer`PackedArrayQ[pathLenghts]*)


(* ::Input:: *)
(*pathLenghts[[1;;2]]*)


(* ::Input:: *)
(*AppendTo[h,Histogram[pathLenghts,{1},"Probability",PlotRange->All,PlotLabel->Row[{"-Log[\[Epsilon]] = ",\[Epsilon]}]]];*)
(*Histogram[pathLenghts,{1},"PDF",PlotRange->{All,All}];*)


(* ::Subsubsection:: *)
(*Mean*)


(* ::Input:: *)
(*AppendTo[lbar,Total@pathLenghts/Length[pathLenghts]//N]*)


(* ::Subsection::Closed:: *)
(*1e-14 (7min41s)*)


(* ::Input:: *)
(*\[Epsilon]=\[Epsilon]+1*)


(* ::Input:: *)
(*(*rawData=Flatten@(ToExpression/@Import[FileNameJoin[{"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\extraction-Histogram.dat"}],"CSV"]); *)*)
(*(* MODIFY FILE NAME *)*)
(*pathWords=ReadList["D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d\\b=2_LRW-2d-square-lattice-data-21_repeat-100000_tol-1e-14.csv",Word,RecordLists->True,WordSeparators->{","," ","\t"}];*)
(*Length@pathWords*)


(* ::Input:: *)
(*pathWords[[1;;2]]*)


(* ::Input:: *)
(*pathLenghts=ToExpression[pathWords[[All,2;;]]];*)


(* ::Input:: *)
(*(*Force the data into a flat,contiguous machine-number array*)*)
(*pathLenghts=Developer`ToPackedArray[Flatten[pathLenghts]];*)
(**)
(*(*Verify it worked (should return True)*)*)
(*Developer`PackedArrayQ[pathLenghts]*)


(* ::Input:: *)
(*pathLenghts[[1;;2]]*)


(* ::Input:: *)
(*AppendTo[h,Histogram[pathLenghts,{1},"Probability",PlotRange->All,PlotLabel->Row[{"-Log[\[Epsilon]] = ",\[Epsilon]}]]];*)
(*Histogram[pathLenghts,{1},"PDF",PlotRange->{All,All}];*)


(* ::Subsubsection:: *)
(*Mean*)


(* ::Input:: *)
(*AppendTo[lbar,Total@pathLenghts/Length[pathLenghts]//N]*)


(* ::Subsection::Closed:: *)
(*1e-15 (7min17s)*)


(* ::Input:: *)
(*\[Epsilon]=\[Epsilon]+1*)


(* ::Input:: *)
(*(*rawData=Flatten@(ToExpression/@Import[FileNameJoin[{"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\extraction-Histogram.dat"}],"CSV"]); *)*)
(*(* MODIFY FILE NAME *)*)
(*pathWords=ReadList["D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d\\b=2_LRW-2d-square-lattice-data-21_repeat-100000_tol-1e-15.csv",Word,RecordLists->True,WordSeparators->{","," ","\t"}];*)
(*Length@pathWords*)


(* ::Input:: *)
(*pathWords[[1;;2]]*)


(* ::Input:: *)
(*pathLenghts=ToExpression[pathWords[[All,2;;]]];*)


(* ::Input:: *)
(*(*Force the data into a flat,contiguous machine-number array*)*)
(*pathLenghts=Developer`ToPackedArray[Flatten[pathLenghts]];*)
(**)
(*(*Verify it worked (should return True)*)*)
(*Developer`PackedArrayQ[pathLenghts]*)


(* ::Input:: *)
(*pathLenghts[[1;;2]]*)


(* ::Input:: *)
(*AppendTo[h,Histogram[pathLenghts,{1},"Probability",PlotRange->All,PlotLabel->Row[{"-Log[\[Epsilon]] = ",\[Epsilon]}]]];*)
(*Histogram[pathLenghts,{1},"PDF",PlotRange->{All,All}];*)


(* ::Subsubsection:: *)
(*Mean*)


(* ::Input:: *)
(*AppendTo[lbar,Total@pathLenghts/Length[pathLenghts]//N]*)


(* ::Subsection::Closed:: *)
(*Plot of lbar VS exponent*)


(* ::Input:: *)
(*lbarData=Table[{i+1,lbar[[i]]},{i,1,Length@lbar}]*)


(* ::Input:: *)
(*ListPlot[lbarData,AxesLabel->{"-Log[\[Epsilon]]","<l>"},PlotRange->All]*)


(* ::Input:: *)
(*(*it seems that already at \[Epsilon]=1e-4 we get a good result*)*)


(* ::Subsection::Closed:: *)
(*Histograms*)


(* ::Input:: *)
(*(*1. Force every histogram in your list to share the exact same Y-axis scale*)synchronizedHistograms=Map[Show[#,PlotRange->{{0,55},{0,.25}},ImageSize->240]&,h];*)
(**)
(*(*2. Display them instantly in a clean grid layout (3 columns)*)*)
(*Multicolumn[synchronizedHistograms,4,Appearance->"Framed"]*)


(* ::Subsection::Closed:: *)
(*Frequency of the mean*)


(* ::Input:: *)
(*bin15Heights=Map[With[{rects=SelectFirst[Cases[#,_Rectangle,All],#[[1,1]]<=15.0<=#[[2,1]]&]},rects[[2,2]]]&,h]*)


(* ::Input:: *)
(*bin15Heightsdata=Table[{i+1,bin15Heights[[i]]},{i,1,Length@lbar}]*)


(* ::Input:: *)
(*ListPlot[bin15Heightsdata,AxesLabel->{"-Log[\[Epsilon]]","mean_freq"},PlotRange->All]*)


(* ::Subsection::Closed:: *)
(*Frequency of the mode*)


(* ::Input:: *)
(*(*1. Find all rectangles inside the graphic object*)allRectangles=Cases[h[[1]],_Rectangle,All];*)
(**)
(*(*2. Find the rectangle with the maximum Y-max (highest bar)*)*)
(*tallestRectangle=MaximalBy[allRectangles,#[[2,2]]&][[1]];*)
(**)
(*(*3. Extract its X boundaries to identify the bin value*)*)
(*modeBin={tallestRectangle[[{1,2},1]]/.Rectangle->Sequence}*)
(**)
(*mode=Mean@modeBin*)


(* ::Input:: *)
(*modesHistory=Map[With[{tallest=MaximalBy[Cases[#,_Rectangle,All],#[[2,2]]&][[1]]},(*Calculate the floor/midpoint of the tallest bin to get the value*)Ceiling[tallest[[1,1]]]]&,h]*)


(* ::Input:: *)
(*(*The mode does not change*)*)


(* ::Input:: *)
(*binModeHeights=Map[With[{rects=SelectFirst[Cases[#,_Rectangle,All],#[[1,1]]<=13.0<=#[[2,1]]&]},rects[[2,2]]]&,h]*)


(* ::Input:: *)
(*binModeHeightsdata=Table[{i+1,binModeHeights[[i]]},{i,1,Length@binModeHeights}]*)


(* ::Input:: *)
(*ListPlot[binModeHeightsdata,AxesLabel->{"-Log[\[Epsilon]]","mean_freq"},PlotRange->{All,{0.2,0.3}}]*)


(* ::Subsection:: *)
(*Observable: \[CapitalSigma]_l (|f(l)-g(l)|)/(f(l)+g(l))*)


(* ::Input:: *)
(*(*1. Find all rectangles that exist*)*)
(*rectangles=Cases[h[[1]],_Rectangle,All];*)
(**)
(*(*2. Find the full absolute limits of the X-axis from the plot geometry*)*)
(*xMin=0.;*)
(*xMax=55.;*)
(**)
(*(*3. Reconstruct your full step-by-step bin sequence (assuming bin width=1)*)*)
(*allBinMidpoints=Range[xMin+0.5,xMax-0.5,1.];*)


(* ::Input:: *)
(*(*4. Build a lookup association from the rectangles that DO exist*)*)
(*existingHeights=Association[Map[(#[[1,1]]+1)->#[[2,2]]&,rectangles]];*)
(**)
(*(*5. Map across all possible bins,filling in 0. if the bin was empty*)*)
(*fullHistogramData=Table[{midpoint,Lookup[existingHeights,midpoint,0.]},{midpoint,allBinMidpoints}];*)


(* ::Input:: *)
(*With[{rects=Cases[#,_Rectangle,All]},Table[{midpoint,Lookup[Association[Map[(#[[1,1]]+1)->#[[2,2]]&,rects]],midpoint,0.]},{midpoint,allBinMidpoints}]]&@h[[5]]*)


(* ::Input:: *)
(*Length[h]*)


(* ::Input:: *)
(*allBins=Map[With[{rects=Cases[#,_Rectangle,All]},Table[{midpoint,Lookup[Association[Map[(#[[1,1]]+1)->#[[2,2]]&,rects]],midpoint,0.]},{midpoint,allBinMidpoints}]]&,h];*)


(* ::Input:: *)
(*allBins[[All,13]]*)
(*allBins[[All,13,2]]*)
(*allBins[[All,13,2]][[1;;2]]*)


(* ::Input:: *)
(*Table[*)
(*Sum[*)
(*With[{f=allBins[[All,i,2]][[j]],g=allBins[[All,i,2]][[j+1]]},*)
(*Limit[Abs[f-g]/(f+x),x->g]]*)
(*,{i,1,Length[allBins[[1]]]}]*)
(*,{j,1,Length[allBins]-1}]*)


(* ::Section::Closed:: *)
(*L=101 path Lengths histograms (From Cluster)*)


(* ::Input:: *)
(*lbar={}*)
(*h={}*)
(*\[Epsilon]=1*)


(* ::Subsection::Closed:: *)
(*1e-2 *)


(* ::Input:: *)
(*\[Epsilon]=\[Epsilon]+1*)


(* ::Input:: *)
(*(*rawData=Flatten@(ToExpression/@Import[FileNameJoin[{"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\extraction-Histogram.dat"}],"CSV"]); *)*)
(*(* MODIFY FILE NAME *)*)
(*pathWords=ReadList["D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d-FromCluster\\DifferentPrecisionAnalysis\\b=2_LRW-2d-square-lattice-data-101_repeat-1000000_tol-1e-2.csv",Word,RecordLists->True,WordSeparators->{","," ","\t"}];*)
(*Length@pathWords*)


(* ::Input:: *)
(*pathWords[[1;;2]]*)


(* ::Input:: *)
(*pathLenghts=ToExpression[pathWords[[All,2;;]]];*)


(* ::Input:: *)
(*(*Force the data into a flat,contiguous machine-number array*)*)
(*pathLenghts=Developer`ToPackedArray[Flatten[pathLenghts]];*)
(**)
(*(*Verify it worked (should return True)*)*)
(*Developer`PackedArrayQ[pathLenghts]*)


(* ::Input:: *)
(*pathLenghts[[1;;2]]*)


(* ::Input:: *)
(*AppendTo[h,Histogram[pathLenghts,{1},"Probability",PlotRange->All,PlotLabel->Row[{"-Log[\[Epsilon]] = ",\[Epsilon]}]]]*)


(* ::Subsubsection:: *)
(*Mean*)


(* ::Input:: *)
(*AppendTo[lbar,Total@pathLenghts/Length[pathLenghts]//N]*)


(* ::Subsection::Closed:: *)
(*1e-3 *)


(* ::Input:: *)
(*\[Epsilon]=\[Epsilon]+1*)


(* ::Input:: *)
(*(*rawData=Flatten@(ToExpression/@Import[FileNameJoin[{"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\extraction-Histogram.dat"}],"CSV"]); *)*)
(*(* MODIFY FILE NAME *)*)
(*pathWords=ReadList["D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d-FromCluster\\DifferentPrecisionAnalysis\\b=2_LRW-2d-square-lattice-data-101_repeat-1000000_tol-1e-3.csv",Word,RecordLists->True,WordSeparators->{","," ","\t"}];*)
(*Length@pathWords*)


(* ::Input:: *)
(*pathWords[[1;;2]]*)


(* ::Input:: *)
(*pathLenghts=ToExpression[pathWords[[All,2;;]]];*)


(* ::Input:: *)
(*(*Force the data into a flat,contiguous machine-number array*)*)
(*pathLenghts=Developer`ToPackedArray[Flatten[pathLenghts]];*)
(**)
(*(*Verify it worked (should return True)*)*)
(*Developer`PackedArrayQ[pathLenghts]*)


(* ::Input:: *)
(*pathLenghts[[1;;2]]*)


(* ::Input:: *)
(*AppendTo[h,Histogram[pathLenghts,{1},"Probability",PlotRange->All,PlotLabel->Row[{"-Log[\[Epsilon]] = ",\[Epsilon]}]]]*)
(*Histogram[pathLenghts,{1},"PDF",PlotRange->{All,All}];*)


(* ::Subsubsection:: *)
(*Mean*)


(* ::Input:: *)
(*AppendTo[lbar,Total@pathLenghts/Length[pathLenghts]//N]*)


(* ::Subsection::Closed:: *)
(*1e-4 *)


(* ::Input:: *)
(*\[Epsilon]=\[Epsilon]+1*)


(* ::Input:: *)
(*(*rawData=Flatten@(ToExpression/@Import[FileNameJoin[{"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\extraction-Histogram.dat"}],"CSV"]); *)*)
(*(* MODIFY FILE NAME *)*)
(*pathWords=ReadList["D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d-FromCluster\\DifferentPrecisionAnalysis\\b=2_LRW-2d-square-lattice-data-101_repeat-1000000_tol-1e-4.csv",Word,RecordLists->True,WordSeparators->{","," ","\t"}];*)
(*Length@pathWords*)


(* ::Input:: *)
(*pathWords[[1;;2]]*)


(* ::Input:: *)
(*pathLenghts=ToExpression[pathWords[[All,2;;]]];*)


(* ::Input:: *)
(*(*Force the data into a flat,contiguous machine-number array*)*)
(*pathLenghts=Developer`ToPackedArray[Flatten[pathLenghts]];*)
(**)
(*(*Verify it worked (should return True)*)*)
(*Developer`PackedArrayQ[pathLenghts]*)


(* ::Input:: *)
(*pathLenghts[[1;;2]]*)


(* ::Input:: *)
(*AppendTo[h,Histogram[pathLenghts,{1},"Probability",PlotRange->All,PlotLabel->Row[{"-Log[\[Epsilon]] = ",\[Epsilon]}]]]*)
(*Histogram[pathLenghts,{1},"PDF",PlotRange->{All,All}];*)


(* ::Subsubsection:: *)
(*Mean*)


(* ::Input:: *)
(*AppendTo[lbar,Total@pathLenghts/Length[pathLenghts]//N]*)


(* ::Subsection::Closed:: *)
(*1e-5 *)


(* ::Input:: *)
(*\[Epsilon]=\[Epsilon]+1*)


(* ::Input:: *)
(*(*rawData=Flatten@(ToExpression/@Import[FileNameJoin[{"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\extraction-Histogram.dat"}],"CSV"]); *)*)
(*(* MODIFY FILE NAME *)*)
(*pathWords=ReadList["D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d-FromCluster\\DifferentPrecisionAnalysis\\b=2_LRW-2d-square-lattice-data-101_repeat-1000000_tol-1e-5.csv",Word,RecordLists->True,WordSeparators->{","," ","\t"}];*)
(*Length@pathWords*)


(* ::Input:: *)
(*pathWords[[1;;2]]*)


(* ::Input:: *)
(*pathLenghts=ToExpression[pathWords[[All,2;;]]];*)


(* ::Input:: *)
(*(*Force the data into a flat,contiguous machine-number array*)*)
(*pathLenghts=Developer`ToPackedArray[Flatten[pathLenghts]];*)
(**)
(*(*Verify it worked (should return True)*)*)
(*Developer`PackedArrayQ[pathLenghts]*)


(* ::Input:: *)
(*pathLenghts[[1;;2]]*)


(* ::Input:: *)
(*AppendTo[h,Histogram[pathLenghts,{1},"Probability",PlotRange->All,PlotLabel->Row[{"-Log[\[Epsilon]] = ",\[Epsilon]}]]]*)
(*Histogram[pathLenghts,{1},"PDF",PlotRange->{All,All}];*)


(* ::Subsubsection:: *)
(*Mean*)


(* ::Input:: *)
(*AppendTo[lbar,Total@pathLenghts/Length[pathLenghts]//N]*)


(* ::Subsection::Closed:: *)
(*1e-6 *)


(* ::Input:: *)
(*\[Epsilon]=\[Epsilon]+1*)


(* ::Input:: *)
(*(*rawData=Flatten@(ToExpression/@Import[FileNameJoin[{"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\extraction-Histogram.dat"}],"CSV"]); *)*)
(*(* MODIFY FILE NAME *)*)
(*pathWords=ReadList["D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d-FromCluster\\DifferentPrecisionAnalysis\\b=2_LRW-2d-square-lattice-data-101_repeat-1000000_tol-1e-6.csv",Word,RecordLists->True,WordSeparators->{","," ","\t"}];*)
(*Length@pathWords*)


(* ::Input:: *)
(*pathWords[[1;;2]]*)


(* ::Input:: *)
(*pathLenghts=ToExpression[pathWords[[All,2;;]]];*)


(* ::Input:: *)
(*(*Force the data into a flat,contiguous machine-number array*)*)
(*pathLenghts=Developer`ToPackedArray[Flatten[pathLenghts]];*)
(**)
(*(*Verify it worked (should return True)*)*)
(*Developer`PackedArrayQ[pathLenghts]*)


(* ::Input:: *)
(*pathLenghts[[1;;2]]*)


(* ::Input:: *)
(*AppendTo[h,Histogram[pathLenghts,{1},"Probability",PlotRange->All,PlotLabel->Row[{"-Log[\[Epsilon]] = ",\[Epsilon]}]]]*)
(*Histogram[pathLenghts,{1},"PDF",PlotRange->{All,All}];*)


(* ::Subsubsection:: *)
(*Mean*)


(* ::Input:: *)
(*AppendTo[lbar,Total@pathLenghts/Length[pathLenghts]//N]*)


(* ::Subsection::Closed:: *)
(*1e-7 *)


(* ::Input:: *)
(*\[Epsilon]=\[Epsilon]+1*)


(* ::Input:: *)
(*(*rawData=Flatten@(ToExpression/@Import[FileNameJoin[{"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\extraction-Histogram.dat"}],"CSV"]); *)*)
(*(* MODIFY FILE NAME *)*)
(*pathWords=ReadList["D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d-FromCluster\\DifferentPrecisionAnalysis\\b=2_LRW-2d-square-lattice-data-101_repeat-1000000_tol-1e-7.csv",Word,RecordLists->True,WordSeparators->{","," ","\t"}];*)
(*Length@pathWords*)


(* ::Input:: *)
(*pathWords[[1;;2]]*)


(* ::Input:: *)
(*pathLenghts=ToExpression[pathWords[[All,2;;]]];*)


(* ::Input:: *)
(*(*Force the data into a flat,contiguous machine-number array*)*)
(*pathLenghts=Developer`ToPackedArray[Flatten[pathLenghts]];*)
(**)
(*(*Verify it worked (should return True)*)*)
(*Developer`PackedArrayQ[pathLenghts]*)


(* ::Input:: *)
(*pathLenghts[[1;;2]]*)


(* ::Input:: *)
(*AppendTo[h,Histogram[pathLenghts,{1},"Probability",PlotRange->All,PlotLabel->Row[{"-Log[\[Epsilon]] = ",\[Epsilon]}]]]*)
(*Histogram[pathLenghts,{1},"PDF",PlotRange->{All,All}];*)


(* ::Subsubsection:: *)
(*Mean*)


(* ::Input:: *)
(*AppendTo[lbar,Total@pathLenghts/Length[pathLenghts]//N]*)


(* ::Subsection::Closed:: *)
(*1e-8 *)


(* ::Input:: *)
(*\[Epsilon]=\[Epsilon]+1*)


(* ::Input:: *)
(*(*rawData=Flatten@(ToExpression/@Import[FileNameJoin[{"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\extraction-Histogram.dat"}],"CSV"]); *)*)
(*(* MODIFY FILE NAME *)*)
(*pathWords=ReadList["D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d-FromCluster\\DifferentPrecisionAnalysis\\b=2_LRW-2d-square-lattice-data-101_repeat-1000000_tol-1e-8.csv",Word,RecordLists->True,WordSeparators->{","," ","\t"}];*)
(*Length@pathWords*)


(* ::Input:: *)
(*pathWords[[1;;2]]*)


(* ::Input:: *)
(*pathLenghts=ToExpression[pathWords[[All,2;;]]];*)


(* ::Input:: *)
(*(*Force the data into a flat,contiguous machine-number array*)*)
(*pathLenghts=Developer`ToPackedArray[Flatten[pathLenghts]];*)
(**)
(*(*Verify it worked (should return True)*)*)
(*Developer`PackedArrayQ[pathLenghts]*)


(* ::Input:: *)
(*pathLenghts[[1;;2]]*)


(* ::Input:: *)
(*AppendTo[h,Histogram[pathLenghts,{1},"Probability",PlotRange->All,PlotLabel->Row[{"-Log[\[Epsilon]] = ",\[Epsilon]}]]]*)
(*Histogram[pathLenghts,{1},"PDF",PlotRange->{All,All}];*)


(* ::Subsubsection:: *)
(*Mean*)


(* ::Input:: *)
(*AppendTo[lbar,Total@pathLenghts/Length[pathLenghts]//N]*)


(* ::Subsection::Closed:: *)
(*1e-9 *)


(* ::Input:: *)
(*\[Epsilon]=\[Epsilon]+1*)


(* ::Input:: *)
(*(*rawData=Flatten@(ToExpression/@Import[FileNameJoin[{"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\extraction-Histogram.dat"}],"CSV"]); *)*)
(*(* MODIFY FILE NAME *)*)
(*pathWords=ReadList["D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d-FromCluster\\DifferentPrecisionAnalysis\\b=2_LRW-2d-square-lattice-data-101_repeat-1000000_tol-1e-9.csv",Word,RecordLists->True,WordSeparators->{","," ","\t"}];*)
(*Length@pathWords*)


(* ::Input:: *)
(*pathWords[[1;;2]]*)


(* ::Input:: *)
(*pathLenghts=ToExpression[pathWords[[All,2;;]]];*)


(* ::Input:: *)
(*(*Force the data into a flat,contiguous machine-number array*)*)
(*pathLenghts=Developer`ToPackedArray[Flatten[pathLenghts]];*)
(**)
(*(*Verify it worked (should return True)*)*)
(*Developer`PackedArrayQ[pathLenghts]*)


(* ::Input:: *)
(*pathLenghts[[1;;2]]*)


(* ::Input:: *)
(*AppendTo[h,Histogram[pathLenghts,{1},"Probability",PlotRange->All,PlotLabel->Row[{"-Log[\[Epsilon]] = ",\[Epsilon]}]]]*)
(*Histogram[pathLenghts,{1},"PDF",PlotRange->{All,All}];*)


(* ::Subsubsection:: *)
(*Mean*)


(* ::Input:: *)
(*AppendTo[lbar,Total@pathLenghts/Length[pathLenghts]//N]*)


(* ::Subsection::Closed:: *)
(*1e-10 *)


(* ::Input:: *)
(*\[Epsilon]=\[Epsilon]+1*)


(* ::Input:: *)
(*(*rawData=Flatten@(ToExpression/@Import[FileNameJoin[{"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\extraction-Histogram.dat"}],"CSV"]); *)*)
(*(* MODIFY FILE NAME *)*)
(*pathWords=ReadList["D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d-FromCluster\\DifferentPrecisionAnalysis\\b=2_LRW-2d-square-lattice-data-101_repeat-1000000_tol-1e-10.csv",Word,RecordLists->True,WordSeparators->{","," ","\t"}];*)
(*Length@pathWords*)


(* ::Input:: *)
(*pathWords[[1;;2]]*)


(* ::Input:: *)
(*pathLenghts=ToExpression[pathWords[[All,2;;]]];*)


(* ::Input:: *)
(*(*Force the data into a flat,contiguous machine-number array*)*)
(*pathLenghts=Developer`ToPackedArray[Flatten[pathLenghts]];*)
(**)
(*(*Verify it worked (should return True)*)*)
(*Developer`PackedArrayQ[pathLenghts]*)


(* ::Input:: *)
(*pathLenghts[[1;;2]]*)


(* ::Input:: *)
(*AppendTo[h,Histogram[pathLenghts,{1},"Probability",PlotRange->All,PlotLabel->Row[{"-Log[\[Epsilon]] = ",\[Epsilon]}]]]*)
(*Histogram[pathLenghts,{1},"PDF",PlotRange->{All,All}];*)


(* ::Subsubsection:: *)
(*Mean*)


(* ::Input:: *)
(*AppendTo[lbar,Total@pathLenghts/Length[pathLenghts]//N]*)


(* ::Subsection::Closed:: *)
(*1e-11 *)


(* ::Input:: *)
(*\[Epsilon]=\[Epsilon]+1*)


(* ::Input:: *)
(*(*rawData=Flatten@(ToExpression/@Import[FileNameJoin[{"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\extraction-Histogram.dat"}],"CSV"]); *)*)
(*(* MODIFY FILE NAME *)*)
(*pathWords=ReadList["D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d-FromCluster\\DifferentPrecisionAnalysis\\b=2_LRW-2d-square-lattice-data-101_repeat-1000000_tol-1e-11.csv",Word,RecordLists->True,WordSeparators->{","," ","\t"}];*)
(*Length@pathWords*)


(* ::Input:: *)
(*pathWords[[1;;2]]*)


(* ::Input:: *)
(*pathLenghts=ToExpression[pathWords[[All,2;;]]];*)


(* ::Input:: *)
(*(*Force the data into a flat,contiguous machine-number array*)*)
(*pathLenghts=Developer`ToPackedArray[Flatten[pathLenghts]];*)
(**)
(*(*Verify it worked (should return True)*)*)
(*Developer`PackedArrayQ[pathLenghts]*)


(* ::Input:: *)
(*pathLenghts[[1;;2]]*)


(* ::Input:: *)
(*AppendTo[h,Histogram[pathLenghts,{1},"Probability",PlotRange->All,PlotLabel->Row[{"-Log[\[Epsilon]] = ",\[Epsilon]}]]]*)
(*Histogram[pathLenghts,{1},"PDF",PlotRange->{All,All}];*)


(* ::Subsubsection:: *)
(*Mean*)


(* ::Input:: *)
(*AppendTo[lbar,Total@pathLenghts/Length[pathLenghts]//N]*)


(* ::Subsection::Closed:: *)
(*1e-12 *)


(* ::Input:: *)
(*\[Epsilon]=\[Epsilon]+1*)


(* ::Input:: *)
(*(*rawData=Flatten@(ToExpression/@Import[FileNameJoin[{"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\extraction-Histogram.dat"}],"CSV"]); *)*)
(*(* MODIFY FILE NAME *)*)
(*pathWords=ReadList["D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d-FromCluster\\DifferentPrecisionAnalysis\\b=2_LRW-2d-square-lattice-data-101_repeat-1000000_tol-1e-12.csv",Word,RecordLists->True,WordSeparators->{","," ","\t"}];*)
(*Length@pathWords*)


(* ::Input:: *)
(*pathWords[[1;;2]]*)


(* ::Input:: *)
(*pathLenghts=ToExpression[pathWords[[All,2;;]]];*)


(* ::Input:: *)
(*(*Force the data into a flat,contiguous machine-number array*)*)
(*pathLenghts=Developer`ToPackedArray[Flatten[pathLenghts]];*)
(**)
(*(*Verify it worked (should return True)*)*)
(*Developer`PackedArrayQ[pathLenghts]*)


(* ::Input:: *)
(*pathLenghts[[1;;2]]*)


(* ::Input:: *)
(*AppendTo[h,Histogram[pathLenghts,{1},"Probability",PlotRange->All,PlotLabel->Row[{"-Log[\[Epsilon]] = ",\[Epsilon]}]]];*)
(*Histogram[pathLenghts,{1},"PDF",PlotRange->{All,All}];*)


(* ::Subsubsection:: *)
(*Mean*)


(* ::Input:: *)
(*AppendTo[lbar,Total@pathLenghts/Length[pathLenghts]//N]*)


(* ::Subsection::Closed:: *)
(*1e-13 *)


(* ::Input:: *)
(*\[Epsilon]=\[Epsilon]+1*)


(* ::Input:: *)
(*(*rawData=Flatten@(ToExpression/@Import[FileNameJoin[{"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\extraction-Histogram.dat"}],"CSV"]); *)*)
(*(* MODIFY FILE NAME *)*)
(*pathWords=ReadList["D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d-FromCluster\\DifferentPrecisionAnalysis\\b=2_LRW-2d-square-lattice-data-101_repeat-1000000_tol-1e-13.csv",Word,RecordLists->True,WordSeparators->{","," ","\t"}];*)
(*Length@pathWords*)


(* ::Input:: *)
(*pathWords[[1;;2]]*)


(* ::Input:: *)
(*pathLenghts=ToExpression[pathWords[[All,2;;]]];*)


(* ::Input:: *)
(*(*Force the data into a flat,contiguous machine-number array*)*)
(*pathLenghts=Developer`ToPackedArray[Flatten[pathLenghts]];*)
(**)
(*(*Verify it worked (should return True)*)*)
(*Developer`PackedArrayQ[pathLenghts]*)


(* ::Input:: *)
(*pathLenghts[[1;;2]]*)


(* ::Input:: *)
(*AppendTo[h,Histogram[pathLenghts,{1},"Probability",PlotRange->All,PlotLabel->Row[{"-Log[\[Epsilon]] = ",\[Epsilon]}]]];*)
(*Histogram[pathLenghts,{1},"PDF",PlotRange->{All,All}];*)


(* ::Subsubsection:: *)
(*Mean*)


(* ::Input:: *)
(*AppendTo[lbar,Total@pathLenghts/Length[pathLenghts]//N]*)


(* ::Subsection::Closed:: *)
(*1e-14 *)


(* ::Input:: *)
(*\[Epsilon]=\[Epsilon]+1*)


(* ::Input:: *)
(*(*rawData=Flatten@(ToExpression/@Import[FileNameJoin[{"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\extraction-Histogram.dat"}],"CSV"]); *)*)
(*(* MODIFY FILE NAME *)*)
(*pathWords=ReadList["D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d-FromCluster\\DifferentPrecisionAnalysis\\b=2_LRW-2d-square-lattice-data-101_repeat-1000000_tol-1e-14.csv",Word,RecordLists->True,WordSeparators->{","," ","\t"}];*)
(*Length@pathWords*)


(* ::Input:: *)
(*pathWords[[1;;2]]*)


(* ::Input:: *)
(*pathLenghts=ToExpression[pathWords[[All,2;;]]];*)


(* ::Input:: *)
(*(*Force the data into a flat,contiguous machine-number array*)*)
(*pathLenghts=Developer`ToPackedArray[Flatten[pathLenghts]];*)
(**)
(*(*Verify it worked (should return True)*)*)
(*Developer`PackedArrayQ[pathLenghts]*)


(* ::Input:: *)
(*pathLenghts[[1;;2]]*)


(* ::Input:: *)
(*AppendTo[h,Histogram[pathLenghts,{1},"Probability",PlotRange->All,PlotLabel->Row[{"-Log[\[Epsilon]] = ",\[Epsilon]}]]];*)
(*Histogram[pathLenghts,{1},"PDF",PlotRange->{All,All}];*)


(* ::Subsubsection:: *)
(*Mean*)


(* ::Input:: *)
(*AppendTo[lbar,Total@pathLenghts/Length[pathLenghts]//N]*)


(* ::Subsection:: *)
(*Plot of lbar VS exponent*)


(* ::Input:: *)
(*lbarData=Table[{i+1,lbar[[i]]},{i,1,Length@lbar}]*)


(* ::Input:: *)
(*ListPlot[lbarData,AxesLabel->{"-Log[\[Epsilon]]","<l>"},PlotRange->All]*)


(* ::Input:: *)
(*(*it seems that already at \[Epsilon]=1e-4 we get a good result*)*)


(* ::Subsection::Closed:: *)
(*Histograms*)


(* ::Input:: *)
(*(*1. Force every histogram in your list to share the exact same Y-axis scale*)synchronizedHistograms=Map[Show[#,PlotRange->{{50,400},{0,0.0001}},ImageSize->220]&,h];*)
(**)
(*(*2. Display them instantly in a clean grid layout (3 columns)*)*)
(*Multicolumn[synchronizedHistograms,4,Appearance->"Framed"]*)


(* ::Subsection::Closed:: *)
(*Frequency of the mean*)


(* ::Input:: *)
(*meanHistory=Table[With[{mean=Select[Cases[#,_Rectangle,All],#[[1,1]]<=lbar[[i]]<=#[[2,1]]&]},(*Calculate the Ceiling of the tallest bin to get the value*)mean[[1,2,2]]]&@h[[i]],*)
(*{i,1,Length[h]}];*)


(* ::Input:: *)
(*meanHistoryData=Table[{i+1,meanHistory[[i]]},{i,1,Length@lbar}]*)


(* ::Input:: *)
(*ListPlot[meanHistoryData,AxesLabel->{"-Log[\[Epsilon]]","mean_freq"},PlotRange->{All,{0.02,0.023}}]*)


(* ::Subsection:: *)
(*Frequency of the mode*)


(* ::Input:: *)
(*(*1. Find all rectangles inside the graphic object*)allRectangles=Cases[h[[1]],_Rectangle,All];*)
(**)
(*(*2. Find the rectangle with the maximum Y-max (highest bar)*)*)
(*tallestRectangle=MaximalBy[allRectangles,#[[2,2]]&][[1]];*)
(**)
(*(*3. Extract its X boundaries to identify the bin value*)*)
(*modeBin={tallestRectangle[[{1,2},1]]/.Rectangle->Sequence}*)
(**)
(*mode=Mean@modeBin*)


(* ::Input:: *)
(*modesHistory=Map[With[{tallest=MaximalBy[Cases[#,_Rectangle,All],#[[2,2]]&][[1]]},(*Calculate the floor/midpoint of the tallest bin to get the value*)Ceiling[tallest[[1,1]]]]&,h]*)


(* ::Input:: *)
(*(*The mode does not change*)*)


(* ::Input:: *)
(*binModeHeights=Table[With[{rects=SelectFirst[Cases[#,_Rectangle,All],#[[1,1]]<=modesHistory[[i]]<=#[[2,1]]&]},rects[[2,2]]]&@h[[i]],*)
(*{i,1,Length[h]}]*)


(* ::Input:: *)
(*binModeHeightsdata=Table[{i+1,binModeHeights[[i]]},{i,1,Length@binModeHeights}]*)


(* ::Input:: *)
(*ListPlot[binModeHeightsdata,AxesLabel->{"-Log[\[Epsilon]]","mean_freq"},PlotRange->{All,{0.025,0.031}}]*)


(* ::Subsection:: *)
(*Observable: \[CapitalSigma]_l (|f(l)-g(l)|)/(f(l)+g(l))*)


(* ::Input:: *)
(*(*1. Find all rectangles that exist*)*)
(*rectangles=Cases[h[[1]],_Rectangle,All];*)
(**)
(*(*2. Find the full absolute limits of the X-axis from the plot geometry*)*)
(*xMin=0.;*)
(*xMax=450.;*)
(**)
(*(*3. Reconstruct your full step-by-step bin sequence (assuming bin width=1)*)*)
(*allBinMidpoints=Range[xMin+0.5,xMax-0.5,1.];*)


(* ::Input:: *)
(*(*4. Build a lookup association from the rectangles that DO exist*)*)
(*existingHeights=Association[Map[(#[[1,1]]+1)->#[[2,2]]&,rectangles]];*)
(**)
(*(*5. Map across all possible bins,filling in 0. if the bin was empty*)*)
(*fullHistogramData=Table[{midpoint,Lookup[existingHeights,midpoint,0.]},{midpoint,allBinMidpoints}];*)


(* ::Input:: *)
(*With[{rects=Cases[#,_Rectangle,All]},Table[{midpoint,Lookup[Association[Map[(#[[1,1]]+1)->#[[2,2]]&,rects]],midpoint,0.]},{midpoint,allBinMidpoints}]]&@h[[5]]*)


(* ::Input:: *)
(*Length[h]*)


(* ::Input:: *)
(*allBins=Map[With[{rects=Cases[#,_Rectangle,All]},Table[{midpoint,Lookup[Association[Map[(#[[1,1]]+1)->#[[2,2]]&,rects]],midpoint,0.]},{midpoint,allBinMidpoints}]]&,h];*)


(* ::Input:: *)
(*allBins[[All,13]]*)
(*allBins[[All,13,2]]*)
(*allBins[[All,13,2]][[1;;2]]*)


(* ::Input:: *)
(*obs=Table[*)
(*Sum[*)
(*With[{f=allBins[[All,i,2]][[j]],g=allBins[[All,i,2]][[j+1]]},*)
(*Limit[Abs[f-g]/(f+x),x->g]]*)
(*,{i,1,Length[allBins[[1]]]}]*)
(*,{j,1,Length[allBins]-1}]*)


(* ::Input:: *)
(*obsData=Table[{i+1,#[[i]]},{i,1,Length@#}]&@obs*)


(* ::Input:: *)
(*ListPlot[obsData,AxesLabel->{"-Log[\[Epsilon]]","mean_freq"},PlotRange->{All,{30,60}}]*)


(* ::Title:: *)
(*Nice path Lengths analysis (MOST OF THE FILES ARE IN A ZIP)*)


(* ::Input:: *)
(*Quit[]*)


(* ::Section::Closed:: *)
(*Function definitions*)


(* ::Input::Initialization:: *)
ParallelMap[Plus,{1,2}]


(* ::Input::Initialization:: *)
ClearAll[Rotate90,ReflectX,GetSymmetryFamily];

(*Rotates 90\[Degree] counter-clockwise around the first point of the path*)Rotate90[path_List]:=With[{center=path[[1]]},Map[({-(#[[2]]-center[[2]])+center[[1]],(#[[1]]-center[[1]])+center[[2]]})&,path]]

(*Reflects horizontally across the vertical line passing through the first point (x->-x)*)
ReflectX[path_List]:=With[{center=path[[1]]},Map[({-(#[[1]]-center[[1]])+center[[1]],#[[2]]})&,path]]

(*Generates all 8 unique rotation and reflection variants of a path*)GetSymmetryFamily[path_List]:=With[{rotations=NestList[Rotate90,path,3]},
Join[rotations,Map[ReflectX,rotations]]]


(* ::Input::Initialization:: *)
ClearAll[AnalyzeSymmetricPaths]

(*Comparison test:Returns True if path2 is a valid symmetry transformation of path1*)AreSymmetricQ[path1_List,path2_List]:=MemberQ[GetSymmetryFamily[path1],path2];

(*The master function to process your paths array*)
AnalyzeSymmetricPaths[allPaths_List]:=Module[{grouped,tally},(*Group identical paths under rotation/reflection*)grouped=Gather[allPaths,AreSymmetricQ];
(*Return a structured dataset:{Unique Representative Path,Family Occurrence Count}*)tally=Table[{First[g],Length[g]},{g,grouped}];

Reverse[SortBy[tally,Last]]
];


(* ::Input::Initialization:: *)
MergeTwoTallies[tally1_List,tally2_List]:=Module[{combined=tally1,matched,path2,count2},
Do[(*Extract components explicitly using Pattern Matching*)
path2=item[[1]];
count2=item[[2]];
(*Your existing logic*)
matched=Position[combined,_?(AreSymmetricQ[#[[1]],path2]&),{1},1]//Quiet;

If[Length[matched]>0,
combined[[matched[[1,1]],2]]+=count2,
AppendTo[combined,{path2,count2}]
],

{item,tally2} (*Loops through each full pair in tally2*)];

combined
];


(* ::Input::Initialization:: *)
IterativeSymmetricTally[allPaths_List,batchSize_Integer:100]:=Module[{batches,currentTallies},
DistributeDefinitions[AnalyzeSymmetricPaths,MergeTwoTallies,AreSymmetricQ];
(*1. Initial Step: partition and analyze individual batches*)
batches=Partition[allPaths,UpTo[batchSize]];
currentTallies=ParallelMap[AnalyzeSymmetricPaths,batches];

(*2. Reduction Loop: merge pairs of batches until only 1 master tally is left*)
While[Length[currentTallies]>1,

currentTallies=If[OddQ[Length[currentTallies]],

(*If odd number of batches,hold the last one and merge the pairs*)Append[Map[Apply[MergeTwoTallies],Partition[Drop[currentTallies,-1],2]],Last[currentTallies]],

(*If even,merge all pairs straight across*)
Map[Apply[MergeTwoTallies],Partition[currentTallies,2]]
];
Print["Remaining batch blocks to merge: ",Length[currentTallies]];
];
(*3. Final Sort by count in descending order*)
Reverse[SortBy[First[currentTallies],Last]]
];


(* ::Title:: *)
(*b=1*)


(* ::Section::Closed:: *)
(*L=4 (n=9, active is 4) path Lengths histograms (b=1_LRW-2d-square-lattice-data-9_repeat-1000000_tol-1e-10-LogSpacing.csv (x2))*)


(* ::Input:: *)
(*n=9;*)
(*(*1. Read as text and split by double carriage returns/newlines*)rawBlocks=Import["D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d\\b=1_LRW-2d-square-lattice-data-9_repeat-1000000_tol-1e-10-LogSpacing.csv","CSV"];*)
(*(*2. Convert each separate text block cleanly into an array of coordinate pairs*)*)
(*paths=Developer`ToPackedArray[rawBlocks];*)
(*paths[[1;;10]]*)
(*pathDistr=paths[[All,2]];*)
(*Length@pathDistr*)


(* ::Input:: *)
(*Histogram[#,Length[#],"PDF",PlotRange->All]&@pathDistr*)


(* ::Input:: *)
(*(*1. Calculate relative frequencies ("probabilities")*)*)
(*Sort@Tally[pathDistr]*)
(*{pathDistr2,symmetricCounts}=Transpose[%]*)


(* ::Input:: *)
(*pathDistr2==Sort@DeleteDuplicates@pathDistr*)


(* ::Input:: *)
(*totalPaths=Length[pathDistr];*)
(*symmetricProbabilities=symmetricCounts/totalPaths;*)
(**)


(* ::Input:: *)
(*(*3. Generate the BarChart with values on top and unique symmetry families on the bottom*)BarChart[symmetricProbabilities[[#]],ChartLabels->Placed[pathDistr2,Axis],LabelingFunction->(Placed[Row[{NumberForm[100.*#1,{5,2}],"%"}],Above]&),Frame->True,FrameLabel->{"Unique Paths (Symmetry Families Included)","Relative Occurrence (Probability)"},PlotLabel->"Symmetric Path Distribution",ChartStyle->RGBColor[0.87,0.71,0.34],ImageSize->900(*,PlotRange->{All,{0,Max[symmetricProbabilities]*1.15}}*)]&@Span[All]*)


(* ::Text:: *)
(*Compared to the analytical result below, (3->16.79%,4->38.1154%) we are a little off *)


(* ::Section:: *)
(*L=4 (n=9, active is 4) path Lengths histograms (b=1_LRW-2d-square-lattice-data-9_repeat-1000000_tol-1e-10-LogSpacing-FullUpdatesOnly.csv (x2))	OK*)


(* ::Input:: *)
(*n=9;*)
(*(*1. Read as text and split by double carriage returns/newlines*)rawBlocks=Import["D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d\\b=1_LRW-2d-square-lattice-data-9_repeat-1000000_tol-1e-10-LogSpacing-FullUpdatesOnly.csv","CSV"];*)
(*(*2. Convert each separate text block cleanly into an array of coordinate pairs*)*)
(*paths=Developer`ToPackedArray[rawBlocks];*)
(*paths[[1;;10]]*)
(*pathDistr=paths[[All,2]];*)
(*Length@pathDistr*)


(* ::Input:: *)
(*Histogram[#,Length[#],"PDF",PlotRange->All]&@pathDistr*)


(* ::Input:: *)
(*(*1. Calculate relative frequencies ("probabilities")*)*)
(*Sort@Tally[pathDistr]*)
(*{pathDistr2,symmetricCounts}=Transpose[%]*)


(* ::Input:: *)
(*pathDistr2==Sort@DeleteDuplicates@pathDistr*)


(* ::Input:: *)
(*Mean[pathDistr]//N *)
(*4.718431`(*WITH 100000*)*)


(* ::Input:: *)
(*totalPaths=Length[pathDistr];*)
(*symmetricProbabilities=symmetricCounts/totalPaths;*)
(**)


(* ::Input:: *)
(*(*3. Generate the BarChart with values on top and unique symmetry families on the bottom*)BarChart[symmetricProbabilities[[#]],ChartLabels->Placed[pathDistr2,Axis],LabelingFunction->(Placed[Row[{NumberForm[100.*#1,{5,2}],"%"}],Above]&),Frame->True,FrameLabel->{"Unique Paths (Symmetry Families Included)","Relative Occurrence (Probability)"},PlotLabel->"Symmetric Path Distribution",ChartStyle->RGBColor[0.87,0.71,0.34],ImageSize->900(*,PlotRange->{All,{0,Max[symmetricProbabilities]*1.15}}*)]&@Span[All]*)


(* ::Text:: *)
(*Compared to the analytical result below, (3->16.79%,4->33.672%) we are a little off, but it's nice!! (gets better with increasing data)*)


(* ::Section::Closed:: *)
(*L=4 (n=9, active is 4) path Lengths histograms (b=1_LRW-2d-square-lattice-data-9_repeat-500000_tol-1e-10-LogSpacing-FullUpdatesEvery3.csv)		OK*)


(* ::Input:: *)
(*n=9;*)
(*(*1. Read as text and split by double carriage returns/newlines*)rawBlocks=Import["D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d\\b=1_LRW-2d-square-lattice-data-9_repeat-500000_tol-1e-10-LogSpacing-FullUpdatesEvery3.csv","CSV"];*)
(*(*2. Convert each separate text block cleanly into an array of coordinate pairs*)*)
(*paths=Developer`ToPackedArray[rawBlocks];*)
(*paths[[1;;10]]*)
(*pathDistr=paths[[All,2]];*)
(*Length@pathDistr*)


(* ::Input:: *)
(*Histogram[#,Length[#],"PDF",PlotRange->All]&@pathDistr*)


(* ::Input:: *)
(*(*1. Calculate relative frequencies ("probabilities")*)*)
(*Sort@Tally[pathDistr]*)
(*{pathDistr2,symmetricCounts}=Transpose[%]*)


(* ::Input:: *)
(*pathDistr2==Sort@DeleteDuplicates@pathDistr*)


(* ::Input:: *)
(*Mean[pathDistr]//N *)
(*4.718431`(*WITH 100000*)*)


(* ::Input:: *)
(*totalPaths=Length[pathDistr];*)
(*symmetricProbabilities=symmetricCounts/totalPaths;*)
(**)


(* ::Input:: *)
(*(*3. Generate the BarChart with values on top and unique symmetry families on the bottom*)BarChart[symmetricProbabilities[[#]],ChartLabels->Placed[pathDistr2,Axis],LabelingFunction->(Placed[Row[{NumberForm[100.*#1,{5,2}],"%"}],Above]&),Frame->True,FrameLabel->{"Unique Paths (Symmetry Families Included)","Relative Occurrence (Probability)"},PlotLabel->"Symmetric Path Distribution",ChartStyle->RGBColor[0.87,0.71,0.34],ImageSize->900(*,PlotRange->{All,{0,Max[symmetricProbabilities]*1.15}}*)]&@Span[All]*)


(* ::Text:: *)
(*Compared to the analytical result below, (3->16.79%,4->33.672%) we are a little off, but it's nice!! (gets better with increasing data)*)


(* ::Section::Closed:: *)
(*L=5 (active is 3) path Lengths histograms (b-1LRW-n5.csv )*)


(* ::Input:: *)
(*n=3;*)
(**)
(*(*1. Read as text and split by double carriage returns/newlines*)rawBlocks=Rest[StringSplit[Import["D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\FullPaths\\b-1LRW-n5.csv","Text"],"\n\n"|"\r\n\r\n"]];*)
(*(*2. Convert each separate text block cleanly into an array of coordinate pairs*)*)
(*paths=ParallelMap[Drop[ImportString[#,"CSV"],{},-2]&,rawBlocks];*)
(*paths[[-1]];*)
(*Length@paths*)


(* ::Input:: *)
(*(*Render all paths simultaneously with distinct indexed colors*)Graphics[{Thick,MapIndexed[{ColorData[97][#2[[1]]],Line[#1]}&,paths]},Frame->True,Axes->False,ImageSize->500,FrameLabel->{"X Coordinate","Y Coordinate"}];*)


(* ::Subsection:: *)
(*No symmetry taken into account*)


(* ::Input:: *)
(*(*2. Count the occurrences of each unique path*)*)
(*pathCounts=Tally[paths];*)
(*pathCounts[[1]]*)
(**)
(*pathCounts=Reverse[SortBy[pathCounts,Last]];*)
(*pathCounts[[1]]*)
(*(*Output format:{{path1,count1},{path2,count2},...}*)*)


(* ::Input:: *)
(*allPoints=Flatten[pathCounts[[All,1]],1];*)
(*{xMin,xMax}={Min[allPoints[[All,1]]],Max[allPoints[[All,1]]]};*)
(*{yMin,yMax}={Min[allPoints[[All,2]]],Max[allPoints[[All,2]]]};*)
(**)
(*globalPlotRange={{xMin-0.5,xMax+0.5},{yMin-0.5,yMax+0.5}};*)
(*globalGridLines={Range[xMin,xMax,1],Range[yMin,yMax,1]};*)
(**)
(*Graphics[{Thick,Line[#[[1]]],{RGBColor[1, 0, 0],Circle[{xMax/2+0.5,yMax/2+0.5},xMax/2-0.5]}},PlotRange->globalPlotRange,GridLines->globalGridLines,Frame->True,Axes->False,ImageSize->100,FrameLabel->{"X","Y"},PlotLabel->Row[{#[[2]]}]]&/@pathCounts[[All]]*)


(* ::Input:: *)
(*(*1. Calculate relative frequencies ("probabilities")*)*)
(*totalPaths=Length[paths];*)
(*countsOnly=pathCounts[[All,2]];*)
(*probabilities=countsOnly/totalPaths;*)
(**)
(*(*2. Convert each path (series of points) into a clean text label*)*)
(*(*Shortens coordinates to strings like "{(0,1),(1,2)}"*)*)
(*pathLabels=Map[Row[Map[RowBox[{"{",Row[#,","],"}"}]//DisplayForm&,#],", "]&,pathCounts[[All,1]]];*)
(**)
(*pathLabels=ParallelMap[Graphics[{Thick,Line[#],{RGBColor[1, 0, 0],Circle[{xMax/2+0.5,yMax/2+0.5},xMax/2-0.5]}},*)
(*PlotRange->globalPlotRange,PlotRangePadding->0.2,*)
(*GridLines->globalGridLines,*)
(*Frame->True,FrameTicks->None,Axes->False,*)
(*ImageSize->200]&*)
(*,pathCounts[[All,1]]];*)


(* ::Input:: *)
(*(*3. Generate the BarChart with values on top and unique symmetry families on the bottom*)*)
(*BarChart[probabilities,*)
(*ChartLabels->Placed[pathLabels,Axis],LabelingFunction->(Placed[Row[{NumberForm[100.*#1,{5,2}],"%"}],Above]&),Frame->True,FrameLabel->{"Paths","Relative Occurrence (Probability)"},PlotLabel->"Path Distribution",*)
(*ChartStyle->RGBColor[0.87, 0.71, 0.34],ImageSize->900,PlotRange->{All,{0,Max[probabilities]*1.15}}]*)


(* ::Input:: *)
(*(*In accordance with the expected homogeneus 1/4*)*)


(* ::Section::Closed:: *)
(*L=5 (active is 3) path Lengths histograms (b-1LRW-n5-UpdatedStopCond.csv) *)


(* ::Subsection:: *)
(*All paths*)


(* ::Input:: *)
(*n=3;*)
(*(*1. Read as text and split by double carriage returns/newlines*)rawBlocks=Rest[StringSplit[Import["D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\FullPaths\\b-1LRW-n5-UpdatedStopCond.csv","Text"],"\n\n"|"\r\n\r\n"]];*)
(*(*2. Convert each separate text block cleanly into an array of coordinate pairs*)*)
(*paths=ParallelMap[Drop[ImportString[#,"CSV"],{},-2]&,rawBlocks];*)
(*paths[[-1]];*)
(*Length@paths*)


(* ::Input:: *)
(*(*Render all paths simultaneously with distinct indexed colors*)Graphics[{Thick,MapIndexed[{ColorData[97][#2[[1]]],Line[#1]}&,paths]},Frame->True,Axes->False,ImageSize->500,FrameLabel->{"X Coordinate","Y Coordinate"}];*)
(**)
(*center=(n+1)/2;*)
(**)
(*Graphics[{Style[Circle[{center,center},center-1],Red],Thick,MapIndexed[{ColorData[97][#2[[1]]],Line[#1]}&,paths[[All]]],Point[{center,center}]},Frame->True,Axes->False,ImageSize->500,FrameLabel->{"X Coordinate","Y Coordinate"},PlotRange->All,GridLines->{Range[0,center*2],Range[0,center*2]}];*)


(* ::Input:: *)
(*allPoints=Flatten[paths,1];*)
(*{xMin,xMax}={Min[allPoints[[All,1]]],Max[allPoints[[All,1]]]};*)
(*{yMin,yMax}={Min[allPoints[[All,2]]],Max[allPoints[[All,2]]]};*)
(**)
(*globalPlotRange={{xMin-0.5,xMax+0.5},{yMin-0.5,yMax+0.5}};*)
(*globalGridLines={Range[xMin,xMax,1],Range[yMin,yMax,1]};*)
(**)
(**)
(*Graphics[{Thick,Line[#],{RGBColor[1, 0, 0],Circle[{xMax/2+0.5,yMax/2+0.5},xMax/2-0.5]}},PlotRange->globalPlotRange,GridLines->globalGridLines,Frame->True,Axes->False,ImageSize->110,FrameLabel->{"X","Y"}]&@paths[[1]]*)
(**)


(* ::Subsection:: *)
(*No symmetry taken into account*)


(* ::Input:: *)
(*(*2. Count the occurrences of each unique path*)*)
(*pathCounts=Tally[paths];*)
(*pathCounts[[1]]*)
(**)
(*pathCounts=Reverse[SortBy[pathCounts,Last]];*)
(*pathCounts[[1]]*)
(*(*Output format:{{path1,count1},{path2,count2},...}*)*)


(* ::Input:: *)
(*allPoints=Flatten[pathCounts[[All,1]],1];*)
(*{xMin,xMax}={Min[allPoints[[All,1]]],Max[allPoints[[All,1]]]};*)
(*{yMin,yMax}={Min[allPoints[[All,2]]],Max[allPoints[[All,2]]]};*)
(**)
(*globalPlotRange={{xMin-0.5,xMax+0.5},{yMin-0.5,yMax+0.5}};*)
(*globalGridLines={Range[xMin,xMax,1],Range[yMin,yMax,1]};*)
(**)
(*Graphics[{Thick,Line[#[[1]]],{RGBColor[1, 0, 0],Circle[{xMax/2+0.5,yMax/2+0.5},xMax/2-0.5]}},PlotRange->globalPlotRange,GridLines->globalGridLines,Frame->True,Axes->False,ImageSize->100,FrameLabel->{"X","Y"},PlotLabel->Row[{#[[2]]}]]&/@pathCounts[[All]]*)


(* ::Input:: *)
(*(*1. Calculate relative frequencies ("probabilities")*)*)
(*totalPaths=Length[paths];*)
(*countsOnly=pathCounts[[All,2]];*)
(*probabilities=countsOnly/totalPaths;*)
(**)
(*(*2. Convert each path (series of points) into a clean text label*)*)
(*(*Shortens coordinates to strings like "{(0,1),(1,2)}"*)*)
(*pathLabels=Map[Row[Map[RowBox[{"{",Row[#,","],"}"}]//DisplayForm&,#],", "]&,pathCounts[[All,1]]];*)
(**)
(*pathLabels=ParallelMap[Graphics[{Thick,Line[#],{RGBColor[1, 0, 0],Circle[{xMax/2+0.5,yMax/2+0.5},xMax/2-0.5]}},*)
(*PlotRange->globalPlotRange,PlotRangePadding->0.2,*)
(*GridLines->globalGridLines,*)
(*Frame->True,FrameTicks->None,Axes->False,*)
(*ImageSize->200]&*)
(*,pathCounts[[All,1]]];*)


(* ::Input:: *)
(*(*3. Generate the BarChart with values on top and unique symmetry families on the bottom*)*)
(*BarChart[probabilities,*)
(*ChartLabels->Placed[pathLabels,Axis],LabelingFunction->(Placed[Row[{NumberForm[100.*#1,{5,2}],"%"}],Above]&),Frame->True,FrameLabel->{"Paths","Relative Occurrence (Probability)"},PlotLabel->"Path Distribution",*)
(*ChartStyle->RGBColor[0.87, 0.71, 0.34],ImageSize->900,PlotRange->{All,{0,Max[probabilities]*1.15}}]*)


(* ::Section::Closed:: *)
(*L=5 (active is 3) path Lengths histograms (b-1LRW-n5-NoReordering.csv )*)


(* ::Subsection::Closed:: *)
(*All paths*)


(* ::Input:: *)
(*n=3;*)
(*(*1. Read as text and split by double carriage returns/newlines*)rawBlocks=Rest[StringSplit[Import["D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\FullPaths\\b-1LRW-n5-NoReordering.csv","Text"],"\n\n"|"\r\n\r\n"]];*)
(*(*2. Convert each separate text block cleanly into an array of coordinate pairs*)*)
(*paths=ParallelMap[Drop[ImportString[#,"CSV"],{},-2]&,rawBlocks];*)
(*paths[[-1]];*)
(*Length@paths*)


(* ::Input:: *)
(*(*Render all paths simultaneously with distinct indexed colors*)Graphics[{Thick,MapIndexed[{ColorData[97][#2[[1]]],Line[#1]}&,paths]},Frame->True,Axes->False,ImageSize->500,FrameLabel->{"X Coordinate","Y Coordinate"}];*)
(**)
(*center=(n+1)/2;*)
(**)
(*Graphics[{Style[Circle[{center,center},center-1],Red],Thick,MapIndexed[{ColorData[97][#2[[1]]],Line[#1]}&,paths[[All]]],Point[{center,center}]},Frame->True,Axes->False,ImageSize->500,FrameLabel->{"X Coordinate","Y Coordinate"},PlotRange->All,GridLines->{Range[0,center*2],Range[0,center*2]}];*)


(* ::Input:: *)
(*allPoints=Flatten[paths,1];*)
(*{xMin,xMax}={Min[allPoints[[All,1]]],Max[allPoints[[All,1]]]};*)
(*{yMin,yMax}={Min[allPoints[[All,2]]],Max[allPoints[[All,2]]]};*)
(**)
(*globalPlotRange={{xMin-0.5,xMax+0.5},{yMin-0.5,yMax+0.5}};*)
(*globalGridLines={Range[xMin,xMax,1],Range[yMin,yMax,1]};*)
(**)
(**)
(*Graphics[{Thick,Line[#],{RGBColor[1, 0, 0],Circle[{xMax/2+0.5,yMax/2+0.5},xMax/2-0.5]}},PlotRange->globalPlotRange,GridLines->globalGridLines,Frame->True,Axes->False,ImageSize->110,FrameLabel->{"X","Y"}]&@paths[[1]]*)
(**)


(* ::Subsection:: *)
(*No symmetry taken into account*)


(* ::Input:: *)
(*(*2. Count the occurrences of each unique path*)*)
(*pathCounts=Tally[paths];*)
(*pathCounts[[1]]*)
(**)
(*pathCounts=Reverse[SortBy[pathCounts,Last]];*)
(*pathCounts[[1]]*)
(*(*Output format:{{path1,count1},{path2,count2},...}*)*)


(* ::Input:: *)
(*allPoints=Flatten[pathCounts[[All,1]],1];*)
(*{xMin,xMax}={Min[allPoints[[All,1]]],Max[allPoints[[All,1]]]};*)
(*{yMin,yMax}={Min[allPoints[[All,2]]],Max[allPoints[[All,2]]]};*)
(**)
(*globalPlotRange={{xMin-0.5,xMax+0.5},{yMin-0.5,yMax+0.5}};*)
(*globalGridLines={Range[xMin,xMax,1],Range[yMin,yMax,1]};*)
(**)
(*Graphics[{Thick,Line[#[[1]]],{RGBColor[1, 0, 0],Circle[{xMax/2+0.5,yMax/2+0.5},xMax/2-0.5]}},PlotRange->globalPlotRange,GridLines->globalGridLines,Frame->True,Axes->False,ImageSize->100,FrameLabel->{"X","Y"},PlotLabel->Row[{#[[2]]}]]&/@pathCounts[[All]]*)


(* ::Input:: *)
(*(*1. Calculate relative frequencies ("probabilities")*)*)
(*totalPaths=Length[paths];*)
(*countsOnly=pathCounts[[All,2]];*)
(*probabilities=countsOnly/totalPaths;*)
(**)
(*(*2. Convert each path (series of points) into a clean text label*)*)
(*(*Shortens coordinates to strings like "{(0,1),(1,2)}"*)*)
(*pathLabels=Map[Row[Map[RowBox[{"{",Row[#,","],"}"}]//DisplayForm&,#],", "]&,pathCounts[[All,1]]];*)
(**)
(*pathLabels=ParallelMap[Graphics[{Thick,Line[#],{RGBColor[1, 0, 0],Circle[{xMax/2+0.5,yMax/2+0.5},xMax/2-0.5]}},*)
(*PlotRange->globalPlotRange,PlotRangePadding->0.2,*)
(*GridLines->globalGridLines,*)
(*Frame->True,FrameTicks->None,Axes->False,*)
(*ImageSize->200]&*)
(*,pathCounts[[All,1]]];*)


(* ::Input:: *)
(*(*3. Generate the BarChart with values on top and unique symmetry families on the bottom*)*)
(*BarChart[probabilities,*)
(*ChartLabels->Placed[pathLabels,Axis],LabelingFunction->(Placed[Row[{NumberForm[100.*#1,{5,2}],"%"}],Above]&),Frame->True,FrameLabel->{"Paths","Relative Occurrence (Probability)"},PlotLabel->"Path Distribution",*)
(*ChartStyle->RGBColor[0.87, 0.71, 0.34],ImageSize->900,PlotRange->{All,{0,Max[probabilities]*1.15}}]*)


(* ::Section::Closed:: *)
(*L=5 (active is 3) path Lengths histograms (b-1LRW-n5-NoReordering-Double.csv )*)


(* ::Subsection:: *)
(*All paths*)


(* ::Input:: *)
(*n=3;*)
(*(*1. Read as text and split by double carriage returns/newlines*)rawBlocks=Rest[StringSplit[Import["D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\FullPaths\\b-1LRW-n5-NoReordering-Double.csv","Text"],"\n\n"|"\r\n\r\n"]];*)
(*(*2. Convert each separate text block cleanly into an array of coordinate pairs*)*)
(*paths=ParallelMap[Drop[ImportString[#,"CSV"],{},-2]&,rawBlocks];*)
(*paths[[-1]];*)
(*Length@paths*)


(* ::Input:: *)
(*(*Render all paths simultaneously with distinct indexed colors*)Graphics[{Thick,MapIndexed[{ColorData[97][#2[[1]]],Line[#1]}&,paths]},Frame->True,Axes->False,ImageSize->500,FrameLabel->{"X Coordinate","Y Coordinate"}];*)
(**)
(*center=(n+1)/2;*)
(**)
(*Graphics[{Style[Circle[{center,center},center-1],Red],Thick,MapIndexed[{ColorData[97][#2[[1]]],Line[#1]}&,paths[[All]]],Point[{center,center}]},Frame->True,Axes->False,ImageSize->500,FrameLabel->{"X Coordinate","Y Coordinate"},PlotRange->All,GridLines->{Range[0,center*2],Range[0,center*2]}];*)


(* ::Input:: *)
(*allPoints=Flatten[paths,1];*)
(*{xMin,xMax}={Min[allPoints[[All,1]]],Max[allPoints[[All,1]]]};*)
(*{yMin,yMax}={Min[allPoints[[All,2]]],Max[allPoints[[All,2]]]};*)
(**)
(*globalPlotRange={{xMin-0.5,xMax+0.5},{yMin-0.5,yMax+0.5}};*)
(*globalGridLines={Range[xMin,xMax,1],Range[yMin,yMax,1]};*)
(**)
(**)
(*Graphics[{Thick,Line[#],{RGBColor[1, 0, 0],Circle[{xMax/2+0.5,yMax/2+0.5},xMax/2-0.5]}},PlotRange->globalPlotRange,GridLines->globalGridLines,Frame->True,Axes->False,ImageSize->110,FrameLabel->{"X","Y"}]&@paths[[1]]*)
(**)


(* ::Subsection:: *)
(*No symmetry taken into account*)


(* ::Input:: *)
(*(*2. Count the occurrences of each unique path*)*)
(*pathCounts=Tally[paths];*)
(*pathCounts[[1]]*)
(**)
(*pathCounts=Reverse[SortBy[pathCounts,Last]];*)
(*pathCounts[[1]]*)
(*(*Output format:{{path1,count1},{path2,count2},...}*)*)


(* ::Input:: *)
(*allPoints=Flatten[pathCounts[[All,1]],1];*)
(*{xMin,xMax}={Min[allPoints[[All,1]]],Max[allPoints[[All,1]]]};*)
(*{yMin,yMax}={Min[allPoints[[All,2]]],Max[allPoints[[All,2]]]};*)
(**)
(*globalPlotRange={{xMin-0.5,xMax+0.5},{yMin-0.5,yMax+0.5}};*)
(*globalGridLines={Range[xMin,xMax,1],Range[yMin,yMax,1]};*)
(**)
(*Graphics[{Thick,Line[#[[1]]],{RGBColor[1, 0, 0],Circle[{xMax/2+0.5,yMax/2+0.5},xMax/2-0.5]}},PlotRange->globalPlotRange,GridLines->globalGridLines,Frame->True,Axes->False,ImageSize->100,FrameLabel->{"X","Y"},PlotLabel->Row[{#[[2]]}]]&/@pathCounts[[All]]*)


(* ::Input:: *)
(*(*1. Calculate relative frequencies ("probabilities")*)*)
(*totalPaths=Length[paths];*)
(*countsOnly=pathCounts[[All,2]];*)
(*probabilities=countsOnly/totalPaths;*)
(**)
(*(*2. Convert each path (series of points) into a clean text label*)*)
(*(*Shortens coordinates to strings like "{(0,1),(1,2)}"*)*)
(*pathLabels=Map[Row[Map[RowBox[{"{",Row[#,","],"}"}]//DisplayForm&,#],", "]&,pathCounts[[All,1]]];*)
(**)
(*pathLabels=ParallelMap[Graphics[{Thick,Line[#],{RGBColor[1, 0, 0],Circle[{xMax/2+0.5,yMax/2+0.5},xMax/2-0.5]}},*)
(*PlotRange->globalPlotRange,PlotRangePadding->0.2,*)
(*GridLines->globalGridLines,*)
(*Frame->True,FrameTicks->None,Axes->False,*)
(*ImageSize->200]&*)
(*,pathCounts[[All,1]]];*)


(* ::Input:: *)
(*(*3. Generate the BarChart with values on top and unique symmetry families on the bottom*)*)
(*BarChart[probabilities,*)
(*ChartLabels->Placed[pathLabels,Axis],LabelingFunction->(Placed[Row[{NumberForm[100.*#1,{5,2}],"%"}],Above]&),Frame->True,FrameLabel->{"Paths","Relative Occurrence (Probability)"},PlotLabel->"Path Distribution",*)
(*ChartStyle->RGBColor[0.87, 0.71, 0.34],ImageSize->900,PlotRange->{All,{0,Max[probabilities]*1.15}}]*)


(* ::Input:: *)
(*Histogram[Table[RandomInteger[3],40000]]*)


(* ::Section::Closed:: *)
(*L=7 (active is 5) path Lengths histograms (b-1LRW-n7.csv )*)


(* ::Input:: *)
(*(*1. Read as text and split by double carriage returns/newlines*)rawBlocks=Rest[StringSplit[Import["D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\FullPaths\\b-1LRW-n7.csv","Text"],"\n\n"|"\r\n\r\n"]];*)


(* ::Input:: *)
(*rawBlocks[[{1}]]*)


(* ::Input:: *)
(*(*2. Convert each separate text block cleanly into an array of coordinate pairs*)*)
(*paths=Map[Drop[ImportString[#,"CSV"],{},-2]&,rawBlocks];*)
(*paths[[-1]]*)


(* ::Subsection::Closed:: *)
(*No symmetry taken into account*)


(* ::Input:: *)
(*(*2. Count the occurrences of each unique path*)*)
(*pathCounts=Tally[paths];*)
(*(*Output format:{{path1,count1},{path2,count2},...}*)*)


(* ::Input:: *)
(*(*Render all paths simultaneously with distinct indexed colors*)Graphics[{Thick,MapIndexed[{ColorData[97,"ColorFunction"][#2[[1]]],Line[#1]}&,pathCounts[[1;;-1,1]]]},Frame->True,Axes->False,ImageSize->500,FrameLabel->{"X Coordinate","Y Coordinate"}]*)


(* ::Input:: *)
(*pathCounts[[All,1]]*)


(* ::Input:: *)
(*allPoints=Flatten[pathCounts[[All,1]],1];*)
(*{xMin,xMax}={Min[allPoints[[All,1]]],Max[allPoints[[All,1]]]};*)
(*{yMin,yMax}={Min[allPoints[[All,2]]],Max[allPoints[[All,2]]]};*)
(**)
(*globalPlotRange={{xMin-0.5,xMax+0.5},{yMin-0.5,yMax+0.5}};*)
(*globalGridLines={Range[xMin,xMax,1],Range[yMin,yMax,1]};*)
(**)
(*Graphics[{Thick,Line[#[[1]]],{RGBColor[1, 0, 0],Circle[{xMax/2+0.5,yMax/2+0.5},xMax/2-0.5]}},PlotRange->globalPlotRange,GridLines->globalGridLines,Frame->True,Axes->False,ImageSize->100,FrameLabel->{"X","Y"},PlotLabel->Row[{#[[2]]}]]&/@pathCounts[[All]]*)


(* ::Input:: *)
(*(*Apply the function across all paths to get a list of numbers*)*)
(*lengthsList=Map[Length,paths];*)
(**)
(*(*Plot the distribution of the resulting lengths*)*)
(*Histogram[lengthsList,Automatic,"Probability",Frame->True,ChartStyle->RGBColor[0.87, 0.71, 0.34],FrameLabel->{"Path Length","Relative Probability"},ImageSize->450]*)


(* ::Input:: *)
(*(*3. Extract just the counts for your histogram*)*)
(*countsOnly=pathCounts[[All,2]];*)
(**)
(*(*4. Plot the histogram of path frequencies*)*)
(*Histogram[countsOnly,{1},"Probability",Frame->True,FrameLabel->{"Number of Occurrences","Number of Unique Paths"},PlotLabel->"Path Duplication Distribution",ChartStyle->RGBColor[0.87, 0.71, 0.34],ImageSize->400]*)


(* ::Input:: *)
(*(*1. Calculate relative frequencies ("probabilities")*)*)
(*totalPaths=Length[paths];*)
(*probabilities=countsOnly/totalPaths;*)
(**)
(*(*2. Convert each path (series of points) into a clean text label*)*)
(*(*Shortens coordinates to strings like "{(0,1),(1,2)}"*)*)
(*pathLabels=Map[Row[Map[RowBox[{"{",Row[#,","],"}"}]//DisplayForm&,#],", "]&,pathCounts[[All,1]]];*)
(**)
(*(*3. Generate the BarChart with values on top and paths on the bottom*)*)
(*BarChart[probabilities,ChartLabels->Placed[pathLabels,Axis,Rotate[#,45Degree]&],(*Rotates labels so they don't overlap*)ChartLabels->Placed[Around[#,0]&/@probabilities,Above],(*Forces values on top of bars*)LabelingFunction->(Placed[Row[{NumberForm[100.*#1,{5,2}],"%"}],Above]&),Frame->True,FrameLabel->{"Unique Paths","Relative Occurrence (Probability)"},PlotLabel->"Path Distribution",ChartStyle->RGBColor[0.87,0.71,0.34],ImageSize->900,(*Made wider to accommodate labels nicely*)PlotRange->{All,{0,Max[probabilities]*1.15}} (*Leaves room at the top for the numbers*)]*)


(* ::Input:: *)
(**)


(* ::Subsection::Closed:: *)
(*Gathering symmetric paths*)


(* ::Subsubsection::Closed:: *)
(*Function definitions*)


(* ::Input:: *)
(*paths[[1;;4]]*)
(*Graphics[{Thick,Line[#],{RGBColor[1, 0, 0],Circle[{xMax/2+0.5,yMax/2+0.5},xMax/2-0.5]}},PlotRange->globalPlotRange,GridLines->globalGridLines,Frame->True,Axes->False,ImageSize->100,FrameLabel->{"X","Y"}]&/@%*)


(* ::Input:: *)
(*ClearAll[Rotate90,ReflectX,GetSymmetryFamily];*)
(**)
(*(*Rotates 90\[Degree] counter-clockwise around the first point of the path*)Rotate90[path_]:=With[{center=path[[1]]},Map[({-(#[[2]]-center[[2]])+center[[1]],(#[[1]]-center[[1]])+center[[2]]})&,path]]*)
(**)
(*(*Reflects horizontally across the vertical line passing through the first point (x->-x)*)*)
(*ReflectX[path_]:=With[{center=path[[1]]},Map[({-(#[[1]]-center[[1]])+center[[1]],#[[2]]})&,path]]*)
(**)
(*(*Generates all 8 unique rotation and reflection variants of a path*)GetSymmetryFamily[path_]:=With[{rotations=NestList[Rotate90,path,3]},*)
(*Join[rotations,Map[ReflectX,rotations]]]*)


(* ::Input:: *)
(*p=paths[[{1,2}]]*)
(*Graphics[{Thick,Line[#],{RGBColor[1, 0, 0],Circle[{xMax/2+0.5,yMax/2+0.5},xMax/2-0.5]}},PlotRange->globalPlotRange,GridLines->globalGridLines,Frame->True,Axes->False,ImageSize->100,FrameLabel->{"X","Y"}]&/@%*)
(**)
(*Rotate90/@p;*)
(*Graphics[{Thick,Line[#],{RGBColor[1, 0, 0],Circle[{xMax/2+0.5,yMax/2+0.5},xMax/2-0.5]}},PlotRange->globalPlotRange,GridLines->globalGridLines,Frame->True,Axes->False,ImageSize->100,FrameLabel->{"X","Y"}]&/@%;*)
(**)
(*ReflectX/@p;*)
(*Graphics[{Thick,Line[#],{RGBColor[1, 0, 0],Circle[{xMax/2+0.5,yMax/2+0.5},xMax/2-0.5]}},PlotRange->globalPlotRange,GridLines->globalGridLines,Frame->True,Axes->False,ImageSize->100,FrameLabel->{"X","Y"}]&/@%;*)
(**)
(*GetSymmetryFamily/@p*)
(*Graphics[{Thick,Line[#],{RGBColor[1, 0, 0],Circle[{xMax/2+0.5,yMax/2+0.5},xMax/2-0.5]}},PlotRange->globalPlotRange,GridLines->globalGridLines,Frame->True,Axes->False,ImageSize->100,FrameLabel->{"X","Y"}]&/@#&/@%*)


(* ::Input:: *)
(*ClearAll[AnalyzeSymmetricPaths]*)
(**)
(*(*Comparison test:Returns True if path2 is a valid symmetry transformation of path1*)AreSymmetricQ[path1_,path2_]:=MemberQ[GetSymmetryFamily[path1],path2];*)
(**)
(*(*The master function to process your paths array*)*)
(*AnalyzeSymmetricPaths[allPaths_List]:=Module[{grouped,tally},(*Group identical paths under rotation/reflection*)grouped=Gather[allPaths,AreSymmetricQ];*)
(*(*Return a structured dataset:{Unique Representative Path,Family Occurrence Count}*)tally=Table[{First[g],Length[g]},{g,grouped}];*)
(**)
(*Reverse[SortBy[tally,Last]]*)
(*];*)


(* ::Input:: *)
(*p=paths[[1;;4]]*)
(*Graphics[{Thick,Line[#],{RGBColor[1, 0, 0],Circle[{xMax/2+0.5,yMax/2+0.5},xMax/2-0.5]}},PlotRange->globalPlotRange,GridLines->globalGridLines,Frame->True,Axes->False,ImageSize->100,FrameLabel->{"X","Y"}]&/@%*)
(**)
(*AnalyzeSymmetricPaths[p]*)
(*Graphics[{Thick,Line[#[[1]]],{RGBColor[1, 0, 0],Circle[{xMax/2+0.5,yMax/2+0.5},xMax/2-0.5]}},PlotRange->globalPlotRange,GridLines->globalGridLines,Frame->True,Axes->False,ImageSize->100,FrameLabel->{"X","Y"},PlotLabel->Row[{#[[2]]}]]&/@%*)


(* ::Subsubsection:: *)
(*Actual application*)


(* ::Input:: *)
(*(*1. Read as text and split by double carriage returns/newlines*)rawBlocks=Rest[StringSplit[Import["D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\FullPaths\\b-1LRW-n7.csv","Text"],"\n\n"|"\r\n\r\n"]];*)
(*(*2. Convert each separate text block cleanly into an array of coordinate pairs*)*)
(*paths=ParallelMap[Drop[ImportString[#,"CSV"],{},-2]&,rawBlocks];*)
(*paths[[-1]]*)


(* ::Input:: *)
(*Length@paths*)
(*Head@paths*)


(* ::Input:: *)
(*symmetryResults=AnalyzeSymmetricPaths[paths];*)
(*Graphics[{Thick,Line[#[[1]]],{RGBColor[1, 0, 0],Circle[{xMax/2+0.5,yMax/2+0.5},xMax/2-0.5]}},PlotRange->globalPlotRange,GridLines->globalGridLines,Frame->True,Axes->False,ImageSize->100,FrameLabel->{"X","Y"},PlotLabel->Row[{#[[2]]}]]&/@%*)
(**)
(*(*2. Extract components for your charts*)*)
(*uniqueSymmetricPaths=symmetryResults[[All,1]];*)
(*symmetricCounts=symmetryResults[[All,2]];*)


(* ::Input:: *)
(*(*1. Calculate relative frequencies ("probabilities")*)*)
(*totalPaths=Length[paths];*)
(*symmetricProbabilities=symmetricCounts/totalPaths;*)
(**)
(**)
(*(*2. Convert each unique representative path into your clean coordinate string format*)symmetricPathLabels=Map[Row[Map[RowBox[{"{",Row[#,","],"}"}]//DisplayForm&,#],", "]&,uniqueSymmetricPaths];*)
(**)
(*symmetricPathLabels=Map[Graphics[{Thick,Line[#],{RGBColor[1, 0, 0],Circle[{xMax/2+0.5,yMax/2+0.5},xMax/2-0.5]}},*)
(*PlotRange->globalPlotRange,PlotRangePadding->0.2,*)
(*GridLines->globalGridLines,*)
(*Frame->True,FrameTicks->None,Axes->False,*)
(*ImageSize->200]&*)
(*,uniqueSymmetricPaths];*)
(**)
(*(*3. Generate the BarChart with values on top and unique symmetry families on the bottom*)BarChart[symmetricProbabilities,ChartLabels->Placed[symmetricPathLabels,Axis],LabelingFunction->(Placed[Row[{NumberForm[100.*#1,{5,2}],"%"}],Above]&),Frame->True,FrameLabel->{"Unique Paths (Symmetry Families Included)","Relative Occurrence (Probability)"},PlotLabel->"Symmetric Path Distribution",ChartStyle->RGBColor[0.87,0.71,0.34],ImageSize->900,PlotRange->{All,{0,Max[symmetricProbabilities]*1.15}}]*)


(* ::Section::Closed:: *)
(*L=7 (active is 5) path Lengths histograms (b-1LRW-n7-UpdatedStopCond.csv)*)


(* ::Subsection::Closed:: *)
(*All paths*)


(* ::Input:: *)
(*Quit*)


(* ::Input:: *)
(*n=5;*)
(*(*1. Read as text and split by double carriage returns/newlines*)rawBlocks=Rest[StringSplit[Import["D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\FullPaths\\b-1LRW-n7-UpdatedStopCond.csv","Text"],"\n\n"|"\r\n\r\n"]];*)
(*(*2. Convert each separate text block cleanly into an array of coordinate pairs*)*)
(*paths=ParallelMap[Drop[ImportString[#,"CSV"],{},-2]&,rawBlocks];*)
(*paths[[-1]];*)
(*Length@paths*)


(* ::Input:: *)
(*(*Render all paths simultaneously with distinct indexed colors*)Graphics[{Thick,MapIndexed[{ColorData[97][#2[[1]]],Line[#1]}&,paths]},Frame->True,Axes->False,ImageSize->500,FrameLabel->{"X Coordinate","Y Coordinate"}];*)
(**)
(*center=(n+1)/2;*)
(**)
(*Graphics[{Style[Circle[{center,center},center-1],Red],Thick,MapIndexed[{ColorData[97][#2[[1]]],Line[#1]}&,paths[[All]]],Point[{center,center}]},Frame->True,Axes->False,ImageSize->500,FrameLabel->{"X Coordinate","Y Coordinate"},PlotRange->All,GridLines->{Range[0,center*2],Range[0,center*2]}]*)


(* ::Input:: *)
(*allPoints=Flatten[paths,1];*)
(*{xMin,xMax}={Min[allPoints[[All,1]]],Max[allPoints[[All,1]]]};*)
(*{yMin,yMax}={Min[allPoints[[All,2]]],Max[allPoints[[All,2]]]};*)
(**)
(*globalPlotRange={{xMin-0.5,xMax+0.5},{yMin-0.5,yMax+0.5}};*)
(*globalGridLines={Range[xMin,xMax,1],Range[yMin,yMax,1]};*)
(**)
(**)
(*Graphics[{Thick,Line[#],{RGBColor[1, 0, 0],Circle[{xMax/2+0.5,yMax/2+0.5},xMax/2-0.5]}},PlotRange->globalPlotRange,GridLines->globalGridLines,Frame->True,Axes->False,ImageSize->110,FrameLabel->{"X","Y"}]&@paths[[1]]*)
(**)


(* ::Subsection::Closed:: *)
(*No symmetry taken into account*)


(* ::Input:: *)
(*(*2. Count the occurrences of each unique path*)*)
(*pathCounts=Tally[paths];*)
(*pathCounts[[1]]*)
(**)
(*pathCounts=Reverse[SortBy[pathCounts,Last]];*)
(*pathCounts[[1]]*)
(*(*Output format:{{path1,count1},{path2,count2},...}*)*)


(* ::Input:: *)
(*allPoints=Flatten[pathCounts[[All,1]],1];*)
(*{xMin,xMax}={Min[allPoints[[All,1]]],Max[allPoints[[All,1]]]};*)
(*{yMin,yMax}={Min[allPoints[[All,2]]],Max[allPoints[[All,2]]]};*)
(**)
(*globalPlotRange={{xMin-0.5,xMax+0.5},{yMin-0.5,yMax+0.5}};*)
(*globalGridLines={Range[xMin,xMax,1],Range[yMin,yMax,1]};*)
(**)
(*Graphics[{Thick,Line[#[[1]]],{RGBColor[1, 0, 0],Circle[{xMax/2+0.5,yMax/2+0.5},xMax/2-0.5]}},PlotRange->globalPlotRange,GridLines->globalGridLines,Frame->True,Axes->False,ImageSize->100,FrameLabel->{"X","Y"},PlotLabel->Row[{#[[2]]}]]&/@pathCounts[[All]]*)


(* ::Input:: *)
(*(*1. Calculate relative frequencies ("probabilities")*)*)
(*totalPaths=Length[paths];*)
(*countsOnly=pathCounts[[All,2]];*)
(*probabilities=countsOnly/totalPaths;*)
(**)
(*(*2. Convert each path (series of points) into a clean text label*)*)
(*(*Shortens coordinates to strings like "{(0,1),(1,2)}"*)*)
(*pathLabels=Map[Row[Map[RowBox[{"{",Row[#,","],"}"}]//DisplayForm&,#],", "]&,pathCounts[[All,1]]];*)
(**)
(*pathLabels=ParallelMap[Graphics[{Thick,Line[#],{RGBColor[1, 0, 0],Circle[{xMax/2+0.5,yMax/2+0.5},xMax/2-0.5]}},*)
(*PlotRange->globalPlotRange,PlotRangePadding->0.2,*)
(*GridLines->globalGridLines,*)
(*Frame->True,FrameTicks->None,Axes->False,*)
(*ImageSize->200]&*)
(*,pathCounts[[All,1]]];*)


(* ::Input:: *)
(*(*3. Generate the BarChart with values on top and unique symmetry families on the bottom*)*)
(*BarChart[probabilities[[1;;15]],*)
(*ChartLabels->Placed[pathLabels,Axis],LabelingFunction->(Placed[Row[{NumberForm[100.*#1,{5,2}],"%"}],Above]&),Frame->True,FrameLabel->{"Paths","Relative Occurrence (Probability)"},PlotLabel->"Path Distribution",*)
(*ChartStyle->RGBColor[0.87, 0.71, 0.34],ImageSize->900,PlotRange->{All,{0,Max[probabilities]*1.15}}]*)


(* ::Section::Closed:: *)
(*L=7 (active is 5) path Lengths histograms (b-1LRW-n7-NoReordering-Double.csv )*)


(* ::Subsection::Closed:: *)
(*All paths*)


(* ::Input:: *)
(*n=3;*)
(*(*1. Read as text and split by double carriage returns/newlines*)rawBlocks=Rest[StringSplit[Import["D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\FullPaths\\b-1LRW-n7-NoReordering-Double.csv","Text"],"\n\n"|"\r\n\r\n"]];*)
(*(*2. Convert each separate text block cleanly into an array of coordinate pairs*)*)
(*paths=ParallelMap[Drop[ImportString[#,"CSV"],{},-2]&,rawBlocks];*)
(*paths[[-1]];*)
(*Length@paths*)


(* ::Input:: *)
(*(*Render all paths simultaneously with distinct indexed colors*)Graphics[{Thick,MapIndexed[{ColorData[97][#2[[1]]],Line[#1]}&,paths]},Frame->True,Axes->False,ImageSize->500,FrameLabel->{"X Coordinate","Y Coordinate"}];*)
(**)
(*center=(n+1)/2;*)
(**)
(*Graphics[{Style[Circle[{center,center},center-1],Red],Thick,MapIndexed[{ColorData[97][#2[[1]]],Line[#1]}&,paths[[All]]],Point[{center,center}]},Frame->True,Axes->False,ImageSize->500,FrameLabel->{"X Coordinate","Y Coordinate"},PlotRange->All,GridLines->{Range[0,center*2],Range[0,center*2]}];*)


(* ::Input:: *)
(*allPoints=Flatten[paths,1];*)
(*{xMin,xMax}={Min[allPoints[[All,1]]],Max[allPoints[[All,1]]]};*)
(*{yMin,yMax}={Min[allPoints[[All,2]]],Max[allPoints[[All,2]]]};*)
(**)
(*globalPlotRange={{xMin-0.5,xMax+0.5},{yMin-0.5,yMax+0.5}};*)
(*globalGridLines={Range[xMin,xMax,1],Range[yMin,yMax,1]};*)
(**)
(**)
(*Graphics[{Thick,Line[#],{RGBColor[1, 0, 0],Circle[{xMax/2+0.5,yMax/2+0.5},xMax/2-0.5]}},PlotRange->globalPlotRange,GridLines->globalGridLines,Frame->True,Axes->False,ImageSize->110,FrameLabel->{"X","Y"}]&@paths[[1]]*)
(**)


(* ::Subsection::Closed:: *)
(*No symmetry taken into account*)


(* ::Input:: *)
(*(*2. Count the occurrences of each unique path*)*)
(*pathCounts=Tally[paths];*)
(*pathCounts[[1]]*)
(**)
(*pathCounts=Reverse[SortBy[pathCounts,Last]];*)
(*pathCounts[[1]]*)
(*(*Output format:{{path1,count1},{path2,count2},...}*)*)


(* ::Input:: *)
(*allPoints=Flatten[pathCounts[[All,1]],1];*)
(*{xMin,xMax}={Min[allPoints[[All,1]]],Max[allPoints[[All,1]]]};*)
(*{yMin,yMax}={Min[allPoints[[All,2]]],Max[allPoints[[All,2]]]};*)
(**)
(*globalPlotRange={{xMin-0.5,xMax+0.5},{yMin-0.5,yMax+0.5}};*)
(*globalGridLines={Range[xMin,xMax,1],Range[yMin,yMax,1]};*)
(**)
(*Graphics[{Thick,Line[#[[1]]],{RGBColor[1, 0, 0],Circle[{xMax/2+0.5,yMax/2+0.5},xMax/2-0.5]}},PlotRange->globalPlotRange,GridLines->globalGridLines,Frame->True,Axes->False,ImageSize->100,FrameLabel->{"X","Y"},PlotLabel->Row[{#[[2]]}]]&/@pathCounts[[All]]*)


(* ::Input:: *)
(*(*1. Calculate relative frequencies ("probabilities")*)*)
(*totalPaths=Length[paths];*)
(*countsOnly=pathCounts[[All,2]];*)
(*probabilities=countsOnly/totalPaths;*)
(**)
(*(*2. Convert each path (series of points) into a clean text label*)*)
(*(*Shortens coordinates to strings like "{(0,1),(1,2)}"*)*)
(*pathLabels=Map[Row[Map[RowBox[{"{",Row[#,","],"}"}]//DisplayForm&,#],", "]&,pathCounts[[All,1]]];*)
(**)
(*pathLabels=ParallelMap[Graphics[{Thick,Line[#],{RGBColor[1, 0, 0],Circle[{xMax/2+0.5,yMax/2+0.5},xMax/2-0.5]}},*)
(*PlotRange->globalPlotRange,PlotRangePadding->0.2,*)
(*GridLines->globalGridLines,*)
(*Frame->True,FrameTicks->None,Axes->False,*)
(*ImageSize->200]&*)
(*,pathCounts[[All,1]]];*)


(* ::Input:: *)
(*(*3. Generate the BarChart with values on top and unique symmetry families on the bottom*)*)
(*BarChart[probabilities[[1;;15]],*)
(*ChartLabels->Placed[pathLabels,Axis],LabelingFunction->(Placed[Row[{NumberForm[100.*#1,{5,2}],"%"}],Above]&),Frame->True,FrameLabel->{"Paths","Relative Occurrence (Probability)"},PlotLabel->"Path Distribution",*)
(*ChartStyle->RGBColor[0.87, 0.71, 0.34],ImageSize->900,PlotRange->{All,{0,Max[probabilities]*1.15}}]*)


(* ::Subsection:: *)
(*Gathering symmetric paths*)


(* ::Subsubsection:: *)
(*Actual application*)


(* ::Input:: *)
(*Length@paths*)
(*Head@paths*)


(* ::Input:: *)
(*symmetryResults=AnalyzeSymmetricPaths[paths];*)
(*Graphics[{Thick,Line[#[[1]]],{RGBColor[1, 0, 0],Circle[{xMax/2+0.5,yMax/2+0.5},xMax/2-0.5]}},PlotRange->globalPlotRange,GridLines->globalGridLines,Frame->True,Axes->False,ImageSize->100,FrameLabel->{"X","Y"},PlotLabel->Row[{#[[2]]}]]&/@%*)
(**)
(*(*2. Extract components for your charts*)*)
(*uniqueSymmetricPaths=symmetryResults[[All,1]];*)
(*symmetricCounts=symmetryResults[[All,2]];*)


(* ::Input:: *)
(*(*1. Calculate relative frequencies ("probabilities")*)*)
(*totalPaths=Length[paths];*)
(*symmetricProbabilities=symmetricCounts/totalPaths;*)
(**)
(**)
(*(*2. Convert each unique representative path into your clean coordinate string format*)symmetricPathLabels=Map[Row[Map[RowBox[{"{",Row[#,","],"}"}]//DisplayForm&,#],", "]&,uniqueSymmetricPaths];*)
(**)
(*symmetricPathLabels=Map[Graphics[{Thick,Line[#],{RGBColor[1, 0, 0],Circle[{xMax/2+0.5,yMax/2+0.5},xMax/2-0.5]}},*)
(*PlotRange->globalPlotRange,PlotRangePadding->0.2,*)
(*GridLines->globalGridLines,*)
(*Frame->True,FrameTicks->None,Axes->False,*)
(*ImageSize->200]&*)
(*,uniqueSymmetricPaths];*)
(**)
(*(*3. Generate the BarChart with values on top and unique symmetry families on the bottom*)BarChart[symmetricProbabilities,ChartLabels->Placed[symmetricPathLabels,Axis],LabelingFunction->(Placed[Row[{NumberForm[100.*#1,{5,2}],"%"}],Above]&),Frame->True,FrameLabel->{"Unique Paths (Symmetry Families Included)","Relative Occurrence (Probability)"},PlotLabel->"Symmetric Path Distribution",ChartStyle->RGBColor[0.87,0.71,0.34],ImageSize->900,PlotRange->{All,{0,Max[symmetricProbabilities]*1.15}}]*)


(* ::Section::Closed:: *)
(*L=7 (active is 5) path Lengths histograms (b-1LRW-n7-YesReordering-Double.csv )*)


(* ::Subsection::Closed:: *)
(*All paths*)


(* ::Input:: *)
(*n=3;*)
(*(*1. Read as text and split by double carriage returns/newlines*)rawBlocks=Rest[StringSplit[Import["D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\FullPaths\\b-1LRW-n7-YesReordering-Double.csv","Text"],"\n\n"|"\r\n\r\n"]];*)
(*(*2. Convert each separate text block cleanly into an array of coordinate pairs*)*)
(*paths=ParallelMap[Drop[ImportString[#,"CSV"],{},-2]&,rawBlocks];*)
(*paths[[-1]];*)
(*Length@paths*)


(* ::Input:: *)
(*(*Render all paths simultaneously with distinct indexed colors*)Graphics[{Thick,MapIndexed[{ColorData[97][#2[[1]]],Line[#1]}&,paths]},Frame->True,Axes->False,ImageSize->500,FrameLabel->{"X Coordinate","Y Coordinate"}];*)
(**)
(*center=(n+1)/2;*)
(**)
(*Graphics[{Style[Circle[{center,center},center-1],Red],Thick,MapIndexed[{ColorData[97][#2[[1]]],Line[#1]}&,paths[[All]]],Point[{center,center}]},Frame->True,Axes->False,ImageSize->500,FrameLabel->{"X Coordinate","Y Coordinate"},PlotRange->All,GridLines->{Range[0,center*2],Range[0,center*2]}];*)


(* ::Input:: *)
(*allPoints=Flatten[paths,1];*)
(*{xMin,xMax}={Min[allPoints[[All,1]]],Max[allPoints[[All,1]]]};*)
(*{yMin,yMax}={Min[allPoints[[All,2]]],Max[allPoints[[All,2]]]};*)
(**)
(*globalPlotRange={{xMin-0.5,xMax+0.5},{yMin-0.5,yMax+0.5}};*)
(*globalGridLines={Range[xMin,xMax,1],Range[yMin,yMax,1]};*)
(**)
(**)
(*Graphics[{Thick,Line[#],{RGBColor[1, 0, 0],Circle[{xMax/2+0.5,yMax/2+0.5},xMax/2-0.5]}},PlotRange->globalPlotRange,GridLines->globalGridLines,Frame->True,Axes->False,ImageSize->110,FrameLabel->{"X","Y"}]&@paths[[1]]*)
(**)


(* ::Subsection::Closed:: *)
(*No symmetry taken into account*)


(* ::Input:: *)
(*(*2. Count the occurrences of each unique path*)*)
(*pathCounts=Tally[paths];*)
(*pathCounts[[1]]*)
(**)
(*pathCounts=Reverse[SortBy[pathCounts,Last]];*)
(*pathCounts[[1]]*)
(*(*Output format:{{path1,count1},{path2,count2},...}*)*)


(* ::Input:: *)
(*allPoints=Flatten[pathCounts[[All,1]],1];*)
(*{xMin,xMax}={Min[allPoints[[All,1]]],Max[allPoints[[All,1]]]};*)
(*{yMin,yMax}={Min[allPoints[[All,2]]],Max[allPoints[[All,2]]]};*)
(**)
(*globalPlotRange={{xMin-0.5,xMax+0.5},{yMin-0.5,yMax+0.5}};*)
(*globalGridLines={Range[xMin,xMax,1],Range[yMin,yMax,1]};*)
(**)
(*Graphics[{Thick,Line[#[[1]]],{RGBColor[1, 0, 0],Circle[{xMax/2+0.5,yMax/2+0.5},xMax/2-0.5]}},PlotRange->globalPlotRange,GridLines->globalGridLines,Frame->True,Axes->False,ImageSize->100,FrameLabel->{"X","Y"},PlotLabel->Row[{#[[2]]}]]&/@pathCounts[[All]]*)


(* ::Input:: *)
(*(*1. Calculate relative frequencies ("probabilities")*)*)
(*totalPaths=Length[paths];*)
(*countsOnly=pathCounts[[All,2]];*)
(*probabilities=countsOnly/totalPaths;*)
(**)
(*(*2. Convert each path (series of points) into a clean text label*)*)
(*(*Shortens coordinates to strings like "{(0,1),(1,2)}"*)*)
(*pathLabels=Map[Row[Map[RowBox[{"{",Row[#,","],"}"}]//DisplayForm&,#],", "]&,pathCounts[[All,1]]];*)
(**)
(*pathLabels=ParallelMap[Graphics[{Thick,Line[#],{RGBColor[1, 0, 0],Circle[{xMax/2+0.5,yMax/2+0.5},xMax/2-0.5]}},*)
(*PlotRange->globalPlotRange,PlotRangePadding->0.2,*)
(*GridLines->globalGridLines,*)
(*Frame->True,FrameTicks->None,Axes->False,*)
(*ImageSize->200]&*)
(*,pathCounts[[All,1]]];*)


(* ::Input:: *)
(*(*3. Generate the BarChart with values on top and unique symmetry families on the bottom*)*)
(*BarChart[probabilities[[1;;15]],*)
(*ChartLabels->Placed[pathLabels,Axis],LabelingFunction->(Placed[Row[{NumberForm[100.*#1,{5,2}],"%"}],Above]&),Frame->True,FrameLabel->{"Paths","Relative Occurrence (Probability)"},PlotLabel->"Path Distribution",*)
(*ChartStyle->RGBColor[0.87, 0.71, 0.34],ImageSize->900,PlotRange->{All,{0,Max[probabilities]*1.15}}]*)


(* ::Subsection:: *)
(*Gathering symmetric paths*)


(* ::Subsubsection:: *)
(*Actual application*)


(* ::Input:: *)
(*Length@paths*)
(*Head@paths*)


(* ::Input:: *)
(*symmetryResults=AnalyzeSymmetricPaths[paths];*)
(*Graphics[{Thick,Line[#[[1]]],{RGBColor[1, 0, 0],Circle[{xMax/2+0.5,yMax/2+0.5},xMax/2-0.5]}},PlotRange->globalPlotRange,GridLines->globalGridLines,Frame->True,Axes->False,ImageSize->100,FrameLabel->{"X","Y"},PlotLabel->Row[{#[[2]]}]]&/@%*)
(**)
(*(*2. Extract components for your charts*)*)
(*uniqueSymmetricPaths=symmetryResults[[All,1]];*)
(*symmetricCounts=symmetryResults[[All,2]];*)


(* ::Input:: *)
(*(*1. Calculate relative frequencies ("probabilities")*)*)
(*totalPaths=Length[paths];*)
(*symmetricProbabilities=symmetricCounts/totalPaths;*)
(**)
(**)
(*(*2. Convert each unique representative path into your clean coordinate string format*)symmetricPathLabels=Map[Row[Map[RowBox[{"{",Row[#,","],"}"}]//DisplayForm&,#],", "]&,uniqueSymmetricPaths];*)
(**)
(*symmetricPathLabels=Map[Graphics[{Thick,Line[#],{RGBColor[1, 0, 0],Circle[{xMax/2+0.5,yMax/2+0.5},xMax/2-0.5]}},*)
(*PlotRange->globalPlotRange,PlotRangePadding->0.2,*)
(*GridLines->globalGridLines,*)
(*Frame->True,FrameTicks->None,Axes->False,*)
(*ImageSize->200]&*)
(*,uniqueSymmetricPaths];*)
(**)
(*(*3. Generate the BarChart with values on top and unique symmetry families on the bottom*)BarChart[symmetricProbabilities,ChartLabels->Placed[symmetricPathLabels,Axis],LabelingFunction->(Placed[Row[{NumberForm[100.*#1,{5,2}],"%"}],Above]&),Frame->True,FrameLabel->{"Unique Paths (Symmetry Families Included)","Relative Occurrence (Probability)"},PlotLabel->"Symmetric Path Distribution",ChartStyle->RGBColor[0.87,0.71,0.34],ImageSize->900,PlotRange->{All,{0,Max[symmetricProbabilities]*1.15}}]*)


(* ::Section::Closed:: *)
(*L=9 (active is 7) path Lengths histograms (b-1LRW-n9.csv )*)


(* ::Subsection:: *)
(*Gathering symmetric paths*)


(* ::Subsubsection:: *)
(*Actual application*)


(* ::Input:: *)
(*(*1. Read as text and split by double carriage returns/newlines*)rawBlocks=Rest[StringSplit[Import["D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\FullPaths\\b-1LRW-n9.csv","Text"],"\n\n"|"\r\n\r\n"]];*)
(*(*2. Convert each separate text block cleanly into an array of coordinate pairs*)*)
(*paths=ParallelMap[Drop[ImportString[#,"CSV"],{},-2]&,rawBlocks];*)
(*paths[[-1]]*)
(*Length@paths*)


(* ::Input:: *)
(*Length@paths*)
(*Head@paths*)


(* ::Input:: *)
(*(*Render all paths simultaneously with distinct indexed colors*)Graphics[{Thick,MapIndexed[{ColorData[97][#2[[1]]],Line[#1]}&,paths]},Frame->True,Axes->False,ImageSize->500,FrameLabel->{"X Coordinate","Y Coordinate"}]*)


(* ::Input:: *)
(*allPoints=Flatten[paths,1];*)
(*{xMin,xMax}={Min[allPoints[[All,1]]],Max[allPoints[[All,1]]]};*)
(*{yMin,yMax}={Min[allPoints[[All,2]]],Max[allPoints[[All,2]]]};*)
(**)
(*globalPlotRange={{xMin-0.5,xMax+0.5},{yMin-0.5,yMax+0.5}};*)
(*globalGridLines={Range[xMin,xMax,1],Range[yMin,yMax,1]};*)
(**)
(**)
(*Graphics[{Thick,Line[#],{RGBColor[1, 0, 0],Circle[{xMax/2+0.5,yMax/2+0.5},xMax/2-0.5]}},PlotRange->globalPlotRange,GridLines->globalGridLines,Frame->True,Axes->False,ImageSize->110,FrameLabel->{"X","Y"}]&@paths[[1]]*)
(**)


(* ::Input:: *)
(*paths[[1]]*)


(* ::Subsubsection:: *)
(*Merge same paths under symmetries*)


(* ::Input:: *)
(*tally1={{{{4,4},{4,5},{5,5},{5,6},{5,7}},1}};*)
(*tally2={{{{4,4},{4,3},{5,3},{5,2},{5,1}},1}};*)
(**)
(*Graphics[{Thick,Line[#[[1]]],{RGBColor[1, 0, 0],Circle[{xMax/2+0.5,yMax/2+0.5},xMax/2-0.5]}},PlotRange->globalPlotRange,GridLines->globalGridLines,Frame->True,Axes->False,ImageSize->110,FrameLabel->{"X","Y"},PlotLabel->Row[{#[[2]]}]]&/@{tally1[[1]],tally2[[1]]}*)


(* ::Input:: *)
(*MergeTwoTallies[{{{{4,4},{4,5},{5,5},{5,6},{5,7}},1}},*)
(*{{{{4,4},{4,3},{5,3},{5,2},{5,1}},1}}]*)
(*Graphics[{Thick,Line[#[[1]]],{RGBColor[1, 0, 0],Circle[{xMax/2+0.5,yMax/2+0.5},xMax/2-0.5]}},PlotRange->globalPlotRange,GridLines->globalGridLines,Frame->True,Axes->False,ImageSize->110,FrameLabel->{"X","Y"},PlotLabel->Row[{#[[2]]}]]&/@%*)


(* ::Input:: *)
(*p=paths[[1;;7]];*)
(*Graphics[{Thick,Line[#],{RGBColor[1, 0, 0],Circle[{xMax/2+0.5,yMax/2+0.5},xMax/2-0.5]}},PlotRange->globalPlotRange,GridLines->globalGridLines,Frame->True,Axes->False,ImageSize->110,FrameLabel->{"X","Y"}]&/@%*)
(**)
(*IterativeSymmetricTally[p]*)
(**)
(*Graphics[{Thick,Line[#[[1]]],{RGBColor[1, 0, 0],Circle[{xMax/2+0.5,yMax/2+0.5},xMax/2-0.5]}},PlotRange->globalPlotRange,GridLines->globalGridLines,Frame->True,Axes->False,ImageSize->110,FrameLabel->{"X","Y"},PlotLabel->Row[{#[[2]]}]]&/@%*)


(* ::Subsubsection:: *)
(*Application to my paths*)


(* ::Input:: *)
(*symmetryResults=IterativeSymmetricTally[paths];*)


(* ::Input:: *)
(*Graphics[{Thick,Line[#[[1]]],{RGBColor[1, 0, 0],Circle[{xMax/2+0.5,yMax/2+0.5},xMax/2-0.5]}},PlotRange->globalPlotRange,GridLines->globalGridLines,Frame->True,Axes->False,ImageSize->110,FrameLabel->{"X","Y"},PlotLabel->Row[{#[[2]]}]]&/@symmetryResults*)


(* ::Input:: *)
(*(*2. Extract components for your charts*)*)
(*Length[symmetryResults]*)
(*uniqueSymmetricPaths=symmetryResults[[All,1]];*)
(*symmetricCounts=symmetryResults[[All,2]];*)


(* ::Input:: *)
(*(*1. Calculate relative frequencies ("probabilities")*)*)
(*totalPaths=Length[paths];*)
(*symmetricProbabilities=symmetricCounts/totalPaths;*)
(**)
(**)
(*(*2. Convert each unique representative path into your clean coordinate string format*)symmetricPathLabels=Map[Row[Map[RowBox[{"{",Row[#,","],"}"}]//DisplayForm&,#],", "]&,uniqueSymmetricPaths];*)
(**)
(*symmetricPathLabels=ParallelMap[Graphics[{Thick,Line[#],{RGBColor[1, 0, 0],Circle[{xMax/2+0.5,yMax/2+0.5},xMax/2-0.5]}},*)
(*PlotRange->globalPlotRange,PlotRangePadding->0.2,*)
(*GridLines->globalGridLines,*)
(*Frame->True,FrameTicks->None,Axes->False,*)
(*ImageSize->200]&*)
(*,uniqueSymmetricPaths];*)


(* ::Input:: *)
(*(*3. Generate the BarChart with values on top and unique symmetry families on the bottom*)*)
(*BarChart[symmetricProbabilities[[1;;15]],*)
(*ChartLabels->Placed[symmetricPathLabels,Axis],LabelingFunction->(Placed[Row[{NumberForm[100.*#1,{5,2}],"%"}],Above]&),Frame->True,FrameLabel->{"Unique Paths (Symmetry Families Included)","Relative Occurrence (Probability)"},PlotLabel->"Symmetric Path Distribution",*)
(*ChartStyle->RGBColor[0.87, 0.71, 0.34],ImageSize->900,PlotRange->{All,{0,Max[symmetricProbabilities]*1.15}}]*)


(* ::Subsubsection::Closed:: *)
(*Merge equal length paths. NOTE THAT THIS CHANGES THE PROBABILITIES BECAUSE THERE IS AN ENTROPIC FACTOR: SAME LENGTH THAT CAN BE REALIZED IN MANY WAYS GETS WEIGHTED MORE*)


(* ::Input:: *)
(*MergeTwoTalliesEqualLength[tally1_List,tally2_List]:=Module[{combined=tally1,matched,path2,count2},*)
(*Do[(*Extract components explicitly using Pattern Matching*)*)
(*path2=item[[1]];*)
(*count2=item[[2]];*)
(**)
(*matched=Position[combined,_?(SameQ[Length[#[[1]]],Length[path2]]&),{1},1]//Quiet;*)
(**)
(*If[Length[matched]>0,*)
(*combined[[matched[[1,1]],2]]+=count2,*)
(*AppendTo[combined,{path2,count2}]*)
(*],*)
(**)
(*{item,tally2} (*Loops through each full pair in tally2*)];*)
(**)
(*combined*)
(*];*)


(* ::Input:: *)
(*SplitListInHalf[expr_List]:={expr[[1;;Floor[Length[expr]/2]]],expr[[Floor[Length[expr]/2]+1;;-1]]}*)


(* ::Input:: *)
(*egList={{{1,2,3},a},{{1,2,3,4},a},{{12,3,4},b},{{12,2,3,4},b},{{1,2,3},a},{{12,3,4},b}}*)
(*(*SplitListInHalf[egList]*)
(*Apply[MergeTwoTalliesEqualLength,SplitListInHalf[#]]&@egList*)*)
(*(*MergeTwoTalliesEqualLength@@%*)*)
(*FixedPoint[Apply[MergeTwoTalliesEqualLength,SplitListInHalf[#]]&,%]*)


(* ::Input:: *)
(*IterativeEqualLengthSymmetricTally[allTallies_List,safetyBreak_:100]:=Module[{merged},*)
(**)
(*merged=FixedPoint[Apply[MergeTwoTalliesEqualLength,SplitListInHalf[#]]&,allTallies,safetyBreak];*)
(**)
(*(*merged=Map[{Length[#[[1]]],#[[2]]}&,merged];*)*)
(**)
(*(*3. Final Sort by count in descending order*)*)
(*Reverse[SortBy[merged,Last]]*)
(**)
(*]*)


(* ::Input:: *)
(*IterativeEqualLengthSymmetricTally[allPaths_List,batchSize_Integer:10]:=Module[{batches,currentTallies},*)
(**)
(*(*1. Initial Step: partition and analyze individual batches*)*)
(*batches=Partition[allPaths,UpTo[batchSize]];*)
(*currentTallies=batches;*)
(*Return[currentTallies]*)
(**)
(*(*2. Reduction Loop: merge pairs of batches until only 1 master tally is left*)*)
(*While[Length[currentTallies]>1,*)
(**)
(*currentTallies=If[OddQ[Length[currentTallies]],*)
(**)
(*(*If odd number of batches,hold the last one and merge the pairs*)Append[Map[Apply[MergeTwoTalliesEqualLength],Partition[Drop[currentTallies,-1],2]],Last[currentTallies]],*)
(**)
(*(*If even,merge all pairs straight across*)*)
(*Map[Apply[MergeTwoTalliesEqualLength],Partition[currentTallies,2]]*)
(*];*)
(*Print["Remaining batch blocks to merge: ",Length[currentTallies]];*)
(*];*)
(**)
(*(*3. Final Sort by count in descending order*)*)
(*Reverse[SortBy[First[currentTallies],Last]]*)
(*];*)


(* ::Input:: *)
(*symmetryResults[[1;;10]]*)
(*(*Partition[%,2]*)*)
(*IterativeEqualLengthSymmetricTally[%,1]*)
(*Map[{Length[#[[1]]],#[[2]]}&,%];*)


(* ::Subsection::Closed:: *)
(*Application to data*)


(* ::Input:: *)
(*ParallelMap[Graphics[{Thick,Line[#],{RGBColor[1, 0, 0],Circle[{xMax/2+0.5,yMax/2+0.5},xMax/2-0.5]}},*)
(*PlotRange->globalPlotRange,PlotRangePadding->0.2,*)
(*GridLines->globalGridLines,*)
(*Frame->True,FrameTicks->None,Axes->False,*)
(*ImageSize->200]&*)
(*,uniqueSymmetricPaths];*)
(**)
(*symmetryResults[[All,1]][[1;;3]]*)
(*ParallelMap[Graphics[{Thick,Line[#],{RGBColor[1, 0, 0],Circle[{xMax/2+0.5,yMax/2+0.5},xMax/2-0.5]}},*)
(*PlotRange->globalPlotRange,PlotRangePadding->0.2,*)
(*GridLines->globalGridLines,*)
(*Frame->True,FrameTicks->None,Axes->False,*)
(*ImageSize->200]&*)
(*,%]*)


(* ::Input:: *)
(*(*REMARK: EVEN IF THE STRAIGHT LINE (LENGHT 3) IS THE MOST PROBABLE IN ABSOLUTE TERMS, WHEN CONSIDERING JUST THE LENGHT OF THE PATHS,  THE ENTROPY OF PATH OF LENGTH 4 AND 5 MAKES THEM ULTIMATELY MORE LIKELY!!! *)*)


(* ::Input:: *)
(*symmetryResults;*)
(**)
(*IterativeEqualLengthSymmetricTally[%,1];*)
(**)
(*equalLengthResults=Map[{Length[#[[1]]]-1,#[[2]]}&,%]*)


(* ::Input:: *)
(*(*2. Extract components for your charts*)*)
(*Length[equalLengthResults]*)
(*uniqueSymmetricPaths=equalLengthResults[[All,1]]*)
(*symmetricCounts=equalLengthResults[[All,2]];*)


(* ::Input:: *)
(*(*1. Calculate relative frequencies ("probabilities")*)*)
(*totalPaths=Length[paths];*)
(*symmetricProbabilities=symmetricCounts/totalPaths;*)
(**)
(**)
(*(*2. Convert each unique representative path into your clean coordinate string format*)symmetricPathLabels=Map[Row[Map[RowBox[{"{",Row[#,","],"}"}]//DisplayForm&,#],", "]&,uniqueSymmetricPaths];*)
(**)
(*symmetricPathLabels=uniqueSymmetricPaths;*)


(* ::Input:: *)
(*(*3. Generate the BarChart with values on top and unique symmetry families on the bottom*)*)
(*BarChart[symmetricProbabilities,*)
(*ChartLabels->Placed[symmetricPathLabels,Axis],LabelingFunction->(Placed[Row[{NumberForm[100.*#1,{5,2}],"%"}],Above]&),Frame->True,FrameLabel->{"Paths of Equal Length","Relative Occurrence (Probability)"},PlotLabel->"Symmetric Path Distribution",*)
(*ChartStyle->RGBColor[0.87, 0.71, 0.34],ImageSize->900,PlotRange->{All,{0,Max[symmetricProbabilities]*1.15}}]*)


(* ::Section::Closed:: *)
(*L=9 (active is 7) path Lengths histograms (b-1LRW-n9-UpdatedStopCond.csv)*)


(* ::Subsection:: *)
(*All paths*)


(* ::Input:: *)
(*n=7;*)
(*(*1. Read as text and split by double carriage returns/newlines*)rawBlocks=Rest[StringSplit[Import["D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\FullPaths\\b-1LRW-n9-UpdatedStopCond.csv","Text"],"\n\n"|"\r\n\r\n"]];*)
(*(*2. Convert each separate text block cleanly into an array of coordinate pairs*)*)
(*paths=ParallelMap[Drop[ImportString[#,"CSV"],{},-2]&,rawBlocks];*)
(*paths[[-1]];*)
(*Length@paths*)


(* ::Input:: *)
(*(*Render all paths simultaneously with distinct indexed colors*)Graphics[{Thick,MapIndexed[{ColorData[97][#2[[1]]],Line[#1]}&,paths]},Frame->True,Axes->False,ImageSize->500,FrameLabel->{"X Coordinate","Y Coordinate"}];*)
(**)
(*center=(n+1)/2;*)
(**)
(*Graphics[{Style[Circle[{center,center},center-1],Red],Thick,MapIndexed[{ColorData[97][#2[[1]]],Line[#1]}&,paths[[All]]],Point[{center,center}]},Frame->True,Axes->False,ImageSize->500,FrameLabel->{"X Coordinate","Y Coordinate"},PlotRange->All,GridLines->{Range[0,center*2],Range[0,center*2]}]*)


(* ::Input:: *)
(*allPoints=Flatten[paths,1];*)
(*{xMin,xMax}={Min[allPoints[[All,1]]],Max[allPoints[[All,1]]]};*)
(*{yMin,yMax}={Min[allPoints[[All,2]]],Max[allPoints[[All,2]]]};*)
(**)
(*globalPlotRange={{xMin-0.5,xMax+0.5},{yMin-0.5,yMax+0.5}};*)
(*globalGridLines={Range[xMin,xMax,1],Range[yMin,yMax,1]};*)
(**)
(**)
(*Graphics[{Thick,Line[#],{RGBColor[1, 0, 0],Circle[{xMax/2+0.5,yMax/2+0.5},xMax/2-0.5]}},PlotRange->globalPlotRange,GridLines->globalGridLines,Frame->True,Axes->False,ImageSize->110,FrameLabel->{"X","Y"}]&@paths[[1]]*)
(**)


(* ::Subsection:: *)
(*No symmetry taken into account*)


(* ::Input:: *)
(*(*2. Count the occurrences of each unique path*)*)
(*pathCounts=Tally[paths];*)
(*pathCounts[[1]]*)
(**)
(*pathCounts=Reverse[SortBy[pathCounts,Last]];*)
(*pathCounts[[1]]*)
(*(*Output format:{{path1,count1},{path2,count2},...}*)*)


(* ::Input:: *)
(*allPoints=Flatten[pathCounts[[All,1]],1];*)
(*{xMin,xMax}={Min[allPoints[[All,1]]],Max[allPoints[[All,1]]]};*)
(*{yMin,yMax}={Min[allPoints[[All,2]]],Max[allPoints[[All,2]]]};*)
(**)
(*globalPlotRange={{xMin-0.5,xMax+0.5},{yMin-0.5,yMax+0.5}};*)
(*globalGridLines={Range[xMin,xMax,1],Range[yMin,yMax,1]};*)
(**)
(*Graphics[{Thick,Line[#[[1]]],{RGBColor[1, 0, 0],Circle[{xMax/2+0.5,yMax/2+0.5},xMax/2-0.5]}},PlotRange->globalPlotRange,GridLines->globalGridLines,Frame->True,Axes->False,ImageSize->100,FrameLabel->{"X","Y"},PlotLabel->Row[{#[[2]]}]]&/@pathCounts[[All]]*)


(* ::Input:: *)
(*(*1. Calculate relative frequencies ("probabilities")*)*)
(*totalPaths=Length[paths];*)
(*countsOnly=pathCounts[[All,2]];*)
(*probabilities=countsOnly/totalPaths;*)
(**)
(*(*2. Convert each path (series of points) into a clean text label*)*)
(*(*Shortens coordinates to strings like "{(0,1),(1,2)}"*)*)
(*pathLabels=Map[Row[Map[RowBox[{"{",Row[#,","],"}"}]//DisplayForm&,#],", "]&,pathCounts[[All,1]]];*)
(**)
(*pathLabels=ParallelMap[Graphics[{Thick,Line[#],{RGBColor[1, 0, 0],Circle[{xMax/2+0.5,yMax/2+0.5},xMax/2-0.5]}},*)
(*PlotRange->globalPlotRange,PlotRangePadding->0.2,*)
(*GridLines->globalGridLines,*)
(*Frame->True,FrameTicks->None,Axes->False,*)
(*ImageSize->200]&*)
(*,pathCounts[[All,1]]];*)


(* ::Input:: *)
(*(*3. Generate the BarChart with values on top and unique symmetry families on the bottom*)*)
(*BarChart[probabilities[[1;;15]],*)
(*ChartLabels->Placed[pathLabels,Axis],LabelingFunction->(Placed[Row[{NumberForm[100.*#1,{5,2}],"%"}],Above]&),Frame->True,FrameLabel->{"Paths","Relative Occurrence (Probability)"},PlotLabel->"Path Distribution",*)
(*ChartStyle->RGBColor[0.87, 0.71, 0.34],ImageSize->900,PlotRange->{All,{0,Max[probabilities]*1.15}}]*)


(* ::Subsection::Closed:: *)
(*Gathering symmetric paths*)


(* ::Input:: *)
(*symmetryResults=IterativeSymmetricTally[paths];*)


(* ::Input:: *)
(*symmetryResults[[1]]*)


(* ::Input:: *)
(*Graphics[{Thick,Line[#[[1]]],{RGBColor[1, 0, 0],Circle[{xMax/2+0.5,yMax/2+0.5},xMax/2-0.5]}},PlotRange->globalPlotRange,GridLines->globalGridLines,Frame->True,Axes->False,ImageSize->110,FrameLabel->{"X","Y"},PlotLabel->Row[{#[[2]]}]]&/@symmetryResults*)


(* ::Input:: *)
(*(*2. Extract components for your charts*)*)
(*Length[symmetryResults]*)
(*uniqueSymmetricPaths=symmetryResults[[All,1]];*)
(*symmetricCounts=symmetryResults[[All,2]];*)


(* ::Input:: *)
(*(*1. Calculate relative frequencies ("probabilities")*)*)
(*totalPaths=Length[paths];*)
(*symmetricProbabilities=symmetricCounts/totalPaths;*)
(**)
(**)
(*(*2. Convert each unique representative path into your clean coordinate string format*)symmetricPathLabels=Map[Row[Map[RowBox[{"{",Row[#,","],"}"}]//DisplayForm&,#],", "]&,uniqueSymmetricPaths];*)
(**)
(*symmetricPathLabels=ParallelMap[Graphics[{Thick,Line[#],{RGBColor[1, 0, 0],Circle[{xMax/2+0.5,yMax/2+0.5},xMax/2-0.5]}},*)
(*PlotRange->globalPlotRange,PlotRangePadding->0.2,*)
(*GridLines->globalGridLines,*)
(*Frame->True,FrameTicks->None,Axes->False,*)
(*ImageSize->200]&*)
(*,uniqueSymmetricPaths];*)


(* ::Input:: *)
(*(*3. Generate the BarChart with values on top and unique symmetry families on the bottom*)*)
(*BarChart[symmetricProbabilities[[1;;15]],*)
(*ChartLabels->Placed[symmetricPathLabels,Axis],LabelingFunction->(Placed[Row[{NumberForm[100.*#1,{5,2}],"%"}],Above]&),Frame->True,FrameLabel->{"Unique Paths (Symmetry Families Included)","Relative Occurrence (Probability)"},PlotLabel->"Symmetric Path Distribution",*)
(*ChartStyle->RGBColor[0.87, 0.71, 0.34],ImageSize->900,PlotRange->{All,{0,Max[symmetricProbabilities]*1.15}}]*)


(* ::Subsubsection::Closed:: *)
(*Merge equal length paths. NOTE THAT THIS CHANGES THE PROBABILITIES BECAUSE THERE IS AN ENTROPIC FACTOR: SAME LENGTH THAT CAN BE REALIZED IN MANY WAYS GETS WEIGHTED MORE*)


(* ::Input:: *)
(*MergeTwoTalliesEqualLength[tally1_List,tally2_List]:=Module[{combined=tally1,matched,path2,count2},*)
(*Do[(*Extract components explicitly using Pattern Matching*)*)
(*path2=item[[1]];*)
(*count2=item[[2]];*)
(**)
(*matched=Position[combined,_?(SameQ[Length[#[[1]]],Length[path2]]&),{1},1]//Quiet;*)
(**)
(*If[Length[matched]>0,*)
(*combined[[matched[[1,1]],2]]+=count2,*)
(*AppendTo[combined,{path2,count2}]*)
(*],*)
(**)
(*{item,tally2} (*Loops through each full pair in tally2*)];*)
(**)
(*combined*)
(*];*)


(* ::Input:: *)
(*SplitListInHalf[expr_List]:={expr[[1;;Floor[Length[expr]/2]]],expr[[Floor[Length[expr]/2]+1;;-1]]}*)


(* ::Input:: *)
(*egList={{{1,2,3},a},{{1,2,3,4},a},{{12,3,4},b},{{12,2,3,4},b},{{1,2,3},a},{{12,3,4},b}}*)
(*(*SplitListInHalf[egList]*)
(*Apply[MergeTwoTalliesEqualLength,SplitListInHalf[#]]&@egList*)*)
(*(*MergeTwoTalliesEqualLength@@%*)*)
(*FixedPoint[Apply[MergeTwoTalliesEqualLength,SplitListInHalf[#]]&,%]*)


(* ::Input:: *)
(*IterativeEqualLengthSymmetricTally[allTallies_List,safetyBreak_:100]:=Module[{merged},*)
(**)
(*merged=FixedPoint[Apply[MergeTwoTalliesEqualLength,SplitListInHalf[#]]&,allTallies,safetyBreak];*)
(**)
(*(*merged=Map[{Length[#[[1]]],#[[2]]}&,merged];*)*)
(**)
(*(*3. Final Sort by count in descending order*)*)
(*Reverse[SortBy[merged,Last]]*)
(**)
(*]*)


(* ::Input:: *)
(*IterativeEqualLengthSymmetricTally[allPaths_List,batchSize_Integer:10]:=Module[{batches,currentTallies},*)
(**)
(*(*1. Initial Step: partition and analyze individual batches*)*)
(*batches=Partition[allPaths,UpTo[batchSize]];*)
(*currentTallies=batches;*)
(*Return[currentTallies]*)
(**)
(*(*2. Reduction Loop: merge pairs of batches until only 1 master tally is left*)*)
(*While[Length[currentTallies]>1,*)
(**)
(*currentTallies=If[OddQ[Length[currentTallies]],*)
(**)
(*(*If odd number of batches,hold the last one and merge the pairs*)Append[Map[Apply[MergeTwoTalliesEqualLength],Partition[Drop[currentTallies,-1],2]],Last[currentTallies]],*)
(**)
(*(*If even,merge all pairs straight across*)*)
(*Map[Apply[MergeTwoTalliesEqualLength],Partition[currentTallies,2]]*)
(*];*)
(*Print["Remaining batch blocks to merge: ",Length[currentTallies]];*)
(*];*)
(**)
(*(*3. Final Sort by count in descending order*)*)
(*Reverse[SortBy[First[currentTallies],Last]]*)
(*];*)


(* ::Input:: *)
(*symmetryResults[[1;;10]]*)
(*(*Partition[%,2]*)*)
(*IterativeEqualLengthSymmetricTally[%,1]*)
(*Map[{Length[#[[1]]],#[[2]]}&,%];*)


(* ::Subsection::Closed:: *)
(*Application to data*)


(* ::Input:: *)
(*ParallelMap[Graphics[{Thick,Line[#],{RGBColor[1, 0, 0],Circle[{xMax/2+0.5,yMax/2+0.5},xMax/2-0.5]}},*)
(*PlotRange->globalPlotRange,PlotRangePadding->0.2,*)
(*GridLines->globalGridLines,*)
(*Frame->True,FrameTicks->None,Axes->False,*)
(*ImageSize->200]&*)
(*,uniqueSymmetricPaths];*)
(**)
(*symmetryResults[[All,1]][[1;;3]]*)
(*ParallelMap[Graphics[{Thick,Line[#],{RGBColor[1, 0, 0],Circle[{xMax/2+0.5,yMax/2+0.5},xMax/2-0.5]}},*)
(*PlotRange->globalPlotRange,PlotRangePadding->0.2,*)
(*GridLines->globalGridLines,*)
(*Frame->True,FrameTicks->None,Axes->False,*)
(*ImageSize->200]&*)
(*,%]*)


(* ::Input:: *)
(*(*REMARK: EVEN IF THE STRAIGHT LINE (LENGHT 3) IS THE MOST PROBABLE IN ABSOLUTE TERMS, WHEN CONSIDERING JUST THE LENGHT OF THE PATHS,  THE ENTROPY OF PATH OF LENGTH 4 AND 5 MAKES THEM ULTIMATELY MORE LIKELY!!! *)*)


(* ::Input:: *)
(*symmetryResults;*)
(**)
(*IterativeEqualLengthSymmetricTally[%,1];*)
(**)
(*equalLengthResults=Map[{Length[#[[1]]]-1,#[[2]]}&,%]*)


(* ::Input:: *)
(*(*2. Extract components for your charts*)*)
(*Length[equalLengthResults]*)
(*uniqueSymmetricPaths=equalLengthResults[[All,1]]*)
(*symmetricCounts=equalLengthResults[[All,2]];*)


(* ::Input:: *)
(*(*1. Calculate relative frequencies ("probabilities")*)*)
(*totalPaths=Length[paths];*)
(*symmetricProbabilities=symmetricCounts/totalPaths;*)
(**)
(**)
(*(*2. Convert each unique representative path into your clean coordinate string format*)symmetricPathLabels=Map[Row[Map[RowBox[{"{",Row[#,","],"}"}]//DisplayForm&,#],", "]&,uniqueSymmetricPaths];*)
(**)
(*symmetricPathLabels=uniqueSymmetricPaths;*)


(* ::Input:: *)
(*(*3. Generate the BarChart with values on top and unique symmetry families on the bottom*)*)
(*BarChart[symmetricProbabilities,*)
(*ChartLabels->Placed[symmetricPathLabels,Axis],LabelingFunction->(Placed[Row[{NumberForm[100.*#1,{5,2}],"%"}],Above]&),Frame->True,FrameLabel->{"Paths of Equal Length","Relative Occurrence (Probability)"},PlotLabel->"Symmetric Path Distribution",*)
(*ChartStyle->RGBColor[0.87, 0.71, 0.34],ImageSize->900,PlotRange->{All,{0,Max[symmetricProbabilities]*1.15}}]*)


(* ::Section::Closed:: *)
(*L=7 (n=15, active is 7) path Lengths histograms (b=1_LRW-2d-square-lattice-data-15_repeat-200000_tol-1e-10-LogSpacing.csv (x2))*)


(* ::Input:: *)
(*n=4;*)
(*(*1. Read as text and split by double carriage returns/newlines*)rawBlocks=Import["D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d\\b=1_LRW-2d-square-lattice-data-15_repeat-200000_tol-1e-10-LogSpacing.csv","CSV"];*)
(*(*2. Convert each separate text block cleanly into an array of coordinate pairs*)*)
(*paths=Developer`ToPackedArray[rawBlocks];*)
(*paths[[1;;10]]*)
(*pathDistr=paths[[All,2]];*)
(*Length@pathDistr*)


(* ::Input:: *)
(*Histogram[#,Length[#],"PDF",PlotRange->All]&@pathDistr*)


(* ::Input:: *)
(*(*1. Calculate relative frequencies ("probabilities")*)*)
(*Sort@Tally[pathDistr]*)
(*{pathDistr2,symmetricCounts}=Transpose[%]*)


(* ::Input:: *)
(*pathDistr2==Sort@DeleteDuplicates@pathDistr*)


(* ::Input:: *)
(*totalPaths=Length[pathDistr];*)
(*symmetricProbabilities=symmetricCounts/totalPaths;*)
(**)


(* ::Input:: *)
(*(*3. Generate the BarChart with values on top and unique symmetry families on the bottom*)BarChart[symmetricProbabilities[[#]],ChartLabels->Placed[pathDistr2,Axis],LabelingFunction->(Placed[Row[{NumberForm[100.*#1,{5,2}],"%"}],Above]&),Frame->True,FrameLabel->{"Unique Paths (Symmetry Families Included)","Relative Occurrence (Probability)"},PlotLabel->"Symmetric Path Distribution",ChartStyle->RGBColor[0.87,0.71,0.34],ImageSize->900(*,PlotRange->{All,{0,Max[symmetricProbabilities]*1.15}}*)]&@Span[All]*)


(* ::Text:: *)
(*Compared to the analytical result below, (6->1.04258%) we are a little off *)


(* ::Section:: *)
(*L=7 (n=15, active is 8) path Lengths histograms (b=1_LRW-2d-square-lattice-data-15_repeat-100000_tol-1e-10-SquareBC-LogSpacing.csv (x2) )*)


(* ::Input:: *)
(*n=4;*)
(*(*1. Read as text and split by double carriage returns/newlines*)rawBlocks=Import["D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\2d\\b=1_LRW-2d-square-lattice-data-15_repeat-100000_tol-1e-10-SquareBC-LogSpacing.csv","CSV"];*)
(*(*2. Convert each separate text block cleanly into an array of coordinate pairs*)*)
(*paths=Developer`ToPackedArray[rawBlocks];*)
(*paths[[1;;10]]*)
(*pathDistr=paths[[All,2]];*)
(*Length@pathDistr*)


(* ::Input:: *)
(*Histogram[#,Length[#],"PDF",PlotRange->All]&@pathDistr*)


(* ::Input:: *)
(*(*1. Calculate relative frequencies ("probabilities")*)*)
(*Sort@Tally[pathDistr]*)
(*{pathDistr2,symmetricCounts}=Transpose[%]*)


(* ::Input:: *)
(*pathDistr2==Sort@DeleteDuplicates@pathDistr*)


(* ::Input:: *)
(*totalPaths=Length[pathDistr];*)
(*symmetricProbabilities=symmetricCounts/totalPaths;*)
(**)


(* ::Input:: *)
(*(*3. Generate the BarChart with values on top and unique symmetry families on the bottom*)BarChart[symmetricProbabilities[[#]],ChartLabels->Placed[pathDistr2,Axis],LabelingFunction->(Placed[Row[{NumberForm[100.*#1,{5,2}],"%"}],Above]&),Frame->True,FrameLabel->{"Unique Paths (Symmetry Families Included)","Relative Occurrence (Probability)"},PlotLabel->"Symmetric Path Distribution",ChartStyle->RGBColor[0.87,0.71,0.34],ImageSize->900(*,PlotRange->{All,{0,Max[symmetricProbabilities]*1.15}}*)]&@Span[All]*)


(* ::Text:: *)
(*Compared to the analytical result below, (7->0.47%, 8->4.57%) we are FAR AWAY!!!!!!  *)


(* ::Section::Closed:: *)
(*L=15 (active is 13) path Lengths histograms (b-1LRW-n15-YesReordering-Double.csv )*)


(* ::Subsection:: *)
(*All paths*)


(* ::Input:: *)
(*n=15;*)
(*(*1. Read as text and split by double carriage returns/newlines*)rawBlocks=Rest[StringSplit[Import["D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\FullPaths\\b-1LRW-n15-YesReordering-Double.csv","Text"],"\n\n"|"\r\n\r\n"]];*)
(*(*2. Convert each separate text block cleanly into an array of coordinate pairs*)*)
(*paths=ParallelMap[Drop[ImportString[#,"CSV"],{},-2]&,rawBlocks];*)
(*paths[[-1]];*)
(*Length@paths*)


(* ::Input:: *)
(*(*Render all paths simultaneously with distinct indexed colors*)Graphics[{Thick,MapIndexed[{ColorData[97][#2[[1]]],Line[#1]}&,paths]},Frame->True,Axes->False,ImageSize->500,FrameLabel->{"X Coordinate","Y Coordinate"}];*)
(**)
(*center=(n+1)/2;*)
(**)
(*Graphics[{Style[Circle[{center,center},center-1],Red],Thick,MapIndexed[{ColorData[97][#2[[1]]],Line[#1]}&,paths[[All]]],Point[{center,center}]},Frame->True,Axes->False,ImageSize->500,FrameLabel->{"X Coordinate","Y Coordinate"},PlotRange->All,GridLines->{Range[0,center*2],Range[0,center*2]}];*)


(* ::Input:: *)
(*allPoints=Flatten[paths,1];*)
(*{xMin,xMax}={Min[allPoints[[All,1]]],Max[allPoints[[All,1]]]};*)
(*{yMin,yMax}={Min[allPoints[[All,2]]],Max[allPoints[[All,2]]]};*)
(**)
(*globalPlotRange={{xMin-0.5,xMax+0.5},{yMin-0.5,yMax+0.5}};*)
(*globalGridLines={Range[xMin,xMax,1],Range[yMin,yMax,1]};*)
(**)
(**)
(*Graphics[{Thick,Line[#],{RGBColor[1, 0, 0],Circle[{xMax/2+0.5,yMax/2+0.5},xMax/2-0.5]}},PlotRange->globalPlotRange,GridLines->globalGridLines,Frame->True,Axes->False,ImageSize->110,FrameLabel->{"X","Y"}]&@paths[[1]]*)
(**)


(* ::Subsection:: *)
(*No symmetry taken into account*)


(* ::Input:: *)
(*(*2. Count the occurrences of each unique path*)*)
(*pathCounts=Tally[paths];*)
(*pathCounts[[1]]*)
(**)
(*pathCounts=Reverse[SortBy[pathCounts,Last]];*)
(*pathCounts[[1]]*)
(**)
(*Graphics[{Thick,Line[#[[1]]],{RGBColor[1, 0, 0],Circle[{xMax/2+0.5,yMax/2+0.5},xMax/2-0.5]}},PlotRange->globalPlotRange,GridLines->globalGridLines,Frame->True,Axes->False,ImageSize->100,FrameLabel->{"X","Y"},PlotLabel->Row[{#[[2]]}]]&/@pathCounts[[{1}]]*)
(*(*Output format:{{path1,count1},{path2,count2},...}*)*)


(* ::Input:: *)
(*allPoints=Flatten[pathCounts[[All,1]],1];*)
(*{xMin,xMax}={Min[allPoints[[All,1]]],Max[allPoints[[All,1]]]};*)
(*{yMin,yMax}={Min[allPoints[[All,2]]],Max[allPoints[[All,2]]]};*)
(**)
(*globalPlotRange={{xMin-0.5,xMax+0.5},{yMin-0.5,yMax+0.5}};*)
(*globalGridLines={Range[xMin,xMax,1],Range[yMin,yMax,1]};*)
(**)
(*Graphics[{Thick,Line[#[[1]]],{RGBColor[1, 0, 0],Circle[{xMax/2+0.5,yMax/2+0.5},xMax/2-0.5]}},PlotRange->globalPlotRange,GridLines->globalGridLines,Frame->True,Axes->False,ImageSize->100,FrameLabel->{"X","Y"},PlotLabel->Row[{#[[2]]}]]&/@pathCounts[[All]];*)


(* ::Input:: *)
(*(*1. Calculate relative frequencies ("probabilities")*)*)
(*totalPaths=Length[paths];*)
(*countsOnly=pathCounts[[All,2]];*)
(*probabilities=countsOnly/totalPaths;*)
(**)
(*(*2. Convert each path (series of points) into a clean text label*)*)
(*(*Shortens coordinates to strings like "{(0,1),(1,2)}"*)*)
(*pathLabels=Map[Row[Map[RowBox[{"{",Row[#,","],"}"}]//DisplayForm&,#],", "]&,pathCounts[[All,1]]];*)
(**)
(*pathLabels=ParallelMap[Graphics[{Thick,Line[#],{RGBColor[1, 0, 0],Circle[{xMax/2+0.5,yMax/2+0.5},xMax/2-0.5]}},*)
(*PlotRange->globalPlotRange,PlotRangePadding->0.2,*)
(*GridLines->globalGridLines,*)
(*Frame->True,FrameTicks->None,Axes->False,*)
(*ImageSize->200]&*)
(*,pathCounts[[All,1]]];*)


(* ::Input:: *)
(*(*3. Generate the BarChart with values on top and unique symmetry families on the bottom*)*)
(*BarChart[probabilities[[#]],*)
(*ChartLabels->Placed[pathLabels[[#]],Axis],LabelingFunction->(Placed[Row[{NumberForm[100.00*#1,{6,3}],"%"}],Above]&),Frame->True,FrameLabel->{"Paths","Relative Occurrence (Probability)"},PlotLabel->"Path Distribution",*)
(*ChartStyle->RGBColor[0.87, 0.71, 0.34],ImageSize->900,PlotRange->{All,{0,Max[probabilities]*1.15}}]&@Span[1,15]*)


(* ::Input:: *)
(*(*Almost correct...Check BC*)*)


(* ::Subsection::Closed:: *)
(*Gathering symmetric paths TOO LONG TO BE RUN*)


(* ::Input:: *)
(*Length@paths*)
(*Head@paths*)


(* ::Input:: *)
(*symmetryResults=IterativeSymmetricTally[paths[[All]],500];*)


(* ::Input:: *)
(*symmetryResults[[1]]*)


(* ::Input:: *)
(* *)


(* ::Input:: *)
(*Graphics[{Thick,Line[#[[1]]],{RGBColor[1, 0, 0],Circle[{xMax/2+0.5,yMax/2+0.5},xMax/2-0.5]}},PlotRange->globalPlotRange,GridLines->globalGridLines,Frame->True,Axes->False,ImageSize->100,FrameLabel->{"X","Y"},PlotLabel->Row[{#[[2]]}]]&/@symmetryResults;*)
(*%[[1;;15]]*)
(**)
(*(*2. Extract components for your charts*)*)
(*uniqueSymmetricPaths=symmetryResults[[All,1]];*)
(*symmetricCounts=symmetryResults[[All,2]];*)


(* ::Input:: *)
(*%*)


(* ::Input:: *)
(*(*1. Calculate relative frequencies ("probabilities")*)*)
(*totalPaths=Length[paths];*)
(*symmetricProbabilities=symmetricCounts/totalPaths;*)
(**)
(**)
(*(*2. Convert each unique representative path into your clean coordinate string format*)symmetricPathLabels=Map[Row[Map[RowBox[{"{",Row[#,","],"}"}]//DisplayForm&,#],", "]&,uniqueSymmetricPaths];*)
(**)
(*symmetricPathLabels=Map[Graphics[{Thick,Line[#],{RGBColor[1, 0, 0],Circle[{xMax/2+0.5,yMax/2+0.5},xMax/2-0.5]}},*)
(*PlotRange->globalPlotRange,PlotRangePadding->0.2,*)
(*GridLines->globalGridLines,*)
(*Frame->True,FrameTicks->None,Axes->False,*)
(*ImageSize->200]&*)
(*,uniqueSymmetricPaths];*)
(**)
(*(*3. Generate the BarChart with values on top and unique symmetry families on the bottom*)BarChart[symmetricProbabilities,ChartLabels->Placed[symmetricPathLabels,Axis],LabelingFunction->(Placed[Row[{NumberForm[100.*#1,{5,2}],"%"}],Above]&),Frame->True,FrameLabel->{"Unique Paths (Symmetry Families Included)","Relative Occurrence (Probability)"},PlotLabel->"Symmetric Path Distribution",ChartStyle->RGBColor[0.87,0.71,0.34],ImageSize->900,PlotRange->{All,{0,Max[symmetricProbabilities]*1.15}}]*)


(* ::Section::Closed:: *)
(*L=43 (active is 41) path Lengths histograms (b-1LRW-n43-UpdatedStopCond.csv)*)


(* ::Input:: *)
(*(*1. Read as text and split by double carriage returns/newlines*)rawBlocks=Rest[StringSplit[Import["D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\FullPaths\\b-1LRW-n43-UpdatedStopCond.csv","Text"],"\n\n"|"\r\n\r\n"]];*)
(*(*2. Convert each separate text block cleanly into an array of coordinate pairs*)*)
(*paths=ParallelMap[Drop[ImportString[#,"CSV"],{},-2]&,rawBlocks];*)
(*paths[[-1]]*)
(*Length@paths*)


(* ::Input:: *)
(*(*Render all paths simultaneously with distinct indexed colors*)Graphics[{Style[Circle[{21,21},20],Red],Point[{21,21}],Thick,MapIndexed[{ColorData[97][#2[[1]]],Line[#1]}&,paths[[-2;;-1]]]},Frame->True,Axes->False,ImageSize->500,FrameLabel->{"X Coordinate","Y Coordinate"},PlotRange->All,GridLines->{Range[0,42],Range[0,42]}]*)


(* ::Section::Closed:: *)
(*L=101 (active is 99) path Lengths histograms (b-1LRW-n101-UpdatedStopCond.csv)*)


(* ::Input:: *)
(*n=99;*)
(*(*1. Read as text and split by double carriage returns/newlines*)rawBlocks=Rest[StringSplit[Import["D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\FullPaths\\b-1LRW-n101-UpdatedStopCond.csv","Text"],"\n\n"|"\r\n\r\n"]];*)
(*(*2. Convert each separate text block cleanly into an array of coordinate pairs*)*)
(*paths=ParallelMap[Drop[ImportString[#,"CSV"],{},-2]&,rawBlocks];*)
(*paths[[-1]];*)
(*Length@paths*)


(* ::Input:: *)
(*(*Render all paths simultaneously with distinct indexed colors*)Graphics[{Thick,MapIndexed[{ColorData[97][#2[[1]]],Line[#1]}&,paths]},Frame->True,Axes->False,ImageSize->500,FrameLabel->{"X Coordinate","Y Coordinate"}];*)
(**)
(*center=(n+1)/2;*)
(**)
(*Graphics[{Style[Circle[{center,center},center-1],Red],Thick,MapIndexed[{ColorData[97][#2[[1]]],Line[#1]}&,paths[[All]]],Point[{center,center}]},Frame->True,Axes->False,ImageSize->500,FrameLabel->{"X Coordinate","Y Coordinate"},PlotRange->All,GridLines->{Range[0,center*2],Range[0,center*2]}]*)


(* ::Title::Closed:: *)
(*b=4*)


(* ::Section:: *)
(*L=9 (active is 7) path Lengths histograms (b-4LRW-n9-YesReordering-Double.csv )*)


(* ::Subsection:: *)
(*All paths*)


(* ::Input:: *)
(*n=4;*)
(*(*1. Read as text and split by double carriage returns/newlines*)rawBlocks=Rest[StringSplit[Import["D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\FullPaths\\b-4LRW-n9-YesReordering-Double.csv","Text"],"\n\n"|"\r\n\r\n"]];*)
(*(*2. Convert each separate text block cleanly into an array of coordinate pairs*)*)
(*paths=ParallelMap[Drop[ImportString[#,"CSV"],{},-2]&,rawBlocks];*)
(*paths[[-1]];*)
(*Length@paths*)


(* ::Input:: *)
(*(*Render all paths simultaneously with distinct indexed colors*)Graphics[{Thick,MapIndexed[{ColorData[97][#2[[1]]],Line[#1]}&,paths]},Frame->True,Axes->False,ImageSize->500,FrameLabel->{"X Coordinate","Y Coordinate"}];*)
(**)
(*center=(n+1)/2;*)
(**)
(*Graphics[{Style[Circle[{center,center},center-1],Red],Thick,MapIndexed[{ColorData[97][#2[[1]]],Line[#1]}&,paths[[All]]],Point[{center,center}]},Frame->True,Axes->False,ImageSize->500,FrameLabel->{"X Coordinate","Y Coordinate"},PlotRange->All,GridLines->{Range[0,center*2],Range[0,center*2]}];*)


(* ::Input:: *)
(*allPoints=Flatten[paths,1];*)
(*{xMin,xMax}={Min[allPoints[[All,1]]],Max[allPoints[[All,1]]]};*)
(*{yMin,yMax}={Min[allPoints[[All,2]]],Max[allPoints[[All,2]]]};*)
(**)
(*globalPlotRange={{xMin-0.5,xMax+0.5},{yMin-0.5,yMax+0.5}};*)
(*globalGridLines={Range[xMin,xMax,1],Range[yMin,yMax,1]};*)
(**)
(**)
(*Graphics[{Thick,Line[#],{RGBColor[1, 0, 0],Circle[{xMax/2+0.5,yMax/2+0.5},xMax/2-0.5]}},PlotRange->globalPlotRange,GridLines->globalGridLines,Frame->True,Axes->False,ImageSize->110,FrameLabel->{"X","Y"}]&@paths[[1]]*)
(**)


(* ::Subsection:: *)
(*No symmetry taken into account*)


(* ::Input:: *)
(*(*2. Count the occurrences of each unique path*)*)
(*pathCounts=Tally[paths];*)
(*pathCounts[[1]]*)
(**)
(*pathCounts=Reverse[SortBy[pathCounts,Last]];*)
(*pathCounts[[1]]*)
(*(*Output format:{{path1,count1},{path2,count2},...}*)*)


(* ::Input:: *)
(*allPoints=Flatten[pathCounts[[All,1]],1];*)
(*{xMin,xMax}={Min[allPoints[[All,1]]],Max[allPoints[[All,1]]]};*)
(*{yMin,yMax}={Min[allPoints[[All,2]]],Max[allPoints[[All,2]]]};*)
(**)
(*globalPlotRange={{xMin-0.5,xMax+0.5},{yMin-0.5,yMax+0.5}};*)
(*globalGridLines={Range[xMin,xMax,1],Range[yMin,yMax,1]};*)
(**)
(*Graphics[{Thick,Line[#[[1]]],{RGBColor[1, 0, 0],Circle[{xMax/2+0.5,yMax/2+0.5},xMax/2-0.5]}},PlotRange->globalPlotRange,GridLines->globalGridLines,Frame->True,Axes->False,ImageSize->100,FrameLabel->{"X","Y"},PlotLabel->Row[{#[[2]]}]]&/@pathCounts[[All]];*)


(* ::Input:: *)
(*(*1. Calculate relative frequencies ("probabilities")*)*)
(*totalPaths=Length[paths];*)
(*countsOnly=pathCounts[[All,2]];*)
(*probabilities=countsOnly/totalPaths;*)
(**)
(*(*2. Convert each path (series of points) into a clean text label*)*)
(*(*Shortens coordinates to strings like "{(0,1),(1,2)}"*)*)
(*pathLabels=Map[Row[Map[RowBox[{"{",Row[#,","],"}"}]//DisplayForm&,#],", "]&,pathCounts[[All,1]]];*)
(**)
(*pathLabels=ParallelMap[Graphics[{Thick,Line[#],{RGBColor[1, 0, 0],Circle[{xMax/2+0.5,yMax/2+0.5},xMax/2-0.5]}},*)
(*PlotRange->globalPlotRange,PlotRangePadding->0.2,*)
(*GridLines->globalGridLines,*)
(*Frame->True,FrameTicks->None,Axes->False,*)
(*ImageSize->200]&*)
(*,pathCounts[[All,1]]];*)


(* ::Input:: *)
(*(*3. Generate the BarChart with values on top and unique symmetry families on the bottom*)*)
(*BarChart[probabilities[[1;;15]],*)
(*ChartLabels->Placed[pathLabels,Axis],LabelingFunction->(Placed[Row[{NumberForm[100.*#1,{5,3}],"%"}],Above]&),Frame->True,FrameLabel->{"Paths","Relative Occurrence (Probability)"},PlotLabel->"Path Distribution",*)
(*ChartStyle->RGBColor[0.87, 0.71, 0.34],ImageSize->900,PlotRange->{All,{0,Max[probabilities]*1.15}}]*)


(* ::Input:: *)
(*(*In agreement with 10.92%*)*)


(* ::Subsection:: *)
(*Gathering symmetric paths*)


(* ::Subsubsection:: *)
(*Actual application*)


(* ::Input:: *)
(*Length@paths*)
(*Head@paths*)


(* ::Input:: *)
(*symmetryResults=IterativeSymmetricTally[paths];*)
(*Graphics[{Thick,Line[#[[1]]],{RGBColor[1, 0, 0],Circle[{xMax/2+0.5,yMax/2+0.5},xMax/2-0.5]}},PlotRange->globalPlotRange,GridLines->globalGridLines,Frame->True,Axes->False,ImageSize->100,FrameLabel->{"X","Y"},PlotLabel->Row[{#[[2]]}]]&/@%*)
(**)
(*(*2. Extract components for your charts*)*)
(*uniqueSymmetricPaths=symmetryResults[[All,1]];*)
(*symmetricCounts=symmetryResults[[All,2]];*)


(* ::Input:: *)
(*(*1. Calculate relative frequencies ("probabilities")*)*)
(*totalPaths=Length[paths];*)
(*symmetricProbabilities=symmetricCounts/totalPaths;*)
(**)
(**)
(*(*2. Convert each unique representative path into your clean coordinate string format*)symmetricPathLabels=Map[Row[Map[RowBox[{"{",Row[#,","],"}"}]//DisplayForm&,#],", "]&,uniqueSymmetricPaths];*)
(**)
(*symmetricPathLabels=Map[Graphics[{Thick,Line[#],{RGBColor[1, 0, 0],Circle[{xMax/2+0.5,yMax/2+0.5},xMax/2-0.5]}},*)
(*PlotRange->globalPlotRange,PlotRangePadding->0.2,*)
(*GridLines->globalGridLines,*)
(*Frame->True,FrameTicks->None,Axes->False,*)
(*ImageSize->200]&*)
(*,uniqueSymmetricPaths];*)
(**)
(*(*3. Generate the BarChart with values on top and unique symmetry families on the bottom*)BarChart[symmetricProbabilities[[#]],ChartLabels->Placed[symmetricPathLabels[[#]],Axis],LabelingFunction->(Placed[Row[{NumberForm[100.*#1,{5,2}],"%"}],Above]&),Frame->True,FrameLabel->{"Unique Paths (Symmetry Families Included)","Relative Occurrence (Probability)"},PlotLabel->"Symmetric Path Distribution",ChartStyle->RGBColor[0.87,0.71,0.34],ImageSize->900,PlotRange->{All,{0,Max[symmetricProbabilities]*1.15}}]&@Span[1,10]*)


(* ::Input:: *)
(*(*Nice agreement with straight "Symmetric paths: "43.71481402913483`"%"*)
(*and BigL "Symmetric paths: "19.859709530369727`"%"*)*)


(* ::Title::Closed:: *)
(*b=15*)


(* ::Section::Closed:: *)
(*L=9 (active is 7) path Lengths histograms (b-15LRW-n9-YesReordering-Double.csv )*)


(* ::Subsection:: *)
(*All paths*)


(* ::Input:: *)
(*n=4;*)
(*(*1. Read as text and split by double carriage returns/newlines*)rawBlocks=Rest[StringSplit[Import["D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\FullPaths\\b-15LRW-n9-YesReordering-Double.csv","Text"],"\n\n"|"\r\n\r\n"]];*)
(*(*2. Convert each separate text block cleanly into an array of coordinate pairs*)*)
(*paths=ParallelMap[Drop[ImportString[#,"CSV"],{},-2]&,rawBlocks];*)
(*paths[[-1]];*)
(*Length@paths*)


(* ::Input:: *)
(*(*Render all paths simultaneously with distinct indexed colors*)Graphics[{Thick,MapIndexed[{ColorData[97][#2[[1]]],Line[#1]}&,paths]},Frame->True,Axes->False,ImageSize->500,FrameLabel->{"X Coordinate","Y Coordinate"}];*)
(**)
(*center=(n+1)/2;*)
(**)
(*Graphics[{Style[Circle[{center,center},center-1],Red],Thick,MapIndexed[{ColorData[97][#2[[1]]],Line[#1]}&,paths[[All]]],Point[{center,center}]},Frame->True,Axes->False,ImageSize->500,FrameLabel->{"X Coordinate","Y Coordinate"},PlotRange->All,GridLines->{Range[0,center*2],Range[0,center*2]}];*)


(* ::Input:: *)
(*allPoints=Flatten[paths,1];*)
(*{xMin,xMax}={Min[allPoints[[All,1]]],Max[allPoints[[All,1]]]};*)
(*{yMin,yMax}={Min[allPoints[[All,2]]],Max[allPoints[[All,2]]]};*)
(**)
(*globalPlotRange={{xMin-0.5,xMax+0.5},{yMin-0.5,yMax+0.5}};*)
(*globalGridLines={Range[xMin,xMax,1],Range[yMin,yMax,1]};*)
(**)
(**)
(*Graphics[{Thick,Line[#],{RGBColor[1, 0, 0],Circle[{xMax/2+0.5,yMax/2+0.5},xMax/2-0.5]}},PlotRange->globalPlotRange,GridLines->globalGridLines,Frame->True,Axes->False,ImageSize->110,FrameLabel->{"X","Y"}]&@paths[[1]]*)
(**)


(* ::Subsection::Closed:: *)
(*No symmetry taken into account*)


(* ::Input:: *)
(*(*2. Count the occurrences of each unique path*)*)
(*pathCounts=Tally[paths];*)
(*pathCounts[[1]]*)
(**)
(*pathCounts=Reverse[SortBy[pathCounts,Last]];*)
(*pathCounts[[1]]*)
(*(*Output format:{{path1,count1},{path2,count2},...}*)*)


(* ::Input:: *)
(*allPoints=Flatten[pathCounts[[All,1]],1];*)
(*{xMin,xMax}={Min[allPoints[[All,1]]],Max[allPoints[[All,1]]]};*)
(*{yMin,yMax}={Min[allPoints[[All,2]]],Max[allPoints[[All,2]]]};*)
(**)
(*globalPlotRange={{xMin-0.5,xMax+0.5},{yMin-0.5,yMax+0.5}};*)
(*globalGridLines={Range[xMin,xMax,1],Range[yMin,yMax,1]};*)
(**)
(*Graphics[{Thick,Line[#[[1]]],{RGBColor[1, 0, 0],Circle[{xMax/2+0.5,yMax/2+0.5},xMax/2-0.5]}},PlotRange->globalPlotRange,GridLines->globalGridLines,Frame->True,Axes->False,ImageSize->100,FrameLabel->{"X","Y"},PlotLabel->Row[{#[[2]]}]]&/@pathCounts[[All]];*)


(* ::Input:: *)
(*(*1. Calculate relative frequencies ("probabilities")*)*)
(*totalPaths=Length[paths];*)
(*countsOnly=pathCounts[[All,2]];*)
(*probabilities=countsOnly/totalPaths;*)
(**)
(*(*2. Convert each path (series of points) into a clean text label*)*)
(*(*Shortens coordinates to strings like "{(0,1),(1,2)}"*)*)
(*pathLabels=Map[Row[Map[RowBox[{"{",Row[#,","],"}"}]//DisplayForm&,#],", "]&,pathCounts[[All,1]]];*)
(**)
(*pathLabels=ParallelMap[Graphics[{Thick,Line[#],{RGBColor[1, 0, 0],Circle[{xMax/2+0.5,yMax/2+0.5},xMax/2-0.5]}},*)
(*PlotRange->globalPlotRange,PlotRangePadding->0.2,*)
(*GridLines->globalGridLines,*)
(*Frame->True,FrameTicks->None,Axes->False,*)
(*ImageSize->200]&*)
(*,pathCounts[[All,1]]];*)


(* ::Input:: *)
(*(*3. Generate the BarChart with values on top and unique symmetry families on the bottom*)*)
(*BarChart[probabilities[[1;;15]],*)
(*ChartLabels->Placed[pathLabels,Axis],LabelingFunction->(Placed[Row[{NumberForm[100.*#1,{5,3}],"%"}],Above]&),Frame->True,FrameLabel->{"Paths","Relative Occurrence (Probability)"},PlotLabel->"Path Distribution",*)
(*ChartStyle->RGBColor[0.87, 0.71, 0.34],ImageSize->900,PlotRange->{All,{0,Max[probabilities]*1.15}}]*)


(* ::Input:: *)
(*(*In agreement with 10.92%*)*)


(* ::Subsection::Closed:: *)
(*Gathering symmetric paths*)


(* ::Subsubsection:: *)
(*Actual application*)


(* ::Input:: *)
(*Length@paths*)
(*Head@paths*)


(* ::Input:: *)
(*symmetryResults=IterativeSymmetricTally[paths,500];*)
(*Graphics[{Thick,Line[#[[1]]],{RGBColor[1, 0, 0],Circle[{xMax/2+0.5,yMax/2+0.5},xMax/2-0.5]}},PlotRange->globalPlotRange,GridLines->globalGridLines,Frame->True,Axes->False,ImageSize->100,FrameLabel->{"X","Y"},PlotLabel->Row[{#[[2]]}]]&/@%;*)
(**)
(*(*2. Extract components for your charts*)*)
(*uniqueSymmetricPaths=symmetryResults[[All,1]];*)
(*symmetricCounts=symmetryResults[[All,2]];*)


(* ::Input:: *)
(*(*1. Calculate relative frequencies ("probabilities")*)*)
(*totalPaths=Length[paths];*)
(*symmetricProbabilities=symmetricCounts/totalPaths;*)
(**)
(**)
(*(*2. Convert each unique representative path into your clean coordinate string format*)symmetricPathLabels=Map[Row[Map[RowBox[{"{",Row[#,","],"}"}]//DisplayForm&,#],", "]&,uniqueSymmetricPaths];*)
(**)
(*symmetricPathLabels=Map[Graphics[{Thick,Line[#],{RGBColor[1, 0, 0],Circle[{xMax/2+0.5,yMax/2+0.5},xMax/2-0.5]}},*)
(*PlotRange->globalPlotRange,PlotRangePadding->0.2,*)
(*GridLines->globalGridLines,*)
(*Frame->True,FrameTicks->None,Axes->False,*)
(*ImageSize->200]&*)
(*,uniqueSymmetricPaths];*)
(**)
(*(*3. Generate the BarChart with values on top and unique symmetry families on the bottom*)BarChart[symmetricProbabilities[[#]],ChartLabels->Placed[symmetricPathLabels[[#]],Axis],LabelingFunction->(Placed[Row[{NumberForm[100.*#1,{5,2}],"%"}],Above]&),Frame->True,FrameLabel->{"Unique Paths (Symmetry Families Included)","Relative Occurrence (Probability)"},PlotLabel->"Symmetric Path Distribution",ChartStyle->RGBColor[0.87,0.71,0.34],ImageSize->900,PlotRange->{All,{0,Max[symmetricProbabilities]*1.15}}]&@Span[1,5]*)


(* ::Input:: *)
(*(*Nice agreement with straight "Symmetric paths: "91.37472209616155`"%"*)
(*and BigL "Symmetric paths: "5.6092387257633165`"%"*)*)


(* ::Title:: *)
(*Automated solver of the Laplace equation*)


(* ::Section::Closed:: *)
(*MyGraph[]*)


(* ::Input:: *)
(*Clear[MyGraph];*)
(*Options[MyGraph]={"undirected" ->True,"label" ->False};*)
(*MyGraph[edges__,root_,source_,OptionsPattern[]]:={Module[{locEdges={},i,locWeights={},locRoot=root,locSource=source,p},*)
(*Clear[\[Beta]];*)
(**)
(*For[i=1, i<=(Dimensions@edges)[[1]],i++,*)
(*If[OptionValue["undirected"] && edges[[i,2]] =!=locSource && edges[[i,2]] =!=edges[[i,1]] ,*)
(*AppendTo[locEdges,edges[[i,2]]->edges[[i,1]]];*)
(*AppendTo[locWeights,\[Beta][edges[[i,2]],edges[[i,1]]] ]*)
(*];*)
(**)
(*If[edges[[i,1]]==locSource,Continue[]];*)
(**)
(*AppendTo[locEdges,edges[[i,1]]->edges[[i,2]]];*)
(*AppendTo[locWeights,\[Beta][edges[[i,1]],edges[[i,2]]] ]*)
(*];*)
(**)
(*locEdges=Sort[locEdges,(#1[[1]]<=#2[[1]] &&#1[[2]]<=#2[[1]])||(#1[[1]]<#2[[2]] &&#1[[2]]<=#2[[2]]) &];*)
(**)
(*p[\[Beta][a_,b_],\[Beta][c_,d_]]:=1/;(a<=c&&b<=c);*)
(*p[\[Beta][a_,b_],\[Beta][c_,d_]]:=-1/;(c<=a&&d<=a);*)
(*p[\[Beta][a_,b_],\[Beta][c_,d_]]:=1/;(a<d&&b<=d);*)
(*p[\[Beta][a_,b_],\[Beta][c_,d_]]:=-1/;(c<b&&d<=b);*)
(*locWeights=Sort[locWeights,p];*)
(**)
(*(*Print[locEdges];*)
(*Print[locWeights]*);*)
(**)
(*If[ OptionValue["label"],*)
(*(*True*)Graph[locEdges,EdgeWeight->locWeights, EdgeStyle->Blue, VertexLabels->{locRoot->ToString[locRoot]<>", ROOT",locSource->ToString[locSource]<>", SOURCE","Name"}, EdgeLabels->"EdgeWeight", VertexStyle->{locRoot->Red,locSource->Green,Blue},EdgeShapeFunction->"FilledArrow"],*)
(*(*False*)Graph[locEdges,EdgeWeight->locWeights, EdgeStyle->Blue, VertexLabels->{locRoot->ToString[locRoot]<>", ROOT",locSource->ToString[locSource]<>", SOURCE","Name"}, VertexStyle->{locRoot->Red,locSource->Green,Blue},EdgeShapeFunction->"FilledArrow"]]*)
(*],*)
(*root(*Root*),source(*Source*)}*)


(* ::Subsection:: *)
(*Usage example of MyGraph[]*)


(* ::Text:: *)
(*"edges" must be a list of ordered pairs containing the vertices of the graph. The edge is intended from the first vertex to the second.*)
(*If "directed" is false (optional, default to true), then a undirected graph is drawn*)


(* ::Input:: *)
(*edges = {{1,2},{2,3},{3,1}};*)
(*root=2;*)
(*source=3;*)
(*g=MyGraph[edges,root,source][[1]]*)


(* ::Input:: *)
(*gLabel=MyGraph[edges,root,source,"printEdgesLabel"->True]*)


(* ::Subsection:: *)
(*not myGraph*)


(* ::Input:: *)
(*n=3;*)
(**)
(*edges = Flatten[Table[Table[{{x*n+y,x*n+y+1},{x*n+y,x*n+y+n}},{x,0,n-1}],{y,0,n-1}],2];*)
(*edges = Flatten[Table[Table[{{{x,y},{x,y+1}},{{x,y},{x+1,y}}},{x,-Floor[n/2],Floor[n/2]}],{y,-Floor[n/2],Floor[n/2]}],2];*)
(*Flatten[edges/.{{a__},{b__}}->{{a}<->{b}},1]*)
(*Graph[%,VertexLabels->"Name"]*)


(* ::Section::Closed:: *)
(*LaplaceEqSolver[]*)


(* ::Input:: *)
(*Map[#[[1]]*n+#[[2]]&,{{1,2}}]*)


(* ::Input::Initialization:: *)
Clear[LaplaceEqSolver];
Options[LaplaceEqSolver]={"BC"->{{},{}} , "selectSolution"->0,"drawSolution"->False,"print"->False};

LaplaceEqSolver[graph_,OptionsPattern[]]:=Module[{locVertices,locIndices,locWeights,totWeights,locRoot,locSource,locBC,equations,variables={},x,solution,solutionLabels,i,n},
Clear[\[CapitalPhi]];


locVertices = VertexList@graph;
locIndices=Table[i,{i,Length[locVertices]}];
locWeights=WeightedAdjacencyMatrix[graph]//Normal;
totWeights =Total[locWeights,{2}];

(*Print[Row[{"locVertices=",locVertices," locWeights=",Normal[locWeights]," totWeights=",totWeights}]];*)


locRoot = 
 Select[(List@@@PropertyValue[graph,VertexLabels])[[2;;]],StringMatchQ[#[[2]],___~~"ROOT"]&][[All,1]][[1]];
locSource = 
 Select[(List@@@PropertyValue[graph,VertexLabels])[[2;;]],StringMatchQ[#[[2]],___~~"SOURCE"]&][[All,1]];


locBC=OptionValue["BC"];
(*
If[OptionValue["n"]>0,
locVertices=Map[#[[1]]*n+#[[2]]&,locVertices];
locRoot=Map[#[[1]]*n+#[[2]]&,{locRoot}][[1]];
locSource=Flatten[Map[#[[1]]*n+#[[2]]&,locSource],1];
];*)

If[Head[locSource]==List,
For[i=1,i<=Length[locSource],i++,

If[MemberQ[locBC[[2]],locSource[[i]]],Continue[]];
AppendTo[locBC[[1]],\[CapitalPhi][locSource[[i]]]== 1];
AppendTo[locBC[[2]],locSource[[i]]];

]
,(*FALSE*)
If[MemberQ[locBC[[2]],locSource],Continue[]];
AppendTo[locBC[[1]],\[CapitalPhi][locSource]== 1];
AppendTo[locBC[[2]],locSource];

];

AppendTo[locBC[[1]],\[CapitalPhi][locRoot]==0];
AppendTo[locBC[[2]],locRoot];

equations =locBC[[1]] ;


Do[
AppendTo[variables,\[CapitalPhi][locVertices[[x]]]];
If[MemberQ[locBC[[2]],locVertices[[x]]],Continue[]];
AppendTo[equations, \[CapitalPhi][locVertices[[x]]]==FullSimplify[Sum[locWeights[[x,y]]/totWeights[[x]] \[CapitalPhi][locVertices[[y]]],{y,Length[locIndices]}]]]
,{x,locIndices}];

If[OptionValue["print"],
Print[" ##### Equations to be solved: \n",equations];
Print[" ##### Equations to be solved: \n",variables];
];

solution=Flatten@Solve[equations,{}(*variables*)];

solutionLabels=N[solution]/.\[CapitalPhi][a_]->Floor[a];

solution=solution/. Rule->List;

If[OptionValue["drawSolution"],
Print[Graph[graph,VertexLabels->solutionLabels]]
];


If[OptionValue["selectSolution"]!=0,(*True*)
solution=Select[solution,#[[1]]==\[CapitalPhi][OptionValue["selectSolution"]] &][[1,2]]
];

Return[solution]
]


(* ::Subsection::Closed:: *)
(*Usage example of LaplaceEqSolver[]*)


(* ::Text:: *)
(*"boundaryConditions" should be a 2d list with the boundary conditions given as a first list, e.g. {\[Phi][1] == 1, \[Phi][2] == 0}. The second list must contain the vertices in the boundary, e.g. {1, 2}*)


(* ::Input:: *)
(*edges = {{1,2},{2,3},{3,1}};*)
(*root=2;*)
(*source=3;*)
(*g=MyGraph[edges,root,source][[1]];*)
(**)
(*BC={{\[CapitalPhi][3]==1,\[CapitalPhi][2]==0},{3,2}};*)
(*path1={{2,1},{1,3}};*)
(*sol=LaplaceEqSolver[g]*)
(*Select[sol,#[[1]]==\[CapitalPhi][path1[[1,2]]] &][[1,2]];*)
(**)
(*sol=LaplaceEqSolver[g,"selectSolution"->1]*)


(* ::Item::Closed:: *)
(*Let's keep the value at the source general. The idea is that we want to use it to impose the normalization such that \[Phi] are already properly normalized for being transition probabilities. *)


(* ::Input:: *)
(*BC={{\[CapitalPhi][3]==Zeta[3],\[CapitalPhi][2]==0},{3,2}};*)
(*path1={{2,1},{1,3}};*)
(*sol=LaplaceEqSolver[g,BC,\[CapitalPhi]]*)


(* ::Text:: *)
(*As you can see from the previous example, the source's value appear at numerator. So, we need to instead take its inverse. THE CRUCIAL THING IS THAT IT IS EXTREMELY EASY TO TAKE THE INVERSE OF A NUMBER, COMPARED TO THE INVERSE OF AN EXPECTATION VALUE!!!!!*)


(* ::Subsection::Closed:: *)
(*With graph*)


(* ::Input:: *)
(*n=3;*)
(*n=2*n-1;*)
(*(**)
(*edges = Flatten[Table[Table[{{{x,y},{x,y+1}},{{x,y},{x+1,y}}},{x,-Floor[n/2],Floor[n/2]}],{y,-Floor[n/2],Floor[n/2]}],2];*)
(**)*)
(* Flatten[*)
(*Table[*)
(*Table[*)
(*With[{i=x*n+y+1},*)
(*{If[Mod[i-1,n]==n-1,{},{i,i+1}],{i,i+n}}]*)
(*,{x,0,n-1}]*)
(*,{y,0,n-1}]*)
(*,2];*)
(*DeleteCases[%,{}];*)
(**)
(*edges =Select[%,#[[1]]<n*n+1&&#[[2]]<n*n+1&];*)
(*(**)
(*properEdges=Flatten[edges/.{{a__},{b__}}->{{a}<->{b}},1];*)*)
(*properEdges=Sort[Flatten[edges/.{a_,b_}->{a<->b},1]];*)
(*vertices=Sort[DeleteDuplicates[Flatten[edges,1]]];*)
(*(*sourceEdges=Select[vertices,Abs[#[[1]]]==Floor[n/2]||Abs[#[[2]]]==Floor[n/2]&];*)*)
(*(*sourceEdges=Select[vertices,*)
(*Abs[#]<=n*)
(*||n*(n-1)<=Abs[#]<=n*n*)
(*||Mod[Abs[#],n]==0*)
(*||Mod[Abs[#],n]==n-1&];*)
(**)*)
(*sourceEdges=Sort[Select[vertices,*)
(*With[{x=Floor[(#-1)/n]-Floor[n/2],y=Mod[(#-1),n] -(Floor[n/2])},*)
(*(Sqrt[x^2+y^2 ]>=(Floor[n/2]))]&]];*)
(*(*myLabels=Flatten[{Map[#->"SOURCE"&,sourceEdges],{0,0}->"ROOT","Name"},1];*)
(**)*)
(*myLabels=Flatten[{Map[#->"SOURCE"&,sourceEdges],(Floor[n/2]*(n))+Floor[n/2]+1->"ROOT","Name"},1];*)
(*Graph[vertices,properEdges,VertexLabels->"Name"];*)
(*g=Graph[vertices,properEdges,VertexLabels->myLabels]*)


(* ::Input:: *)
(*n*)


(* ::Input:: *)
(*LaplaceEqSolver[g,"drawSolution"->True]//N*)


(* ::Input:: *)
(*LaplaceEqSolver[g,"BC"->{{\[CapitalPhi][112]==0,\[CapitalPhi][98]==0,\[CapitalPhi][99]==0},{112,98,99}},"drawSolution"->True]//N*)


(* ::Section:: *)
(*bLaplacianRW[]*)


(* ::Item::Closed:: *)
(*Computes the probability of a given bLaplacian RW OLD*)


(* ::Input:: *)
(*ClearAll[bLaplacianRW];*)
(*Options[bLaplacianRW]={"draw"->False,"print"->TTrue};*)
(**)
(**)
(*bLaplacianRW[graph_,path__,OptionsPattern[],b_:1]:=Module[{locSource,locRoot,locWeights,locVertices,denominator,tempProb,prob=1},*)
(**)
(*locWeights=WeightedAdjacencyMatrix[graph]/.\[Beta][__]->1//Normal;*)
(*locVertices=VertexList@graph;*)
(**)
(*(*Print[locVertices];*)*)
(**)
(*locRoot = *)
(* Select[(List@@@PropertyValue[graph,VertexLabels])[[2;;]],StringMatchQ[#[[2]],___~~"ROOT"]&][[All,1]][[1]];*)
(*locSource = *)
(* Select[(List@@@PropertyValue[graph,VertexLabels])[[2;;]],StringMatchQ[#[[2]],___~~"SOURCE"]&][[All,1]];*)
(**)
(*If[locRoot =!= path[[1,1]], Print[Style["####  WRONG ROOT  ####",RGBColor[1, 0, 0]]]; Return[NULL]];*)
(*If[!MemberQ[locSource,path[[-1,2]]], Print[Style["####  WRONG SOURCE  ####",RGBColor[1, 0, 0]]];Return[NULL]];*)
(**)
(*Module[{locBC={{},{}},i,\[Phi]sol},*)
(*If[Head[locSource]==List,*)
(*For[i=1,i<=Length[locSource],i++,*)
(*AppendTo[locBC[[1]],\[CapitalPhi][locSource[[i]]]== 1];*)
(*AppendTo[locBC[[2]],locSource[[i]]];*)
(*]*)
(*,(*FALSE*)*)
(*AppendTo[locBC[[1]],\[CapitalPhi][locSource]== 1];*)
(*AppendTo[locBC[[2]],locSource];*)
(*];*)
(*For[i=1,i<=Length[path],i++,*)
(*AppendTo[locBC[[1]],\[CapitalPhi][path[[i,1]]]==0];*)
(*AppendTo[locBC[[2]],path[[i,1]]];*)
(**)
(*(*Print["locBC=",locBC];*)*)
(**)
(**)
(**)
(*\[Phi]sol=LaplaceEqSolver[graph,"BC"->locBC(*,\[Phi]*)];*)
(**)
(*(*Print[\[Phi]sol];*)*)
(*(*Return["good up to LaplaceEqSolver"];*)*)
(**)
(*denominator=Sum[locWeights[[path[[i,1]],y]]*(Select[\[Phi]sol,#[[1]]==\[CapitalPhi][y] &][[1,2]])^b,{y,Length[locVertices]}];*)
(*(*Print[denominator];*)*)
(**)
(*tempProb=(locWeights[[path[[i,1]],path[[i,2]]]]*(Select[\[Phi]sol,#[[1]]==\[CapitalPhi][path[[i,2]]] &][[1,2]])^b)/denominator//FullSimplify;*)
(**)
(*If[OptionValue["print"],*)
(*Print["#####  Transition probability "<>ToString[path[[i,1]]]<>" to "<>ToString[path[[i,2]]]<>"  #####"];*)
(*Print[tempProb];*)
(*];*)
(**)
(*prob*=tempProb;*)
(*Clear[\[Phi]sol];*)
(*]*)
(*];*)
(*(*If[OptionValue["draw"],Print[DrawOnGraph[graph,path]]];*)*)
(*Return[prob]*)
(*]*)
(**)
(*(**)
(*bLaplacianRW[graph_,path__,options:OptionsPattern[]]:=bLaplacianRW[graph,path,options,1];*)*)


(* ::Item:: *)
(*Computes the probability of a given bLaplacian RW. UPDATED TO SOLVE ON ANY GRAPH*)


(* ::Input::Initialization:: *)
ClearAll[bLaplacianRW];
Options[bLaplacianRW]={"draw"->False,"print"->tTrue};


bLaplacianRW[graph_,path__,OptionsPattern[],b_:1,n_:n]:=Module[{locSource,locRoot,locWeights,locVertices,
locIndices,denominator,tempProb,prob=1},

locWeights=WeightedAdjacencyMatrix[graph]/.\[Beta][__]->1//Normal;
locVertices=VertexList@graph;

locIndices=Table[i,{i,Length[locVertices]}];

(*Print[locVertices];*)

locRoot = 
 Select[(List@@@PropertyValue[graph,VertexLabels])[[2;;]],StringMatchQ[#[[2]],___~~"ROOT"]&][[All,1]][[1]];
locSource = 
 Select[(List@@@PropertyValue[graph,VertexLabels])[[2;;]],StringMatchQ[#[[2]],___~~"SOURCE"]&][[All,1]];

If[locRoot =!= path[[1,1]], Print[Style["####  WRONG ROOT  ####",RGBColor[1, 0, 0]]]; Return[NULL]];
If[!MemberQ[locSource,path[[-1,2]]], Print[Style["####  WRONG SOURCE  ####",RGBColor[1, 0, 0]]];Return[NULL]];

Module[{locBC={{},{}},i,\[Phi]sol},
If[Head[locSource]==List,
For[i=1,i<=Length[locSource],i++,
AppendTo[locBC[[1]],\[CapitalPhi][locSource[[i]]]== 1];
AppendTo[locBC[[2]],locSource[[i]]];
]
,(*FALSE*)
AppendTo[locBC[[1]],\[CapitalPhi][locSource]== 1];
AppendTo[locBC[[2]],locSource];
];

For[i=1,i<=Length[path],i++,
AppendTo[locBC[[1]],\[CapitalPhi][path[[i,1]]]==0];
AppendTo[locBC[[2]],path[[i,1]]];

If[OptionValue["print"],
Print["locBC=",Sort/@locBC];
];


\[Phi]sol=LaplaceEqSolver[graph,"BC"->locBC(*,\[Phi]*)];

If[OptionValue["print"],
Print["#####  Laplace solution for transition "<>ToString[path[[i,1]]]<>" to "<>ToString[path[[i,2]]]<>"  #####"];
Print[\[Phi]sol];
];
(*Return[\[Phi]sol];*)
(*Return["good up to LaplaceEqSolver"];*)

denominator=Sum[locWeights[[Position[locVertices,path[[i,1]]][[1,1]],y]]*(Select[\[Phi]sol,#[[1]]==\[CapitalPhi][locVertices[[y]]] &][[1,2]])^b,{y,Length[locIndices]}];
(*Print[denominator];*)

tempProb=1/denominatorlocWeights[[Position[locVertices,path[[i,1]]][[1,1]],Position[locVertices,path[[i,2]]][[1,1]]]]*(Select[\[Phi]sol,#[[1]]==\[CapitalPhi][path[[i,2]]] &][[1,2]])^b//FullSimplify;

If[OptionValue["print"],
Print["#####  Transition probability "<>ToString[path[[i,1]]]<>" to "<>ToString[path[[i,2]]]<>"  #####"];
Print[tempProb];
];

prob*=tempProb;
Clear[\[Phi]sol];
]
];
(*If[OptionValue["draw"],Print[DrawOnGraph[graph,path]]];*)
Return[prob]
]

(*
bLaplacianRW[graph_,path__,options:OptionsPattern[]]:=bLaplacianRW[graph,path,options,1];*)


(* ::Item::Closed:: *)
(**)


(* ::Input:: *)
(*Clear[LaplacianRW2];*)
(*Options[LaplacianRW2]={"draw"->False,"print"->False};*)
(**)
(*LaplacianRW2[graph_,path__,OptionsPattern[]]:=Module[{locSource,locRoot,locWeights,totWeights,locVertices,denominator,prob=1, Z,numerator},*)
(*locWeights=WeightedAdjacencyMatrix[graph]//Normal;*)
(*totWeights =Total[locWeights,{2}];*)
(**)
(*locVertices=VertexList@graph;*)
(**)
(*locRoot = *)
(* Select[(List@@@PropertyValue[graph,VertexLabels])[[2;;]],StringMatchQ[#[[2]],___~~"ROOT"]&][[All,1]][[1]];*)
(*locSource = *)
(* Select[(List@@@PropertyValue[graph,VertexLabels])[[2;;]],StringMatchQ[#[[2]],___~~"SOURCE"]&][[All,1]][[1]];*)
(**)
(*If[locRoot =!= path[[1,1]], Print["####  WRONG ROOT  ####"]; Return[NULL]];*)
(*If[locSource =!= path[[-1,2]], Print["####  WRONG SOURCE  ####"];Return[NULL]];*)
(**)
(*Module[{locBC={{},{}},\[Phi],i=1,j,\[Phi]sol},*)
(*AppendTo[locBC[[1]],\[Phi][locSource]==1];*)
(*AppendTo[locBC[[2]],locSource];*)
(**)
(*AppendTo[locBC[[1]],\[Phi][path[[i,1]]]==0];*)
(*AppendTo[locBC[[2]],path[[i,1]]];*)
(**)
(*\[Phi]sol=LaplaceEqSolver[graph,locBC,\[Phi]];*)
(*(*Print[\[Phi]sol]*);*)
(**)
(*denominator=Sum[locWeights[[path[[i,1]],y]]*Select[\[Phi]sol,#[[1]]==\[Phi][y] &][[1,2]],{y,Length[locVertices]}];*)
(*(*Print[denominator]*);*)
(**)
(*numerator=locWeights[[path[[i,1]],path[[i,2]]]] * Select[\[Phi]sol,#[[1]]==\[Phi][path[[i,2]]] &][[1,2]]//FullSimplify;*)
(**)
(*prob*=numerator/denominator//FullSimplify;*)
(**)
(*Print["#####  Transition probability "<>ToString[path[[i,1]]]<>" to "<>ToString[path[[i,2]]]<>"  #####"];*)
(*Print[prob/.\[Beta][_,_]->1//FullSimplify];*)
(**)
(*Clear[\[Phi]sol,locBC];*)
(**)
(*For[i=2,i<=Length[path],i++,*)
(*(*We need to solve it once with standard BCs at the previous step, i.e. \[Phi][locSource]==1*)*)
(*locBC={{\[Phi][locSource]==1},{locSource}};*)
(**)
(*For[j=1,j<=i-1,j++,*)
(*AppendTo[locBC[[1]],\[Phi][path[[j,1]]]==0];*)
(*AppendTo[locBC[[2]],path[[j,1]]]*)
(*]*)
(**)
(*(*Print[locBC]*);*)
(**)
(*Z=VertexAssignment[graph, "excludedVertices"->locBC[[2]]];*)
(*Z=ExpectationValue[Z];*)
(**)
(*If[OptionValue["print"],*)
(*Print["#####  Previous Partition Function  #####"];*)
(*Print[Z]*)
(*];*)
(**)
(*\[Phi]sol=LaplaceEqSolver[graph,locBC,\[Phi]];*)
(*(*Print[\[Phi]sol];*)*)
(**)
(*denominator=1/Z*Select[\[Phi]sol,#[[1]]==\[Phi][path[[i,1]]] &][[1,2]]//FullSimplify;*)
(**)
(*Clear[\[Phi]sol, locBC];*)
(**)
(**)
(*(*Then we need to solve it again with updated BC at the current step*)*)
(*locBC={{\[Phi][locSource]==denominator^-1},{locSource}};*)
(**)
(*For[j=1,j<=i,j++,*)
(*AppendTo[locBC[[1]],\[Phi][path[[j,1]]]==0];*)
(*AppendTo[locBC[[2]],path[[j,1]]]*)
(*]*)
(**)
(*(*Print[locBC]*);*)
(**)
(*Z=VertexAssignment[graph, "excludedVertices"->locBC[[2]]];*)
(*Z=ExpectationValue[Z];*)
(**)
(*If[OptionValue["print"],*)
(*Print["#####  Current Partition Function  #####"];*)
(*Print[Z]*)
(*];*)
(**)
(*\[Phi]sol=LaplaceEqSolver[graph,locBC,\[Phi]];*)
(*(*Print[\[Phi]sol]*);*)
(**)
(*Print["#####  Transition probability "<>ToString[path[[i,1]]]<>" to "<>ToString[path[[i,2]]]<>"  #####"];*)
(*Print[1/Z*locWeights[[path[[i,1]],path[[i,2]]]]/totWeights[[path[[i,1]]]]*Select[\[Phi]sol,#[[1]]==\[Phi][path[[i,2]]] &][[1,2]]/.\[Beta][_,_]->1//FullSimplify];*)
(**)
(*prob*=1/Z*locWeights[[path[[i,1]],path[[i,2]]]]/totWeights[[path[[i,1]]]]*Select[\[Phi]sol,#[[1]]==\[Phi][path[[i,2]]] &][[1,2]]//FullSimplify;*)
(**)
(*Clear[\[Phi]sol,locBC];*)
(*]*)
(*];*)
(*If[OptionValue["draw"],Print[DrawOnGraph[graph,path]]];*)
(*Return[prob]*)
(*]*)


(* ::Subsection::Closed:: *)
(*Usage example of LaplacianRW[] &LERWtransitionProb[]*)


(* ::Text:: *)
(*"path" should be an ordered list of pairs of vertices, i.e. edges, that form the desired Laplacian RW.*)
(*If "draw" is true, then draw the path*)


(* ::Item:: *)
(*Simple cases first*)


(* ::Input:: *)
(*edges = {{1,2},{2,3},{3,1}};*)
(*root=2;*)
(*source=3;*)
(*g2=MyGraph[edges,root,source][[1]]/.\[Beta][__]->1*)
(**)
(**)
(*path1={{2,1},{1,3}};*)
(*LaplacianRW[g2,path1](*/.\[Beta][_,_]->1*)*)


(* ::Input:: *)
(*(\[Beta][1,3] \[Beta][2,1])/((\[Beta][1,2]+\[Beta][1,3]) ((\[Beta][1,3] \[Beta][2,1])/(\[Beta][1,2]+\[Beta][1,3])+\[Beta][2,3]))//FullSimplify*)


(* ::Input:: *)
(*%//FullSimplify*)


(* ::Input:: *)
(*(*Correct!!*)*)


(* ::Section::Closed:: *)
(*bLaplacianRWlogBC[]*)


(* ::Item:: *)
(*Computes the probability of a given bLaplacian RW*)


(* ::Input::Initialization:: *)
ClearAll[bLaplacianRWlogBC];
Options[bLaplacianRWlogBC]={"draw"->False,"print"->tTrue};


bLaplacianRWlogBC[graph_,path__,OptionsPattern[],b_:1,n_:n]:=Module[{locSource,locRoot,locWeights,locVertices,
locIndices,denominator,tempProb,prob=1},

locWeights=WeightedAdjacencyMatrix[graph]/.\[Beta][__]->1//Normal;
locVertices=VertexList@graph;

locIndices=Table[i,{i,Length[locVertices]}];

(*Print[locVertices];*)

locRoot = 
 Select[(List@@@PropertyValue[graph,VertexLabels])[[2;;]],StringMatchQ[#[[2]],___~~"ROOT"]&][[All,1]][[1]];
locSource = 
 Select[(List@@@PropertyValue[graph,VertexLabels])[[2;;]],StringMatchQ[#[[2]],___~~"SOURCE"]&][[All,1]];

If[locRoot =!= path[[1,1]], Print[Style["####  WRONG ROOT  ####",RGBColor[1, 0, 0]]]; Return[NULL]];
If[!MemberQ[locSource,path[[-1,2]]], Print[Style["####  WRONG SOURCE  ####",RGBColor[1, 0, 0]]];Return[NULL]];

Module[{locBC={{},{}},i,\[Phi]sol},
If[Head[locSource]==List,
For[i=1,i<=Length[locSource],i++,
AppendTo[locBC[[1]],\[CapitalPhi][locSource[[i]]]== With[{x=Floor[(locSource[[i]]-1)/n]-Floor[n/2],y=Mod[(locSource[[i]]-1),n] -(Floor[n/2])},
(Log[x^2+y^2 +1](*/Log[x^2+y^2 ]*)//N)]];
AppendTo[locBC[[2]],locSource[[i]]];
]
,(*FALSE*)
AppendTo[locBC[[1]],\[CapitalPhi][locSource]== 1];
AppendTo[locBC[[2]],locSource];
];

For[i=1,i<=Length[path],i++,
AppendTo[locBC[[1]],\[CapitalPhi][path[[i,1]]]==0];
AppendTo[locBC[[2]],path[[i,1]]];

If[OptionValue["print"],
Print["locBC=",Sort/@locBC];
];


\[Phi]sol=LaplaceEqSolver[graph,"BC"->locBC(*,\[Phi]*)];

If[OptionValue["print"],
Print["#####  Laplace solution for transition "<>ToString[path[[i,1]]]<>" to "<>ToString[path[[i,2]]]<>"  #####"];
Print[\[Phi]sol];
];
(*Return[\[Phi]sol];*)
(*Return["good up to LaplaceEqSolver"];*)

denominator=Sum[locWeights[[Position[locVertices,path[[i,1]]][[1,1]],y]]*(Select[\[Phi]sol,#[[1]]==\[CapitalPhi][locVertices[[y]]] &][[1,2]])^b,{y,Length[locIndices]}];
(*Print[denominator];*)

tempProb=1/denominatorlocWeights[[Position[locVertices,path[[i,1]]][[1,1]],Position[locVertices,path[[i,2]]][[1,1]]]]*(Select[\[Phi]sol,#[[1]]==\[CapitalPhi][path[[i,2]]] &][[1,2]])^b//FullSimplify;

If[OptionValue["print"],
Print["#####  Transition probability "<>ToString[path[[i,1]]]<>" to "<>ToString[path[[i,2]]]<>"  #####"];
Print[tempProb];
];

prob*=tempProb;
Clear[\[Phi]sol];
]
];
(*If[OptionValue["draw"],Print[DrawOnGraph[graph,path]]];*)
Return[prob]
]

(*
bLaplacianRW[graph_,path__,options:OptionsPattern[]]:=bLaplacianRW[graph,path,options,1];*)


(* ::Input:: *)
(*Position[{4,5,6},6][[1,1]]*)


(* ::Item::Closed:: *)
(**)


(* ::Input:: *)
(*Clear[LaplacianRW2];*)
(*Options[LaplacianRW2]={"draw"->False,"print"->False};*)
(**)
(*LaplacianRW2[graph_,path__,OptionsPattern[]]:=Module[{locSource,locRoot,locWeights,totWeights,locVertices,denominator,prob=1, Z,numerator},*)
(*locWeights=WeightedAdjacencyMatrix[graph]//Normal;*)
(*totWeights =Total[locWeights,{2}];*)
(**)
(*locVertices=VertexList@graph;*)
(**)
(*locRoot = *)
(* Select[(List@@@PropertyValue[graph,VertexLabels])[[2;;]],StringMatchQ[#[[2]],___~~"ROOT"]&][[All,1]][[1]];*)
(*locSource = *)
(* Select[(List@@@PropertyValue[graph,VertexLabels])[[2;;]],StringMatchQ[#[[2]],___~~"SOURCE"]&][[All,1]][[1]];*)
(**)
(*If[locRoot =!= path[[1,1]], Print["####  WRONG ROOT  ####"]; Return[NULL]];*)
(*If[locSource =!= path[[-1,2]], Print["####  WRONG SOURCE  ####"];Return[NULL]];*)
(**)
(*Module[{locBC={{},{}},\[Phi],i=1,j,\[Phi]sol},*)
(*AppendTo[locBC[[1]],\[Phi][locSource]==1];*)
(*AppendTo[locBC[[2]],locSource];*)
(**)
(*AppendTo[locBC[[1]],\[Phi][path[[i,1]]]==0];*)
(*AppendTo[locBC[[2]],path[[i,1]]];*)
(**)
(*\[Phi]sol=LaplaceEqSolver[graph,locBC,\[Phi]];*)
(*(*Print[\[Phi]sol]*);*)
(**)
(*denominator=Sum[locWeights[[path[[i,1]],y]]*Select[\[Phi]sol,#[[1]]==\[Phi][y] &][[1,2]],{y,Length[locVertices]}];*)
(*(*Print[denominator]*);*)
(**)
(*numerator=locWeights[[path[[i,1]],path[[i,2]]]] * Select[\[Phi]sol,#[[1]]==\[Phi][path[[i,2]]] &][[1,2]]//FullSimplify;*)
(**)
(*prob*=numerator/denominator//FullSimplify;*)
(**)
(*Print["#####  Transition probability "<>ToString[path[[i,1]]]<>" to "<>ToString[path[[i,2]]]<>"  #####"];*)
(*Print[prob/.\[Beta][_,_]->1//FullSimplify];*)
(**)
(*Clear[\[Phi]sol,locBC];*)
(**)
(*For[i=2,i<=Length[path],i++,*)
(*(*We need to solve it once with standard BCs at the previous step, i.e. \[Phi][locSource]==1*)*)
(*locBC={{\[Phi][locSource]==1},{locSource}};*)
(**)
(*For[j=1,j<=i-1,j++,*)
(*AppendTo[locBC[[1]],\[Phi][path[[j,1]]]==0];*)
(*AppendTo[locBC[[2]],path[[j,1]]]*)
(*]*)
(**)
(*(*Print[locBC]*);*)
(**)
(*Z=VertexAssignment[graph, "excludedVertices"->locBC[[2]]];*)
(*Z=ExpectationValue[Z];*)
(**)
(*If[OptionValue["print"],*)
(*Print["#####  Previous Partition Function  #####"];*)
(*Print[Z]*)
(*];*)
(**)
(*\[Phi]sol=LaplaceEqSolver[graph,locBC,\[Phi]];*)
(*(*Print[\[Phi]sol];*)*)
(**)
(*denominator=1/Z*Select[\[Phi]sol,#[[1]]==\[Phi][path[[i,1]]] &][[1,2]]//FullSimplify;*)
(**)
(*Clear[\[Phi]sol, locBC];*)
(**)
(**)
(*(*Then we need to solve it again with updated BC at the current step*)*)
(*locBC={{\[Phi][locSource]==denominator^-1},{locSource}};*)
(**)
(*For[j=1,j<=i,j++,*)
(*AppendTo[locBC[[1]],\[Phi][path[[j,1]]]==0];*)
(*AppendTo[locBC[[2]],path[[j,1]]]*)
(*]*)
(**)
(*(*Print[locBC]*);*)
(**)
(*Z=VertexAssignment[graph, "excludedVertices"->locBC[[2]]];*)
(*Z=ExpectationValue[Z];*)
(**)
(*If[OptionValue["print"],*)
(*Print["#####  Current Partition Function  #####"];*)
(*Print[Z]*)
(*];*)
(**)
(*\[Phi]sol=LaplaceEqSolver[graph,locBC,\[Phi]];*)
(*(*Print[\[Phi]sol]*);*)
(**)
(*Print["#####  Transition probability "<>ToString[path[[i,1]]]<>" to "<>ToString[path[[i,2]]]<>"  #####"];*)
(*Print[1/Z*locWeights[[path[[i,1]],path[[i,2]]]]/totWeights[[path[[i,1]]]]*Select[\[Phi]sol,#[[1]]==\[Phi][path[[i,2]]] &][[1,2]]/.\[Beta][_,_]->1//FullSimplify];*)
(**)
(*prob*=1/Z*locWeights[[path[[i,1]],path[[i,2]]]]/totWeights[[path[[i,1]]]]*Select[\[Phi]sol,#[[1]]==\[Phi][path[[i,2]]] &][[1,2]]//FullSimplify;*)
(**)
(*Clear[\[Phi]sol,locBC];*)
(*]*)
(*];*)
(*If[OptionValue["draw"],Print[DrawOnGraph[graph,path]]];*)
(*Return[prob]*)
(*]*)


(* ::Subsection::Closed:: *)
(*Usage example of LaplacianRW[] &LERWtransitionProb[]*)


(* ::Text:: *)
(*"path" should be an ordered list of pairs of vertices, i.e. edges, that form the desired Laplacian RW.*)
(*If "draw" is true, then draw the path*)


(* ::Item:: *)
(*Simple cases first*)


(* ::Input:: *)
(*edges = {{1,2},{2,3},{3,1}};*)
(*root=2;*)
(*source=3;*)
(*g2=MyGraph[edges,root,source][[1]]/.\[Beta][__]->1*)
(**)
(**)
(*path1={{2,1},{1,3}};*)
(*LaplacianRW[g2,path1](*/.\[Beta][_,_]->1*)*)


(* ::Input:: *)
(*(\[Beta][1,3] \[Beta][2,1])/((\[Beta][1,2]+\[Beta][1,3]) ((\[Beta][1,3] \[Beta][2,1])/(\[Beta][1,2]+\[Beta][1,3])+\[Beta][2,3]))//FullSimplify*)


(* ::Input:: *)
(*%//FullSimplify*)


(* ::Input:: *)
(*(*Correct!!*)*)


(* ::Section:: *)
(*Application to Square Lattice*)


(* ::Subsection:: *)
(*n=3*)


(* ::Input:: *)
(*Graph[vertices,properEdges,VertexLabels->"Name"]*)


(* ::Subsubsection::Closed:: *)
(*Graph def*)


(* ::Input:: *)
(*n=3;*)
(*n=2*n-1;*)
(*(**)
(*edges = Flatten[Table[Table[{{{x,y},{x,y+1}},{{x,y},{x+1,y}}},{x,-Floor[n/2],Floor[n/2]}],{y,-Floor[n/2],Floor[n/2]}],2];*)
(**)*)
(* Flatten[*)
(*Table[*)
(*Table[*)
(*With[{i=x*n+y+1},*)
(*{If[Mod[i-1,n]==n-1,{},{i,i+1}],{i,i+n}}]*)
(*,{x,0,n-1}]*)
(*,{y,0,n-1}]*)
(*,2];*)
(*DeleteCases[%,{}];*)
(**)
(*edges =Select[%,#[[1]]<n*n+1&&#[[2]]<n*n+1&];*)
(*(**)
(*properEdges=Flatten[edges/.{{a__},{b__}}->{{a}<->{b}},1];*)*)
(*properEdges=Sort[Flatten[edges/.{a_,b_}->{a<->b},1]];*)
(*vertices=Sort[DeleteDuplicates[Flatten[edges,1]]];*)
(*(*sourceEdges=Select[vertices,Abs[#[[1]]]==Floor[n/2]||Abs[#[[2]]]==Floor[n/2]&];*)*)
(*(*sourceEdges=Select[vertices,*)
(*Abs[#]<=n*)
(*||n*(n-1)<=Abs[#]<=n*n*)
(*||Mod[Abs[#],n]==0*)
(*||Mod[Abs[#],n]==n-1&];*)
(**)*)
(*sourceEdges=Sort[Select[vertices,*)
(*With[{x=Floor[(#-1)/n]-Floor[n/2],y=Mod[(#-1),n] -(Floor[n/2])},*)
(*(Sqrt[x^2+y^2 ]>=(Floor[n/2]))]&]];*)
(*(*myLabels=Flatten[{Map[#->"SOURCE"&,sourceEdges],{0,0}->"ROOT","Name"},1];*)
(**)*)
(*myLabels=Flatten[{Map[#->"SOURCE"&,sourceEdges],(Floor[n/2]*(n))+Floor[n/2]+1->"ROOT","Name"},1];*)
(*Graph[vertices,properEdges,VertexLabels->"Name"];*)
(*g=Graph[vertices,properEdges,VertexLabels->myLabels]*)


(* ::Subsubsection:: *)
(*Linear path: {{Root,12,Source}}*)


(* ::Input:: *)
(*path={{13,14},{14,15}};*)
(*LaplacianRW[g,path]*)
(*Row[{"Single path",path,": ",N[%]*100,"%"}]*)
(**)
(*Row[{"Symmetric paths: ",N[%%]*100*4,"%"}]*)


(* ::Subsubsection::Closed:: *)
(*ZigZag path: *)


(* ::Input:: *)
(*path={{13,14},{14,19},{19,20}};*)
(*LaplacianRW[g,path]*)
(*Row[{"Single path",path,": ",N[%]*100,"%"}]*)
(**)
(*Row[{"Symmetric paths: ",N[%%]*100*4*2,"%"}]*)


(* ::Subsubsection::Closed:: *)
(*BigL path: *)


(* ::Input:: *)
(*path={{13,14},{14,19},{19,24}};*)
(*LaplacianRW[g,path]*)
(*Row[{"Single path",path,": ",N[%]*100,"%"}]*)
(**)
(*Row[{"Symmetric paths: ",N[%%]*100*4*2,"%"}]*)


(* ::Subsubsection::Closed:: *)
(*To be used with LogBC*)


(* ::Input:: *)
(*n=3;*)
(*n=2*n-1;*)
(*(**)
(*edges = Flatten[Table[Table[{{{x,y},{x,y+1}},{{x,y},{x+1,y}}},{x,-Floor[n/2],Floor[n/2]}],{y,-Floor[n/2],Floor[n/2]}],2];*)
(**)*)
(* Flatten[*)
(*Table[*)
(*Table[*)
(*With[{i=x*n+y+1},*)
(*{If[Mod[i-1,n]==n-1,{},{i,i+1}],{i,i+n}}]*)
(*,{x,0,n-1}]*)
(*,{y,0,n-1}]*)
(*,2];*)
(*DeleteCases[%,{}];*)
(**)
(*edges =Sort@Select[%,#[[1]]<n*n+1&&#[[2]]<n*n+1&];*)
(*(**)
(*properEdges=Flatten[edges/.{{a__},{b__}}->{{a}<->{b}},1];*)*)
(*properEdges=Sort[Flatten[edges/.{a_,b_}:>{a<->b},1]];*)
(*vertices=Sort[DeleteDuplicates[Flatten[edges,1]]];*)
(*(*sourceEdges=Select[vertices,Abs[#[[1]]]==Floor[n/2]||Abs[#[[2]]]==Floor[n/2]&];*)*)
(*(*sourceEdges=Select[vertices,*)
(*Abs[#]<=n*)
(*||n*(n-1)<=Abs[#]<=n*n*)
(*||Mod[Abs[#],n]==0*)
(*||Mod[Abs[#],n]==n-1&];*)
(**)*)
(**)
(*threshold=(Floor[n/2]+0.3);*)
(*sourceThreshold=(Floor[n/2]-0.5);*)
(**)
(*excludeVertices=Sort@Select[vertices,With[{x=Floor[(#-1)/n]-Floor[n/2],y=Mod[(#-1),n] -(Floor[n/2])},*)
(*(Sqrt[x^2+y^2 ]>threshold)]&];*)
(**)
(*vertices=Sort@Select[vertices,With[{x=Floor[(#-1)/n]-Floor[n/2],y=Mod[(#-1),n] -(Floor[n/2])},*)
(*(Sqrt[x^2+y^2 ]<=threshold)]&];*)
(**)
(*properEdges=Sort@Select[properEdges,!MemberQ[excludeVertices,#[[1]]]&&!MemberQ[excludeVertices,#[[2]]]&];*)
(*sourceEdges=Sort[Select[vertices,*)
(*With[{x=Floor[(#-1)/n]-Floor[n/2],y=Mod[(#-1),n] -(Floor[n/2])},*)
(*(sourceThreshold<=Sqrt[x^2+y^2 ]<=threshold)]&]];*)
(*(*myLabels=Flatten[{Map[#->"SOURCE"&,sourceEdges],{0,0}->"ROOT","Name"},1];*)
(**)*)
(*myLabels=Flatten[{Map[#->"SOURCE"&,sourceEdges],(Floor[n/2]*(n))+Floor[n/2]+1->"ROOT","Name"},1];*)
(*Graph[vertices,properEdges,VertexLabels->"Name"]*)
(*g=Graph[vertices,properEdges,VertexLabels->myLabels,ImageSize->Large]*)


(* ::Subsubsection:: *)
(*Linear path (length=2) with bLaplacianRWlogBC[]*)


(* ::Input:: *)
(*{{13,14,15}};*)
(*%//.{d___,{a_,b_,c___}}:>{d,{a,b},{b,c}};*)
(*path=Most[%](*{{113,114},{114,115},{115,116},{116,117},{117,118},{118,119},{119,120}};*)*)
(*bLaplacianRWlogBC[g,path]*)
(*Row[{"Single path",path,": ",N[%]*100,"%"}]*)
(**)
(*Row[{"Symmetric paths: ",N[%%]*100*4,"%"}]*)


(* ::Text:: *)
(*Compared to Symmetric paths: 43.3036% from the BC=1 everywhere!!!!!*)


(* ::Subsection::Closed:: *)
(*n=4*)


(* ::Input:: *)
(*Graph[vertices,properEdges,VertexLabels->"Name"]*)


(* ::Subsubsection::Closed:: *)
(*Graph def*)


(* ::Input:: *)
(*n=4;*)
(*n=2*n-1;*)
(*(**)
(*edges = Flatten[Table[Table[{{{x,y},{x,y+1}},{{x,y},{x+1,y}}},{x,-Floor[n/2],Floor[n/2]}],{y,-Floor[n/2],Floor[n/2]}],2];*)
(**)*)
(* Flatten[*)
(*Table[*)
(*Table[*)
(*With[{i=x*n+y+1},*)
(*{If[Mod[i-1,n]==n-1,{},{i,i+1}],{i,i+n}}]*)
(*,{x,0,n-1}]*)
(*,{y,0,n-1}]*)
(*,2];*)
(*DeleteCases[%,{}];*)
(**)
(*edges =Select[%,#[[1]]<n*n+1&&#[[2]]<n*n+1&];*)
(*(**)
(*properEdges=Flatten[edges/.{{a__},{b__}}->{{a}<->{b}},1];*)*)
(*properEdges=Sort[Flatten[edges/.{a_,b_}:>{a<->b},1]];*)
(*vertices=Sort[DeleteDuplicates[Flatten[edges,1]]];*)
(*(*sourceEdges=Select[vertices,Abs[#[[1]]]==Floor[n/2]||Abs[#[[2]]]==Floor[n/2]&];*)*)
(*(*sourceEdges=Select[vertices,*)
(*Abs[#]<=n*)
(*||n*(n-1)<=Abs[#]<=n*n*)
(*||Mod[Abs[#],n]==0*)
(*||Mod[Abs[#],n]==n-1&];*)
(**)*)
(*sourceEdges=Sort[Select[vertices,*)
(*With[{x=Floor[(#-1)/n]-Floor[n/2],y=Mod[(#-1),n] -(Floor[n/2])},*)
(*(Sqrt[x^2+y^2 ]>=(Floor[n/2]-0.))]&]];*)
(*(*myLabels=Flatten[{Map[#->"SOURCE"&,sourceEdges],{0,0}->"ROOT","Name"},1];*)
(**)*)
(*myLabels=Flatten[{Map[#->"SOURCE"&,sourceEdges],(Floor[n/2]*(n))+Floor[n/2]+1->"ROOT","Name"},1];*)
(*Graph[vertices,properEdges,VertexLabels->"Name"];*)
(*g=Graph[vertices,properEdges,VertexLabels->myLabels]*)


(* ::Subsubsection::Closed:: *)
(*b=1*)


(* ::Item::Closed:: *)
(*Linear path: {{25,26,27,Source}}*)


(* ::Input:: *)
(*path={{25,26},{26,27},{27,28}};*)
(*(bLaplacianRW[g,path]//N)*100*)
(*Print[Row[{"Accounting for symmetry: ",%*4,"%"}]]*)


(* ::Item::Closed:: *)
(*LittleL path: {{25,26,27,20,Source}} (IF (Floor[n/2] - 0.6) )*)


(* ::Input:: *)
(*{{25,26,27,20,21}};*)
(*%//.{d___,{a_,b_,c___}}:>{d,{a,b},{b,c}};*)
(*path=Most[%]*)
(*(bLaplacianRW[g,path]//N)*100*)
(*Print[Row[{"Accounting for symmetry: ",l4p1=%*4*2,"%"}]]*)


(* ::Input:: *)
(*{{25,26,27,20,13}};*)
(*%//.{d___,{a_,b_,c___}}:>{d,{a,b},{b,c}};*)
(*path=Most[%]*)
(*(bLaplacianRW[g,path]//N)*100*)
(*Print[Row[{"Accounting for symmetry: ",l4p2=%*4*2,"%"}]]*)


(* ::Input:: *)
(*{{25,26,19,20,21}};*)
(*%//.{d___,{a_,b_,c___}}:>{d,{a,b},{b,c}};*)
(*path=Most[%]*)
(*(bLaplacianRW[g,path]//N)*100*)
(*Print[Row[{"Accounting for symmetry: ",l4p3=%*4*2,"%"}]]*)


(* ::Input:: *)
(*{{25,26,19,20,13}};*)
(*%//.{d___,{a_,b_,c___}}:>{d,{a,b},{b,c}};*)
(*path=Most[%]*)
(*(bLaplacianRW[g,path]//N)*100*)
(*Print[Row[{"Accounting for symmetry: ",l4p4=%*4*2,"%"}]]*)


(* ::Input:: *)
(*{{25,26,19,12,13}};*)
(*%//.{d___,{a_,b_,c___}}:>{d,{a,b},{b,c}};*)
(*path=Most[%]*)
(*(bLaplacianRW[g,path]//N)*100*)
(*Print[Row[{"Accounting for symmetry: ",l4p5=%*4*2,"%"}]]*)


(* ::Input:: *)
(*{{25,26,19,12,5}};*)
(*%//.{d___,{a_,b_,c___}}:>{d,{a,b},{b,c}};*)
(*path=Most[%]*)
(*(bLaplacianRW[g,path]//N)*100*)
(*Print[Row[{"Accounting for symmetry: ",l4p6=%*4*2,"%"}]]*)


(* ::Input:: *)
(*l4p1+l4p2+l4p3+l4p4+l4p5+l4p6*)


(* ::Item:: *)
(*LittleL path: {{25,26,27,20,Source}} (IF (Floor[n/2] - 0.) )*)


(* ::Input:: *)
(*{{25,26,27,20,21}};*)
(*%//.{d___,{a_,b_,c___}}:>{d,{a,b},{b,c}};*)
(*path=Most[%]*)
(*(bLaplacianRW[g,path]//N)*100*)
(*Print[Row[{"Accounting for symmetry: ",l4p1=%*4*2,"%"}]]*)


(* ::Input:: *)
(*{{25,26,19,20,21}};*)
(*%//.{d___,{a_,b_,c___}}:>{d,{a,b},{b,c}};*)
(*path=Most[%]*)
(*(bLaplacianRW[g,path]//N)*100*)
(*Print[Row[{"Accounting for symmetry: ",l4p2=%*4*2,"%"}]]*)


(* ::Input:: *)
(*{{25,26,19,12,5}};*)
(*%//.{d___,{a_,b_,c___}}:>{d,{a,b},{b,c}};*)
(*path=Most[%]*)
(*(bLaplacianRW[g,path]//N)*100*)
(*Print[Row[{"Accounting for symmetry: ",l4p3=%*4*2,"%"}]]*)


(* ::Input:: *)
(*l4p1+l4p2+l4p3*)


(* ::Subsubsection::Closed:: *)
(*b=4*)


(* ::Item:: *)
(*Linear path: {{Root,12,Source}}*)


(* ::Input:: *)
(*path={{25,26},{26,27},{27,28}};*)
(*bLaplacianRW[g,path,4]//N;*)
(**)
(*Row[{"Single path",path,": ",N[%]*100,"%"}]*)
(**)
(*Row[{"Symmetric paths: ",N[%%]*100*4,"%"}]*)


(* ::Item:: *)
(*BigL path*)


(* ::Input:: *)
(*path={{25,26},{26,19},{19,12},{12,5}};*)
(*bLaplacianRW[g,path,4]//N;*)
(**)
(*Row[{"Single path",path,": ",N[%]*100,"%"}]*)
(**)
(*Row[{"Symmetric paths: ",N[%%]*100*4*2,"%"}]*)


(* ::Subsubsection::Closed:: *)
(*b=15*)


(* ::Item:: *)
(*Linear path: {{Root,12,Source}}*)


(* ::Input:: *)
(*path={{25,26},{26,27},{27,28}};*)
(*bLaplacianRW[g,path,15]//N;*)
(**)
(*Row[{"Single path",path,": ",N[%]*100,"%"}]*)
(**)
(*Row[{"Symmetric paths: ",N[%%]*100*4,"%"}]*)


(* ::Item:: *)
(*BigL path*)


(* ::Input:: *)
(*path={{25,26},{26,19},{19,12},{12,5}};*)
(*bLaplacianRW[g,path,15]//N;*)
(**)
(*Row[{"Single path",path,": ",N[%]*100,"%"}]*)
(**)
(*Row[{"Symmetric paths: ",N[%%]*100*4*2,"%"}]*)


(* ::Subsection::Closed:: *)
(*n=7*)


(* ::Input:: *)
(*Graph[vertices,properEdges,VertexLabels->"Name"]*)


(* ::Subsubsection::Closed:: *)
(*Graph def*)


(* ::Input:: *)
(*n=7;*)
(*n=2*n-1;*)
(*(**)
(*edges = Flatten[Table[Table[{{{x,y},{x,y+1}},{{x,y},{x+1,y}}},{x,-Floor[n/2],Floor[n/2]}],{y,-Floor[n/2],Floor[n/2]}],2];*)
(**)*)
(* Flatten[*)
(*Table[*)
(*Table[*)
(*With[{i=x*n+y+1},*)
(*{If[Mod[i-1,n]==n-1,{},{i,i+1}],{i,i+n}}]*)
(*,{x,0,n-1}]*)
(*,{y,0,n-1}]*)
(*,2];*)
(*DeleteCases[%,{}];*)
(**)
(*edges =Select[%,#[[1]]<n*n+1&&#[[2]]<n*n+1&];*)
(*(**)
(*properEdges=Flatten[edges/.{{a__},{b__}}->{{a}<->{b}},1];*)*)
(*properEdges=Sort[Flatten[edges/.{a_,b_}:>{a<->b},1]];*)
(*vertices=Sort[DeleteDuplicates[Flatten[edges,1]]];*)
(*(*sourceEdges=Select[vertices,Abs[#[[1]]]==Floor[n/2]||Abs[#[[2]]]==Floor[n/2]&];*)*)
(*(*sourceEdges=Select[vertices,*)
(*Abs[#]<=n*)
(*||n*(n-1)<=Abs[#]<=n*n*)
(*||Mod[Abs[#],n]==0*)
(*||Mod[Abs[#],n]==n-1&];*)
(**)*)
(*sourceEdges=Sort[Select[vertices,*)
(*With[{x=Floor[(#-1)/n]-Floor[n/2],y=Mod[(#-1),n] -(Floor[n/2])},*)
(*(Sqrt[x^2+y^2 ]>=(Floor[n/2]-0.7))]&]];*)
(*(*myLabels=Flatten[{Map[#->"SOURCE"&,sourceEdges],{0,0}->"ROOT","Name"},1];*)
(**)*)
(*myLabels=Flatten[{Map[#->"SOURCE"&,sourceEdges],(Floor[n/2]*(n))+Floor[n/2]+1->"ROOT","Name"},1];*)
(*Graph[vertices,properEdges,VertexLabels->"Name"];*)
(*g=Graph[vertices,properEdges,VertexLabels->myLabels,ImageSize->Large]*)


(* ::Subsubsection::Closed:: *)
(*Linear path (length=6)*)


(* ::Input:: *)
(*{{85,86,87,88,89,90,91}};*)
(*%//.{d___,{a_,b_,c___}}:>{d,{a,b},{b,c}};*)
(*path=Most[%]*)
(*bLaplacianRW[g,path]*)
(*Row[{"Single path",path,": ",N[%]*100,"%"}]*)
(**)
(*Row[{"Symmetric paths: ",N[%%]*100*4,"%"}]*)


(* ::Subsubsection:: *)
(*2nd-to-Linear paths (length=7)*)


(* ::Text:: *)
(*The symmetry factor comes from: *)
(*1) restriction to one quadrant: 4x2, i.e. choice to go Down than Right*)
(*2) an extra x2 from the choice between going straight after the first Right turn, or to turn Left and go down again *)


(* ::Input:: *)
(*{{85,72,73,74,75,76,77,78}};*)
(*%//.{d___,{a_,b_,c___}}:>{d,{a,b},{b,c}};*)
(*path=Most[%]*)
(*bLaplacianRW[g,path]*)
(*Row[{"Single path",path,": ",N[%]*100,"%"}]*)
(**)
(*Row[{"Symmetric paths: ",l7p1=N[%%]*100*4*2*2,"%"}]*)


(* ::Input:: *)
(*{{85,86,73,74,75,76,77,78}};*)
(*%//.{d___,{a_,b_,c___}}:>{d,{a,b},{b,c}};*)
(*path=Most[%]*)
(*bLaplacianRW[g,path]*)
(*Row[{"Single path",path,": ",N[%]*100,"%"}]*)
(**)
(*Row[{"Symmetric paths: ",l7p2=N[%%]*100*4*2*2,"%"}]*)


(* ::Input:: *)
(*{{85,86,87,74,75,76,77,78}};*)
(*%//.{d___,{a_,b_,c___}}:>{d,{a,b},{b,c}};*)
(*path=Most[%]*)
(*bLaplacianRW[g,path]*)
(*Row[{"Single path",path,": ",N[%]*100,"%"}]*)
(**)
(*Row[{"Symmetric paths: ",l7p3=N[%%]*100*4*2*2,"%"}]*)


(* ::Input:: *)
(*{{85,86,87,88,75,76,77,78}};*)
(*%//.{d___,{a_,b_,c___}}:>{d,{a,b},{b,c}};*)
(*path=Most[%]*)
(*bLaplacianRW[g,path]*)
(*Row[{"Single path",path,": ",N[%]*100,"%"}]*)
(**)
(*Row[{"Symmetric paths: ",l7p4=N[%%]*100*4*2*2,"%"}]*)


(* ::Input:: *)
(*{{85,86,87,88,89,76,77,78}};*)
(*%//.{d___,{a_,b_,c___}}:>{d,{a,b},{b,c}};*)
(*path=Most[%]*)
(*bLaplacianRW[g,path]*)
(*Row[{"Single path",path,": ",N[%]*100,"%"}]*)
(**)
(*Row[{"Symmetric paths: ",l7p5=N[%%]*100*4*2*2,"%"}]*)


(* ::Input:: *)
(*{{85,86,87,88,89,90,77,78}};*)
(*%//.{d___,{a_,b_,c___}}:>{d,{a,b},{b,c}};*)
(*path=Most[%]*)
(*bLaplacianRW[g,path]*)
(*Row[{"Single path",path,": ",N[%]*100,"%"}]*)
(**)
(*Row[{"Symmetric paths: ",l7p6=N[%%]*100*4*2*2,"%"}]*)


(* ::Input:: *)
(*l7p1+l7p2+l7p3+l7p4+l7p5+l7p6*)


(* ::Subsection:: *)
(*n=8*)


(* ::Input:: *)
(*Graph[vertices,properEdges,VertexLabels->"Name",ImageSize->Large]*)


(* ::Subsubsection:: *)
(*Graph def*)


(* ::Input:: *)
(*n=8;*)
(*n=2*n-1;*)
(*(**)
(*edges = Flatten[Table[Table[{{{x,y},{x,y+1}},{{x,y},{x+1,y}}},{x,-Floor[n/2],Floor[n/2]}],{y,-Floor[n/2],Floor[n/2]}],2];*)
(**)*)
(* Flatten[*)
(*Table[*)
(*Table[*)
(*With[{i=x*n+y+1},*)
(*{If[Mod[i-1,n]==n-1,{},{i,i+1}],{i,i+n}}]*)
(*,{x,0,n-1}]*)
(*,{y,0,n-1}]*)
(*,2];*)
(*DeleteCases[%,{}];*)
(**)
(*edges =Sort@Select[%,#[[1]]<n*n+1&&#[[2]]<n*n+1&];*)
(*(**)
(*properEdges=Flatten[edges/.{{a__},{b__}}->{{a}<->{b}},1];*)*)
(*properEdges=Sort[Flatten[edges/.{a_,b_}:>{a<->b},1]];*)
(*vertices=Sort[DeleteDuplicates[Flatten[edges,1]]];*)
(*(*sourceEdges=Select[vertices,Abs[#[[1]]]==Floor[n/2]||Abs[#[[2]]]==Floor[n/2]&];*)*)
(*(*sourceEdges=Select[vertices,*)
(*Abs[#]<=n*)
(*||n*(n-1)<=Abs[#]<=n*n*)
(*||Mod[Abs[#],n]==0*)
(*||Mod[Abs[#],n]==n-1&];*)
(**)*)
(**)
(*threshold=(Floor[n/2]+0);*)
(*sourceThreshold=(Floor[n/2]-0.95);*)
(**)
(*excludeVertices=Sort@Select[vertices,With[{x=Floor[(#-1)/n]-Floor[n/2],y=Mod[(#-1),n] -(Floor[n/2])},*)
(*(Sqrt[x^2+y^2 ]>threshold)]&];*)
(**)
(*vertices=Sort@Select[vertices,With[{x=Floor[(#-1)/n]-Floor[n/2],y=Mod[(#-1),n] -(Floor[n/2])},*)
(*(Sqrt[x^2+y^2 ]<=threshold)]&];*)
(**)
(*properEdges=Sort@Select[properEdges,!MemberQ[excludeVertices,#[[1]]]&&!MemberQ[excludeVertices,#[[2]]]&];*)
(*sourceEdges=Sort[Select[vertices,*)
(*With[{x=Floor[(#-1)/n]-Floor[n/2],y=Mod[(#-1),n] -(Floor[n/2])},*)
(*(sourceThreshold<=Sqrt[x^2+y^2 ]<=threshold)]&]];*)
(*(*myLabels=Flatten[{Map[#->"SOURCE"&,sourceEdges],{0,0}->"ROOT","Name"},1];*)
(**)*)
(*myLabels=Flatten[{Map[#->"SOURCE"&,sourceEdges],(Floor[n/2]*(n))+Floor[n/2]+1->"ROOT","Name"},1];*)
(*Graph[vertices,properEdges,VertexLabels->"Name"];*)
(*g=Graph[vertices,properEdges,VertexLabels->myLabels,ImageSize->Large]*)


(* ::Subsubsection:: *)
(*Linear path (length=7) with bLaplacianRW[]*)


(* ::Input:: *)
(*{{113,114,115,116,117,118,119,120}};*)
(*%//.{d___,{a_,b_,c___}}:>{d,{a,b},{b,c}};*)
(*path=Most[%](*{{113,114},{114,115},{115,116},{116,117},{117,118},{118,119},{119,120}};*)*)
(*bLaplacianRW[g,path]*)
(*Row[{"Single path",path,": ",N[%]*100,"%"}]*)
(**)
(*Row[{"Symmetric paths: ",N[%%]*100*4*3,"%"}]*)


(* ::Subsubsection:: *)
(*Linear path (length=7) with bLaplacianRWlogBC[]*)


(* ::Input:: *)
(*{{113,114,115,116,117,118,119,120}};*)
(*%//.{d___,{a_,b_,c___}}:>{d,{a,b},{b,c}};*)
(*path=Most[%](*{{113,114},{114,115},{115,116},{116,117},{117,118},{118,119},{119,120}};*)*)
(*bLaplacianRWlogBC[g,path]*)
(*Row[{"Single path",path,": ",N[%]*100,"%"}]*)
(**)
(*Row[{"Symmetric paths: ",N[%%]*100*4*3,"%"}]*)


(* ::Subsubsection::Closed:: *)
(*2nd-to-Linear paths (length=8)*)


(* ::Text:: *)
(*The symmetry factor comes from: *)
(*1) restriction to one quadrant: 4x2, i.e. choice to go Down than Right*)
(*2) an extra x2 from the choice between going straight after the first Right turn, or to turn Left and go down again *)


(* ::Input:: *)
(*path={{113,128},{128,129},{129,130},{130,131},{131,132},{132,133},{133,134},{134,135}};*)
(*bLaplacianRW[g,path]*)
(*Row[{"Single path",path,": ",N[%]*100,"%"}]*)
(**)
(*Row[{"Symmetric paths: ",l8p1=N[%%]*100*4*2*2,"%"}]*)


(* ::Input:: *)
(*path={{113,114},{114,129},{129,130},{130,131},{131,132},{132,133},{133,134},{134,135}};*)
(*bLaplacianRW[g,path]*)
(*Row[{"Single path",path,": ",N[%]*100,"%"}]*)
(**)
(*Row[{"Symmetric paths: ",l8p2=N[%%]*100*4*2*2,"%"}]*)


(* ::Input:: *)
(*path={{113,114},{114,115},{115,130},{130,131},{131,132},{132,133},{133,134},{134,135}};*)
(*bLaplacianRW[g,path]*)
(*Row[{"Single path",path,": ",N[%]*100,"%"}]*)
(**)
(*Row[{"Symmetric paths: ",l8p3=N[%%]*100*4*2*2,"%"}]*)


(* ::Input:: *)
(*path={{113,114},{114,115},{115,116},{116,131},{131,132},{132,133},{133,134},{134,135}};*)
(*bLaplacianRW[g,path]*)
(*Row[{"Single path",path,": ",N[%]*100,"%"}]*)
(**)
(*Row[{"Symmetric paths: ",l8p4=N[%%]*100*4*2*2,"%"}]*)


(* ::Input:: *)
(*path={{113,114},{114,115},{115,116},{116,117},{117,132},{132,133},{133,134},{134,135}};*)
(*bLaplacianRW[g,path]*)
(*Row[{"Single path",path,": ",N[%]*100,"%"}]*)
(**)
(*Row[{"Symmetric paths: ",l8p5=N[%%]*100*4*2*2,"%"}]*)


(* ::Input:: *)
(*path={{113,114},{114,115},{115,116},{116,117},{117,118},{118,133},{133,134},{134,135}};*)
(*bLaplacianRW[g,path]*)
(*Row[{"Single path",path,": ",N[%]*100,"%"}]*)
(**)
(*Row[{"Symmetric paths: ",l8p6=N[%%]*100*4*2*2,"%"}]*)


(* ::Input:: *)
(*path={{113,114},{114,115},{115,116},{116,117},{117,118},{118,119},{119,134},{134,135}};*)
(*bLaplacianRW[g,path]*)
(*Row[{"Single path",path,": ",N[%]*100,"%"}]*)
(**)
(*Row[{"Symmetric paths: ",l8p7=N[%%]*100*4*2*2,"%"}]*)


(* ::Input:: *)
(*l8p1+l8p2+l8p3+l8p4+l8p5+l8p6+l8p7*)


(* ::Section::Closed:: *)
(*PathFinder[]  BROKEN HERE*)


(* ::Input::Initialization:: *)
Clear[PathFinder];

PathFinder[graph_]:=Module[{pathList={{}}},
Step[subgraph_]:=Module[{locEdges,locVertices,locWeights,locRoot,locSource,locMoves,newEdges, newRoot,smallerGraph,i},
locEdges = EdgeList@subgraph/.DirectedEdge->List;
locVertices=VertexList@subgraph;
If[Length[locVertices]==1,Return[NULL]];
locWeights=WeightedAdjacencyMatrix[subgraph]//Normal;
locRoot = 
 Select[(List@@@PropertyValue[subgraph,VertexLabels])[[2;;]],StringMatchQ[#[[2]],___~~"ROOT"]&][[All,1]][[1]];
locSource = 
 Select[(List@@@PropertyValue[subgraph,VertexLabels])[[2;;]],StringMatchQ[#[[2]],___~~"SOURCE"]&][[All,1]][[1]];
locMoves=Select[locEdges,#[[1]]==locRoot || #[[2]]==locRoot&][[All]];

Print["###  LocMoves  ####"];
Print[locMoves];

For[i=1, i<=Length[locMoves],i++,

If[i ==Length[locMoves],
(*True*)If[locMoves[[i,2]]==locSource,
 (*Ture*)pathList=Append[pathList[[2;;]],Append[pathList[[1]],locMoves[[i]]]]; Continue[],
 (*False*)pathList=Prepend[pathList[[2;;]],Append[pathList[[1]],locMoves[[i]]]]
],
(*False*)
If[locMoves[[i,2]]==locSource,
 (*True*)pathList=Append[pathList,Append[pathList[[1]],locMoves[[i]]]];Continue[],
 (*False*)pathList=Prepend[pathList,Append[pathList[[1]],locMoves[[i]]]]
]];

(*Print["###  pathList  ####"];
Print[pathList];*);

newEdges=Select[locEdges,#[[1]]=!=locRoot && #[[2]]=!=locRoot &][[All]];
newRoot = locMoves[[i,2]];
If[Length[newEdges] <= 0,Continue[]];
smallerGraph=MyGraph[newEdges,newRoot,locSource,"undirected"->False][[1]];

(*Print[smallerGraph];*);

Step[smallerGraph]
]
];

Step[graph];
Return[pathList]
]


(* ::Subsection:: *)
(*Usage example of PathFinder[]*)


(* ::Input:: *)
(*Step[g]*)


(* ::Input:: *)
(*g2*)
(*PathFinder[g2]*)


(* ::Input:: *)
(*Graph[g,VertexLabels->"Name"]*)
(*PathFinder[%]*)
(*[[1;;2]]*)


(* ::Item::Closed:: *)
(*So*)


(* ::Input:: *)
(*(*Last step available*)*)
(*v=Prepend[v[[2;;]],Append[v[[1]],step0]]*)


(* ::Input:: *)
(*(*Multiple steps available*)*)
(*v=Prepend[v,Append[v[[1]],step1]]*)


(* ::Input:: *)
(*(*Source reached with last step available*)*)
(*v=Append[v[[2;;]],Append[v[[1]],step2]]*)


(* ::Input:: *)
(*(*Source reached with multiple steps available*)*)
(*v=Append[v,Append[v[[1]],step2]]*)


(* ::Subsubitem:: *)
(*Test*)


(* ::Input:: *)
(*Clear[a];*)
(*(*The first position is the one we are gonna update*)*)
(*v={{}}*)
(*rooot=a;*)
(*v=Prepend[v[[2;;]],Append[v[[1]],rooot]]*)
(*step0=b;(*If b is the only step available, then we do not need a copy of v*)*)
(*v=Prepend[v[[2;;]],Append[v[[1]],step0]]*)
(*step1=c;(*Imagine we can now move to c or c2. We then need a copy of v to be able to come back at this bifurcation*)*)
(*v=Prepend[v,Append[v[[1]],step1]]*)
(*step2=d;(*If d reaches the source, then add it to the end*)*)
(*v=Append[v,Append[v[[1]],step2]]*)
(*step2bis=d2;(*If d2 is the only step available, then we do not need a copy of v*)*)
(*v=Prepend[v[[2;;]],Append[v[[1]],step2bis]]*)
(*step3=e;(*If e reaches the source, and is the only step available, add it at the end without copying v*)*)
(*v=Append[v[[2;;]],Append[v[[1]],step3]]*)


(* ::Section::Closed:: *)
(*AllLaplacianRWs[] BROKEN*)


(* ::Input::Initialization:: *)
Clear[AllLaplacianRWs]

Options[AllLaplacianRWs]={"draw"->False,"print"->False};

AllLaplacianRWs[graph_,OptionsPattern[]]:=Module[{possibleLRW,i,probabilityLRW={}},
possibleLRW=PathFinder[graph];
For[i=1, i<= Length[possibleLRW],i++,
AppendTo[probabilityLRW,LaplacianRW[graph,possibleLRW[[i]],OptionValue["draw"]]];
If[OptionValue["print"],Print[i,") Laplacian RW ",possibleLRW[[i]]," with probability " ,probabilityLRW[[i]]]
]
];
Return[{possibleLRW,probabilityLRW}]
]


(* ::Subsection::Closed:: *)
(*Usage example of AllLaplacianRWs[]*)


(* ::Input:: *)
(*AllLaplacianRWs[g,"draw"->True]*)



