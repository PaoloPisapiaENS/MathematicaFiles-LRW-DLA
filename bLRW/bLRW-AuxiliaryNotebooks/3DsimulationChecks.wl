(* ::Package:: *)

(* ::Title::Closed:: *)
(*Initialization*)


(* ::Input:: *)
(*(*Quit*)*)


(* ::Input::Initialization:: *)
<<PaoloInitialization`
??PaoloInitialization`*


(* ::Input::Initialization:: *)
SetOptions[EvaluationNotebook[],CommonDefaultFormatTypes->{"Output"->StandardForm}];


(* ::Input:: *)
(*"D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\MathematicaFiles-LRW-DLA-bLRW\\bLRW\\bLRW-MicroscopicAction-RG-FractalDim-etc\\bLRW-RGfunctions.nb"*)


(* ::Input:: *)
(*(*FrontEndTokenExecute["SelectAll"]*)
(*FrontEndTokenExecute["SelectionCloseAllGroups"]*)*)


(* ::Title:: *)
(*b=1*)


(* ::Section::Closed:: *)
(*(n=5, active L=1) b=1.0_LRW-3d-square-lattice-data-5_repeat-500000_tol-1e-10-HybridSq-LogSpacing-LogBC-UpdatedLatStruct-padding-nOver3.csv	This is a stupid test: all the paths have length 1...*)


(* ::Input:: *)
(*n=5;*)
(*(*1. Read as text and split by double carriage returns/newlines*)rawBlocks=Import["D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\3d\\b=1.0_LRW-3d-square-lattice-data-5_repeat-500000_tol-1e-10-HybridSq-LogSpacing-LogBC-UpdatedLatStruct-padding-nOver3.csv","CSV"];*)
(*(*2. Convert each separate text block cleanly into an array of coordinate pairs*)*)
(*paths=Developer`ToPackedArray[rawBlocks];*)
(*paths[[1;;10]]*)
(*Length@paths*)


(* ::Input:: *)
(*paths=Pick[paths,Length/@paths,2];*)
(*Length[paths]*)


(* ::Input:: *)
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


(* ::Input:: *)
(*totalPaths=Length[pathDistr];*)
(*symmetricProbabilities=symmetricCounts/totalPaths;*)
(**)


(* ::Input:: *)
(*(*3. Generate the BarChart with values on top and unique symmetry families on the bottom*)BarChart[symmetricProbabilities[[#]],ChartLabels->Placed[pathDistr2,Axis],LabelingFunction->(Placed[Row[{NumberForm[100.*#1,{5,2}],"%"}],Above]&),Frame->True,FrameLabel->{"Unique Paths (Symmetry Families Included)","Relative Occurrence (Probability)"},PlotLabel->"Symmetric Path Distribution",ChartStyle->RGBColor[0.87,0.71,0.34],ImageSize->900(*,PlotRange->{All,{0,Max[symmetricProbabilities]*1.15}}*)]&@Span[All]*)


(* ::Text:: *)
(*Compared to the analytical result below, (3->??%)*)


(* ::Section::Closed:: *)
(*(n=9, active L=2) b=1.0_LRW-3d-square-lattice-data-9_repeat-500000_tol-1e-10-HybridSq-LogSpacing-LogBC-UpdatedLatStruct-padding-nOver3.csv		Very good agreement (gets better as the statistic increases)*)


(* ::Input:: *)
(*n=9;*)
(*(*1. Read as text and split by double carriage returns/newlines*)rawBlocks=Import["D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\3d\\b=1.0_LRW-3d-square-lattice-data-9_repeat-500000_tol-1e-10-HybridSq-LogSpacing-LogBC-UpdatedLatStruct-padding-nOver3.csv","CSV"];*)
(*(*2. Convert each separate text block cleanly into an array of coordinate pairs*)*)
(*paths=Developer`ToPackedArray[rawBlocks];*)
(*paths[[1;;10]]*)
(*Length@paths*)


(* ::Input:: *)
(*paths=Pick[paths,Length/@paths,2];*)
(*Length[paths]*)


(* ::Input:: *)
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


(* ::Input:: *)
(*totalPaths=Length[pathDistr];*)
(*symmetricProbabilities=symmetricCounts/totalPaths;*)
(**)


(* ::Input:: *)
(*(*3. Generate the BarChart with values on top and unique symmetry families on the bottom*)BarChart[symmetricProbabilities[[#]],ChartLabels->Placed[pathDistr2,Axis],LabelingFunction->(Placed[Row[{NumberForm[100.*#1,{5,2}],"%"}],Above]&),Frame->True,FrameLabel->{"Unique Paths (Symmetry Families Included)","Relative Occurrence (Probability)"},PlotLabel->"Symmetric Path Distribution",ChartStyle->RGBColor[0.87,0.71,0.34],ImageSize->900(*,PlotRange->{All,{0,Max[symmetricProbabilities]*1.15}}*)]&@Span[All]*)


(* ::Text:: *)
(*Compared to the analytical result below, (3->5.38775%) THAT IS VERY GOOD!!*)


(* ::Section::Closed:: *)
(*(n=15, L=7) b=1.0_LRW-3d-square-lattice-data-15_repeat-500000_tol-1e-10-HybridSq-LogSpacing-LogBC-UpdatedLatStruct-padding-nOver3.csv		How is it??*)


(* ::Input:: *)
(*n=15;*)
(*(*1. Read as text and split by double carriage returns/newlines*)rawBlocks=Import["D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\3d\\b=1.0_LRW-3d-square-lattice-data-15_repeat-500000_tol-1e-10-HybridSq-LogSpacing-LogBC-UpdatedLatStruct-padding-nOver3.csv","CSV"];*)
(*(*2. Convert each separate text block cleanly into an array of coordinate pairs*)*)
(*paths=Developer`ToPackedArray[rawBlocks];*)
(*paths[[1;;10]]*)
(*Length@paths*)


(* ::Input:: *)
(*paths=Pick[paths,Length/@paths,2];*)
(*Length[paths]*)


(* ::Input:: *)
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


(* ::Input:: *)
(*totalPaths=Length[pathDistr];*)
(*symmetricProbabilities=symmetricCounts/totalPaths;*)
(**)


(* ::Input:: *)
(*(*3. Generate the BarChart with values on top and unique symmetry families on the bottom*)BarChart[symmetricProbabilities[[#]],ChartLabels->Placed[pathDistr2,Axis],LabelingFunction->(Placed[Row[{NumberForm[100.*#1,{5,2}],"%"}],Above]&),Frame->True,FrameLabel->{"Unique Paths (Symmetry Families Included)","Relative Occurrence (Probability)"},PlotLabel->"Symmetric Path Distribution",ChartStyle->RGBColor[0.87,0.71,0.34],ImageSize->900(*,PlotRange->{All,{0,Max[symmetricProbabilities]*1.15}}*)]&@Span[1,30]*)


(* ::Text:: *)
(*Compared to the analytical result below, (6->??%)*)


(* ::Chapter::Closed:: *)
(*Checking parameters with hybrid strategy (n=9): maybe best is n/4	*)


(* ::Section::Closed:: *)
(*Check padding (b=1.0_LRW-3d-square-lattice-data-9_repeat-500000_tol-1e-10-HybridSq-LogSpacing-padding-nOver1.csv)		*)


(* ::Input:: *)
(*n=9;*)
(*(*1. Read as text and split by double carriage returns/newlines*)rawBlocks=Import["D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\3d\\b=1.0_LRW-3d-square-lattice-data-9_repeat-500000_tol-1e-10-HybridSq-LogSpacing-padding-nOver1.csv","CSV"];*)
(**)
(*(*2. Convert each separate text block cleanly into an array of coordinate pairs*)*)
(*paths=Developer`ToPackedArray[rawBlocks];*)
(*paths[[1;;10]]*)
(*Length[paths]*)


(* ::Input:: *)
(*paths=Pick[paths,Length/@paths,2];*)
(*Length[paths]*)


(* ::Input:: *)
(*pathDistrnOver1=paths[[All,2]];*)
(*Length@pathDistrnOver1*)


(* ::Input:: *)
(*Histogram[#,Length[#],"PDF",PlotRange->All]&@pathDistrnOver1*)
(*Histogram[#,Automatic(*(Length[#]/20)*),"PDF",PlotRange->All]&@pathDistrnOver1*)
(**)


(* ::Input:: *)
(*(*1. Calculate relative frequencies ("probabilities")*)*)
(*Sort@Tally[pathDistrnOver1];*)
(*{pathDistr2nOver1,symmetricCountsnOver1}=Transpose[%]*)


(* ::Input:: *)
(*pathDistr2nOver1==Sort@DeleteDuplicates@pathDistrnOver1*)


(* ::Input:: *)
(*meannOver1=Mean[pathDistrnOver1]//N *)
(*stdDevnOver1=StandardDeviation[pathDistrnOver1]//N*)


(* ::Input:: *)
(*totalPathsnOver1=Length[pathDistrnOver1];*)
(*symmetricProbabilitiesnOver1=symmetricCountsnOver1/totalPathsnOver1;*)
(**)


(* ::Input:: *)
(*(*3. Generate the BarChart with values on top and unique symmetry families on the bottom*)barChartnOver1=BarChart[symmetricProbabilitiesnOver1[[#]],ChartLabels->Placed[pathDistr2nOver1,Axis],LabelingFunction->(Placed[Row[{NumberForm[100.*#1,{5,2}],"%"}],Above]&),Frame->True,FrameLabel->{"Path Length","Relative Occurrence (Probability)"},PlotLabel->Row[{"Path Distribution nOver1"}],ChartStyle->RGBColor[0.87,0.71,0.34],ImageSize->900(*,PlotRange->{All,{0,Max[symmetricProbabilities]*1.15}}*)]&@Span[All]*)


(* ::Section::Closed:: *)
(*Check padding (b=1.0_LRW-3d-square-lattice-data-9_repeat-500000_tol-1e-10-HybridSq-LogSpacing-padding-nOver2.csv)		*)


(* ::Input:: *)
(*n=9;*)
(*(*1. Read as text and split by double carriage returns/newlines*)rawBlocks=Import["D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\3d\\b=1.0_LRW-3d-square-lattice-data-9_repeat-500000_tol-1e-10-HybridSq-LogSpacing-padding-nOver2.csv","CSV"];*)
(**)
(*(*2. Convert each separate text block cleanly into an array of coordinate pairs*)*)
(*paths=Developer`ToPackedArray[rawBlocks];*)
(*paths[[1;;10]]*)
(*Length[paths]*)


(* ::Input:: *)
(*paths=Pick[paths,Length/@paths,2];*)
(*Length[paths]*)


(* ::Input:: *)
(*pathDistrnOver2=paths[[All,2]];*)
(*Length@pathDistrnOver2*)


(* ::Input:: *)
(*Histogram[#,Length[#],"PDF",PlotRange->All]&@pathDistrnOver2*)
(*Histogram[#,Automatic(*(Length[#]/20)*),"PDF",PlotRange->All]&@pathDistrnOver2*)
(**)


(* ::Input:: *)
(*(*1. Calculate relative frequencies ("probabilities")*)*)
(*Sort@Tally[pathDistrnOver2];*)
(*{pathDistr2nOver2,symmetricCountsnOver2}=Transpose[%]*)


(* ::Input:: *)
(*pathDistr2nOver2==Sort@DeleteDuplicates@pathDistrnOver2*)


(* ::Input:: *)
(*meannOver2=Mean[pathDistrnOver2]//N *)
(**)
(*stdDevnOver2=StandardDeviation[pathDistrnOver2]//N*)


(* ::Input:: *)
(*totalPathsnOver2=Length[pathDistrnOver2];*)
(*symmetricProbabilitiesnOver2=symmetricCountsnOver2/totalPathsnOver2;*)
(**)


(* ::Input:: *)
(*(*3. Generate the BarChart with values on top and unique symmetry families on the bottom*)barChartnOver2=BarChart[symmetricProbabilitiesnOver2[[#]],ChartLabels->Placed[pathDistr2nOver2,Axis],LabelingFunction->(Placed[Row[{NumberForm[100.*#1,{5,2}],"%"}],Above]&),Frame->True,FrameLabel->{"Path Length","Relative Occurrence (Probability)"},PlotLabel->Row[{"Path Distribution nOver2"}],ChartStyle->RGBColor[0.87,0.71,0.34],ImageSize->900(*,PlotRange->{All,{0,Max[symmetricProbabilities]*1.15}}*)]&@Span[All]*)


(* ::Section::Closed:: *)
(*Check padding (b=1.0_LRW-3d-square-lattice-data-9_repeat-500000_tol-1e-10-HybridSq-LogSpacing-padding-nOver3.csv)		*)


(* ::Input:: *)
(*n=9;*)
(*(*1. Read as text and split by double carriage returns/newlines*)rawBlocks=Import["D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\3d\\b=1.0_LRW-3d-square-lattice-data-9_repeat-500000_tol-1e-10-HybridSq-LogSpacing-padding-nOver3.csv","CSV"];*)
(**)
(*(*2. Convert each separate text block cleanly into an array of coordinate pairs*)*)
(*paths=Developer`ToPackedArray[rawBlocks];*)
(*paths[[1;;10]]*)
(*Length[paths]*)
(*paths=Pick[paths,Length/@paths,2];*)
(*Length[paths]*)


(* ::Input:: *)
(*pathDistrnOver3=paths[[All,2]];*)
(*Length@pathDistrnOver3*)


(* ::Input:: *)
(*Histogram[#,Length[#],"PDF",PlotRange->All]&@pathDistrnOver3*)
(*Histogram[#,Automatic(*(Length[#]/20)*),"PDF",PlotRange->All]&@pathDistrnOver3*)
(**)


(* ::Input:: *)
(*(*1. Calculate relative frequencies ("probabilities")*)*)
(*Sort@Tally[pathDistrnOver3];*)
(*{pathDistr2nOver3,symmetricCountsnOver3}=Transpose[%]*)


(* ::Input:: *)
(*pathDistr2nOver3==Sort@DeleteDuplicates@pathDistrnOver3*)


(* ::Input:: *)
(*meannOver3=Mean[pathDistrnOver3]//N *)
(*stdDevnOver3=StandardDeviation[pathDistrnOver3]//N*)


(* ::Input:: *)
(*totalPathsnOver3=Length[pathDistrnOver3];*)
(*symmetricProbabilitiesnOver3=symmetricCountsnOver3/totalPathsnOver3;*)
(**)


(* ::Input:: *)
(*(*3. Generate the BarChart with values on top and unique symmetry families on the bottom*)barChartnOver3=BarChart[symmetricProbabilitiesnOver3[[#]],ChartLabels->Placed[pathDistr2nOver3,Axis],LabelingFunction->(Placed[Row[{NumberForm[100.*#1,{5,2}],"%"}],Above]&),Frame->True,FrameLabel->{"Path Length","Relative Occurrence (Probability)"},PlotLabel->Row[{"Path Distribution nOver3"}],ChartStyle->RGBColor[0.87,0.71,0.34],ImageSize->900(*,PlotRange->{All,{0,Max[symmetricProbabilities]*1.15}}*)]&@Span[All]*)


(* ::Section::Closed:: *)
(*Check padding (b=1.0_LRW-3d-square-lattice-data-9_repeat-500000_tol-1e-10-HybridSq-LogSpacing-padding-nOver4.csv)		*)


(* ::Input:: *)
(*n=9;*)
(*(*1. Read as text and split by double carriage returns/newlines*)rawBlocks=Import["D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\3d\\b=1.0_LRW-3d-square-lattice-data-9_repeat-500000_tol-1e-10-HybridSq-LogSpacing-padding-nOver4.csv","CSV"];*)
(**)
(*(*2. Convert each separate text block cleanly into an array of coordinate pairs*)*)
(*paths=Developer`ToPackedArray[rawBlocks];*)
(*paths[[1;;10]]*)
(*Length[paths]*)
(*paths=Pick[paths,Length/@paths,2];*)
(*Length[paths]*)


(* ::Input:: *)
(*pathDistrnOver4=paths[[All,2]];*)
(*Length@pathDistrnOver4*)


(* ::Input:: *)
(*Histogram[#,Length[#],"PDF",PlotRange->All]&@pathDistrnOver4*)
(*Histogram[#,Automatic(*(Length[#]/20)*),"PDF",PlotRange->All]&@pathDistrnOver4*)
(**)


(* ::Input:: *)
(*(*1. Calculate relative frequencies ("probabilities")*)*)
(*Sort@Tally[pathDistrnOver4];*)
(*{pathDistr2nOver4,symmetricCountsnOver4}=Transpose[%]*)


(* ::Input:: *)
(*pathDistr2nOver4==Sort@DeleteDuplicates@pathDistrnOver4*)


(* ::Input:: *)
(*meannOver4=Mean[pathDistrnOver4]//N *)
(*stdDevnOver4=StandardDeviation[pathDistrnOver4]//N*)


(* ::Input:: *)
(*totalPathsnOver4=Length[pathDistrnOver4];*)
(*symmetricProbabilitiesnOver4=symmetricCountsnOver4/totalPathsnOver4;*)
(**)


(* ::Input:: *)
(*(*3. Generate the BarChart with values on top and unique symmetry families on the bottom*)barChartnOver4=BarChart[symmetricProbabilitiesnOver4[[#]],ChartLabels->Placed[pathDistr2nOver4,Axis],LabelingFunction->(Placed[Row[{NumberForm[100.*#1,{5,2}],"%"}],Above]&),Frame->True,FrameLabel->{"Path Length","Relative Occurrence (Probability)"},PlotLabel->Row[{"Path Distribution nOver4"}],ChartStyle->RGBColor[0.87,0.71,0.34],ImageSize->900(*,PlotRange->{All,{0,Max[symmetricProbabilities]*1.15}}*)]&@Span[All]*)


(* ::Section::Closed:: *)
(*Check padding (b=1.0_LRW-3d-square-lattice-data-9_repeat-500000_tol-1e-10-HybridSq-LogSpacing-padding-nOver5.csv)		*)


(* ::Input:: *)
(*n=9;*)
(*(*1. Read as text and split by double carriage returns/newlines*)rawBlocks=Import["D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\3d\\b=1.0_LRW-3d-square-lattice-data-9_repeat-500000_tol-1e-10-HybridSq-LogSpacing-padding-nOver5.csv","CSV"];*)
(**)
(*(*2. Convert each separate text block cleanly into an array of coordinate pairs*)*)
(*paths=Developer`ToPackedArray[rawBlocks];*)
(*paths[[1;;10]]*)
(*Length[paths]*)
(*paths=Pick[paths,Length/@paths,2];*)
(*Length[paths]*)


(* ::Input:: *)
(*pathDistrnOver5=paths[[All,2]];*)
(*Length@pathDistrnOver5*)


(* ::Input:: *)
(*Histogram[#,Length[#],"PDF",PlotRange->All]&@pathDistrnOver5*)
(*Histogram[#,Automatic(*(Length[#]/20)*),"PDF",PlotRange->All]&@pathDistrnOver5*)
(**)


(* ::Input:: *)
(*(*1. Calculate relative frequencies ("probabilities")*)*)
(*Sort@Tally[pathDistrnOver5];*)
(*{pathDistr2nOver5,symmetricCountsnOver5}=Transpose[%]*)


(* ::Input:: *)
(*pathDistr2nOver5==Sort@DeleteDuplicates@pathDistrnOver5*)


(* ::Input:: *)
(*meannOver5=Mean[pathDistrnOver5]//N *)
(*stdDevnOver5=StandardDeviation[pathDistrnOver5]//N*)


(* ::Input:: *)
(*totalPathsnOver5=Length[pathDistrnOver5];*)
(*symmetricProbabilitiesnOver5=symmetricCountsnOver5/totalPathsnOver5;*)
(**)


(* ::Input:: *)
(*(*3. Generate the BarChart with values on top and unique symmetry families on the bottom*)barChartnOver5=BarChart[symmetricProbabilitiesnOver5[[#]],ChartLabels->Placed[pathDistr2nOver5,Axis],LabelingFunction->(Placed[Row[{NumberForm[100.*#1,{5,2}],"%"}],Above]&),Frame->True,FrameLabel->{"Path Length","Relative Occurrence (Probability)"},PlotLabel->Row[{"Path Distribution nOver5"}],ChartStyle->RGBColor[0.87,0.71,0.34],ImageSize->900(*,PlotRange->{All,{0,Max[symmetricProbabilities]*1.15}}*)]&@Span[All]*)


(* ::Section::Closed:: *)
(*Check padding (b=1.0_LRW-3d-square-lattice-data-9_repeat-500000_tol-1e-10-HybridSq-LogSpacing-padding-nOver6.csv)		*)


(* ::Input:: *)
(*n=9;*)
(*(*1. Read as text and split by double carriage returns/newlines*)rawBlocks=Import["D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\3d\\b=1.0_LRW-3d-square-lattice-data-9_repeat-500000_tol-1e-10-HybridSq-LogSpacing-padding-nOver6.csv","CSV"];*)
(**)
(*(*2. Convert each separate text block cleanly into an array of coordinate pairs*)*)
(*paths=Developer`ToPackedArray[rawBlocks];*)
(*paths[[1;;10]]*)
(*Length[paths]*)
(*paths=Pick[paths,Length/@paths,2];*)
(*Length[paths]*)


(* ::Input:: *)
(*pathDistrnOver6=paths[[All,2]];*)
(*Length@pathDistrnOver6*)


(* ::Input:: *)
(*Histogram[#,Length[#],"PDF",PlotRange->All]&@pathDistrnOver6*)
(*Histogram[#,Automatic(*(Length[#]/20)*),"PDF",PlotRange->All]&@pathDistrnOver6*)
(**)


(* ::Input:: *)
(*(*1. Calculate relative frequencies ("probabilities")*)*)
(*Sort@Tally[pathDistrnOver6];*)
(*{pathDistr2nOver6,symmetricCountsnOver6}=Transpose[%]*)


(* ::Input:: *)
(*pathDistr2nOver6==Sort@DeleteDuplicates@pathDistrnOver6*)


(* ::Input:: *)
(*meannOver6=Mean[pathDistrnOver6]//N *)
(*stdDevnOver6=StandardDeviation[pathDistrnOver6]//N*)


(* ::Input:: *)
(*totalPathsnOver6=Length[pathDistrnOver6];*)
(*symmetricProbabilitiesnOver6=symmetricCountsnOver6/totalPathsnOver6;*)
(**)


(* ::Input:: *)
(*(*3. Generate the BarChart with values on top and unique symmetry families on the bottom*)barChartnOver6=BarChart[symmetricProbabilitiesnOver6[[#]],ChartLabels->Placed[pathDistr2nOver6,Axis],LabelingFunction->(Placed[Row[{NumberForm[100.*#1,{5,2}],"%"}],Above]&),Frame->True,FrameLabel->{"Path Length","Relative Occurrence (Probability)"},PlotLabel->Row[{"Path Distribution nOver6"}],ChartStyle->RGBColor[0.87,0.71,0.34],ImageSize->900(*,PlotRange->{All,{0,Max[symmetricProbabilities]*1.15}}*)]&@Span[All]*)


(* ::Section::Closed:: *)
(*Comparison*)


(* ::Input:: *)
(*Multicolumn[{barChartnOver1*)
(*,barChartnOver2*)
(*,barChartnOver3*)
(*,barChartnOver4*)
(*,barChartnOver5*)
(*,barChartnOver6}/.{(ImageSize->a_)->(ImageSize->Medium)},3]*)


(* ::Input:: *)
(*DataLength=Sqrt[500000];*)
(*{{1,Around[meannOver1,stdDevnOver1/DataLength]}*)
(*,{2,Around[meannOver2,stdDevnOver2/DataLength]}*)
(*,{3,Around[meannOver3,stdDevnOver3/DataLength]}*)
(*,{4,Around[meannOver4,stdDevnOver4/DataLength]}*)
(*,{5,Around[meannOver5,stdDevnOver5/DataLength]}*)
(*,{6,Around[meannOver6,stdDevnOver6/DataLength]}}*)
(*ListPlot[%]*)


(* ::Text:: *)
(*padding=n/4 seems the best here. Now checking with bigger systems*)


(* ::Input:: *)
(*2*Round[Exp[7.3]]+1*)


(* ::Chapter:: *)
(*Checking parameters with hybrid strategy (n=15): hard to tell the best*)


(* ::Section::Closed:: *)
(*Check padding (b=1.0_LRW-3d-square-lattice-data-15_repeat-500000_tol-1e-10-HybridSq-LogSpacing-padding-nOver1.csv)		*)


(* ::Input:: *)
(*n=9;*)
(*(*1. Read as text and split by double carriage returns/newlines*)rawBlocks=Import["D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\3d\\b=1.0_LRW-3d-square-lattice-data-15_repeat-500000_tol-1e-10-HybridSq-LogSpacing-padding-nOver1.csv","CSV"];*)
(**)
(*(*2. Convert each separate text block cleanly into an array of coordinate pairs*)*)
(*paths=Developer`ToPackedArray[rawBlocks];*)
(*paths[[1;;10]]*)
(*Length[paths]*)


(* ::Input:: *)
(*paths=Pick[paths,Length/@paths,2];*)
(*Length[paths]*)


(* ::Input:: *)
(*pathDistrnOver1=paths[[All,2]];*)
(*Length@pathDistrnOver1*)


(* ::Input:: *)
(*Histogram[#,Length[#],"PDF",PlotRange->All]&@pathDistrnOver1*)
(*Histogram[#,Automatic(*(Length[#]/20)*),"PDF",PlotRange->All]&@pathDistrnOver1*)
(**)


(* ::Input:: *)
(*(*1. Calculate relative frequencies ("probabilities")*)*)
(*Sort@Tally[pathDistrnOver1];*)
(*{pathDistr2nOver1,symmetricCountsnOver1}=Transpose[%]*)


(* ::Input:: *)
(*pathDistr2nOver1==Sort@DeleteDuplicates@pathDistrnOver1*)


(* ::Input:: *)
(*meannOver1=Mean[pathDistrnOver1]//N *)
(*stdDevnOver1=StandardDeviation[pathDistrnOver1]//N*)


(* ::Input:: *)
(*totalPathsnOver1=Length[pathDistrnOver1];*)
(*symmetricProbabilitiesnOver1=symmetricCountsnOver1/totalPathsnOver1;*)
(**)


(* ::Input:: *)
(*(*3. Generate the BarChart with values on top and unique symmetry families on the bottom*)barChartnOver1=BarChart[symmetricProbabilitiesnOver1[[#]],ChartLabels->Placed[pathDistr2nOver1,Axis],LabelingFunction->(Placed[Row[{NumberForm[100.*#1,{5,2}],"%"}],Above]&),Frame->True,FrameLabel->{"Path Length","Relative Occurrence (Probability)"},PlotLabel->Row[{"Path Distribution nOver1"}],ChartStyle->RGBColor[0.87,0.71,0.34],ImageSize->900(*,PlotRange->{All,{0,Max[symmetricProbabilities]*1.15}}*)]&@Span[1,30]*)


(* ::Section::Closed:: *)
(*Check padding (b=1.0_LRW-3d-square-lattice-data-15_repeat-500000_tol-1e-10-HybridSq-LogSpacing-padding-nOver2.csv)		*)


(* ::Input:: *)
(*n=9;*)
(*(*1. Read as text and split by double carriage returns/newlines*)rawBlocks=Import["D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\3d\\b=1.0_LRW-3d-square-lattice-data-15_repeat-500000_tol-1e-10-HybridSq-LogSpacing-padding-nOver2.csv","CSV"];*)
(**)
(*(*2. Convert each separate text block cleanly into an array of coordinate pairs*)*)
(*paths=Developer`ToPackedArray[rawBlocks];*)
(*paths[[1;;10]]*)
(*Length[paths]*)


(* ::Input:: *)
(*paths=Pick[paths,Length/@paths,2];*)
(*Length[paths]*)


(* ::Input:: *)
(*pathDistrnOver2=paths[[All,2]];*)
(*Length@pathDistrnOver2*)


(* ::Input:: *)
(*Histogram[#,Length[#],"PDF",PlotRange->All]&@pathDistrnOver2*)
(*Histogram[#,Automatic(*(Length[#]/20)*),"PDF",PlotRange->All]&@pathDistrnOver2*)
(**)


(* ::Input:: *)
(*(*1. Calculate relative frequencies ("probabilities")*)*)
(*Sort@Tally[pathDistrnOver2];*)
(*{pathDistr2nOver2,symmetricCountsnOver2}=Transpose[%]*)


(* ::Input:: *)
(*pathDistr2nOver2==Sort@DeleteDuplicates@pathDistrnOver2*)


(* ::Input:: *)
(*meannOver2=Mean[pathDistrnOver2]//N *)
(**)
(*stdDevnOver2=StandardDeviation[pathDistrnOver2]//N*)


(* ::Input:: *)
(*totalPathsnOver2=Length[pathDistrnOver2];*)
(*symmetricProbabilitiesnOver2=symmetricCountsnOver2/totalPathsnOver2;*)
(**)


(* ::Input:: *)
(*(*3. Generate the BarChart with values on top and unique symmetry families on the bottom*)barChartnOver2=BarChart[symmetricProbabilitiesnOver2[[#]],ChartLabels->Placed[pathDistr2nOver2,Axis],LabelingFunction->(Placed[Row[{NumberForm[100.*#1,{5,2}],"%"}],Above]&),Frame->True,FrameLabel->{"Path Length","Relative Occurrence (Probability)"},PlotLabel->Row[{"Path Distribution nOver2"}],ChartStyle->RGBColor[0.87,0.71,0.34],ImageSize->900(*,PlotRange->{All,{0,Max[symmetricProbabilities]*1.15}}*)]&@Span[1,30]*)


(* ::Section::Closed:: *)
(*Check padding (b=1.0_LRW-3d-square-lattice-data-15_repeat-500000_tol-1e-10-HybridSq-LogSpacing-padding-nOver3.csv)		*)


(* ::Input:: *)
(*n=9;*)
(*(*1. Read as text and split by double carriage returns/newlines*)rawBlocks=Import["D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\3d\\b=1.0_LRW-3d-square-lattice-data-15_repeat-500000_tol-1e-10-HybridSq-LogSpacing-padding-nOver3.csv","CSV"];*)
(**)
(*(*2. Convert each separate text block cleanly into an array of coordinate pairs*)*)
(*paths=Developer`ToPackedArray[rawBlocks];*)
(*paths[[1;;10]]*)
(*Length[paths]*)
(*paths=Pick[paths,Length/@paths,2];*)
(*Length[paths]*)


(* ::Input:: *)
(*pathDistrnOver3=paths[[All,2]];*)
(*Length@pathDistrnOver3*)


(* ::Input:: *)
(*Histogram[#,Length[#],"PDF",PlotRange->All]&@pathDistrnOver3*)
(*Histogram[#,Automatic(*(Length[#]/20)*),"PDF",PlotRange->All]&@pathDistrnOver3*)
(**)


(* ::Input:: *)
(*(*1. Calculate relative frequencies ("probabilities")*)*)
(*Sort@Tally[pathDistrnOver3];*)
(*{pathDistr2nOver3,symmetricCountsnOver3}=Transpose[%]*)


(* ::Input:: *)
(*pathDistr2nOver3==Sort@DeleteDuplicates@pathDistrnOver3*)


(* ::Input:: *)
(*meannOver3=Mean[pathDistrnOver3]//N *)
(*stdDevnOver3=StandardDeviation[pathDistrnOver3]//N*)


(* ::Input:: *)
(*totalPathsnOver3=Length[pathDistrnOver3];*)
(*symmetricProbabilitiesnOver3=symmetricCountsnOver3/totalPathsnOver3;*)
(**)


(* ::Input:: *)
(*(*3. Generate the BarChart with values on top and unique symmetry families on the bottom*)barChartnOver3=BarChart[symmetricProbabilitiesnOver3[[#]],ChartLabels->Placed[pathDistr2nOver3,Axis],LabelingFunction->(Placed[Row[{NumberForm[100.*#1,{5,2}],"%"}],Above]&),Frame->True,FrameLabel->{"Path Length","Relative Occurrence (Probability)"},PlotLabel->Row[{"Path Distribution nOver3"}],ChartStyle->RGBColor[0.87,0.71,0.34],ImageSize->900(*,PlotRange->{All,{0,Max[symmetricProbabilities]*1.15}}*)]&@Span[1,30]*)


(* ::Section::Closed:: *)
(*Check padding (b=1.0_LRW-3d-square-lattice-data-15_repeat-500000_tol-1e-10-HybridSq-LogSpacing-padding-nOver4.csv)		*)


(* ::Input:: *)
(*n=9;*)
(*(*1. Read as text and split by double carriage returns/newlines*)rawBlocks=Import["D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\3d\\b=1.0_LRW-3d-square-lattice-data-15_repeat-500000_tol-1e-10-HybridSq-LogSpacing-padding-nOver4.csv","CSV"];*)
(**)
(*(*2. Convert each separate text block cleanly into an array of coordinate pairs*)*)
(*paths=Developer`ToPackedArray[rawBlocks];*)
(*paths[[1;;10]]*)
(*Length[paths]*)
(*paths=Pick[paths,Length/@paths,2];*)
(*Length[paths]*)


(* ::Input:: *)
(*pathDistrnOver4=paths[[All,2]];*)
(*Length@pathDistrnOver4*)


(* ::Input:: *)
(*Histogram[#,Length[#],"PDF",PlotRange->All]&@pathDistrnOver4*)
(*Histogram[#,Automatic(*(Length[#]/20)*),"PDF",PlotRange->All]&@pathDistrnOver4*)
(**)


(* ::Input:: *)
(*(*1. Calculate relative frequencies ("probabilities")*)*)
(*Sort@Tally[pathDistrnOver4];*)
(*{pathDistr2nOver4,symmetricCountsnOver4}=Transpose[%]*)


(* ::Input:: *)
(*pathDistr2nOver4==Sort@DeleteDuplicates@pathDistrnOver4*)


(* ::Input:: *)
(*meannOver4=Mean[pathDistrnOver4]//N *)
(*stdDevnOver4=StandardDeviation[pathDistrnOver4]//N*)


(* ::Input:: *)
(*totalPathsnOver4=Length[pathDistrnOver4];*)
(*symmetricProbabilitiesnOver4=symmetricCountsnOver4/totalPathsnOver4;*)
(**)


(* ::Input:: *)
(*(*3. Generate the BarChart with values on top and unique symmetry families on the bottom*)barChartnOver4=BarChart[symmetricProbabilitiesnOver4[[#]],ChartLabels->Placed[pathDistr2nOver4,Axis],LabelingFunction->(Placed[Row[{NumberForm[100.*#1,{5,2}],"%"}],Above]&),Frame->True,FrameLabel->{"Path Length","Relative Occurrence (Probability)"},PlotLabel->Row[{"Path Distribution nOver4"}],ChartStyle->RGBColor[0.87,0.71,0.34],ImageSize->900(*,PlotRange->{All,{0,Max[symmetricProbabilities]*1.15}}*)]&@Span[1,30]*)


(* ::Section::Closed:: *)
(*Check padding (b=1.0_LRW-3d-square-lattice-data-15_repeat-500000_tol-1e-10-HybridSq-LogSpacing-padding-nOver5.csv)		*)


(* ::Input:: *)
(*n=9;*)
(*(*1. Read as text and split by double carriage returns/newlines*)rawBlocks=Import["D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\3d\\b=1.0_LRW-3d-square-lattice-data-15_repeat-500000_tol-1e-10-HybridSq-LogSpacing-padding-nOver5.csv","CSV"];*)
(**)
(*(*2. Convert each separate text block cleanly into an array of coordinate pairs*)*)
(*paths=Developer`ToPackedArray[rawBlocks];*)
(*paths[[1;;10]]*)
(*Length[paths]*)
(*paths=Pick[paths,Length/@paths,2];*)
(*Length[paths]*)


(* ::Input:: *)
(*pathDistrnOver5=paths[[All,2]];*)
(*Length@pathDistrnOver5*)


(* ::Input:: *)
(*Histogram[#,Length[#],"PDF",PlotRange->All]&@pathDistrnOver5*)
(*Histogram[#,Automatic(*(Length[#]/20)*),"PDF",PlotRange->All]&@pathDistrnOver5*)
(**)


(* ::Input:: *)
(*(*1. Calculate relative frequencies ("probabilities")*)*)
(*Sort@Tally[pathDistrnOver5];*)
(*{pathDistr2nOver5,symmetricCountsnOver5}=Transpose[%]*)


(* ::Input:: *)
(*pathDistr2nOver5==Sort@DeleteDuplicates@pathDistrnOver5*)


(* ::Input:: *)
(*meannOver5=Mean[pathDistrnOver5]//N *)
(*stdDevnOver5=StandardDeviation[pathDistrnOver5]//N*)


(* ::Input:: *)
(*totalPathsnOver5=Length[pathDistrnOver5];*)
(*symmetricProbabilitiesnOver5=symmetricCountsnOver5/totalPathsnOver5;*)
(**)


(* ::Input:: *)
(*(*3. Generate the BarChart with values on top and unique symmetry families on the bottom*)barChartnOver5=BarChart[symmetricProbabilitiesnOver5[[#]],ChartLabels->Placed[pathDistr2nOver5,Axis],LabelingFunction->(Placed[Row[{NumberForm[100.*#1,{5,2}],"%"}],Above]&),Frame->True,FrameLabel->{"Path Length","Relative Occurrence (Probability)"},PlotLabel->Row[{"Path Distribution nOver5"}],ChartStyle->RGBColor[0.87,0.71,0.34],ImageSize->900(*,PlotRange->{All,{0,Max[symmetricProbabilities]*1.15}}*)]&@Span[1,30]*)


(* ::Section::Closed:: *)
(*Check padding (b=1.0_LRW-3d-square-lattice-data-15_repeat-500000_tol-1e-10-HybridSq-LogSpacing-padding-nOver6.csv)		*)


(* ::Input:: *)
(*n=9;*)
(*(*1. Read as text and split by double carriage returns/newlines*)rawBlocks=Import["D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\3d\\b=1.0_LRW-3d-square-lattice-data-15_repeat-500000_tol-1e-10-HybridSq-LogSpacing-padding-nOver6.csv","CSV"];*)
(**)
(*(*2. Convert each separate text block cleanly into an array of coordinate pairs*)*)
(*paths=Developer`ToPackedArray[rawBlocks];*)
(*paths[[1;;10]]*)
(*Length[paths]*)
(*paths=Pick[paths,Length/@paths,2];*)
(*Length[paths]*)


(* ::Input:: *)
(*pathDistrnOver6=paths[[All,2]];*)
(*Length@pathDistrnOver6*)


(* ::Input:: *)
(*Histogram[#,Length[#],"PDF",PlotRange->All]&@pathDistrnOver6*)
(*Histogram[#,Automatic(*(Length[#]/20)*),"PDF",PlotRange->All]&@pathDistrnOver6*)
(**)


(* ::Input:: *)
(*(*1. Calculate relative frequencies ("probabilities")*)*)
(*Sort@Tally[pathDistrnOver6];*)
(*{pathDistr2nOver6,symmetricCountsnOver6}=Transpose[%]*)


(* ::Input:: *)
(*pathDistr2nOver6==Sort@DeleteDuplicates@pathDistrnOver6*)


(* ::Input:: *)
(*meannOver6=Mean[pathDistrnOver6]//N *)
(*stdDevnOver6=StandardDeviation[pathDistrnOver6]//N*)


(* ::Input:: *)
(*totalPathsnOver6=Length[pathDistrnOver6];*)
(*symmetricProbabilitiesnOver6=symmetricCountsnOver6/totalPathsnOver6;*)
(**)


(* ::Input:: *)
(*(*3. Generate the BarChart with values on top and unique symmetry families on the bottom*)barChartnOver6=BarChart[symmetricProbabilitiesnOver6[[#]],ChartLabels->Placed[pathDistr2nOver6,Axis],LabelingFunction->(Placed[Row[{NumberForm[100.*#1,{5,2}],"%"}],Above]&),Frame->True,FrameLabel->{"Path Length","Relative Occurrence (Probability)"},PlotLabel->Row[{"Path Distribution nOver6"}],ChartStyle->RGBColor[0.87,0.71,0.34],ImageSize->900(*,PlotRange->{All,{0,Max[symmetricProbabilities]*1.15}}*)]&@Span[1,30]*)


(* ::Section::Closed:: *)
(*Comparison*)


(* ::Input:: *)
(*Multicolumn[{barChartnOver1*)
(*,barChartnOver2*)
(*,barChartnOver3*)
(*,barChartnOver4*)
(*,barChartnOver5*)
(*,barChartnOver6}/.{(ImageSize->a_)->(ImageSize->Medium),Span[All]->Span[1,30]},3]*)


(* ::Input:: *)
(*DataLength=Sqrt[500000];*)
(*{{1,Around[meannOver1,stdDevnOver1/DataLength]}*)
(*,{2,Around[meannOver2,stdDevnOver2/DataLength]}*)
(*,{3,Around[meannOver3,stdDevnOver3/DataLength]}*)
(*,{4,Around[meannOver4,stdDevnOver4/DataLength]}*)
(*,{5,Around[meannOver5,stdDevnOver5/DataLength]}*)
(*,{6,Around[meannOver6,stdDevnOver6/DataLength]}}*)
(*ListPlot[%]*)


(* ::Text:: *)
(*padding=n/4 seems the best here. Now checking with bigger systems*)


(* ::Chapter::Closed:: *)
(*Checking parameters with hybrid strategy (n=25)	TBD*)


(* ::Section:: *)
(*Check padding (b=1.0_LRW-3d-square-lattice-data-25_repeat-500000_tol-1e-10-HybridSq-LogSpacing-padding-nOver1.csv)		*)


(* ::Input:: *)
(*n=9;*)
(*(*1. Read as text and split by double carriage returns/newlines*)rawBlocks=Import["D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\3d\\b=1.0_LRW-3d-square-lattice-data-25_repeat-500000_tol-1e-10-HybridSq-LogSpacing-padding-nOver1.csv","CSV"];*)
(**)
(*(*2. Convert each separate text block cleanly into an array of coordinate pairs*)*)
(*paths=Developer`ToPackedArray[rawBlocks];*)
(*paths[[1;;10]]*)
(*Length[paths]*)


(* ::Input:: *)
(*paths=Pick[paths,Length/@paths,2];*)
(*Length[paths]*)


(* ::Input:: *)
(*pathDistrnOver1=paths[[All,2]];*)
(*Length@pathDistrnOver1*)


(* ::Input:: *)
(*Histogram[#,Length[#],"PDF",PlotRange->All]&@pathDistrnOver1*)
(*Histogram[#,Automatic(*(Length[#]/20)*),"PDF",PlotRange->All]&@pathDistrnOver1*)
(**)


(* ::Input:: *)
(*(*1. Calculate relative frequencies ("probabilities")*)*)
(*Sort@Tally[pathDistrnOver1];*)
(*{pathDistr2nOver1,symmetricCountsnOver1}=Transpose[%]*)


(* ::Input:: *)
(*pathDistr2nOver1==Sort@DeleteDuplicates@pathDistrnOver1*)


(* ::Input:: *)
(*meannOver1=Mean[pathDistrnOver1]//N *)
(*stdDevnOver1=StandardDeviation[pathDistrnOver1]//N*)


(* ::Input:: *)
(*totalPathsnOver1=Length[pathDistrnOver1];*)
(*symmetricProbabilitiesnOver1=symmetricCountsnOver1/totalPathsnOver1;*)
(**)


(* ::Input:: *)
(*(*3. Generate the BarChart with values on top and unique symmetry families on the bottom*)barChartnOver1=BarChart[symmetricProbabilitiesnOver1[[#]],ChartLabels->Placed[pathDistr2nOver1,Axis],LabelingFunction->(Placed[Row[{NumberForm[100.*#1,{5,2}],"%"}],Above]&),Frame->True,FrameLabel->{"Path Length","Relative Occurrence (Probability)"},PlotLabel->Row[{"Path Distribution nOver1"}],ChartStyle->RGBColor[0.87,0.71,0.34],ImageSize->900(*,PlotRange->{All,{0,Max[symmetricProbabilities]*1.15}}*)]&@Span[1,30]*)


(* ::Section:: *)
(*Check padding (b=1.0_LRW-3d-square-lattice-data-25_repeat-500000_tol-1e-10-HybridSq-LogSpacing-padding-nOver2.csv)		*)


(* ::Input:: *)
(*n=9;*)
(*(*1. Read as text and split by double carriage returns/newlines*)rawBlocks=Import["D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\3d\\b=1.0_LRW-3d-square-lattice-data-25_repeat-500000_tol-1e-10-HybridSq-LogSpacing-padding-nOver2.csv","CSV"];*)
(**)
(*(*2. Convert each separate text block cleanly into an array of coordinate pairs*)*)
(*paths=Developer`ToPackedArray[rawBlocks];*)
(*paths[[1;;10]]*)
(*Length[paths]*)


(* ::Input:: *)
(*paths=Pick[paths,Length/@paths,2];*)
(*Length[paths]*)


(* ::Input:: *)
(*pathDistrnOver2=paths[[All,2]];*)
(*Length@pathDistrnOver2*)


(* ::Input:: *)
(*Histogram[#,Length[#],"PDF",PlotRange->All]&@pathDistrnOver2*)
(*Histogram[#,Automatic(*(Length[#]/20)*),"PDF",PlotRange->All]&@pathDistrnOver2*)
(**)


(* ::Input:: *)
(*(*1. Calculate relative frequencies ("probabilities")*)*)
(*Sort@Tally[pathDistrnOver2];*)
(*{pathDistr2nOver2,symmetricCountsnOver2}=Transpose[%]*)


(* ::Input:: *)
(*pathDistr2nOver2==Sort@DeleteDuplicates@pathDistrnOver2*)


(* ::Input:: *)
(*meannOver2=Mean[pathDistrnOver2]//N *)
(**)
(*stdDevnOver2=StandardDeviation[pathDistrnOver2]//N*)


(* ::Input:: *)
(*totalPathsnOver2=Length[pathDistrnOver2];*)
(*symmetricProbabilitiesnOver2=symmetricCountsnOver2/totalPathsnOver2;*)
(**)


(* ::Input:: *)
(*(*3. Generate the BarChart with values on top and unique symmetry families on the bottom*)barChartnOver2=BarChart[symmetricProbabilitiesnOver2[[#]],ChartLabels->Placed[pathDistr2nOver2,Axis],LabelingFunction->(Placed[Row[{NumberForm[100.*#1,{5,2}],"%"}],Above]&),Frame->True,FrameLabel->{"Path Length","Relative Occurrence (Probability)"},PlotLabel->Row[{"Path Distribution nOver2"}],ChartStyle->RGBColor[0.87,0.71,0.34],ImageSize->900(*,PlotRange->{All,{0,Max[symmetricProbabilities]*1.15}}*)]&@Span[1,30]*)


(* ::Section:: *)
(*Check padding (b=1.0_LRW-3d-square-lattice-data-25_repeat-500000_tol-1e-10-HybridSq-LogSpacing-padding-nOver3.csv)		*)


(* ::Input:: *)
(*n=9;*)
(*(*1. Read as text and split by double carriage returns/newlines*)rawBlocks=Import["D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\3d\\b=1.0_LRW-3d-square-lattice-data-25_repeat-500000_tol-1e-10-HybridSq-LogSpacing-padding-nOver3.csv","CSV"];*)
(**)
(*(*2. Convert each separate text block cleanly into an array of coordinate pairs*)*)
(*paths=Developer`ToPackedArray[rawBlocks];*)
(*paths[[1;;10]]*)
(*Length[paths]*)
(*paths=Pick[paths,Length/@paths,2];*)
(*Length[paths]*)


(* ::Input:: *)
(*pathDistrnOver3=paths[[All,2]];*)
(*Length@pathDistrnOver3*)


(* ::Input:: *)
(*Histogram[#,Length[#],"PDF",PlotRange->All]&@pathDistrnOver3*)
(*Histogram[#,Automatic(*(Length[#]/20)*),"PDF",PlotRange->All]&@pathDistrnOver3*)
(**)


(* ::Input:: *)
(*(*1. Calculate relative frequencies ("probabilities")*)*)
(*Sort@Tally[pathDistrnOver3];*)
(*{pathDistr2nOver3,symmetricCountsnOver3}=Transpose[%]*)


(* ::Input:: *)
(*pathDistr2nOver3==Sort@DeleteDuplicates@pathDistrnOver3*)


(* ::Input:: *)
(*meannOver3=Mean[pathDistrnOver3]//N *)
(*stdDevnOver3=StandardDeviation[pathDistrnOver3]//N*)


(* ::Input:: *)
(*totalPathsnOver3=Length[pathDistrnOver3];*)
(*symmetricProbabilitiesnOver3=symmetricCountsnOver3/totalPathsnOver3;*)
(**)


(* ::Input:: *)
(*(*3. Generate the BarChart with values on top and unique symmetry families on the bottom*)barChartnOver3=BarChart[symmetricProbabilitiesnOver3[[#]],ChartLabels->Placed[pathDistr2nOver3,Axis],LabelingFunction->(Placed[Row[{NumberForm[100.*#1,{5,2}],"%"}],Above]&),Frame->True,FrameLabel->{"Path Length","Relative Occurrence (Probability)"},PlotLabel->Row[{"Path Distribution nOver3"}],ChartStyle->RGBColor[0.87,0.71,0.34],ImageSize->900(*,PlotRange->{All,{0,Max[symmetricProbabilities]*1.15}}*)]&@Span[1,30]*)


(* ::Section:: *)
(*Check padding (b=1.0_LRW-3d-square-lattice-data-25_repeat-500000_tol-1e-10-HybridSq-LogSpacing-padding-nOver4.csv)		*)


(* ::Input:: *)
(*n=9;*)
(*(*1. Read as text and split by double carriage returns/newlines*)rawBlocks=Import["D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\3d\\b=1.0_LRW-3d-square-lattice-data-25_repeat-500000_tol-1e-10-HybridSq-LogSpacing-padding-nOver4.csv","CSV"];*)
(**)
(*(*2. Convert each separate text block cleanly into an array of coordinate pairs*)*)
(*paths=Developer`ToPackedArray[rawBlocks];*)
(*paths[[1;;10]]*)
(*Length[paths]*)
(*paths=Pick[paths,Length/@paths,2];*)
(*Length[paths]*)


(* ::Input:: *)
(*pathDistrnOver4=paths[[All,2]];*)
(*Length@pathDistrnOver4*)


(* ::Input:: *)
(*Histogram[#,Length[#],"PDF",PlotRange->All]&@pathDistrnOver4*)
(*Histogram[#,Automatic(*(Length[#]/20)*),"PDF",PlotRange->All]&@pathDistrnOver4*)
(**)


(* ::Input:: *)
(*(*1. Calculate relative frequencies ("probabilities")*)*)
(*Sort@Tally[pathDistrnOver4];*)
(*{pathDistr2nOver4,symmetricCountsnOver4}=Transpose[%]*)


(* ::Input:: *)
(*pathDistr2nOver4==Sort@DeleteDuplicates@pathDistrnOver4*)


(* ::Input:: *)
(*meannOver4=Mean[pathDistrnOver4]//N *)
(*stdDevnOver4=StandardDeviation[pathDistrnOver4]//N*)


(* ::Input:: *)
(*totalPathsnOver4=Length[pathDistrnOver4];*)
(*symmetricProbabilitiesnOver4=symmetricCountsnOver4/totalPathsnOver4;*)
(**)


(* ::Input:: *)
(*(*3. Generate the BarChart with values on top and unique symmetry families on the bottom*)barChartnOver4=BarChart[symmetricProbabilitiesnOver4[[#]],ChartLabels->Placed[pathDistr2nOver4,Axis],LabelingFunction->(Placed[Row[{NumberForm[100.*#1,{5,2}],"%"}],Above]&),Frame->True,FrameLabel->{"Path Length","Relative Occurrence (Probability)"},PlotLabel->Row[{"Path Distribution nOver4"}],ChartStyle->RGBColor[0.87,0.71,0.34],ImageSize->900(*,PlotRange->{All,{0,Max[symmetricProbabilities]*1.15}}*)]&@Span[1,30]*)


(* ::Section:: *)
(*Check padding (b=1.0_LRW-3d-square-lattice-data-25_repeat-500000_tol-1e-10-HybridSq-LogSpacing-padding-nOver5.csv)		*)


(* ::Input:: *)
(*n=9;*)
(*(*1. Read as text and split by double carriage returns/newlines*)rawBlocks=Import["D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\3d\\b=1.0_LRW-3d-square-lattice-data-25_repeat-500000_tol-1e-10-HybridSq-LogSpacing-padding-nOver5.csv","CSV"];*)
(**)
(*(*2. Convert each separate text block cleanly into an array of coordinate pairs*)*)
(*paths=Developer`ToPackedArray[rawBlocks];*)
(*paths[[1;;10]]*)
(*Length[paths]*)
(*paths=Pick[paths,Length/@paths,2];*)
(*Length[paths]*)


(* ::Input:: *)
(*pathDistrnOver5=paths[[All,2]];*)
(*Length@pathDistrnOver5*)


(* ::Input:: *)
(*Histogram[#,Length[#],"PDF",PlotRange->All]&@pathDistrnOver5*)
(*Histogram[#,Automatic(*(Length[#]/20)*),"PDF",PlotRange->All]&@pathDistrnOver5*)
(**)


(* ::Input:: *)
(*(*1. Calculate relative frequencies ("probabilities")*)*)
(*Sort@Tally[pathDistrnOver5];*)
(*{pathDistr2nOver5,symmetricCountsnOver5}=Transpose[%]*)


(* ::Input:: *)
(*pathDistr2nOver5==Sort@DeleteDuplicates@pathDistrnOver5*)


(* ::Input:: *)
(*meannOver5=Mean[pathDistrnOver5]//N *)
(*stdDevnOver5=StandardDeviation[pathDistrnOver5]//N*)


(* ::Input:: *)
(*totalPathsnOver5=Length[pathDistrnOver5];*)
(*symmetricProbabilitiesnOver5=symmetricCountsnOver5/totalPathsnOver5;*)
(**)


(* ::Input:: *)
(*(*3. Generate the BarChart with values on top and unique symmetry families on the bottom*)barChartnOver5=BarChart[symmetricProbabilitiesnOver5[[#]],ChartLabels->Placed[pathDistr2nOver5,Axis],LabelingFunction->(Placed[Row[{NumberForm[100.*#1,{5,2}],"%"}],Above]&),Frame->True,FrameLabel->{"Path Length","Relative Occurrence (Probability)"},PlotLabel->Row[{"Path Distribution nOver5"}],ChartStyle->RGBColor[0.87,0.71,0.34],ImageSize->900(*,PlotRange->{All,{0,Max[symmetricProbabilities]*1.15}}*)]&@Span[1,30]*)


(* ::Section:: *)
(*Check padding (b=1.0_LRW-3d-square-lattice-data-25_repeat-500000_tol-1e-10-HybridSq-LogSpacing-padding-nOver6.csv)		*)


(* ::Input:: *)
(*n=9;*)
(*(*1. Read as text and split by double carriage returns/newlines*)rawBlocks=Import["D:\\Offline_Documents\\University\\PhD_Paris\\PhD_work\\Simulations\\bLRW\\b-LRWdata\\3d\\b=1.0_LRW-3d-square-lattice-data-25_repeat-500000_tol-1e-10-HybridSq-LogSpacing-padding-nOver6.csv","CSV"];*)
(**)
(*(*2. Convert each separate text block cleanly into an array of coordinate pairs*)*)
(*paths=Developer`ToPackedArray[rawBlocks];*)
(*paths[[1;;10]]*)
(*Length[paths]*)
(*paths=Pick[paths,Length/@paths,2];*)
(*Length[paths]*)


(* ::Input:: *)
(*pathDistrnOver6=paths[[All,2]];*)
(*Length@pathDistrnOver6*)


(* ::Input:: *)
(*Histogram[#,Length[#],"PDF",PlotRange->All]&@pathDistrnOver6*)
(*Histogram[#,Automatic(*(Length[#]/20)*),"PDF",PlotRange->All]&@pathDistrnOver6*)
(**)


(* ::Input:: *)
(*(*1. Calculate relative frequencies ("probabilities")*)*)
(*Sort@Tally[pathDistrnOver6];*)
(*{pathDistr2nOver6,symmetricCountsnOver6}=Transpose[%]*)


(* ::Input:: *)
(*pathDistr2nOver6==Sort@DeleteDuplicates@pathDistrnOver6*)


(* ::Input:: *)
(*meannOver6=Mean[pathDistrnOver6]//N *)
(*stdDevnOver6=StandardDeviation[pathDistrnOver6]//N*)


(* ::Input:: *)
(*totalPathsnOver6=Length[pathDistrnOver6];*)
(*symmetricProbabilitiesnOver6=symmetricCountsnOver6/totalPathsnOver6;*)
(**)


(* ::Input:: *)
(*(*3. Generate the BarChart with values on top and unique symmetry families on the bottom*)barChartnOver6=BarChart[symmetricProbabilitiesnOver6[[#]],ChartLabels->Placed[pathDistr2nOver6,Axis],LabelingFunction->(Placed[Row[{NumberForm[100.*#1,{5,2}],"%"}],Above]&),Frame->True,FrameLabel->{"Path Length","Relative Occurrence (Probability)"},PlotLabel->Row[{"Path Distribution nOver6"}],ChartStyle->RGBColor[0.87,0.71,0.34],ImageSize->900(*,PlotRange->{All,{0,Max[symmetricProbabilities]*1.15}}*)]&@Span[1,30]*)


(* ::Section:: *)
(*Comparison*)


(* ::Input:: *)
(*Multicolumn[{barChartnOver1*)
(*,barChartnOver2*)
(*,barChartnOver3*)
(*,barChartnOver4*)
(*,barChartnOver5*)
(*,barChartnOver6}/.{(ImageSize->a_)->(ImageSize->Medium),Span[All]->Span[1,30]},3]*)


(* ::Input:: *)
(*DataLength=Sqrt[500000];*)
(*{{1,Around[meannOver1,stdDevnOver1/DataLength]}*)
(*,{2,Around[meannOver2,stdDevnOver2/DataLength]}*)
(*,{3,Around[meannOver3,stdDevnOver3/DataLength]}*)
(*,{4,Around[meannOver4,stdDevnOver4/DataLength]}*)
(*,{5,Around[meannOver5,stdDevnOver5/DataLength]}*)
(*,{6,Around[meannOver6,stdDevnOver6/DataLength]}}*)
(*ListPlot[%]*)


(* ::Text:: *)
(*padding=n/4 seems the best here. Now checking with bigger systems*)


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


(* ::Subsection::Closed:: *)
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


(* ::Subsection::Closed:: *)
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


(* ::Section::Closed:: *)
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


(* ::Item::Closed:: *)
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

tempProb=1/denominator locWeights[[Position[locVertices,path[[i,1]]][[1,1]],Position[locVertices,path[[i,2]]][[1,1]]]]*(Select[\[Phi]sol,#[[1]]==\[CapitalPhi][path[[i,2]]] &][[1,2]])^b//FullSimplify;

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


(* ::Item::Closed:: *)
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


(* ::Item::Closed:: *)
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
(Log[x^2+y^2 ](*/Log[x^2+y^2 ]*)//N)]];
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

tempProb=1/denominator locWeights[[Position[locVertices,path[[i,1]]][[1,1]],Position[locVertices,path[[i,2]]][[1,1]]]]*(Select[\[Phi]sol,#[[1]]==\[CapitalPhi][path[[i,2]]] &][[1,2]])^b//FullSimplify;

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


(* ::Item::Closed:: *)
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
(*bLaplacianRW3dBC[]*)


(* ::Text:: *)
(*Computes the probability of a given bLaplacian RW*)


(* ::Input::Initialization:: *)
ClearAll[bLaplacianRW3dBC];
Options[bLaplacianRW3dBC]={"draw"->False,"print"->tTrue};

(*bLaplacianRW3dBC[graph_,path__,options:OptionsPattern[]]:=bLaplacianRW3dBC[graph,path,options,1,n];*)

bLaplacianRW3dBC[graph_,path__,OptionsPattern[],b_:1,n_:n]:=Module[{locSource,locRoot,locWeights,locVertices,
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
If[!MemberQ[locSource,path[[-1,2]]], Print[Style["####  WRONG TARGET  ####",RGBColor[1, 0, 0]]];Return[NULL]];

Module[{locBC={{},{}},i,\[Phi]sol},
If[Head[locSource]==List,
For[i=1,i<=Length[locSource],i++,
AppendTo[locBC[[1]],\[CapitalPhi][locSource[[i]]]== 
With[{x=Floor[(locSource[[i]]-1)/n^2]-Floor[n/2],y=Mod[Floor[(locSource[[i]]-1)/n],n] -(Floor[n/2]),z=Mod[(locSource[[i]]-1),n] -(Floor[n/2])},
(Floor[n/2]/Sqrt[x^2+y^2+z^2]*(Sqrt[x^2+y^2+z^2]-0.9)/(Floor[n/2]-0.9))]
];
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

tempProb=1/denominator locWeights[[Position[locVertices,path[[i,1]]][[1,1]],Position[locVertices,path[[i,2]]][[1,1]]]]*(Select[\[Phi]sol,#[[1]]==\[CapitalPhi][path[[i,2]]] &][[1,2]])^b//FullSimplify;

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


(* ::Section::Closed:: *)
(*Application to Square Lattice*)


(* ::Subsection::Closed:: *)
(*L=1, n=3*)


(* ::Input:: *)
(*Graph[vertices,properEdges,VertexLabels->"Name"]*)


(* ::Subsubsection::Closed:: *)
(*Graph def*)


(* ::Input:: *)
(*L=1;*)
(*n=2*L+1;*)
(*(**)
(*edges = Flatten[Table[Table[{{{x,y},{x,y+1}},{{x,y},{x+1,y}}},{x,-Floor[n/2],Floor[n/2]}],{y,-Floor[n/2],Floor[n/2]}],2];*)
(**)*)
(* Flatten[*)
(*Table[*)
(*Table[*)
(*Table[*)
(*With[{i=x*n*n+y*n+z+1},*)
(*{If[z==n-1,{},{i,i+1}],If[y==n-1,{},{i,i+n}],If[x==n-1,{},{i,i+n*n}]}]*)
(*,{x,0,n-1}]*)
(*,{y,0,n-1}]*)
(*,{z,0,n-1}]*)
(*,2];*)
(*DeleteCases[%,{},All];*)
(*(*DeleteCases[%,{},All]*)*)
(*Flatten[%,1];*)
(**)
(*edges =%;(*Select[%,#[[1]]<n*n+1&&#[[2]]<n*n+1&]*)*)
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
(*sourceVertices=Sort[Select[vertices,*)
(*With[{x=Floor[(#-1)/n^2]-Floor[n/2],y=Mod[Floor[(#-1)/n],n] -(Floor[n/2]),z=Mod[(#-1),n] -(Floor[n/2])},*)
(*(Sqrt[x^2+y^2+z^2]>=(Floor[n/2]))]&]];*)
(*(*myLabels=Flatten[{Map[#->"SOURCE"&,sourceEdges],{0,0}->"ROOT","Name"},1];*)
(**)*)
(*myLabels=Flatten[{Map[#->"SOURCE"&,sourceVertices],(Floor[n/2]*(n^2))+Floor[n/2]*n+Floor[n/2]+1->"ROOT","Name"},1];*)
(*Graph[vertices,properEdges,VertexLabels->"Name"];*)
(*g=Graph[vertices,properEdges,VertexLabels->myLabels]*)


(* ::Subsubsection::Closed:: *)
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


(* ::Subsubsection::Closed:: *)
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
(*L=2, n=5*)


(* ::Input:: *)
(*Graph[vertices,properEdges,VertexLabels->"",ImageSize->Large]*)


(* ::Subsubsection::Closed:: *)
(*Graph def*)


(* ::Input:: *)
(*Floor[n/2]*)


(* ::Input:: *)
(*L=2;*)
(*n=2*L+1;*)
(*(**)
(*edges = Flatten[Table[Table[{{{x,y},{x,y+1}},{{x,y},{x+1,y}}},{x,-Floor[n/2],Floor[n/2]}],{y,-Floor[n/2],Floor[n/2]}],2];*)
(**)*)
(* Flatten[*)
(*Table[*)
(*Table[*)
(*Table[*)
(*With[{i=x*n*n+y*n+z+1},*)
(*{If[z==n-1,{},{i,i+1}],If[y==n-1,{},{i,i+n}],If[x==n-1,{},{i,i+n*n}]}]*)
(*,{x,0,n-1}]*)
(*,{y,0,n-1}]*)
(*,{z,0,n-1}]*)
(*,2];*)
(*DeleteCases[%,{},All];*)
(*(*DeleteCases[%,{},All]*)*)
(*Flatten[%,1];*)
(**)
(*edges =%;*)
(*Length@%*)
(*(*Select[%,#[[1]]<n*n+1&&#[[2]]<n*n+1&]*)*)
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
(*sourceVertices=Sort[Select[vertices,*)
(*With[{x=Floor[(#-1)/n^2]-Floor[n/2],y=Mod[Floor[(#-1)/n],n] -(Floor[n/2]),z=Mod[(#-1),n] -(Floor[n/2])},*)
(*(Sqrt[x^2+y^2+z^2]>=(Floor[n/2]))]&]];*)
(*(*myLabels=Flatten[{Map[#->"SOURCE"&,sourceEdges],{0,0}->"ROOT","Name"},1];*)
(**)*)
(*myLabels=Flatten[{Map[#->"SOURCE"&,sourceVertices],(Floor[n/2]*(n^2))+Floor[n/2]*n+Floor[n/2]+1->"ROOT","Name"},1];*)
(*Graph[vertices,properEdges,VertexLabels->"Name"];*)
(*g=Graph[vertices,properEdges,VertexLabels->myLabels]*)


(* ::Subsubsection::Closed:: *)
(*b=1*)


(* ::Input:: *)
(*(Floor[n/2]*(n^2))+Floor[n/2]*n+Floor[n/2]+1*)


(* ::Item::Closed:: *)
(*Linear path: {63,64,65}*)


(* ::Subitem::Closed:: *)
(*bLaplacianRW standard BC*)


(* ::Input:: *)
(*{{63,64,65}};*)
(*%//.{d___,{a_,b_,c___}}:>{d,{a,b},{b,c}};*)
(*path=Most[%]*)
(*(bLaplacianRW[g,path]//N)*100*)
(*Print[Row[{"Accounting for symmetry: ",%*6,"%"}]]*)


(* ::Subitem::Closed:: *)
(*bLaplacianRW3dBC standard BC*)


(* ::Input:: *)
(*{{63,64,65}};*)
(*%//.{d___,{a_,b_,c___}}:>{d,{a,b},{b,c}};*)
(*path=Most[%]*)
(*(bLaplacianRW3dBC[g,path,"print"->True,1,n]//N)*100*)
(*Print[Row[{"Accounting for symmetry: ",%*6,"%"}]]*)


(* ::Subsection:: *)
(*L=3, n=7*)


(* ::Input:: *)
(*Graph[vertices,properEdges,VertexLabels->"Name",ImageSize->Full]*)


(* ::Subsubsection::Closed:: *)
(*Graph def*)


(* ::Input:: *)
(*L=3;*)
(*n=2*L+1;*)
(*(**)
(*edges = Flatten[Table[Table[{{{x,y},{x,y+1}},{{x,y},{x+1,y}}},{x,-Floor[n/2],Floor[n/2]}],{y,-Floor[n/2],Floor[n/2]}],2];*)
(**)*)
(* Flatten[*)
(*Table[*)
(*Table[*)
(*Table[*)
(*With[{i=x*n*n+y*n+z+1},*)
(*{If[z==n-1,{},{i,i+1}],If[y==n-1,{},{i,i+n}],If[x==n-1,{},{i,i+n*n}]}]*)
(*,{x,0,n-1}]*)
(*,{y,0,n-1}]*)
(*,{z,0,n-1}]*)
(*,2];*)
(*DeleteCases[%,{},All];*)
(*(*DeleteCases[%,{},All]*)*)
(*Flatten[%,1];*)
(**)
(*edges =%;(*Select[%,#[[1]]<n*n+1&&#[[2]]<n*n+1&]*)*)
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
(*sourceVertices=Sort[Select[vertices,*)
(*With[{x=Floor[(#-1)/n^2]-Floor[n/2],y=Mod[Floor[(#-1)/n],n] -(Floor[n/2]),z=Mod[(#-1),n] -(Floor[n/2])},*)
(*(Sqrt[x^2+y^2+z^2]>=(Floor[n/2]))]&]];*)
(*(*myLabels=Flatten[{Map[#->"SOURCE"&,sourceEdges],{0,0}->"ROOT","Name"},1];*)
(**)*)
(*myLabels=Flatten[{Map[#->"SOURCE"&,sourceVertices],(Floor[n/2]*(n^2))+Floor[n/2]*n+Floor[n/2]+1->"ROOT","Name"},1];*)
(*Graph[vertices,properEdges,VertexLabels->"Name"];*)


(* ::Input:: *)
(*(Floor[n/2]*(n^2))+Floor[n/2]*n+Floor[n/2]+1*)


(* ::Input:: *)
(*g=Graph[vertices,properEdges,VertexLabels->myLabels,ImageSize->Large]*)


(* ::Subsubsection:: *)
(*b=1*)


(* ::Input:: *)
(*(Floor[n/2]*(n^2))+Floor[n/2]*n+Floor[n/2]+1*)


(* ::Item:: *)
(*Linear path:{{172, 173, 174, 175}};*)


(* ::Subitem::Closed:: *)
(*bLaplacianRW standard BC*)


(* ::Input:: *)
(*{{63,64,65}};*)
(*%//.{d___,{a_,b_,c___}}:>{d,{a,b},{b,c}};*)
(*path=Most[%]*)
(*(bLaplacianRW[g,path]//N)*100*)
(*Print[Row[{"Accounting for symmetry: ",%*6,"%"}]]*)


(* ::Subitem:: *)
(*bLaplacianRW3dBC standard BC*)


(* ::Input:: *)
(*{{172,173,174,175}};;*)
(*%//.{d___,{a_,b_,c___}}:>{d,{a,b},{b,c}};*)
(*path=Most[%]*)
(*(bLaplacianRW3dBC[g,path,"print"->tTrue,1,n]//N)*100*)
(*Print[Row[{"Accounting for symmetry: ",%*6,"%"}]]*)


(* ::Subsection::Closed:: *)
(*To be used with LogBC*)


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
(*threshold=(Floor[n/2]+2);*)
(*sourceThreshold=(Floor[n/2]-0.);*)
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


(* ::Subsection::Closed:: *)
(*Linear path (length=3) with bLaplacianRWlogBC[]*)


(* ::Subsubsection::Closed:: *)
(*b=0.5*)


(* ::Input:: *)
(*{{25,26,27,28}};*)
(*%//.{d___,{a_,b_,c___}}:>{d,{a,b},{b,c}};*)
(*path=Most[%]*)
(*bLaplacianRWlogBC[g,path,0.5]*)
(*Row[{"Single path",path,": ",N[%]*100,"%"}]*)
(**)
(*Row[{"Symmetric paths: ",N[%%]*100*4,"%"}]*)


(* ::Item::Closed:: *)
(*LittleL path: {{25,26,27,20,Source}}*)


(* ::Input:: *)
(*{{25,26,27,20,21}};*)
(*%//.{d___,{a_,b_,c___}}:>{d,{a,b},{b,c}};*)
(*path=Most[%]*)
(*(bLaplacianRWlogBC[g,path,0.5]//N)*100*)
(*Print[Row[{"Accounting for symmetry: ",l4p1=%*4*2,"%"}]]*)


(* ::Input:: *)
(*{{25,26,19,20,21}};*)
(*%//.{d___,{a_,b_,c___}}:>{d,{a,b},{b,c}};*)
(*path=Most[%]*)
(*(bLaplacianRWlogBC[g,path,0.5]//N)*100*)
(*Print[Row[{"Accounting for symmetry: ",l4p2=%*4*2,"%"}]]*)


(* ::Input:: *)
(*{{25,26,19,12,5}};*)
(*%//.{d___,{a_,b_,c___}}:>{d,{a,b},{b,c}};*)
(*path=Most[%]*)
(*(bLaplacianRWlogBC[g,path,0.5]//N)*100*)
(*Print[Row[{"Accounting for symmetry: ",l4p3=%*4*2,"%"}]]*)


(* ::Input:: *)
(*l4p1+l4p2+l4p3*)


(* ::Subsubsection::Closed:: *)
(*b=1*)


(* ::Input:: *)
(*{{25,26,27,28}};*)
(*%//.{d___,{a_,b_,c___}}:>{d,{a,b},{b,c}};*)
(*path=Most[%](*{{113,114},{114,115},{115,116},{116,117},{117,118},{118,119},{119,120}};*)*)
(*bLaplacianRWlogBC[g,path]*)
(*Row[{"Single path",path,": ",N[%]*100,"%"}]*)
(**)
(*Row[{"Symmetric paths: ",N[%%]*100*4,"%"}]*)


(* ::Text:: *)
(*Compared to Symmetric paths: 17.73% from the BC=1 everywhere!!!!!*)


(* ::Item::Closed:: *)
(*LittleL path: {{25,26,27,20,Source}}*)


(* ::Input:: *)
(*{{25,26,27,20,21}};*)
(*%//.{d___,{a_,b_,c___}}:>{d,{a,b},{b,c}};*)
(*path=Most[%]*)
(*(bLaplacianRWlogBC[g,path]//N)*100*)
(*Print[Row[{"Accounting for symmetry: ",l4p1=%*4*2,"%"}]]*)


(* ::Input:: *)
(*{{25,26,19,20,21}};*)
(*%//.{d___,{a_,b_,c___}}:>{d,{a,b},{b,c}};*)
(*path=Most[%]*)
(*(bLaplacianRWlogBC[g,path]//N)*100*)
(*Print[Row[{"Accounting for symmetry: ",l4p2=%*4*2,"%"}]]*)


(* ::Input:: *)
(*{{25,26,19,12,5}};*)
(*%//.{d___,{a_,b_,c___}}:>{d,{a,b},{b,c}};*)
(*path=Most[%]*)
(*(bLaplacianRWlogBC[g,path]//N)*100*)
(*Print[Row[{"Accounting for symmetry: ",l4p3=%*4*2,"%"}]]*)


(* ::Input:: *)
(*l4p1+l4p2+l4p3*)


(* ::Subsection:: *)
(*L=6, n=13 TOO BIG, IMPOSSIBLE TO COMPUTE*)


(* ::Input:: *)
(*Graph[vertices,properEdges,VertexLabels->"Name"]*)


(* ::Input:: *)
(*(Floor[n/2]*(n^2))+Floor[n/2]*n+Floor[n/2]+1*)


(* ::Subsubsection::Closed:: *)
(*Graph def*)


(* ::Input:: *)
(*L=6;*)
(*n=2*L+1;*)
(*(**)
(*edges = Flatten[Table[Table[{{{x,y},{x,y+1}},{{x,y},{x+1,y}}},{x,-Floor[n/2],Floor[n/2]}],{y,-Floor[n/2],Floor[n/2]}],2];*)
(**)*)
(* Flatten[*)
(*Table[*)
(*Table[*)
(*Table[*)
(*With[{i=x*n*n+y*n+z+1},*)
(*{If[z==n-1,{},{i,i+1}],If[y==n-1,{},{i,i+n}],If[x==n-1,{},{i,i+n*n}]}]*)
(*,{x,0,n-1}]*)
(*,{y,0,n-1}]*)
(*,{z,0,n-1}]*)
(*,2];*)
(*DeleteCases[%,{},All];*)
(*(*DeleteCases[%,{},All]*)*)
(*Flatten[%,1];*)
(**)
(*edges =%;(*Select[%,#[[1]]<n*n+1&&#[[2]]<n*n+1&]*)*)
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
(*sourceVertices=Sort[Select[vertices,*)
(*With[{x=Floor[(#-1)/n^2]-Floor[n/2],y=Mod[Floor[(#-1)/n],n] -(Floor[n/2]),z=Mod[(#-1),n] -(Floor[n/2])},*)
(*(Sqrt[x^2+y^2+z^2]>=(Floor[n/2]))]&]];*)
(*(*myLabels=Flatten[{Map[#->"SOURCE"&,sourceEdges],{0,0}->"ROOT","Name"},1];*)
(**)*)
(*myLabels=Flatten[{Map[#->"SOURCE"&,sourceVertices],(Floor[n/2]*(n^2))+Floor[n/2]*n+Floor[n/2]+1->"ROOT","Name"},1];*)
(*Graph[vertices,properEdges,VertexLabels->"Name"];*)
(*g=Graph[vertices,properEdges,VertexLabels->myLabels]*)


(* ::Subsubsection:: *)
(*Linear path of length 6*)


(* ::Input:: *)
(*{{1099,1100(*,1101,1102,1103,1104*),1105}};*)
(*%//.{d___,{a_,b_,c___}}:>{d,{a,b},{b,c}};*)
(*path=Most[%]*)
(*(bLaplacianRW3dBC[g,path,"print"->True,1,n]//N)*100*)
(*Print[Row[{"Accounting for symmetry: ",%*6,"%"}]]*)


(* ::Subsection::Closed:: *)
(*L=7, n=15*)


(* ::Input:: *)
(*Graph[vertices,properEdges,VertexLabels->"Name"]*)


(* ::Subsubsection:: *)
(*Graph def*)


(* ::Input:: *)
(*L=7;*)
(*n=2*L+1;*)
(*(**)
(*edges = Flatten[Table[Table[{{{x,y},{x,y+1}},{{x,y},{x+1,y}}},{x,-Floor[n/2],Floor[n/2]}],{y,-Floor[n/2],Floor[n/2]}],2];*)
(**)*)
(* Flatten[*)
(*Table[*)
(*Table[*)
(*Table[*)
(*With[{i=x*n*n+y*n+z+1},*)
(*{If[z==n-1,{},{i,i+1}],If[y==n-1,{},{i,i+n}],If[x==n-1,{},{i,i+n*n}]}]*)
(*,{x,0,n-1}]*)
(*,{y,0,n-1}]*)
(*,{z,0,n-1}]*)
(*,2];*)
(*DeleteCases[%,{},All];*)
(*(*DeleteCases[%,{},All]*)*)
(*Flatten[%,1];*)
(**)
(*edges =%;(*Select[%,#[[1]]<n*n+1&&#[[2]]<n*n+1&]*)*)
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
(*sourceVertices=Sort[Select[vertices,*)
(*With[{x=Floor[(#-1)/n^2]-Floor[n/2],y=Mod[Floor[(#-1)/n],n] -(Floor[n/2]),z=Mod[(#-1),n] -(Floor[n/2])},*)
(*(Sqrt[x^2+y^2+z^2]>=(Floor[n/2]))]&]];*)
(*(*myLabels=Flatten[{Map[#->"SOURCE"&,sourceEdges],{0,0}->"ROOT","Name"},1];*)
(**)*)
(*myLabels=Flatten[{Map[#->"SOURCE"&,sourceVertices],(Floor[n/2]*(n^2))+Floor[n/2]*n+Floor[n/2]+1->"ROOT","Name"},1];*)
(*Graph[vertices,properEdges,VertexLabels->"Name"];*)
(*g=Graph[vertices,properEdges,VertexLabels->myLabels]*)


(* ::Subsubsection::Closed:: *)
(*Linear path: {{Root,12,Source}}*)


(* ::Input:: *)
(*{{25,26,27,28}};*)
(*%//.{d___,{a_,b_,c___}}:>{d,{a,b},{b,c}};*)
(*path=Most[%]*)
(*LaplacianRW[g,path]*)
(*Row[{"Single path",path,": ",N[%]*100,"%"}]*)
(**)
(*Row[{"Symmetric paths: ",N[%%]*100*6,"%"}]*)


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


(* ::Subsubsection::Closed:: *)
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
