(* ::Package:: *)

(* ::Title:: *)
(*Diagrams computed on the Lattice*)


(* ::Text:: *)
(*Useful relation*)


Assuming[a < 0 && b < 0 && x > 0
 , Integrate[(y)^(2 a) Abs[x - y]^(2 b), {y, 0, \[Infinity]}]]


(* ::Section:: *)
(*Hat*)


Assuming[\[Epsilon] > 0 && 
  x > 0 && \[CapitalLambda] > a > 0 && \[CapitalLambda] > 100,
 int = Integrate[
   x y ((Abs[y - x])^(2 (\[Epsilon] - 2))), {y, 
    a, \[CapitalLambda]}, {x, a, \[CapitalLambda]}](*;
 Series[(\[Epsilon]-2)^2 int,{x,a,6}]//Normal*)
 ]


Series[int/(\[CapitalLambda]^(2 \[Epsilon]) - a^(
    2 \[Epsilon])), {\[Epsilon], 
  0, -1}(*,{\[CapitalLambda],\[Infinity],0}*)]
%/((a^2 + \[CapitalLambda]^2)/ (Log[a] - Log[\[CapitalLambda]]) )
Normal /@ Series[%, {a, 0, 0}(*,{\[CapitalLambda],\[Infinity],0}*)]


(* ::Section:: *)
(*{Sunset (it should be , Cell[FormBox[FractionBox[RowBox[{-, 1}], RowBox[{8, ϵ}]], TraditionalForm]], )}*)


Assuming[\[Epsilon] > 0 && 
  x > 0 && \[CapitalLambda] > a > 0 && \[CapitalLambda] > 100,
 int = Integrate[x ^(2 \[Epsilon] - 3), {x, a, \[CapitalLambda]}](*;
 Series[(\[Epsilon]-2)^2 int,{x,a,6}]//Normal*)
 ]


Series[int/(\[CapitalLambda]^(2 \[Epsilon]) - a^(
    2 \[Epsilon])), {\[Epsilon], 
  0, -1}(*,{\[CapitalLambda],\[Infinity],0}*)]
%/((-(1/a^2) + 
   1/\[CapitalLambda]^2)/(-Log[a] + Log[\[CapitalLambda]])) // FS
(*Normal/@*)Series[%, {a, 
  0, -1}(*,{\[CapitalLambda],\[Infinity],0}*)]
Series[%, {\[CapitalLambda], \[Infinity], 0}](*//Normal*)


(* ::Section:: *)
(*Hat with derivatives*)


Assuming[\[Epsilon] > 0 && 
  x > 0 && \[CapitalLambda] > a > 0 && \[CapitalLambda] > 100,
 int = Integrate[
   y ((Abs[y - x])^(\[Epsilon] - 2)), {y, a, \[CapitalLambda]}, {x, 
    a, \[CapitalLambda]}](*;
 Series[(\[Epsilon]-2)^2 int,{x,a,6}]//Normal*)
 ]


Series[int/(\[CapitalLambda]^(2 \[Epsilon]) - a^(
    2 \[Epsilon])), {\[Epsilon], 
  0, -1}(*,{\[CapitalLambda],\[Infinity],0}*)]
%/((a^2 + \[CapitalLambda]^2)/ (Log[a] - Log[\[CapitalLambda]]) )
Normal /@ Series[%, {a, 0, 0}(*,{\[CapitalLambda],\[Infinity],0}*)]