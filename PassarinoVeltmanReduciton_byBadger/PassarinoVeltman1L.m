(* ::Package:: *)

SetAttributes[dot,Orderless]


ExpandDot = {
dot[0,0]->0,
dot[Plus[a_,b__],c_]:>dot[a,c]+dot[Plus[b],c],
dot[Times[a_?NumericQ,b__],c_]:>a*dot[Times[b],c],
dot[Times[a_aa,b__],c_]:>a*dot[Times[b],c] (* asume aa always represents constant, like a Feynman parameter *)
};


LorentzContract = {
dot[a_mu,b_]*dot[a_mu,c_]:>dot[b,c],
dot[a_mu,b_]^2:>dot[b,b],
dot[a_mu,a_mu]:>d
};


CollectIntegrals[expr_]:=Module[{tmp},
tmp = expr //. INT[n_,dens_List]/prop[x_,m_]:>INT[n,Transpose@Join[Transpose[dens],{{x,m}}]];
tmp = tmp //. INT[n_,dens_List]*prop[x_,m_]:>(INT[n*(dot[x,x]-m^2),dens]);
tmp = tmp //. INT[n_,dens_List]*Power[prop[x_,m_],mm_]:>(INT[n*(dot[x,x]-m^2)^mm,dens]);
tmp = tmp //. INT[n_,dens_List]*dot[x__]:>(INT[n*dot[x],dens]) /; !FreeQ[{x},k];
tmp = tmp //. INT[n_,dens_List]*Power[dot[x__],m_]:>(INT[n*dot[x]^m,dens]) /; !FreeQ[{x},k];
tmp = tmp /. INT[n_,{{mom1_,moms___},masses_}]:>((INT[n,{{mom1,moms},masses}] /. Solve[mom1==kp,k][[1]] /. kp->k) /; !MatchQ[mom1,k]);
Return[tmp//.ExpandDot]
]


Options[GetUFpoly] = {"Kinematics"->{}};
GetUFpoly[dens_,OptionsPattern[]]:=Module[{arg,AA,JJ,QQ},
If[MatchQ[dens[[1]],{}],Return[{0,0}]];
(* 1L only *)
arg = ((aa/@Range[Length[dens[[1]]]]) . (dot[#,#]&/@dens[[1]] - dens[[2]]^2) //. ExpandDot);
AA = Coefficient[arg,dot[k,k]];
JJ = arg /. dot[k,_]:>0;
QQ = (Expand[arg - AA*dot[k,k] - JJ] /. dot[k,p_]:>p)/2;
Return[{AA,Factor[JJ-dot[QQ,QQ]/AA]} //. ExpandDot /. OptionValue["Kinematics"]]
]


Options[CancelAndReduceNumerator] = {"Kinematics"->{}, "ReductionRules"->{}};
CancelAndReduceNumerator[I_INT,OptionsPattern[]]:=Module[{lmom,masses,DD,dots,dot2prop,expr},
lmom = I[[2,1]];
masses = I[[2,2]];
DD = dot[#,#]&/@lmom - masses^2 //. ExpandDot;
dots = Select[Cases[Variables[DD],_dot],!FreeQ[#,k]&];
dot2prop = Solve[Equal@@@Transpose@{prop@@@Transpose[{lmom,masses}],DD},dots[[;;Length[lmom]]]][[1]] //. OptionValue["Kinematics"];
expr = I[[1]] /. dot2prop;
expr = CollectIntegrals@Expand[INT[1,{{},{}}]*expr/Times@@(prop@@@Transpose[{lmom,masses}])] //. OptionValue["Kinematics"];
expr = expr /. INT[n_,dens_]:>0 /; GetUFpoly[dens,{"Kinematics"->OptionValue["Kinematics"]}][[2]]==0;

(* The time substitute the provided rules *)
expr = expr /. OptionValue["ReductionRules"] //. ExpandDot;
(* eliminate scaleless integrals first since kinematic ids may cause division by zero otherwise *)
expr = expr /. INT[n_,dens_]:>0 /; GetUFpoly[dens,{"Kinematics"->OptionValue["Kinematics"]}][[2]]==0;
expr = expr //. OptionValue["Kinematics"];
(* eliminate scaleless integrals again since Kinematics has been applied to the provided reduction rules *)
expr = expr /. INT[n_,dens_]:>0 /; GetUFpoly[dens,{"Kinematics"->OptionValue["Kinematics"]}][[2]]==0;

Return[expr]
]


Options[PVReductionRules]={"Kinematics"->{}, "ReductionRules"->{}};
PVReductionRules[I_INT,tensorbasis_,formfactors_,OptionsPattern[]]:=Module[{tensornumerator,lmom,masses,DD,dots,dot2prop,tensorintegal,Icontracted,Bcontracted,ReductionSol,PVRules,PVRule},

tensornumerator = I[[1]];
lmom = I[[2,1]];
masses = I[[2,2]];
DD = dot[#,#]&/@lmom - masses^2//. ExpandDot;
dots = Select[Cases[Variables[DD],_dot],!FreeQ[#,k]&];
dot2prop = Solve[Equal@@@Transpose@{prop@@@Transpose[{lmom,masses}],DD},dots[[;;Length[lmom]]]][[1]] //. OptionValue["Kinematics"];
tensorintegal = tensornumerator/Times@@(prop@@@Transpose[{lmom,masses}]);
(* first multiply tensor integral by tensorbasis and collect into scalar integrals *)
Icontracted = Expand[tensorintegal*tensorbasis]//. LorentzContract /. dot2prop /. OptionValue["Kinematics"];
Icontracted = Collect[CollectIntegrals[Expand[Icontracted*INT[1,{{},{}}]]],_INT,Factor] /. INT[n_,dens_]:>0 /; GetUFpoly[dens,{"Kinematics"->OptionValue["Kinematics"]}][[2]]==0;
Icontracted = Collect[Icontracted /. II_INT:>CancelAndReduceNumerator[II,{"Kinematics"->OptionValue["Kinematics"], "ReductionRules"->OptionValue["ReductionRules"]}],_INT,Factor];
(* now contract the formfactor expansion with the tensor basis *)
Bcontracted = Expand[(formfactors . tensorbasis)*tensorbasis]//. LorentzContract /. OptionValue["Kinematics"];
(* then solve the linear system *)

ReductionSol = Solve[Equal@@@Transpose@{Icontracted,Bcontracted},formfactors][[1]] /. Rule[a_,b_]:>Rule[a,Collect[b,_INT,Factor]];
PVRules = {};
Do[
PVRule = xRuleDelayed[I /. {
mu[1]->xPattern[x1,_], mu[2]->xPattern[x2,_], mu[3]->xPattern[x3,_],
p->xPattern[p,_], p1->xPattern[p1,_], p2->xPattern[p2,_], p3->xPattern[p3,_],
m1->xPattern[m1,_], m2->xPattern[m2,_], m3->xPattern[m3,_]
},tensorbasis . formfactors /. ReductionSol /. {
mu[1]->x1, mu[2]->x2, mu[3]->x3
}];
PVRules = Join[PVRules,{PVRule}];
,{x1,{x}},{x2,{x,y}},{x3,{x,y,z}}
];

Return[DeleteDuplicates[PVRules] /. xPattern->Pattern/. xRuleDelayed->RuleDelayed];
]


Kinematics={};

PVbub1 = PVReductionRules[INT[dot[k,mu[1]],{{k,k+p},{m1,m2}}],{dot[p,mu[1]]},{b[1]}];
PVbub2 = PVReductionRules[INT[dot[k,mu[1]]*dot[k,mu[2]],{{k,k+p},{m1,m2}}],{dot[mu[1],mu[2]],dot[p,mu[1]]*dot[p,mu[2]]},{b[0,0],b[1,1]}];
PVbub = Join[PVbub1,PVbub2];


PVtri1 = PVReductionRules[INT[dot[k,mu[1]],{{k,k+p1,k+p2},{m1,m2,m3}}],{dot[p1,mu[1]],dot[p2,mu[1]]},{c[1],c[2]},{"Kinematics"->Kinematics,"ReductionRules"->PVbub}];

tensorbasis = {
dot[mu[1],mu[2]],
dot[p1,mu[1]]*dot[p1,mu[2]],
dot[p2,mu[1]]*dot[p2,mu[2]],
1/2*(dot[p1,mu[1]]*dot[p2,mu[2]]+dot[p2,mu[1]]*dot[p1,mu[2]])
};
formfactors = {c[0,0],c[1,1],c[2,2],c[1,2]};
PVtri2 = PVReductionRules[INT[dot[k,mu[1]]*dot[k,mu[2]],{{k,k+p1,k+p2},{m1,m2,m3}}],tensorbasis,formfactors,{"Kinematics"->Kinematics,"ReductionRules"->PVbub}];

tensorbasis = {
dot[mu[1],mu[2]]*dot[p1,mu[3]]+dot[mu[2],mu[3]]*dot[p1,mu[1]]+dot[mu[3],mu[1]]*dot[p1,mu[2]],
dot[mu[1],mu[2]]*dot[p2,mu[3]]+dot[mu[2],mu[3]]*dot[p2,mu[1]]+dot[mu[3],mu[1]]*dot[p2,mu[2]],
dot[p1,mu[1]]*dot[p1,mu[2]]*dot[p1,mu[3]],
dot[p2,mu[1]]*dot[p2,mu[2]]*dot[p2,mu[3]],
dot[p1,mu[1]]*dot[p1,mu[2]]*dot[p2,mu[3]]+dot[p1,mu[1]]*dot[p2,mu[2]]*dot[p1,mu[3]]+dot[p2,mu[1]]*dot[p1,mu[2]]*dot[p1,mu[3]],
dot[p2,mu[1]]*dot[p2,mu[2]]*dot[p1,mu[3]]+dot[p2,mu[1]]*dot[p1,mu[2]]*dot[p2,mu[3]]+dot[p1,mu[1]]*dot[p2,mu[2]]*dot[p2,mu[3]]
};
formfactors = {c[0,0,1],c[0,0,2],c[1,1,1],c[2,2,2],c[1,1,2],c[1,2,2]};
PVtri3 = PVReductionRules[INT[dot[k,mu[1]]*dot[k,mu[2]]*dot[k,mu[3]],{{k,k+p1,k+p2},{m1,m2,m3}}],tensorbasis,formfactors,{"Kinematics"->Kinematics,"ReductionRules"->PVbub}];

PVtri = Join[PVtri1,PVtri2,PVtri3];


PVrules = Join[PVbub,PVtri];


ApplyCliffordAlgebra[expr_,ordering_List]:=Module[{tmp},
tmp = Expand[expr //. ExpandDot] //. LorentzContract;
tmp = tmp //. {
gstr[x___,g[b_],g[c_],y___]:>(-gstr[x,g[c],g[b],y]+2*dot[b,c]*gstr[x,y] /; (Position[ordering,b][[1,1]]>Position[ordering,c][[1,1]])),
dot[a_mu,b_]*dot[a_mu,c_]:>dot[b,c],
dot[a_mu,b_]^2:>dot[b,b],
dot[a_mu,a_mu]:>d,
dot[a_mu,b_]*gstr[x___,g[a_mu],y___]:>gstr[x,g[b],y],
gstr[x___,g[a_],g[a_],y___]:>gstr[x,y]*dot[a,a],
gstr[ub[i_],g[i_],x__]:>m*gstr[ub[i],x],
gstr[x__,g[i_],u[i_]]:>m*gstr[x,u[i]],
(* traces *)
gstr[x_al,g[a1_],x_al]:>0,
gstr[x_al,g[a1_],g[a2_],x_al]:>4*dot[a1,a2],
gstr[x_al,g[a1_],g[a2_],g[a3_],x_al]:>0,
gstr[x_al,g[a1_],g[a2_],g[a3_],g[a4_],x_al]:>4*(dot[a1,a2]*dot[a3,a4]-dot[a1,a3]*dot[a2,a4]+dot[a1,a4]*dot[a2,a3])
};
Return[tmp //. LorentzContract];
]
