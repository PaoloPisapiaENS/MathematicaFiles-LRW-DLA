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
dot[a_mu,b_mu]:>d
}


lmom = {k,k-p[2],k+p[1]}
DD = dot[#,#]&/@lmom//. ExpandDot
dot2prop = Solve[Equal@@@Transpose@{prop/@lmom,DD},{dot[k,k],dot[k,p[1]],dot[k,p[2]]}][[1]]


Kinematics = {
dot[p[1],p[1]]:>0,
dot[p[2],p[2]]:>0,
dot[p[1],p[2]]:>s[3]/2
};


dot[k,mu[1]]*dot[k,mu[2]]*dot[mu[1],mu[2]] //. LorentzContract


CollectIntegrals[expr_]:=Module[{tmp},
tmp = expr //. INT[n_,dens_List]/prop[x_]:>INT[n,Join[dens,{x}]];
tmp = tmp //. INT[n_,dens_List]*prop[x_]:>(INT[n*dot[x,x],dens]);
tmp = tmp //. INT[n_,dens_List]*Power[prop[x_],m_]:>(INT[n*dot[x,x]^m,dens]);
tmp = tmp //. INT[n_,dens_List]*dot[x__]:>(INT[n*dot[x],dens]) /; !FreeQ[{x},k];
tmp = tmp //. INT[n_,dens_List]*Power[dot[x__],m_]:>(INT[n*dot[x]^m,dens]) /; !FreeQ[{x},k];
tmp = tmp /. INT[n_,{mom1_,moms__}]:>((INT[n,{mom1,moms}] /. Solve[mom1==kp,k][[1]] /. kp->k) /; !MatchQ[mom1,k]);
Return[tmp//.ExpandDot]
]


CollectIntegrals[Expand[dot[k,mu[1]]/prop[k]/prop[k-p[1]]*INT[1,{}]]]


Options[GetUFpoly] = {"Kinematics"->{}};
GetUFpoly[dens_,OptionsPattern[]]:=Module[{arg,AA,JJ,QQ},
If[MatchQ[dens,{}],Return[{0,0}]];
(* 1L only *)
arg = ((aa/@Range[Length[dens]]) . (dot[#,#]&/@dens) //. ExpandDot);
AA = Coefficient[arg,dot[k,k]];
JJ = arg /. dot[k,_]:>0;
QQ = (Expand[arg - AA*dot[k,k] - JJ] /. dot[k,p_]:>p)/2;
Return[{AA,Factor[JJ-dot[QQ,QQ]/AA]} //. ExpandDot /. OptionValue["Kinematics"]]
]
(* remove scaleless integrals *)
INT[1,{k,k-p[1]}] /. INT[n_,dens_]:>0 /; GetUFpoly[dens,{"Kinematics"->Kinematics}][[2]]==0
INT[1,{k}] /. INT[n_,dens_]:>0 /; GetUFpoly[dens,{"Kinematics"->Kinematics}][[2]]==0


tensorintegal = dot[k,mu[1]]/Times@@(prop/@lmom)
tensorbasis = {dot[p[1],mu[1]],dot[p[2],mu[1]]}
formfactors = {c[1],c[2]}

(* first multiply tensor integral by tensorbasis and collect into scalar integrals *)
Expand[tensorintegal*tensorbasis]//. LorentzContract /. dot2prop /. Kinematics
Collect[CollectIntegrals[Expand[%*INT[1,{}]]],_INT,Factor] /. INT[n_,dens_]:>0 /; GetUFpoly[dens,{"Kinematics"->Kinematics}][[2]]==0
(* now contract the formfactor expansion with the tensor basis *)
Expand[(formfactors . tensorbasis)*tensorbasis]//. LorentzContract /. Kinematics
(* then solve the linear system *)
Solve[Equal@@@Transpose@{%,%%},formfactors][[1]] /. Rule[a_,b_]:>Rule[a,Collect[b,_INT,Factor]]


Options[CancelAndReduceNumerator] = {"Kinematics"->{}};
CancelAndReduceNumerator[I_INT,OptionsPattern[]]:=Module[{lmom,DD,dots,dot2prop,expr},
lmom = I[[2]];
DD = dot[#,#]&/@lmom//. ExpandDot;
dots = Select[Cases[Variables[DD],_dot],!FreeQ[#,k]&];
dot2prop = Solve[Equal@@@Transpose@{prop/@lmom,DD},dots[[;;Length[lmom]]]][[1]] //. OptionValue["Kinematics"];
expr = I[[1]] /. dot2prop;
expr = CollectIntegrals@Expand[INT[1,{}]*expr/Times@@(prop/@lmom)] //. OptionValue["Kinematics"];
expr = expr /. INT[n_,dens_]:>0 /; GetUFpoly[dens,{"Kinematics"->OptionValue["Kinematics"]}][[2]]==0;

(* Here we should include a general substitution of known PV reduction ids *)
(* start by hard coding necessary features *)
expr = expr /. {
INT[dot[k,x_],{k,k+p_}]:>-1/2*dot[p,x]*INT[1,{k,k+p}],
INT[dot[k,x_]^2,{k,k+p_}]:>1/4/(1-d)*(dot[p,p]*dot[x,x]-d*dot[p,x]^2)*INT[1,{k,k+p}]
} //. ExpandDot //. Kinematics;

Return[expr]
]


CancelAndReduceNumerator[INT[dot[k,k]+2 dot[k,p[1]]+dot[p[1],p[1]],{k,k+p[1]+p[2]}],{"Kinematics"->Kinematics}]


tensorintegal = dot[k,mu[1]]*dot[k,mu[2]]/Times@@(prop/@lmom)
tensorbasis = {dot[mu[1],mu[2]],dot[p[1],mu[1]]*dot[p[1],mu[2]],dot[p[2],mu[1]]*dot[p[2],mu[2]],1/2*(dot[p[1],mu[1]]*dot[p[2],mu[2]]+dot[p[2],mu[1]]*dot[p[1],mu[2]])}
formfactors = {c[0,0],c[1,1],c[2,2],c[1,2]}

(* first multiply tensor integral by tensorbasis and collect into scalar integrals *)
Expand[tensorintegal*tensorbasis]//. LorentzContract /. dot2prop /. Kinematics;
Collect[CollectIntegrals[Expand[%*INT[1,{}]]],_INT,Factor] /. INT[n_,dens_]:>0 /; GetUFpoly[dens,{"Kinematics"->Kinematics}][[2]]==0;
Collect[% /. I_INT:>CancelAndReduceNumerator[I,{"Kinematics"->Kinematics}],_INT,Factor]
(* now contract the formfactor expansion with the tensor basis *)
Expand[(formfactors . tensorbasis)*tensorbasis]//. LorentzContract /. Kinematics
(* then solve the linear system *)
Solve[Equal@@@Transpose@{%,%%},formfactors][[1]] /. Rule[a_,b_]:>Rule[a,Collect[b,_INT,Factor]]
%[[;;,2]]/INT[1,{k,k-p[1]-p[2]}]*(d-2)*2//Factor


tensorintegal = dot[k,mu[1]]*dot[k,mu[2]]*dot[k,mu[3]]/Times@@(prop/@lmom)
tensorbasis = {
dot[mu[1],mu[2]]*dot[p[1],mu[3]]+dot[mu[2],mu[3]]*dot[p[1],mu[1]]+dot[mu[3],mu[1]]*dot[p[1],mu[2]],
dot[mu[1],mu[2]]*dot[p[2],mu[3]]+dot[mu[2],mu[3]]*dot[p[2],mu[1]]+dot[mu[3],mu[1]]*dot[p[2],mu[2]],
dot[p[1],mu[1]]*dot[p[1],mu[2]]*dot[p[1],mu[3]],
dot[p[2],mu[1]]*dot[p[2],mu[2]]*dot[p[2],mu[3]],
dot[p[1],mu[1]]*dot[p[1],mu[2]]*dot[p[2],mu[3]]+dot[p[1],mu[1]]*dot[p[2],mu[2]]*dot[p[1],mu[3]]+dot[p[2],mu[1]]*dot[p[1],mu[2]]*dot[p[1],mu[3]],
dot[p[2],mu[1]]*dot[p[2],mu[2]]*dot[p[1],mu[3]]+dot[p[2],mu[1]]*dot[p[1],mu[2]]*dot[p[2],mu[3]]+dot[p[1],mu[1]]*dot[p[2],mu[2]]*dot[p[2],mu[3]]
};
formfactors = {c[0,0,1],c[0,0,2],c[1,1,1],c[2,2,2],c[1,1,2],c[1,2,2]};

(* first multiply tensor integral by tensorbasis and collect into scalar integrals *)
Expand[tensorintegal*tensorbasis]//. LorentzContract /. dot2prop /. Kinematics;
Collect[CollectIntegrals[Expand[%*INT[1,{}]]],_INT,Factor] /. INT[n_,dens_]:>0 /; GetUFpoly[dens,{"Kinematics"->Kinematics}][[2]]==0;
Collect[% /. I_INT:>CancelAndReduceNumerator[I,{"Kinematics"->Kinematics}],_INT,Factor]
(* now contract the formfactor expansion with the tensor basis *)
Expand[(formfactors . tensorbasis)*tensorbasis]//. LorentzContract /. Kinematics
(* then solve the linear system *)
Solve[Equal@@@Transpose@{%,%%},formfactors][[1]] /. Rule[a_,b_]:>Rule[a,Collect[b,_INT,Factor]]
%[[;;,2]]/INT[1,{k,k-p[1]-p[2]}]*(d-1)*4//Factor


(* ::Subsection::Closed:: *)
(*bubble reduction (can use to cross check hard coded ids)*)


lmom = {k,k+p[1]}
DD = dot[#,#]&/@lmom//. ExpandDot
dot2prop = Solve[Equal@@@Transpose@{prop/@lmom,DD},{dot[k,k],dot[k,p[1]]}][[1]]
Kinematics = {};


tensorintegal = dot[k,mu[1]]/Times@@(prop/@lmom)
tensorbasis = {dot[p[1],mu[1]]}
formfactors = {b[1]}

(* first multiply tensor integral by tensorbasis and collect into scalar integrals *)
Expand[tensorintegal*tensorbasis]//. LorentzContract /. dot2prop /. Kinematics;
Collect[CollectIntegrals[Expand[%*INT[1,{}]]],_INT,Factor] /. INT[n_,dens_]:>0 /; GetUFpoly[dens,{"Kinematics"->Kinematics}][[2]]==0;
Collect[% /. I_INT:>CancelAndReduceNumerator[I,{"Kinematics"->Kinematics}],_INT,Factor]
(* now contract the formfactor expansion with the tensor basis *)
Expand[(formfactors . tensorbasis)*tensorbasis]//. LorentzContract /. Kinematics
(* then solve the linear system *)
Solve[Equal@@@Transpose@{%,%%},formfactors][[1]] /. Rule[a_,b_]:>Rule[a,Collect[b,_INT,Factor]]


tensorintegal = dot[k,mu[1]]*dot[k,mu[2]]/Times@@(prop/@lmom)
tensorbasis = {dot[mu[1],mu[2]],dot[p[1],mu[1]]*dot[p[1],mu[2]]}
formfactors = {b[0,0],b[1,1]}

(* first multiply tensor integral by tensorbasis and collect into scalar integrals *)
Expand[tensorintegal*tensorbasis]//. LorentzContract /. dot2prop /. Kinematics;
Collect[CollectIntegrals[Expand[%*INT[1,{}]]],_INT,Factor] /. INT[n_,dens_]:>0 /; GetUFpoly[dens,{"Kinematics"->Kinematics}][[2]]==0;
Collect[% /. I_INT:>CancelAndReduceNumerator[I,{"Kinematics"->Kinematics}],_INT,Factor]
(* now contract the formfactor expansion with the tensor basis *)
Expand[(formfactors . tensorbasis)*tensorbasis]//. LorentzContract /. Kinematics
(* then solve the linear system *)
Solve[Equal@@@Transpose@{%,%%},formfactors][[1]] /. Rule[a_,b_]:>Rule[a,Collect[b,_INT,Factor]]


(* ::Subsection::Closed:: *)
(*neat version*)


Clear[CancelAndReduceNumerator,PVReductionRules]


Options[CancelAndReduceNumerator] = {"Kinematics"->{}, "ReductionRules"->{}};
CancelAndReduceNumerator[I_INT,OptionsPattern[]]:=Module[{lmom,DD,dots,dot2prop,expr},
lmom = I[[2]];
DD = dot[#,#]&/@lmom//. ExpandDot;
dots = Select[Cases[Variables[DD],_dot],!FreeQ[#,k]&];
dot2prop = Solve[Equal@@@Transpose@{prop/@lmom,DD},dots[[;;Length[lmom]]]][[1]] //. OptionValue["Kinematics"];
expr = I[[1]] /. dot2prop;
expr = CollectIntegrals@Expand[INT[1,{}]*expr/Times@@(prop/@lmom)] //. OptionValue["Kinematics"];
expr = expr /. INT[n_,dens_]:>0 /; GetUFpoly[dens,{"Kinematics"->OptionValue["Kinematics"]}][[2]]==0;

(* The time substitute the provided rules *)
expr = expr /. OptionValue["ReductionRules"] //. ExpandDot //. OptionValue["Kinematics"];
expr = expr /. INT[n_,dens_]:>0 /; GetUFpoly[dens,{"Kinematics"->OptionValue["Kinematics"]}][[2]]==0;

Return[expr]
]


Options[PVReductionRules]={"Kinematics"->{}, "ReductionRules"->{}};
PVReductionRules[I_INT,tensorbasis_,formfactors_,OptionsPattern[]]:=Module[{tensornumerator,lmom,DD,dots,dot2prop,tensorintegal,Icontracted,Bcontracted,ReductionSol,PVRules,PVRule},

tensornumerator = I[[1]];
lmom = I[[2]];
DD = dot[#,#]&/@lmom//. ExpandDot;
dots = Select[Cases[Variables[DD],_dot],!FreeQ[#,k]&];
dot2prop = Solve[Equal@@@Transpose@{prop/@lmom,DD},dots[[;;Length[lmom]]]][[1]] //. OptionValue["Kinematics"];
tensorintegal = tensornumerator/Times@@(prop/@lmom);
(* first multiply tensor integral by tensorbasis and collect into scalar integrals *)
Icontracted = Expand[tensorintegal*tensorbasis]//. LorentzContract /. dot2prop /. OptionValue["Kinematics"];
Icontracted = Collect[CollectIntegrals[Expand[Icontracted*INT[1,{}]]],_INT,Factor] /. INT[n_,dens_]:>0 /; GetUFpoly[dens,{"Kinematics"->OptionValue["Kinematics"]}][[2]]==0;
Icontracted = Collect[Icontracted /. II_INT:>CancelAndReduceNumerator[II,{"Kinematics"->OptionValue["Kinematics"], "ReductionRules"->OptionValue["ReductionRules"]}],_INT,Factor];
(* now contract the formfactor expansion with the tensor basis *)
Bcontracted = Expand[(formfactors . tensorbasis)*tensorbasis]//. LorentzContract /. OptionValue["Kinematics"];
(* then solve the linear system *)

ReductionSol = Solve[Equal@@@Transpose@{Icontracted,Bcontracted},formfactors][[1]] /. Rule[a_,b_]:>Rule[a,Collect[b,_INT,Factor]];
PVRules = {};
Do[
PVRule = xRuleDelayed[I /. {
mu[1]->xPattern[x1,_], mu[2]->xPattern[x2,_], mu[3]->xPattern[x3,_], p->xPattern[p,_], p1->xPattern[p1,_], p2->xPattern[p2,_], p3->xPattern[p3,_]
},tensorbasis . formfactors /. ReductionSol /. {
mu[1]->x1, mu[2]->x2, mu[3]->x3
}];
PVRules = Join[PVRules,{PVRule}];
,{x1,{x}},{x2,{x,y}},{x3,{x,y,z}}
];

Return[DeleteDuplicates[PVRules] /. xPattern->Pattern/. xRuleDelayed->RuleDelayed];
]


PVbub1 = PVReductionRules[INT[dot[k,mu[1]],{k,k+p}],{dot[p,mu[1]]},{b[1]}];
PVbub2 = PVReductionRules[INT[dot[k,mu[1]]*dot[k,mu[2]],{k,k+p}],{dot[mu[1],mu[2]],dot[p,mu[1]]*dot[p,mu[2]]},{b[0,0],b[1,1]}];
PVbub = Join[PVbub1,PVbub2]


Kinematics = {
dot[p1,p1]:>0,
dot[p2,p2]:>0
};

PVReductionRules[INT[dot[k,mu[1]],{k,k-p2,k+p1}],{dot[p1,mu[1]],dot[p2,mu[1]]},{c[1],c[2]},{"Kinematics"->Kinematics,"ReductionRules"->PVbub}]

tensorbasis = {dot[mu[1],mu[2]],dot[p[1],mu[1]]*dot[p[1],mu[2]],dot[p[2],mu[1]]*dot[p[2],mu[2]],1/2*(dot[p[1],mu[1]]*dot[p[2],mu[2]]+dot[p[2],mu[1]]*dot[p[1],mu[2]])};
formfactors = {c[0,0],c[1,1],c[2,2],c[1,2]};
PVReductionRules[INT[dot[k,mu[1]]*dot[k,mu[2]],{k,k-p2,k+p1}],tensorbasis /. p[1]->p1 /. p[2]->p2,formfactors,{"Kinematics"->Kinematics,"ReductionRules"->PVbub}]

tensorbasis = {
dot[mu[1],mu[2]]*dot[p[1],mu[3]]+dot[mu[2],mu[3]]*dot[p[1],mu[1]]+dot[mu[3],mu[1]]*dot[p[1],mu[2]],
dot[mu[1],mu[2]]*dot[p[2],mu[3]]+dot[mu[2],mu[3]]*dot[p[2],mu[1]]+dot[mu[3],mu[1]]*dot[p[2],mu[2]],
dot[p[1],mu[1]]*dot[p[1],mu[2]]*dot[p[1],mu[3]],
dot[p[2],mu[1]]*dot[p[2],mu[2]]*dot[p[2],mu[3]],
dot[p[1],mu[1]]*dot[p[1],mu[2]]*dot[p[2],mu[3]]+dot[p[1],mu[1]]*dot[p[2],mu[2]]*dot[p[1],mu[3]]+dot[p[2],mu[1]]*dot[p[1],mu[2]]*dot[p[1],mu[3]],
dot[p[2],mu[1]]*dot[p[2],mu[2]]*dot[p[1],mu[3]]+dot[p[2],mu[1]]*dot[p[1],mu[2]]*dot[p[2],mu[3]]+dot[p[1],mu[1]]*dot[p[2],mu[2]]*dot[p[2],mu[3]]
};
formfactors = {c[0,0,1],c[0,0,2],c[1,1,1],c[2,2,2],c[1,1,2],c[1,2,2]};

PVReductionRules[
INT[dot[k,mu[1]]*dot[k,mu[2]]*dot[k,mu[3]],{k,k+p1,k+p2}],
tensorbasis /. p[1]->p1 /. p[2]->p2,
formfactors,
{"Kinematics"->Kinematics,"ReductionRules"->PVbub}]


Kinematics={};

PVtri1 = PVReductionRules[INT[dot[k,mu[1]],{k,k+p1,k+p2}],{dot[p1,mu[1]],dot[p2,mu[1]]},{c[1],c[2]},{"Kinematics"->Kinematics,"ReductionRules"->PVbub}];

tensorbasis = {dot[mu[1],mu[2]],dot[p[1],mu[1]]*dot[p[1],mu[2]],dot[p[2],mu[1]]*dot[p[2],mu[2]],1/2*(dot[p[1],mu[1]]*dot[p[2],mu[2]]+dot[p[2],mu[1]]*dot[p[1],mu[2]])};
formfactors = {c[0,0],c[1,1],c[2,2],c[1,2]};
PVtri2 = PVReductionRules[INT[dot[k,mu[1]]*dot[k,mu[2]],{k,k+p1,k+p2}],tensorbasis /. p[1]->p1 /. p[2]->p2,formfactors,{"Kinematics"->Kinematics,"ReductionRules"->PVbub}];

tensorbasis = {
dot[mu[1],mu[2]]*dot[p[1],mu[3]]+dot[mu[2],mu[3]]*dot[p[1],mu[1]]+dot[mu[3],mu[1]]*dot[p[1],mu[2]],
dot[mu[1],mu[2]]*dot[p[2],mu[3]]+dot[mu[2],mu[3]]*dot[p[2],mu[1]]+dot[mu[3],mu[1]]*dot[p[2],mu[2]],
dot[p[1],mu[1]]*dot[p[1],mu[2]]*dot[p[1],mu[3]],
dot[p[2],mu[1]]*dot[p[2],mu[2]]*dot[p[2],mu[3]],
dot[p[1],mu[1]]*dot[p[1],mu[2]]*dot[p[2],mu[3]]+dot[p[1],mu[1]]*dot[p[2],mu[2]]*dot[p[1],mu[3]]+dot[p[2],mu[1]]*dot[p[1],mu[2]]*dot[p[1],mu[3]],
dot[p[2],mu[1]]*dot[p[2],mu[2]]*dot[p[1],mu[3]]+dot[p[2],mu[1]]*dot[p[1],mu[2]]*dot[p[2],mu[3]]+dot[p[1],mu[1]]*dot[p[2],mu[2]]*dot[p[2],mu[3]]
};
formfactors = {c[0,0,1],c[0,0,2],c[1,1,1],c[2,2,2],c[1,1,2],c[1,2,2]};
PVtri3 = PVReductionRules[INT[dot[k,mu[1]]*dot[k,mu[2]]*dot[k,mu[3]],{k,k+p1,k+p2}],tensorbasis /. p[1]->p1 /. p[2]->p2,formfactors,{"Kinematics"->Kinematics,"ReductionRules"->PVbub}];
PVtri = Join[PVtri1,PVtri2,PVtri3];


Factor/@CancelAndReduceNumerator[INT[dot[k,mu[1]]*dot[k,mu[2]],{k,k-p2,k+p1}],{"ReductionRules"->PVtri,"Kinematics"->{dot[p1,p1]:>0,dot[p2,p2]:>0}}]


(* rank 1 box : p1^2=p2^2=p3^2=0, p4^2=s4 *)

Kinematics={
dot[p1,p1]->0, dot[p2,p2]->0, dot[p3,p3]->0,
dot[p1,p2]->s12/2,
dot[p2,p3]->s23/2,
dot[p1,p3]->(s4-s12-s23)/2
};

PVReductionRules[
  INT[dot[k,mu[1]],{k,k+p1,k+p2,k+p3}],
  {dot[p1,mu[1]],dot[p2,mu[1]],dot[p3,mu[1]]},{d[1],d[2],d[3]},
  {"Kinematics"->Kinematics,"ReductionRules"->Join[PVbub,PVtri]}]
