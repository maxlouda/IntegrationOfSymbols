(* ::Package:: *)

BeginPackage["Symbols`"];


ListToSymbolicExpression::usage = "In: list of the form {{\[Lambda]1, {a, b}}, {\[Lambda]2, {c, d}}}. Out: Symbolic expression SymbPlus[SymbScalar[\[Lambda]1,Symb[a,b]],SymbScalar[\[Lambda]2,Symb[c,d]]].";
SymbolicExpressionToList::usage = "In: Symbolic expression of the form SymbPlus[SymbScalar[\[Lambda]1,Symb[a,b]],SymbScalar[\[Lambda]2,Symb[c,d]]]. Out: list {{\[Lambda]1, {a, b}}, {\[Lambda]2, {c, d}}}.";
SymbolicExpressionToReadable::usage = "In: Symbolic expression of the form SymbPlus[SymbScalar[\[Lambda]1,Symb[a,b]],SymbScalar[\[Lambda]2,Symb[c,d]]]. Out: more readable expression \[Lambda]1 a\[CircleTimes]b+\[Lambda]2 c\[CircleTimes]d.";
ReadableToSymbolicExpression::usage = "In: More readable expression of the form \[Lambda]1 a\[CircleTimes]b+\[Lambda]2 c\[CircleTimes]d. Out: Symbolic expression SymbPlus[SymbScalar[\[Lambda]1,Symb[a,b]],SymbScalar[\[Lambda]2,Symb[c,d]]].";
FlattenSymbolicExpression::usage = "In: Complicated symbolic expression. Out: Flattened out symbolic expression. Makes use of commutativity, associativity and distributivity of SymbPlus and SymbScalar.";
FactorRationalsAndIntegers::usage = "In: some rational expression. Out: factorized version. Torsion is neglected. 0, 1, -1 are filtered out.";
MySymbolExpand::usage = "LEGACY. Use SymbolExpand. In: a symbol consisting of an overall factor and some rational functions as tensor products. Out: Expanded symbol.";
SymbolExpand::usage = "In: a symbolic expression representing a (nested) linear combination of tensor products. Out: Expanded symbol.";
SymbG::usage = "In: a MPL in the G-representation G[a1, a2, a3, ..., an, x]. Out: Its symbol.";
Shuffle::usage = "In: List of symbolic expressions. Out: shuffle product of those symbolic expressions.";
(*Sh::usage = "Code intern Shuffle. Do not use.";*)


Begin["`Private`"];


Unprotect["`*"];
Clear["`*"];


ReadableToSymbolicExpression[read_]:=Block[{res, coeffs, coeff, syms},
Symb = Symbol["Symb"];
SymbPlus = Symbol["SymbPlus"];
SymbScalar = Symbol["SymbScalar"];
res = Expand[read]/.{CircleTimes->Symb};
If[Head[res]===Plus,
coeffs = Flatten[(Cases[#,n_ Symb[__]:>n,{0,Infinity}]/. {}->{1})& /@ (List@@res)];
syms = Cases[List@@res, Symb[__], Infinity];
res = SymbPlus@@MapThread[SymbScalar[#1, #2]&, {coeffs, syms}];
(*Skalar beim zweiten return statement muss noch richtig angepasst werden.*)
Return[Sort[res]], coeff = Flatten[(Cases[#,n_ Symb[__]:>n,{0,Infinity}]/. {}->{1})& /@ ({res})][[1]]; Return[SymbScalar[coeff,1/coeff * res]]]]


SymbolicExpressionToReadable[expr_]:=Block[{res},
Symb = Symbol["Symb"];
SymbPlus = Symbol["SymbPlus"];
SymbScalar = Symbol["SymbScalar"];
res = expr/.{Symb->CircleTimes, SymbScalar->Times, SymbPlus->Plus};
Return[Expand[res]]]


ListToSymbolicExpression[list_]:=Block[{res},
Symb = Symbol["Symb"];
SymbPlus = Symbol["SymbPlus"];
SymbScalar = Symbol["SymbScalar"];
(*res = SymbPlus@@Map[SymbScalar[#[[1]], #[[2]]]&, Apply[Symb, list, {2}], {1}];*)
res = SymbPlus@@Map[SymbScalar[#[[1]], #[[2]]]&, Map[If[Head[#]===List, Symb @@ #, #]&, list, {2}], {1}];
Return[res]]


SymbolicExpressionToList[expr_]:= Block[{res},
Symb = Symbol["Symb"];
SymbPlus = Symbol["SymbPlus"];
SymbScalar = Symbol["SymbScalar"];
res = FlattenSymbolicExpression[expr];
If[Head[res]===SymbPlus,
res = Apply[List, res];
(*res = Complement[res, Cases[expr, SymbScalar[_, Symb[__]]]];
Print[res];
res = SymbScalar[1, #]& /@ res;
Print[res];*)
res = {#[[1]],List@@#[[2]]}&/@ Apply[List,Apply[List,res,{1}]];
Return[Sort[res]], Return[{{res[[1]],List@@res[[2]]}}]]]


FlattenSymbolicExpression[expr_]:=Block[{res},
Symb = Symbol["Symb"];
SymbPlus = Symbol["SymbPlus"];
SymbScalar = Symbol["SymbScalar"];
(*Explicitly (may be prone to error for highly nested Symbolic expressions):
res = expr /. SymbScalar[q_, SymbPlus[args__]]:>SymbPlus @@ (SymbScalar[q, #]& /@ {args});
res = Sort[Flatten[res]]*)
(*Instead: use SymbolicExpressionToReadable -> builtin laws -> ReadableToSymbolicExpression:*)
res = SymbolicExpressionToReadable[expr];
res = ReadableToSymbolicExpression[res];
Return[res]]


FactorListNew[poly_]:=Block[{res},
res = FactorList[poly];
res[[1]] = FactorInteger[res[[1,1]]];
res = FlattenAt[res,1];
Return[res]]


FactorRationalsAndIntegers[expr_]:= Block[{res, numerator, denominator},
res = {Numerator[Factor[expr]], Denominator[Factor[expr]]};
(*res = If[IntegerQ[#], FactorInteger[#], FactorListNew[#]]& /@ res;*)
res = FactorListNew[#]& /@ res;
For[i = 1, i <= Length[res[[2]]], i++, res[[2]][[i]][[2]] = - res[[2]][[i]][[2]]];
res = DeleteCases[Flatten[res,1], {1, 1} | {1, -1} | {-1, 1} | {-1, -1} | {0, 1} | {0, -1}];
Return[res]]


(* Input for MySymbolExpand: {{x^3 + 1, 4, 3, x^2 - 1, 1/(x+1)}, {x^2, x, 1/x, 1/x^2}, {x, x, x,x}, {x, 0, 2}}.
Output: 5 Symb[x,x,x,x]-2 Symb[1+x,2,3,-1+x,1+x]-2 Symb[1+x,2,3,1+x,1+x]-2 Symb[1-x+x^2,2,3,-1+x,1+x]-2 Symb[1-x+x^2,2,3,1+x,1+x].*)


MySymbolExpand[symbol_]:= Block[{res},
Symb = Symbol["Symb"];
SymbPlus = Symbol["SymbPlus"];
SymbScalar = Symbol["SymbScalar"];
res = Flatten[Map[Tuples, Map[FactorRationalsAndIntegers, symbol[[#]]]& /@ Range[Length[symbol]],1],1];
res = {Times @@ #[[All, 2]], #[[All, 1]]}& /@ res;
(*res = Plus@@(Times[#[[1]],Symb@@#[[2]]]&/@ res);*)
res = SymbPlus@@(SymbScalar[#[[1]], Symb@@#[[2]]]& /@ res);
Clear[Symb];
Return[res]]


SymbolExpand[expr_]:=Block[{res, inp, rules},
Symb = Symbol["Symb"];
SymbPlus = Symbol["SymbPlus"];
SymbScalar = Symbol["SymbScalar"];
inp = SymbolicExpressionToList[expr];
res = Map[Tuples, Map[FactorRationalsAndIntegers,inp[[#, 2]]]& /@ Range[Length[inp]],1];
res = DeleteCases[Prepend[{res[[#]]}, inp[[#, 1]]]& /@ Range[Length[inp]], {q_Integer, {}} | {q_Rational, {}}];
(*Print[res];
res = res/.{res[[#, 2]]->({Times @@ #[[All, 2]], #[[All, 1]]}& /@ res[[#,2]])}& /@ Range[Length[res]];*)
Do[res[[i,2]]={Times@@#[[All,2]],#[[All,1]]}&/@res[[i,2]],{i,Length[res]}];
(*Print[res];
res = Cases[Flatten[res, 1], {q_Integer, {{p_Integer, ___}, ___}} | {q_Rational, {{p_Integer, ___}, ___}} | {q_Integer, {{p_Rational, ___}, ___}} | {q_Rational, {{p_Rational, ___}, ___}} | {_, {{_Integer, ___}, ___}}];*)
Do[res[[i]][[2]][[j]][[1]] = res[[i]][[1]]*res[[i]][[2]][[j]][[1]],{i, Length[res]}, {j, Length[res[[i]][[2]]]}];
res = Flatten[Table[res[[i]][[2]],{i,Length[res]}], 1];
Return[ListToSymbolicExpression[res]]]


(*Input form for Gexpr_: G[a1, a2, a3, ..., an, x]*)


SymbG[Gexpr_]:=Block[{res, rules, args},
rules = {S[x___,a_+b_,c_,y___]:>S[x,a,c,y]+S[x,b,c,y],S[x___,a_,b_+c_,y___]:>S[x,a,b,y]+S[x,a,c,y],S[x___,a_ b_,y___]/;NumericQ[b]:>b S[x,a,y],S[x___,b_ c_,y___]/;NumericQ[c]:>c S[x,b,y],
S[___,-1,___]:>0, S[___,1,___]:>0, S[___,0,___]:>0};
args = Join[Delete[Insert[List@@Gexpr,List@@Gexpr[[-1]],1],-1],{0}];
ClearAll[SymbGintern];
SymbGintern[G_[a0_,a1_,a2_]]:=S[a0-a1]-S[a2-a1];
SymbGintern[G_[a0_,rest__, an_]]:=Sum[S[SymbGintern[G[a0,Sequence@@Delete[{rest},i],an]],S[Join[{a0},{rest},{an}][[i]]-Join[{a0},{rest},{an}][[i+1]]]-S[Join[{a0},{rest},{an}][[i+2]]-Join[{a0},{rest},{an}][[i+1]]]],{i,1,Length[{rest}]}];
res = ((SymbGintern[G@@args]//.rules)/.{S->Symb})/.{Symb->CircleTimes};
Return[SymbolExpand[ReadableToSymbolicExpression[res]]]]


(*Input form for expr_: {SymbolicExpression1, SymbolicExpression2, ..., SymbolicExpressionN}*)


Shuffle[expr_]:=Module[{res, args, rules1, rules2, rules3},
args = expr /. {Symb->S, SymbPlus->SPlus, SymbScalar->SScalar};
Sh[S[a_, b___], S[c_, d___]]:=SPlus[S[a, Sh[S[b], S[c,d]]],S[c,Sh[S[a,b],S[d]]]];
Sh[S[],a_]:=a;
Sh[a_,S[]]:=a;
Sh[a_,b_,c__]:=Sh[Sh[a,b],c];
Sh[a_SPlus, y___]:=SPlus@@(Sh[#, y]& /@ a);
Sh[y___, a_SPlus]:=SPlus@@(Sh[y, #]& /@ a);
Sh[x___, SScalar[a_, b__], y___]:=SScalar[a, Sh[x, b, y]] /; NumericQ[a];
rules1 ={S[x___, a_SPlus, y___]:>SPlus@@(S[x,#,y]& /@ a),S[x___,SScalar[b_ ,a__S],y___]/;NumericQ[b]:>SScalar[b, S[x,a,y]],
S[___,-1,___]:>0, S[___,1,___]:>0, S[___,0,___]:>0};
rules2 = {S[a___,S[b__], c___]:>S[a,b,c], SPlus[a_S]:>a, SPlus[x___, a__SPlus, y___]:>SPlus[x, a, y]};
rules3 = {S[x___,a_Plus,y___]:>Plus@@(S[x, #, y]& /@ a),S[x___,b_ a_S ,y___]/;NumericQ[b]:>b S[x,a,y]};
res = Evaluate[Sh@@args] /. {Plus->P, Times->T}/.{SPlus->Plus, SScalar->Times} //. Union[rules2, rules3];
res = res /. {S->Symb, Plus->SymbPlus, Times->SymbScalar} /. {P->Plus, T->Times};
Return[res]]


(*In order to differentiate clearly between the + and * on the vectorspace of rational functions vs on the tensor product space of symbols, we introduce the symbolic expressions SymbNull, SymbPlus and SymbScalar.*)
(* Basic recursive definition of the shuffle operation:
SymbShuffle[Symb[a_, b___], Symb[c_, d___]]:=SymbPlus[Symb[a, SymbShuffle[Symb[b], Symb[c, d]]], Symb[c, SymbShuffle[Symb[a, b], Symb[d]]]]
SymbShuffle[Symb[], a_]:=a;
SymbShuffle[a_, Symb[]]:=a;
SymbShuffle[a_, b_, c__]:=SymbShuffle[SymbShuffle[a, b], c];*)
(*SymbShuffle[a_]:=a;*)
(* Establish vector space axioms on the tensor product space: *)
SymbPlus[a__Symb, SymbPlus[b__Symb]]:= SymbPlus[a, b];
SymbPlus[SymbPlus[a__Symb], b__Symb]:= SymbPlus[a, b];
SymbPlus[SymbPlus[a__Symb], SymbPlus[b__Symb]] := SymbPlus[a, b];
SymbPlus[SymbPlus[a__Symb], SymbPlus[b__Symb], SymbPlus[c__Symb]]:=SymbPlus[a, b, c];
(*SymbPlus[a___]:=Flatten[SymbPlus[a]];*)
(*SymbPlus[SymbPlus[a__Symb], SymbPlus[b1___Symb, SymbScalar[q_, Symb[b2__]], b3___Symb]]:= SymbPlus[a, b1, SymbScalar[q, b2], b3];*)
SymbPlus[b_Symb]:= b;
SymbPlus[SymbScalar[q_Rational, a___Symb]]:= SymbScalar[q, a];
SymbPlus[SymbScalar[q_Integer, a___Symb]]:= SymbScalar[q, a];
SymbPlus[a___Symb, SymbNull, b___Symb]:= SymbPlus[a, b];
SymbPlus[SymbScalar[q_Rational, a_Symb], SymbScalar[-q_Rational, a_Symb]] := SymbNull;
SymbPlus[SymbScalar[q_Integer, a_Symb], SymbScalar[-q_Integer, a_Symb]] := SymbNull;
SymbScalar[q_Rational, SymbScalar[p_Rational, a_Symb]]:= SymbScalar[q*p, a];
SymbScalar[q_Integer, SymbScalar[p_Integer, a_Symb]]:= SymbScalar[q*p, a];
(*SymbScalar[1, a__Symb]:= a;*)
(*Usually, we don't want to distribute:
SymbScalar[q_Rational, SymbPlus[a_Symb, b___Symb]]:= SymbPlus[SymbScalar[q, a], SymbScalar[q, b]];
SymbScalar[Plus[q_Rational, p_Rational], a___Symb] := SymbPlus[SymbScalar[q, a], SymbScalar[p, a]];
SymbScalar[Plus[q_Integer, p_Integer], a___Symb] := SymbPlus[SymbScalar[q, a], SymbScalar[p, a]];*)
(* Establish multilinearity and associativity of the tensor product: *)
Symb[a_, SymbPlus[b_]] := Symb[a, b];
Symb[a___, SymbPlus[b__Symb], c___]:= SymbPlus @@ (Symb[a, #, c]& /@ {b});
Symb[a___, SymbScalar[q_Rational, b__Symb], c___] := SymbScalar[q, Symb[a, b, c]];
Symb[a___, SymbScalar[q_Integer, b__Symb], c___] := SymbScalar[q, Symb[a, b, c]];
Symb[a___, Symb[b__], c___]:= Symb[a, b, c];
(* Establish linearity of the shuffle operation:
SymbShuffle[a___, SymbScalar[q_, b_Symb], c___]:=SymbScalar[q, SymbShuffle[a, b, c]];
SymbShuffle[a___, SymbPlus[b__Symb], c___]:=SymbPlus @@ (SymbShuffle[a, #, c]& /@ {b});
SymbShuffle[a___, SymbPlus[b1__Symb, SymbScalar[q_Rational, b2__Symb]], c___]:= SymbPlus[SymbShuffle[a, b1, c], SymbScalar[q, SymbShuffle[a, b2, c]]];
SymbShuffle[a___, SymbPlus[b1__Symb, SymbScalar[q_Integer, b2__Symb]], c___]:= SymbPlus[SymbShuffle[a, b1, c], SymbScalar[q, SymbShuffle[a, b2, c]]];*)


End[];


EndPackage[];
