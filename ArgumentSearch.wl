(* ::Package:: *)

(*If necessary, add the address to the file where the packages are saved to the $Path variable.*)


BeginPackage["ArgumentSearch`", {"Factorizations`", "GenerateEqualSumArrays`"}];


GenerateRS1::usage = "In: user defined cut-off integer n, list of multiplicatively independent polynomials. Out: candidates for depth one arguments in list RS1.";
IfDivisibleThenDivide::usage = "In: integer num, listPIsEvalAtPrimes. Out: -1, 1, or zero";
FindGoodIndices::usage = "In: list tobechecked, list PIsEvalAtPrimes. Out: all good indices of tobechecked.";
DetermineIfGoodTuples::usage = "In: list of tuples, PIsEvalAtPrimes, primes, vars. Out: boolean.";
GenerateRSn::usage = "In: list rs1, integer k. Out: all admissible k-tuples of arguments.";


Begin["`Private`"];


Unprotect["`*"];
Clear["`*"];


IfDivisibleThenDivide[num_, PIsEvalAtPrimes_] := 
Block[{res = num, divBools, pos, eval},
eval = DeleteCases[Reverse[Sort[Abs/@PIsEvalAtPrimes]],0|1|-1];
While[Not[Or[res == 0, res == 1, res == -1]],
divBools = Divisible[res, #]& /@ eval;
pos = FirstPosition[divBools, True, {-1}];
If[pos[[1]]==-1, res = 0, 
res = res / eval[[pos[[1]]]]];
];
Return[res]]


FindGoodIndices[tobechecked_, PIsEvalAtPrimes_]:=
Block[{res},
res = IfDivisibleThenDivide[#, PIsEvalAtPrimes]& /@ tobechecked;
res = Flatten[Position[Abs[res],1]];
Return[res]
]


GenerateRS1[n_, PIlist_]:= Block[{vars, res={}, PIsEvalAtPrimes1, PIsEvalAtPrimes2, PIsEvalAtPrimes3, ntuples, ntuplesRed, nn=1, temp, len, primes1, primes2, primes3, functions, numeratorPi1, numeratorPi2, numeratorPi3, goodInd1, goodInd2, goodInd3, goodInd},
len = Length[PIlist];
primes1 = FindPrimes1[PIlist];
primes2 = FindPrimes2[PIlist];
primes3 = FindPrimes3[PIlist];
vars = Union @@ (Variables[Apply[List, #]]& /@ PIlist);
PIsEvalAtPrimes1 = Flatten[Map[#[primes1]&, Map[Function[Evaluate[vars], Evaluate[#]]&, PIlist]]];
PIsEvalAtPrimes2 = Flatten[Map[#[primes2]&, Map[Function[Evaluate[vars], Evaluate[#]]&, PIlist]]];
PIsEvalAtPrimes3 = Flatten[Map[#[primes3]&, Map[Function[Evaluate[vars], Evaluate[#]]&, PIlist]]];
While[nn<=n,
(*ntuples = SumAbsValues[nn,Length[PIlist]]*)
ntuples = DeleteDuplicates[Reverse /@ Sort /@ SumAbsValues[nn, Length[PIlist]]];
Do[ntuplesRed = DeleteCases[ntuples[[i]],0];
temp = DeleteDuplicates[Times@@@((Power @@ {#, ntuplesRed})& /@ Flatten[Permutations/@Subsets[PIlist, {Length[ntuplesRed]}],1])];
temp = Select[temp, Not[MemberQ[res,Expand[#]]]&];
temp = Join[temp, -1*temp];
(*functions = Map[Function[Evaluate[vars], Evaluate[#]]&, MapThread[Times, {(1-#)& /@ temp, Denominator/@temp}]]*)
functions = Map[Function[Evaluate[vars], Evaluate[#]]&, Numerator[Factor[1-#]]& /@ temp];
numeratorPi1 = Flatten[Map[#[primes1]&, functions]];
numeratorPi2 = Flatten[Map[#[primes2]&, functions]];
numeratorPi3 = Flatten[Map[#[primes3]&, functions]];
goodInd1 = FindGoodIndices[numeratorPi1, PIsEvalAtPrimes1];
goodInd2 = FindGoodIndices[numeratorPi2, PIsEvalAtPrimes2];
goodInd3 = FindGoodIndices[numeratorPi3, PIsEvalAtPrimes3];
goodInd = Intersection[goodInd1, goodInd2, goodInd3];
temp = temp[[goodInd]];
res = Union[res, Simplify[Factor[Flatten[S3IdOrbit /@ temp]]]],
{i,Length[ntuples]}];
nn++;];
Return[res]]


DetermineIfGoodTuples[tuples_, PIsEvalAtPrimes_, primes_, vars_] := Block[{res, functions, bool, numeratorPi},
functions =  Map[Function[Evaluate[vars], Evaluate[#]]&, Map[Numerator[Factor[tuples[[#]][[1]]-tuples[[#]][[2]]]]&, Range[Length[tuples]]]];
numeratorPi = Flatten[Map[#[primes]&, functions]];
If[Length[FindGoodIndices[numeratorPi, PIsEvalAtPrimes]]==Length[numeratorPi], bool = 1, bool = 0];
Return[bool]]


GenerateRSn[rs1_, k_, PIlist_]:= Block[{vars, res, bools, subs, bools1, bools2, bools3, len, primes1, primes2, primes3, PIsEvalAtPrimes1, PIsEvalAtPrimes2, PIsEvalAtPrimes3},
len = Length[PIlist];
primes1 = FindPrimes1[PIlist];
primes2 = FindPrimes2[PIlist];
primes3 = FindPrimes3[PIlist];
vars = Union @@ (Variables[Apply[List, #]]& /@ PIlist);
PIsEvalAtPrimes1 = Flatten[Map[#[primes1]&, Map[Function[Evaluate[vars], Evaluate[#]]&, PIlist]]];
PIsEvalAtPrimes2 = Flatten[Map[#[primes2]&, Map[Function[Evaluate[vars], Evaluate[#]]&, PIlist]]];
PIsEvalAtPrimes3 = Flatten[Map[#[primes3]&, Map[Function[Evaluate[vars], Evaluate[#]]&, PIlist]]];
subs = Subsets[rs1,{k}];
res = Map[Subsets[#,{2}]&,subs,{1}];
bools1 = DetermineIfGoodTuples[#,PIsEvalAtPrimes1, primes1, vars]& /@ res;
bools2 = DetermineIfGoodTuples[#,PIsEvalAtPrimes2, primes2, vars]& /@ res;
bools3 = DetermineIfGoodTuples[#,PIsEvalAtPrimes3, primes3, vars]& /@ res;
bools = bools1 * bools2 * bools3;
res = Pick[subs, bools, 1];
res = DeleteDuplicates[Flatten[Permutations/@ Flatten[S3IdOrbit /@ res, 1], 1]];
Return[res]]


End[];


EndPackage[];
