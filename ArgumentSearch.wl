(* ::Package:: *)

(*If necessary, add the address to the file where the packages are saved to the $Path variable.*)


BeginPackage["ArgumentSearch`", {"Factorizations`", "GenerateEqualSumArrays`"}];


GenerateRS1::usage = "In: user defined cut-off integer n, list of multiplicatively independent polynomials. Out: candidates for depth one arguments in list RS1.";


Begin["`Private`"];


Unprotect["`*"];
Clear["`*"];


GenerateRS1[n_, PIlist_]:= Block[{vars, res={}, ntuples, nn=1, temp, perm, len, primes, negexps, numeratorPi},
perm = Permutations[PIlist];
len = Length[PIlist];
primes = FindPrimes[PIlist];
vars = Union @@ (Variables[Apply[List, #]]& /@ PIlist);
While[nn<n,
ntuples = SumAbsValues[nn,Length[PIlist]];
Do[temp = Times@@@DeleteDuplicates[MapThread[Power,{perm, Table[ntuples[[i]], Factorial[len]]},1]];
temp = Select[temp, Not[MemberQ[res,#]]&];
negexps = Denominator/@temp;
numeratorPi = ,
{i,Length[ntuples]}];
nn++;];
Return[res]]


End[];


EndPackage[];
