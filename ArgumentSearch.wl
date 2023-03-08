(* ::Package:: *)

(*If necessary, add the address to the file where the packages are saved to the $Path variable.*)


BeginPackage["ArgumentSearch`", {"Factorizations`", "GenerateEqualSumArrays`"}];


GenerateRS1::usage = "In: user defined cut-off integer n, list of multiplicatively independent polynomials. Out: candidates for depth one arguments in list RS1.";


Begin["`Private`"];


Unprotect["`*"];
Clear["`*"];


GenerateRS1[n_, PIlist_]:= Block[{res},
res = PIlist;
Return[res]]


End[];


EndPackage[];
