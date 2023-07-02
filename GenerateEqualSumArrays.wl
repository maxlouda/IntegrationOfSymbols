(* ::Package:: *)

BeginPackage["GenerateEqualSumArrays`"];


AllSigns::usage = "In: integer len. Out: list of lists which contain all possible signs distributed over a list of length len with the condition that the first argument has to be +1. Example: AllSigns[3]=={{1,1,1}, {1,1,-1},{1,-1,1},{1,-1,-1}}.";

AddSignsSymb::usage = "In: list of integers. Out: list of lists in which the original list together with all signed variants of it appear.";

SumAbsValues::usage = "In: integer N, integer len. Out: list of lists, which contain all signed integers whose absolute values sum to N and are (potentially) filled up with zeroes until they have length len.";


Begin["`Private`"];


Unprotect["`*"];
Clear["`*"];


AllSigns[len_] := AllSigns[len]=
    Block[{res},
        res = Table[PadLeft[ConstantArray[1, i], len - 1, 0], {i, 0, 
            len - 1}];
        res = Flatten[Permutations /@ res, 1];
        res = Map[(-1) ^ #&, res, {2}];
        res = Prepend[#, 1]& /@ res;
        If[len == 1,
            Return[{1}]
            ,
            Return[res]
        ];
    ];


AddSignsSymb[list_] := AddSignsSymb[list] = 
    Block[{res, a, b},
        res = Reap[
                Do[
                    a = Table[list[[i]], 2 ^ (Length[list[[i]]]-1)];
                    b = AllSigns[Length[list[[i]]]];
                    Sow[a * b]
                    ,
                    {i, 0, Length[list]}
                ]
            ][[2]];
        Return[Flatten[res,2]]
    ]


SumAbsValues[N_, len_] :=
    Block[{res},
        res = Select[IntegerPartitions[N], Length[#] <= len&];
        res = AddSignsSymb[res];
        res = Map[Join[#, ConstantArray[0, len - Length[#]]]&, res, {
            1}];
        (*res = Flatten[Permutations /@ res, 1];*);
        Return[DeleteDuplicates[res]]
    ];


End[];


EndPackage[];
