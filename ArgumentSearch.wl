(* ::Package:: *)

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


GetPIsFromRs[Rlist_] :=
    Block[{res},
        res = Sort[DeleteCases[DeleteDuplicates[First /@ Flatten[Map[
            FactorList[#]&, Flatten[NumeratorDenominator[Factor[FullSimplify[Rlist
            ]]]], 1], 1]], 1, 1]];
        Return[HandleDegZero[res]]
    ]


GetPIBarsFromPIs[PIlist_] :=
    Block[{res},
        res = DeleteDuplicates[First /@ Flatten[Map[FactorList[#]&, Union[
            Map[Plus[#[[1]], #[[2]]]&, Tuples[Prepend[PIlist, 1], 2], 1], Map[Plus[
            #[[1]], -#[[2]]]&, Tuples[Prepend[PIlist, 1], 2], 1]], 1], 1]];
        Return[DeleteElements[HandleDegZero[res],{0}]]
    ]


HandleDegZero[polylist_] :=
    Block[{res, vars, intgs},
        vars = Union @@ (Variables[Apply[List, #]]& /@ polylist);
        intgs = Select[polylist, Exponent[#, vars] == ConstantArray[0,
             Length[vars]]&];
        res = DeleteElements[polylist, intgs];
        intgs = DeleteElements[DeleteDuplicates[First /@ Flatten[Map[
            FactorInteger[#]&, intgs], 1]], {-1, 1}];
        res = Join[res, intgs];
        Return[res]
    ]


FindPrimes1[PIlist_] :=
    Block[{res, vars, primes, functions, found, n, pos},
        vars = Union @@ (Variables[Apply[List, #]]& /@ PIlist);
        functions =
            Map[
                Function[Evaluate[vars],
                    Evaluate[#]
                ]&
                ,
                PIlist
            ];
        found = False;
        n = 5;
        pos = 1;
        While[
            found == False
            ,
            primes = Tuples[Prime /@ Range[50, 50 + n], Length[vars]];
            res = Outer[#2 @@ #1&, primes, functions, 1];
            found = SelectFirst[res, Length[DeleteDuplicates[Abs/@#]] == Length[
                functions]&,False];
            pos = FirstPosition[res, found];
            n++;
        ];
        Return[Flatten[primes[[pos]]]]
    ]


FindPrimes2[PIlist_] :=
    Block[{res, vars, primes, functions, found, n, pos},
        vars = Union @@ (Variables[Apply[List, #]]& /@ PIlist);
        functions =
            Map[
                Function[Evaluate[vars],
                    Evaluate[#]
                ]&
                ,
                PIlist
            ];
        found = False;
        n = 5;
        pos = 1;
        While[
            found == False
            ,
            primes = Tuples[Prime /@ Range[100, 100 + n], Length[vars]];
            res = Outer[#2 @@ #1&, primes, functions, 1];
            found = SelectFirst[res, Length[DeleteDuplicates[Abs/@#]] == Length[
                functions]&,False];
            pos = FirstPosition[res, found];
            n++;
        ];
        Return[Flatten[primes[[pos]]]]
    ]


FindPrimes3[PIlist_] :=
    Block[{res, vars, primes, functions, found, n, pos},
        vars = Union @@ (Variables[Apply[List, #]]& /@ PIlist);
        functions =
            Map[
                Function[Evaluate[vars],
                    Evaluate[#]
                ]&
                ,
                PIlist
            ];
        found = False;
        n = 5;
        pos = 1;
        While[
            found == False
            ,
            primes = Tuples[Prime /@ Range[150, 150 + n], Length[vars]];
            res = Outer[#2 @@ #1&, primes, functions, 1];
            found = SelectFirst[res, Length[DeleteDuplicates[Abs/@#]] == Length[
                functions]&,False];
            pos = FirstPosition[res, found];
            n++;
        ];
        Return[Flatten[primes[[pos]]]]
    ]


S3IdOrbit[R_]:= Block[{res},
res = {R, 1-R, 1/R, 1/(1-R), 1 - 1/R, R/(R-1)};
Return[FullSimplify[res]]]


S3SkOrbit[Rs_]:= Block[{res},
res = Flatten[S3IdOrbit/@(Permutations[Rs]),1];
Return[res]]


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
PIsEvalAtPrimes1 = Map[#[Sequence@@primes1]&, Map[Function[Evaluate[vars], Evaluate[#]]&, PIlist]];
PIsEvalAtPrimes2 = Map[#[Sequence@@primes2]&, Map[Function[Evaluate[vars], Evaluate[#]]&, PIlist]];
PIsEvalAtPrimes3 = Map[#[Sequence@@primes3]&, Map[Function[Evaluate[vars], Evaluate[#]]&, PIlist]];
While[nn<=n,
(*ntuples = SumAbsValues[nn,Length[PIlist]]*)
ntuples = DeleteDuplicates[Reverse /@ Sort /@ SumAbsValues[nn, Length[PIlist]]];
Do[ntuplesRed = DeleteCases[ntuples[[i]],0];
temp = DeleteDuplicates[Times@@@((Power @@ {#, ntuplesRed})& /@ Flatten[Permutations/@Subsets[PIlist, {Length[ntuplesRed]}],1])];
temp = Select[temp, Not[MemberQ[res,Expand[#]]]&];
temp = Join[temp, -1*temp];
(*functions = Map[Function[Evaluate[vars], Evaluate[#]]&, MapThread[Times, {(1-#)& /@ temp, Denominator/@temp}]]*)
functions = Map[Function[Evaluate[vars], Evaluate[#]]&, Numerator[Factor[1-#]]& /@ temp];
numeratorPi1 = Map[#[Sequence@@primes1]&, functions];
numeratorPi2 = Map[#[Sequence@@primes2]&, functions];
numeratorPi3 = Map[#[Sequence@@primes3]&, functions];
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
numeratorPi = Map[#[Sequence@@primes]&, functions];
If[Length[FindGoodIndices[numeratorPi, PIsEvalAtPrimes]]==Length[numeratorPi], bool = 1, bool = 0];
Return[bool]]


GenerateRSn[rs1_, k_, PIlist_]:= Block[{vars, res, bools, subs, bools1, bools2, bools3, len, primes1, primes2, primes3, PIsEvalAtPrimes1, PIsEvalAtPrimes2, PIsEvalAtPrimes3},
len = Length[PIlist];
primes1 = FindPrimes1[PIlist];
primes2 = FindPrimes2[PIlist];
primes3 = FindPrimes3[PIlist];
vars = Union @@ (Variables[Apply[List, #]]& /@ PIlist);
PIsEvalAtPrimes1 = Map[#[Sequence@@primes1]&, Map[Function[Evaluate[vars], Evaluate[#]]&, PIlist]];
PIsEvalAtPrimes2 = Map[#[Sequence@@primes2]&, Map[Function[Evaluate[vars], Evaluate[#]]&, PIlist]];
PIsEvalAtPrimes3 = Map[#[Sequence@@primes3]&, Map[Function[Evaluate[vars], Evaluate[#]]&, PIlist]];
subs = Subsets[rs1,{k}];
res = Map[Subsets[#,{2}]&,subs,{1}];
bools1 = DetermineIfGoodTuples[#,PIsEvalAtPrimes1, primes1, vars]& /@ res;
bools2 = DetermineIfGoodTuples[#,PIsEvalAtPrimes2, primes2, vars]& /@ res;
bools3 = DetermineIfGoodTuples[#,PIsEvalAtPrimes3, primes3, vars]& /@ res;
bools = bools1 * bools2 * bools3;
res = Pick[subs, bools, 1];
res = DeleteDuplicates[Flatten[Permutations/@ Flatten[S3IdOrbit /@ res, 1], 1]];
Return[res]]


GenerateRSnRefined[rs1_, k_, PIlist_]:=Block[{res, tuples, len},
res = GenerateRSn[rs1, k, PIlist];
len = Length[res[[1]]];
tuples = Tuples[{1,-1}, len];
res = DeleteDuplicates[Simplify/@Flatten[Map[Function[x,MapThread[If[#2==1,#1,1/#1]&,{x,#}]&/@tuples],res],1]];
Return[res]]
