(* ::Package:: *)

BeginPackage["Factorizations`"];


GetPIsFromRs::usage = "In: list of arbitrary rational functions. Out: list of irreducible polynomials over Q.";
GetPIBarsFromPIs::usage = "In: list of irreducible polynomials. Out: list of irreducible polynomials.";
HandleDegZero::usage = "In: list of polynomials, some of which may be of degree zero. Out: list of polynomials where the degree zero polynomials have been treated correctly.";
FindPrimes1::usage = "In: list of multiplicatively independent polynomials with n variables. Out: list of n not necessarily different prime numbers.";
FindPrimes2::usage = "same as FindPrimes1";
FindPrimes3::usage = "same as FindPrimes2";
S3IdOrbit::usage = "In: rational function. Out: the group action of S3 applied on this function.";
S3SkOrbit::usage = "In: k-tuple of rational functions. Out the group action of S3 x Sk applied on this k-tuple";


Begin["`Private`"];


Unprotect["`*"];
Clear["`*"];


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


End[];


EndPackage[];
