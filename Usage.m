(* ::Package:: *)

(* ::Section:: *)
(*General definitions*)


G::usage = "G[a1,...,an,z] represents the multiple polylogarithm G(\!\(\*SubscriptBox[\(a\), \(1\)]\),...,\!\(\*SubscriptBox[\(a\), \(n\)]\);z). The indices \!\(\*SubscriptBox[\(a\), \(i\)]\) can be numbers, symbolic constants or functions of other variables.";
H::usage = "H[a1,...,an,z] represents the harmonic polylogarithm H(\!\(\*SubscriptBox[\(a\), \(1\)]\),...,\!\(\*SubscriptBox[\(a\), \(n\)]\);z). The indices \!\(\*SubscriptBox[\(a\), \(i\)]\) have to from the set {0,1,-1}.";
Li::usage = "Li[{m1,...,mk},{z1,...,zk}] represents the multiple polylogarithm \!\(\*SubscriptBox[\(Li\), \(\*SubscriptBox[\(m\), \(1\)],  ... , \*SubscriptBox[\(m\), \(k\)]\)]\)(\!\(\*SubscriptBox[\(z\), \(1\)]\),...,\!\(\*SubscriptBox[\(z\), \(n\)]\)). The arguments \!\(\*SubscriptBox[\(z\), \(i\)]\) can be numbers, symbolic constants or functions of other variables. The indices \!\(\*SubscriptBox[\(m\), \(i\)]\) have to fpositive integers.";


GToH::usage = "GToH[expr] replaces every G[a1,...,an,z] in the expression expr with the corresponding H[a1,...,an,z], if the indices of the multiple polylogarithm are integers from the set {-1,0,1}.";
GToHPL::usage = "GToHPL[expr] replaces every G[a1,...,an,z] in the expression expr with the corresponding HPL[{a1,...,an},z], if the indices of the multiple polylogarithm are integers from the set {-1,0,1}.";
HToG::usage = "HToG[expr] replaces every H[a1,...,an,z] in the expression expr with the corresponding G[a1,...,an,z].";
HToHPL::usage = "HToHPL[expr] converts every H[a1,...,an,z] in expr to the HPL[{a1,...,an},z] notation.";
HPLToH::usage = "HPLToH[expr] replaces every HPL in the expression expr with the corresponding H. Automatically converts from the a\[Minus]notation defined by HPL to the m\[Minus]notation.";
HPLToG::usage = "HPLToG[expr] is equivalent to HToG[HPLToH[expr]].";
GToLi::usage = "GToLi[expr] converts every G in the expression expr to the Li notation.";
LiToG::usage = "LiToG[expr] converts every Li in the expression expr to the G notation.";


(* ::Section:: *)
(*Shuffle and stuffle algebra*)


ShuffleG::usage = "ShuffleG[expr] expands all products of G-functions that end in the same argument into a sum of shuffles.";

StuffleLi::usage = "StuffleLi[expr] expands all products of Li-functions into a sum of stuffles.";

DecomposeToLyndonWords::usage = "DecomposeToLyndonWords[expr] decomposes all G-functions into a sum of products of Lyndon words. 
The set of letters and their ordering can be passed as a list via the optional argument Alphabet. The default value is {0,1,-1}. 
MPLs which depend on a letter that is not in the Alphabet are not decomposed. The same applies to MPLs that would be decomposed into divergent quantities.";

ExtractZeroes::usage = "ExtractZeroes[expr] uses the shuffle algebra to remove all trailing zeroes from all G-functions in expr.";


(* ::Section:: *)
(*Hopf algebra and coproduct*)


Delta::usage = "Delta[expr] applies the coproduct \[CapitalDelta] on expr. expr can be any valid MPL expression up to weight 8, i.e., a polynomial in the following objects:
* Rational/algebraic functions
* The transcendental functions G, H, Li, HPL, PolyLog, Log, cG.
* The transcendental constants Pi, Zeta, MZV.\n
Delta[{i1,...,ik},expr] applies the map \!\(\*SubscriptBox[\(\[CapitalDelta]\), \(\(\*SubscriptBox[\(i\), \(1\)] ... \) \*SubscriptBox[\(i\), \(k\)]\)]\) to expr, and return the (\!\(\*SubscriptBox[\(i\), \(1\)]\)...\!\(\*SubscriptBox[\(i\), \(k\)]\)) component of the coproduct of expr.";

CT::usage = "CT[a1,...,an] represents the tensor product \!\(\*SubscriptBox[\(a\), \(1\)]\)\[CircleTimes]...\[CircleTimes]\!\(\*SubscriptBox[\(a\), \(n\)]\).";

Antipode::usage = "Antipode[expr] computes the antipode of the MPL expression expr (up to weight 8).";

ProductProjector::usage = "ProductProjector[expr] returns the projection of expr into the space of indecomposables. 
Here expr can be any valid MPL expression up to weight 8, or a symbol expression.";

Cobracket::usage = "Cobracket[expr] applies the cobracket \[Delta] on expr. expr can be any valid MPL expression up to weight 8.\n
Cobracket[{p,q},expr] returns the (p,q) component of the cobracket on expr.";

CTW::usage = "CTW[a,b] represents the wedge product a\[Wedge]b.";


(* ::Subsection::Closed:: *)
(*others*)


Delta11::usage = "Delta11[f] returns the (1,1) component of the coproduct of f.";
Delta21::usage = "Delta21[f] returns the (2,1) component of the coproduct of f.";
Delta12::usage = "Delta12[f] returns the (1,2) component of the coproduct of f.";
Delta111::usage = "Delta111[f] returns the (1,1,1) component of the coproduct of f.";
Delta31::usage = "Delta31[f] returns the (3,1) component of the coproduct of f.";
Delta13::usage = "Delta13[f] returns the (1,3) component of the coproduct of f.";
Delta22::usage = "Delta22[f] returns the (2,2) component of the coproduct of f.";
Delta211::usage = "Delta211[f] returns the (2,1,1) component of the coproduct of f.";
Delta121::usage = "Delta121[f] returns the (1,2,1) component of the coproduct of f.";
Delta112::usage = "Delta112[f] returns the (1,1,2) component of the coproduct of f.";
Delta1111::usage = "Delta1111[f] returns the (1,1,1,1) component of the coproduct of f.";
Delta41::usage = "Delta41[f] returns the (4,1) component of the coproduct of f.";
Delta14::usage = "Delta14[f] returns the (1,4) component of the coproduct of f.";
Delta32::usage = "Delta32[f] returns the (3,2) component of the coproduct of f.";
Delta23::usage = "Delta23[f] returns the (2,3) component of the coproduct of f.";
Delta311::usage = "Delta311[f] returns the (3,1,1) component of the coproduct of f.";
Delta131::usage = "Delta131[f] returns the (1,3,1) component of the coproduct of f.";
Delta113::usage = "Delta113[f] returns the (1,1,3) component of the coproduct of f.";
Delta221::usage = "Delta221[f] returns the (2,2,1) component of the coproduct of f.";
Delta212::usage = "Delta212[f] returns the (2,1,2) component of the coproduct of f.";
Delta122::usage = "Delta122[f] returns the (1,2,2) component of the coproduct of f.";
Delta2111::usage = "Delta2111[f] returns the (2,1,1,1) component of the coproduct of f.";
Delta1211::usage = "Delta1211[f] returns the (1,2,1,1) component of the coproduct of f.";
Delta1121::usage = "Delta1121[f] returns the (1,1,2,1) component of the coproduct of f.";
Delta1112::usage = "Delta1112[f] returns the (1,1,1,2) component of the coproduct of f.";
Delta11111::usage = "Delta11111[f] returns the (1,1,1,1,1) component of the coproduct of f.";
Delta51::usage = "Delta51[f] returns the (5,1) component of the coproduct of f.";
Delta15::usage = "Delta15[f] returns the (1,5) component of the coproduct of f.";
Delta42::usage = "Delta42[f] returns the (4,2) component of the coproduct of f.";
Delta24::usage = "Delta24[f] returns the (2,4) component of the coproduct of f.";
Delta411::usage = "Delta411[f] returns the (4,1,1) component of the coproduct of f.";
Delta141::usage = "Delta141[f] returns the (1,4,1) component of the coproduct of f.";
Delta114::usage = "Delta114[f] returns the (1,1,4) component of the coproduct of f.";
Delta33::usage = "Delta33[f] returns the (3,3) component of the coproduct of f.";
Delta321::usage = "Delta321[f] returns the (3,2,1) component of the coproduct of f.";
Delta312::usage = "Delta312[f] returns the (3,1,2) component of the coproduct of f.";
Delta231::usage = "Delta231[f] returns the (2,3,1) component of the coproduct of f.";
Delta213::usage = "Delta213[f] returns the (2,1,3) component of the coproduct of f.";
Delta132::usage = "Delta132[f] returns the (1,3,2) component of the coproduct of f.";
Delta123::usage = "Delta123[f] returns the (1,2,3) component of the coproduct of f.";
Delta3111::usage = "Delta3111[f] returns the (3,1,1,1) component of the coproduct of f.";
Delta1311::usage = "Delta1311[f] returns the (1,3,1,1) component of the coproduct of f.";
Delta1131::usage = "Delta1131[f] returns the (1,1,3,1) component of the coproduct of f.";
Delta1113::usage = "Delta1113[f] returns the (1,1,1,3) component of the coproduct of f.";
Delta222::usage = "Delta222[f] returns the (2,2,2) component of the coproduct of f.";
Delta2211::usage = "Delta2211[f] returns the (2,2,1,1) component of the coproduct of f.";
Delta2121::usage = "Delta2121[f] returns the (2,1,2,1) component of the coproduct of f.";
Delta2112::usage = "Delta2112[f] returns the (2,1,1,2) component of the coproduct of f.";
Delta1221::usage = "Delta1221[f] returns the (1,2,2,1) component of the coproduct of f.";
Delta1212::usage = "Delta1212[f] returns the (1,2,1,2) component of the coproduct of f.";
Delta1122::usage = "Delta1122[f] returns the (1,1,2,2) component of the coproduct of f.";
Delta21111::usage = "Delta21111[f] returns the (2,1,1,1,1) component of the coproduct of f.";
Delta12111::usage = "Delta12111[f] returns the (1,2,1,1,1) component of the coproduct of f.";
Delta11211::usage = "Delta11211[f] returns the (1,1,2,1,1) component of the coproduct of f.";
Delta11121::usage = "Delta11121[f] returns the (1,1,1,2,1) component of the coproduct of f.";
Delta11112::usage = "Delta11112[f] returns the (1,1,1,1,2) component of the coproduct of f.";
Delta111111::usage = "Delta111111[f] returns the (1,1,1,1,1,1) component of the coproduct of f.";
Delta61::usage = "Delta61[f] returns the (6,1) component of the coproduct of f.";
Delta16::usage = "Delta16[f] returns the (1,6) component of the coproduct of f.";
Delta52::usage = "Delta52[f] returns the (5,2) component of the coproduct of f.";
Delta25::usage = "Delta25[f] returns the (2,5) component of the coproduct of f.";
Delta511::usage = "Delta511[f] returns the (5,1,1) component of the coproduct of f.";
Delta151::usage = "Delta151[f] returns the (1,5,1) component of the coproduct of f.";
Delta115::usage = "Delta115[f] returns the (1,1,5) component of the coproduct of f.";
Delta43::usage = "Delta43[f] returns the (4,3) component of the coproduct of f.";
Delta34::usage = "Delta34[f] returns the (3,4) component of the coproduct of f.";
Delta421::usage = "Delta421[f] returns the (4,2,1) component of the coproduct of f.";
Delta412::usage = "Delta412[f] returns the (4,1,2) component of the coproduct of f.";
Delta241::usage = "Delta241[f] returns the (2,4,1) component of the coproduct of f.";
Delta214::usage = "Delta214[f] returns the (2,1,4) component of the coproduct of f.";
Delta142::usage = "Delta142[f] returns the (1,4,2) component of the coproduct of f.";
Delta124::usage = "Delta124[f] returns the (1,2,4) component of the coproduct of f.";
Delta4111::usage = "Delta4111[f] returns the (4,1,1,1) component of the coproduct of f.";
Delta1411::usage = "Delta1411[f] returns the (1,4,1,1) component of the coproduct of f.";
Delta1141::usage = "Delta1141[f] returns the (1,1,4,1) component of the coproduct of f.";
Delta1114::usage = "Delta1114[f] returns the (1,1,1,4) component of the coproduct of f.";
Delta331::usage = "Delta331[f] returns the (3,3,1) component of the coproduct of f.";
Delta313::usage = "Delta313[f] returns the (3,1,3) component of the coproduct of f.";
Delta133::usage = "Delta133[f] returns the (1,3,3) component of the coproduct of f.";
Delta322::usage = "Delta322[f] returns the (3,2,2) component of the coproduct of f.";
Delta232::usage = "Delta232[f] returns the (2,3,2) component of the coproduct of f.";
Delta223::usage = "Delta223[f] returns the (2,2,3) component of the coproduct of f.";
Delta3211::usage = "Delta3211[f] returns the (3,2,1,1) component of the coproduct of f.";
Delta3121::usage = "Delta3121[f] returns the (3,1,2,1) component of the coproduct of f.";
Delta3112::usage = "Delta3112[f] returns the (3,1,1,2) component of the coproduct of f.";
Delta2311::usage = "Delta2311[f] returns the (2,3,1,1) component of the coproduct of f.";
Delta2131::usage = "Delta2131[f] returns the (2,1,3,1) component of the coproduct of f.";
Delta2113::usage = "Delta2113[f] returns the (2,1,1,3) component of the coproduct of f.";
Delta1321::usage = "Delta1321[f] returns the (1,3,2,1) component of the coproduct of f.";
Delta1312::usage = "Delta1312[f] returns the (1,3,1,2) component of the coproduct of f.";
Delta1231::usage = "Delta1231[f] returns the (1,2,3,1) component of the coproduct of f.";
Delta1213::usage = "Delta1213[f] returns the (1,2,1,3) component of the coproduct of f.";
Delta1132::usage = "Delta1132[f] returns the (1,1,3,2) component of the coproduct of f.";
Delta1123::usage = "Delta1123[f] returns the (1,1,2,3) component of the coproduct of f.";
Delta31111::usage = "Delta31111[f] returns the (3,1,1,1,1) component of the coproduct of f.";
Delta13111::usage = "Delta13111[f] returns the (1,3,1,1,1) component of the coproduct of f.";
Delta11311::usage = "Delta11311[f] returns the (1,1,3,1,1) component of the coproduct of f.";
Delta11131::usage = "Delta11131[f] returns the (1,1,1,3,1) component of the coproduct of f.";
Delta11113::usage = "Delta11113[f] returns the (1,1,1,1,3) component of the coproduct of f.";
Delta2221::usage = "Delta2221[f] returns the (2,2,2,1) component of the coproduct of f.";
Delta2212::usage = "Delta2212[f] returns the (2,2,1,2) component of the coproduct of f.";
Delta2122::usage = "Delta2122[f] returns the (2,1,2,2) component of the coproduct of f.";
Delta1222::usage = "Delta1222[f] returns the (1,2,2,2) component of the coproduct of f.";
Delta22111::usage = "Delta22111[f] returns the (2,2,1,1,1) component of the coproduct of f.";
Delta21211::usage = "Delta21211[f] returns the (2,1,2,1,1) component of the coproduct of f.";
Delta21121::usage = "Delta21121[f] returns the (2,1,1,2,1) component of the coproduct of f.";
Delta21112::usage = "Delta21112[f] returns the (2,1,1,1,2) component of the coproduct of f.";
Delta12211::usage = "Delta12211[f] returns the (1,2,2,1,1) component of the coproduct of f.";
Delta12121::usage = "Delta12121[f] returns the (1,2,1,2,1) component of the coproduct of f.";
Delta12112::usage = "Delta12112[f] returns the (1,2,1,1,2) component of the coproduct of f.";
Delta11221::usage = "Delta11221[f] returns the (1,1,2,2,1) component of the coproduct of f.";
Delta11212::usage = "Delta11212[f] returns the (1,1,2,1,2) component of the coproduct of f.";
Delta11122::usage = "Delta11122[f] returns the (1,1,1,2,2) component of the coproduct of f.";
Delta211111::usage = "Delta211111[f] returns the (2,1,1,1,1,1) component of the coproduct of f.";
Delta121111::usage = "Delta121111[f] returns the (1,2,1,1,1,1) component of the coproduct of f.";
Delta112111::usage = "Delta112111[f] returns the (1,1,2,1,1,1) component of the coproduct of f.";
Delta111211::usage = "Delta111211[f] returns the (1,1,1,2,1,1) component of the coproduct of f.";
Delta111121::usage = "Delta111121[f] returns the (1,1,1,1,2,1) component of the coproduct of f.";
Delta111112::usage = "Delta111112[f] returns the (1,1,1,1,1,2) component of the coproduct of f.";
Delta1111111::usage = "Delta1111111[f] returns the (1,1,1,1,1,1,1) component of the coproduct of f.";
Delta71::usage = "Delta71[f] returns the (7,1) component of the coproduct of f.";
Delta17::usage = "Delta17[f] returns the (1,7) component of the coproduct of f.";
Delta62::usage = "Delta62[f] returns the (6,2) component of the coproduct of f.";
Delta26::usage = "Delta26[f] returns the (2,6) component of the coproduct of f.";
Delta611::usage = "Delta611[f] returns the (6,1,1) component of the coproduct of f.";
Delta53::usage = "Delta53[f] returns the (5,3) component of the coproduct of f.";
Delta35::usage = "Delta35[f] returns the (3,5) component of the coproduct of f.";
Delta5111::usage = "Delta5111[f] returns the (5,1,1,1) component of the coproduct of f.";
Delta44::usage = "Delta44[f] returns the (4,4) component of the coproduct of f.";
Delta41111::usage = "Delta41111[f] returns the (4,1,1,1,1) component of the coproduct of f.";
Delta311111::usage = "Delta311111[f] returns the (3,1,1,1,1,1) component of the coproduct of f.";
Delta2111111::usage = "Delta2111111[f] returns the (2,1,1,1,1,1,1) component of the coproduct of f.";
Delta11111111::usage = "Delta11111111[f] returns the (1,1,1,1,1,1,1,1) component of the coproduct of f.";


Delta4::usage = "Delta4[f] is a shorthand to obtain the (1,1,1,1) component of f, if f only consists of classical polylogs.";


(* ::Section:: *)
(*Symbols*)


SymbolMap::usage = "SymbolMap[expr] computes the symbol of expr as the maximal iteration of the coproduct.";

ComputeSymbol::usage = "ComputeSymbol[expr] computes the symbol of expr from dissections decorated polygons.";

ToSymbol::usage = "ToSymbol[f] converts the maximal (i.e. the (1,...,1) component) coproduct f into the corresponding symbol, dropping terms proportional to \[Pi].";

CiTi::usage = "Represents the symbol tensor \!\(\*SubscriptBox[\(a\), \(1\)]\)\[CircleTimes]...\[CircleTimes]\!\(\*SubscriptBox[\(a\), \(n\)]\).";

SymbolExpand::usage = "SymbolExpand[sym] maximally factors all the polynomial symbol letter and uses the additivity of the symbol sym to expand them out.";

GetSymbolAlphabet::usage = "GetSymbolAlphabet[sym] returns the symbol alphabet of the symbol sym.";

IntegrabilityCondition::usage = "IntegrabilityCondition[sym,p] returns the constraint from the inte- grability condition applied to the fac- tors p and p+1 in the symbol sym.";

DeltaDeconcatanation::usage = "DeltaDeconcatanation[sym] computes the deconcatenation coprod- uct of the symbol sym.";

AntipodeDeconcatanation::usage = "AntipodeDeconcatanation[sym] computes the deconcatenation an- tipode of the symbol sym.";

CobracketDeconcatenation::usage = "CobracketDeconcatenation[sym] computes the cobracket attached to the deconcatenation coproduct of the symbol sym.";


(* ::Section:: *)
(*Manipulating expressions*)


TranscendentalWeight::usage = "TranscendentalWeight[ex] returns the transcendental weight of ex. Does not evaluate on sums of transcendental terms.";

GetWeightTerms::usage = "GetWeightTerms[expr,n] returns all terms in expr that have transcendental weight n.";

GetGs::usage = "GetGs[expr] returns a set of all Gs in expr.";

GetGArguments::usage = "GetGArguments[expr] returns the set of all arguments of the Gs in expr.";

GetGIndices::usage = "GetGIndices[expr] returns the set of all entries in the weight vectors of the Gs in expr.";

NormalizeG::usage = "NormalizeG[expr] first applies ExtractZeroes, then replaces every G(\!\(\*SubscriptBox[\(a\), \(1\)]\),...,\!\(\*SubscriptBox[\(a\), \(n\)]\);z) in expr with G(\!\(\*SubscriptBox[\(a\), \(1\)]\)/z,...,\!\(\*SubscriptBox[\(a\), \(n\)]\)/z;1) if \!\(\*SubscriptBox[\(a\), \(n\)]\) \[NotEqual] 0.";

GArgumentSimplify::usage = "GArgumentSimplify[expr] simplifies the arguments of all G, Li, PolyLog and Log functions in expr.";

GatherTranscendentals::usage = "GatherTranscendentals[expr] groups terms in expr by transcendental object.";

GatherPrefactors::usage = "GatherPrefactors[expr] groups terms in expr by ratio- nal/algebraic prefacture.";

GCoefficientSimplify::usage = "GCoefficientSimplify[expr] is the same as GatherTranscendentals[expr], but applies the Simplify function to the coefficients.";


ExpandPolyLogs::usage = "ExpandPolyLogs[expr,{x,0,n}] expands all MPLs of the form G(\!\(\*OverscriptBox[\(a\), \(\[RightVector]\)]\); x), with \!\(\*OverscriptBox[\(a\), \(\[RightVector]\)]\) independent of x, in expr into a series around x = 0 up to order n.";


Ginsh::usage = "Ginsh[expr, {x1->c1, ...}] uses GiNaC to evaluate expr numerically. 
The second argument is a replacement list that specifies the numerical value \!\(\*SubscriptBox[\(c\), \(i\)]\) that should be assigned to the variable \!\(\*SubscriptBox[\(x\), \(i\)]\) in expr. \n
Ginsh has an option Debug. If set to True, the temporary files containing the GiNaC code are not deleted from the disc.";

RunPSLQ::usage = "RunPSLQ[num, list, prec] uses the PSLQ implementation with precision prec by P. Bertok to express num as a rational linear combination of the quantities in list. 
The argument num must be real.";

PSLQFit::usage = "PSLQFit[num, list, prec] Uses the FindIntegerNullVector function to express num as a rational linear combination of the quantities in list. 
The argument num can be either real or complex. 
The integer prec specifies that all floats should be interpreted as having a precision of prec digits.";


DG::usage = "DG[expr, x] computes the partial derivative of expr with respect to x.";

GIntegrate::usage = "GIntegrate[expr, x] computes the primitive of expr with respect to x. 
Only expressions that contain rational functions x and MPLs of the form G(\!\(\*OverscriptBox[\(a\), \(\[RightVector]\)]\);x), with \!\(\*OverscriptBox[\(a\), \(\[RightVector]\)]\) independent of x are allowed inside expr.";


ShuffleRegulate::usage = "ShuffleRegulate[expr] replaces all divergent MPLs in expr by their shuffle-regularised version.";


FiberSymbol::usage = "FiberSymbol[sym, {\!\(\*SubscriptBox[\(x\), \(1\)]\), \!\(\*SubscriptBox[\(x\), \(2\)]\),...}] returns a combination of MPLs in a fibration basis with respect to the varibles and the ordering specified in {\!\(\*SubscriptBox[\(x\), \(1\)]\), \!\(\*SubscriptBox[\(x\), \(2\)]\),...} whose symbol matches sym. 
The symbol sym is assumed integrable and linearly reducible.";

ToFibrationBasis::usage = "ToFibrationBasis[expr,list] returns expr in a fibration basis with respect to the variables and the ordering specified in list.\n
ToFibrationBasis has the following options:
* FitValue: A list of replacements, which specifies the numerical values of the all the variables used in the numerical fit. 
            The default is Automatic, which assigns random values between 0 and 1 to each variable.
* Save: The value is a string (default: the empty string). If the string is non-empty, then intermediate results of ToFibrationBasis are written to the file whose name is the specified string.
* Input: The value is a string (default: the empty string). If the string is non-empty, then the file specified by the string is read in.
* ProgressIndicator: If set to True, a dynam- ically updated text indicates how many MPLs still need to be converted to the fibration basis.";

FitValue::usage = "Option of ToFibrationBasis. A list of replacements, which specifies the numerical values of the all the variables used in the numerical fit. 
The default is Automatic, which assigns random values between 0 and 1 to each variable.";


(* ::Section:: *)
(*Single-valued MPLs*)


cG::usage = "cG[a1,...,an,z] represents the single-valued multiple polylogarithm \\mathcal{G}(\!\(\*SubscriptBox[\(a\), \(1\)]\),...,\!\(\*SubscriptBox[\(a\), \(n\)]\);z). The indices \!\(\*SubscriptBox[\(a\), \(i\)]\) can be numbers, symbolic constants or functions of other variables.";

cC::usage = "cC[a1,...,an,z] represents the clean single-valued multiple polylogarithm \\mathcal{C}(\!\(\*SubscriptBox[\(a\), \(1\)]\),...,\!\(\*SubscriptBox[\(a\), \(n\)]\);z). The indices \!\(\*SubscriptBox[\(a\), \(i\)]\) can be numbers, symbolic constants or functions of other variables.";

SV::usage = "SV[expr] replaces all objects in expr by their single-valued version.";

cGToG::usage = "cGToG[expr] replaces all cG functions in expr by ordinary MPLs and their complex conjugates.";

cCTocG::usage = "cCTocG[expr] replaces all cC functions in expr by single-valued MPLs.";

cCToG::usage = "Equivalent to cGToG[cCTocG[expr]].";

cGIntegrate::usage = "cGIntegrate[expr, x] computes the primitive of expr with respect to x. 
Only expressions that contain rational functions x and MPLs of the form cG[a1,...,an,x], with \!\(\*OverscriptBox[\(a\), \(\[RightVector]\)]\) independent of x are allowed inside expr.";

DeltaSV::usage = "DeltaSV[expr] applies the coproduct \!\(\*SuperscriptBox[\(\[CapitalDelta]\), \(sv\)]\) to expr.";

AntipodeSV::usage = "AntipodeSV[expr] applies the coproduct \!\(\*SuperscriptBox[\(S\), \(sv\)]\) to expr.";

ProductProjectorSV::usage = "ProductProjectorSV[expr] applies the coproduct \!\(\*SuperscriptBox[\(P\), \(sv\)]\) to expr.";

CobracketSV::usage = "CobracketSV[expr] applies the coproduct \!\(\*SuperscriptBox[\(\[Delta]\), \(sv\)]\) to expr.";


ListToSymbolicExpression::usage = "ListToSymbolicExpression[list_expr] In: list of the form {{\[Lambda]1, {a, b}}, {\[Lambda]2, {c, d}}}. Out: Symbolic expression SymbPlus[SymbScalar[\[Lambda]1,Symb[a,b]],SymbScalar[\[Lambda]2,Symb[c,d]]].";
SymbolicExpressionToList::usage = "SymbolicExpressionToList[symb_expr] In: Symbolic expression of the form SymbPlus[SymbScalar[\[Lambda]1,Symb[a,b]],SymbScalar[\[Lambda]2,Symb[c,d]]]. Out: list {{\[Lambda]1, {a, b}}, {\[Lambda]2, {c, d}}}.";
SymbolicExpressionToReadable::usage = "SymbolicExpressionToReadable[symb_expr] In: Symbolic expression of the form SymbPlus[SymbScalar[\[Lambda]1,Symb[a,b]],SymbScalar[\[Lambda]2,Symb[c,d]]]. Out: more readable expression \[Lambda]1 a\[CircleTimes]b+\[Lambda]2 c\[CircleTimes]d.";
ReadableToSymbolicExpression::usage = "ReadableToSymbolicExpression[read_expr] In: More readable expression of the form \[Lambda]1 a\[CircleTimes]b+\[Lambda]2 c\[CircleTimes]d. Out: Symbolic expression SymbPlus[SymbScalar[\[Lambda]1,Symb[a,b]],SymbScalar[\[Lambda]2,Symb[c,d]]].";
FlattenSymbolicExpression::usage = "FlattenSymbolicExpression[symb_expr] In: Complicated symbolic expression. Out: Flattened out symbolic expression.";
FactorRationalsAndIntegers::usage = "FactorRationalsAndIntegers[expr] In: some rational expression. Out: factorized version. Torsion is neglected. 0, 1, -1 are filtered out.";
SymbolExpandSymb::usage = "SymbolExpandSymb[symb_expr] In: a symbolic expression representing a (nested) linear combination of tensor products. Out: Expanded symbol.";
SymbGSymb::usage = "SymbGSymb[G_expr] In: a MPL in the G-representation G[a1, a2, a3, ..., an, x]. Out: Its symbol.";
ShuffleSymb::usage = "ShuffleSymb[{symb_expr, ..., symb_expr}] In: List of symbolic expressions. Out: shuffle product of those symbolic expressions.";
ShSymb::usage = "ShSymb[expr] Code intern ShuffleSymb. Do not use. Use ShuffleSymb instead.";
SConcatenation::usage = "SConcatenation[symb_expr__] Models concatenation of symbols while respecting linearity.";
Proj::usage = "Proj[symb_expr] In: a symbolic expression. Out: the projector applied to the symbolic expression.";
SymbGExp::usage = "SymbGExp[G_expr] In: a symbolic G expression. Out: its symbol.";
SymbGFastIntern::usage = "SymbGFastIntern[G_expr] Precomputed symbols of generic Gs. Do not use. Use SymbGFast instead.";
SymbGFast::usage = "SymbGFast[G_expr] Analogous to SymbGSymb, but makes use of precomputed symbols. Works only up to and including weight 6.";
ProjLambda::usage = "ProjLambda[symb_expr, {n1, ..., nk}] In: symbolic expression, integer partition. Out: Proj applied to symbolic expression according to the specified integer partition.";


AllSigns::usage = "AllSigns[n] In: integer len. Out: list of lists which contain all possible signs distributed over a list of length len with the condition that the first argument has to be +1. Example: AllSigns[3]=={{1,1,1}, {1,1,-1},{1,-1,1},{1,-1,-1}}.";
AddSignsSymb::usage = "AddSignsSymb[{n1, ..., nk}] In: list of integers. Out: list of lists in which the original list together with all signed variants of it appear.";
SumAbsValues::usage = "SumAbsValues[N, len] In: integer N, integer len. Out: list of lists, which contain all signed integers whose absolute values sum to N and are (potentially) filled up with zeroes until they have length len.";
GetPIsFromRs::usage = "GetPIsFromRs[{R1, ..., Rk}] In: list of arbitrary rational functions. Out: list of irreducible polynomials over Q.";
GetPIBarsFromPIs::usage = "GetPIBarsFromPIs[{R1, ..., Rk}] In: list of irreducible polynomials. Out: extended list of irreducible polynomials.";
HandleDegZero::usage = "HandleDegZero[{poly1, ..., polyn}] In: list of polynomials, some of which may be of degree zero. Out: list of polynomials where the degree zero polynomials have been treated correctly.";
FindPrimes1::usage = "FindPrimes1[{poly1, ..., polyn}] In: list of multiplicatively independent polynomials with n variables. Out: list of n not necessarily different prime numbers.";
FindPrimes2::usage = "FindPrimes2[{poly1, ..., polyn}] Analogous to FindPrimes1 but searching for primes of a different magnitude";
FindPrimes3::usage = "FindPrimes3[{poly1, ..., polyn}] Analogous to FindPrimes1 but searching for primes of a different magnitude";
S3IdOrbit::usage = "S3IdOrbit[R] In: rational function. Out: the group action of S3 applied on this function.";
S3SkOrbit::usage = "S3SkOrbit[{R1, ..., Rk}] In: k-tuple of rational functions. Out the group action of S3 x Sk applied on this k-tuple";
GenerateRS1::usage = "GenerateRS1[n, {poly1, ..., polyn}] In: user defined cut-off integer n, list of multiplicatively independent polynomials. Out: candidates for depth one arguments in list RS1.";
(*IfDivisibleThenDivide::usage = "IfDivisibleThenDivide[n, {c1, ..., ck}] In: integer num, list PIsEvalAtPrimes. Out: -1, 1, or 0";
FindGoodIndices::usage = "FindGoodIndices[lst1, lst2] In: list tobechecked, list PIsEvalAtPrimes. Out: all good indices of tobechecked.";
DetermineIfGoodTuples::usage = "DetermineIfGoodTuples[{tpl1, ..., tpln}] In: list of tuples, PIsEvalAtPrimes, primes, vars. Out: boolean.";*)
GenerateRSn::usage = "GenerateRSn[ArgsDepth1, depth, PIlist] In: list ArgsDepth1, integer depth, list PIlist. Out: all probably admissible k-tuples of arguments for Li function types.";
GenerateRSn2::usage = "GenerateRSn2[ArgsDepth1, depth, PIlist] In: list ArgsDepth1, integer depth, list PIlist. Out: all probably admissible k-tuples of arguments for G function types.";
