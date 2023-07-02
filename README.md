# IntegrationOfSymbols
This repository contains packages and notebooks for the task of integrating symbols following the algorithm described in "From polygons and symbols to polylogarithmic functions" by Claude Duhr, Herbert Gangl and John R. Rhodes (https://arxiv.org/abs/1110.0458).
The code uses the Mathematica interface to the GiNaC library provided by PolyLogTools developed by Cluade Duhr and Falko Dulat (https://arxiv.org/abs/1904.07279 and https://gitlab.com/pltteam/plt/-/tree/master).

The packages ArgumentSearch.wl and Symbols2.wl provide the functionality necessary for tackling the problem. The actual task is however carried out in the notebooks IntegrationWeight2-4.nb. The notebook 2dHPLs.nb contains results regarding the special class of 2d Harmonic Polylogarithms (see, for example, https://arxiv.org/abs/hep-ph/0111255 by Thomas Gehrmann and Ettore Remiddi).

For more information, see my thesis "On general algorithms to manipulate multiple polylogarithms" (especially chapter 2).

# How to use this repository
The above packages can be integrated into PolyLogTools as follows:
1. Paste the packages into the core directory (typically plt-master/core).
2. Open PolyLogTools.m and add
   ```Get["ArgumentSearch`"]; Get["Symbols2`"];```
   to the cell handling subdirectory structure and subroutines.
