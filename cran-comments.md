## Test environments
* ubuntu 14.04, RRO 3.2.2 (local system)
* ubuntu 12.04, R 3.2.2 (travis-ci)
* Mac OSX 10.9.5, R 3.2.2 (travis-ci)
* Windows Server 2012 R2 (x64) (appveyor)
* win-builder (devel and release) 

## R CMD check results
There were no ERRORs or WARNINGs.

There were 3 NOTEs

* checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Jeffrey O Hanson <jeffrey.hanson@uqconnect.edu.au>'
  New submission
  
  This is my first submission to CRAN.

* checking package dependencies ... NOTE
	Package suggested but not available for checking: 'gurobi' 
  
  This package depends heavily on the gurobi R package. It would not be practical to use another R package (eg. lpSolveAPI) because non-commerical exact algorithm
  solvers cannot solve moderately sized problems in a feasible amount of time. Additionally, several existing R packages on CRAN use the gurobi R package (eg. cherry, DESP).
  
* checking installed package size ... NOTE
  installed size is  9.1Mb
  sub-directories of 1Mb or more:
    data   2.2Mb
    libs   6.4Mb

  +data: The ./data directory contains case-study data for a vignette. Although a seperate package could be created to store the case-study data, this
  seems unnecessary because the datsets are used in both package testing in the vignette. I would prefer to keep the datasets in this pacakge for simplicity.
  
  +libs: To improve performance, many functions in the package are written in C++. This has resulted in a large ./libs folder. The ./libs folder could be reduced
  in size by rewriting the functions in R, however, this would severly reduce the useability of the package. I would prefer to keep the C++ functions and the
  large ./libs folder.

  
## Downstream dependencies
This package is not present on CRAN and therefore has no pacakges depending on it. As a consequence, there were no issues detected when running devtools::revdep_check().
The results of this check are in ./revdep
