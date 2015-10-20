## Test environments
* ubuntu 14.04, R 3.2.2
* ubuntu 12.04 (on travis-ci), R 3.1.2
* win-builder (devel and release) 

## R CMD check results
There were no ERRORs or WARNINGs.

There were 4 NOTEs

* checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Jeffrey O Hanson <jeffrey.hanson@uqconnect.edu.au>'
  New submission
  
  This is my first submission to CRAN.
  
* checking installed package size ... NOTE
  installed size is  9.6Mb
  sub-directories of 1Mb or more:
    data   3.9Mb
    libs   5.4Mb
    
  The ./data directory contains case-study data for a vignette. Although a seperate package could be created to store the case-study data, this
  seems unnecessary because it would just contain 3 objects. I would prefer to keep the datasets in this pacakge for simplicity.
  
  To improve performance, many functions in the package are written in C++. This has resulted in a large ./libs folder. The ./libs folder could be reduced
  in size by rewriting the functions in R, however, this would severly reduce the useability of the package. I would prefer to keep the C++ functions and the
  large ./libs folder.

* checking package dependencies ... NOTE
  Package suggested but not available for checking: 'gurobi'
  
  This package depends heavily on high performance optimisation software. While the lpSolveAPI R package on CRAN could be used instead, this would severly limit the 
  useability of the package. This is because 'lp_solve' cannot accomodate the large-scale problems that users need to solve.
  
* checking Rd cross-references ... NOTE
  Package unavailable to check Rd xrefs: 'maptools'
  
  The 'maptools' R package has lots of dependencies. I would prefer to avoid including this package as a dependency to reduce the total installation time of this package.
  
## Downstream dependencies
This package is not present on CRAN and therefore has no pacakges depending on it. Consequently, there were no issues detected when running devtools::revdep_check().
The results of this are in ./revdep
