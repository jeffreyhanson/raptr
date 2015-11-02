## Test environments
* ubuntu 14.04, RRO 3.2.2 (local system)
* ubuntu 12.04, R 3.2.2 (travis-ci)
* Mac OSX 10.9.5, R 3.2.2 (travis-ci)
* Windows (appveyor)
* win-builder (devel and release) 

## R CMD check results
There were no ERRORs or WARNINGs.

There were 2 NOTEs

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

  
## Downstream dependencies
This package is not present on CRAN and therefore has no pacakges depending on it. Consequently, there were no issues detected when running devtools::revdep_check().
The results of this are in ./revdep
