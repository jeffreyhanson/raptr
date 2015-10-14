## Test environments
* ubuntu 12.04 (on travis-ci), R 3.1.2
* win-builder (devel and release) 

## R CMD check results
There were no ERRORs or WARNINGs.

There was 1 NOTE

* checking installed package size ... NOTE
  installed size is 10.0Mb
  sub-directories of 1Mb or more:
    data   4.1Mb
    libs   5.6Mb

  The ./data directory contains case-study data for a vignette. Although a seperate package could be created to store the case-study data this
  seems unnecessary and would just contain 3 object. I would prefer to keep the datasets in this pacakge for simplicity.
  
  To improve performance the performance of package, many functions written in C++. This results in a large ./libs folder. The libs folder could be reduced
  in size by rewriting the functions in R, however, this would severly reduce the useability of the package. I would prefer to keep the C++ functions and the associated
  large ./libs folder.

## Downstream dependencies
This package is not present on CRAN and therefore has no pacakges depending on it. Consequently, there were no issues detected when running devtools::revdep_check().
The results of this are in ./revdep
