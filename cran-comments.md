# Comments for submission to CRAN

Sorry for the delay in submitting a new version that passes the CRAN checks. This version should fix the issues that are causing the current version to fail the checks. It also removes a dependency that is no longer needed and has an updated citation file.

Thanks!

Jeffrey Hanson

---------

## Test environments

* [Ubuntu 14.04 (travis-ci)](https://travis-ci.org/jeffreyhanson/raptr/builds)
  + release
  + devel
* [Mac OSX 10.9.5 (travis-ci](https://travis-ci.org/jeffreyhanson/raptr/builds)
  + release
* [Windows Server 2012 R2 (x64) (appveyor)](https://ci.appveyor.com/project/jeffreyhanson/raptr)
  + release
* [Win-Builder](https://win-builder.r-project.org/)
  + devel

## R CMD check results

0 errors | 0 warnings | 2 notes

### Notes

* checking package dependencies ... NOTE
  package suggested but not available for checking: 'gurobi'

  _This package depends heavily on the Gurobi software platform and the gurobi R package distributed with it. Instructions for installing the gurobi R package are contained in the package's description and README files. The 'gurobi' R package is an R package that is distributed along with the gurobi program. It would not be practical to use another R package (eg. lpSolveAPI) because non-commercial exact algorithm solvers cannot solve even moderately sized problems in a feasible amount of time. Several existing R packages on CRAN use the gurobi R package (eg. cherry, DESP)._

* checking installed package size ... NOTE
  installed size is  5.8Mb
  sub-directories of 1Mb or more:
    data   3.6Mb

  + _data: The ./data directory contains case-study data for a vignette. Although a separate package could be created to store the case-study data, this seems unnecessary because the data sets are used in both package testing in the vignette. I would prefer to keep the data sets in this package for simplicity._

## Reverse dependencies

This package does not have any "Reverse depends", "Reverse imports", "Reverse suggests", or "Reverse linking to".
