# Comments for submission to CRAN

## Package updates

This version has the following updates:

* compatibility updates with new version of the hypervolume R package
* added more links to the package documentation
* use `requireNamespace` to  check which packages are installed on a user's system
* registers native routines and disables symbol search to avoid generating an additional note

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

* Possibly mis-spelled words in DESCRIPTION:
  al (21:33)
  et (21:30)
  prioritizations (15:6)

  _These words have not been mis-spelled._

* checking package dependencies ... NOTE
  package suggested but not available for checking: 'gurobi', 'rgurobi'

  _This package depends heavily on the gurobi software platform. Below, I have included my comments regarding this note in the initial submission._

  _The installation instructions for both of these packages are contained in this package's description._

  _The 'gurobi' R package is an R package that is distributed along with the gurobi program. It would not be practical to use another R package (eg. lpSolveAPI) because non-commercial exact algorithm solvers cannot solve even moderately sized problems in a feasible amount of time. Several existing R packages on CRAN use the gurobi R package (eg. cherry, DESP)._

  _The 'rgurobi' provides additional functionality not contained in the gurobi package. This is a package I have written and placed on github. This package offers additional functionality. Note that users cannot install rgurobi package without the gurobi program already having been installed on their system because it contains compiled code that uses header files distributed with the program. Thus the rgurobi package cannot be submitted to CRAN._

* checking installed package size ... NOTE
  installed size is  7.9Mb
  sub-directories of 1Mb or more:
    data   3.6Mb
    doc    1.7Mb
    libs   2.1Mb

  + _data: The ./data directory contains case-study data for a vignette. Although a separate package could be created to store the case-study data, this
  seems unnecessary because the data sets are used in both package testing in the vignette. I would prefer to keep the data sets in this package for simplicity._

  + _doc: The documentation for this package is comprehensive. It contains a html vignette with several detailed examples._

  + _libs: To improve performance, many functions in this package are written in C++. This has resulted in a large ./libs folder. The ./libs folder could be reduced
  in size by rewriting the functions in R, however, this would severely reduce the usability of the package. I would prefer to keep the C++ functions and the
  large ./libs folder._

## Reverse dependencies

This package does not have any "Reverse depends", "Reverse imports", "Reverse suggests", or "Reverse linking to".

Thanks!

Jeffrey Hanson
