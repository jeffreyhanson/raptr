Comments for submission to CRAN 
===============================

## Test environments
* [Ubuntu 14.04 (travis-ci)](https://travis-ci.org/jeffreyhanson/raptr/builds)
  + R 3.4.0
  + R devel
* [Mac OSX 10.9.5 (travis-ci](https://travis-ci.org/jeffreyhanson/raptr/builds)
  + R 3.4.0
  + R devel
* [Windows Server 2012 R2 (x64) (appveyor)](https://ci.appveyor.com/project/jeffreyhanson/raptr)
  + R 3.3.2 
* [Windows Server 2008 (x64) (win-builder)](https://win-builder.r-project.org/)
  + R devel

## R CMD check results

0 errors | 0 warnings | 3 notes

### Notes

This version of the package does not encounter any new notes. All of these notes were encountered in the package's initial submission to CRAN. To help with reviewing this submission, I have included the notes and a copy of my original comments regarding them.


* Possibly mis-spelled words in DESCRIPTION:
    prioritizations (14:67, 16:29)
    
  _The appearance of this note can be attributed to two reasons. First, this is my first submission to CRAN. Second, the spell-checker used does not recognize the "prioritizations" as a word. This is a commonly used word in conservation science, [for instance it is present in the title of this paper](http://onlinelibrary.wiley.com/doi/10.1111/acv.12222/full), and not a mis-spelled word._
  
* checking package dependencies ... NOTE
  package suggested but not available for checking: 'gurobi', 'rgurobi'
  
  _As previously mentioned, this package depends heavily on the gurobi software platform. Below, I have included my comments regarding this note in the initial submission._
  
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

## Package updates

This version has the following updates:

* added more links to the package documentation
* use `requireNamespace` to  check which packages are installed on a user's system
* registers native routines and disables symbol search to avoid generating an additional note
* [fixes error encountered on Solaris environment](https://www.r-project.org/nosvn/R.check/r-patched-solaris-sparc/raptr-00check.html)

Thanks!

Jeffrey Hanson
