Comments for submission to CRAN 
===============================

## Test environments
* [ubuntu 12.04, R 3.3.2 (travis-ci)](https://travis-ci.org/jeffreyhanson/raptr/builds)
* [Mac OSX 10.9.5, R 3.3.2 (travis-ci](https://travis-ci.org/jeffreyhanson/raptr/builds)
* [Windows Server 2012 R2 (x64), R 3.3.2 (appveyor)](https://ci.appveyor.com/project/jeffreyhanson/raptr)
* Windows Server 2008 (x64), R 3.4.0 (win-builder)

## R CMD check results

3 errors | 0 warnings | 4 notes

### Errors

This package fails to install on several environments (r-patched-solaris-sparc, r-patched-solaris-x86, r-release-osx-x86_64-mavericks) because it has dependencies that cannot be installed on these environments (RandomFields, gdalUtils). These errors are documented [here](https://cran.r-project.org/web/checks/check_results_raptr.html).

* Result: ERROR 
    Package required but not available: 'RandomFields'
    
    Packages suggested but not available for checking: 'gurobi' 'rgurobi'
    
    See section 'The DESCRIPTION file' in the 'Writing R Extensions'
    manual. 
Flavors: r-patched-solaris-sparc, r-patched-solaris-x86

* Result: ERROR 
    Package required but not available: 'gdalUtils'
    
    Packages suggested but not available for checking: 'gurobi' 'rgurobi'
    
    See section 'The DESCRIPTION file' in the 'Writing R Extensions'
    manual. 
Flavor: r-release-osx-x86_64-mavericks

### Notes

* checking DESCRIPION meta-information ... NOTE
  Author field differs from that derived from Authors@R
    Author:    'Jeffrey O Hanson [aut, cre], Jonathon R Rhodes [aut], Hugh P Possingham [aut], Richard A Fuller [aut]'
    Authors@R: 'Jeffrey O Hanson [aut, cre], Jonathan R Rhodes [aut], Hugh P Possingham [aut], Richard A Fuller [aut]'
  
  _I accidentally misspelled "Jonathan" as "Jonathon" in the initial CRAN submission. I have corrected it in the new version._

The remaining notes were present in the initial CRAN submission. I have included these notes and my comments from the initial submission below.

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

------------

This version has a reduced installation time and fixes several typos in the documentation. Additionally, this version corrects a typo in one of the author's names.

Thanks!

Jeffrey Hanson

