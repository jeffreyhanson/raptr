Dear CRAN maintainers,

This update addresses the NOTE:

>    Namespace in Imports field not imported from: ‘rgdal’
>     All declared Imports should be used.

Cheers,

Jeff


# Test environments

* [Ubuntu 20.04, R-release](https://github.com/jeffreyhanson/raptr/actions?query=workflow%3AUbuntu)
* [Ubuntu 20.04, R-devel](https://github.com/jeffreyhanson/raptr/actions?query=workflow%3AUbuntu)
* [Mac OSX 10.15, R-release](https://github.com/jeffreyhanson/raptr/actions?query=workflow%3A%22Mac+OSX%22)
* [Windows Server 2019, R-release](https://github.com/jeffreyhanson/raptr/actions?query=workflow%3AWindows)
* [Windows Server 2019, R-devel](https://github.com/jeffreyhanson/raptr/actions?query=workflow%3AWindows)
* Windows Server 2008 (x64), R-devel (win-builder)

# R CMD check results

0 errors | 0 warnings | 0 note

# Notes

* checking package dependencies ... NOTE
  package suggested but not available for checking: 'gurobi'

    **This package depends heavily on the Gurobi software platform and the gurobi R package distributed with it. Instructions for installing the gurobi R package are contained in the package's description and README files. The 'gurobi' R package is an R package that is distributed along with the gurobi program. It would not be practical to use another R package (eg. lpSolveAPI) because non-commercial exact algorithm solvers cannot solve even moderately sized problems in a feasible amount of time. Several existing R packages on CRAN use the gurobi R package (eg. cherry, DESP).**

* checking installed package size ... NOTE
    installed size is 18.6Mb
    sub-directories of 1Mb or more:
      data   3.6Mb
      doc    1.4Mb
      libs  13.0Mb

    **data: The ./data directory contains case-study data for a vignette. Although a separate package could be created to store the case-study data, this seems unnecessary because the data sets are used in both package testing in the vignette. I would prefer to keep the data sets in this package for simplicity.**

    **libs: The package makes extensive use of C++ code to reduce run time.**

# Downstream dependencies

There are no existing packages that depend on this package.
