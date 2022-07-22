Dear CRAN maintainers,

Thank you very much for reviewing this submission. This package was formerly available on CRAN. It was archived because one of its dependencies was archived (i.e., the RandomFields package). To address this, I have updated the package so that the RandomFields package is no longer listed as a dependency. I have also taken this opportunity to update the package to address issues identified by CRAN package checks. Specifically, these issue includes a note raised during package checks (i.e., `Namespace in Imports field not imported from: ‘rgdal’; All declared Imports should be used.`), and a compiler warning (i.e., `warning: use of bitwise '&' with boolean operands [-Wbitwise-instead-of-logical]`).

Cheers,

Jeff

# Test environments

* [Ubuntu 20.04, R-release](https://github.com/jeffreyhanson/raptr/actions?query=workflow%3AUbuntu)
* [Ubuntu 20.04, R-devel](https://github.com/jeffreyhanson/raptr/actions?query=workflow%3AUbuntu)
* [Mac OSX 10.15, R-release](https://github.com/jeffreyhanson/raptr/actions?query=workflow%3A%22Mac+OSX%22)
* [Windows Server 2019, R-release](https://github.com/jeffreyhanson/raptr/actions?query=workflow%3AWindows)
* [Windows Server 2008 (x64), R-devel (win-builder)](https://win-builder.r-project.org/)

# R CMD check results

0 errors | 0 warnings | 2 note

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
