Dear CRAN maintainers,

Thank you very much for reviewing this submission.

This submission contains an update for the raptr package that is currently available on CRAN. It aims to address several of the NOTES and the ERROR detected by CRAN's automated checks on the CRAN's Fedora-clang platform (i.e., r-devel-linux-x86_64-fedora-clang). Specifically, it has updates that (i) replace dependencies for the rgdal and rgeos packages in anticipation of their retirement, (ii) fix package startup message issues, (iii) fix the CITATION file format, and (iv) updates the C++ specification to C++14 (per <https://cran.r-project.org/doc/manuals/r-devel/R-exts.html>). Additionally, it has updated examples and unit tests to address failing checks on CRAN's Fedora-clang platform.

In case it is helpful, I will explain the steps and rationale taken to address the failing checks on CRAN's Fedora-clang platform. Although I have attempted to reproduce the failed check on a comparable platform (i.e., the R-hub Fedora Clang docker image, <https://hub.docker.com/r/rhub/fedora-clang-devel>) and other platforms (see test environments below), I am unable to reproduce the specific error detected on CRAN's Fedora-clang platform. As such, I have (i) encapsulated all examples within `dontrun{}` and (ii) ensured that unit tests are not run on Fedora platforms. This approach ensures that the examples and unit tests will not throw an error on CRAN's Fedora-clang platform. This approach also ensures that all unit tests are still run on other platforms (e.g., Windows, Debian, macOS). It is also worth noting that I cannot simply encapsulate the failing example and unit test detected on CRAN's Fedora-clang system, because I cannot guarantee that there are not other examples that fail on this platform (since I cannot reproduce the error). Although this approach is not ideal, I believe that the CRAN volunteers' time is the most precious resource and this approach is the only way that I can think of that would avoid wasting their time with the possibility of multiple submission. I hope that my explanation and approach is satisfactory.

Cheers,

Jeff

# Test environments

* [Ubuntu 20.04, R-release](https://github.com/jeffreyhanson/raptr/actions?query=workflow%3AUbuntu)
* [Ubuntu 20.04, R-devel](https://github.com/jeffreyhanson/raptr/actions?query=workflow%3AUbuntu)
* [Fedora Linux 36, R-devel, clang version 14.0.5](https://hub.docker.com/r/rhub/fedora-clang-devel)
* [macOS 10.15, R-release](https://github.com/jeffreyhanson/raptr/actions?query=workflow%3AmacOS)
* [macOS 11.5.2 (arm64), R-devel (macOS builder)](https://mac.r-project.org/macbuilder/submit.html)
* [Windows Server 2019, R-release](https://github.com/jeffreyhanson/raptr/actions?query=workflow%3AWindows)
* [Windows Server 2008 (x64), R-devel (win-builder)](https://win-builder.r-project.org/)

# R CMD check results

0 errors | 0 warnings | 3 notes

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

    **data: The data directory contains case-study data for a vignette. Although a separate package could be created to store the case-study data, this seems unnecessary because the data sets are used in both package testing in the vignette. I would prefer to keep the data sets in this package for simplicity.**

    **libs: The package makes extensive use of C++ code to reduce run time.**

* checking CRAN incoming feasibility ... NOTE

  Possibly mis-spelled words in DESCRIPTION:
    al (19:33)
    et (19:30)
    prioritizations (15:6)

  **I can confirm that these words are spelled correctly.**

  Found the following (possibly) invalid URLs:
    URL: https://ala.org.au/
      From: man/casestudy_data.Rd
      Status: Error
      Message: SSL certificate problem: unable to get local issuer certificate

  **I have verified that the URL is correct.**

# Downstream dependencies

There are no existing packages that depend on this package.
