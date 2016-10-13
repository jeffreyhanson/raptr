raptr
=====

[![Travis Build Status](https://img.shields.io/travis/jeffreyhanson/raptr/master.svg?label=Mac%20OSX%20%26%20Linux)](https://travis-ci.org/jeffreyhanson/raptr)
[![AppVeyor Build Status](https://img.shields.io/appveyor/ci/jeffreyhanson/raptr/master.svg?label=Windows)](https://ci.appveyor.com/project/jeffreyhanson/raptr)
[![Coverage Status](https://codecov.io/github/jeffreyhanson/raptr/coverage.svg?branch=master)](https://codecov.io/github/jeffreyhanson/raptr?branch=master)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/raptr)](http://CRAN.R-project.org/package=raptr)

#### Biodiversity is in crisis. The overarching aim of conservation is to preserve biodiversity patterns and processes. To this end, protected areas are established to buffer species and preserve biodiversity processes. But resources are limited and so protected areas must be cost-effective. This package contains functions to generate plans for protected areas. Conservation planning data are used to construct an optimisation problem, which in turn is then solved to yield prioritisations. Amount-based targets can be used to identify prioritisations that contain an adequate amount of the target species. Additionally, space-based targets can be used to ensure that a representative sample of the target species are preserved. To solve the optimisation problems in a feasible amount of time, this package uses the commerical 'Gurobi' software package (obtained from <http://www.gurobi.com/>).

This package depends on several packages which can be dificult to install under Linux and Mac operating systems.

Linux users can install them typing the following code into the terminal:
```
sudo apt-get update
sudo apt-get install libgdal-dev
sudo apt-get install libproj-dev
sudo apt-get build-dep r-cran-rgl
```

Mac users can install them using this code in the terminal:
```
brew install Caskroom/cask/xquartz
brew install gdal
Rscript -e "setRepositories(ind=1:2);install.packages(c('rgdal','rgeos'))"
```

To install the development version, use the following R code:

```
if (!require('devtools'))
	install.packages('devtools', repo='http://cran.rstudio.com', dep=TRUE)
devtools:::install_github('jeffreyhanson/raptr')
```

Once this package has been installed, you can read through the vignette for a tutorial on how to use it.

[View it here](https://rawgit.com/jeffreyhanson/raptr/master/inst/doc/raptr.html), or by running this R code:

```
# open vignette in web browser
vignette('raptr', package='raptr')
```

**If this R package helped you, please cite it.**

Hanson J.O., Rhodes J. R., Fuller R. A. (2015). raptr: Representative and Adequate Prioritisation Toolkit in R. Version 0.0.1.
