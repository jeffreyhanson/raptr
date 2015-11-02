raspr
============

[![Travis Build Status](https://img.shields.io/travis/paleo13/raspr/master.svg?label=Mac%20OSX%20%26%20Linux)](https://travis-ci.org/paleo13/raspr)
[![AppVeyor Build Status](https://img.shields.io/appveyor/ci/paleo13/raspr/master.svg?label=Windows)](https://ci.appveyor.com/project/paleo13/raspr)
[![Coverage Status](https://img.shields.io/coveralls/paleo13/raspr/master.svg?label=coverage)](https://codecov.io/github/paleo13/raspr?branch=master)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/raspr)](http://cran.r-project.org/package=raspr)

## This package is not ready for release yet!

#### Biodiversity is in crisis. The overarching aim of conservation is to preserve biodiversity patterns and processes. To this end, protected areas are established to buffer species and preserve biodiversity processes. But resources are limited and so protected areas must be cost-effective. This package contains functions to generate plans for protected areas--prioritisations--using the Representatiev and Adequate Sample Problem (RASP). Reserve selection problems are expressed as a mixed linear integer problem (MILP) and solved using exact-algorithm solvers. Prioritisations can be obtained using [Gurobi](http://www.gurobi.com/).

This package depends on several pacakges which can be dificult to install under Linux and Mac operating systems.

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

To install the raspr R package, execute the following commands in R:

```
if (!require('devtools'))
	install.packages('devtools', repo='http://cran.rstudio.com', dep=TRUE)
devtools:::install_github('paleo13/rapsr')
```

Once this package has been installed, you can explore its functions by reading through the vignette. You can access it in R by running the code below:

```
# open vignette in web browser
vignette('raspr', package='raspr')
```

**If this R package helped you, please cite it.**

Hanson J.O., Rhodes J. R., Fuller R. A. (2015). raspr: Identify Representative and Adequate Prioritisations in R. Version 0.0.1.
