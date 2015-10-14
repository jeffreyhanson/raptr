raspr
============

[![Build Status](https://travis-ci.org/paleo13/raspr.svg?branch=master)](https://travis-ci.org/paleo13/rapsr)
[![Coverage Status](https://codecov.io/github/paleo13/rapsr/coverage.svg?branch=master)](https://codecov.io/github/paleo13/rapsr?branch=master)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/raspr)](http://cran.r-project.org/package=raspr)

Biodiversity is in crisis. The overarching aim of conservation is to preserve biodiversity patterns and processes. To this end, protected areas are established to buffer species and preserve biodiversity processes. But resources are limited and so protected areas must be cost-effective. This package contains functions to generate plans for protected areas--prioritisations--using the Representative and Adequate Sample Problem (RASP). Reserve selection problems are expressed as a mixed linear integer problem (MILP) and solved using exact-algorithm methods. Prioritisations can be obtained using [Gurobi](http://www.gurobi.com/).

Linux and Mac OSX users will first need to install several dependencies. To install them, type the following code in the bash or the terminal:

```
sudo apt-get update
sudo apt-get install libgdal-dev
sudo apt-get install libproj-dev
```

To install the raspr R package, execute the following commands in R:

```
if (!require('devtools'))
	install.packages('devtools', repo='http://cran.rstudio.com', dep=TRUE)
devtools:::install_github('paleo13/rapsr')
```

Once this package has been installed, you can explore the functions of this package by reading through the vignette. You can access it in R by running the code below:

```
# open vignette in web browser
vignette('raspr', package='raspr')
```

**If this R package helped you, please cite it.**

Hanson J.O., Rhodes J. R., Fuller R. A. (2015). raspr (version 1.0.0): identify prioritisations using the Representative and Adequate Sample Problem (RASP).

