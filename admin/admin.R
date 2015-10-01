# compile c++ attributes
library(devtools)
library(Rcpp)
setwd(file.path(Sys.getenv('HOME'), 'BitBucket', 'raspr'))
compileAttributes()

# document code
setwd(file.path(Sys.getenv('HOME'), 'BitBucket', 'raspr'))
library(devtools)
library(roxygen2)
document()

# find obvious errors
setwd(file.path(Sys.getenv('HOME'), 'BitBucket', 'raspr'))
library(devtools)
library(roxygen2)
load_all() 

# make vignettes
library(devtools)
library(knitr)
setwd(file.path(Sys.getenv('HOME'), 'BitBucket', 'raspr'))
build_vignettes()

# formal package tests
setwd(file.path(Sys.getenv('HOME'), 'BitBucket', 'raspr'))
library(devtools)
library(roxygen2)
test()

# cran checks
setwd(file.path(Sys.getenv('HOME'), 'BitBucket', 'raspr'))
library(devtools)
library(roxygen2)
check()

# local install
# library(devtools)
# install_local(
# 	file.path(Sys.getenv('HOME'), 'BitBucket', 'raspr')
# )

