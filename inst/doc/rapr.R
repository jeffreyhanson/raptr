## ----global, include=FALSE-----------------------------------------------
# load rapr R package
library(rapr)

# load packages for vignette
library(plyr)
library(dplyr)
library(ggplot2)
library(RandomFields)

# set cache globally
knitr::opts_chunk$set(cache=TRUE)

# use built-in solutions or Gurobi?
use.Gurobi=FALSE
if (use.Gurobi & !is.GurobiInstalled())
	use.Gurobi=FALSE

# set seed for reproducibility
set.seed(500)

## ---- message=FALSE, result='hide'---------------------------------------
# load packages for tutorial
library(plyr)
library(dplyr)
library(ggplot2)
library(RandomFields)

# set seed for reproducibility
set.seed(500)

