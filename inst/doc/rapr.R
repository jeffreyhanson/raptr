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
use.Gurobi=TRUE
if (use.Gurobi & !is.GurobiInstalled())
	use.Gurobi=FALSE

# load solution cache if gurobi not avaliable
if (!use.Gurobi)
	cache <- readRDS('vignette_solutions.rds')
	
# set seed for reproducibility
set.seed(500)

