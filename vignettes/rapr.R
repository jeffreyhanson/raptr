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

## ---- message=FALSE------------------------------------------------------
# make amount-based prioritisation
# and ignore existing protected areas by discarding values in the 
# status (third) column of the attribute table
cs_rs_amount <- rap(
	cs_pus[,-2], cs_spp, cs_space,
  amount.target=0.1, space.target=NA, n.demand.points=100L,
  include.geographic.space=TRUE, formulation='unreliable',
  kernel.method='hypervolume', solve=FALSE
)

