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

## ---- eval=use.Gurobi----------------------------------------------------
# solve problem to identify prioritisation
sim_rs_s1_amount <- solve(sim_ru_s1)

## ---- eval=!use.Gurobi, include=FALSE------------------------------------
#  sim_rs_s1_amount <- solve(sim_ru_s1, b=cache$sim_rs_s1_amount)

## ---- eval=use.Gurobi----------------------------------------------------
# update amount targets to 20% and space targets to 0% and solve it
sim_rs_s2_amount <- update(sim_ru_s2, amount.target=0.2, space.target=NA, solve=TRUE)

## ---- eval=!use.Gurobi, include=FALSE------------------------------------
#  # update amount targets to 20% and space targets to 0% and solve it
#  sim_rs_s2_amount <- update(sim_ru_s2, amount.target=0.2, space.target=NA, solve=FALSE)
#  sim_rs_s2_amount <-solve(
#  	sim_rs_s2_amount,
#  	b=cache$sim_rs_s2_amount
#  )

## ---- eval={use.Gurobi}--------------------------------------------------
# update amount targets to 20% and space targets to 0% and solve it
sim_rs_s3_amount <- update(sim_ru_s3, amount.target=0.2, space.target=NA)

## ---- eval=!use.Gurobi, include=FALSE------------------------------------
#  # update amount targets to 20% and space targets to 0% and solve it
#  sim_rs_s3_amount <- update(sim_ru_s3, amount.target=0.2, space.target=NA, solve=FALSE)
#  sim_rs_s3_amount <- solve(
#  	sim_rs_s3_amount,
#  	b=cache$sim_rs_s3_amount
#  )

## ---- eval=use.Gurobi----------------------------------------------------
# make new prioritisation
sim_rs_s1_space <- update(sim_rs_s1_amount, amount.target=0.2, space.target=0.85)

## ---- eval=!use.Gurobi, include=FALSE------------------------------------
#  # make new prioritisation
#  sim_rs_s1_space <- update(sim_rs_s1_amount, amount.target=0.2, space.target=0.5, solve=FALSE)
#  sim_rs_s1_space <- solve(
#  	sim_rs_s1_space,
#  	b=cache$sim_rs_s1_space
#  )

## ---- eval=use.Gurobi----------------------------------------------------
# make new prioritisation
sim_rs_s2_space <- update(sim_rs_s2_amount, amount.target=0.2, space.target=0.85)

## ---- eval=!use.Gurobi, include=FALSE------------------------------------
#  # make new prioritisation
#  sim_rs_s2_space <- update(sim_rs_s2_amount, amount.target=0.2, space.target=0.85, solve=FALSE)
#  sim_rs_s2_space <- solve(
#  	sim_rs_s2_space,
#  	b=cache$sim_rs_s2_space
#  )

## ---- eval=use.Gurobi----------------------------------------------------
# make new prioritisation
sim_rs_s3_space <- update(sim_rs_s3_amount, amount.target=0.2, space.target=0.85)

## ---- eval=!use.Gurobi, include=FALSE------------------------------------
#  # make new prioritisation
#  sim_rs_s3_space <- update(sim_rs_s3_amount, amount.target=0.2, space.target=0.85, solve=FALSE)
#  sim_rs_s3_space <- solve(
#  	sim_rs_s3_space,
#  	b=cache$sim_rs_s3_space
#  )

## ---- eval=use.Gurobi----------------------------------------------------
# make prioritisations
sim_mrs_amount <- update(
	sim_ru,
	amount.target=c(0.2,0.2,0.2),
	space.target=c(0,0,0)
)

sim_mrs_space <- update(
	sim_ru,
	amount.target=c(0.2,0.2,0.2),
	space.target=c(0.85, 0.85, 0.85)
)

## ---- eval=!use.Gurobi, include=FALSE------------------------------------
#  # make prioritisations
#  sim_mrs_amount <- update(sim_ru, amount.target=c(0.2,0.2,0.2), space.target=c(0,0,0), solve=FALSE)
#  sim_mrs_amount <- solve(
#  	sim_mrs_amount,
#  	b=cache$sim_mrs_amount
#  )
#  
#  sim_mrs_space <- update(sim_ru, amount.target=c(0.2,0.2,0.2), space.target=c(0.85, 0.85, 0.85), solve=FALSE)
#  sim_mrs_space <- solve(
#  	sim_mrs_space,
#  	b=cache$sim_mrs_space
#  )

## ---- eval={use.Gurobi}--------------------------------------------------
# make new prioritisation with probability threshold of 0.5 for each species
sim_mrs_space2 <- solve(
	prob.subset(
		sim_mrs_space,
		species=1:3,
		threshold=c(0.1,0.1,0.1)
	)
)

## ---- eval=!use.Gurobi, include=FALSE------------------------------------
#  # make new prioritisation with probability threshold of 0.5 for each species
#  sim_mrs_space2 <- solve(
#  	prob.subset(
#  		sim_mrs_space,
#  		species=1:3,
#  		threshold=c(0.1,0.1,0.1)
#  	),
#  	b=cache$sim_mrs_space2
#  )

## ---- eval=use.Gurobi----------------------------------------------------
# make new prioritisation using reliable formulation and reduced targets
sim_mrs_space3 <- update(
	sim_mrs_space,
	formulation='reliable',
	max.r.level=1L,
	space.target=-25
)

## ---- eval=!use.Gurobi, include=FALSE------------------------------------
#  # make new prioritisation using reliable formulation
#  sim_mrs_space3 <- update(
#  	sim_mrs_space,
#  	formulation='reliable',
#  	max.r.level=1L,
#  	space.target=-25,
#  	solve=FALSE
#  )
#  
#  sim_mrs_space3 <- solve(
#  	sim_mrs_space3,
#  	b=cache$sim_mrs_space3
#  )

## ---- eval=use.Gurobi----------------------------------------------------
# update prioritisation
sim_mrs_amount_blm <- update(sim_mrs_amount, BLM=100)

## ---- eval=!use.Gurobi, include=FALSE------------------------------------
#  sim_mrs_amount_blm <- update(sim_mrs_amount, BLM=100, solve=FALSE)
#  sim_mrs_amount_blm <- solve(
#  	sim_mrs_amount_blm,
#  	b=cache$sim_mrs_amount_blm
#  )

## ---- eval=use.Gurobi, markup='hide'-------------------------------------
# create vector with distance metrics
dist.metrics <- c(
	'euclidean', 'bray', 'manhattan','gower',
	'canberra', 'mahalanobis',
	'jaccard', 'kulczynski'
)

# generate solutions
solutions <- list()
for (i in dist.metrics) {
	solutions[[i]] <- update(sim_ru_gp, distance.metric=i)
}

## ---- eval=!use.Gurobi, include=FALSE------------------------------------
#  # create vector with distance metrics
#  dist.metrics <- c(
#  	'euclidean', 'bray', 'manhattan','gower',
#  	'canberra', 'mahalanobis',
#  	'jaccard', 'kulczynski'
#  )
#  
#  # generate solutions
#  solutions <- list()
#  for (i in dist.metrics) {
#  	solutions[[i]] <- update(sim_ru_gp, distance.metric=i, solve=FALSE)
#  	solutions[[i]] <- solve(
#  		solutions[[i]],
#  		b=cache$solutions[[i]]
#  	)
#  }

## ---- eval=use.Gurobi----------------------------------------------------
# generate prioritisation
cs_rs_amount <- solve(cs_rs_amount)

## ---- eval=!use.Gurobi, include=FALSE------------------------------------
#  cs_rs_amount <- solve(
#  	cs_rs_amount,
#  	b=cache$cs_rs_amount
#  )

## ---- eval={use.Gurobi}--------------------------------------------------
# make amount- and space-based prioritisation
cs_rs_space <- update(cs_rs_amount, space.target=0.5)

## ---- eval={!use.Gurobi}, include=FALSE----------------------------------
#  # make amount- and space-based prioritisation
#  cs_rs_space <- update(cs_rs_amount, space.target=0.5, solve=FALSE)
#  cs_rs_space <- solve(
#  	cs_rs_space,
#  	b=cache$cs_rs_space
#  )

