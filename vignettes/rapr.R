## ----global, include=FALSE-----------------------------------------------
# load rapr R package
library(rapr)
# set seed for reproducibility
set.seed(500)

## ---- echo=FALSE, fig.height=4, fig.width=4, fig.cap="Figure 1. An example of an environmental attribute space. This environmental space has dimensions relating to temperature and rainfall values. Points denote demand points. Letters denote populations. The populations A and C have relatively similar environmental conditions. Whereas, populations A and B have relatively different environmental conditions."----
## load packages
library(ggplot2)

## generate data
# population data
populations <- data.frame(
	population=LETTERS[1:4],
	temperature=c(22,29,21.6,28),
	rainfall=c(6,12,5.5,6)
)
# demand point data
make.dp <- function(x, n, pad.percent=0.2) {
	padding<-diff(range(x))*pad.percent
	return(seq(min(x)-padding, max(x)+padding, length.out=n))
}
demand.points <- expand.grid(
	temperature=make.dp(populations$temperature, n=4, pad.percent=0.2),
	rainfall=make.dp(populations$rainfall, n=4, pad.percent=0.2)
)

## plot data
ggplot(aes(x=temperature, y=rainfall), data=populations) +
	geom_point(data=demand.points, fill='gray70') +
	geom_text(aes(label=population), size=10) +
	xlab(expression('Temperature ('*degree*'C)')) +
	ylab('Rainfall (mm)') +
	theme_classic()

## ---- eval=FALSE---------------------------------------------------------
#  # load rapr R package
#  library(rapr)
#  
#  # show package overview
#  ?rapr

## ------------------------------------------------------------------------
# make planning units
sim_pus <- sim.pus(100L)

# simulate species distributions
sim_spp <- lapply(
	c('uniform', 'normal', 'bimodal'),
	sim.species,
  n=1,
  x=sim_pus,
  res=1
)

## ---- fig.width=7, fig.height=2.5----------------------------------------
# plot species
plot(
	stack(sim_spp),
	main=c('Uniform species','Normal species','Bimodal species'),
	addfun=function(){lines(sim_pus)},
	nc=3
)

## ------------------------------------------------------------------------
# generate coordinates for pus/demand points
pu_coords <- rgeos::gCentroid(sim_pus, byid=TRUE)

# calculate weights
sim_dps <- lapply(
	sim_spp,
	function(x) {
		return(extract(x, pu_coords))
	}
)

# create demand point objects
sim_dps <- lapply(
	sim_dps,
	function(x) {
		return(
			DemandPoints(
				SimplePoints(pu_coords@coords),
				c(x)
			)
		)
	}
)

## ------------------------------------------------------------------------
## create RapUnreliableOpts object
# this stores parameters for the unreliable formulation problem (eg. BLM)
sim_ro <- RapUnreliableOpts()

## create RapData object
# create data.frame with species info
species <- data.frame(
  name=c('uniform', 'normal', 'bimodal')
)

## create data.frame with species and space targets
# amount targets at 20% (denoted with target=0)
# space targets at 20% (denoted with target=1)
targets <- expand.grid(
  species=1:3,
  target=0:1,
  proportion=0.2
)

# calculate probability of each species in each pu
pu_probabilities <- calcSpeciesAverageInPus(sim_pus, stack(sim_spp))

## create AttributeSpace object
# this stores the coordinates of the planning units in an attribute space
# and the coordinates and weights of demand points in the space
attr_space <- AttributeSpace(
  SimplePoints(pu_coords@coords),
  sim_dps
)

# generate boundary data information
boundary <- calcBoundaryData(sim_pus)

## create RapData object
# this store all the input data for the prioritisation
sim_rd <- RapData(
  sim_pus@data,
  species,
  targets,
  pu_probabilities,
  list(attr_space),
  boundary,
  SpatialPolygons2PolySet(sim_pus)
)

## create RapUnsolved object
# this store all the input data and parameters needed to generate prioritisations
sim_ru <- RapUnsolved(sim_ro, sim_rd)

## ------------------------------------------------------------------------
# create new object with just the uniform species
sim_ru_s1 <- spp.subset(sim_ru, 'uniform')

# update amount targets to 20% and space targets to 0%
sim_ru_s1 <- update(sim_ru_s1, amount.target=0.2, space.target=0, solve=FALSE)

## ---- eval=is.GurobiInstalled(verbose=FALSE)-----------------------------
# solve problem to identify prioritisation
sim_rs_s1_amount <- solve(sim_ru_s1)

## ---- eval=!is.GurobiInstalled(verbose=FALSE), include=FALSE-------------
#  sim_rs_s1_amount <- solve(sim_ru_s1, b=81:100)

## ------------------------------------------------------------------------
## show summary
# note the format for this is identical to that used by Marxan
summary(sim_rs_s1_amount)

# show amount held
amount.held(sim_rs_s1_amount)

# show space held
space.held(sim_rs_s1_amount)

## ---- fig.height=5.5, fig.width=5.5--------------------------------------
# plot the first (and only) solution in the prioritisation
plot(sim_rs_s1_amount, 1)

## ---- fig.height=5.5, fig.width=5.5--------------------------------------
# plot the prioritisation and the uniform species' distribution
spp.plot(sim_rs_s1_amount, 1, main='Uniform species')

## ------------------------------------------------------------------------
# create new object with just the normal species
sim_ru_s2 <- spp.subset(sim_ru, 'normal')

## ---- eval=is.GurobiInstalled(verbose=FALSE)-----------------------------
# update amount targets to 20% and space targets to 0% and solve it
sim_rs_s2_amount <- update(sim_ru_s2, amount.target=0.2, space.target=0, solve=TRUE)

## ---- eval=!is.GurobiInstalled(verbose=FALSE), include=FALSE-------------
#  # update amount targets to 20% and space targets to 0% and solve it
#  sim_rs_s2_amount <- update(sim_ru_s2, amount.target=0.2, space.target=0, solve=FALSE)
#  sim_rs_s2_amount <-solve(
#  	sim_rs_s2_amount,
#  	b=c(35L, 36L, 44L, 45L, 46L, 55L, 56L, 57L, 65L, 66L)
#  )

## ------------------------------------------------------------------------
# show summary
summary(sim_rs_s2_amount)

# show amount held
amount.held(sim_rs_s2_amount)

# show space held
space.held(sim_rs_s2_amount)

## ---- fig.height=5.5, fig.width=5.5--------------------------------------
# plot the prioritisation
plot(sim_rs_s2_amount, 1)

# plot the prioritisation and the normal species' distribution
spp.plot(sim_rs_s2_amount, 1, main='Normal species')

## ------------------------------------------------------------------------
# create new object with just the bimodal species
sim_ru_s3 <- spp.subset(sim_ru, 'bimodal')

## ---- eval={is.GurobiInstalled(verbose=FALSE)}---------------------------
# update amount targets to 20% and space targets to 0% and solve it
sim_rs_s3_amount <- update(sim_ru_s3, amount.target=0.2, space.target=0)

## ---- eval=!is.GurobiInstalled(verbose=FALSE), include=FALSE-------------
#  # update amount targets to 20% and space targets to 0% and solve it
#  sim_rs_s3_amount <- update(sim_ru_s3, amount.target=0.2, space.target=0, solve=FALSE)
#  sim_rs_s3_amount <- solve(
#  	sim_rs_s3_amount,
#  	b=c(53L, 63L, 64L, 72L, 73L, 74L, 83L, 84L)
#  )

## ---- fig.height=5.5, fig.width=5.5--------------------------------------
# plot the prioritisation
plot(sim_rs_s3_amount, 1)

# plot the prioritisation and the bimodal species' distribution
spp.plot(sim_rs_s3_amount, 1, main='Bimodal species')

# show summary
summary(sim_rs_s3_amount)

# show amount held
amount.held(sim_rs_s3_amount)

# show space held
space.held(sim_rs_s3_amount)

## ---- eval=is.GurobiInstalled(verbose=FALSE)-----------------------------
# make new prioritisation
sim_rs_s1_space <- update(sim_rs_s1_amount, amount.target=0.2, space.target=0.85)

## ---- eval=!is.GurobiInstalled(verbose=FALSE), include=FALSE-------------
#  # make new prioritisation
#  sim_rs_s1_space <- update(sim_rs_s1_amount, amount.target=0.2, space.target=0.85, solve=FALSE)
#  sim_rs_s1_space <- solve(sim_rs_s1_space, b=c(53L, 63L, 64L, 72L, 73L, 74L, 83L, 84L))

## ------------------------------------------------------------------------
# show summary
summary(sim_rs_s1_space)

# show amount held
amount.held(sim_rs_s1_space)

# show space held
space.held(sim_rs_s1_space)

## ---- fig.height=5.5, fig.width=5.5--------------------------------------
# plot the prioritisation
plot(sim_rs_s1_space, 1)

# plot the prioritisation and the uniform species' distribution
spp.plot(sim_rs_s1_space, 'uniform', main='Uniform species')

## ---- fig.height=5.5, fig.width=5.5--------------------------------------
# plot the difference between old and new prioritisations
plot(sim_rs_s1_amount, sim_rs_s1_space, 1, 1, main='Difference between solutions')

## ---- eval=is.GurobiInstalled(verbose=FALSE)-----------------------------
# make new prioritisation
sim_rs_s2_space <- update(sim_rs_s2_amount, amount.target=0.2, space.target=0.85)

## ---- eval=!is.GurobiInstalled(verbose=FALSE), include=FALSE-------------
#  # make new prioritisation
#  sim_rs_s2_space <- update(sim_rs_s2_amount, amount.target=0.2, space.target=0.85, solve=FALSE)
#  sim_rs_s2_space <- solve(sim_rs_s2_space, b=c(23L, 25L, 28L, 44L, 46L, 47L, 53L, 56L, 59L, 65L, 66L, 73L, 78L, 86L))
#  

## ---- fig.height=5.5, fig.width=5.5--------------------------------------
# show summary
summary(sim_rs_s2_space)

# show amount held
amount.held(sim_rs_s2_space)

# show space held
space.held(sim_rs_s2_space)

# plot the prioritisation
plot(sim_rs_s2_space, 1)

# plot the prioritisation and the normal species' distribution
spp.plot(sim_rs_s2_space, 'normal', main='Normal species')

## ---- fig.height=5.5, fig.width=5.5--------------------------------------
# plot the difference between old and new prioritisations
plot(sim_rs_s2_amount, sim_rs_s2_space, 1, 1, main='Difference between solutions')

## ---- eval=is.GurobiInstalled(verbose=FALSE)-----------------------------
# make new prioritisation
sim_rs_s3_space <- update(sim_rs_s3_amount, amount.target=0.2, space.target=0.85)

## ---- eval=!is.GurobiInstalled(verbose=FALSE), include=FALSE-------------
#  # make new prioritisation
#  sim_rs_s3_space <- update(sim_rs_s3_amount, amount.target=0.2, space.target=0.85, solve=FALSE)
#  sim_rs_s3_space <- solve(sim_rs_s3_space, b=c(18L, 29L, 42L, 55L, 63L, 64L, 72L, 74L, 76L, 84L))

## ---- fig.height=5.5, fig.width=5.5--------------------------------------
# show summary
summary(sim_rs_s3_space)

# show amount held
amount.held(sim_rs_s3_space)

# show space held
space.held(sim_rs_s3_space)

# plot the prioritisation
plot(sim_rs_s3_space, 1)

# plot the prioritisation and the bimodal species' distribution
spp.plot(sim_rs_s3_space, 'bimodal', main='Bimodal species')

## ---- fig.height=5.5, fig.width=5.5--------------------------------------
# plot the difference between old and new prioritisations
plot(sim_rs_s3_amount, sim_rs_s3_space, 1, 1, main='Difference between solutions')

## ---- eval=!is.GurobiInstalled(verbose=FALSE), include=FALSE-------------
#  # make prioritisations
#  sim_mrs_amount <- update(sim_ru, amount.target=c(0.2,0.2,0.2), space.target=c(0,0,0), solve=FALSE)
#  sim_mrs_amount <- solve(sim_mrs_amount, b=c(14L, 35L, 36L, 46L, 47L, 53L, 54L, 55L, 57L, 62L, 63L, 65L, 69L, 71L, 72L, 73L, 74L, 75L, 83L, 93L))
#  
#  sim_mrs_space <- update(sim_ru, amount.target=c(0.2,0.2,0.2), space.target=c(0.85, 0.85, 0.85), solve=FALSE)
#  sim_mrs_space <- solve(sim_mrs_space, b=c(3L, 16L, 17L, 22L, 25L, 30L, 36L, 38L, 43L, 46L, 56L, 62L, 64L, 66L, 69L, 70L, 73L, 85L, 87L, 92L, 100L))

## ---- fig.height=5.5, fig.width=5.5--------------------------------------
# show summaries
summary(sim_mrs_amount)
summary(sim_mrs_space)

# show amount held for each prioritisation
amount.held(sim_mrs_amount)
amount.held(sim_mrs_space)

# show space held for each prioritisation
space.held(sim_mrs_amount)
space.held(sim_mrs_space)

# plot multi-species prioritisation with amount-based targets
plot(sim_mrs_amount, 1, main='Amount-based targets')

# plot multi-species prioritisation with amount- and space-based targets
plot(sim_mrs_space, 1, main='Amount and space-based targets')

## ---- eval={is.GurobiInstalled(verbose=FALSE)}, fig.height=5.5, fig.width=5.5----
# difference between the two prioritisations
plot(sim_mrs_amount, sim_mrs_space, 1, 1, main='Difference between solutions')

## ---- eval=FALSE---------------------------------------------------------
#  # update prioritisation
#  sim_mrs_space_blm <- update(sim_mrs_space, BLM=100)

## ---- eval={!is.GurobiInstalled(verbose=FALSE) || !exists('sim_mrs_space_blm')}, include=FALSE----
sim_mrs_space_blm <- update(sim_mrs_space, BLM=100, solve=FALSE)
sim_mrs_space_blm <- solve(
	sim_mrs_space_blm,
	b=c(13L, 23L, 24L, 25L, 26L, 27L, 28L, 33L, 34L, 35L, 36L, 37L, 
		38L, 43L, 44L, 45L, 46L, 47L, 48L, 53L, 54L, 55L, 56L, 57L, 58L, 
		63L, 64L, 65L, 66L, 67L, 68L, 73L, 74L, 75L, 76L, 77L, 78L
	)
)

## ---- fig.height=5.5, fig.width=5.5--------------------------------------
# show summary of prioritisation
summary(sim_mrs_space_blm)

# show amount held for each prioritisation
amount.held(sim_mrs_space_blm)

# show space held for each prioritisation
space.held(sim_mrs_space_blm)

# plot prioritisation
plot(sim_mrs_space_blm, 1)

## ---- fig.height=5.5, fig.width=5.5--------------------------------------
# difference between the two prioritisations
plot(sim_mrs_space_blm, sim_mrs_space, 1, 1, main='Difference between solutions')

## ------------------------------------------------------------------------
# create a vector with BLM values
blms <- seq(0, 100, length.out=10)

# create empty list to store solutions
solutions <- list()

## ---- eval=FALSE---------------------------------------------------------
#  # iterate over blms and create solutions
#  for (i in seq_along(blms))
#  	solutions[[i]] <- update(sim_mrs_space, BLM=blms[i])

## ---- eval={!is.GurobiInstalled(verbose=FALSE) | length(solutions)==0}, include=FALSE----
# iterate over blms and create solutions
for (i in seq_along(blms)) {
	solutions[[i]] <- update(sim_mrs_space, BLM=blms[i], solve=FALSE)
	solutions[[i]] <- solve(solutions[[i]], b=1:5)
}

## ------------------------------------------------------------------------
# create data.frame with BLM values, cost, and fragmentation
blm_results <- data.frame(
	BLM=blms,
	cost=sapply(solutions, function(i) {summary(i)$Cost}),
	fragmentation=sapply(solutions, function(i) {summary(i)$Connectivity_In_Fraction})
)

## ---- fig.height=5.5, fig.width=7.5--------------------------------------
# load ggplot2 package
library(ggplot2)

# plot showing cost, fragmentation, and 
ggplot(aes(x=fragmentation, y=cost), data=blm_results) +
	geom_line(color='grey60') +
	geom_text(aes(label=BLM), size=3) +
	ylab('Cost (number of planning units)') +
	xlab('Fragmentation (ratio of edge lengths inside reserves to exposed lengths)') +
	theme_classic()

## ---- fig.height=5.5, fig.width=5.5--------------------------------------
# plot prioritisation with blm=50
plot(solutions[[3]], 1)

## ---- eval={is.GurobiInstalled(verbose=FALSE)}---------------------------
# make new prioritisation with probability threshold of 0.5 for each species
sim_mrs_space2 <- solve(
	prob.subset(
		sim_mrs_space,
		species=1:3,
		threshold=c(0.5,0.5,0.5)
	)
)

## ---- eval=!is.GurobiInstalled(verbose=FALSE), include=FALSE-------------
#  # make new prioritisation with probability threshold of 0.5 for each species
#  sim_mrs_space2 <- solve(
#  	prob.subset(
#  		sim_mrs_space,
#  		species=1:3,
#  		threshold=c(0.5,0.5,0.5)
#  	),
#  	b=c(
#  		7L, 12L, 20L, 28L, 29L, 34L, 36L, 37L, 48L, 52L,
#  		53L, 55L, 56L, 64L, 67L, 70L, 72L, 75L, 84L, 89L
#  	)
#  )

## ---- fig.height=5.5, fig.width=5.5--------------------------------------
# show summary
summary(sim_mrs_space2)

# plot prioritisation
plot(sim_mrs_space2, 1)

## ---- fig.height=5.5, fig.width=5.5--------------------------------------
# difference between prioritisations that use and do not use thresholds
plot(sim_mrs_space2, sim_mrs_space, 1, 1, main='Difference between solutions')

