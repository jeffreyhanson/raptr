## ----global, include=FALSE-----------------------------------------------
# set cache globally
knitr::opts_chunk$set(cache=TRUE)
library(raptr)

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

## ---- fig.width=7, fig.height=2.5, fig.cap="_Distribution of three simulated species. Each square represents a planning unit. The colour of each square denotes the probability that individuals from each species occupy it._"----
# plot species
plot(
	stack(sim_spp),
	main=c('Uniform species','Normal species','Bimodal species'),
	addfun=function(){lines(sim_pus)},
	nc=3
)

## ------------------------------------------------------------------------
# generate coordinates for pus/demand points
pu_coords <- gCentroid(sim_pus, byid=TRUE)

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
				pu_coords@coords,
				weights=c(x)
			)
		)
	}
)

## ------------------------------------------------------------------------
## create RapUnreliableOpts object
# this stores parameters for the unreliable formulation problem (ie. BLM)
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

pu_points <- PlanningUnitPoints(
	coords=pu_coords@coords,
	ids=seq_len(nrow(sim_pus@data))
)

attr_spaces <- AttributeSpaces(
	list(
		AttributeSpace(
			planning.unit.points=pu_points,
			demand.points=sim_dps[[1]],
			species=1L
		),
		AttributeSpace(
			planning.unit.points=pu_points,
			demand.points=sim_dps[[2]],
			species=2L
		),
		AttributeSpace(
			planning.unit.points=pu_points,
			demand.points=sim_dps[[3]],
			species=3L
		)
	),
	name='geographic'
)

# generate boundary data information
boundary <- calcBoundaryData(sim_pus)

## create RapData object
# this stores all the input data for the prioritisation
sim_rd <- RapData(
	sim_pus@data,
	species,
	targets,
	pu_probabilities,
	list(attr_spaces),
	boundary,
	SpatialPolygons2PolySet(sim_pus)
)

## create RapUnsolved object
# this stores all the input data and parameters needed to generate prioritisations
sim_ru <- RapUnsolved(sim_ro, sim_rd)

## ------------------------------------------------------------------------
# create new object with just the uniform species
sim_ru_s1 <- spp.subset(sim_ru, 'uniform')

# update amount targets to 20% and space targets to 0%
sim_ru_s1 <- update(sim_ru_s1, amount.target=0.2, space.target=NA, solve=FALSE)

## ------------------------------------------------------------------------
# solve problem to identify prioritisation
sim_rs_s1_amount <- solve(sim_ru_s1, Threads=threads)

## ------------------------------------------------------------------------
## show summary
# note the format for this is similar to that used by Marxan
# see ?raptr::summary for details on this table
summary(sim_rs_s1_amount)

# show amount held
amount.held(sim_rs_s1_amount)

# show space held
space.held(sim_rs_s1_amount)

## ---- fig.height=3.5, fig.width=4.5, fig.align='center', fig.cap="_A prioritisation for the uniformly distributed species generated using amount-based targets (20\\%). Sqaures represent planning units. Planning units with a green border are selected for prioritisation, and their colour denotes the probability they are inhabited by the species._"----
# plot the prioritisation and the uniform species' distribution
spp.plot(sim_rs_s1_amount, 1, main='Uniform species')

## ------------------------------------------------------------------------
# create new object with just the normal species
sim_ru_s2 <- spp.subset(sim_ru, 'normal')

## ------------------------------------------------------------------------
# update amount targets to 20% and space targets to 0% and solve it
sim_rs_s2_amount <- update(sim_ru_s2, amount.target=0.2, space.target=NA, solve=TRUE, Threads=threads)

## ------------------------------------------------------------------------
# show summary
summary(sim_rs_s2_amount)

# show amount held
amount.held(sim_rs_s2_amount)

# show space held
space.held(sim_rs_s2_amount)

## ---- fig.height=3.5, fig.width=4.5,fig.align='center', fig.cap="_A prioritisation for the normally distributed species generated using amount-based targets (20\\%). See Figure 3 caption for conventions._"----
# plot the prioritisation and the normal species' distribution
spp.plot(sim_rs_s2_amount, 1, main='Normal species')

## ------------------------------------------------------------------------
# create new object with just the bimodal species
sim_ru_s3 <- spp.subset(sim_ru, 'bimodal')

## ------------------------------------------------------------------------
# update amount targets to 20% and space targets to 0% and solve it
sim_rs_s3_amount <- update(sim_ru_s3, amount.target=0.2, space.target=NA, Threads=threads)

## ---- fig.height=3.5, fig.width=4.5,fig.align='center', fig.cap="_A prioritisation for the bimodally distributed species generated using amount-based targets (20\\%). See Figure 3 caption for conventions._"----
# plot the prioritisation and the bimodal species' distribution
spp.plot(sim_rs_s3_amount, 1, main='Bimodal species')

## ------------------------------------------------------------------------
# show summary
summary(sim_rs_s3_amount)

# show amount held
amount.held(sim_rs_s3_amount)

# show space held
space.held(sim_rs_s3_amount)

## ------------------------------------------------------------------------
# make new prioritisation
sim_rs_s1_space <- update(sim_rs_s1_amount, amount.target=0.2, space.target=0.85, Threads=threads)

## ------------------------------------------------------------------------
# show summary
summary(sim_rs_s1_space)

# show amount held
amount.held(sim_rs_s1_space)

# show space held
space.held(sim_rs_s1_space)

## ---- fig.height=3.5, fig.width=4.5,fig.align='center', fig.cap="_A prioritisation for the uniformly distributed species generated using amount-based targets (20\\%) and space-based targets (85\\%). See Figure 3 caption for conventions._"----
# plot the prioritisation and the uniform species' distribution
spp.plot(sim_rs_s1_space, 'uniform', main='Uniform species')

## ---- fig.height=3.5, fig.width=4.5,fig.align='center', fig.cap="_Difference between two prioritisations for the uniformly distributed species. Prioritisation $X$ was generated using just amount-based targets (20\\%), and prioritisation $Y$ was generated using an additional space-based target (85\\%)._"----
# plot the difference between old and new prioritisations
plot(sim_rs_s1_amount, sim_rs_s1_space, 1, 1, main='Difference between solutions')

## ------------------------------------------------------------------------
# make new prioritisation
sim_rs_s2_space <- update(sim_rs_s2_amount, amount.target=0.2, space.target=0.85, Threads=threads)

## ------------------------------------------------------------------------
# show summary
summary(sim_rs_s2_space)

# show amount held
amount.held(sim_rs_s2_space)

# show space held
space.held(sim_rs_s2_space)

## ---- fig.height=3.5, fig.width=4.5,fig.align='center', fig.cap="_A prioritisation for the normally distributed species generated using amount-based targets (20\\%) and space-based targets (85\\%). See Figure 3 caption for conventions._"----

# plot the prioritisation and the normal species' distribution
spp.plot(sim_rs_s2_space, 'normal', main='Normal species')

## ---- fig.height=3.5, fig.width=4.5,fig.align='center', fig.cap="_Difference between two prioritisations for the normally distributed species. See Figure 7 caption for conventions._"----
# plot the difference between old and new prioritisations
plot(sim_rs_s2_amount, sim_rs_s2_space, 1, 1, main='Difference between solutions')

## ------------------------------------------------------------------------
# make new prioritisation
sim_rs_s3_space <- update(sim_rs_s3_amount, amount.target=0.2, space.target=0.85, Threads=threads)

## ------------------------------------------------------------------------
# show summary
summary(sim_rs_s3_space)

# show amount held
amount.held(sim_rs_s3_space)

# show space held
space.held(sim_rs_s3_space)


## ---- fig.height=3.5, fig.width=4.5,fig.align='center', fig.cap="_A prioritisation for the normally distributed species generated using amount-based targets (20\\%) and space-based targets (85\\%). See Figure 3 caption for conventions._"----
# plot the prioritisation and the bimodal species' distribution
spp.plot(sim_rs_s3_space, 'bimodal', main='Bimodal species')

## ---- fig.height=3.5, fig.width=4.5,fig.align='center', fig.cap="_Difference between two prioritisations for the bimodally distributed species. See Figure 7 caption for conventions._"----
# plot the difference between old and new prioritisations
plot(sim_rs_s3_amount, sim_rs_s3_space, 1, 1, main='Difference between solutions')

## ------------------------------------------------------------------------
# make prioritisations
sim_mrs_amount <- update(
	sim_ru,
	amount.target=c(0.2,0.2,0.2),
	space.target=c(0,0,0),
	Threads=threads
)

sim_mrs_space <- update(
	sim_ru,
	amount.target=c(0.2,0.2,0.2),
	space.target=c(0.85, 0.85, 0.85),
	Threads=threads
)

## ------------------------------------------------------------------------
# show summaries
summary(sim_mrs_amount)
summary(sim_mrs_space)

# show amount held for each prioritisation
amount.held(sim_mrs_amount)
amount.held(sim_mrs_space)

# show space held for each prioritisation
space.held(sim_mrs_amount)
space.held(sim_mrs_space)

## ---- fig.height=3.5, fig.width=4.5,fig.align='center', fig.cap="_A multi-species prioritisation for the uniformly, normally, and bimodally distributed species generated using just amount-based targets (20\\%). Squares represent planning units. Dark green planning units are selected for preservation._"----
# plot multi-species prioritisation with amount-based targets
plot(sim_mrs_amount, 1, main='Amount-based targets')

## ---- fig.height=3.5, fig.width=4.5,fig.align='center', fig.cap="_A multi-species prioritisation for the uniformly, normally, and bimodally distributed species generated using amount-based targets (20\\%) and space-based targets (85\\%). See Figure 12 caption for conventions._"----
# plot multi-species prioritisation with amount- and space-based targets
plot(sim_mrs_space, 1, main='Amount and space-based targets')

## ---- fig.height=3.5, fig.width=4.5,fig.align='center', fig.cap="_Difference between two multi-species prioritisations. See Figure 7 caption for conventions.)"----
# difference between the two prioritisations
plot(sim_mrs_amount, sim_mrs_space, 1, 1, main='Difference between solutions')

## ------------------------------------------------------------------------
# make new prioritisation with probability threshold of 0.1 for each species
sim_mrs_space2 <- solve(
	prob.subset(
		sim_mrs_space,
		species=1:3,
		threshold=c(0.1,0.1,0.1)
	),
	Threads=threads
)

## ------------------------------------------------------------------------
# show summary
summary(sim_mrs_space2)

## ---- fig.height=3.5, fig.width=4.5,fig.align='center', fig.cap="_A multi-species prioritisation for the uniformly, normally, and bimodally distributed species generated using amount-based targets (20\\%) and space-based targets (85\\%). This priorititisation was generated to be robust against low occupancy probabilities, by preventing planning units with low probabilities from being used to represent demand points. See Figure 12 caption for conventions._"----
# plot prioritisation
plot(sim_mrs_space2, 1)

## ---- fig.height=3.5, fig.width=4.5,fig.align='center', fig.cap="_Difference between two multi-species prioritisations. See Figure 7 caption for conventions._"----
# difference between prioritisations that use and do not use thresholds
plot(sim_mrs_space2, sim_mrs_space, 1, 1, main='Difference between solutions')

## ------------------------------------------------------------------------
# make new prioritisation using reliable formulation
sim_mrs_space3 <- try(update(sim_mrs_space, formulation='reliable', max.r.level=1L, Threads=threads))

## ------------------------------------------------------------------------
# make new prioritisation using reliable formulation and reduced targets
sim_mrs_space3 <- update(
	sim_mrs_space,
	formulation='reliable',
	max.r.level=1L,
	space.target=-1000,
	Threads=threads
)

## ------------------------------------------------------------------------
# show summary
summary(sim_mrs_space3)

## ---- fig.height=3.5, fig.width=4.5,fig.align='center', fig.cap="_A multi-species prioritisation for the uniformly, normally, and bimodally distributed species generated using amount-based targets (20\\%) and space-based targets (85\\%). This priorititisation was generated to be robust against low occupancy probabilities, by explicitly using the probability of occupancy data when deriving a solution. See Figure 12 caption for conventions._"----
# plot prioritisation
plot(sim_mrs_space3, 1)

## ---- fig.height=3.5, fig.width=4.5,fig.align='center', fig.cap="_Difference between two multi-species prioritisations. See Figure 7 caption for conventions._"----
# difference between prioritisations based on unreliable and reliable formulation
plot(sim_mrs_space3, sim_mrs_space, 1, 1, main='Difference between solutions')

