## ----global, include=FALSE-----------------------------------------------
# load rapr R package
library(rapr)

## ---- eval=FALSE---------------------------------------------------------
#  # load rapr R package
#  library(rapr)
#  
#  # see package help file
#  ?rapr

## ---- eval={is.GurobiInstalled(verbose=FALSE)}---------------------------
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

## ---- eval={is.GurobiInstalled(verbose=FALSE)}, fig.width=7, fig.height=2.5----
# change the plot parameters, so we can plot the distributions side by side
par(mfrow=c(1,3), mar=c(5.1, 4.1, 4.1, 4))

# uniform species
plot(sim_spp[[1]], main='uniform species')
lines(sim_pus)

# normal species
plot(sim_spp[[2]], main='normal species')
lines(sim_pus)

# bimodal species
plot(sim_spp[[3]], main='bimodal species')
lines(sim_pus)

# reset plot parameters
par(mfrow=c(1,1), mar=c(5.1, 4.1, 4.1, 2.1))

## ---- eval={is.GurobiInstalled(verbose=FALSE)}---------------------------
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

## ---- eval={is.GurobiInstalled(verbose=FALSE)}---------------------------
## create RapUnreliableOpts object
# this stores parameters for the problem (eg. BLM)
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

## ---- eval={is.GurobiInstalled(verbose=FALSE)}---------------------------
# create new object with just the uniform species
sim_ru_s1 <- spp.subset(sim_ru, 'uniform')

# update amount targets to 20% and space targets to 0%
sim_ru_s1 <- update(sim_ru_s1, amount.target=0.2, space.target=0, solve=FALSE)

# solve problem to identify prioritisation
sim_rs_s1_amount <- solve(sim_ru_s1)

## ---- eval={is.GurobiInstalled(verbose=FALSE)}, fig.height=4.6, fig.width=4.6----
# plot the prioritisation
plot(sim_rs_s1_amount)

# plot the prioritisation and the uniform species' distribution
spp.plot(sim_rs_s1_amount, 1, main='Uniform species')

## ---- eval={is.GurobiInstalled(verbose=FALSE)}---------------------------
# create new object with just the normal species
sim_ru_s2 <- spp.subset(sim_ru, 'normal')

# update amount targets to 20% and space targets to 0% and solve it
sim_rs_s2_amount <- update(sim_ru_s2, amount.target=0.2, space.target=0, solve=TRUE)

## ---- eval={is.GurobiInstalled(verbose=FALSE)}, fig.height=4.6, fig.width=4.6----
# plot the prioritisation
plot(sim_rs_s2_amount)

# plot the prioritisation and the normal species' distribution
spp.plot(sim_rs_s2_amount, 1, main='Normal species')

## ---- eval={is.GurobiInstalled(verbose=FALSE)}, fig.height=4.6, fig.width=4.6----
# create new object with just the bimodal species
sim_ru_s3 <- spp.subset(sim_ru, 'bimodal')

# update amount targets to 20% and space targets to 0% and solve it
sim_rs_s3_amount <- update(sim_ru_s3, amount.target=0.2, space.target=0)

# plot the prioritisation
plot(sim_rs_s3_amount)

# plot the prioritisation and the bimodal species' distribution
spp.plot(sim_rs_s3_amount, 1, main='Bimodal species')

## ---- eval={is.GurobiInstalled(verbose=FALSE)}---------------------------
# make new prioritisation
sim_rs_s1_space <- update(sim_rs_s1_amount, amount.target=0.2, space.target=0.85)

## ---- eval={is.GurobiInstalled(verbose=FALSE)}, fig.height=4.6, fig.width=4.6----
# plot the prioritisation
plot(sim_rs_s1_space)

# plot the prioritisation and the uniform species' distribution
spp.plot(sim_rs_s1_space, 'uniform', main='Uniform species')

## ---- eval={is.GurobiInstalled(verbose=FALSE)}, fig.height=5.5, fig.width=5.5----
# plot the difference between old and new prioritisations
plot(sim_rs_s1_amount, sim_rs_s1_space, 1, 1, main='Difference between solutions')

## ---- eval={is.GurobiInstalled(verbose=FALSE)}, fig.height=4.6, fig.width=4.6----
# make new prioritisation
sim_rs_s2_space <- update(sim_rs_s2_amount, amount.target=0.2, space.target=0.85)

# plot the prioritisation
plot(sim_rs_s2_space)

# plot the prioritisation and the normal species' distribution
spp.plot(sim_rs_s2_space, 'normal', main='Normal species')

## ---- eval={is.GurobiInstalled(verbose=FALSE)}, fig.height=5.5, fig.width=5.5----
# plot the difference between old and new prioritisations
plot(sim_rs_s2_amount, sim_rs_s2_space, 1, 1, main='Difference between solutions')

## ---- eval={is.GurobiInstalled(verbose=FALSE)}, fig.height=4.6, fig.width=4.6----
# make new prioritisation
sim_rs_s3_space <- update(sim_rs_s3_amount, amount.target=0.2, space.target=0.85)

# plot the prioritisation
plot(sim_rs_s3_space)

# plot the prioritisation and the bimodal species' distribution
spp.plot(sim_rs_s3_space, 'bimodal', main='Bimodal species')


## ---- eval={is.GurobiInstalled(verbose=FALSE)}, fig.height=5.5, fig.width=5.5----
# plot the difference between old and new prioritisations
plot(sim_rs_s3_amount, sim_rs_s3_space, 1, 1, main='Difference between solutions')

## ---- eval={is.GurobiInstalled(verbose=FALSE)}, fig.height=4.6, fig.width=4.6----
# make prioritisations
sim_mrs_amount <- update(sim_ru, amount.target=c(0.2,0.2,0.2), space.target=c(0,0,0))
sim_mrs_space <- update(sim_ru, amount.target=c(0.2,0.2,0.2), space.target=c(0.85, 0.85, 0.85))

# plot multi-species prioritisation with amount-based targets
plot(sim_mrs_amount, main='Amount-based targets')

# plot multi-species prioritisation with amount- and space-based targets
plot(sim_mrs_space, main='Amount and space-based targets')

## ---- eval={is.GurobiInstalled(verbose=FALSE)}, fig.height=5.5, fig.width=5.5----
# difference between the two prioritisations
plot(sim_mrs_amount, sim_mrs_space, 1, 1, main='Difference between solutions')

## ---- eval={is.GurobiInstalled(verbose=FALSE)}, fig.height=5.5, fig.width=5.5----
# load data
data(cs_spp)

# plot species distribution
plot(cs_spp, main='Pale-headed rosella distribution')

# add outline of Australia coastline
# requires the rworldmap R package
plot(spTransform(rworldmap::getMap(), cs_spp@crs), add=TRUE)

## ---- eval={is.GurobiInstalled(verbose=FALSE)}, fig.height=4, fig.width=4----
# load data
data(cs_pus)

## plot planning units
# convert SpatialPolygons to PolySet for quick plotting
cs_pus2 <- SpatialPolygons2PolySet(cs_pus)

# create vector of colors for planning units
# + light green: units not already inside reserve
# + yellow: units already inside reserve
cs_pus_cols <- rep('#c7e9c0', nrow(cs_pus@data))
cs_pus_cols[which(cs_pus$status==2)] <- 'yellow'

# set plotting window
par(mar=c(0.1, 0.1, 4.1, 0.1))

# plot polygons
PBSmapping::plotPolys(cs_pus2, col=cs_pus_cols, border='gray30', xlab='', ylab='', axes=FALSE, main='Case-study planning units')

# reset plotting window
par(mfrow=c(1,1), mar=c(5.1, 4.1, 4.1, 2.1))

## ---- eval={is.GurobiInstalled(verbose=FALSE)}, fig.height=3.5, fig.width=7.2----
# set up plotting window
par(mfrow=c(1,3), mar=c(5.1, 4.1, 4.1, 4.5))

par(mfrow=c(1,2))
# plot first variable
plot(cs_space[[1]], main='DC1')

# plot second variable
plot(cs_space[[2]], main='DC2')

# reset plot parameters
par(mfrow=c(1,1), mar=c(5.1, 4.1, 4.1, 2.1))

## ---- eval={is.GurobiInstalled(verbose=FALSE)}, fig.height=4.6, fig.width=4.6----
# make amount-based prioritisation
cs_rs_amount <- rap(
	cs_pus, cs_spp, cs_space,
  amount.target=0.2, space.target=0, n.demand.points=100L,
  include.geographic.space=TRUE, formulation='unreliable'
)

# plot prioritisation
plot(cs_rs_amount)

## ---- eval={is.GurobiInstalled(verbose=FALSE)}, fig.height=3.5, fig.width=7.2----
# plot prioritisation in geographic attribute space
space.plot(cs_rs_amount, 1, 2, main='Geographic space')

# plot prioritisation in environmental attribute space
space.plot(cs_rs_amount, 1, 1, main='Environmental space')

## ---- eval={is.GurobiInstalled(verbose=FALSE)}, fig.height=4.6, fig.width=4.6----
# make amount- and space-based prioritisation
cs_rs_space <- update(cs_rs_amount, space.target=0.85)

# plot prioritisation
plot(cs_rs_space)

## ---- eval={is.GurobiInstalled(verbose=FALSE)}, fig.height=3.5, fig.width=7.2----
# plot prioritisation in geographic attribute space
space.plot(cs_rs_space, 1, 2, main='Geographic space')

# plot prioritisation in environmental attribute space
space.plot(cs_rs_space, 1, 1, main='Environmental space')

