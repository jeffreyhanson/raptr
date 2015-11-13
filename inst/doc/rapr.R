## ----global, include=FALSE-----------------------------------------------
# load rapr R package
library(rapr)

## ---- echo=FALSE, fig.height=4, fig.width=4, fig.cap="Figure 1. An example of an environmental attribute space. This environmental space has dimensions relating to temperature $\\circ$ and rainfall values. Points denote demand points. Letters denote populations. The populations A and C have relatively similar environmental conditions. Whereas, populations A and B have relatively different environmental conditions."----
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

# show summary
summary(sim_rs_s1_amount)

# show amount held
amount.held(sim_rs_s1_amount)

# show space held
space.held(sim_rs_s1_amount)

## ---- eval={is.GurobiInstalled(verbose=FALSE)}, fig.height=5.5, fig.width=5.5----
# plot the first (and only) solution in the prioritisation
plot(sim_rs_s1_amount, 1)

# plot the prioritisation and the uniform species' distribution
spp.plot(sim_rs_s1_amount, 1, main='Uniform species')

## ---- eval={is.GurobiInstalled(verbose=FALSE)}---------------------------
# create new object with just the normal species
sim_ru_s2 <- spp.subset(sim_ru, 'normal')

# update amount targets to 20% and space targets to 0% and solve it
sim_rs_s2_amount <- update(sim_ru_s2, amount.target=0.2, space.target=0, solve=TRUE)

# show summary
summary(sim_rs_s2_amount)

# show amount held
amount.held(sim_rs_s2_amount)

# show space held
space.held(sim_rs_s2_amount)

## ---- eval={is.GurobiInstalled(verbose=FALSE)}, fig.height=5.5, fig.width=5.5----
# plot the prioritisation
plot(sim_rs_s2_amount, 1)

# plot the prioritisation and the normal species' distribution
spp.plot(sim_rs_s2_amount, 1, main='Normal species')

## ---- eval={is.GurobiInstalled(verbose=FALSE)}, fig.height=5.5, fig.width=5.5----
# create new object with just the bimodal species
sim_ru_s3 <- spp.subset(sim_ru, 'bimodal')

# update amount targets to 20% and space targets to 0% and solve it
sim_rs_s3_amount <- update(sim_ru_s3, amount.target=0.2, space.target=0)

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

## ---- eval={is.GurobiInstalled(verbose=FALSE)}---------------------------
# make new prioritisation
sim_rs_s1_space <- update(sim_rs_s1_amount, amount.target=0.2, space.target=0.85)

# show summary
summary(sim_rs_s1_space)

# show amount held
amount.held(sim_rs_s1_space)

# show space held
space.held(sim_rs_s1_space)

## ---- eval={is.GurobiInstalled(verbose=FALSE)}, fig.height=5.5, fig.width=5.5----
# plot the prioritisation
plot(sim_rs_s1_space)

# plot the prioritisation and the uniform species' distribution
spp.plot(sim_rs_s1_space, 'uniform', main='Uniform species')

## ---- eval={is.GurobiInstalled(verbose=FALSE)}, fig.height=5.5, fig.width=5.5----
# plot the difference between old and new prioritisations
plot(sim_rs_s1_amount, sim_rs_s1_space, 1, 1, main='Difference between solutions')

## ---- eval={is.GurobiInstalled(verbose=FALSE)}, fig.height=5.5, fig.width=5.5----
# make new prioritisation
sim_rs_s2_space <- update(sim_rs_s2_amount, amount.target=0.2, space.target=0.85)

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

## ---- eval={is.GurobiInstalled(verbose=FALSE)}, fig.height=5.5, fig.width=5.5----
# plot the difference between old and new prioritisations
plot(sim_rs_s2_amount, sim_rs_s2_space, 1, 1, main='Difference between solutions')

## ---- eval={is.GurobiInstalled(verbose=FALSE)}, fig.height=5.5, fig.width=5.5----
# make new prioritisation
sim_rs_s3_space <- update(sim_rs_s3_amount, amount.target=0.2, space.target=0.85)

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


## ---- eval={is.GurobiInstalled(verbose=FALSE)}, fig.height=5.5, fig.width=5.5----
# plot the difference between old and new prioritisations
plot(sim_rs_s3_amount, sim_rs_s3_space, 1, 1, main='Difference between solutions')

## ---- eval={is.GurobiInstalled(verbose=FALSE)}, fig.height=5.5, fig.width=5.5----
# make prioritisations
sim_mrs_amount <- update(sim_ru, amount.target=c(0.2,0.2,0.2), space.target=c(0,0,0))

sim_mrs_space <- update(sim_ru, amount.target=c(0.2,0.2,0.2), space.target=c(0.85, 0.85, 0.85))

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

## ---- eval={is.GurobiInstalled(verbose=FALSE)}, fig.height=5.5, fig.width=5.5----
# make new prioritisation with probability threshold of 0.7 for each species
sim_mrs_space2 <- solve(
	prob.subset(
		sim_mrs_space,
		species=2:3,
		threshold=c(0.7,0.7)
	)
)

# show summary
summary(sim_mrs_space2)

# plot prioritisation
plot(sim_mrs_space2, 1)

## ---- eval={is.GurobiInstalled(verbose=FALSE)}, fig.height=5.5, fig.width=5.5----
# difference between prioritisations that use and do not use thresholds
plot(sim_mrs_space2, sim_mrs_space, 1, 1, main='Difference between solutions')

## ---- eval={is.GurobiInstalled(verbose=FALSE)}, fig.height=5.5, fig.width=5.5----
# make new prioritisation using reliable formulation
sim_mrs_space3 <- update(sim_mrs_space, formulation='reliable', MaxRLevel=1L)

# show summary
summary(sim_mrs_space3)

# plot prioritisation
plot(sim_mrs_space3, 1)

## ---- eval={is.GurobiInstalled(verbose=FALSE)}, fig.height=5.5, fig.width=5.5----
# difference between prioritisations based on unreliable and reliable formulation
plot(sim_mrs_space3, sim_mrs_space, 1, 1, main='Difference between solutions')

## ---- eval={is.GurobiInstalled(verbose=FALSE)}, fig.height=5.5, fig.width=5.5----
# load data
data(cs_spp)

# plot species' distributions
plot(cs_spp)

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
PBSmapping::plotPolys(
	cs_pus2, col=cs_pus_cols, border='gray30', 
	xlab='', ylab='', axes=FALSE, 
	main='Case-study planning units'
)

# reset plotting window
par(mfrow=c(1,1), mar=c(5.1, 4.1, 4.1, 2.1))

## ---- eval={is.GurobiInstalled(verbose=FALSE)}, fig.height=3.5, fig.width=7.2----
# load data
data(cs_space)

# plot variables
plot(cs_space)

## ---- eval={is.GurobiInstalled(verbose=FALSE)}, fig.height=14, fig.width=7.2----
# plot prioritisation in geographic attribute space
p1 <- space.plot(cs_rs_amount, 1, 2, main='Blue-winged kookuburra')
p2 <- space.plot(cs_rs_amount, 2, 2, main='Brown-backed honeyeater')
p3 <- space.plot(cs_rs_amount, 3, 2, main='Brown falcon')
p4 <- space.plot(cs_rs_amount, 4, 2, main='Pale-headed rosella')
gridExtra::grid.arrange(p1, p2, p3, p4, ncol=1)

# plot prioritisation in environmental attribute space
p1 <- space.plot(cs_rs_amount, 1, 1, main='Blue-winged kookuburra')
p2 <- space.plot(cs_rs_amount, 2, 1, main='Brown-backed honeyeater')
p3 <- space.plot(cs_rs_amount, 3, 1, main='Brown falcon')
p4 <- space.plot(cs_rs_amount, 4, 1, main='Pale-headed rosella')
gridExtra::grid.arrange(p1, p2, p3, p4, ncol=1)

## ---- eval={is.GurobiInstalled(verbose=FALSE)}, fig.height=14, fig.width=7.2----
# plot prioritisation in geographic attribute space
p1 <- space.plot(cs_rs_space, 1, 2, main='Blue-winged kookuburra')
p2 <- space.plot(cs_rs_space, 2, 2, main='Brown-backed honeyeater')
p3 <- space.plot(cs_rs_space, 3, 2, main='Brown falcon')
p4 <- space.plot(cs_rs_space, 4, 2, main='Pale-headed rosella')
gridExtra::grid.arrange(p1, p2, p3, p4, ncol=1)

# plot prioritisation in environmental attribute space
p1 <- space.plot(cs_rs_space, 1, 1, main='Blue-winged kookuburra')
p2 <- space.plot(cs_rs_space, 2, 1, main='Brown-backed honeyeater')
p3 <- space.plot(cs_rs_space, 3, 1, main='Brown falcon')
p4 <- space.plot(cs_rs_space, 4, 1, main='Pale-headed rosella')
gridExtra::grid.arrange(p1, p2, p3, p4, ncol=1)

## ---- eval={is.GurobiInstalled(verbose=FALSE)}---------------------------
# generate vector with Australia's selections
aus_selections <- which(cs_pus$status>0)

# create new object with Australia's network
cs_rs_aus <- update(cs_rs_amount, b=aus_selections)

## ---- eval={is.GurobiInstalled(verbose=FALSE)}, fig.height=4.5, fig.width=7.2----
# load packages
library(dplyr)
library(ggplot2)

# define standard error function
se=function(x){sd(x,na.rm=TRUE)/sqrt(sum(!is.na(x)))}

# create a table to store the values for the 3 prioritisations
cs_results <- data.frame(
	name=rep(rep(c('Amount-based prioritisation', 'Amount- & space-based prioritsation', 
		'Australian reserve network'),each=4),3),
	variable=rep(c('Amount', 'Geographic space', 'Environmental space'), each=12),
	species=colnames(amount.held(cs_rs_amount)),
	value=c(
		amount.held(cs_rs_amount)[1,], amount.held(cs_rs_space)[1,], amount.held(cs_rs_aus)[1,],
		space.held(cs_rs_amount, space=2)[1,], space.held(cs_rs_space, space=2)[1,], 
			space.held(cs_rs_aus, space=2)[1,],
		space.held(cs_rs_amount, space=1)[1,], space.held(cs_rs_space, space=1)[1,],
			space.held(cs_rs_aus, space=1)[1,]
	)
) %>% group_by(
	name,
	variable
) %>% summarise(
	mean=mean(value),
	se=se(value)
)

# plot the performance metrics
ggplot(aes(x=variable, y=mean, fill=name), data=cs_results) +
	geom_bar(position=position_dodge(0.9), stat='identity') +
	geom_errorbar(aes(ymin=mean-se, ymax=mean+se), position=position_dodge(0.9), width=0.2) +
	xlab('Property of species') +
	ylab('Proportion held in selected planning units (%)') +
	scale_fill_discrete(
		name=''
	) +
	theme_classic() +
	theme(legend.position='bottom',legend.direction='horizontal')

## ---- eval=FALSE, fig.height=5.5, fig.width=5.5--------------------------
#  # update prioritisation
#  cs_rs_space_blm <- update(cs_rs_space, BLM=100000)
#  
#  # show summary of prioritisation
#  summary(cs_rs_space_blm)
#  
#  # plot prioritisation
#  plot(cs_rs_space_blm, 1)

## ---- eval=FALSE, fig.height=5.5, fig.width=7.5--------------------------
#  # load ggplot2 package
#  library(ggplot2)
#  
#  # plot showing BLM and cost
#  ggplot(aes(x=BLM, y=cost), data=blm_results) +
#  	geom_point() +
#  	xlab('Boundary length modifier (BLM)') +
#  	ylab('Cost (number of planning units)') +
#  	theme_classic()
#  
#  # plot showing BLM and fragmentation
#  ggplot(aes(x=BLM, y=fragmentation), data=blm_results) +
#  	geom_point() +
#  	xlab('Boundary length modifier (BLM)') +
#  	ylab('Fragmentation (ratio of edge lengths inside reserves to exposed lengths)') +
#  	theme_classic()

## ---- eval=FALSE, fig.height=5.5, fig.width=5.5--------------------------
#  # plot prioritisation with blm=300
#  plot(solutions[[3]], 1)

