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

## ------------------------------------------------------------------------
# create new object with just the uniform species
sim_ru_s1 <- spp.subset(sim_ru, 'uniform')

# update amount targets to 20% and space targets to 0%
sim_ru_s1 <- update(sim_ru_s1, amount.target=0.2, space.target=NA, solve=FALSE)

## ---- eval=use.Gurobi----------------------------------------------------
# solve problem to identify prioritisation
sim_rs_s1_amount <- solve(sim_ru_s1)

## ---- eval=!use.Gurobi, include=FALSE------------------------------------
#  sim_rs_s1_amount <- solve(sim_ru_s1, b=cache$sim_rs_s1_amount)

## ------------------------------------------------------------------------
## show summary
# note the format for this is similar to that used by Marxan
# see ?rapr::summary for details on this table
summary(sim_rs_s1_amount)

# show amount held
amount.held(sim_rs_s1_amount)

# show space held
space.held(sim_rs_s1_amount)

## ---- fig.height=5.5, fig.width=5.5, out.width='3.5in', out.height='3.5in', fig.align='center',fig.cap="A prioritisation for the uniformly distributed species generated using amount-based targets (20\\%). Sqaures represent planning units. Planning units with a green border are selected for prioritisation, and their colour denotes the probability they are inhabited by the species."----
# plot the prioritisation and the uniform species' distribution
spp.plot(sim_rs_s1_amount, 1, main='Uniform species')

## ------------------------------------------------------------------------
# create new object with just the normal species
sim_ru_s2 <- spp.subset(sim_ru, 'normal')

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

## ------------------------------------------------------------------------
# show summary
summary(sim_rs_s2_amount)

# show amount held
amount.held(sim_rs_s2_amount)

# show space held
space.held(sim_rs_s2_amount)

## ---- fig.height=5.5, fig.width=5.5, out.width='3.5in', out.height='3.5in', fig.align='center', fig.cap="A prioritisation for the normally distributed species generated using amount-based targets (20\\%). See Figure 3 caption for conventions."----
# plot the prioritisation and the normal species' distribution
spp.plot(sim_rs_s2_amount, 1, main='Normal species')

## ------------------------------------------------------------------------
# create new object with just the bimodal species
sim_ru_s3 <- spp.subset(sim_ru, 'bimodal')

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

## ---- fig.height=5.5, fig.width=5.5, out.width='3.5in', out.height='3.5in', fig.align='center', fig.cap="A prioritisation for the bimodally distributed species generated using amount-based targets (20\\%). See Figure 3 caption for conventions."----
# plot the prioritisation and the bimodal species' distribution
spp.plot(sim_rs_s3_amount, 1, main='Bimodal species')

## ------------------------------------------------------------------------
# show summary
summary(sim_rs_s3_amount)

# show amount held
amount.held(sim_rs_s3_amount)

# show space held
space.held(sim_rs_s3_amount)

## ---- eval=use.Gurobi----------------------------------------------------
# make new prioritisation
sim_rs_s1_space <- update(sim_rs_s1_amount, amount.target=0.2, space.target=0.5)

## ---- eval=!use.Gurobi, include=FALSE------------------------------------
#  # make new prioritisation
#  sim_rs_s1_space <- update(sim_rs_s1_amount, amount.target=0.2, space.target=0.5, solve=FALSE)
#  sim_rs_s1_space <- solve(
#  	sim_rs_s1_space,
#  	b=cache$sim_rs_s1_space
#  )

## ------------------------------------------------------------------------
# show summary
summary(sim_rs_s1_space)

# show amount held
amount.held(sim_rs_s1_space)

# show space held
space.held(sim_rs_s1_space)

## ---- fig.height=5.5, fig.width=5.5, out.width='3.5in', out.height='3.5in', fig.align='center', fig.cap="A prioritisation for the uniformly distributed species generated using amount-based targets (20\\%) and space-based targets (85\\%). See Figure 3 caption for conventions."----
# plot the prioritisation and the uniform species' distribution
spp.plot(sim_rs_s1_space, 'uniform', main='Uniform species')

## ---- fig.height=5.5, fig.width=5.5, out.width='3.5in', out.height='3.5in', fig.align='center', fig.cap="Difference between two prioritisations for the uniformly distributed species. Prioritisation $X$ was generated using just amount-based targets (20\\%), and prioritisation $Y$ was generated using an additional space-based target (85\\%)."----
# plot the difference between old and new prioritisations
plot(sim_rs_s1_amount, sim_rs_s1_space, 1, 1, main='Difference between solutions')

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

## ------------------------------------------------------------------------
# show summary
summary(sim_rs_s2_space)

# show amount held
amount.held(sim_rs_s2_space)

# show space held
space.held(sim_rs_s2_space)

## ---- fig.height=5.5, fig.width=5.5, out.width='3.5in', out.height='3.5in', fig.align='center', fig.cap="A prioritisation for the normally distributed species generated using amount-based targets (20\\%) and space-based targets (85\\%). See Figure 3 caption for conventions."----

# plot the prioritisation and the normal species' distribution
spp.plot(sim_rs_s2_space, 'normal', main='Normal species')

## ---- fig.height=5.5, fig.width=5.5, out.width='3.5in', out.height='3.5in', fig.align='center', fig.cap="Difference between two prioritisations for the normally distributed species. See Figure 7 caption for conventions."----
# plot the difference between old and new prioritisations
plot(sim_rs_s2_amount, sim_rs_s2_space, 1, 1, main='Difference between solutions')

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

## ------------------------------------------------------------------------
# show summary
summary(sim_rs_s3_space)

# show amount held
amount.held(sim_rs_s3_space)

# show space held
space.held(sim_rs_s3_space)


## ---- fig.height=5.5, fig.width=5.5, out.width='3.5in', out.height='3.5in', fig.align='center', fig.cap="A prioritisation for the normally distributed species generated using amount-based targets (20\\%) and space-based targets (85\\%). See Figure 3 caption for conventions."----
# plot the prioritisation and the bimodal species' distribution
spp.plot(sim_rs_s3_space, 'bimodal', main='Bimodal species')

## ---- fig.height=5.5, fig.width=5.5, out.width='3.5in', out.height='3.5in', fig.align='center', fig.cap="Difference between two prioritisations for the bimodally distributed species. See Figure 7 caption for conventions."----
# plot the difference between old and new prioritisations
plot(sim_rs_s3_amount, sim_rs_s3_space, 1, 1, main='Difference between solutions')

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

## ---- fig.height=5.5, fig.width=5.5, out.width='3.5in', out.height='3.5in', fig.align='center', fig.cap="A multi-species prioritisation for the uniformly, normally, and bimodally distributed species generated using just amount-based targets (20\\%). Squares represent planning units. Dark green planning units are selected for preservation."----
# plot multi-species prioritisation with amount-based targets
plot(sim_mrs_amount, 1, main='Amount-based targets')

## ---- fig.height=5.5, fig.width=5.5, out.width='3.5in', out.height='3.5in', fig.align='center', fig.cap="A multi-species prioritisation for the uniformly, normally, and bimodally distributed species generated using amount-based targets (20\\%) and space-based targets (85\\%). See Figure 12 caption for conventions."----
# plot multi-species prioritisation with amount- and space-based targets
plot(sim_mrs_space, 1, main='Amount and space-based targets')

## ---- fig.height=5.5, fig.width=5.5, out.width='3.5in', out.height='3.5in', fig.align='center', fig.cap="Difference between two multi-species prioritisations. See Figure 7 caption for conventions."----
# difference between the two prioritisations
plot(sim_mrs_amount, sim_mrs_space, 1, 1, main='Difference between solutions')

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

## ------------------------------------------------------------------------
# show summary
summary(sim_mrs_space2)

## ---- fig.height=5.5, fig.width=5.5, out.width='3.5in', out.height='3.5in', fig.align='center', fig.cap="A multi-species prioritisation for the uniformly, normally, and bimodally distributed species generated using amount-based targets (20\\%) and space-based targets (85\\%). This priorititisation was generated to be robust against low occupancy probabilities, by preventing planning units with low probabilities from being used to represent demand points. See Figure 12 caption for conventions."----
# plot prioritisation
plot(sim_mrs_space2, 1)

## ---- fig.height=5.5, fig.width=5.5, out.width='3.5in', out.height='3.5in', fig.align='center', fig.cap="Difference between two multi-species prioritisations. See Figure 7 caption for conventions."----
# difference between prioritisations that use and do not use thresholds
plot(sim_mrs_space2, sim_mrs_space, 1, 1, main='Difference between solutions')

## ------------------------------------------------------------------------
# make new prioritisation using reliable formulation
sim_mrs_space3 <- try(update(sim_mrs_space, formulation='reliable', max.r.level=1L))

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

## ------------------------------------------------------------------------
# show summary
summary(sim_mrs_space3)

## ---- fig.height=5.5, fig.width=5.5, out.width='3.5in', out.height='3.5in', fig.align='center', fig.cap="A multi-species prioritisation for the uniformly, normally, and bimodally distributed species generated using amount-based targets (20\\%) and space-based targets (85\\%). This priorititisation was generated to be robust against low occupancy probabilities, by explicitly using the probability of occupancy data when deriving a solution. See Figure 12 caption for conventions."----
# plot prioritisation
plot(sim_mrs_space3, 1)

## ---- fig.height=5.5, fig.width=5.5, out.width='3.5in', out.height='3.5in', fig.align='center', fig.cap="Difference between two multi-species prioritisations. See Figure 7 caption for conventions."----
# difference between prioritisations based on unreliable and reliable formulation
plot(sim_mrs_space3, sim_mrs_space, 1, 1, main='Difference between solutions')

## ---- eval=use.Gurobi----------------------------------------------------
# update prioritisation
sim_mrs_amount_blm <- update(sim_mrs_amount, BLM=100)

## ---- eval=!use.Gurobi, include=FALSE------------------------------------
#  sim_mrs_amount_blm <- update(sim_mrs_amount, BLM=100, solve=FALSE)
#  sim_mrs_amount_blm <- solve(
#  	sim_mrs_amount_blm,
#  	b=cache$sim_mrs_amount_blm
#  )

## ------------------------------------------------------------------------
# show summary of prioritisation
summary(sim_mrs_amount_blm)

# show amount held for each prioritisation
amount.held(sim_mrs_amount_blm)

# show space held for each prioritisation
space.held(sim_mrs_amount_blm)


## ---- fig.height=5.5, fig.width=5.5, out.width='3.5in', out.height='3.5in', fig.align='center', fig.cap="A multi-species prioritisation for the uniformly, normally, and bimodally distributed species generated using only amount-based targets (20\\%). Additionally, this priorititisation was specified to have high connectivity, by using a high $BLM$ parameter. See Figure 12 caption for conventions."----

# plot prioritisation
plot(sim_mrs_amount_blm, 1)

## ---- fig.height=5.5, fig.width=5.5, out.width='3.5in', out.height='3.5in', fig.align='center', fig.cap="Difference between two multi-species prioritisations. See Figure 7 caption for conventions."----
# difference between the two prioritisations
plot(sim_mrs_amount_blm, sim_mrs_amount, 1, 1, main='Difference between solutions')

## ---- markup='hide'------------------------------------------------------
# set seed for simulations
set.seed(500)

## simulate planning units
sim_pus <- sim.pus(25L)

# simulate species
sim_gspp <- sim.species(sim_pus, model=RPgauss(), n=1, res=0.1)

# simulate space
sim_gspace <- sim.space(sim_pus, model=RMgauss(scale=3), d=3, res=0.1)

# increment simulated space values by 100 so there are no negative values
# so we can investigate all distance metrics
sim_gspace <- sim_gspace + 100


## ------------------------------------------------------------------------
# generate RapUnsolved object containing data to generate prioritisations
sim_ru_gp <- rap(
	sim_pus, sim_gspp, sim_gspace, 
	amount.target=0.2, space.target=0.85,
	n.demand.points=50L, kernel.method='hypervolume',
	include.geographic.space=FALSE, scale=FALSE, solve=FALSE
)

## ---- fig.height=5.5, fig.width=5.5, out.width='3.5in', out.height='3.5in', fig.align='center', fig.cap="Distribution map of a species simulated using Gaussian prorceses. See Figure 2 caption for conventions."----
# plot species distribution
plot(
	sim_gspp,
	main='Simulated species',
	col=colorRampPalette(c("#FFFFD9", "#EDF8B1", "#C7E9B4", "#7FCDBB",
		"#41B6C4", "#1D91C0", "#225EA8", "#253494", "#081D58"
	))(100)
)
lines(sim_pus)

## ---- fig.height=2.5, fig.width=7, out.width='7in', out.height='2.5in', fig.align='center', fig.cap="Distribution of spatial variables across the species' geographic range. These variables each represent a dimension of a three-dimensional attribute space."----
# plot distribution of each dimension in the attribute space across geographic space
plot(
	sim_gspace,
	main=c('Attribute space (d=1)', 'Attribute space (d=2)', 'Attribute space (d=3)'),
	addfun=function(){lines(sim_pus)},
	nc=3
)

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

## ---- fig.height=8, fig.width=8, markup='hide', warning=FALSE, message=FALSE, fig.cap="Prioritisations generated using different distance metrics. See Figure 12 for conventions."----
# set plotting window
par(mfrow=c(3,3), mar=c(0, 0, 4.1, 0))

## create plots showing the selected planning units (dark green)
for (i in seq_along(solutions)) {
	# plot i'th solution
	plot(
		sim_pus,
		main=dist.metrics[i],
		col=replace(
			rep('#ccece6',nrow(sim_pus@data)),
			which(selections(solutions[[i]])==1),
			'#00441b'
		),
		axes=FALSE
	)
}

# reset plotting window
par(mfrow=c(1,1), mar=c(5.1, 4.1, 4.1, 2.1))

## ---- fig.height=5, fig.width=5, fig.cap="Distribution map for four Australian bird species. Pixel colours denote probability of occupancy."----
# load data
data(cs_spp)

# plot species' distributions
plot(cs_spp, main=c(
	"Blue-winged kookaburra", "Brown-backed honeyeater", 
	"Brown falcon", "Pale-headed rosella"
))

## ---- fig.height=5, fig.width=5, out.width='3.5in', out.height='3.5in', fig.align='center', fig.cap="Planning units for the case-study examples. Yellow polygons represent planning units with more then 50\\% of their area already in existing reserves."----
# load data
data(cs_pus)

## plot planning units
# convert SpatialPolygons to PolySet for quick plotting
cs_pus2 <- SpatialPolygons2PolySet(cs_pus)

# create vector of colours for planning units
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
par(mar=c(5.1, 4.1, 4.1, 2.1))

## ---- fig.height=2.5, fig.width=7, fig.cap="Broad-scale environmental variation across Australia. The variable DC1 describes the transition from wet and cool to dry and hot conditions. The variable DC2 describes the transition from wet and hot to dry and cool conditions."----
# load data
data(cs_space)

# plot variables
plot(cs_space, main=c('DC1', 'DC2'))

## ------------------------------------------------------------------------
# make amount-based prioritisation,
# and ignore existing protected areas by discarding values in the 
# status (third) column of the attribute table
cs_rs_amount <- rap(
	cs_pus[,-2], cs_spp, cs_space,
  amount.target=0.2, space.target=NA, n.demand.points=50L,
  include.geographic.space=TRUE, formulation='unreliable',
  solve=FALSE
)

# threshold probabilities to 0.1 for space calculations
cs_rs_amount <- prob.subset(cs_rs_amount, species=1:4, threshold=rep(0.1,4))

## ---- eval=use.Gurobi----------------------------------------------------
# generate prioritisation
cs_rs_amount <- solve(cs_rs_amount)

## ---- eval=!use.Gurobi, include=FALSE------------------------------------
#  cs_rs_amount <- solve(
#  	cs_rs_amount,
#  	b=cache$cs_rs_amount
#  )

## ---- fig.height=5.5, fig.width=5.5, out.width='3.5in', out.height='3.5in', fig.align='center', fig.cap="Multi-species prioritisation generated for four bird species using amount-based targets (20\\%). See Figure 12 captions for conventions."----
# show summary
summary(cs_rs_amount)

# plot prioritisation
plot(cs_rs_amount, 1)

## ---- fig.height=6, fig.width=9, fig.cap="Distribution of amount-based prioritisation in the geographic attribute space. Points denote combinations of environmental conditions. Green and grey points represent planning unit selected for and not selected for prioritisation (respectively). Blue points denote demand points, and their size indicates their weighting."----
# plot prioritisation in geographic attribute space
p1 <- space.plot(cs_rs_amount, 1, 2, main='Blue-winged kookaburra')
p2 <- space.plot(cs_rs_amount, 2, 2, main='Brown-backed honeyeater')
p3 <- space.plot(cs_rs_amount, 3, 2, main='Brown falcon')
p4 <- space.plot(cs_rs_amount, 4, 2, main='Pale-headed rosella')
gridExtra::grid.arrange(p1, p2, p3, p4, ncol=2)

## ---- fig.height=6, fig.width=9, fig.cap="Distribution of amount-based prioritisation in the environmental attribute space. See Figure 28 caption for conventions."----
# plot prioritisation in environmental attribute space
p1 <- space.plot(cs_rs_amount, 1, 1, main='Blue-winged kookaburra')
p2 <- space.plot(cs_rs_amount, 2, 1, main='Brown-backed honeyeater')
p3 <- space.plot(cs_rs_amount, 3, 1, main='Brown falcon')
p4 <- space.plot(cs_rs_amount, 4, 1, main='Pale-headed rosella')
gridExtra::grid.arrange(p1, p2, p3, p4, ncol=2)

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

## ---- fig.height=5.5, fig.width=5.5, out.width='3.5in', out.height='3.5in', fig.align='center'----
# show summary
summary(cs_rs_space)

# plot prioritisation
plot(cs_rs_space,1)

## ---- fig.height=6, fig.width=9, fig.cap="Distribution of the amount- and space-based prioritisation in the geographic attribute space. See Figure 28 caption for conventions."----
# plot prioritisation in geographic attribute space
p1 <- space.plot(cs_rs_space, 1, 2, main='Blue-winged kookaburra')
p2 <- space.plot(cs_rs_space, 2, 2, main='Brown-backed honeyeater')
p3 <- space.plot(cs_rs_space, 3, 2, main='Brown falcon')
p4 <- space.plot(cs_rs_space, 4, 2, main='Pale-headed rosella')
gridExtra::grid.arrange(p1, p2, p3, p4, ncol=2)

## ---- fig.height=6, fig.width=9, fig.cap="Distribution of the amount- and space-based prioritisation in the environmental attribute space. See Figure 28 caption for conventions."----
# plot prioritisation in environmental attribute space
p1 <- space.plot(cs_rs_space, 1, 1, main='Blue-winged kookaburra')
p2 <- space.plot(cs_rs_space, 2, 1, main='Brown-backed honeyeater')
p3 <- space.plot(cs_rs_space, 3, 1, main='Brown falcon')
p4 <- space.plot(cs_rs_space, 4, 1, main='Pale-headed rosella')
gridExtra::grid.arrange(p1, p2, p3, p4, ncol=2)

## ------------------------------------------------------------------------
# generate vector with Australia's selections
aus_selections <- which(cs_pus$status>0)

# create new object with Australia's network
cs_rs_aus <- update(cs_rs_amount, b=aus_selections)

## ---- fig.height=3.5, fig.width=8, fig.cap="Prioritisations were generated using amount-based targets (20\\%), and with additional space-based targets (85\\%). These are compared to the Queensland reserve network. Data represent means and standard errors for the four species in each prioritisation."----
# define standard error function
se=function(x){sd(x,na.rm=TRUE)/sqrt(sum(!is.na(x)))}

# create a table to store the values for the 3 prioritisations
cs_results <- data.frame(
	name=rep(rep(c('Amount-based prioritisation', 
		'Amount+space-based prioritsation', 'Queensland reserve network'),
		each=4),3),
	variable=rep(c('Amount', 'Geographic space', 'Environmental space'), each=12),
	species=colnames(amount.held(cs_rs_amount)),
	value=c(
		amount.held(cs_rs_amount)[1,], amount.held(cs_rs_space)[1,],
			amount.held(cs_rs_aus)[1,],
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
	geom_errorbar(
		aes(ymin=mean-se, ymax=mean+se), position=position_dodge(0.9),
		width=0.2
	) +
	xlab('Property of species') +
	ylab('Proportion held in\nselected planning units (%)') +
	scale_fill_discrete(
		name=''
	) +
	theme_classic() +
	theme(legend.position='bottom',legend.direction='horizontal')

## ---- include=FALSE------------------------------------------------------
# save solutions cache
if (use.Gurobi) {
	# init
	all_objects <- data.frame(
		name=ls(),
		class=sapply(ls(), function(x){return(class(get(x))[[1]])})
	)
	object_names <- as.character(all_objects[which(all_objects$class=='RapSolved'),'name'])
	list_names <- as.character(all_objects[which(all_objects$class=='list'),1])
	list_names <- data.frame(
		name=list_names,
		class=sapply(
			list_names,
			function(x){return(class(get(x)[[1]])[[1]])})
	)
	list_names <- as.character(list_names[which(list_names$class=='RapSolved'),'name'])
	# store solutions for objects
	cache <- lapply(
		object_names,
		function(x) {
			which(as.logical(selections(get(x))))
		}
	)
	names(cache) <- object_names 
	# store solutions for list of objects
	cache2 <- lapply(
		list_names,
		function(x) {
			lapply(
				get(x),
				function(y) {
					return(which(as.logical(selections(y))))
				}
			)
		}
	)
	names(cache2) <- list_names
	# save cache
	saveRDS(
		append(cache, cache2),
		file='vignette_solutions.rds'
	)
	try(system('touch -m -a -t 201512180130.09 vignette_solutions.rds'))
}

