## ---- eval=FALSE---------------------------------------------------------
#  # make planning units
#  sim_pus <- sim.pus(225L)
#  
#  # simulate species distributions
#  sim_spp <- lapply(
#  	c('uniform', 'normal', 'bimodal'),
#  	sim.species,
#    n=1,
#    x=sim_pus,
#    res=1
#  )

## ---- eval=FALSE---------------------------------------------------------
#  # change the plot parameters, so we can plot the distributions side by side
#  par(mfrow=c(1,3))
#  
#  # uniform species
#  plot(sim_spp[[1]], main='uniform species')
#  lines(sim_pus)
#  
#  # normal species
#  plot(sim_spp[[2]], main='normal species')
#  lines(sim_pus)
#  
#  # bimodal species
#  plot(sim_spp[[3]], main='bimodal species')
#  lines(sim_pus)
#  
#  # reset plot parameters
#  par(mfrow=c(1,1))

## ---- eval=FALSE---------------------------------------------------------
#  # generate coordinates for pus/demand points
#  pu_coords <- rgeos::gCentroid(sim_pus, byid=TRUE)
#  
#  # calculate weights
#  sim_dps <- lapply(
#  	sim_spp,
#  	function(x) {
#  		return(extract(x, pu_coords))
#  	}
#  )
#  
#  # create demand point objects
#  sim_dps <- lapply(
#  	sim_dps,
#  	function(x) {
#  		return(
#  			DemandPoints(
#  				SimplePoints(pu_coords@coords),
#  				c(x)
#  			)
#  		)
#  	}
#  )

## ---- eval=FALSE---------------------------------------------------------
#  ## create RaspUnreliableOpts object
#  # this stores parameters for the problem (eg. BLM)
#  sim_ro <- RaspUnreliableOpts()
#  
#  ## create GurobiOpts object
#  # this stores parameters for solving the problem (eg. MIPGap)
#  sim_go <- GurobiOpts()
#  
#  ## create RaspData object
#  # create data.frame with species info
#  species <- data.frame(
#    name=c('uniform', 'normal', 'bimodal')
#  )
#  
#  ## create data.frame with species and space targets
#  # amount targets at 20% (denoted with target=0)
#  # space targets at 20% (denoted with target=1)
#  targets <- expand.grid(
#    species=1:3,
#    target=0:1,
#    proportion=0.2
#  )
#  
#  # calculate probability of each species in each pu
#  pu_probabilities <- calcSpeciesAverageInPus(sim_pus, stack(sim_spp))
#  
#  ## create AttributeSpace object
#  # this stores the coordinates of the planning units in an attribute space
#  # and the coordinates and weights of demand points in the space
#  attr_space <- AttributeSpace(
#    SimplePoints(pu_coords@coords),
#    sim_dps
#  )
#  
#  # generate boundary data information
#  boundary <- calcBoundaryData(sim_pus)
#  
#  ## create RaspData object
#  # this store all the input data for the prioritisation
#  sim_rd <- RaspData(
#    sim_pus@data,
#    species,
#    targets,
#    pu_probabilities,
#    list(attr_space),
#    boundary,
#    SpatialPolygons2PolySet(sim_pus)
#  )
#  
#  ## create RaspUnsolved object
#  # this store all the input data and parameters needed to generate prioritisations
#  sim_ru <- RaspUnsolved(sim_ro, sim_go, sim_rd)

## ---- eval=FALSE---------------------------------------------------------
#  # create new object with just the uniform species
#  sim_ru_s1 <- spp.subset(sim_ru, 'uniform')
#  
#  # update amount targets to 20% and space targets to 0%
#  sim_ru_s1 <- update(sim_ru_s1, amount.target=0.2, space.target=0, solve=FALSE)
#  
#  # solve problem to identify prioritisation
#  sim_rs_s1_amount <- solve(sim_ru_s1)

## ---- eval=FALSE---------------------------------------------------------
#  # plot the prioritisation
#  plot(sim_rs_s1_amount)
#  
#  # plot the prioritisation and the uniform species' distribution
#  spp.plot(sim_rs_s1_amount)

## ---- eval=FALSE---------------------------------------------------------
#  # create new object with just the normal species
#  sim_ru_s2 <- spp.subset(sim_ru, 'normal')
#  
#  # update amount targets to 20% and space targets to 0% and solve it
#  sim_rs_s2_amount <- update(sim_ru_s2, amount.target=0.2, space.target=0, solve=TRUE)

## ---- eval=FALSE---------------------------------------------------------
#  # plot the prioritisation
#  plot(sim_rs_s2_amount)
#  
#  # plot the prioritisation and the normal species' distribution
#  spp.plot(sim_rs_s2_amount)

## ---- eval=FALSE---------------------------------------------------------
#  # create new object with just the bimodal species
#  sim_ru_s3 <- spp.subset(sim_ru, 'bimodal')
#  
#  # update amount targets to 20% and space targets to 0% and solve it
#  sim_rs_s3_amount <- update(sim_ru_s3, amount.target=0.2, space.target=0)
#  
#  # plot the prioritisation
#  plot(sim_rs_s3_amount)
#  
#  # plot the prioritisation and the bimodal species' distribution
#  spp.plot(sim_rs_s3_amount)

## ---- eval=FALSE---------------------------------------------------------
#  # make new prioritisation
#  sim_rs_s1_space <- update(sim_rs_s1_amount, space.target=0.2)

## ---- eval=FALSE---------------------------------------------------------
#  # plot the prioritisation
#  plot(sim_rs_s1_space)
#  
#  # plot the prioritisation and the uniform species' distribution
#  spp.plot(sim_rs_s1_space)
#  
#  # plot the difference between old and new prioritisations
#  plot(sim_rs_s1_amount, sim_rs_s1_space, 1, 1)

## ---- eval=FALSE---------------------------------------------------------
#  # make new prioritisation
#  sim_rs_s2_space <- update(sim_rs_s2_amount, space.target=0.2)
#  
#  # plot the prioritisation
#  plot(sim_rs_s2_space)
#  
#  # plot the prioritisation and the normal species' distribution
#  spp.plot(sim_rs_s2_space)
#  
#  # plot the difference between old and new prioritisations
#  plot(sim_rs_s2_amount, sim_rs_s2_space, 1, 1)

## ---- eval=FALSE---------------------------------------------------------
#  # make new prioritisation
#  sim_rs_s3_space <- update(sim_rs_s3_amount, space.target=0.2)
#  
#  # plot the prioritisation
#  plot(sim_rs_s3_space)
#  
#  # plot the prioritisation and the bimodal species' distribution
#  spp.plot(sim_rs_s3_space)
#  
#  # plot the difference between old and new prioritisations
#  plot(sim_rs_s3_amount, sim_rs_s3_space, 1, 1)

## ---- eval=FALSE---------------------------------------------------------
#  # make prioritisations
#  sim_mrs_area <- solve(sim_ru_area)
#  sim_mrs_space <- solve(sim_ru_space)
#  
#  # plot prioritisations
#  plot(sim_mrs_area)
#  plot(sim_mrs_space)

## ---- eval=FALSE---------------------------------------------------------
#  # load data
#  data(cs_spp)
#  
#  # plot species distribution
#  plot(cs_spp)

## ---- eval=FALSE---------------------------------------------------------
#  # load data
#  data(cs_pu)
#  
#  ## plot planning units
#  # denote units not inside a protected area with white
#  plot(cs_pu[which(cs_pu$status==0),], col='white')
#  
#  # denote units inside a protected area with green
#  plot(cs_pu[which(cs_pu$status==2),], col='white')

## ---- eval=FALSE---------------------------------------------------------
#  # set up plotting window
#  par(mfrow=c(1,2))
#  # plot first variable
#  plot(cs_space[[1]])
#  
#  # plot second variable
#  plot(cs_space[[2]])
#  
#  # reset window
#  par(mfrow=c(1,1))

## ---- eval=FALSE---------------------------------------------------------
#  # make area-based prioritisation
#  cs_rs_area <- rasp(cs_pus, cs_species, cs_spaces,
#    area.target=0.2, space.target=0.2, n.demand.points=100L,
#    include.geographic.space=TRUE, formulation='unreliable'
#  )
#  
#  # plot prioritisation
#  plot(cs_rs_area)
#  
#  # plot prioritisation in environmental space
#  space.plot(cs_rs_area)

