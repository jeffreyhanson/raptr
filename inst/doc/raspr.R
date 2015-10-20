## ---- eval=FALSE---------------------------------------------------------
#  # make planning units
#  sim_pus <- sim.pus(225L)
#  
#  # simulate species distributions
#  sim_spp <- lapply(
#  	c('constant', 'normal', 'bimodal'),
#  	sim.species,
#    n=1,
#    x=sim_pus,
#    res=1
#  )

## ---- eval=FALSE---------------------------------------------------------
#  # change the plot parameters, so we can plot the distributions side by side
#  par(mfrow=c(1,3))
#  
#  # constant species
#  plot(sim_spp[[1]], main='constant species')
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
#  ## create RaspOpts object
#  # this stores parameters for the problem (eg. BLM)
#  sim_ro <- RaspOpts()
#  
#  ## create GurobiOpts object
#  # this stores parameters for solving the problem (eg. MIPGap)
#  sim_go <- GurobiOpts()
#  
#  ## create RaspData object
#  # create data.frame with species targets
#  # area targets at 20% and space targets at 0%
#  spp_targets <- data.frame(
#  	area.target=rep(0.2, 3),
#  	space.target=rep(0, 3),
#  	name=c('constant', 'normal', 'bimodal')
#  )
#  
#  # calculate probability of each species in each pu
#  pu_probabilities <- calcSpeciesAverageInPus(sim_pus, stack(sim_spp))
#  
#  ## create AttributeSpace object
#  # this stores the coordinates of the planning units in an attribute space
#  # and the coordinates and weights of demand points in the space
#  attr_space <- AttributeSpace(
#  	SimplePoints(pu_coords@coords),
#  	sim_dps
#  )
#  
#  # generate boundary data information
#  boundary <- calcBoundaryData(sim_pus)
#  
#  ## create RaspData object
#  # this store all the input data for the prioritisation
#  sim_rd <- RaspData(
#  	sim_pus@data,
#  	spp_targets,
#  	pu_probabilities,
#  	list(attr_space),
#    boundary,
#    SpatialPolygons2PolySet(sim_pus)
#  )
#  
#  ## create RaspUnsolved object
#  # this store all the input data and parameters needed to generate prioritisations
#  sim_ru_area <- RaspUnsolved(sim_ro, sim_go, sim_rd)
#  

## ---- eval=FALSE---------------------------------------------------------
#  # make prioritisations for each species using area targets
#  sp1_area <- solve(spp.subset(sim_ru_area, 'constant'))
#  sp2_area <- solve(spp.subset(sim_ru_area, 'normal'))
#  sp3_area <- solve(spp.subset(sim_ru_area, 'bimodal'))
#  
#  # plot prioritisations
#  par(mfrow=c(1,3))
#  plot(sp1_area)
#  plot(sp2_area)
#  plot(sp3_area)
#  par(mfrow=c(1,1))

## ---- eval=FALSE---------------------------------------------------------
#  # set 20% space targets in sim_ru
#  sim_ru_space<-update(sim_ru_area, space.targets=0.2)
#  
#  # make prioritisations for each species
#  sp1_space <- solve(spp.subset(sim_ru_space, 'constant'))
#  sp2_space <- solve(spp.subset(sim_ru_space, 'normal'))
#  sp3_space <- solve(spp.subset(sim_ru_space, 'bimodal'))
#  
#  # plot prioritisations
#  par(mfrow=c(1,3))
#  plot(sp1_space)
#  plot(sp2_space)
#  plot(sp3_space)
#  par(mfrow=c(1,1)))

## ---- eval=FALSE---------------------------------------------------------
#  # make prioritisations
#  sim_mru_area<-solve(sim_ru_area)
#  sim_mru_space<-solve(sim_ru_space)
#  
#  # plot prioritisations
#  plot(sim_mru_area)
#  plot(sim_mru_space)

