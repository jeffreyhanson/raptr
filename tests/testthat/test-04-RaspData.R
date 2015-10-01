test_that('RaspData', {
	# load data
	data(pus, species, space)
	# preliminary processing
	pu.points=list(
		rgeos::gCentroid(pus, byid=TRUE)@coords,
		extract(space,pus,fun='mean')
	)
	demand.points=list(
		make.DemandPoints(SpatialPoints(coords=dismo::randomPoints(species[[1]], n=100, prob=TRUE)), space),
		make.DemandPoints(SpatialPoints(coords=dismo::randomPoints(species[[1]], n=100, prob=TRUE)), NULL)
	)
	pu.species.probabilities=calcSpeciesAverageInPus(pus, species[[1]])
	polygons=SpatialPolygons2PolySet(pus)
	boundary=calcBoundaryData(pus)
	
	# create object
	x<-RaspData(
		pu=pus@data,
		pu.points=pu.points,
		species=data.frame(id=1L, area.target=0.2, space.target=0.2, name='test'),
		demand.points=demand.points,
		pu.species.probabilities=pu.species.probabilities,
		polygons=polygons,
		boundary=boundary
	)
	# tests are implicit in the validity method when creating the object	
})

test_that('make.RaspData', {
	# load data
	data(pus, species, space)
	# create object
	x<-make.RaspData(pus, species, space, include.geographic.space=TRUE)
	# tests are implicit in the validity method when creating the object
})

test_that('RaspData methods', {
	# load data
	data(pus, species, space)
	# create object
	x<-make.RaspData(pus, species, space, include.geographic.space=TRUE)
	# test methods
	x
	print(x)
	
})

