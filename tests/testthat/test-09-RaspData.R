test_that('RaspData', {
	# load data
	data(pus, species, space)
	# preliminary processing
	attribute.spaces=list(
		AttributeSpace(
			pu=SimplePoints(rgeos::gCentroid(pus, byid=TRUE)@coords),
			dp=list(make.DemandPoints(SpatialPoints(coords=dismo::randomPoints(species[[1]], n=100, prob=TRUE)), NULL))
		),
		AttributeSpace(
			pu=SimplePoints(extract(space,pus,fun='mean')),
			dp=list(make.DemandPoints(SpatialPoints(coords=dismo::randomPoints(species[[1]], n=100, prob=TRUE)), space))
		)
	)
	pu.species.probabilities=calcSpeciesAverageInPus(pus, species[[1]])
	polygons=SpatialPolygons2PolySet(pus)
	boundary=calcBoundaryData(pus)
	
	# create object
	x<-RaspData(
		pu=pus@data,
		species=data.frame(area.target=0.2, space.target=0.2, name='test'),
		pu.species.probabilities=pu.species.probabilities,
		attribute.spaces=attribute.spaces,
		polygons=polygons,
		boundary=boundary
	)
	# tests are implicit in the validity method when creating the object
	
	# test methods
	x
	print(x)

})

test_that('make.RaspData', {
	# load data
	data(pus, species, space)
	# create object
	x<-make.RaspData(pus, species, space, include.geographic.space=TRUE)
	# tests are implicit in the validity method when creating the object
	
})
