test_that('AttributeSpace generator function', {
	# load data
	data(pus, species, space)
	x=AttributeSpace(
		pu=SimplePoints(rgeos::gCentroid(pus, byid=TRUE)@coords),
		dp=list(make.DemandPoints(SpatialPoints(coords=dismo::randomPoints(species[[1]], n=100, prob=TRUE)), NULL))
	)
	# checks are internal
})
