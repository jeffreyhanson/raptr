test_that('AttributeSpace generator function', {
	# load data
	data(cs_spp, cs_pus, cs_bio12)
	x=AttributeSpace(
		pu=SimplePoints(rgeos::gCentroid(cs_pus, byid=TRUE)@coords),
		dp=list(make.DemandPoints(SpatialPoints(coords=randomPoints(cs_spp, n=100, prob=TRUE)), NULL))
	)
	# checks are internal
})
