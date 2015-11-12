test_that('AttributeSpace generator function', {
	# load data
	data(cs_spp, cs_pus)
	x=AttributeSpace(
		pu=SimplePoints(rgeos::gCentroid(cs_pus, byid=TRUE)@coords),
		dp=list(make.DemandPoints(SpatialPoints(coords=randomPoints(cs_spp[[1]], n=100, prob=TRUE))@coords))
	)
	# checks are internal
})
