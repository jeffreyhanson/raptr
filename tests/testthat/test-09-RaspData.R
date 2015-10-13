test_that('RaspData', {
	# load data
	data(cs_pus, cs_spp, cs_bio12)
	# preliminary processing
	attribute.spaces=list(
		AttributeSpace(
			pu=SimplePoints(rgeos::gCentroid(cs_pus[1:10,], byid=TRUE)@coords),
			dp=list(make.DemandPoints(SpatialPoints(coords=dismo::randomPoints(cs_spp, n=100, prob=TRUE)), NULL))
		),
		AttributeSpace(
			pu=SimplePoints(extract(cs_bio12,cs_pus[1:10,],fun=mean)),
			dp=list(make.DemandPoints(SpatialPoints(coords=dismo::randomPoints(cs_spp, n=100, prob=TRUE)), cs_bio12))
		)
	)
	pu.species.probabilities=calcSpeciesAverageInPus(cs_pus[1:10,], cs_spp)
	polygons=SpatialPolygons2PolySet(cs_pus[1:10,])
	boundary=calcBoundaryData(cs_pus[1:10,])
	
	# create object
	x<-RaspData(
		pu=cs_pus@data[1:10,],
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
	data(cs_pus, cs_spp, cs_bio12)
	# create object
	x<-make.RaspData(cs_pus[1:10,], cs_spp, cs_bio12, include.geographic.space=TRUE)
	# tests are implicit in the validity method when creating the object
	
})
