test_that('RaspData', {
	# load data
	data(cs_pus, cs_spp, cs_space)
	# preliminary processing
	attribute.spaces=list(
		AttributeSpace(
			pu=SimplePoints(rgeos::gCentroid(cs_pus[1:10,], byid=TRUE)@coords),
			dp=list(make.DemandPoints(SpatialPoints(coords=randomPoints(cs_spp, n=100, prob=TRUE)), NULL))
		),
		AttributeSpace(
			pu=SimplePoints(extract(cs_space[[1]],cs_pus[1:10,],fun=mean)),
			dp=list(make.DemandPoints(SpatialPoints(coords=randomPoints(cs_spp, n=100, prob=TRUE)), cs_space[[1]]))
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

test_that('make.RaspData (1 species)', {
	# load data
	data(cs_pus, cs_spp, cs_space)
	# create object
	x<-make.RaspData(cs_pus[1:10,], cs_spp, cs_space, include.geographic.space=TRUE)
	# tests are implicit in the validity method when creating the object
})

test_that('make.RaspData (multiple species)', {
	# create RaspUnsolved object
	set.seed(500)
	data(sim_pus, sim_spp)
	ro<-RaspUnreliableOpts()
	rd<-make.RaspData(sim_pus, sim_spp, NULL, include.geographic.space=TRUE, n.demand.points=5L)
	# validity checks are internal
})

test_that('pu.subset.RaspData', {
	# create RaspUnsolved object
	set.seed(500)
	data(sim_pus, sim_spp)
	ro<-RaspUnreliableOpts()
	rd<-make.RaspData(sim_pus, sim_spp, NULL, include.geographic.space=TRUE, n.demand.points=5L)
	rd2<-pu.subset(rd, 1:10)
	# tests
	expect_equal(nrow(rd2@pu), 10)
	expect_true(all(rd2@pu.species.probabilities$pu %in% 1:10))
	expect_true(all(rd2@boundary$id1 %in% 1:10))
	expect_true(all(rd2@boundary$id2 %in% 1:10))
	expect_true(all(rd2@polygons$PID %in% 1:10))
	expect_equal(nrow(rd2@attribute.spaces[[1]]@pu@coords), 10)
})

test_that('spp.subset.RaspData', {
	# create RaspUnsolved object
	set.seed(500)
	data(sim_pus, sim_spp)
	ro<-RaspUnreliableOpts()
	rd<-make.RaspData(sim_pus, sim_spp, NULL, include.geographic.space=TRUE, n.demand.points=5L)
	rd2<-spp.subset(rd, 1)
	# tests
	expect_equal(nrow(rd2@species), 1)
	expect_true(all(rd2@pu.species.probabilities$species==1L))
	expect_equal(length(rd2@attribute.spaces[[1]]@dp), 1)
})

test_that('update.RaspData', {
	# generate objects
	data(sim_ru)
	x=sim_ru@data
	y=update(x, name=c('a', 'b', 'c'), area.target=c(0.1,0.2,0.3), space.target=c(0.4,0.5,0.6))
	# tests
	expect_equal(y@species$name, c('a', 'b', 'c'))
	expect_equal(y@species$area.target, c(0.1,0.2,0.3))
	expect_equal(y@species$space.target, c(0.4,0.5,0.6))
})

test_that('amount.target.RaspData', {
	data(sim_rs)
	expect_equal(amount.target(sim_rs@data), sim_rs@data@species$amount.target)
})

test_that('space.target.RaspData', {
	data(sim_rs)
	expect_equal(space.target(sim_rs@data), sim_rs@data@species$space.target)
})
