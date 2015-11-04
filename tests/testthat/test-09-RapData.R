test_that('RapData', {
	# load data
	data(cs_pus, cs_spp, cs_space)
	# preliminary processing
	attribute.spaces=list(
		AttributeSpace(
			pu=SimplePoints(rgeos::gCentroid(cs_pus[1:10,], byid=TRUE)@coords),
			dp=list(make.DemandPoints(SpatialPoints(coords=randomPoints(cs_spp, n=100, prob=TRUE))@coords))
		),
		AttributeSpace(
			pu=SimplePoints(extract(cs_space[[1]],cs_pus[1:10,],fun=mean)),
			dp=list(make.DemandPoints(extract(cs_space[[1]], SpatialPoints(coords=randomPoints(cs_spp, n=100, prob=TRUE)))))
		)
	)
	pu.species.probabilities=calcSpeciesAverageInPus(cs_pus[1:10,], cs_spp)
	polygons=SpatialPolygons2PolySet(cs_pus[1:10,])
	boundary=calcBoundaryData(cs_pus[1:10,])

	# create object
	x<-RapData(
		pu=cs_pus@data[1:10,],
		targets=data.frame(species=1L, target=c(0L,1L), proportion=0.2),
		species=data.frame(name='test'),
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

test_that('make.RapData (1 species)', {
	# load data
	data(cs_pus, cs_spp, cs_space)
	# create object
	x<-make.RapData(cs_pus[1:10,], cs_spp, cs_space, include.geographic.space=TRUE)
	# tests are implicit in the validity method when creating the object
})

test_that('make.RapData (multiple species)', {
	# create RapUnsolved object
	set.seed(500)
	pus<-sim.pus(225L)
	spp<-lapply(c('uniform', 'normal', 'bimodal'), sim.species, n=1, res=1, x=pus)
	rd<-make.RapData(pus,stack(spp), NULL, include.geographic.space=TRUE, n.demand.points=5L)
	# validity checks are internal
})

test_that('pu.subset.RapData', {
	# create RapUnsolved object
	set.seed(500)
	data(sim_ru)
	rd<-sim_ru@data
	rd2<-pu.subset(rd, 1:10)
	# tests
	expect_equal(nrow(rd2@pu), 10)
	expect_true(all(rd2@pu.species.probabilities$pu %in% 1:10))
	expect_true(all(rd2@boundary$id1 %in% 1:10))
	expect_true(all(rd2@boundary$id2 %in% 1:10))
	expect_true(all(rd2@polygons$PID %in% 1:10))
	expect_equal(nrow(rd2@attribute.spaces[[1]]@pu@coords), 10)
})

test_that('spp.subset.RapData', {
	# create RapUnsolved object
	data(sim_ru)
	rd<-sim_ru@data
	rd2<-spp.subset(rd, 1)
	rd3<-spp.subset(rd, 'uniform')
	# tests
	expect_equal(nrow(rd2@species), 1)
	expect_true(all(rd2@pu.species.probabilities$species==1L))
	expect_equal(length(rd2@attribute.spaces[[1]]@dp), 1)
	expect_true(all(rd2@targets$species==1L))
	expect_equal(nrow(rd2@targets), 2)
	
	expect_equal(nrow(rd3@species), 1)
	expect_true(all(rd3@pu.species.probabilities$species==1L))
	expect_equal(length(rd3@attribute.spaces[[1]]@dp), 1)
	expect_true(all(rd3@targets$species==1L))
	expect_equal(nrow(rd3@targets), 2)
	
})

test_that('dp.subset.RapData', {
	# create RapUnsolved object
	data(sim_ru)
	rd<-sim_ru@data
	rd2<-dp.subset(rd, 1, 1, 1:10)
	# tests
	expect_equal(rd@attribute.spaces[[1]]@dp[[1]]@points@coords[1:10,], rd2@attribute.spaces[[1]]@dp[[1]]@points@coords)
	expect_equal(rd@attribute.spaces[[1]]@dp[[1]]@weights[1:10], rd2@attribute.spaces[[1]]@dp[[1]]@weights)
})

test_that('update.RapData', {
	# generate objects
	data(sim_ru)
	x=sim_ru@data
	y=update(x, name=c('a', 'b', 'c'), amount.target=c(0.1,0.2,0.3), space.target=c(0.4,0.5,0.6))
	z=update(y, species=1, name='a1', amount.target=0.9, space.target=0.8)
	# y tests
	expect_equal(y@species$name, c('a', 'b', 'c'))
	expect_equal(y@targets$proportion[which(y@targets$target==0)], c(0.1,0.2,0.3))
	expect_equal(y@targets$proportion[which(y@targets$target==1)], c(0.4,0.5,0.6))
	# z tests
	expect_equal(z@species$name, c('a1', 'b', 'c'))
	expect_equal(z@targets$proportion[which(z@targets$target==0)], c(0.9,0.2,0.3))
	expect_equal(z@targets$proportion[which(z@targets$target==1)], c(0.8,0.5,0.6))
})

test_that('amount.target.RapData', {
	data(sim_ru)
	expect_equal(
		unname(amount.target(sim_ru@data)),
		rep(0.2, 3)
	)
	expect_equal(
		unname(amount.target(sim_ru@data, 1)),
		0.2
	)
})

test_that('amount.target<-.RapData', {
	data(sim_ru)
	sim_rd<-sim_ru@data
	amount.target(sim_rd)<-0.3
	expect_equal(unname(amount.target(sim_rd)), rep(0.3, 3))
	amount.target(sim_rd, 1)<-0.5
	expect_equal(unname(amount.target(sim_rd)), c(0.5, 0.3, 0.3))
})

test_that('space.target.RapData', {
	data(sim_ru)
	expect_equal(
		unname(space.target(sim_ru@data)[,1]),
		rep(0.2, 3)
	)
	expect_equal(
		unname(space.target(sim_ru@data, species=1)[,1]),
		rep(0.2)
	)
	expect_equal(
		unname(space.target(sim_ru@data, space=1)[,1]),
		rep(0.2, 3)
	)
	expect_equal(
		unname(space.target(sim_ru@data, species=1, space=1)[,1]),
		0.2
	)
})

test_that('space.target<-.RapData', {
	data(sim_ru)
	sim_rd<-sim_ru@data
	space.target(sim_rd)<-0.3
	expect_equal(unname(space.target(sim_rd)[,1]), rep(0.3, 3))
	space.target(sim_rd, 1)<-0.5
	expect_equal(unname(space.target(sim_rd)[,1]), c(0.5, 0.3, 0.3))
})

test_that('spp.plot.RapData', {
	data(sim_ru)
	spp.plot(sim_ru@data, 1)
	spp.plot(sim_ru@data, 'uniform')
})

test_that('space.plot.RapData', {
	data(sim_ru)
	space.plot(sim_ru@data, 1, 1)
	space.plot(sim_ru@data, 'normal', 1)
})

