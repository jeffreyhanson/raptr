test_that('DemandPoints generator function', {
	# create object
	dp<-DemandPoints(
		points=SimplePoints(matrix(runif(100), ncol=2)),
		weights=rnorm(50)
	)
	# check object
	expect_is(dp, 'DemandPoints')
	expect_equal(ncol(dp@points@coords), 2)
	expect_equal(nrow(dp@points@coords), 50)
	expect_equal(length(dp@weights), 50)
})

test_that('demand.points (sm.density, geographic space)', {
	# load data
	data(sim_spp)
	# generate points
	pts<-SpatialPoints(coords=randomPoints(sim_spp[[1]], n=10, prob=TRUE))
	# create demand points
	dp<-make.DemandPoints(
		species.points=pts,
		space.rasters=NULL,
		n=100L,
		kernel.method='sm.density'
	)
	# check objects
	expect_is(dp, 'DemandPoints')
	expect_equal(ncol(dp@points@coords), 2)
	expect_equal(nrow(dp@points@coords), 100)
	expect_equal(sum(is.na(dp@points@coords)), 0)
	expect_equal(length(dp@weights), 100)
	expect_equal(sum(is.na((dp@weights))), 0)
})

test_that('demand.points (sm.density, attribute space)', {
	# load data
	data(cs_spp, cs_bio12)
	# generate points
	pts<-SpatialPoints(coords=randomPoints(cs_spp[[1]], n=10, prob=TRUE))
	# create demand points
	dp<-make.DemandPoints(
		species.points=pts,
		space.rasters=cs_bio12,
		n=100L,
		kernel.method='sm.density'
	)
	# check objects
	expect_is(dp, 'DemandPoints')
	expect_equal(ncol(dp@points@coords), 1)
	expect_equal(nrow(dp@points@coords), 100)
	expect_equal(sum(is.na(dp@points@coords)), 0)
	expect_equal(length(dp@weights), 100)
	expect_equal(sum(is.na((dp@weights))), 0)
})

test_that('demand.points (hpyervolume, geographic space)', {
	# load data
	data(cs_spp)
	# generate points
	pts<-SpatialPoints(coords=randomPoints(cs_spp[[1]], n=100, prob=TRUE))
	# create demand points
	dp<-make.DemandPoints(
		species.points=pts,
		space.rasters=NULL,
		n=100L,
		kernel.method='hypervolume'
	)
	# check objects
	expect_is(dp, 'DemandPoints')
	expect_equal(ncol(dp@points@coords), 2)
	expect_equal(nrow(dp@points@coords), 100)
	expect_equal(sum(is.na(dp@points@coords)), 0)
	expect_equal(length(dp@weights), 100)
	expect_equal(sum(is.na((dp@weights))), 0)
})

test_that('demand.points (hypervolume, attribute space)', {
	# load data
	data(cs_spp, cs_bio12)
	# generate points
	pts<-SpatialPoints(coords=randomPoints(cs_spp[[1]], n=100, prob=TRUE)	)
	# create demand points
	dp<-make.DemandPoints(
		species.points=pts,
		space.rasters=cs_bio12,
		n=100L,
		kernel.method='hypervolume'
	)
	# check objects
	expect_is(dp, 'DemandPoints')
	expect_equal(ncol(dp@points@coords), 1)
	expect_equal(nrow(dp@points@coords), 100)
	expect_equal(sum(is.na(dp@points@coords)), 0)
	expect_equal(length(dp@weights), 100)
	expect_equal(sum(is.na((dp@weights))), 0)
})

