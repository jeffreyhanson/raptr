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
	data(pus, species, space)
	# generate points
	pts<-SpatialPoints(coords=dismo::randomPoints(species[[1]], n=100, prob=TRUE)	)
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
	data(pus, species, space)
	# generate points
	pts<-SpatialPoints(coords=randomPoints(species[[1]], n=100, prob=TRUE)	)
	# create demand points
	dp<-make.DemandPoints(
		species.points=pts,
		space.rasters=space,
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

test_that('demand.points (hpyervolume, geographic space)', {
	# load data
	data(pus, species, space)
	# generate points
	pts<-SpatialPoints(coords=randomPoints(species[[1]], n=100, prob=TRUE)	)
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
	data(pus, species, space)
	# generate points
	pts<-SpatialPoints(coords=randomPoints(species[[1]], n=100, prob=TRUE)	)
	# create demand points
	dp<-make.DemandPoints(
		species.points=pts,
		space.rasters=space,
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

