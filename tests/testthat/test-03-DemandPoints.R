test_that('Error: DemandPoints generator function', {
	# create object
	dp<-DemandPoints(
		coords=matrix(runif(100), ncol=2),
		weights=rnorm(50),
		species=seq_len(50)
	)
	# check object
	expect_is(dp, 'DemandPoints')
	expect_equal(ncol(dp@coords), 2)
	expect_equal(nrow(dp@coords), 50)
	expect_equal(length(dp@weights), 50)
	expect_equal(length(dp@species), 50)
})

test_that('Error: DemandPoints merge function', {
	# create object
	dp1<-DemandPoints(
		coords=matrix(0.1, nrow=50, ncol=2),
		weights=1:50,
		species=rep(1L,50)
	)
	dp2<-DemandPoints(
		coords=matrix(0.1, nrow=50, ncol=2),
		weights=51:100,
		species=rep(2L,50)
	)
	# merge objects
	dp3<-merge.DemandPoints(list(dp1, dp2))
	# check object
	expect_is(dp3, 'DemandPoints')
	expect_equal(ncol(dp3@coords), 2)
	expect_equal(nrow(dp3@coords), 100)
	expect_identical(dp3@weights, 1:100)
	expect_identical(dp3@species, c(rep(1L,50), rep(2L,50)))
})



test_that('Error: demand.points (sm.density, geographic space)', {
	# load data
	data(pus, species, space)
	# generate points
	pts<-SpatialPoints(coords=randomPoints(species[[1]], n=100, prob=TRUE)	)
	# create demand points
	dp<-make.DemandPoints(
		species.points=pts,
		space.rasters=NULL,
		n=100L,
		kernel.method='sm.density'
	)
	# check objects
	expect_is(dp, 'DemandPoints')
	expect_equal(ncol(dp@coords), 2)
	expect_equal(nrow(dp@coords), 100)
	expect_equal(sum(is.na(dp@coords)), 0)
	expect_equal(length(dp@species), 100)
	expect_equal(sum(is.na((dp@species))), 0)
	expect_equal(length(dp@weights), 100)
	expect_equal(sum(is.na((dp@weights))), 0)
})

test_that('Error: demand.points (sm.density, attribute space)', {
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
	expect_equal(ncol(dp@coords), 2)
	expect_equal(nrow(dp@coords), 100)
	expect_equal(sum(is.na(dp@coords)), 0)
	expect_equal(length(dp@species), 100)
	expect_equal(sum(is.na((dp@species))), 0)
	expect_equal(length(dp@weights), 100)
	expect_equal(sum(is.na((dp@weights))), 0)
})

test_that('Error: demand.points (hpyervolume, geographic space)', {
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
	expect_equal(ncol(dp@coords), 2)
	expect_equal(nrow(dp@coords), 100)
	expect_equal(sum(is.na(dp@coords)), 0)
	expect_equal(length(dp@species), 100)
	expect_equal(sum(is.na((dp@species))), 0)
	expect_equal(length(dp@weights), 100)
	expect_equal(sum(is.na((dp@weights))), 0)
})

test_that('Error: demand.points (hypervolume, attribute space)', {
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
	expect_equal(ncol(dp@coords), 2)
	expect_equal(nrow(dp@coords), 100)
	expect_equal(sum(is.na(dp@coords)), 0)
	expect_equal(length(dp@species), 100)
	expect_equal(sum(is.na((dp@species))), 0)
	expect_equal(length(dp@weights), 100)
	expect_equal(sum(is.na((dp@weights))), 0)
})

