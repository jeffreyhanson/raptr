context('02-misc')

test_that('gdal installation checks', {
	is.gdalInstalled()
})

test_that('Gurobi installation checks', {
	is.GurobiInstalled(FALSE)
})

test_that("blank.raster function", {
	#	generate planning units
	rst<-blank.raster(sim.pus(n=225L),1)
	# check simulated dataset
	expect_true(inherits(rst, 'RasterLayer'))
	expect_equal(ncell(rst),225L)
	expect_equal(xmin(rst),-7.5)
	expect_equal(ymin(rst),-7.5)
	expect_equal(xmax(rst),7.5)
	expect_equal(xmax(rst),7.5)
	expect_equal(sum(is.na(rst[])), 0)
})

test_that("randomPoints function", {
	sim_spp <- sim.species(sim.pus(225L), model='normal', res=1)
	pts <- randomPoints(sim_spp, 10)
	expect_equal(ncol(pts), 2)
	expect_equal(nrow(pts), 10)
	expect_equal(sum(is.na(c(pts))), 0)
})
