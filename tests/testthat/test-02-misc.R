test_that('gdal installation checks', {
	is.gdalInstalled()
})

test_that('Gurobi installation checks', {
	expect_true(is.GurobiInstalled())
})

test_that("blank.raster function", {
# 	generate planning units
	rst=blank.raster(simulate.pus(n=225L),1)
# 	check simulated dataset
	expect_true(inherits(rst, 'RasterLayer'))
	expect_equal(ncell(rst),225L)
	expect_equal(xmin(rst),-7.5)
	expect_equal(ymin(rst),-7.5)
	expect_equal(xmax(rst),7.5)
	expect_equal(xmax(rst),7.5)
	expect_equal(sum(is.na(rst[])), 0)
})

