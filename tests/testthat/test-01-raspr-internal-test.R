test_that('demand.points.density1d', {
	# make points
	pts<-matrix(rnorm(100), ncol=1)
	# make demand points
	dp<-raspr:::demand.points.density1d(pts, 1000)
	# check properties of demand points
	expect_true(ncol(dp$coords)==1)
	expect_true(nrow(dp$coords)==1000)
	expect_is(dp$coords, 'matrix')
	expect_is(dp$weights, 'numeric')
})

test_that('demand.points.density2d', {
	# make points
	pts<-matrix(rnorm(100), ncol=2)
	# make demand points
	dp<-raspr:::demand.points.density2d(pts, 1000)
	# check properties of demand points
	expect_true(ncol(dp$coords)==2)
	expect_true(nrow(dp$coords)==1000)
	expect_is(dp$coords, 'matrix')
	expect_is(dp$weights, 'numeric')
})

test_that('demand.points.hypervolume', {
	# make points
	pts<-matrix(rnorm(999), ncol=3)
	# make demand points
	dp<-raspr:::demand.points.hypervolume(pts, 1000)
	# check properties of demand points
	expect_true(ncol(dp$coords)==3)
	expect_true(nrow(dp$coords)==1000)
	expect_is(dp$coords, 'matrix')
	expect_is(dp$weights, 'numeric')

})

test_that("ZonalMean functions", {
	purast<-disaggregate(raster(matrix(1:9, ncol=3)),fact=100)
	species<-purast*abs(rnorm(ncell(purast)))
	z1<-zonal(species, purast, fun='mean')
	z2<-raspr:::zonalMean(purast, species[[1]], ncores=2)
	z3<-raspr:::zonalMean(purast, species[[1]])
	expect_equal(round(z1[,2],5), round(z2[[3]],5), round(z3[[3]],5))
})

test_that("calcSpeciesAverageInPus functions", {
	template<-disaggregate(raster(matrix(1:9, ncol=3), xmn=0, xmx=1, ymn=0, ymx=1, crs=CRS('+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs ')),fact=5)
	polys<-rasterToPolygons(template, n=4, dissolve=TRUE)
	species<-setValues(template, round(runif(ncell(template))))
	p1<-zonal(species, template, "mean")
	p2<-calcSpeciesAverageInPus(polys, species)
	p3<-calcSpeciesAverageInPus(polys, species, ncores=2)
	expect_equal(round(p1[,2],5),round(p2[[3]],5),round(p3[[3]],5))
})

test_that("PolySet conversion function", {
	template<-disaggregate(raster(matrix(1:9, ncol=3), xmn=0, xmx=1, ymn=0, ymx=1, crs=CRS('+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs ')),fact=5)
	polys<-rasterToPolygons(template, n=4, dissolve=TRUE)
	pdf1<-raspr:::rcpp_Polygons2PolySet(polys@polygons)
	pdf2<-maptools::SpatialPolygons2PolySet(polys)
	expect_identical(pdf1[[1]], pdf2[[1]])
	expect_identical(pdf1[[2]], pdf2[[2]])
	expect_identical(pdf1[[3]], pdf2[[3]])
	expect_identical(pdf1[[4]], pdf2[[4]])
	expect_identical(pdf1[[5]], pdf2[[5]])
})

test_that("boundary length data functions", {
	polys<-rasterToPolygons(
		raster(
			matrix(1:9, ncol=3), 
			xmn=0, xmx=1, ymn=0, ymx=1, 
			crs=CRS('+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs ')
		),
		n=4
	)
	bldf1<-calcBoundaryData(maptools::SpatialPolygons2PolySet(polys))
	bldf2<-calcBoundaryData(polys)
	expect_equal(bldf1, bldf2)
})
