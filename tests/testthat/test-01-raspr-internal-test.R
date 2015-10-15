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
	pdf2<-structure(list(PID = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 
2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 
3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 
3L, 3L, 3L, 3L, 3L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 
4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 5L, 5L, 5L, 5L, 5L, 5L, 
5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 6L, 
6L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 
6L, 6L, 6L, 6L, 7L, 7L, 7L, 7L, 7L, 7L, 7L, 7L, 7L, 7L, 7L, 7L, 
7L, 7L, 7L, 7L, 7L, 7L, 7L, 7L, 7L, 8L, 8L, 8L, 8L, 8L, 8L, 8L, 
8L, 8L, 8L, 8L, 8L, 8L, 8L, 8L, 8L, 8L, 8L, 8L, 8L, 8L, 9L, 9L, 
9L, 9L, 9L, 9L, 9L, 9L, 9L, 9L, 9L, 9L, 9L, 9L, 9L, 9L, 9L, 9L, 
9L, 9L, 9L), SID = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
1L, 1L, 1L), POS = c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 
11L, 12L, 13L, 14L, 15L, 16L, 17L, 18L, 19L, 20L, 21L, 1L, 2L, 
3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L, 15L, 16L, 
17L, 18L, 19L, 20L, 21L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 
10L, 11L, 12L, 13L, 14L, 15L, 16L, 17L, 18L, 19L, 20L, 21L, 1L, 
2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L, 15L, 
16L, 17L, 18L, 19L, 20L, 21L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 
9L, 10L, 11L, 12L, 13L, 14L, 15L, 16L, 17L, 18L, 19L, 20L, 21L, 
1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L, 
15L, 16L, 17L, 18L, 19L, 20L, 21L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 
8L, 9L, 10L, 11L, 12L, 13L, 14L, 15L, 16L, 17L, 18L, 19L, 20L, 
21L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 
14L, 15L, 16L, 17L, 18L, 19L, 20L, 21L, 1L, 2L, 3L, 4L, 5L, 6L, 
7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L, 15L, 16L, 17L, 18L, 19L, 
20L, 21L), X = c(0.33333333, 0.33333333, 0.26666667, 0.2, 0.13333333, 
0.06666667, 0, 0, 0, 0, 0, 0, 0.06666667, 0.13333333, 0.2, 0.26666667, 
0.33333333, 0.33333333, 0.33333333, 0.33333333, 0.33333333, 0.33333333, 
0.33333333, 0.26666667, 0.2, 0.13333333, 0.06666667, 0, 0, 0, 
0, 0, 0, 0.06666667, 0.13333333, 0.2, 0.26666667, 0.33333333, 
0.33333333, 0.33333333, 0.33333333, 0.33333333, 0.33333333, 0.33333333, 
0.26666667, 0.2, 0.13333333, 0.06666667, 0, 0, 0, 0, 0, 0, 0.06666667, 
0.13333333, 0.2, 0.26666667, 0.33333333, 0.33333333, 0.33333333, 
0.33333333, 0.33333333, 0.66666667, 0.66666667, 0.6, 0.53333333, 
0.46666667, 0.4, 0.33333333, 0.33333333, 0.33333333, 0.33333333, 
0.33333333, 0.33333333, 0.4, 0.46666667, 0.53333333, 0.6, 0.66666667, 
0.66666667, 0.66666667, 0.66666667, 0.66666667, 0.66666667, 0.66666667, 
0.6, 0.53333333, 0.46666667, 0.4, 0.33333333, 0.33333333, 0.33333333, 
0.33333333, 0.33333333, 0.33333333, 0.4, 0.46666667, 0.53333333, 
0.6, 0.66666667, 0.66666667, 0.66666667, 0.66666667, 0.66666667, 
0.66666667, 0.66666667, 0.6, 0.53333333, 0.46666667, 0.4, 0.33333333, 
0.33333333, 0.33333333, 0.33333333, 0.33333333, 0.33333333, 0.4, 
0.46666667, 0.53333333, 0.6, 0.66666667, 0.66666667, 0.66666667, 
0.66666667, 0.66666667, 1, 1, 0.93333333, 0.86666667, 0.8, 0.73333333, 
0.66666667, 0.66666667, 0.66666667, 0.66666667, 0.66666667, 0.66666667, 
0.73333333, 0.8, 0.86666667, 0.93333333, 1, 1, 1, 1, 1, 1, 1, 
0.93333333, 0.86666667, 0.8, 0.73333333, 0.66666667, 0.66666667, 
0.66666667, 0.66666667, 0.66666667, 0.66666667, 0.73333333, 0.8, 
0.86666667, 0.93333333, 1, 1, 1, 1, 1, 1, 1, 0.93333333, 0.86666667, 
0.8, 0.73333333, 0.66666667, 0.66666667, 0.66666667, 0.66666667, 
0.66666667, 0.66666667, 0.73333333, 0.8, 0.86666667, 0.93333333, 
1, 1, 1, 1, 1), Y = c(0.73333333, 0.66666667, 0.66666667, 0.66666667, 
0.66666667, 0.66666667, 0.66666667, 0.73333333, 0.8, 0.86666667, 
0.93333333, 1, 1, 1, 1, 1, 1, 0.93333333, 0.86666667, 0.8, 0.73333333, 
0.4, 0.33333333, 0.33333333, 0.33333333, 0.33333333, 0.33333333, 
0.33333333, 0.4, 0.46666667, 0.53333333, 0.6, 0.66666667, 0.66666667, 
0.66666667, 0.66666667, 0.66666667, 0.66666667, 0.6, 0.53333333, 
0.46666667, 0.4, 0.06666667, 0, 0, 0, 0, 0, 0, 0.06666667, 0.13333333, 
0.2, 0.26666667, 0.33333333, 0.33333333, 0.33333333, 0.33333333, 
0.33333333, 0.33333333, 0.26666667, 0.2, 0.13333333, 0.06666667, 
0.73333333, 0.66666667, 0.66666667, 0.66666667, 0.66666667, 0.66666667, 
0.66666667, 0.73333333, 0.8, 0.86666667, 0.93333333, 1, 1, 1, 
1, 1, 1, 0.93333333, 0.86666667, 0.8, 0.73333333, 0.4, 0.33333333, 
0.33333333, 0.33333333, 0.33333333, 0.33333333, 0.33333333, 0.4, 
0.46666667, 0.53333333, 0.6, 0.66666667, 0.66666667, 0.66666667, 
0.66666667, 0.66666667, 0.66666667, 0.6, 0.53333333, 0.46666667, 
0.4, 0.06666667, 0, 0, 0, 0, 0, 0, 0.06666667, 0.13333333, 0.2, 
0.26666667, 0.33333333, 0.33333333, 0.33333333, 0.33333333, 0.33333333, 
0.33333333, 0.26666667, 0.2, 0.13333333, 0.06666667, 0.73333333, 
0.66666667, 0.66666667, 0.66666667, 0.66666667, 0.66666667, 0.66666667, 
0.73333333, 0.8, 0.86666667, 0.93333333, 1, 1, 1, 1, 1, 1, 0.93333333, 
0.86666667, 0.8, 0.73333333, 0.4, 0.33333333, 0.33333333, 0.33333333, 
0.33333333, 0.33333333, 0.33333333, 0.4, 0.46666667, 0.53333333, 
0.6, 0.66666667, 0.66666667, 0.66666667, 0.66666667, 0.66666667, 
0.66666667, 0.6, 0.53333333, 0.46666667, 0.4, 0.06666667, 0, 
0, 0, 0, 0, 0, 0.06666667, 0.13333333, 0.2, 0.26666667, 0.33333333, 
0.33333333, 0.33333333, 0.33333333, 0.33333333, 0.33333333, 0.26666667, 
0.2, 0.13333333, 0.06666667)), .Names = c("PID", "SID", "POS", 
"X", "Y"), row.names = c(NA, -189L), class = c("PolySet", "data.frame"
), projection = "1")

	expect_identical(pdf1[[1]], pdf2[[1]])
	expect_identical(pdf1[[2]], pdf2[[2]])
	expect_identical(pdf1[[3]], pdf2[[3]])
	expect_identical(pdf1[[4]], pdf2[[4]])
	expect_identical(pdf1[[5]], pdf2[[5]])
})

test_that("boundary length data functions", {
	# generate polygons
	polys<-rasterToPolygons(
		raster(
			matrix(1:9, ncol=3), 
			xmn=0, xmx=3, ymn=0, ymx=3, 
			crs=CRS('+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs ')
		),
		n=4
	)
	# make boundary length files
	bldf1<-calcBoundaryData(polys)
	bldf1[[3]]<-as.integer(bldf1[[3]]) # convert to integer for floating point comparisons
	bldf2<-structure(list(id1 = c(1L, 2L, 2L, 3L, 3L, 4L, 4L, 5L, 5L, 6L, 
		6L, 6L, 7L, 7L, 8L, 8L, 8L, 9L, 9L, 9L), id2 = c(1L, 1L, 2L, 
		2L, 3L, 1L, 4L, 2L, 4L, 3L, 5L, 6L, 4L, 7L, 5L, 7L, 8L, 6L, 8L, 
		9L), boundary = c(2, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 
		1, 1, 1, 1, 2)), .Names = c("id1", "id2", "boundary"), row.names = c(NA, 
		-20L), class = "data.frame")
	bldf2[[3]]<-as.integer(bldf2[[3]]) # convert to integer for floating point comparisons
	# tests
	expect_identical(bldf1[[1]], bldf2[[1]])
	expect_identical(bldf1[[2]], bldf2[[2]])
	expect_identical(bldf1[[3]], bldf2[[3]])
})
