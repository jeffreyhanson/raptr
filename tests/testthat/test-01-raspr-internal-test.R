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

test_that('spacePlot.1d', {
	# make plot
	spacePlot.1d(
		pu=data.frame(
			X1=rnorm(100),
			status=c(
				rep('Not Selected',97),
				'Selected',
				'Locked In',
				'Locked Out'
			)
		),
		dp=data.frame(
			X1=runif(100, min=-4, max=4),
			weights=runif(100)
		),
		pu.color.palette='RdYlGn',
		locked.in.color='#000000FF',
		locked.out.color='#D7D7D7FF',
		main='test 1d'
	)
})

test_that('spacePlot.2d', {
	# make plot
	spacePlot.2d(
		pu=data.frame(
			X1=rnorm(100),
			X2=rnorm(100),
			status=c(
				rep('Not Selected',97),
				'Selected',
				'Locked In',
				'Locked Out'
			)
		),
		dp=data.frame(
			X1=runif(100, min=-4, max=4),
			X2=runif(100, min=-4, max=4),
			weights=runif(100)
		),
		pu.color.palette='RdYlGn',
		locked.in.color='#000000FF',
		locked.out.color='#D7D7D7FF',
		main='test 2d'
	)
})

test_that('spacePlot.3d', {
	# skip on cran and travis
	skip_on_cran()
	skip_on_travis()
	# make plot
	spacePlot.3d(
		pu=data.frame(
			X1=rnorm(100),
			X2=rnorm(100),
			X3=rnorm(100),
			status=c(
				rep('Not Selected',97),
				'Selected',
				'Locked In',
				'Locked Out'
			)
		),
		dp=data.frame(
			X1=runif(100, min=-4, max=4),
			X2=runif(100, min=-4, max=4),
			X3=runif(100, min=-4, max=4),
			weights=runif(100)
		),
		pu.color.palette='RdYlGn',
		locked.in.color='#000000FF',
		locked.out.color='#D7D7D7FF',
		main='test 3d'
	)
	# close rgl device
	rgl::rgl.close()
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
	bldf2<-structure(list(id1 = c(1L, 2L, 2L, 3L, 3L, 4L, 4L, 5L, 5L, 6L,
		6L, 6L, 7L, 7L, 8L, 8L, 8L, 9L, 9L, 9L), id2 = c(1L, 1L, 2L,
		2L, 3L, 1L, 4L, 2L, 4L, 3L, 5L, 6L, 4L, 7L, 5L, 7L, 8L, 6L, 8L,
		9L), boundary = c(2, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1,
		1, 1, 1, 1, 2)), .Names = c("id1", "id2", "boundary"), row.names = c(NA,
		-20L), class = "data.frame")
	bldf2[[2]]<-as.integer(bldf2[[2]]) # convert to integer for floating point comparisons

	# sort by ids
	bldf1$ids=apply(as.matrix(bldf1[,1:2,drop=FALSE]), 1, function(x) {
		return(paste(sort(x), collapse='_'))
	})
	bldf2$ids=apply(as.matrix(bldf2[,1:2,drop=FALSE]), 1, function(x) {
		return(paste(sort(x), collapse='_'))
	})
	bldf1=bldf1[order(bldf1$ids),]
	bldf2=bldf2[order(bldf2$ids),]

	# check that values are correct
	expect_equal(bldf1$ids, bldf2$ids)
	expect_equal(bldf1[[3]], bldf2[[3]])
})
