test_that('SimplePoints generator function', {
	# create object
	sp<-SimplePoints(
		coords=matrix(runif(100), ncol=2)
	)
	# check object
	expect_is(sp, 'SimplePoints')
	expect_equal(ncol(sp@coords), 2)
	expect_equal(nrow(sp@coords), 50)
})


