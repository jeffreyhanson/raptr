test_that('RaspOpts generator function', {
	# generate objects
	x=RaspOpts()
	y=RaspOpts(BLM=0, MAXRLEVEL=5L, NUMREPS=1L)
	# check defaults are correct
	expect_equal(x@BLM, y@BLM)
	expect_equal(x@MAXRLEVEL, y@MAXRLEVEL)
	expect_equal(x@NUMREPS, y@NUMREPS)
})

