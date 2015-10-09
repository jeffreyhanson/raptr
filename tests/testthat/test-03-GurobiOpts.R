test_that('GurobiOpts generator function', {
	# generate objects
	x=GurobiOpts()
	y=GurobiOpts(
		Threads=1L,
		MIPGap=0.05,
		Presolve=2L,
		TimeLimit=NA_integer_
	)
	# check defaults are correct
	expect_equal(x@Threads, y@Threads)
	expect_equal(x@MIPGap, y@MIPGap)
	expect_equal(x@Presolve, y@Presolve)
	expect_equal(x@TimeLimit, y@TimeLimit)
})

 
