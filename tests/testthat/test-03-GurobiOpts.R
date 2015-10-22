test_that('GurobiOpts generator function', {
	# generate objects
	x=GurobiOpts()
	y=GurobiOpts(
		Threads=1L,
		MIPGap=0.05,
		Presolve=2L,
		TimeLimit=NA_integer_,
		NumberSolutions=1L
	)
	# check defaults are correct
	expect_equal(x@Threads, y@Threads)
	expect_equal(x@MIPGap, y@MIPGap)
	expect_equal(x@Presolve, y@Presolve)
	expect_equal(x@TimeLimit, y@TimeLimit)
	expect_equal(x@NumberSolutions, y@NumberSolutions)
})

test_that('as.list.GurobiOpts ', {
	# generate objects
	x=GurobiOpts()
	y=as.list(x)
	# tests
	expect_equal(x@Threads,y$Threads)
	expect_equal(x@MIPGap,y$MIPGap)
	expect_equal(x@Presolve,y$Presolve)
	expect_true(!'TimeLimit' %in% names(y))
	expect_true(!'NumberSolutions' %in% names(y))
})

test_that('update.GurobiOpts', {
	# generate objects
	x=GurobiOpts()
	y=update(x, Presolve=1L, MIPGap=0.4, TimeLimit=5L, NumberSolutions=5L)
	# tests
	expect_equal(y@Presolve, 1L)
	expect_equal(y@MIPGap, 0.4)
	expect_equal(y@TimeLimit, 5L)
	expect_equal(y@NumberSolutions, 5L)
})
