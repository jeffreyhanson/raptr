test_that('RapUnreliableOpts generator function', {
	# generate objects
	x=RapUnreliableOpts()
	y=RapUnreliableOpts(BLM=0)
	# check defaults are correct
	expect_equal(x@BLM, y@BLM)
})

test_that('update.RapUnreliableOpts', {
	# generate objects
	x=RapUnreliableOpts()
	y=update(x, BLM=30)
	# tests
	expect_equal(y@BLM, 30)
})

test_that('RapReliableOpts generator function', {
	# generate objects
	x=RapReliableOpts()
	y=RapReliableOpts(BLM=0, MaxRLevel=5L, FailureMultiplier=1.1)
	# check defaults are correct
	expect_equal(x@BLM, y@BLM)
	expect_equal(x@MaxRLevel, y@MaxRLevel)
	expect_equal(x@FailureMultiplier, y@FailureMultiplier)
})

test_that('update.RapReliableOpts', {
	# generate objects
	x=RapReliableOpts()
	y=update(x, BLM=50, MaxRLevel=10L, FailureMultiplier=100)
	# tests
	expect_equal(y@BLM, 50)
	expect_equal(y@MaxRLevel, 10L)
	expect_equal(y@FailureMultiplier, 100)
})
