test_that('RaspUnreliableOpts generator function', {
	# generate objects
	x=RaspUnreliableOpts()
	y=RaspUnreliableOpts(BLM=0)
	# check defaults are correct
	expect_equal(x@BLM, y@BLM)
})

test_that('update.RaspUnreliableOpts', {
	# generate objects
	x=RaspUnreliableOpts()
	y=update(x, BLM=30)
	# tests
	expect_equal(y@BLM, 30)
})

test_that('RaspReliableOpts generator function', {
	# generate objects
	x=RaspReliableOpts()
	y=RaspReliableOpts(BLM=0, MAXRLEVEL=5L, FAILUREMULTIPLIER=1.1)
	# check defaults are correct
	expect_equal(x@BLM, y@BLM)
	expect_equal(x@MAXRLEVEL, y@MAXRLEVEL)
	expect_equal(x@FAILUREMULTIPLIER, y@FAILUREMULTIPLIER)
})

test_that('update.RaspReliableOpts', {
	# generate objects
	x=RaspReliableOpts()
	y=update(x, BLM=50, MAXRLEVEL=10L, FAILUREMULTIPLIER=100)
	# tests
	expect_equal(y@BLM, 50)
	expect_equal(y@MAXRLEVEL, 10L)
	expect_equal(y@FAILUREMULTIPLIER, 100)
})
