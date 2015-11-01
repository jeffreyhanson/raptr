### create RaspSolved object
### tests
test_that('plot (x=RaspSolved, y=missing)', {
	data(sim_rs)
	plot(sim_rs)
})

test_that('plot (x=RaspSolved, y=0)', {
	data(sim_rs)
	plot(sim_rs, 0)
})


test_that('plot (x=RaspSolved, y=1)', {
	data(sim_rs)
	plot(sim_rs, 1)
})

test_that('plot (x=RaspSolved, y=RaspSolved, i=NULL, j=NULL)', {
	data(sim_rs)
	sim_rs2<-sim_rs
	sim_rs2@results@selections[]<-sample(c(0,1), size=prod(dim(sim_rs@results@selections)), replace=TRUE)
	sim_rs2@results@.cache<-new.env()
	plot(sim_rs, sim_rs2)
})

test_that('plot (x=RaspSolved, y=RaspSolved, i=0, j=1)', {
	data(sim_rs)
	sim_rs2<-sim_rs
	sim_rs2@results@selections[]=sample(c(0,1), size=prod(dim(sim_rs@results@selections)), replace=TRUE)
	sim_rs2@results@.cache<-new.env()
	plot(sim_rs, sim_rs2, 0, 1)
})

test_that('spp.plot (x=RaspSolved, y=0)', {
	data(sim_rs)
	spp.plot(sim_rs, 1)
})

test_that('space.plot (x=RaspSolved, species=1, space=1, y=0)', {
	data(sim_rs)
	space.plot(sim_rs, 1, 1)
})

test_that('selections.RaspSolved', {
	data(sim_rs)
	expect_identical(selections(sim_rs), sim_rs@results@selections[sim_rs@results@best,])
	expect_identical(selections(sim_rs, NULL), sim_rs@results@selections)
	expect_identical(selections(sim_rs, 1), sim_rs@results@selections[1,])
})

test_that('score.RaspSolved', {
	data(sim_rs)
	expect_identical(score(sim_rs), sim_rs@results@summary$Score[sim_rs@results@best])
	expect_identical(score(sim_rs, NULL), sim_rs@results@summary$Score)
	expect_identical(score(sim_rs, 1), sim_rs@results@summary$Score[1])
})

test_that('amount.held.RaspSolved', {
	data(sim_rs)
	expect_identical(amount.held(sim_rs), sim_rs@results@amount.held[sim_rs@results@best,])
	expect_identical(amount.held(sim_rs, NULL), sim_rs@results@amount.held)
	expect_identical(amount.held(sim_rs, 1), sim_rs@results@amount.held[1,])
})

test_that('space.held.RaspSolved', {
	data(sim_rs)
	expect_identical(space.held(sim_rs), sim_rs@results@space.held[sim_rs@results@best,])
	expect_identical(space.held(sim_rs, NULL), sim_rs@results@space.held)
	expect_identical(space.held(sim_rs, 1), sim_rs@results@space.held[1,])
})

test_that('summary.RaspSolved', {
	data(sim_rs)
	expect_identical(summary(sim_rs), sim_rs@results@summary)
})

test_that('logging.file.RaspSolved', {
	data(sim_rs)
	expect_identical(logging.file(sim_rs), sim_rs@results@logging.file[sim_rs@results@best])
	expect_identical(logging.file(sim_rs, NULL), sim_rs@results@logging.file)
	expect_identical(logging.file(sim_rs, 1), sim_rs@results@logging.file[1])
})


test_that('update.RaspSolved', {
	# load RaspSolved object
	data(sim_rs)
	# update object
	sim_rs2 <- update(sim_rs, BLM=100, MIPGap=0.4, name=letters[1:3], solve=FALSE)
	# checks
	expect_equal(sim_rs2@opts@BLM, 100)
	expect_equal(sim_rs2@data@species$name, letters[1:3])
})

test_that('amount.target.RaspSolved', {
	data(sim_rs)
	expect_equal(unname(amount.target(sim_rs)), sim_rs@data@targets$proportion[which(sim_rs@data@targets$target==0)])
})

test_that('space.target.RaspSolved', {
	data(sim_rs)
	expect_equal(unname(space.target(sim_rs)[,1]), sim_rs@data@targets$proportion[which(sim_rs@data@targets$target==1)])
})
