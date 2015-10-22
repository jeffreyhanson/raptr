test_that('Model compiler (unreliable)', {
	# load RaspUnsolved object
	data(sim_ru)
	sim_ru <- pu.subset(sim_ru, 1:10)
	# generate model code
	model<-rcpp_generate_unreliable_model_object(RaspUnreliableOpts(), sim_ru@data, FALSE)
})

test_that('Model compiler (reliable)', {
	# load RaspUnsolved object
	data(sim_ru)
	sim_ru <- pu.subset(sim_ru, 1:10)
	# generate model code
	model<-rcpp_generate_reliable_model_object(RaspReliableOpts(), sim_ru@data, FALSE)
})

test_that('Gurobi solver (unreliable)', {
	# skip if gurobi not installed
	if (!is.GurobiInstalled(FALSE)) skip('Gurobi not installed on system')
	# load RaspUnsolved object
	data(sim_ru)
	sim_ru <- pu.subset(sim_ru, 1:10)
	# generate model matrix
	model<-rcpp_generate_unreliable_model_object(RaspUnreliableOpts(), sim_ru@data, FALSE)
	model$A<-Matrix::sparseMatrix(i=model$Ar[[1]]+1, j=model$Ar[[2]]+1, x=model$Ar[[3]])
	# solve the model
	result<-gurobi::gurobi(model, append(as.list(GurobiOpts(MIPGap=0.99, Presolve=2L)), list('LogFile'=tempfile(fileext='.log'))))
})

test_that('Gurobi solver (reliable)', {
	# skip if gurobi not installed
	if (!is.GurobiInstalled(FALSE)) skip('Gurobi not installed on system')
	# load RaspUnsolved object
	data(sim_ru)
	sim_ru <- pu.subset(sim_ru, 1:10)
	# generate model code
	model<-rcpp_generate_reliable_model_object(RaspReliableOpts(), sim_ru@data, FALSE)
	model$A<-Matrix::sparseMatrix(i=model$Ar[[1]]+1, j=model$Ar[[2]]+1, x=model$Ar[[3]])
	# solve the model
	result<-gurobi::gurobi(model, append(as.list(GurobiOpts(MIPGap=0.99, Presolve=2L)), list('LogFile'=tempfile(fileext='.log'))))
})

test_that('solve.RaspUnsolved (unreliable - NUMREPS=1)', {
	# skip if gurobi not installed
	if (!is.GurobiInstalled(FALSE)) skip('Gurobi not installed on system')
	# load RaspUnsolved object
	data(sim_ru)
	sim_ru <- pu.subset(sim_ru, 1:10)
	# make unsolved object
	ru<-RaspUnsolved(RaspUnreliableOpts(), GurobiOpts(MIPGap=0.99, Presolve=2L), sim_ru@data)
	# solve it
	rs<-raspr::solve(ru)
})

test_that('solve.RaspUnsolved (reliable - NUMREPS=1)', {
	# skip if gurobi not installed
	if (!is.GurobiInstalled(FALSE)) skip('Gurobi not installed on system')
	# load RaspUnsolved object
	data(sim_ru)
	sim_ru <- pu.subset(sim_ru, 1:10)
	# make unsolved object
	ru<-RaspUnsolved(RaspReliableOpts(), GurobiOpts(MIPGap=0.99, Presolve=2L), sim_ru@data)
	# solve it
	rs<-raspr::solve(ru)
})

test_that('solve.RaspUnsolved (unreliable - NumberSolutions=2)', {
	# skip if gurobi not installed
	if (!is.GurobiInstalled(FALSE)) skip('Gurobi not installed on system')
	# load RaspUnsolved object
	data(sim_ru)
	sim_ru <- pu.subset(sim_ru, 1:10)
	# make unsolved object
	ru<-RaspUnsolved(RaspUnreliableOpts(), GurobiOpts(MIPGap=0.99, Presolve=2L,NumberSolutions=2L), sim_ru@data)
	# solve it
	rs<-raspr::solve(ru)
})

test_that('solve.RaspUnsolved (reliable - NUMREPS=2)', {
	# skip if gurobi not installed
	if (!is.GurobiInstalled(FALSE)) skip('Gurobi not installed on system')
	# load RaspUnsolved object
	data(sim_ru)
	sim_ru <- pu.subset(sim_ru, 1:10)
	# make unsolved object
	ru<-RaspUnsolved(RaspReliableOpts(), GurobiOpts(MIPGap=0.99, Presolve=2L,NumberSolutions=2L), sim_ru@data)
	# solve it
	rs<-raspr::solve(ru)
})


test_that('solve.RaspUnsolved (unreliable - number of requested solutions > number feasible solutions)', {
	# skip if gurobi not installed
	if (!is.GurobiInstalled(FALSE)) skip('Gurobi not installed on system')
	# load RaspUnsolved object
	data(sim_ru)
	sim_ru <- pu.subset(sim_ru, 1:10)
	# make unsolved object
	ru<-RaspUnsolved(RaspUnreliableOpts(), GurobiOpts(MIPGap=0.99, Presolve=2L,NumberSolutions=10L), sim_ru@data)
	# solve it
	rs<-raspr::solve(ru)
})

test_that('solve.RaspUnsolved (reliable - number of requested solutions > number feasible solutions)', {
	# skip if gurobi not installed
	if (!is.GurobiInstalled(FALSE)) skip('Gurobi not installed on system')
	# load RaspUnsolved object
	data(sim_ru)
	sim_ru <- pu.subset(sim_ru, 1:10)
	# make unsolved object
	ru<-RaspUnsolved(RaspReliableOpts(), GurobiOpts(MIPGap=0.99, Presolve=2L,NumberSolutions=10L), sim_ru@data)
	# solve it
	rs<-raspr::solve(ru)
})


test_that('solve.RaspUnsolved (unreliable - STATUS test)', {
	# skip if gurobi not installed
	if (!is.GurobiInstalled(FALSE)) skip('Gurobi not installed on system')
	# load RaspUnsolved object
	data(sim_ru)
	sim_ru <- pu.subset(sim_ru, 1:10)
	# lock in and lock out planning units
	sim_ru@data@pu$status[1]=0
	sim_ru@data@pu$status[2]=2
	sim_ru@data@pu$status[3]=3
	# make unsolved object
	ru<-RaspUnsolved(RaspUnreliableOpts(), GurobiOpts(MIPGap=0.99, Presolve=2L), sim_ru@data)
	# solve it
	rs<-raspr::solve(ru)
	# check that planning units locked in and out are set accordingly
	expect_identical(selections(rs)[2], 1L)
	expect_identical(selections(rs)[3], 0L)
})

test_that('solve.RaspUnsolved (reliable - STATUS test)', {
	# skip if gurobi not installed
	if (!is.GurobiInstalled(FALSE)) skip('Gurobi not installed on system')
	# load RaspUnsolved object
	data(sim_ru)
	sim_ru <- pu.subset(sim_ru, 1:10)
	# lock in and lock out planning units
	sim_ru@data@pu$status[1]=0
	sim_ru@data@pu$status[2]=2
	sim_ru@data@pu$status[3]=3
	# make unsolved object
	ru<-RaspUnsolved(RaspReliableOpts(), GurobiOpts(MIPGap=0.99, Presolve=2L), sim_ru@data)
	# solve it
	rs<-raspr::solve(ru)
	# check that planning units locked in and out are set accordingly
	expect_identical(selections(rs)[2], 1L)
	expect_identical(selections(rs)[3], 0L)
})

test_that('solve.RaspUnsolved (unreliable - BLM test)', {
	# skip if gurobi not installed
	if (!is.GurobiInstalled(FALSE)) skip('Gurobi not installed on system')
	# load RaspUnsolved object
	data(sim_ru)
	sim_ru <- pu.subset(sim_ru, 1:10)
	# make unsolved object
	ru<-RaspUnsolved(RaspUnreliableOpts(BLM=100), GurobiOpts(MIPGap=0.99, Presolve=2L), sim_ru@data)
	# solve it
	rs<-raspr::solve(ru)
})

test_that('solve.RaspUnsolved (reliable - BLM test)', {
	# skip if gurobi not installed
	if (!is.GurobiInstalled(FALSE)) skip('Gurobi not installed on system')
	# load RaspUnsolved object
	data(sim_ru)
	sim_ru <- pu.subset(sim_ru, 1:10)
	# make unsolved object
	ru<-RaspUnsolved(RaspReliableOpts(BLM=100), GurobiOpts(MIPGap=0.99, Presolve=2L), sim_ru@data)
	# solve it
	rs<-raspr::solve(ru)
})

test_that('update.RaspUnsolved', {
	# load RaspUnsolved object
	data(sim_ru)
	# update object
	sim_ru2 <- update(sim_ru, BLM=100, MIPGap=0.4, name=letters[1:3], solve=FALSE)
	# checks
	expect_equal(sim_ru2@opts@BLM, 100)
	expect_equal(sim_ru2@gurobi@MIPGap, 0.4)
	expect_equal(sim_ru2@data@species$name, letters[1:3])
})

test_that('amount.target.RaspUnsolved', {
	data(sim_ru)
	expect_equal(amount.target(sim_ru), sim_ru@data@species$amount.target)
})

test_that('space.target.RaspUnsolved', {
	data(sim_ru)
	expect_equal(space.target(sim_ru), sim_ru@data@species$space.target)
})
