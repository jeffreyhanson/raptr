
test_that('RaspUnsolved', {
	# create RaspUnsolved object
	data(small.pus, small.species)
	ro<-RaspOpts()
	rd<-make.RaspData(pus, small.species, NULL, include.geographic.space=TRUE, n.demand.points=5L)
	# validity checks are internal
})

test_that('Gurobi model compiler', {
	# create RaspUnsolved object
	data(small.pus, small.species)
	ro<-RaspOpts()
	rd<-make.RaspData(pus, small.species, NULL, include.geographic.space=TRUE, n.demand.points=5L)
	# generate model code
	model<-rcpp_generate_modelfile(ro, rd)
})

test_that('Gurobi solver', {
	# skip if gurobi not installed
	if (!is.GurobiInstalled()) skip('Gurobi not installed on system')
	# create RaspUnsolved object
	pth=file.path(tempdir(), 'gmodel')
	cat('path = ', pth, '.lp\n')
	set.seed(500)
	data(small.pus, small.species)
	ro<-RaspOpts()
	rd<-make.RaspData(small.pus, small.species, NULL, include.geographic.space=TRUE, n.demand.points=5L)
	# generate model code
	model<-rcpp_generate_modelfile(ro, rd)
	# solve the model
	writeLines(model, paste0(pth, '.lp'))
	result<-call.Gurobi(GurobiOpts(MIPGap=0.9), paste0(pth, '.lp'), paste0(pth, '.log'), paste0(pth, '.sol'))
})

test_that('solve.RaspUnsolved (NUMREPS=1)', {
	# skip if gurobi not installed
	if (!is.GurobiInstalled()) skip('Gurobi not installed on system')
	# create RaspUnsolved object
	data(small.pus, small.species)
	go<-GurobiOpts(MIPGap=0.9)
	ro<-RaspOpts(NUMREPS=1L)
	rd<-make.RaspData(small.pus, small.species, NULL, include.geographic.space=TRUE,n.demand.points=5L)
	# solve object
	ru<-RaspUnsolved(ro, go, rd)
	# solve it
	rs<-solve(ru)
})

test_that('solve.RaspUnsolved (NUMREPS=2)', {
	# skip if gurobi not installed
	if (!is.GurobiInstalled()) skip('Gurobi not installed on system')
	# create RaspUnsolved object
	data(small.pus, small.species)
	go<-GurobiOpts(MIPGap=0.9)
	ro<-RaspOpts(NUMREPS=2L)
	rd<-make.RaspData(small.pus, small.species, NULL, include.geographic.space=TRUE,n.demand.points=5L)
	# solve object
	ru<-RaspUnsolved(ro, go, rd)
	# solve it
	rs<-solve(ru)
})


test_that('solve.RaspUnsolved (number of requested solutions > number feasible solutions)', {
	# skip if gurobi not installed
	if (!is.GurobiInstalled()) skip('Gurobi not installed on system')
	# create RaspUnsolved object
	data(small.pus, small.species)
	go<-GurobiOpts(MIPGap=0.9)
	ro<-RaspOpts(NUMREPS=2L)
	rd<-make.RaspData(small.pus, small.species, area.target=0.99999, NULL, include.geographic.space=TRUE,n.demand.points=5L)
	# solve object
	ru<-RaspUnsolved(ro, go, rd)
	# solve it
	rs<-solve(ru)
})


test_that('solve.RaspUnsolved (STATUS test)', {
	# skip if gurobi not installed
	if (!is.GurobiInstalled()) skip('Gurobi not installed on system')
	# create RaspUnsolved object
	data(small.pus, small.species)
	go<-GurobiOpts(MIPGap=0.9)
	ro<-RaspOpts(NUMREPS=1L)
	rd<-make.RaspData(small.pus, small.species, NULL, include.geographic.space=TRUE,n.demand.points=5L)
	rd@pu$status[1]=0
	rd@pu$status[2]=2
	rd@pu$status[3]=3
	# solve object
	ru<-RaspUnsolved(ro, go, rd)
	# solve it
	rs<-solve(ru)
	# do checks
	expect_identical(selections(rs)[2], 1L)
	expect_identical(selections(rs)[3], 0L)
})

test_that('solve.RaspUnsolved (BLM test)', {
	# skip if gurobi not installed
	if (!is.GurobiInstalled()) skip('Gurobi not installed on system')
	# create RaspUnsolved object
	data(small.pus, small.species)
	go<-GurobiOpts(MIPGap=0.9)
	ro<-RaspOpts(NUMREPS=1L, BLM=100)
	rd<-make.RaspData(small.pus, small.species, NULL, include.geographic.space=TRUE,n.demand.points=5L)
	# solve object
	ru<-RaspUnsolved(ro, go, rd)
	# solve it
	rs<-solve(ru)
})


