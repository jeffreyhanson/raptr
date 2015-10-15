
test_that('RaspUnsolved', {
	# create RaspUnsolved object
	set.seed(500)
	data(sim_pus, sim_spp)
	ro<-RaspOpts()
	rd<-make.RaspData(sim_pus, sim_spp, NULL, include.geographic.space=TRUE, n.demand.points=5L)
	# validity checks are internal
})

test_that('Gurobi model compiler', {
	# create RaspUnsolved object
	set.seed(500)
	data(sim_pus, sim_spp)
	ro<-RaspOpts()
	rd<-make.RaspData(sim_pus[1:10,], sim_spp, NULL, include.geographic.space=TRUE, n.demand.points=5L)
	# generate model code
	model<-rcpp_generate_model_object(ro, rd)
})

test_that('Gurobi solver', {
	# skip if gurobi not installed
	if (!is.GurobiInstalled(FALSE)) skip('Gurobi not installed on system')
	# create RaspUnsolved object
	set.seed(500)
	data(sim_pus, sim_spp)
	go<-GurobiOpts(MIPGap=0.9)
	ro<-RaspOpts()
	rd<-make.RaspData(sim_pus[1:10,], sim_spp, NULL, include.geographic.space=TRUE, n.demand.points=5L)
	# generate model code
	model<-rcpp_generate_model_object(ro, rd)
	model$A<-sparseMatrix(i=model$Ar[[1]]+1, j=model$Ar[[2]]+1, x=model$Ar[[3]])
	# solve the model
	result<-gurobi::gurobi(model, append(as.list(go), list('LogFile'=tempfile(fileext='.log'))))
})

test_that('solve.RaspUnsolved (NUMREPS=1)', {
	# skip if gurobi not installed
	if (!is.GurobiInstalled(FALSE)) skip('Gurobi not installed on system')
	# create RaspUnsolved object
	set.seed(500)
	data(sim_pus, sim_spp)
	go<-GurobiOpts(MIPGap=0.9)
	ro<-RaspOpts(NUMREPS=1L, FAILUREMULTIPLIER=1.1)
	rd<-make.RaspData(sim_pus[1:10,], sim_spp, NULL, include.geographic.space=TRUE,n.demand.points=5L)
	# solve object
	ru<-RaspUnsolved(ro, go, rd)
	# solve it
	rs<-raspr::solve(ru)
})

test_that('solve.RaspUnsolved (NUMREPS=2)', {
	# skip if gurobi not installed
	if (!is.GurobiInstalled(FALSE)) skip('Gurobi not installed on system')
	# create RaspUnsolved object
	set.seed(500)
	data(sim_pus, sim_spp)
	go<-GurobiOpts(MIPGap=0.9)
	ro<-RaspOpts(NUMREPS=2L)
	rd<-make.RaspData(sim_pus[1:10,], sim_spp, NULL, include.geographic.space=TRUE, n.demand.points=5L)
	# solve object
	ru<-RaspUnsolved(ro, go, rd)
	# solve it
	rs<-raspr::solve(ru)
})


test_that('solve.RaspUnsolved (number of requested solutions > number feasible solutions)', {
	# skip if gurobi not installed
	if (!is.GurobiInstalled(FALSE)) skip('Gurobi not installed on system')
	# create RaspUnsolved object
	set.seed(500)
	data(sim_pus, sim_spp)
	go<-GurobiOpts(MIPGap=0.9)
	ro<-RaspOpts(NUMREPS=10L)
	rd<-make.RaspData(sim_pus[1:10,], sim_spp, area.target=0.99999, NULL, include.geographic.space=TRUE,n.demand.points=5L)
	# solve object
	ru<-RaspUnsolved(ro, go, rd)
	# solve it
	rs<-raspr::solve(ru)
})


test_that('solve.RaspUnsolved (STATUS test)', {
	# skip if gurobi not installed
	if (!is.GurobiInstalled(FALSE)) skip('Gurobi not installed on system')
	# create RaspUnsolved object
	set.seed(500)
	data(sim_pus, sim_spp)
	go<-GurobiOpts(MIPGap=0.9)
	ro<-RaspOpts(NUMREPS=1L)
	rd<-make.RaspData(sim_pus[1:10,], sim_spp, NULL, include.geographic.space=TRUE,n.demand.points=10L)
	rd@pu$status[1]=0
	rd@pu$status[2]=2
	rd@pu$status[3]=3
	# solve object
	ru<-RaspUnsolved(ro, go, rd)
	# solve it
	rs<-raspr::solve(ru)
	# do checks
	expect_identical(selections(rs)[2], 1L)
	expect_identical(selections(rs)[3], 0L)
})

test_that('solve.RaspUnsolved (BLM test)', {
	# skip if gurobi not installed
	if (!is.GurobiInstalled(FALSE)) skip('Gurobi not installed on system')
	# create RaspUnsolved object
	set.seed(500)
	data(sim_pus, sim_spp)
	go<-GurobiOpts(MIPGap=0.9)
	ro<-RaspOpts(NUMREPS=1L, BLM=100)
	rd<-make.RaspData(sim_pus[1:10,], sim_spp, NULL, include.geographic.space=TRUE,n.demand.points=5L)
	# solve object
	ru<-RaspUnsolved(ro, go, rd)
	# solve it
	rs<-raspr::solve(ru)
})

