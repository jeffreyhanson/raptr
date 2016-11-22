context('11-RapUnsolved')
source('functions.R')

test_that('Gurobi solver (unreliable)', {
	# skip if gurobi not installed
	if (!is.GurobiInstalled(FALSE)) skip('Gurobi not installed on system')
	# load RapUnsolved object
	set.seed(500)
	data(sim_ru)
	sim_ru <- dp.subset(pu.subset(spp.subset(sim_ru, 1:2), 1:10), 1, 1:2, 1:10)
	# generate model matrix
	model<-rcpp_generate_model_object(RapUnreliableOpts(), TRUE, sim_ru@data, FALSE)
	model$A<-Matrix::sparseMatrix(i=model$Ar[[1]]+1, j=model$Ar[[2]]+1, x=model$Ar[[3]])
	# solve the model
	result<-gurobi::gurobi(model, append(as.list(GurobiOpts(MIPGap=0.99, Presolve=1L)), list('LogFile'=tempfile(fileext='.log'))))
	if (file.exists('gurobi.log')) unlink('gurobi.log')
	# check solution variables
	expect_true(all(result$x[grep('pu_',dump_object(model$cache$variables, 'character'),fixed=TRUE)] %in% c(0,1)))
	expect_true(all(result$x[grep('Y_',dump_object(model$cache$variables, 'character'),fixed=TRUE)] %in% c(0,1)))
})

test_that('Gurobi solver (reliable)', {
	# skip if gurobi not installed
	if (!is.GurobiInstalled(FALSE)) skip('Gurobi not installed on system')
	# load RapUnsolved object
	set.seed(500)
	data(sim_ru)
	sim_ru <- dp.subset(pu.subset(spp.subset(sim_ru, 1:2), 1:10), 1, 1:2, 1:10)
	sim_ru@data@targets[[3]] <- c(0.5, 0.5, -1000, -1000)
	# generate model code
	model<-rcpp_generate_model_object(RapReliableOpts(), FALSE, sim_ru@data, FALSE)
	model$A<-Matrix::sparseMatrix(i=model$Ar[[1]]+1, j=model$Ar[[2]]+1, x=model$Ar[[3]])
	# solve the model
	result<-gurobi::gurobi(model, append(as.list(GurobiOpts(MIPGap=0.99, Presolve=1L)), list('LogFile'=tempfile(fileext='.log'))))
	if (file.exists('gurobi.log')) unlink('gurobi.log')
	# checks
	expect_true(all(result$x[grep('pu_',dump_object(model$cache$variables, 'character'),fixed=TRUE)] %in% c(0,1)))
	expect_true(all(result$x[grep('Y_',dump_object(model$cache$variables, 'character'),fixed=TRUE)] %in% c(0,1)))
	expect_true(all(round(result$x[grep('P_',dump_object(model$cache$variables, 'character'),fixed=TRUE)],5) >= 0))
	expect_true(all(round(result$x[grep('P_',dump_object(model$cache$variables, 'character'),fixed=TRUE)],5) <= 1))
	expect_true(all(
		round(result$x[grep('W_',dump_object(model$cache$variables, 'character'),fixed=TRUE)],5) ==  round(
			result$x[grep('Y_',dump_object(model$cache$variables, 'character'),fixed=TRUE)] * result$x[grep('P_',dump_object(model$cache$variables, 'character'),fixed=TRUE)],
			5
		)
	))
})

test_that('maximum.targets (unreliable)', {
	data(sim_ru)
	sim_ru@data@targets$name<-paste0(sim_ru@data@species$name, ' (space', 1:3, ')')
	x<-maximum.targets(sim_ru)
	expect_equal(x$proportion,rep(1, nrow(x)))
})

test_that('maximum.targets (reliable)', {
	data(sim_ru)
	sim_ru@opts <- RapReliableOpts(max.r.level = 1L)
	x<-maximum.targets(sim_ru)
})

test_that('solve.RapUnsolved (unreliable - NumberSolutions=1 - MultipleSolutionsMethod=benders.cuts)', {
	# skip if gurobi not installed
	if (!is.GurobiInstalled(FALSE)) skip('Gurobi not installed on system')
	# load RapUnsolved object
	set.seed(500)
	data(sim_ru)
	sim_ru<-pu.subset(sim_ru, 1:10)
	sim_ru<-dp.subset(sim_ru, species=1:3, space=1, points=1:5)
	sim_ru@data@targets[[3]] <- c(0.5,0.5,0.5,-10,-10,-10)
	# solve it
	sim_rs<-raptr::solve(sim_ru, GurobiOpts(MIPGap=0.99, Presolve=2L), verbose=TRUE)
	# run checks
	expect_equal(nrow(summary(sim_rs)), 1L)
	runUnreliableChecks(sim_rs)
})

test_that('solve.RapUnsolved (reliable - NumberSolutions=1 - MultipleSolutionsMethod=benders.cuts)', {
	# skip if gurobi not installed
	if (!is.GurobiInstalled(FALSE)) skip('Gurobi not installed on system')
	# load RapUnsolved object
	set.seed(500)
	data(sim_ru)
	sim_ru<-pu.subset(sim_ru, 1:10)
	sim_ru<-dp.subset(sim_ru, species=1:3, space=1, points=1:5)
	sim_ru@opts<-RapReliableOpts(failure.multiplier=10)
	sim_ru@data@targets[[3]]<-c(0.5,0.5,0.5,-10000,-10000,-10000)
	# solve it
	sim_rs<-raptr::solve(sim_ru, GurobiOpts(MIPGap=0.99, Presolve=2L), verbose=TRUE)
	# run checks
	expect_equal(nrow(summary(sim_rs)), 1L)
	runReliableChecks(sim_rs)
})

test_that('solve.RapUnsolved (unreliable - NumberSolutions=1 - MultipleSolutionsMethod=solution.pool)', {
	# skip if gurobi not installed
	if (!is.GurobiInstalled(FALSE)) skip('Gurobi not installed on system')
	# load RapUnsolved object
	set.seed(500)
	data(sim_ru)
	sim_ru<-pu.subset(sim_ru, 1:10)
	sim_ru<-dp.subset(sim_ru, species=1:3, space=1, points=1:5)
	sim_ru@data@targets[[3]]<-c(0.5,0.5,0.5,-10,-10,-10)
	# solve it
	sim_rs<-raptr::solve(sim_ru, GurobiOpts(MIPGap=0.99, Presolve=2L, MultipleSolutionsMethod='solution.pool'), verbose=TRUE)
	# run checks
	expect_equal(nrow(summary(sim_rs)), 1L)
	runUnreliableChecks(sim_rs)
})

test_that('solve.RapUnsolved (reliable - NumberSolutions=1 - MultipleSolutionsMethod=solution.pool)', {
	# skip if gurobi not installed
	if (!is.GurobiInstalled(FALSE)) skip('Gurobi not installed on system')
	# load RapUnsolved object
	set.seed(500)
	data(sim_ru)
	sim_ru<-pu.subset(sim_ru, 1:10)
	sim_ru<-dp.subset(sim_ru, species=1:3, space=1, points=1:5)
	sim_ru@opts<-RapReliableOpts(failure.multiplier=10)
	sim_ru@data@targets[[3]]<-c(0.5,0.5,0.5,-10000,-10000,-10000)
	# solve it
	sim_rs<-raptr::solve(sim_ru, GurobiOpts(MIPGap=0.99, Presolve=2L, MultipleSolutionsMethod='solution.pool'), verbose=TRUE)
	# run checks
	expect_equal(nrow(summary(sim_rs)), 1L)
	runReliableChecks(sim_rs)
})

test_that('solve.RapUnsolved (unreliable - NumberSolutions=1 - sparse occupancy)', {
	# skip if gurobi not installed
	if (!is.GurobiInstalled(FALSE)) skip('Gurobi not installed on system')
	# load RapUnsolved object
	set.seed(500)
	data(sim_ru)
	sim_ru<-pu.subset(sim_ru, 1:10)
	sim_ru<-dp.subset(sim_ru, species=1:3, space=1, points=1:30)
	sim_ru@data@targets[[3]]<-c(0.5,0.5,0.5,-1000000,-1000000,-1000000)
	sim_ru@opts<-RapUnreliableOpts()
	sim_ru@data@pu.species.probabilities<-sim_ru@data@pu.species.probabilities[sample(
		seq_len(nrow(sim_ru@data@pu.species.probabilities)),
		size=ceiling(nrow(sim_ru@data@pu.species.probabilities)*0.7)
	),]
	sim_ru@data@attribute.spaces <- lapply(seq_along(sim_ru@data@attribute.spaces), function(i) {
		AttributeSpaces(
			spaces=lapply(
				seq_along(sim_ru@data@attribute.spaces[[i]]@spaces),
				function(j) {
					curr.pu <- sim_ru@data@pu.species.probabilities$pu[which(sim_ru@data@pu.species.probabilities$species==j)]
					AttributeSpace(
						species=sim_ru@data@attribute.spaces[[i]]@spaces[[j]]@species,
						demand.points=sim_ru@data@attribute.spaces[[i]]@spaces[[j]]@demand.points,
						planning.unit.points=PlanningUnitPoints(
							coords=sim_ru@data@attribute.spaces[[i]]@spaces[[j]]@planning.unit.points@coords[curr.pu,],
							ids=curr.pu
						)
					)
				}
			),
			name=sim_ru@data@attribute.spaces[[i]]@name
		)
	})
	# solve it
	sim_rs<-raptr::solve(sim_ru, GurobiOpts(MIPGap=0.99, Presolve=2L))
	# run checks
	expect_equal(nrow(summary(sim_rs)), 1L)
	runUnreliableChecks(sim_rs)
})

test_that('solve.RapUnsolved (reliable - NumberSolutions=1 - sparse occupancy)', {
	# skip if gurobi not installed
	if (!is.GurobiInstalled(FALSE)) skip('Gurobi not installed on system')
	# load RapUnsolved object
	set.seed(500)
	data(sim_ru)
	sim_ru<- pu.subset(sim_ru, 1:10)
	sim_ru<-dp.subset(sim_ru, species=1:3, space=1, points=1:30)
	sim_ru@data@pu.species.probabilities<-sim_ru@data@pu.species.probabilities[sample(
		seq_len(nrow(sim_ru@data@pu.species.probabilities)),
		ceiling(nrow(sim_ru@data@pu.species.probabilities)*0.7)
	),]
	sim_ru@data@attribute.spaces <- lapply(seq_along(sim_ru@data@attribute.spaces), function(i) {
		AttributeSpaces(
			spaces=lapply(
				seq_along(sim_ru@data@attribute.spaces[[i]]@spaces),
				function(j) {
					curr.pu <- sim_ru@data@pu.species.probabilities$pu[which(sim_ru@data@pu.species.probabilities$species==j)]
					AttributeSpace(
						species=sim_ru@data@attribute.spaces[[i]]@spaces[[j]]@species,
						demand.points=sim_ru@data@attribute.spaces[[i]]@spaces[[j]]@demand.points,
						planning.unit.points=PlanningUnitPoints(
							coords=sim_ru@data@attribute.spaces[[i]]@spaces[[j]]@planning.unit.points@coords[curr.pu,],
							ids=curr.pu
						)
					)
				}
			),
			name=sim_ru@data@attribute.spaces[[i]]@name
		)
	})
	sim_ru@opts<-RapReliableOpts()
	sim_ru@data@targets[[3]]<-c(0.5,0.5,0.5,-1000000,-1000000,-1000000)
	# solve it
	sim_rs<-raptr::solve(sim_ru, GurobiOpts(MIPGap=0.15, Presolve=2L), verbose=TRUE)
	# run checks
	expect_equal(nrow(summary(sim_rs)), 1L)
	runReliableChecks(sim_rs)
})

test_that('solve.RapUnsolved (unreliable - NumberSolutions=3 - MultipleSolutionsMethod=benders.cuts)', {
	# skip if gurobi not installed
	if (!is.GurobiInstalled(FALSE)) skip('Gurobi not installed on system')
	# load RapUnsolved object
	set.seed(500)
	data(sim_ru)
	sim_ru<- pu.subset(sim_ru, 1:20)
	sim_ru<-dp.subset(sim_ru, species=1:3, space=1, points=1:5)
	sim_ru@opts<-RapUnreliableOpts()
	sim_ru@data@targets[[3]]<-c(0.5,0.5,0.5,-10000,-10000,-10000)
	# solve it
	sim_rs<-raptr::solve(sim_ru, GurobiOpts(MIPGap=0.99, Presolve=2L, NumberSolutions=3L))
	# run checks
	expect_equal(nrow(summary(sim_rs)), 3L)
	runUnreliableChecks(sim_rs)
})

test_that('solve.RapUnsolved (reliable - NumberSolutions=3 - MultipleSolutionsMethod=benders.cuts)', {
	# skip if gurobi not installed
	if (!is.GurobiInstalled(FALSE)) skip('Gurobi not installed on system')
	# load RapUnsolved object
	set.seed(500)
	data(sim_ru)
	sim_ru<-pu.subset(sim_ru, 1:20)
	sim_ru<-dp.subset(sim_ru, species=1:3, space=1, points=1:3)
	sim_ru@data@targets[[3]]<-c(0.5,0.5,0.5,-10000,-10000,-10000)
	sim_ru@opts<-RapReliableOpts()
	# solve it
	sim_rs<-raptr::solve(sim_ru, GurobiOpts(MIPGap=0.99, Presolve=2L, NumberSolutions=3L))
	# check number of selections is 1
	expect_equal(nrow(summary(sim_rs)), 3L)
	runReliableChecks(sim_rs)
})

test_that('solve.RapUnsolved (unreliable - NumberSolutions=2 - MultipleSolutionsMethod=solution.pool)', {
	# skip if gurobi not installed
	if (!is.GurobiInstalled(FALSE)) skip('Gurobi not installed on system')
	# load RapUnsolved object
	set.seed(500)
	data(sim_ru)
	sim_ru<- pu.subset(sim_ru, 1:20)
	sim_ru<-dp.subset(sim_ru, species=1:3, space=1, points=1:5)
	sim_ru@opts<-RapUnreliableOpts()
	sim_ru@data@targets[[3]]<-c(0.5,0.5,0.5,-10000,-10000,-10000)
	# solve it
	sim_rs<-raptr::solve(sim_ru, GurobiOpts(MIPGap=0.99, Presolve=2L, NumberSolutions=2L, MultipleSolutionsMethod='solution.pool'))
	# run checks
	expect_equal(nrow(summary(sim_rs)), 2L)
	runUnreliableChecks(sim_rs)
})

test_that('solve.RapUnsolved (unreliable - STATUS test)', {
	# skip if gurobi not installed
	if (!is.GurobiInstalled(FALSE)) skip('Gurobi not installed on system')
	# load RapUnsolved object
	set.seed(500)
	data(sim_ru)
	sim_ru<- pu.subset(sim_ru, 1:10)
	sim_ru<-dp.subset(sim_ru, species=1:3, space=1, points=1:30)
	sim_ru@data@targets[[3]]<-c(0.5,0.5,0.5,-10000,-10000,-10000)
	sim_ru@opts<-RapUnreliableOpts()
	# lock in and lock out planning units
	sim_ru@data@pu$status[1]<-0L
	sim_ru@data@pu$status[2]<-2L
	sim_ru@data@pu$status[3]<-3L
	# solve it
	sim_rs<-raptr::solve(sim_ru, GurobiOpts(MIPGap=0.99, Presolve=2L))
	# run checks
	expect_equal(nrow(summary(sim_rs)), 1L)
	expect_identical(selections(sim_rs)[2], 1L)
	expect_identical(selections(sim_rs)[3], 0L)
	runUnreliableChecks(sim_rs)
	
})

test_that('solve.RapUnsolved (reliable - STATUS test)', {
	# skip if gurobi not installed
	if (!is.GurobiInstalled(FALSE)) skip('Gurobi not installed on system')
	# load RapUnsolved object
	set.seed(500)
	data(sim_ru)
	sim_ru<- pu.subset(sim_ru, 1:10)
	sim_ru<-dp.subset(sim_ru, species=1:3, space=1, points=1:30)
	sim_ru@data@targets[[3]]<-c(0.5,0.5,0.5,-10000,-10000,-10000)
	sim_ru@opts<-RapReliableOpts()
	# lock in and lock out planning units
	sim_ru@data@pu$status[1]<-0L
	sim_ru@data@pu$status[2]<-2L
	sim_ru@data@pu$status[3]<-3L
	# solve it
	sim_rs<-raptr::solve(sim_ru, GurobiOpts(MIPGap=0.99, Presolve=2L))
	# run checks
	expect_equal(nrow(summary(sim_rs)), 1L)
	expect_identical(selections(sim_rs)[2], 1L)
	expect_identical(selections(sim_rs)[3], 0L)
	runReliableChecks(sim_rs)
})

test_that('solve.RapUnsolved (unreliable - BLM test)', {
	# skip if gurobi not installed
	if (!is.GurobiInstalled(FALSE)) skip('Gurobi not installed on system')
	# load RapUnsolved object
	set.seed(500)
	data(sim_ru)
	sim_ru<- pu.subset(sim_ru, 1:10)
	sim_ru@opts<-RapUnreliableOpts(BLM=100)
	sim_ru<-dp.subset(sim_ru, species=1:3, space=1, points=1:30)
	sim_ru@data@targets[[3]]<-c(0.5,0.5,0.5,-10000,-10000,-10000)
	# solve it
	sim_rs<-raptr::solve(sim_ru, GurobiOpts(MIPGap=0.99, Presolve=2L))
	# run checks
	runUnreliableChecks(sim_rs)
})

test_that('solve.RapUnsolved (reliable - BLM test)', {
	# skip if gurobi not installed
	if (!is.GurobiInstalled(FALSE)) skip('Gurobi not installed on system')
	# load RapUnsolved object
	set.seed(500)
	data(sim_ru)
	sim_ru<- pu.subset(sim_ru, 1:10)
	sim_ru<-dp.subset(sim_ru, species=1:3, space=1, points=1:30)
	sim_ru@opts<-RapReliableOpts(BLM=100)
	sim_ru@data@targets[[3]]<-c(0.5,0.5,0.5,-10000,-10000,-10000)
	# solve it
	sim_rs<-raptr::solve(sim_ru, GurobiOpts(MIPGap=0.99, Presolve=2L))
	# run checks
	runReliableChecks(sim_rs)
})

test_that('update.RapUnsolved', {
	# load RapUnsolved object
	set.seed(500)
	data(sim_ru)
	# update object
	sim_ru2 <- update(sim_ru, BLM=100, name=letters[1:3], solve=FALSE)
	# checks
	expect_equal(sim_ru2@opts@BLM, 100)
	expect_equal(sim_ru2@data@species$name, letters[1:3])
})

test_that('update.RapUnsolved (formulation argument)', {
	# load RapUnsolved object
	set.seed(500)
	data(sim_ru)
	# update object
	sim_ru@opts@BLM<-100
	sim_ru2 <- update(sim_ru, formulation='reliable', solve=FALSE)
	# checks
	expect_equal(sim_ru2@opts@BLM, 100)
	expect_is(sim_ru2@opts,'RapReliableOpts')
})

test_that('amount.target.RapData', {
	data(sim_ru)
	expect_equal(
		unname(amount.target(sim_ru)),
		rep(0.2, 3)
	)
	expect_equal(
		unname(amount.target(sim_ru, 1)),
		0.2
	)
})

test_that('amount.target<-.RapUnsolved', {
	data(sim_ru)
	amount.target(sim_ru)<-0.3
	expect_equal(unname(amount.target(sim_ru)), rep(0.3, 3))
	amount.target(sim_ru, 1)<-0.5
	expect_equal(unname(amount.target(sim_ru)), c(0.5, 0.3, 0.3))
})

test_that('space.target.RapData', {
	data(sim_ru)
	expect_equal(
		unname(space.target(sim_ru@data)[,1]),
		rep(0.85, 3)
	)
	expect_equal(
		unname(space.target(sim_ru@data, species=1)[,1]),
		rep(0.85)
	)
	expect_equal(
		unname(space.target(sim_ru@data, space=1)[,1]),
		rep(0.85, 3)
	)
	expect_equal(
		unname(space.target(sim_ru@data, species=1, space=1)[,1]),
		0.85
	)
})

test_that('space.target<-.RapUnsolved', {
	data(sim_ru)
	space.target(sim_ru)<-0.3
	expect_equal(unname(space.target(sim_ru)[,1]), rep(0.3, 3))
	space.target(sim_ru, 1)<-0.5
	expect_equal(unname(space.target(sim_ru)[,1]), c(0.5, 0.3, 0.3))
})

test_that('space.plot.RapUnsolved (sparse)', {
	# skip if gurobi not installed
	if (!is.GurobiInstalled(FALSE)) skip('Gurobi not installed on system')
	# load RapUnsolved object
	set.seed(500)
	data(sim_ru)
	sim_ru<-pu.subset(sim_ru, 1:10)
	sim_ru<-dp.subset(sim_ru, species=1:3, space=1, points=1:30)
	sim_ru@data@targets[[3]]<-c(0.5,0.5,0.5,-1000000,-1000000,-1000000)
	sim_ru@opts<-RapUnreliableOpts()
	sim_ru@data@pu.species.probabilities<-sim_ru@data@pu.species.probabilities[sample(
		seq_len(nrow(sim_ru@data@pu.species.probabilities)),
		size=ceiling(nrow(sim_ru@data@pu.species.probabilities)*0.7)
	),]
	sim_ru@data@attribute.spaces <- lapply(seq_along(sim_ru@data@attribute.spaces), function(i) {
		AttributeSpaces(
			spaces=lapply(
				seq_along(sim_ru@data@attribute.spaces[[i]]@spaces),
				function(j) {
					curr.pu <- sim_ru@data@pu.species.probabilities$pu[which(sim_ru@data@pu.species.probabilities$species==j)]
					AttributeSpace(
						species=sim_ru@data@attribute.spaces[[i]]@spaces[[j]]@species,
						demand.points=sim_ru@data@attribute.spaces[[i]]@spaces[[j]]@demand.points,
						planning.unit.points=PlanningUnitPoints(
							coords=sim_ru@data@attribute.spaces[[i]]@spaces[[j]]@planning.unit.points@coords[curr.pu,],
							ids=curr.pu
						)
					)
				}
			),
			name=sim_ru@data@attribute.spaces[[i]]@name
		)
	})
	# try plotting spase data
	space.plot(sim_rs, 1, 1, main='spp1')
	space.plot(sim_rs, 2, 1, main='spp2')
	space.plot(sim_rs, 3, 1, main='spp3')
})
