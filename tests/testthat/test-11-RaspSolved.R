### create RaspSolved object
# skip if gurobi not installed
if (is.GurobiInstalled(FALSE)) {
	# create RaspUnsolved object
	data(sim_pus, sim_spp)
	go<-GurobiOpts(MIPGap=0.9)
	rd<-make.RaspData(sim_pus[1:10,], sim_spp[[1]], NULL, include.geographic.space=TRUE,n.demand.points=5L)
	rd@pu$status[1]=1
	rd@pu$status[2]=2
	ro1<-RaspOpts(NUMREPS=2L)
	ro2<-RaspOpts(NUMREPS=2L, BLM=5)
	# solve object
	ru1<-RaspUnsolved(ro1, go, rd)
	ru2<-RaspUnsolved(ro2, go, rd)
	# solve it
	rs1<-raspr::solve(ru1)
	rs2<-raspr::solve(ru2)

	### tests
	test_that('plotting function (x=RaspSolved, y=NULL)', {
		plot(rs1)
	})

	test_that('plotting function (x=RaspSolved, y=0)', {
		plot(rs1, 0)
	})


	test_that('plotting function (x=RaspSolved, y=1)', {
		plot(rs1, 1)
	})

	test_that('plotting function (x=RaspSolved, y=RaspSolved, i=NULL, j=NULL)', {
		plot(rs1, rs2)
	})

	test_that('plotting function (x=RaspSolved, y=RaspSolved, i=0, j=1)', {
		plot(rs1, rs2, 0, 1)
	})

	test_that('selections.RaspSolved', {
		expect_identical(selections(rs1), rs1@results@selections[rs1@results@best,])
		expect_identical(selections(rs1, NULL), rs1@results@selections)
		expect_identical(selections(rs1, 1), rs1@results@selections[1,])
	})

	test_that('score.RaspSolved', {
		expect_identical(score(rs1), rs1@results@summary$Score[rs1@results@best])
		expect_identical(score(rs1, NULL), rs1@results@summary$Score)
		expect_identical(score(rs1, 1), rs1@results@summary$Score[1])
	})

	test_that('amount.held.RaspSolved', {
		expect_identical(amount.held(rs1), rs1@results@amount.held[rs1@results@best])
		expect_identical(amount.held(rs1, NULL), rs1@results@amount.held)
		expect_identical(amount.held(rs1, 1), rs1@results@amount.held[1])
	})

	test_that('space.held.RaspSolved', {
		expect_identical(space.held(rs1), rs1@results@space.held[rs1@results@best])
		expect_identical(space.held(rs1, NULL), rs1@results@space.held)
		expect_identical(space.held(rs1, 1), rs1@results@space.held[1])
	})

	test_that('summary.RaspSolved', {
		expect_identical(summary(rs1), rs1@results@summary)
	})

	test_that('logging.file.RaspSolved', {
		expect_identical(logging.file(rs1), rs1@results@logging.file[rs1@results@best])
		expect_identical(logging.file(rs1, NULL), rs1@results@logging.file)
		expect_identical(logging.file(rs1, 1), rs1@results@logging.file[1])
	})
}
