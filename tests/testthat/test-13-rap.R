context('13-rap')

test_that('rap (unreliable - default RapOpts and GurobiOpts - solve=FALSE)', {
  # load data
  set.seed(500)
  data(cs_pus, cs_spp)
  cs_pus<-cs_pus[1:10,]
  # run function
  cs_ru<-rap(
    pus=cs_pus,
    species=cs_spp,
    formulation='unreliable',
    amount.target=0.2,
    space.target=-10000,
    n.demand.points=3L,
    include.geographic.space=TRUE,
    solve=FALSE
  )
})

test_that('rap (unreliable - custom RapOpts and GurobiOpts - solve=TRUE)', {
  if (!is.GurobiInstalled(FALSE)) skip('Gurobi not installed on system')
  # load data
  set.seed(500)
  data(cs_pus, cs_spp)
	cs_pus<-cs_pus[1:10,]
	# run function
	cs_rs<-rap(
		pus=cs_pus,
		species=cs_spp,
		formulation='unreliable',
		amount.target=0.2,
		space.target=-100,
		n.demand.points=3L,
		include.geographic.space=TRUE,
		MIPGap=0.9,
		BLM=100
	)
  # check calculations
  expect_true(all(space.held(cs_rs) >= -100))
  expect_true(all(space.held(cs_rs) <= 1))
  expect_true(all(amount.held(cs_rs) >= 0.2))
  expect_true(all(amount.held(cs_rs) <= 1))
  expect_equal(cs_rs@opts@BLM,100)
})

test_that('rap (reliable - default RapOpts and GurobiOpts - solve=FALSE)', {
	# laod data
  set.seed(500)	
	data(cs_pus, cs_spp)
	cs_pus<-cs_pus[1:10,]
	# run function
	cs_ru<-rap(
		pus=cs_pus,
		species=cs_spp,
		formulation='reliable',
		n.demand.points=5L,
    amount.target=0.2,
    space.target=-10000,
		include.geographic.space=TRUE,
		verbose=TRUE,
		solve=FALSE
	)
})

test_that('rap (reliable - custom RapOpts and GurobiOpts - solve=TRUE)', {
  if (!is.GurobiInstalled(FALSE)) skip('Gurobi not installed on system')
  # load data
  set.seed(500)
  data(cs_pus, cs_spp)
  cs_pus<-cs_pus[1:10,]
  # run function
  cs_rs<-rap(
    pus=cs_pus,
    species=cs_spp,
    formulation='reliable',
    amount.target=0.2,
    space.target=-10000,
    n.demand.points=3L,
    include.geographic.space=TRUE,
    MIPGap=0.9,
    BLM=100
  )
  # check calculations
  expect_true(all(space.held(cs_rs) >= -10000))
  expect_true(all(amount.held(cs_rs) >= 0.2))
  expect_true(all(amount.held(cs_rs) <= 1))
  expect_equal(cs_rs@opts@BLM,100)
})
