test_that('rasp (unreliable - default RaspOpts and GurobiOpts - solve=FALSE)', {
  # load data
  data(cs_pus, cs_spp)
  cs_pus<-cs_pus[1:10,]
  # run function
  cs_ru<-rasp(
    pus=cs_pus,
    species=cs_spp,
    formulation='unreliable',
    n.demand.points=3L,
    include.geographic.space=TRUE,
    solve=FALSE
  )
  # check calculations
  expect_true(all(space.held(cs_ru) >= 0.2))
  expect_true(all(space.held(cs_ru) <= 1))
  expect_true(all(amount.held(cs_ru) >= 0.2))
  expect_true(all(amount.held(cs_ru) <= 1))
})

test_that('rasp (unreliable - custom RaspOpts and GurobiOpts - solve=FALSE)', {
  if (!is.GurobiInstalled(FALSE)) skip('Gurobi not installed on system')
  # load data
	data(cs_pus, cs_spp)
	cs_pus<-cs_pus[1:10,]
	# run function
	cs_rs<-rasp(
		pus=cs_pus,
		species=cs_spp,
		formulation='unreliable',
		n.demand.points=3L,
		include.geographic.space=TRUE,
		MIPGap=0.9,
		BLM=100
	)
  # check calculations
  expect_true(all(space.held(cs_rs) >= 0.2))
  expect_true(all(space.held(cs_rs) <= 1))
  expect_true(all(amount.held(cs_rs) >= 0.2))
  expect_true(all(amount.held(cs_rs) <= 1))
})

test_that('rasp (reliable - default RaspOpts and GurobiOpts - solve=FALSE)', {
	# laod data
	data(cs_pus, cs_spp)
	cs_pus<-cs_pus[1:10,]
	# run function
	cs_ru<-rasp(
		pus=cs_pus,
		species=cs_spp,
		formulation='reliable',
		n.demand.points=5L,
		include.geographic.space=TRUE,
		verbose=TRUE,
		solve=FALSE
	)
	# check calculations
  expect_true(all(space.held(cs_ru) >= 0.2))
  expect_true(all(space.held(cs_ru) <= 1))
  expect_true(all(amount.held(cs_ru) >= 0.2))
  expect_true(all(amount.held(cs_ru) <= 1))
})

test_that('rasp (reliable - default RaspOpts and GurobiOpts - solve=TRUE)', {
  if (!is.GurobiInstalled(FALSE)) skip('Gurobi not installed on system')
	data(cs_pus, cs_spp)
	cs_pus<-cs_pus[1:10,]
	# run function
	cs_rs<-rasp(
		pus=cs_pus,
		species=cs_spp,
		formulation='reliable',
		n.demand.points=5L,
		include.geographic.space=TRUE,
		verbose=TRUE
	)
	# check calculations
  expect_true(all(space.held(cs_rs) >= 0.2))
  expect_true(all(space.held(cs_rs) <= 1))
  expect_true(all(amount.held(cs_rs) >= 0.2))
  expect_true(all(amount.held(cs_rs) <= 1))
})

test_that('rasp (reliable - custom RaspOpts and GurobiOpts - solve=TRUE)', {
  if (!is.GurobiInstalled(FALSE)) skip('Gurobi not installed on system')
  # load data
  data(cs_pus, cs_spp)
  cs_pus<-cs_pus[1:10,]
  # run function
  cs_rs<-rasp(
    pus=cs_pus,
    species=cs_spp,
    formulation='reliable',
    n.demand.points=3L,
    include.geographic.space=TRUE,
    MIPGap=0.9,
    BLM=100
  )
  # check calculations
  expect_true(all(space.held(cs_rs) >= 0.2))
  expect_true(all(space.held(cs_rs) <= 1))
  expect_true(all(amount.held(cs_rs) >= 0.2))
  expect_true(all(amount.held(cs_rs) <= 1))  
})
